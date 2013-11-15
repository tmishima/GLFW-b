module Graphics.UI.GLFW
  ( -- * Error handling
    Error (..)
    --
  , setErrorCallback, ErrorCallback

    -- * Initialization and version information
  , Version (..)
    --
  , init
  , terminate
  , getVersion
  , getVersionString

    -- * Monitor handling
  , Monitor
  , MonitorState (..)
  , VideoMode    (..)
  , GammaRamp    (gammaRampRed, gammaRampGreen, gammaRampBlue)
  , makeGammaRamp
    --
  , getMonitors
  , getPrimaryMonitor
  , getMonitorPos
  , getMonitorPhysicalSize
  , getMonitorName
  , setMonitorCallback, MonitorCallback
  , getVideoModes
  , getVideoMode
  , setGamma
  , getGammaRamp
  , setGammaRamp

    -- * Window handling
  , Window
  , WindowHint        (..)
  , FocusState        (..)
  , IconifyState      (..)
  , ContextRobustness (..)
  , OpenGLProfile     (..)
  , ClientAPI         (..)
    --
  , defaultWindowHints
  , windowHint
  , createWindow
  , destroyWindow
  , windowShouldClose
  , setWindowShouldClose
  , setWindowTitle
  , getWindowPos
  , setWindowPos
  , getWindowSize
  , setWindowSize
  , getFramebufferSize
  , iconifyWindow
  , restoreWindow
  , showWindow
  , hideWindow
  , getWindowMonitor
  , setCursorPos
    -- related to c'glfwGetWindowAttrib --.
  , getWindowFocused                   -- |
  , getWindowIconified                 -- |
  , getWindowResizable                 -- |
  , getWindowDecorated                 -- |
  , getWindowVisible                   -- |
  , getWindowClientAPI                 -- |
  , getWindowContextVersionMajor       -- |
  , getWindowContextVersionMinor       -- |
  , getWindowContextVersionRevision    -- |
  , getWindowContextRobustness         -- |
  , getWindowOpenGLForwardCompat       -- |
  , getWindowOpenGLDebugContext        -- |
  , getWindowOpenGLProfile  --------------'
  , setWindowPosCallback,       WindowPosCallback
  , setWindowSizeCallback,      WindowSizeCallback
  , setWindowCloseCallback,     WindowCloseCallback
  , setWindowRefreshCallback,   WindowRefreshCallback
  , setWindowFocusCallback,     WindowFocusCallback
  , setWindowIconifyCallback,   WindowIconifyCallback
  , setFramebufferSizeCallback, FramebufferSizeCallback
  , pollEvents
  , waitEvents

    -- * Input handling
  , Key                         (..)
  , KeyState                    (..)
  , Joystick                    (..)
  , JoystickButtonState         (..)
  , MouseButton                 (..)
  , MouseButtonState            (..)
  , CursorState                 (..)
  , CursorInputMode             (..)
  , StickyKeysInputMode         (..)
  , StickyMouseButtonsInputMode (..)
  , ModifierKeys                (..)
    --
    -- related to c'glfwSetInputMode ----.
  , getCursorInputMode                -- |
  , setCursorInputMode                -- |
  , getStickyKeysInputMode            -- |
  , setStickyKeysInputMode            -- |
  , getStickyMouseButtonsInputMode    -- |
  , setStickyMouseButtonsInputMode  -----'
  , getKey
  , getMouseButton
  , getCursorPos
  , setKeyCallback,         KeyCallback
  , setCharCallback,        CharCallback
  , setMouseButtonCallback, MouseButtonCallback
  , setCursorPosCallback,   CursorPosCallback
  , setCursorEnterCallback, CursorEnterCallback
  , setScrollCallback,      ScrollCallback
  , joystickPresent
  , getJoystickAxes
  , getJoystickButtons
  , getJoystickName

    -- * Time
  , getTime
  , setTime

    -- * Context
  , makeContextCurrent
  , getCurrentContext
  , swapBuffers
  , swapInterval
  , extensionSupported

    -- * Clipboard
  , getClipboardString
  , setClipboardString
  ) where

--------------------------------------------------------------------------------

import Prelude hiding (init)

import Control.Monad         (when, liftM)
import Data.IORef            (IORef, atomicModifyIORef, newIORef, readIORef)
import Foreign.C.String      (peekCString, withCString)
import Foreign.C.Types       (CUInt, CUShort)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (advancePtr, allocaArray, peekArray, withArray)
import Foreign.Ptr           (FunPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.StablePtr
import Foreign.Storable      (Storable (..))
import System.IO.Unsafe      (unsafePerformIO)

import Graphics.UI.GLFW.C
import Graphics.UI.GLFW.Types

import Bindings.GLFW

--------------------------------------------------------------------------------

-- We store FunPtrs from mk'GLFW*fun in these stored*Fun IORefs. Initialized
-- with unsafePerformIO, they are basically mutable global variables.

storedErrorFun           :: IORef C'GLFWerrorfun
storedMonitorFun         :: IORef C'GLFWmonitorfun

storedErrorFun           = unsafePerformIO $ newIORef nullFunPtr
storedMonitorFun         = unsafePerformIO $ newIORef nullFunPtr

-- These NOINLINE pragmas are due to use of unsafePerformIO.
-- See http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO-Unsafe.html#v:unsafePerformIO .

{-# NOINLINE storedErrorFun           #-}
{-# NOINLINE storedMonitorFun         #-}

setWindowCallback
  :: (c -> IO (FunPtr c))                    -- wf   wrapper function
  -> (h -> c)                                -- af   adapter function
  -> (FunPtr c -> IO (FunPtr c))             -- gf   c'glfwSet*Callback function
  -> (WindowCallbacks -> IORef (FunPtr c))   -- ior  accessor for storage location
  -> Window                                  -- win  window
  -> Maybe h                                 -- mcb  Haskell callback
  -> IO ()
setWindowCallback wr af gf ior win mcb = do
    pcallbacks <- castPtrToStablePtr `liftM` c'glfwGetWindowUserPointer (unWindow win)
    callbacks <- deRefStablePtr pcallbacks
    setCallback wr af gf (ior callbacks) mcb

setCallback
  :: (c -> IO (FunPtr c))          -- wf   wrapper function
  -> (h -> c)                      -- af   adapter function
  -> (FunPtr c -> IO (FunPtr c))   -- gf   c'glfwSet*Callback function
  -> IORef (FunPtr c)              -- ior  storage location
  -> Maybe h                       -- mcb  Haskell callback
  -> IO ()
setCallback wf af gf ior mcb = do
    -- If mcb is Just, make ccb the FunPtr of the adapted callback. Otherwise a
    -- null FunPtr.
    ccb <- maybe (return nullFunPtr) (wf . af) mcb
    -- Call the GLFW callback-setting function.
    _ <- gf ccb
    -- Store it.
    storeCallback ior ccb

storeCallback :: IORef (FunPtr a) -> FunPtr a -> IO ()
storeCallback ior new = do
    -- Store the new FunPtr, retrieve the previous one.
    prev <- atomicModifyIORef ior (\cur -> (new, cur))
    -- Free the old FunPtr if necessary.
    when (prev /= nullFunPtr) $ freeHaskellFunPtr prev

--------------------------------------------------------------------------------

type ErrorCallback           = Error -> String                                           -> IO ()
type WindowPosCallback       = Window -> Int -> Int                                      -> IO ()
type WindowSizeCallback      = Window -> Int -> Int                                      -> IO ()
type WindowCloseCallback     = Window                                                    -> IO ()
type WindowRefreshCallback   = Window                                                    -> IO ()
type WindowFocusCallback     = Window -> FocusState                                      -> IO ()
type WindowIconifyCallback   = Window -> IconifyState                                    -> IO ()
type FramebufferSizeCallback = Window -> Int -> Int                                      -> IO ()
type MouseButtonCallback     = Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
type CursorPosCallback       = Window -> Double -> Double                                -> IO ()
type CursorEnterCallback     = Window -> CursorState                                     -> IO ()
type ScrollCallback          = Window -> Double -> Double                                -> IO ()
type KeyCallback             = Window -> Key -> Int -> KeyState -> ModifierKeys          -> IO ()
type CharCallback            = Window -> Char                                            -> IO ()
type MonitorCallback         = Monitor -> MonitorState                                   -> IO ()

--------------------------------------------------------------------------------
-- Error handling

setErrorCallback :: Maybe ErrorCallback -> IO ()
setErrorCallback = setCallback
    mk'GLFWerrorfun
    (\cb a0 a1 -> do
        s <- peekCString a1
        cb (fromC a0) s)
    c'glfwSetErrorCallback
    storedErrorFun

--------------------------------------------------------------------------------
-- Initialization and version information

init :: IO Bool
init =
    fromC `fmap` c'glfwInit

terminate :: IO ()
terminate = do
    c'glfwTerminate
    -- Free all stored FunPtrs.
    storeCallback storedErrorFun           nullFunPtr
    storeCallback storedMonitorFun         nullFunPtr

getVersion :: IO Version
getVersion =
    allocaArray 3 $ \p -> do
        let p0 = p
            p1 = p `advancePtr` 1
            p2 = p `advancePtr` 2
        c'glfwGetVersion p0 p1 p2
        v0 <- fromC `fmap` peek p0
        v1 <- fromC `fmap` peek p1
        v2 <- fromC `fmap` peek p2
        return $ Version v0 v1 v2

getVersionString :: IO (Maybe String)
getVersionString = do
    p'vs <- c'glfwGetVersionString
    if p'vs /= nullPtr
      then Just `fmap` peekCString p'vs
      else return Nothing

--------------------------------------------------------------------------------
-- Monitor handling

getMonitors :: IO (Maybe [Monitor])
getMonitors =
    alloca $ \p'n -> do
        p'mon <- c'glfwGetMonitors p'n
        n <- fromC `fmap` peek p'n
        if p'mon == nullPtr || n <= 0
          then return Nothing
          else (Just . map fromC) `fmap` peekArray n p'mon

getPrimaryMonitor :: IO (Maybe Monitor)
getPrimaryMonitor = do
    p'mon <- c'glfwGetPrimaryMonitor
    return $
      if p'mon == nullPtr
        then Nothing
        else Just $ fromC p'mon

-- | This function returns the position, in screen coordinates, of the
--   upper-left corner of the specified monitor.
--
getMonitorPos :: Monitor      -- ^ The monitor to query. 
              -> IO (Int, Int) -- ^ (x-coordinate,y-coordinate)
getMonitorPos mon =
    allocaArray 2 $ \p -> do
        let p'x = p
            p'y = p `advancePtr` 1
        c'glfwGetMonitorPos (toC mon) p'x p'y
        x <- fromC `fmap` peek p'x
        y <- fromC `fmap` peek p'y
        return (x, y)

-- | This function returns the size, in millimetres, of the display area of
--   the specified monitor.
--
getMonitorPhysicalSize
  :: Monitor -- ^ The monitor to query.
  -> IO (Int, Int) -- ^ (width,height)[mm],of the monitor's display area
getMonitorPhysicalSize mon =
    allocaArray 2 $ \p -> do
        let p'w = p
            p'h = p `advancePtr` 1
        c'glfwGetMonitorPhysicalSize (toC mon) p'w p'h
        w <- fromC `fmap` peek p'w
        h <- fromC `fmap` peek p'h
        return (w, h)

-- | This function returns a human-readable name of the specified monitor.
getMonitorName :: Monitor -> IO (Maybe String)
getMonitorName mon = do
    p'name <- c'glfwGetMonitorName (toC mon)
    if p'name == nullPtr
      then return Nothing
      else Just `fmap` peekCString p'name

-- | This function sets the monitor configuration callback, or removes the
-- currently set callback. This is called when a monitor is connected to or
-- disconnected from the system.
--
-- * Bug:
--     X11: This callback is not yet called on monitor configuration
--     changes.
setMonitorCallback :: Maybe MonitorCallback -> IO ()
setMonitorCallback = setCallback
    mk'GLFWmonitorfun
    (\cb a0 a1 -> cb (fromC a0) (fromC a1))
    c'glfwSetMonitorCallback
    storedMonitorFun

getVideoModes :: Monitor -> IO (Maybe [VideoMode])
getVideoModes mon =
    alloca $ \p'n -> do
        p'vms <- c'glfwGetVideoModes (toC mon) p'n
        n <- fromC `fmap` peek p'n
        if p'vms == nullPtr || n <= 0
          then return Nothing
          else (Just . map fromC) `fmap` peekArray n p'vms

getVideoMode :: Monitor -> IO (Maybe VideoMode)
getVideoMode mon = do
    p'vm <- c'glfwGetVideoMode (toC mon)
    if p'vm == nullPtr
      then return Nothing
      else (Just . fromC) `fmap` peek p'vm

-- | This function generates a 256-element gamma ramp from the specified
--   exponent and then calls setGammaRamp with it.
--
setGamma :: Monitor -- ^ The monitor whose gamma ramp to set.
         -> Double  -- ^ The desired exponent.
         -> IO ()
setGamma mon e =
    c'glfwSetGamma (toC mon) (toC e)

getGammaRamp :: Monitor -> IO (Maybe GammaRamp)
getGammaRamp m = do
    p'ggr <- c'glfwGetGammaRamp (toC m)
    if p'ggr == nullPtr
      then return Nothing
      else do
          ggr <- peek p'ggr
          let p'rs = c'GLFWgammaramp'red   ggr
              p'gs = c'GLFWgammaramp'green ggr
              p'bs = c'GLFWgammaramp'blue  ggr
              cn   = c'GLFWgammaramp'size  ggr
              n    = fromC cn
          if n == 0 || nullPtr `elem` [p'rs, p'gs, p'bs]
            then return Nothing
            else do
                rs <- map fromC `fmap` peekArray n p'rs
                gs <- map fromC `fmap` peekArray n p'gs
                bs <- map fromC `fmap` peekArray n p'bs
                return $ Just GammaRamp
                  { gammaRampRed   = rs
                  , gammaRampGreen = gs
                  , gammaRampBlue  = bs
                  }

-- | This function sets the current gamma ramp for the specified monitor.
setGammaRamp :: Monitor -> GammaRamp -> IO ()
setGammaRamp mon gr =
    let rs = map toC $ gammaRampRed   gr :: [CUShort]
        gs = map toC $ gammaRampGreen gr :: [CUShort]
        bs = map toC $ gammaRampBlue  gr :: [CUShort]
        -- GammaRamp's smart constructor ensures that the RGB lists all have
        -- equal length, so just use the number of reds.
        cn = toC $ length rs :: CUInt
    in alloca       $ \p'ggr ->
       withArray rs $ \p'rs  ->
       withArray gs $ \p'gs  ->
       withArray bs $ \p'bs  -> do
          let ggr = C'GLFWgammaramp
                      { c'GLFWgammaramp'red   = p'rs
                      , c'GLFWgammaramp'green = p'gs
                      , c'GLFWgammaramp'blue  = p'bs
                      , c'GLFWgammaramp'size  = cn
                      }
          poke p'ggr ggr
          c'glfwSetGammaRamp (toC mon) p'ggr

--------------------------------------------------------------------------------
-- Window handling

defaultWindowHints :: IO ()
defaultWindowHints =
    c'glfwDefaultWindowHints

windowHint :: WindowHint -> IO ()
windowHint wh =
    let (t, v) = unpack
    in c'glfwWindowHint t v
  where
    unpack = case wh of
      (WindowHint'Resizable           x) -> (c'GLFW_RESIZABLE,             toC x)
      (WindowHint'Visible             x) -> (c'GLFW_VISIBLE,               toC x)
      (WindowHint'Decorated           x) -> (c'GLFW_DECORATED,             toC x)
      (WindowHint'RedBits             x) -> (c'GLFW_RED_BITS,              toC x)
      (WindowHint'GreenBits           x) -> (c'GLFW_GREEN_BITS,            toC x)
      (WindowHint'BlueBits            x) -> (c'GLFW_BLUE_BITS,             toC x)
      (WindowHint'AlphaBits           x) -> (c'GLFW_ALPHA_BITS,            toC x)
      (WindowHint'DepthBits           x) -> (c'GLFW_DEPTH_BITS,            toC x)
      (WindowHint'StencilBits         x) -> (c'GLFW_STENCIL_BITS,          toC x)
      (WindowHint'AccumRedBits        x) -> (c'GLFW_ACCUM_RED_BITS,        toC x)
      (WindowHint'AccumGreenBits      x) -> (c'GLFW_ACCUM_GREEN_BITS,      toC x)
      (WindowHint'AccumBlueBits       x) -> (c'GLFW_ACCUM_BLUE_BITS,       toC x)
      (WindowHint'AccumAlphaBits      x) -> (c'GLFW_ACCUM_ALPHA_BITS,      toC x)
      (WindowHint'AuxBuffers          x) -> (c'GLFW_AUX_BUFFERS,           toC x)
      (WindowHint'Samples             x) -> (c'GLFW_SAMPLES,               toC x)
      (WindowHint'RefreshRate         x) -> (c'GLFW_REFRESH_RATE,          toC x)
      (WindowHint'Stereo              x) -> (c'GLFW_STEREO,                toC x)
      (WindowHint'sRGBCapable         x) -> (c'GLFW_SRGB_CAPABLE,          toC x)
      (WindowHint'ClientAPI           x) -> (c'GLFW_CLIENT_API,            toC x)
      (WindowHint'ContextVersionMajor x) -> (c'GLFW_CONTEXT_VERSION_MAJOR, toC x)
      (WindowHint'ContextVersionMinor x) -> (c'GLFW_CONTEXT_VERSION_MINOR, toC x)
      (WindowHint'ContextRobustness   x) -> (c'GLFW_CONTEXT_ROBUSTNESS,    toC x)
      (WindowHint'OpenGLForwardCompat x) -> (c'GLFW_OPENGL_FORWARD_COMPAT, toC x)
      (WindowHint'OpenGLDebugContext  x) -> (c'GLFW_OPENGL_DEBUG_CONTEXT,  toC x)
      (WindowHint'OpenGLProfile       x) -> (c'GLFW_OPENGL_PROFILE,        toC x)

-- | This function creates a window and its associated context. Most of the
-- options controlling how the window and its context should be created are
-- specified through windowHint.
--
createWindow :: Int -- ^ Width(>0), in screen coordinates, of the window. 
             -> Int -- ^ Height(>0), in screen coordinates, of the window.
             -> String -- ^ The initial window title. 
             -> Maybe Monitor -- ^ The monitor to use for full screen mode, or Nothing to use windowed mode.
             -> Maybe Window -- ^ The window whose context to share resources with, or Nothing to not share resources. 
             -> IO (Maybe Window)
createWindow w h title mmon mwin =
    withCString title $ \ptitle -> do
        charFun             <- newIORef nullFunPtr
        cursorEnterFun      <- newIORef nullFunPtr
        cursorPosFun        <- newIORef nullFunPtr
        framebufferSizeFun  <- newIORef nullFunPtr
        keyFun              <- newIORef nullFunPtr
        mouseButtonFun      <- newIORef nullFunPtr
        scrollFun           <- newIORef nullFunPtr
        windowCloseFun      <- newIORef nullFunPtr
        windowFocusFun      <- newIORef nullFunPtr
        windowIconifyFun    <- newIORef nullFunPtr
        windowPosFun        <- newIORef nullFunPtr
        windowRefreshFun    <- newIORef nullFunPtr
        windowSizeFun       <- newIORef nullFunPtr
        let callbacks = WindowCallbacks
              { storedCharFun             = charFun
              , storedCursorEnterFun      = cursorEnterFun
              , storedCursorPosFun        = cursorPosFun
              , storedFramebufferSizeFun  = framebufferSizeFun
              , storedKeyFun              = keyFun
              , storedMouseButtonFun      = mouseButtonFun
              , storedScrollFun           = scrollFun
              , storedWindowCloseFun      = windowCloseFun
              , storedWindowFocusFun      = windowFocusFun
              , storedWindowIconifyFun    = windowIconifyFun
              , storedWindowPosFun        = windowPosFun
              , storedWindowRefreshFun    = windowRefreshFun
              , storedWindowSizeFun       = windowSizeFun
              }
        p'win <- c'glfwCreateWindow
          (toC w)
          (toC h)
          ptitle
          (maybe nullPtr toC mmon)
          (maybe nullPtr toC mwin)
        if p'win == nullPtr
          then return Nothing
          else do callbackPtr <- newStablePtr callbacks
                  c'glfwSetWindowUserPointer p'win (castStablePtrToPtr callbackPtr)
                  return $ Just $ fromC p'win

destroyWindow :: Window -> IO ()
destroyWindow win = do
    pcb <- castPtrToStablePtr `liftM` c'glfwGetWindowUserPointer (toC win)
    cbs <- deRefStablePtr pcb
    c'glfwDestroyWindow (toC win)

    let free callback = do funptr <- readIORef (callback cbs)
                           when (funptr /= nullFunPtr) $ freeHaskellFunPtr funptr
    free storedCharFun
    free storedCursorEnterFun
    free storedCursorPosFun
    free storedFramebufferSizeFun
    free storedKeyFun
    free storedMouseButtonFun
    free storedScrollFun
    free storedWindowCloseFun
    free storedWindowFocusFun
    free storedWindowIconifyFun
    free storedWindowPosFun
    free storedWindowRefreshFun
    free storedWindowSizeFun
    freeStablePtr pcb

-- | This function returns the value of the close flag of the specified
--   window.
--
windowShouldClose :: Window -- ^ The window to query. 
                  -> IO Bool -- ^ The value of the close flag.
windowShouldClose win =
    fromC `fmap` c'glfwWindowShouldClose (toC win)

-- | This function sets the value of the close flag of the specified
-- window. This can be used to override the user's attempt to close the
-- window, or to signal that it should be closed.
--
setWindowShouldClose :: Window -- ^ The window whose flag to change. 
                     -> Bool -- ^ The new value.
                     -> IO ()
setWindowShouldClose win b =
    c'glfwSetWindowShouldClose (toC win) (toC b)

setWindowTitle :: Window -> String -> IO ()
setWindowTitle win title =
    withCString title $ c'glfwSetWindowTitle (toC win)

getWindowPos :: Window -> IO (Int, Int)
getWindowPos win =
    allocaArray 2 $ \p -> do
        let p'x = p
            p'y = p `advancePtr` 1
        c'glfwGetWindowPos (toC win) p'x p'y
        x <- fromC `fmap` peek p'x
        y <- fromC `fmap` peek p'y
        return (x, y)

setWindowPos :: Window -> Int -> Int -> IO ()
setWindowPos win x y =
    c'glfwSetWindowPos (toC win) (toC x) (toC y)

getWindowSize :: Window -> IO (Int, Int)
getWindowSize win =
    allocaArray 2 $ \p -> do
        let p'w = p
            p'h = p `advancePtr` 1
        c'glfwGetWindowSize (toC win) p'w p'h
        w <- fromC `fmap` peek p'w
        h <- fromC `fmap` peek p'h
        return (w, h)

setWindowSize :: Window -> Int -> Int -> IO ()
setWindowSize win w h =
    c'glfwSetWindowSize (toC win) (toC w) (toC h)

getFramebufferSize :: Window -> IO (Int, Int)
getFramebufferSize win =
    allocaArray 2 $ \p -> do
        let p'w = p
            p'h = p `advancePtr` 1
        c'glfwGetFramebufferSize (toC win) p'w p'h
        w <- fromC `fmap` peek p'w
        h <- fromC `fmap` peek p'h
        return (w, h)

-- | This function iconifies/minimizes the specified window, if it was
-- previously restored. If it is a full screen window, the original monitor
-- resolution is restored until the window is restored. If the window is
-- already iconified, this function does nothing.
--
iconifyWindow :: Window -> IO ()
iconifyWindow =
    c'glfwIconifyWindow . toC

-- | This function restores the specified window, if it was previously
-- iconified/minimized. If it is a full screen window, the resolution
-- chosen for the window is restored on the selected monitor. If the window
-- is already restored, this function does nothing.
--
restoreWindow :: Window -> IO ()
restoreWindow =
    c'glfwRestoreWindow . toC

-- | This function makes the specified window visible, if it was previously
-- hidden. If the window is already visible or is in full screen mode, this
-- function does nothing.
--
showWindow :: Window -> IO ()
showWindow =
    c'glfwShowWindow . toC

-- | This function hides the specified window, if it was previously
-- visible. If the window is already hidden or is in full screen mode, this
-- function does nothing.
--
hideWindow :: Window -> IO ()
hideWindow =
    c'glfwHideWindow . toC

-- | This function returns the handle of the monitor that the specified
-- window is in full screen on.
getWindowMonitor :: Window -> IO (Maybe Monitor)
getWindowMonitor win = do
    p'mon <- c'glfwGetWindowMonitor (toC win)
    return $ if p'mon == nullPtr
      then Nothing
      else Just $ fromC p'mon

-- | This function sets the position, in screen coordinates, of the cursor
-- relative to the upper-left corner of the client area of the specified
-- window. The window must be focused. If the window does not have focus
-- when this function is called, it fails silently.
--
-- If the cursor is disabled (with GLFW_CURSOR_DISABLED) then the cursor
-- position is unbounded and limited only by the minimum and maximum values
-- of a double.
--
setCursorPos :: Window -- ^ The desired window. 
             -> Double -- ^ x,relative to the left edge of the client area.
             -> Double -- ^ y,relative to the top edge of the client area.
             -> IO ()
setCursorPos win x y =
    c'glfwSetCursorPos (toC win) (toC x) (toC y)

-- start of functions related to c'glfwGetWindowAttrib

getWindowFocused :: Window -> IO FocusState
getWindowFocused win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_FOCUSED

getWindowIconified :: Window -> IO IconifyState
getWindowIconified win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_ICONIFIED

getWindowResizable :: Window -> IO Bool
getWindowResizable win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_RESIZABLE

getWindowDecorated :: Window -> IO Bool
getWindowDecorated win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_DECORATED

getWindowVisible :: Window -> IO Bool
getWindowVisible win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_VISIBLE

getWindowClientAPI :: Window -> IO ClientAPI
getWindowClientAPI win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_CLIENT_API

getWindowContextVersionMajor :: Window -> IO Int
getWindowContextVersionMajor win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_CONTEXT_VERSION_MAJOR

getWindowContextVersionMinor :: Window -> IO Int
getWindowContextVersionMinor win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_CONTEXT_VERSION_MINOR

getWindowContextVersionRevision :: Window -> IO Int
getWindowContextVersionRevision win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_CONTEXT_REVISION

getWindowContextRobustness :: Window -> IO ContextRobustness
getWindowContextRobustness win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_CONTEXT_ROBUSTNESS

getWindowOpenGLForwardCompat :: Window -> IO Bool
getWindowOpenGLForwardCompat win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_OPENGL_FORWARD_COMPAT

getWindowOpenGLDebugContext :: Window -> IO Bool
getWindowOpenGLDebugContext win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_OPENGL_DEBUG_CONTEXT

getWindowOpenGLProfile :: Window -> IO OpenGLProfile
getWindowOpenGLProfile win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_OPENGL_PROFILE

-- end of functions related to c'glfwGetWindowAttrib

setWindowPosCallback :: Window -> Maybe WindowPosCallback -> IO ()
setWindowPosCallback win = setWindowCallback
    mk'GLFWwindowposfun
    (\cb a0 a1 a2 ->
      cb (fromC a0) (fromC a1) (fromC a2))
    (c'glfwSetWindowPosCallback (toC win))
    storedWindowPosFun
    win

setWindowSizeCallback :: Window -> Maybe WindowSizeCallback -> IO ()
setWindowSizeCallback win = setWindowCallback
    mk'GLFWwindowsizefun
    (\cb a0 a1 a2 ->
      cb (fromC a0) (fromC a1) (fromC a2))
    (c'glfwSetWindowSizeCallback (toC win))
    storedWindowSizeFun
    win

setWindowCloseCallback :: Window -> Maybe WindowCloseCallback -> IO ()
setWindowCloseCallback win = setWindowCallback
    mk'GLFWwindowclosefun
    (. fromC)
    (c'glfwSetWindowCloseCallback (toC win))
    storedWindowCloseFun
    win

setWindowRefreshCallback :: Window -> Maybe WindowRefreshCallback -> IO ()
setWindowRefreshCallback win = setWindowCallback
    mk'GLFWwindowrefreshfun
    (. fromC)
    (c'glfwSetWindowRefreshCallback (toC win))
    storedWindowRefreshFun
    win

setWindowFocusCallback :: Window -> Maybe WindowFocusCallback -> IO ()
setWindowFocusCallback win = setWindowCallback
    mk'GLFWwindowfocusfun
    (\cb a0 a1 -> cb (fromC a0) (fromC a1))
    (c'glfwSetWindowFocusCallback (toC win))
    storedWindowFocusFun
    win

setWindowIconifyCallback :: Window -> Maybe WindowIconifyCallback -> IO ()
setWindowIconifyCallback win = setWindowCallback
    mk'GLFWwindowiconifyfun
    (\cb a0 a1 -> cb (fromC a0) (fromC a1))
    (c'glfwSetWindowIconifyCallback (toC win))
    storedWindowIconifyFun
    win

setFramebufferSizeCallback :: Window -> Maybe FramebufferSizeCallback -> IO ()
setFramebufferSizeCallback win = setWindowCallback
    mk'GLFWframebuffersizefun
    (\cb a0 a1 a2 -> cb (fromC a0) (fromC a1) (fromC a2))
    (c'glfwSetFramebufferSizeCallback (toC win))
    storedFramebufferSizeFun
    win

-- |This function processes only those events that have already been
--  received and then returns immediately. Processing events will cause the
--  window and input callbacks associated with those events to be called.
--
--  This function is not required for joystick input to work.
--
--  * New in GLFW 3 : 
--     This function is no longer called by 'swapBuffers'. You need to
--     call it or waitEvents yourself.
--
pollEvents :: IO ()
pollEvents = c'glfwPollEvents

-- | This function puts the calling thread to sleep until at least one
-- event has been received. Once one or more events have been received, it
-- behaves as if glfwPollEvents was called, i.e. the events are processed
-- and the function then returns immediately. Processing events will cause
-- the window and input callbacks associated with those events to be
-- called.
--
-- Since not all events are associated with callbacks, this function may
-- return without a callback having been called even if you are monitoring
-- all callbacks.
--
-- This function is not required for joystick input to work.
--
-- * Note : 
--     This function may only be called from the main thread.
--     This function may not be called from a callback.
--     On some platforms, certain callbacks may be called outside
--     of a call to one of the event processing functions.
--
waitEvents :: IO ()
waitEvents = c'glfwWaitEvents

--------------------------------------------------------------------------------
-- Input handling

-- start of glfw{GS}etInputMode-related functions

-- | This function set the 'CursorInputMode'.
--
-- * GLFW_CURSOR_NORMAL makes the cursor visible and behaving normally.
--
-- * GLFW_CURSOR_HIDDEN makes the cursor invisible when it is over
--   the client area of the window.
--
-- * GLFW_CURSOR_DISABLED disables the cursor and removes any limitations
--   on cursor movement.
--
getCursorInputMode :: Window -> IO CursorInputMode
getCursorInputMode win =
    fromC `fmap` c'glfwGetInputMode (toC win) c'GLFW_CURSOR

setCursorInputMode :: Window -> CursorInputMode -> IO ()
setCursorInputMode win c =
    c'glfwSetInputMode (toC win) c'GLFW_CURSOR (toC c)

-- | This function returns the 'StickyKeysInputMode'.
getStickyKeysInputMode :: Window -> IO StickyKeysInputMode
getStickyKeysInputMode win =
    fromC `fmap` c'glfwGetInputMode (toC win) c'GLFW_STICKY_KEYS

-- | If sticky keys are enabled, a key press will ensure that 'getKey'
-- returns 'KeyState'Pressed' the next time it is called even if the
-- key had been released before the call. This is useful when you are
-- only interested in whether keys have been pressed but not when or
-- in which order.
--
setStickyKeysInputMode :: Window -> StickyKeysInputMode -> IO ()
setStickyKeysInputMode win sk =
    c'glfwSetInputMode (toC win) c'GLFW_STICKY_KEYS (toC sk)

-- | This function returns the 'StickyMouseButtonsInputMode'.
getStickyMouseButtonsInputMode :: Window -> IO StickyMouseButtonsInputMode
getStickyMouseButtonsInputMode win =
    fromC `fmap` c'glfwGetInputMode (toC win) c'GLFW_STICKY_MOUSE_BUTTONS

-- | If sticky mouse buttons are enabled, a mouse button press will
-- ensure that 'getMouseButton' returns 'MouseButtonState'Pressed' the
-- next time it is called even if the mouse button had been released
-- before the call. This is useful when you are only interested in
-- whether mouse buttons have been pressed but not when or in which
-- order.
--
setStickyMouseButtonsInputMode :: Window -> StickyMouseButtonsInputMode -> IO ()
setStickyMouseButtonsInputMode win smb =
    c'glfwSetInputMode (toC win) c'GLFW_STICKY_MOUSE_BUTTONS (toC smb)

-- end of glfw{GS}etInputMode-related functions

-- | This function returns the last state reported for the specified key to
-- the specified window. The returned state is one of GLFW_PRESS or
-- GLFW_RELEASE. The higher-level state GLFW_REPEAT is only reported to the
-- key callback.
--
-- If the GLFW_STICKY_KEYS input mode is enabled, this function returns
-- GLFW_PRESS the first time you call this function after a key has been
-- pressed, even if the key has already been released.
--
-- The key functions deal with physical keys, with key tokens named after
-- their use on the standard US keyboard layout. If you want to input text,
-- use the Unicode character callback instead.
--
getKey :: Window -> Key -> IO KeyState
getKey win k =
    fromC `fmap` c'glfwGetKey (toC win) (toC k)

-- | This function returns the last state reported for the specified mouse
-- button to the specified window.
--
-- If the GLFW_STICKY_MOUSE_BUTTONS input mode is enabled, this function
-- returns GLFW_PRESS the first time you call this function after a mouse
-- button has been pressed, even if the mouse button has already been
-- released.
--
getMouseButton :: Window -> MouseButton -> IO MouseButtonState
getMouseButton win b =
    fromC `fmap` c'glfwGetMouseButton (toC win) (toC b)

-- | This function returns the last reported position of the cursor, in
-- screen coordinates, relative to the upper-left corner of the client area
-- of the specified window.
--
-- If the cursor is disabled (with GLFW_CURSOR_DISABLED) then the cursor
-- position is unbounded and limited only by the minimum and maximum values
-- of a double.
--
-- The coordinate can be converted to their integer equivalents with the
-- floor function. Casting directly to an integer type works for positive
-- coordinates, but fails for negative ones.
--
getCursorPos :: Window -> IO (Double, Double)
getCursorPos win =
    allocaArray 2 $ \p -> do
        let p'x = p
            p'y = p `advancePtr` 1
        c'glfwGetCursorPos (toC win) p'x p'y
        x <- fromC `fmap` peek p'x
        y <- fromC `fmap` peek p'y
        return (x, y)

-- | This function sets the key callback of the specific window, which is
-- called when a key is pressed, repeated or released.
--
-- The key functions deal with physical keys, with layout independent key
-- tokens named after their values in the standard US keyboard layout. If
-- you want to input text, use the character callback instead.
--
-- When a window loses focus, it will generate synthetic key release events
-- for all pressed keys. You can tell these events from user-generated
-- events by the fact that the synthetic ones are generated after the
-- window has lost focus, i.e. GLFW_FOCUSED will be false and the focus
-- callback will have already been called.
--
-- The scancode of a key is specific to that platform or sometimes even to
-- that machine. Scancodes are intended to allow users to bind keys that
-- don't have a GLFW key token. Such keys have key set to GLFW_KEY_UNKNOWN,
-- their state is not saved and so it cannot be retrieved with 'getKey'.
--
-- Sometimes GLFW needs to generate synthetic key events, in which case the
-- scancode may be zero.
--
setKeyCallback :: Window -> Maybe KeyCallback -> IO ()
setKeyCallback win = setWindowCallback
    mk'GLFWkeyfun
    (\cb a0 a1 a2 a3 a4 ->
      cb (fromC a0) (fromC a1) (fromC a2) (fromC a3) (fromC a4))
    (c'glfwSetKeyCallback (toC win))
    storedKeyFun
    win

-- | This function sets the character callback of the specific window,
-- which is called when a Unicode character is input.
--
-- The character callback is intended for text input. If you want to know
-- whether a specific key was pressed or released, use the key callback
-- instead.
--
setCharCallback :: Window -> Maybe CharCallback -> IO ()
setCharCallback win = setWindowCallback
    mk'GLFWcharfun
    (\cb a0 a1 -> cb (fromC a0) (fromC a1))
    (c'glfwSetCharCallback (toC win))
    storedCharFun
    win

-- | This function sets the mouse button callback of the specified window,
-- which is called when a mouse button is pressed or released.
--
-- When a window loses focus, it will generate synthetic mouse button
-- release events for all pressed mouse buttons. You can tell these events
-- from user-generated events by the fact that the synthetic ones are
-- generated after the window has lost focus, i.e. GLFW_FOCUSED will be
-- false and the focus callback will have already been called.
--
setMouseButtonCallback :: Window -> Maybe MouseButtonCallback -> IO ()
setMouseButtonCallback win = setWindowCallback
    mk'GLFWmousebuttonfun
    (\cb a0 a1 a2 a3 -> cb (fromC a0) (fromC a1) (fromC a2) (fromC a3))
    (c'glfwSetMouseButtonCallback (toC win))
    storedMouseButtonFun
    win

-- | This function sets the cursor position callback of the specified
-- window, which is called when the cursor is moved. The callback is
-- provided with the position, in screen coordinates, relative to the
-- upper-left corner of the client area of the window.
--
setCursorPosCallback :: Window -> Maybe CursorPosCallback -> IO ()
setCursorPosCallback win = setWindowCallback
    mk'GLFWcursorposfun
    (\cb a0 a1 a2 -> cb (fromC a0) (fromC a1) (fromC a2))
    (c'glfwSetCursorPosCallback (toC win))
    storedCursorPosFun
    win

-- | This function sets the cursor boundary crossing callback of the
-- specified window, which is called when the cursor enters or leaves the
-- client area of the window.
--
setCursorEnterCallback :: Window -> Maybe CursorEnterCallback -> IO ()
setCursorEnterCallback win = setWindowCallback
    mk'GLFWcursorenterfun
    (\cb a0 a1 -> cb (fromC a0) (fromC a1))
    (c'glfwSetCursorEnterCallback (toC win))
    storedCursorEnterFun
    win

-- | This function sets the scroll callback of the specified window, which
-- is called when a scrolling device is used, such as a mouse wheel or
-- scrolling area of a touchpad.
--
-- The scroll callback receives all scrolling input, like that from a mouse
-- wheel or a touchpad scrolling area.
--
setScrollCallback :: Window -> Maybe ScrollCallback -> IO ()
setScrollCallback win = setWindowCallback
    mk'GLFWscrollfun
    (\cb a0 a1 a2 -> cb (fromC a0) (fromC a1) (fromC a2))
    (c'glfwSetScrollCallback (toC win))
    storedScrollFun
    win

-- | This function returns whether the specified joystick is present.
--
joystickPresent :: Joystick
                -> IO Bool -- ^ True if the joystick is present, or False otherwise. 
joystickPresent js =
    fromC `fmap` c'glfwJoystickPresent (toC js)

-- | This function returns the values of all axes of the specified
-- joystick.
--
getJoystickAxes :: Joystick -> IO (Maybe [Double])
getJoystickAxes js =
    alloca $ \p'n -> do
        p'axes <- c'glfwGetJoystickAxes (toC js) p'n
        n <- fromC `fmap` peek p'n
        if p'axes == nullPtr || n <= 0
          then return Nothing
          else (Just . map fromC) `fmap` peekArray n p'axes

-- | This function returns the state of all buttons of the specified
-- joystick.
--
getJoystickButtons :: Joystick -> IO (Maybe [JoystickButtonState])
getJoystickButtons js =
    alloca $ \p'n -> do
        p'buttons <- c'glfwGetJoystickButtons (toC js) p'n
        n <- fromC `fmap` peek p'n
        if p'buttons == nullPtr || n <= 0
          then return Nothing
          else (Just . map fromC) `fmap` peekArray n p'buttons

-- | This function returns the name, encoded as UTF-8, of the specified
-- joystick.
--
getJoystickName :: Joystick -> IO (Maybe String)
getJoystickName js = do
    p'name <- c'glfwGetJoystickName (toC js)
    if p'name == nullPtr
      then return Nothing
      else Just `fmap` peekCString p'name

--------------------------------------------------------------------------------
-- Time

-- | This function returns the value of the GLFW timer. Unless the timer
--   has been set using setTime, the timer measures time elapsed since
--   GLFW was initialized.
--
-- * Note : 
--       The resolution of the timer is system dependent, but is usually on
--       the order of a few micro- or nanoseconds. It uses the
--       highest-resolution monotonic time source on each supported
--       platform. 
--
getTime :: IO (Maybe Double) -- ^ The current value, in seconds, or zero if an error occurred.
getTime = do
    t <- fromC `fmap` c'glfwGetTime
    return $ if t == 0
      then Nothing
      else Just t

-- | This function sets the value of the GLFW timer. It then continues to
--   count up from that value.
--
setTime :: Double -- ^ The new value, in seconds.
        -> IO ()
setTime =
    c'glfwSetTime . toC

--------------------------------------------------------------------------------
-- Context

-- | This function makes the context of the specified window current on the
-- calling thread. A context can only be made current on a single thread at
-- a time and each thread can have only a single current context at a time.
--
makeContextCurrent :: Maybe Window -- ^ The window whose context to make current, or Nothing to detach the current context.
                   -> IO ()
makeContextCurrent =
    c'glfwMakeContextCurrent . maybe nullPtr toC

-- | This function returns the window whose context is current on the
-- calling thread.
--
getCurrentContext :: IO (Maybe Window) -- ^ The window whose context is current, or Nothing if no window's context is current.
getCurrentContext = do
    p'win <- c'glfwGetCurrentContext
    return $ if p'win == nullPtr
      then Nothing
      else Just $ fromC p'win

-- | This function swaps the front and back buffers of the specified
--   window. If the swap interval is greater than zero, the GPU driver waits
--   the specified number of screen updates before swapping the buffers.
--
-- * New in GLFW 3
--    This function no longer calls 'pollEvents'. You need to call it or
--    'waitEvents' yourself.
--
swapBuffers :: Window -- ^ The window whose buffers to swap.
            -> IO ()
swapBuffers =
    c'glfwSwapBuffers . toC

-- | This function sets the swap interval for the current context, i.e. the
--   number of screen updates to wait before swapping the buffers of a window
--   and returning from 'swapBuffers'. This is sometimes called 'vertical
--   synchronization', 'vertical retrace synchronization' or 'vsync'.
--
--   Contexts that support either of the WGL_EXT_swap_control_tear and
--   GLX_EXT_swap_control_tear extensions also accept negative swap
--   intervals, which allow the driver to swap even if a frame arrives
--   a little bit late. You can check for the presence of these extensions
--   using 'extensionSupported'. For more information about swap tearing,
--   see the extension specifications.
--
-- * Note : 
--    This function is not called during window creation, leaving the swap
--    interval set to whatever is the default on that platform. This is
--    done because some swap interval extensions used by GLFW do not allow
--    the swap interval to be reset to zero once it has been set to
--    a non-zero value.
--    Some GPU drivers do not honor the requested swap interval, either
--    because of user settings that override the request or due to bugs
--    in the driver.
--
swapInterval :: Int -- ^ The minimum number of screen updates to wait for until the buffers are swapped by 'swapBuffers'.
             -> IO ()
swapInterval =
    c'glfwSwapInterval . toC

-- | This function returns whether the specified OpenGL or context creation
-- API extension is supported by the current context. For example, on
-- Windows both the OpenGL and WGL extension strings are checked.
--
-- * Note :
--    As this functions searches one or more extension strings on each
--    call, it is recommended that you cache its results if it's going to
--    be used frequently. The extension strings will not change during the
--    lifetime of a context, so there is no danger in doing this. 
--
extensionSupported :: String -- ^ name of the extension.
                   -> IO Bool
extensionSupported ext =
    withCString ext $ \p'ext ->
      fromC `fmap` c'glfwExtensionSupported p'ext

--------------------------------------------------------------------------------
-- Clipboard

-- | This function sets the system clipboard to the specified, UTF-8
-- encoded string. The string is copied before returning, so you don't have
-- to retain it afterwards.
--
setClipboardString :: Window -- ^ The window that will own the clipboard contents. 
                   -> String
                   -> IO ()
setClipboardString win s =
    withCString s (c'glfwSetClipboardString (toC win))

-- | This function returns the contents of the system clipboard, if it
-- contains or is convertible to a UTF-8 encoded string.
--
-- * Note
--   This function may only be called from the main thread.
--   The returned string is allocated and freed by GLFW. You should
--   not free it yourself.
--   The returned string is valid only until the next call to
--   'getClipboardString' or 'setClipboardString'.
--
getClipboardString :: Window -- ^ The window that will request the clipboard contents. 
                   -> IO (Maybe String)
getClipboardString win = do
    p's <- c'glfwGetClipboardString (toC win)
    if p's == nullPtr
      then return Nothing
      else Just `fmap` peekCString p's
