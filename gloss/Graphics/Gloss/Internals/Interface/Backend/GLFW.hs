{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}

-- | Support for using GLFW as the window manager backend.
module Graphics.Gloss.Internals.Interface.Backend.GLFW
        (GLFWState)
where
import Data.IORef
import Data.Char                           (toLower)
import Data.Maybe
import Control.Monad
import Graphics.Gloss.Data.Display
import qualified Graphics.UI.GLFW          as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Control.Exception         as X
import qualified Control.Concurrent        as CON

-- [Note: FreeGlut]
-- ~~~~~~~~~~~~~~~~
-- We use GLUT for font rendering.
--   On freeglut-based installations (usually linux) we need to explicitly
--   initialize GLUT before we can use any of it's functions.
--
---  We also need to deinitialize (exit) GLUT when we close the GLFW
--   window, otherwise opening a gloss window again from GHCi will crash. 
--   For the OS X and Windows version of GLUT there are no such restrictions.
--
--   We assume also assume that only linux installations use freeglut.
--
#ifdef linux_HOST_OS
import qualified Graphics.UI.GLUT          as GLUT
#endif

import Graphics.Gloss.Internals.Interface.Backend.Types

-- | State of the GLFW backend library.
data GLFWState
        = GLFWState
        { -- | Status of Ctrl, Alt or Shift (Up or Down?)
          modifiers     :: Modifiers

        -- | Latest mouse position
        , mousePosition :: (Int,Int)

        -- | Does the screen need to be redrawn?
        , dirtyScreen   :: Bool

        -- | Action that draws on the screen
        , display       :: IO ()

        -- | Action perforrmed when idling
        , idle          :: IO ()

        -- | The window for the GLFWState uses, if window = Nothing there is no window
        , window        :: Maybe GLFW.Window
        }


-- | Initial GLFW state.
glfwStateInit :: GLFWState
glfwStateInit
        = GLFWState
        { modifiers      = Modifiers Up Up Up
        , mousePosition = (0, 0)
        , dirtyScreen   = True
        , display       = return ()
        , idle          = return ()
        , window        = Nothing }



instance Backend GLFWState where
        initBackendState           = glfwStateInit
        initializeBackend          = initializeGLFW
        exitBackend                = exitGLFW
        openWindow                 = openWindowGLFW
        dumpBackendState           = dumpStateGLFW
        installDisplayCallback     = installDisplayCallbackGLFW
        installWindowCloseCallback = installWindowCloseCallbackGLFW
        installReshapeCallback     = installReshapeCallbackGLFW
        installKeyMouseCallback    = installKeyMouseCallbackGLFW
        installMotionCallback      = installMotionCallbackGLFW
        installIdleCallback        = installIdleCallbackGLFW
        runMainLoop                = runMainLoopGLFW
        postRedisplay              = postRedisplayGLFW
        getWindowDimensions        = getWindowSize
        elapsedTime                = (\_     -> fmap fromJust GLFW.getTime)
        sleep                      = (\_ sec -> newSleep sec)

-- Pretty simple sleep function (only works on ghc).
-- This is needed because GLFW removed the sleep function.
newSleep :: Double -> IO ()
newSleep = CON.threadDelay . floor . (* 1000)

-- Get Window Size
-- | Returns the size of the window assiociated with the GLFWState.
getWindowSize :: IORef GLFWState -> IO (Int, Int)
getWindowSize stateRef = do 
    win <- fmap window $ readIORef stateRef
    case win of
        Just w -> GLFW.getWindowSize w
        Nothing -> return (0,0)


-- Initialise -----------------------------------------------------------------
-- | Initialise the GLFW backend.
initializeGLFW :: IORef GLFWState -> Bool-> IO ()
initializeGLFW _ debug
 = do
        _                   <- GLFW.init
        glfwVersion         <- GLFW.getVersion

#ifdef linux_HOST_OS
-- See [Note: FreeGlut] for why we need this.
        (_progName, _args)  <- GLUT.getArgsAndInitialize
#endif

        when debug
         $ putStr  $ "  glfwVersion        = " ++ show glfwVersion   ++ "\n"


-- Exit -----------------------------------------------------------------------
-- | Tell the GLFW backend to close the window and exit.
exitGLFW :: IORef GLFWState -> IO ()
exitGLFW stateRef
 = do
#ifdef linux_HOST_OS
-- See [Note: FreeGlut] on why we exit GLUT for Linux
        GLUT.exit
#endif
        win <- fmap window $ readIORef stateRef
        case win of
            Just w -> do GLFW.setWindowShouldClose w True
                         GLFW.destroyWindow w
                         modifyIORef stateRef (\s -> s { window = Nothing })
            Nothing -> return ()


-- Open Window ----------------------------------------------------------------
-- | Open a new window.
openWindowGLFW 
        :: IORef GLFWState
        -> Display
        -> IO ()

openWindowGLFW stateRef (InWindow title (sizeX, sizeY) pos)
 = do win <- GLFW.createWindow sizeX sizeY title Nothing Nothing
      GLFW.makeContextCurrent win

      case win of
        Just w  -> do uncurry (GLFW.setWindowPos w) pos
                      -- Try to enable sync-to-vertical-refresh by setting the number 
                      -- of buffer swaps per vertical refresh to 1.
                      GLFW.swapInterval 1
        Nothing -> return ()
      modifyIORef stateRef (\s -> s { window = win })

openWindowGLFW stateRef (FullScreen (sizeX, sizeY))
 = do   primaryMonitor <- GLFW.getPrimaryMonitor
        win <- GLFW.createWindow sizeX sizeY "" primaryMonitor Nothing
        
        GLFW.makeContextCurrent win -- It seems you have to make the window's context current for fullscreen

        -- Try to enable sync-to-vertical-refresh by setting the number 
        -- of buffer swaps per vertical refresh to 1.
        GLFW.swapInterval 1
        modifyIORef stateRef (\s -> s { window = win })

-- Dump State -----------------------------------------------------------------
-- | Print out the internal GLFW state.
dumpStateGLFW :: IORef GLFWState -> IO ()
dumpStateGLFW stateRef
 = do   win <- fmap window $ readIORef stateRef
        case win of
            Just w -> do (ww,wh)     <- GLFW.getWindowSize w

                         monitor     <- fmap fromJust $ GLFW.getWindowMonitor w
                         mode        <- fmap fromJust $ GLFW.getVideoMode monitor

                         let r       = GLFW.videoModeRedBits mode
                             g       = GLFW.videoModeGreenBits mode
                             b       = GLFW.videoModeBlueBits mode
                         let rgbaBD  = [r,g,b]      

                         putStr  $ "* dumpGlfwState\n"
                                 ++ " windowWidth  = " ++ show ww          ++ "\n"
                                 ++ " windowHeight = " ++ show wh          ++ "\n"
                                 ++ " depth rgb    = " ++ show rgbaBD      ++ "\n"
                                 ++ "\n"
            Nothing -> putStr "Can't dump a window that does not exist."


-- Display Callback -----------------------------------------------------------
-- | Callback for when GLFW needs us to redraw the contents of the window.
installDisplayCallbackGLFW
        :: IORef GLFWState -> [Callback] -> IO ()

installDisplayCallbackGLFW stateRef callbacks
 =  modifyIORef stateRef
        $ \s -> s { display = callbackDisplay stateRef callbacks }


callbackDisplay
        :: IORef GLFWState -> [Callback]
        -> IO ()

callbackDisplay stateRef callbacks
 = do  -- clear the display
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)

        -- get the display callbacks from the chain
        let funs  = [f stateRef | (Display f) <- callbacks]
        sequence_ funs

        return ()


-- Close Callback -------------------------------------------------------------
-- | Callback for when the user closes the window.
--   We can do some cleanup here.
installWindowCloseCallbackGLFW 
        :: IORef GLFWState -> IO ()

installWindowCloseCallbackGLFW stateRef
 = do win <- fmap window $ readIORef stateRef
      case win of 
        Just w -> GLFW.setWindowCloseCallback w $
                    Just $ \_ -> do
#ifdef linux_HOST_OS
-- See [Note: FreeGlut] for why we need this.
                            GLUT.exit
#endif
                            return ()
        Nothing -> return ()


-- Reshape --------------------------------------------------------------------
-- | Callback for when the user reshapes the window.
installReshapeCallbackGLFW
        :: IORef GLFWState -> [Callback] -> IO ()

installReshapeCallbackGLFW stateRef callbacks
        = do win <- fmap window $ readIORef stateRef
             case win of
                Just w -> GLFW.setWindowSizeCallback w (Just $ callbackReshape stateRef callbacks)
                Nothing -> return ()

callbackReshape 
        :: Backend a
        => IORef a -> [Callback]
        -> GLFW.Window
        -> Int -> Int
        -> IO ()

callbackReshape glfwState callbacks _ sizeX sizeY
  = sequence_
  $ map   (\f -> f (sizeX, sizeY))
    [f glfwState | Reshape f  <- callbacks]


-- KeyMouse -----------------------------------------------------------------------
-- | Callbacks for when the user presses a key or moves / clicks the mouse.
--   This is a bit verbose because we have to do impedence matching between
--   GLFW's event system, and the one use by Gloss which was originally
--   based on GLUT. The main problem is that GLUT only provides a single callback
--   slot for character keys, arrow keys, mouse buttons and mouse wheel movement, 
--   while GLFW provides a single slot for each.
--
installKeyMouseCallbackGLFW
        :: IORef GLFWState -> [Callback]
        -> IO ()

installKeyMouseCallbackGLFW stateRef callbacks
 = do   win <- fmap window $ readIORef stateRef
        case win of
            Just w -> do GLFW.setKeyCallback         w $ (Just . const $ callbackKeyboard    stateRef callbacks)
                         GLFW.setCharCallback        w $ (Just . const $ callbackChar        stateRef callbacks)
                         GLFW.setMouseButtonCallback w $ (Just . const $ callbackMouseButton stateRef callbacks)
                         GLFW.setScrollCallback      w $ (Just . const $ callbackMouseWheel  stateRef callbacks)
            Nothing -> return ()


-- GLFW calls this on a non-character keyboard action.
callbackKeyboard 
        :: IORef GLFWState -> [Callback]
        -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys
        -> IO ()

callbackKeyboard stateRef callbacks key _ keystate _
 = do   
        let ks = if keystate == GLFW.KeyState'Pressed then True else False
        (modsSet, GLFWState mods pos _ _ _ _ _)
                <- setModifiers stateRef key ks     
        let key'      = fromGLFW key
        let keystate' = if ks then Down else Up

        -- Call the Gloss KeyMouse actions with the new state.
        unless (modsSet || keystate == GLFW.KeyState'Repeating)
            $ sequence_ 
            $ map  (\f -> f key' keystate' mods pos) 
            [f stateRef | KeyMouse f <- callbacks]


setModifiers 
        :: IORef GLFWState
        -> GLFW.Key -> Bool
        -> IO (Bool, GLFWState)

setModifiers stateRef key pressed
 = do   glfwState <- readIORef stateRef
        let mods  = modifiers glfwState
        let mods' = case key of
                GLFW.Key'LeftShift -> mods {shift = if pressed then Down else Up}
                GLFW.Key'LeftControl  -> mods {ctrl  = if pressed then Down else Up}
                GLFW.Key'LeftAlt   -> mods {alt   = if pressed then Down else Up}
                _                 -> mods

        if (mods' /= mods)
         then do
                let glfwState' = glfwState {modifiers = mods'}
                writeIORef stateRef glfwState'
                return (True, glfwState')
         else return (False, glfwState)


-- GLFW calls this on a when the user presses or releases a character key.
callbackChar 
        :: IORef GLFWState -> [Callback]
        -> Char -> IO ()

callbackChar _ _ _
 = return ()


-- GLFW calls on this when the user clicks or releases a mouse button.
callbackMouseButton 
        :: IORef GLFWState -> [Callback]
        -> GLFW.MouseButton
        -> GLFW.MouseButtonState
        -> GLFW.ModifierKeys
        -> IO ()

callbackMouseButton stateRef callbacks key keystate _
 = do   (GLFWState mods pos _ _ _ _ _) <- readIORef stateRef
        let key'      = fromGLFW key
        let keystate' = if keystate == GLFW.MouseButtonState'Pressed then Down else Up

        -- Call all the Gloss KeyMouse actions with the new state.
        sequence_ 
         $ map  (\f -> f key' keystate' mods pos)
                [f stateRef | KeyMouse f <- callbacks]


-- GLFW calls on this when the user moves the mouse wheel.
callbackMouseWheel
        :: IORef GLFWState -> [Callback]
        -> Double -> Double
        -> IO ()

callbackMouseWheel stateRef callbacks _ y
 = do   (key, keystate)  <- setMouseWheel stateRef y
        (GLFWState mods pos _ _ _ _ _) <- readIORef stateRef

        -- Call all the Gloss KeyMouse actions with the new state.
        sequence_ 
         $ map  (\f -> f key keystate mods pos)
                [f stateRef | KeyMouse f <- callbacks]

setMouseWheel
        :: IORef GLFWState
        -> Double
        -> IO (Key, KeyState)

setMouseWheel _ w
 = case compare w 0 of
        LT -> return (MouseButton WheelDown , Down)
        GT -> return (MouseButton WheelUp   , Down)
        EQ -> return (SpecialKey  KeyUnknown, Up  )


-- Motion Callback ------------------------------------------------------------
-- | Callback for when the user moves the mouse.
installMotionCallbackGLFW 
        :: IORef GLFWState -> [Callback]
        -> IO ()

installMotionCallbackGLFW stateRef callbacks
        = do win <- fmap window $ readIORef stateRef
             case win of
                Just w -> GLFW.setCursorPosCallback w $ (Just . const $ callbackMotion stateRef callbacks)
                Nothing -> return ()

callbackMotion 
        :: IORef GLFWState -> [Callback]
        -> Double -> Double
        -> IO ()
callbackMotion stateRef callbacks x y
 = do   pos <- setMousePos stateRef (floor x) (floor y)
        -- Call all the Gloss Motion actions with the new state.
        sequence_ 
         $ map  (\f -> f pos)
                [f stateRef | Motion f <- callbacks]

setMousePos
        :: IORef GLFWState
        -> Int -> Int
        -> IO (Int, Int)
setMousePos stateRef x y
 = do   let pos = (x,y)
        modifyIORef stateRef (\s -> s {mousePosition = pos})
        return pos


-- Idle Callback --------------------------------------------------------------
-- | Callback for when GLFW has finished its jobs and it's time for us to do
--   something for our application.
installIdleCallbackGLFW
        :: IORef GLFWState -> [Callback]
        -> IO ()

installIdleCallbackGLFW stateRef callbacks 
        = modifyIORef stateRef (\s -> s {idle = callbackIdle stateRef callbacks})

callbackIdle 
        :: IORef GLFWState -> [Callback]
        -> IO ()

callbackIdle stateRef callbacks
        = sequence_
        $ [f stateRef | Idle f <- callbacks]


-- Main Loop ------------------------------------------------------------------
runMainLoopGLFW
        :: IORef GLFWState
        -> IO ()

runMainLoopGLFW stateRef 
 = X.catch go exit
 where
  exit :: X.SomeException -> IO ()
  exit e = print e >> exitGLFW stateRef

  go   :: IO ()
  go 
   = do win <- fmap window $ readIORef stateRef
        case win of
            Just w -> do 
                windowIsOpen <- fmap not . GLFW.windowShouldClose $ w
                when windowIsOpen 
                    $ do  GLFW.pollEvents
                          dirty <- fmap dirtyScreen $ readIORef stateRef

                          when dirty
                            $ do   s <- readIORef stateRef
                                   display s
                                   GLFW.swapBuffers w

                          modifyIORef stateRef $ \s -> s { dirtyScreen = False }
                          (readIORef stateRef) >>= (\s -> idle s)
                          newSleep 0.001
                          runMainLoopGLFW stateRef
            Nothing -> return ()


-- Redisplay ------------------------------------------------------------------
postRedisplayGLFW 
        :: IORef GLFWState
        -> IO ()

postRedisplayGLFW stateRef
        = modifyIORef stateRef
        $ \s -> s { dirtyScreen = True }


-- Key Code Conversion --------------------------------------------------------
class GLFWKey a where
  fromGLFW :: a -> Key

instance GLFWKey GLFW.Key where
  fromGLFW key 
   = case key of
        GLFW.Key'Apostrophe   -> charToSpecial (toLower '\'')
        GLFW.Key'Comma        -> charToSpecial (toLower ',')
        GLFW.Key'Minus        -> charToSpecial (toLower '-')
        GLFW.Key'Period       -> charToSpecial (toLower '.')
        GLFW.Key'Slash        -> charToSpecial (toLower '/')
        GLFW.Key'0            -> charToSpecial (toLower '0')
        GLFW.Key'1            -> charToSpecial (toLower '1')
        GLFW.Key'2            -> charToSpecial (toLower '2')
        GLFW.Key'3            -> charToSpecial (toLower '3')
        GLFW.Key'4            -> charToSpecial (toLower '4')
        GLFW.Key'5            -> charToSpecial (toLower '5')
        GLFW.Key'6            -> charToSpecial (toLower '6')
        GLFW.Key'7            -> charToSpecial (toLower '7')
        GLFW.Key'8            -> charToSpecial (toLower '8')
        GLFW.Key'9            -> charToSpecial (toLower '9')
        GLFW.Key'Semicolon    -> charToSpecial (toLower ';')
        GLFW.Key'Equal        -> charToSpecial (toLower '=')
        GLFW.Key'A            -> charToSpecial (toLower 'A')
        GLFW.Key'B            -> charToSpecial (toLower 'B')
        GLFW.Key'C            -> charToSpecial (toLower 'C')
        GLFW.Key'D            -> charToSpecial (toLower 'D')
        GLFW.Key'E            -> charToSpecial (toLower 'E')
        GLFW.Key'F            -> charToSpecial (toLower 'F')
        GLFW.Key'G            -> charToSpecial (toLower 'G')
        GLFW.Key'H            -> charToSpecial (toLower 'H')
        GLFW.Key'I            -> charToSpecial (toLower 'I')
        GLFW.Key'J            -> charToSpecial (toLower 'J')
        GLFW.Key'K            -> charToSpecial (toLower 'K')
        GLFW.Key'L            -> charToSpecial (toLower 'L')
        GLFW.Key'M            -> charToSpecial (toLower 'M')
        GLFW.Key'N            -> charToSpecial (toLower 'N')
        GLFW.Key'O            -> charToSpecial (toLower 'O')
        GLFW.Key'P            -> charToSpecial (toLower 'P')
        GLFW.Key'Q            -> charToSpecial (toLower 'Q')
        GLFW.Key'R            -> charToSpecial (toLower 'R')
        GLFW.Key'S            -> charToSpecial (toLower 'S')
        GLFW.Key'T            -> charToSpecial (toLower 'T')
        GLFW.Key'U            -> charToSpecial (toLower 'U')
        GLFW.Key'V            -> charToSpecial (toLower 'V')
        GLFW.Key'W            -> charToSpecial (toLower 'W')
        GLFW.Key'X            -> charToSpecial (toLower 'X')
        GLFW.Key'Y            -> charToSpecial (toLower 'Y')        
        GLFW.Key'Z            -> charToSpecial (toLower 'Z')
        GLFW.Key'LeftBracket  -> charToSpecial (toLower '[')
        GLFW.Key'Backslash    -> charToSpecial (toLower '\\')
        GLFW.Key'RightBracket -> charToSpecial (toLower ']')    
        GLFW.Key'GraveAccent  -> charToSpecial (toLower '`')
        GLFW.Key'Space        -> SpecialKey KeySpace
        GLFW.Key'Escape       -> SpecialKey KeyEsc
        GLFW.Key'F1           -> SpecialKey KeyF1
        GLFW.Key'F2           -> SpecialKey KeyF2
        GLFW.Key'F3           -> SpecialKey KeyF3
        GLFW.Key'F4           -> SpecialKey KeyF4
        GLFW.Key'F5           -> SpecialKey KeyF5
        GLFW.Key'F6           -> SpecialKey KeyF6
        GLFW.Key'F7           -> SpecialKey KeyF7
        GLFW.Key'F8           -> SpecialKey KeyF8
        GLFW.Key'F9           -> SpecialKey KeyF9
        GLFW.Key'F10          -> SpecialKey KeyF10
        GLFW.Key'F11          -> SpecialKey KeyF11
        GLFW.Key'F12          -> SpecialKey KeyF12
        GLFW.Key'F13          -> SpecialKey KeyF13
        GLFW.Key'F14          -> SpecialKey KeyF14
        GLFW.Key'F15          -> SpecialKey KeyF15
        GLFW.Key'F16          -> SpecialKey KeyF16
        GLFW.Key'F17          -> SpecialKey KeyF17
        GLFW.Key'F18          -> SpecialKey KeyF18
        GLFW.Key'F19          -> SpecialKey KeyF19
        GLFW.Key'F20          -> SpecialKey KeyF20
        GLFW.Key'F21          -> SpecialKey KeyF21
        GLFW.Key'F22          -> SpecialKey KeyF22
        GLFW.Key'F23          -> SpecialKey KeyF23
        GLFW.Key'F24          -> SpecialKey KeyF24
        GLFW.Key'F25          -> SpecialKey KeyF25
        GLFW.Key'Up           -> SpecialKey KeyUp
        GLFW.Key'Down         -> SpecialKey KeyDown
        GLFW.Key'Left         -> SpecialKey KeyLeft
        GLFW.Key'Right        -> SpecialKey KeyRight
        GLFW.Key'Tab          -> SpecialKey KeyTab
        GLFW.Key'Enter        -> SpecialKey KeyEnter
        GLFW.Key'Backspace    -> SpecialKey KeyBackspace
        GLFW.Key'Insert       -> SpecialKey KeyInsert
        GLFW.Key'Delete       -> SpecialKey KeyDelete
        GLFW.Key'PageUp       -> SpecialKey KeyPageUp
        GLFW.Key'PageDown     -> SpecialKey KeyPageDown
        GLFW.Key'Home         -> SpecialKey KeyHome
        GLFW.Key'End          -> SpecialKey KeyEnd
        GLFW.Key'Pad0         -> SpecialKey KeyPad0
        GLFW.Key'Pad1         -> SpecialKey KeyPad1
        GLFW.Key'Pad2         -> SpecialKey KeyPad2
        GLFW.Key'Pad3         -> SpecialKey KeyPad3
        GLFW.Key'Pad4         -> SpecialKey KeyPad4
        GLFW.Key'Pad5         -> SpecialKey KeyPad5
        GLFW.Key'Pad6         -> SpecialKey KeyPad6
        GLFW.Key'Pad7         -> SpecialKey KeyPad7
        GLFW.Key'Pad8         -> SpecialKey KeyPad8
        GLFW.Key'Pad9         -> SpecialKey KeyPad9
        GLFW.Key'PadDivide    -> SpecialKey KeyPadDivide
        GLFW.Key'PadMultiply  -> SpecialKey KeyPadMultiply
        GLFW.Key'PadSubtract  -> SpecialKey KeyPadSubtract
        GLFW.Key'PadAdd       -> SpecialKey KeyPadAdd
        GLFW.Key'PadDecimal   -> SpecialKey KeyPadDecimal
        GLFW.Key'PadEqual     -> Char '='
        GLFW.Key'PadEnter     -> SpecialKey KeyPadEnter
        _                     -> SpecialKey KeyUnknown


-- | Convert char keys to special keys to work around a bug in 
--   GLFW 2.7. On OS X, GLFW sometimes registers special keys as char keys,
--   so we convert them back here.
--   GLFW 2.7 is current as of Nov 2011, and is shipped with the Hackage
--   binding GLFW-b 0.2.*
charToSpecial :: Char -> Key
charToSpecial c = case (fromEnum c) of
        32    -> SpecialKey KeySpace
        63232 -> SpecialKey KeyUp
        63233 -> SpecialKey KeyDown
        63234 -> SpecialKey KeyLeft
        63235 -> SpecialKey KeyRight
        63236 -> SpecialKey KeyF1
        63237 -> SpecialKey KeyF2
        63238 -> SpecialKey KeyF3
        63239 -> SpecialKey KeyF4
        63240 -> SpecialKey KeyF5
        63241 -> SpecialKey KeyF6
        63242 -> SpecialKey KeyF7
        63243 -> SpecialKey KeyF8
        63244 -> SpecialKey KeyF9
        63245 -> SpecialKey KeyF10
        63246 -> SpecialKey KeyF11
        63247 -> SpecialKey KeyF12
        63248 -> SpecialKey KeyF13
        63272 -> SpecialKey KeyDelete
        63273 -> SpecialKey KeyHome
        63275 -> SpecialKey KeyEnd
        63276 -> SpecialKey KeyPageUp
        63277 -> SpecialKey KeyPageDown
        _     -> Char c

instance GLFWKey GLFW.MouseButton where
  fromGLFW mouse
   = case mouse of
        GLFW.MouseButton'1 -> MouseButton LeftButton
        GLFW.MouseButton'2 -> MouseButton RightButton
        GLFW.MouseButton'3 -> MouseButton MiddleButton
        GLFW.MouseButton'4 -> MouseButton $ AdditionalButton 3
        GLFW.MouseButton'5 -> MouseButton $ AdditionalButton 4
        GLFW.MouseButton'6 -> MouseButton $ AdditionalButton 5
        GLFW.MouseButton'7 -> MouseButton $ AdditionalButton 6
        GLFW.MouseButton'8 -> MouseButton $ AdditionalButton 7
