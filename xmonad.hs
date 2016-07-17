{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           AppendFileAdv                  (appendFileAdvPrompt)
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (when)
import           Data.Char                      (chr, ord)
import qualified Data.Map                       as M
import           System.IO                      (hClose)
import           System.Locale                  (defaultTimeLocale)
import           System.Time                    (formatCalendarTime,
                                                 getClockTime, toCalendarTime)
import           XMonad
import           XMonad.Actions.CopyWindow      (copy, kill1)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Search          (escape, google,
                                                 promptSearchBrowser,
                                                 selectSearchBrowser)
import           XMonad.Actions.SinkAll         (sinkAll)
import           XMonad.Actions.Submap
import           XMonad.Actions.SwapWorkspaces
import           XMonad.Config.Desktop          (desktopLayoutModifiers)
import           XMonad.Config.Gnome
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Minimize         (MinimizeMsg (RestoreNextMinimizedWin),
                                                 minimize, minimizeWindow)
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile    (MirrorResize (MirrorShrink, MirrorExpand),
                                                 ResizableTall (ResizableTall))
import           XMonad.Layout.ToggleLayouts
import           XMonad.Layout.WindowNavigation
import           XMonad.Prompt
import qualified XMonad.StackSet                as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run                (hPutStrLn, runProcessWithInput,
                                                 safeSpawn, spawnPipe)
import           XMonad.Util.XSelection         (getSelection)

--
-- Dan's xmonad layout
--
-- In its root, it only uses only the two following dominating hot keys:
--
--    'Windows' button for controlling the management of the windows themselves, used as a combination key.
--    'Menu' button, used as a prefix key for stuff that don't relate directly to windows management.
--

-- Our font for prompt and notification bars
ourFont = "-adobe-*-*-r-*-*-20-*-*-*-*-*-*-*"

-- Basic config is gnome
baseConfig = gnomeConfig
promptConfig = greenXPConfig {
      font        = ourFont
    , historySize = 0
    , borderColor = "#008800"
    , defaultText = ""
    , promptBorderWidth = 1
    , height      = 22
  }

-- Spawn stuff via zsh's environment
spawnZsh cmd = spawn $ "zsh -c " ++ (show cmd)

-- Notification bar
notify s = do
  liftIO $ forkIO $ do
    let time_to_wait = max 2 ((length s) `div` 20)
    -- I use a patched version of dzen2 that accepts bottom-screen relative y coordinate. I can provide this patch...
    h <- spawnPipe $ concat ["dzen2p -p " ++ (show time_to_wait) ++ " -fn '" ++ ourFont ++ "' -ta l -fg white -bg black -y ^-40"]
    hPutStrLn h s
    hClose h
  return ()

-- Google translate chrome popup
googleTranslate s lfrom lto = do
  safeSpawn "google-chrome" ["--new-window", "http://translate.google.com/?q=i#" ++ lfrom ++ "|" ++ lto ++ "|" ++ (escape s)]
  return ()

-- For some reason the following function doesn't work reliably.
popupTranslate s lfrom lto = do
  liftIO $ forkIO $ do
    t <- runProcessWithInput "translate-bin" ["-f", lfrom, "-t", lto] s
    notify t
  return ()

translateSelection engine lfrom lto = do
  s <- getSelection
  engine s lfrom lto

data TranslatePrompt = TranslatePrompt String
instance XPrompt TranslatePrompt where
    showXPrompt (TranslatePrompt s) = s

translatePrompt engine lfrom lto = do
  mkXPrompt (TranslatePrompt $ concat [lfrom, "->", lto, ": "]) promptConfig (const (return [])) $ \s ->
    engine s lfrom lto

wrapStringWithCurrentTime :: String -> IO String
wrapStringWithCurrentTime s = do
    time <- (getClockTime >>= toCalendarTime)
    let stime = formatCalendarTime defaultTimeLocale "%a %b %e %H:%M:%S %Z %Y" time
    return $ stime ++ ": " ++ s

{-
Prevent new windows from spawning in the master pane.
Source: <http://ruderich.org/simon/config/xmonad>
-}

manageFocus :: ManageHook
manageFocus = composeOne
    [
      -- prevent new windows from spawning in the master pane
      return True -?> doF avoidMaster
      -- prevent windows moved to other workspaces to steal focus
    , return True -?> doF W.focusDown
    ]

-- | Prevent windows from spawning in the master pane.
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) -> W.Stack r [] (t:rs)
    _                   -> c

keepFocusOnSpawn :: ManageHook
keepFocusOnSpawn = doF keepFocus
  where
    keepFocus = W.modify' $ \(W.Stack t ls (r:rs)) -> W.Stack r ls (t:rs)

mateRun :: X ()
mateRun = withDisplay $ \dpy -> do
    rw <- asks theRoot
    mate_panel <- getAtom "_MATE_PANEL_ACTION"
    panel_run   <- getAtom "_MATE_PANEL_ACTION_RUN_DIALOG"

    io $ allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw mate_panel 32 panel_run 0
        sendEvent dpy rw False structureNotifyMask e
        sync dpy False

-- Using the 'Windows' button as combination prefix, there are various actions that we use to manage the windows
myManagingKeys conf = [
    -- For navigating the focus:
      ("M-<L>", sendMessage $ Go L)
    , ("M-<R>", sendMessage $ Go R)
    , ("M-<U>", sendMessage $ Go U)
    , ("M-<D>", sendMessage $ Go D)

    -- meddling with the layout
    , ("M-<Page_Up>", sendMessage (IncMasterN 1))
    , ("M-<Page_Down>", sendMessage (IncMasterN (-1)))
    , ("M-S-<L>", sendMessage $ Swap L)
    , ("M-S-<R>", sendMessage $ Swap R)
    , ("M-S-<U>", sendMessage $ Swap U)
    , ("M-S-<D>", sendMessage $ Swap D)
    , ("M-j", sendMessage MirrorShrink)
    , ("M-u", sendMessage MirrorExpand)
    , ("M-<KP_Divide>", sendMessage MirrorShrink)
    , ("M-<KP_Multiply>", sendMessage MirrorExpand)

    -- Minimize/un-minimize (though its still in the rotating focus list...)
    , ("M-<KP_Subtract>", withFocused minimizeWindow)
    , ("M-<KP_Add>", sendMessage RestoreNextMinimizedWin)
    , ("M-Return", windows W.swapMaster)
    , ("M-<Delete>", kill1)

    -- Some time we just want a fully maximized window
    , ("M-f", sendMessage (Toggle "Full"))

    -- Where did our floaters go?
    , ("M-<Home>", sinkAll)
    , ("M-m", submap . M.fromList $ menuActions)

    -- Other
    , ("M-p", mateRun)
   ] ++ (concat $ map (\i ->
      let key = [chr ((ord '0') + i)]
          ws = (workspaces conf) !! (i-1)
       -- Actions that bind different key combo's depending on workspace:
       in [
               -- xmonad lets you have the same window "copied" to different workspaces - very useful
               ("M-c " ++ key,
                  do windows $ copy ws
                     windows $ W.view ws),
               ("M-s " ++ key,
                  do windows $ swapWithCurrent ws),
               ("M-S-c " ++ key,
                  do windows $ copy ws)
          ]
       ) [1..8])
    ++ [
      (otherModMasks ++ "M-" ++ [key], action tag)
      | (tag, key)  <- zip myWorkspaces "123456789"
      , (otherModMasks, action) <- [ ("", greedyView)
                                   , ("M-0 ", windows . W.greedyView)
                                   , ("S-", windows . W.shift)]
    ]
    ++ [(m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip ["q", "w"] [0..]
        , (f, m) <- [(W.view, "M-"), (W.shift, "M-S-")]]
  where
    myWorkspaces = ["1","2","3","4","5","6","7","8", "9"]
    greedyView x = do
        -- A hack to prevent accidental switching on the little latop
        -- screen. TODO: use a better predicate.
        XState { windowset = windowset } <- get
        let W.StackSet (W.Screen _ _ sid) _ _ _ = windowset
        let screenHeight = rect_height $ screenRect sid
        when (screenHeight /= 720) $
            windows $ do
                W.greedyView x

-- Here we have actions, all prefixed with the Menu key
menuActions = M.toList $ mkKeymap baseConfig ([
    -- Sometimes this is convinent:
      ("r r", mateRun)
    , ("r c", spawnZsh "google-chrome")
    , ("s <Escape>", spawnZsh "my-xrandr laptop  && xorg-touch-screen 1 ; nmcli radio wifi on")
    , ("s <Return>", spawnZsh "my-xrandr desktop && xorg-touch-screen 0 ; nmcli radio wifi off")
    , ("s z", spawnZsh "xnotify Display to standby & ; sleep 3 ; remote-display-standby")
    , ("s l", spawnZsh "xnotify AFK & ; sleep 3 ; mate-screensaver-command -l")
    , ("s c", spawnZsh "nmcli device connect cdc-wdm1 && xnotify GSM on ; nmcli radio wifi off")
    , ("s d", spawnZsh "nmcli device disconnect cdc-wdm1 ; xnotify GSM off; nmcli radio wifi on")
    , ("s h", spawnZsh "cat ~/.xmonad/xmonad.hs | grep '\"s ' | xnotify -x -")

    -- Google translation for educational purposes...
    , ("t f", translateSelection googleTranslate "en" "fr")
    , ("t e", translateSelection googleTranslate "fr" "en")
    , ("t 4 f", translatePrompt googleTranslate "en" "fr")
    , ("t 4 e", translatePrompt googleTranslate "fr" "en")

    , ("c", do s <- getSelection
               notify s)

    -- Simulate middle button click, for laptops that lack it
    , ("x", spawnZsh "xdotool click 2")

    -- Google search
    , ("'", promptSearchBrowser promptConfig "google-chrome" google)
    , ("/", selectSearchBrowser "google-chrome" google)

    -- Various spawns
    , ("`", spawnZsh "~/.xmonad/recompile.sh && xnotify xmonad reloaded")
    , ("e", spawnZsh "e-ibuffer")
    , ("m m", spawnZsh "amixer set PCM 30%")
    , ("m <U>", spawnZsh "amixer set PCM 11.11111%+")
    , ("m <D>", spawnZsh "amixer set PCM 10%-")
    , ("m p", spawnZsh "xorg-keysym play")
    , ("m <L>", spawnZsh "xorg-keysym prev")
    , ("m <R>", spawnZsh "xorg-keysym next")

    -- Keyboard settings
    , ("k i", spawnZsh "setxkbmap us -option grp:alt_lshift_toggle -variant intl")
    , ("k n", spawnZsh "setxkbmap us,il -option grp:lalt_lshift_toggle")

    -- Captain's log... :)
    , ("<Return>", appendFileAdvPrompt promptConfig "/home/dan/var/notes/log.txt" "Log: " wrapStringWithCurrentTime)
    , ("<Insert>", appendFileAdvPrompt promptConfig "/home/dan/var/notes/time-usage.txt" "Time usage: " wrapStringWithCurrentTime)

    -- 'dup' is just a convenient external wrapper I use for spawning a shellscripts in a new rxvt-unicode console.
    , ("d m m", spawnZsh "dup mutt")
    , ("d m k", spawnZsh "dup ~/kernelim/env.sh mutt")
    , ("d m l", spawnZsh "dup ~/kernelim/env.sh mutt-lkml")
    , ("d m e", spawnZsh "dup ~/elastifile/env.sh mutt")
    , ("d d", spawnZsh "dup")
    , ("d h", spawnZsh "dup ghci")
    , ("d p", spawnZsh "dup python")
  ])

myMenuKeys = [((0, xK_Menu), submap . M.fromList $ menuActions),
              ((0, xK_Print), submap . M.fromList $ menuActions)]

-- Use button scroll along with Windows holding in order to shrink/expand windows vertically
myMouse (XConfig {XMonad.modMask = modMask'}) = [
      ((modMask', button4), \_ -> sendMessage $ MirrorExpand)
    , ((modMask', button5), \_ -> sendMessage $ MirrorShrink)
  ]

-- Our standard but resizable layout with the usual xmonad extensions
myBasicLayout = tiled ||| Mirror tiled ||| Full
  where
     tiled   = ResizableTall nmaster delta ratio []
     nmaster = 1
     ratio   = 1/2
     delta   = 5/100

myLayoutHook = toggleLayouts (avoidStruts $ noBorders Full) $
               smartBorders $
               windowNavigation $
               minimize $
               desktopLayoutModifiers myBasicLayout

myFilterOfSomeKeys :: XConfig x -> XConfig x
myFilterOfSomeKeys conf@(XConfig {XMonad.modMask = modMask}) = conf {
     keys = \i -> f $ (keys conf) i
  }
 where f y = y `M.difference` delete_list
       delete_list = M.fromList $ map (\x->(x, ())) list
       list = [
           (modMask, xK_space) -- Used instead to switch language layouts
         ]

startup :: X ()
startup = do
  return ()

main :: IO ()
main = do
  let base_config = baseConfig {
      layoutHook = myLayoutHook
    -- , manageHook = manageFocus <+> manageHook baseConfig
    , focusedBorderColor 	= "#00ff00"
    , normalBorderColor 	= "#444444"
    , startupHook               = startup
    , borderWidth = 3  -- Pixles are small these days...
    , modMask = mod4Mask
    , terminal = "dup"
    , focusFollowsMouse = False -- Hell no...
    }
  xmonad $ myFilterOfSomeKeys $ base_config
    `additionalKeys` myMenuKeys
    `additionalKeysP` (myManagingKeys base_config)
    `additionalMouseBindings` (myMouse base_config)
