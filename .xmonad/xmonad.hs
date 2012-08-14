import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO
import XMonad.Layout.NoBorders
import XMonad.Layout.IM
import Data.Ratio ((%))
import XMonad.Layout.Grid
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Actions.GridSelect
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Reflect
import qualified XMonad.StackSet as W
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Hooks.ManageHelpers
 
myManageHook = composeAll
    [ (role =? "gimp-toolbox" <||> role =? "gimp-image-window") --> (ask >>= doF . W.sink)
    , className =? "Skype"                --> doShift "3:skype"
--    , className =? "Chrome"               --> doShift "2:web1"
    , className =? "Shiretoko"            --> doShift "3:web2"
    , className =? "Evince"               --> doShift "6:pdf"
    , className =? "feh"                  --> doCenterFloat
    , className =? "Gimp"                 --> doShift "4:gimp"
    , className =? "Keepassx"             --> doShift "2:web"
    , className =? "ROX-Filer"            --> doCenterFloat
    , className =? "XCalc"                --> doCenterFloat
    , className =? "OpenOffice.org 3.1"   --> doShift "7:office"
    , className =? "Pidgin"               --> doShift "5:pidgin"
    --, isFullscreen --> doFullFloat
    ]
  where role = stringProperty "WM_WINDOW_ROLE"
 
myLayoutHook = smartBorders $ avoidStruts $ onWorkspace "5:pidgin" pidginLayout $ onWorkspace "4:gimp" gimpLayout $ onWorkspaces ["2:web", "6:pdf", "7:office"] Full $ onWorkspace "3:skype" Grid $ tiled ||| Full ||| Grid ||| simplestFloat
  where
    tiled = ResizableTall 1 (3/100) (1/2) []
    gimpLayout = withIM (0.20) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.20) (Role "gimp-dock") Full
    pidginLayout = withIM (1/7) (Role "buddy_list") Grid
 
myXPConfig = defaultXPConfig { fgColor   = "#FFFFFF"
                              , bgColor  = "#000000"
                              , bgHLight = "cyan"
                              , fgHLight = "#FFFFFF"
                              , font     = "xft:Terminus:pixelsize=12"
                              }
 
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }
 
customPP :: PP
customPP = defaultPP { ppCurrent = xmobarColor "#FFFFFF" ""
                     , ppUrgent  = xmobarColor "#FF0000" ""
                     , ppLayout  = xmobarColor "orange" ""
                     , ppTitle   = xmobarColor "#FFFFFF" "" . shorten 80
                     }
 
myWorkspaces = ["1:main", "2:web1", "3:web2", "4:dev1", "5:dev2", "6:VM", "7:bg"] ++ map show [8..9]
 
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = myLayoutHook
        , logHook = myLogHook xmproc
        , modMask = mod4Mask
        , terminal = "urxvt"
        , focusFollowsMouse = False
        , borderWidth = 1
        , normalBorderColor  = "#999"
        , focusedBorderColor = "#ee9a00"
        , workspaces = myWorkspaces
        } `additionalKeys`
        [ ((0, 0x1008ff13), spawn "amixer sset PCM 5+")
        , ((0, 0x1008ff11), spawn "amixer sset PCM 5-")
        , ((mod4Mask .|. shiftMask, xK_p), shellPrompt myXPConfig)
        , ((mod4Mask, xK_g), goToSelected defaultGSConfig)
        , ((mod4Mask, xK_a), sendMessage MirrorShrink)
        , ((mod4Mask, xK_z), sendMessage MirrorExpand)
	--mod1Mask
        ]
