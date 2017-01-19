import System.IO

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.Spacing
import XMonad.Layout.EqualSpacing

import XMonad.Util.Run(spawnPipe, runInTerm)
import XMonad.Util.EZConfig(additionalKeys)

import XMonad.Actions.WindowGo

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook         = manageHook'
        , layoutHook         = layoutHook'
        , logHook            = logHook' xmproc
        , borderWidth        = borderWidth'
        , terminal           = terminal'
        , normalBorderColor  = normalBorderColor'
        , focusedBorderColor = focusedBorderColor'
        } `additionalKeys`
        [ newFirefox
        , selectFirefox
        ]

-- Basic configs
borderWidth' = 1
terminal' = "xfce4-terminal"
normalBorderColor' = "#cccccc"
focusedBorderColor' = "#8A745E"

-- Hooks
manageHook' = manageDocks <+> manageHook defaultConfig
layoutHook' = equalSpacing 15 0 0 1 $ avoidStruts $ layoutHook defaultConfig
logHook' xmproc = dynamicLogWithPP xmobarPP
    { ppOutput = hPutStrLn xmproc
    , ppTitle  = xmobarColor "brown" "" . shorten 50
    }

-- Keymappings
-- Should be of type XConfig a -> [((ButtonMask, KeySym), X ())] -> XConfig a 
newFirefox = ((mod1Mask .|. shiftMask, xK_f), spawn "firefox")
selectFirefox = ((mod1Mask, xK_f), raise (className =? "Firefox"))
