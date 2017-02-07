import System.IO

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.EqualSpacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import XMonad.Util.Run(spawnPipe, runInTerm)
import XMonad.Util.EZConfig(additionalKeys)

import XMonad.Actions.WindowGo

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ defaultConfig
        { startupHook        = startup
        , manageHook         = manageHook'
        , layoutHook         = layout
        , logHook            = logHook' xmproc
        , borderWidth        = borderWidth'
        , terminal           = terminal'
        , normalBorderColor  = normalBorderColor'
        , focusedBorderColor = focusedBorderColor'
        } `additionalKeys`
        [ toggleFullScreen
        , firefox
        , zathura
        , rtv
        , vim
        , ranger
        , weechat
        ]

-- Basic configs
borderWidth' = 1
terminal' = "urxvt"
normalBorderColor' = "#cccccc"
focusedBorderColor' = "#8A745E"

startup :: X ()
startup = do
    spawn "wal -i '$(< '${HOME}/.cache/wal/wal')'"

-- Layouts
layout = id
    . equalSpacing gapWidth gapShrink mult minWidth
    . avoidStruts
    . mkToggle (single FULL) 
    $ tiled ||| Mirror tiled ||| simplestFloat
  where
    gapWidth  = 15
    gapShrink = 0
    mult      = 0
    minWidth  = 1
    tiled     = Tall nmaster delta ratio
    nmaster   = 1
    ratio     = 1/2
    delta     = 3/100

-- Hooks
manageHook' = manageDocks <+> manageHook defaultConfig
logHook' xmproc = dynamicLogWithPP xmobarPP
    { ppOutput = hPutStrLn xmproc
    , ppTitle  = xmobarColor "brown" "" . shorten 50
    }

-- Keymappings
-- Should be of type XConfig a -> [((ButtonMask, KeySym), X ())] -> XConfig a 
-- keybind :: XConfig a -> [((ButtonMask, KeySym), X ())] -> XConfig a

firefox = ((mod1Mask .|. shiftMask, xK_b), spawn "firefox")
zathura = ((mod1Mask .|. shiftMask, xK_z), spawn "zathura")
rtv     = ((mod1Mask .|. shiftMask, xK_r), runInTerm "" "rtv")
vim     = ((mod1Mask .|. shiftMask, xK_v), runInTerm "" "nvim")
ranger  = ((mod1Mask .|. shiftMask, xK_t), runInTerm "" "env EDITOR=nvim ranger")
weechat = ((mod1Mask .|. shiftMask, xK_m), runInTerm "" "weechat")
toggleFullScreen = ((mod1Mask, xK_f), sendMessage $ Toggle FULL)
