-----------------------------------------------------------------------------
-- |
-- Module      :  AppendFileMore
-- Copyright   :  (c) 2007 Brent Yorgey, (c) 2009 Dan Aloni
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  stable
-- Portability :  unportable
--
-- A prompt for appending a single line of text to a file.  Useful for
-- keeping a file of notes, things to remember for later, and so on---
-- using a keybinding, you can write things down just about as quickly
-- as you think of them, so it doesn't have to interrupt whatever else
-- you're doing.
--
-- Who knows, it might be useful for other purposes as well!
--
-----------------------------------------------------------------------------

module AppendFileAdv (
                                 -- * Usage
                                 -- $usage

                                 appendFileAdvPrompt
                                ) where

import XMonad.Core
import XMonad.Prompt

import System.IO
import Control.Exception

-- $usage
--
-- You can use this module by importing it, along with
-- "XMonad.Prompt", into your ~\/.xmonad\/xmonad.hs file:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.AppendFile
--
-- and adding an appropriate keybinding, for example:
--
-- >  , ((modm .|. controlMask, xK_n), appendFilePrompt defaultXPConfig "/home/me/NOTES")
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

data AppendFile = AppendFile FilePath String

instance XPrompt AppendFile where
    showXPrompt (AppendFile _ s) = s

-- | Given an XPrompt configuration and a file path, prompt the user
--   for a line of text, and append it to the given file.
appendFileAdvPrompt :: XPConfig -> FilePath -> String -> (String -> IO String) -> X ()
appendFileAdvPrompt c fn s rfunc =
  mkXPrompt (AppendFile fn s)
    c
    (const (return []))
    (doAppend fn rfunc)

-- | Append a string to a file.
doAppend :: FilePath -> (String -> IO String) -> String -> X ()
doAppend fn rfunc = io . bracket (openFile fn AppendMode) hClose . flip (\h s -> rfunc s >>= hPutStrLn h)
