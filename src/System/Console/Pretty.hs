{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-| Useful helpers to style and color text with ANSI escape sequences.
-}
module System.Console.Pretty
( Color(..) , Pretty(..) , Section(..) , Style(..)
, supportsPretty, hSupportsPretty)
where

import qualified Data.Char          as C
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import           GHC.IO.Handle      (Handle)
import           System.Environment (lookupEnv)
import           System.IO          (hIsTerminalDevice, stdout)

---------------------------------------------------------------------------------
-- TYPES

-- | A section to be colored, either foreground or background.
data Section = Foreground | Background

-- | Colors for an ANSI terminal
data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default
  deriving (Enum)

-- | SGR paramaters, aka text styles for an ANSI terminal
data Style
  = Normal | Bold | Faint | Italic
  | Underline | SlowBlink | ColoredNormal | Reverse
  deriving (Enum)


---------------------------------------------------------------------------------
-- CLASS

-- | A class to color and style
class Pretty a where
  -- | Helper to set foreground color
  color :: Color -> a -> a
  color = colorize Foreground
  -- | Helper to set background color
  bgColor :: Color -> a -> a
  bgColor = colorize Background
  -- | Set the color of the (fg/bg) with the color
  colorize :: Section -> Color -> a -> a
  -- | Set the style
  style :: Style -> a -> a

---------------------------------------------------------------------------------
-- TEXT

-- | Instance of `Pretty` for `T.Text`
instance Pretty T.Text where
  colorize section col str =
    "\x1b[" <>                                  -- escape code
    sectionNum <>                               -- bg/foreground
    (T.singleton $ C.intToDigit $ fromEnum col) -- color code
    <> "m" <>                                   -- delim
    str <>                                      -- inner string
    "\x1b[0m"                                   -- reset
    where
      sectionNum :: T.Text
      sectionNum = case section of
        Foreground -> "9"
        Background -> "4"

  style sty str =
    "\x1b[" <>                                  -- escape code
    (T.singleton $ C.intToDigit $ fromEnum sty) -- style
    <> "m" <>                                   -- delim
    str <>                                      -- inner string
    "\x1b[0m"                                   -- reset

---------------------------------------------------------------------------------
-- STRING

-- | Instance of `Pretty` for `String`
instance Pretty String where
  colorize section col str =
    "\x1b[" <>          -- escape code
    sectionNum <>       -- bg/foreground
    show (fromEnum col) -- color code
    <> "m" <>           -- delim
    str <>              -- inner string
    "\x1b[0m"           -- reset
    where
      sectionNum :: String
      sectionNum = case section of
        Foreground -> "9"
        Background -> "4"

  style sty str =
    "\x1b[" <>             -- escape code
    show (fromEnum sty)    -- style
    <> "m" <>              -- delim
    str <>                 -- string
    "\x1b[0m"              -- reset


---------------------------------------------------------------------------------
-- SUPPORTED CHECK

-- | Whether or not the current terminal supports pretty-terminal
supportsPretty :: IO Bool
supportsPretty = hSupportsPretty stdout

-- | Use heuristics to determine whether the functions defined in this
-- package will work with a given handle.
--
-- The current implementation checks that the handle is a terminal, and
-- that the @TERM@ environment variable doesn't say @dumb@ (which is what
-- Emacs sets for its own terminal).
hSupportsPretty :: Handle -> IO Bool
-- Borrowed from an HSpec patch by Simon Hengel
-- (https://github.com/hspec/hspec/commit/d932f03317e0e2bd08c85b23903fb8616ae642bd)
hSupportsPretty h = (&&) <$> hIsTerminalDevice h <*> (not <$> isDumb)
  where
    isDumb = (== Just "dumb") <$> lookupEnv "TERM"
