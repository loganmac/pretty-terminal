{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO          as T
import           System.Console.Pretty (Color (..), Style (..), bgColor, color,
                                        style, supportsPretty)

main :: IO ()
main = do
  inColor <- supportsPretty
  if inColor then example
             else putStrLn "Sorry, this terminal doesn't support pretty ANSI codes"

example :: IO ()
example = do
  -- simple style
  putStrLn ( style Underline "hello there!" )

  -- simple color
  putStrLn ( color Yellow "this lib was designed to be easy" )

  -- simple background
  putStrLn ( bgColor Blue "and the functions layer together easily" )

  -- combining
  putStrLn ( bgColor White . color Red . style Bold $ "like so!" )

  -- custom style & color
  let primary = bgColor Magenta . color Green . style Italic
  putStrLn ( primary "easily create your own helpers & styles" )

  -- with both unicode string types
  putStrLn ( color Cyan "String...")
  T.putStrLn (color Red "and Text")

  -- set styling to none
  putStrLn ( primary $ style Normal "or if you need to remove any styling..." )
