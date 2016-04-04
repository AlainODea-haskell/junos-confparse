{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text (parseOnly)
import Data.Text.IO as T
import Lib (addressBookParser)

main :: IO ()
main = T.readFile logFile >>= print . parseOnly addressBookParser
  where logFile = "address-book-example.txt"
