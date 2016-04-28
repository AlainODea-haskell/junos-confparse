{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (catMaybes)
import Data.List (sort)
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Lib (addressBookParser, cidr, deref, names)
import Data.HashMap.Strict (empty, insert, lookupDefault)
import qualified Data.Set as Set
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let logFile = head args
  contents <- T.readFile logFile
  let Right parsed = parseOnly addressBookParser contents
  let ns = names $ catMaybes parsed
  let setPrefix = T.toLower "Public_Lockdown_IPs"
  let setNames = setPrefix : [T.append setPrefix $ T.pack (show x) | x <- [1..5]::[Int]]
  let sets = concat $ Prelude.map ((flip $ lookupDefault []) ns) setNames
  let dereffer = flip deref $ ns
  let dereffed = concat $ Prelude.map dereffer sets
  let ips = Prelude.map cidr $ dereffed
  let uniqIps = sort $ Set.toList $ Set.fromList ips
  mapM_ (maybe (return ()) T.putStrLn) uniqIps

-- TODO: show unused address entries
-- This requires entire config parsing
-- or at least a significant subset with zone awareness
-- This is non-trivial
