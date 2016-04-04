{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( addressBookParser
    ) where

import Data.Attoparsec.Text (
  char
  , decimal
  , string
  , Parser
  , endOfLine
  , takeTill
  , isEndOfLine
  )
import Data.Text (Text)
import Data.Word
import Control.Applicative (many, (<|>))

type AddressBook = [AddressBookEntry]

data AddressBookEntry =
    Address AddressName CIDR
  | AddressSet AddressSetName AddressSetEntry
  deriving Show

type AddressName = Text

type AddressSetName = Text

data AddressSetEntry =
    AddressReference AddressName
  | AddressSetReference AddressSetName
  deriving Show

data CIDR = CIDR IP Prefix
  deriving Show

data IP = IP Word8 Word8 Word8 Word8 deriving Show

type Prefix = Int

addressBookParser :: Parser AddressBook
addressBookParser = many $ addressBookEntryParser <* endOfLine

addressBookEntryParser :: Parser AddressBookEntry
addressBookEntryParser =
     addressBookEntryAddressParser
 <|> addressBookEntryAddressSetParser

addressBookEntryAddressParser :: Parser AddressBookEntry
addressBookEntryAddressParser = do
  _ <- string "set security zones security-zone untrust address-book address "
  n <- takeTill (==' ')
  _ <- char ' '
  cidr <- cidrParser
  return $ Address n cidr

cidrParser :: Parser CIDR
cidrParser = do
  d1 <- decimal
  _ <- char '.'
  d2 <- decimal
  _ <- char '.'
  d3 <- decimal
  _ <- char '.'
  d4 <- decimal
  _ <- char '/'
  d5 <- decimal
  return $ CIDR (IP d1 d2 d3 d4) d5

addressBookEntryAddressSetParser :: Parser AddressBookEntry
addressBookEntryAddressSetParser = do
  _ <- string "set security zones security-zone untrust address-book address-set "
  n <- takeTill (==' ')
  e <- addressSetEntryParser
  return $ AddressSet n e

addressSetEntryParser :: Parser AddressSetEntry
addressSetEntryParser =
      addressSetEntryAddressParser
  <|> addressSetEntryAddressSetParser

addressSetEntryAddressParser :: Parser AddressSetEntry
addressSetEntryAddressParser = do
  _ <- string " address "
  n <- takeTill isEndOfLine
  return $ AddressReference n

addressSetEntryAddressSetParser :: Parser AddressSetEntry
addressSetEntryAddressSetParser = do
  _ <- string " address-set "
  n <- takeTill isEndOfLine
  return $ AddressSetReference n
