{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( addressBookParser
    , names
    , deref
    , derefAll
    , cidr
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
import Data.Text (Text(), toLower)
import Data.Word
import Control.Applicative (many, (<|>))
import Data.HashMap.Strict (HashMap(), empty, insert, lookupDefault)
import qualified Data.Set as Set

type AddressBook = [AddressBookEntry]

data AddressBookEntry =
    Address AddressName CIDR
  | AddressDNS AddressDNSName FQDN
  | AddressSet AddressSetName AddressSetEntry
  deriving (Show, Ord, Eq)

type AddressName = Text

type AddressDNSName = Text

type FQDN = Text

type AddressSetName = Text

data AddressSetEntry =
    AddressReference AddressName
  | AddressSetReference AddressSetName
  deriving (Show, Ord, Eq)

data CIDR = CIDR IP Prefix
  deriving (Show, Ord, Eq)

data IP = IP Word8 Word8 Word8 Word8
  deriving (Show, Ord, Eq)

type Prefix = Int

names :: [AddressBookEntry] -> HashMap Text [AddressBookEntry]
names = foldr g empty
  where g :: AddressBookEntry -> HashMap Text [AddressBookEntry] -> HashMap Text [AddressBookEntry]
        g x@(AddressDNS name _) y = insert name (x:(lookupDefault [] name y)) y
        g x@(Address name _) y = insert name (x:(lookupDefault [] name y)) y
        g x@(AddressSet name _) y = insert name (x:(lookupDefault [] name y)) y

derefAll :: [AddressBookEntry] -> [AddressBookEntry]
derefAll parsed = Set.toList . Set.fromList . concat $
  Prelude.map ((flip deref) $
               names parsed) parsed

cidr :: AddressBookEntry -> Maybe CIDR
cidr (Address _ x) = Just x
cidr _ = Nothing

deref :: AddressBookEntry -> HashMap Text [AddressBookEntry] -> [AddressBookEntry]
deref (AddressSet _ entry) xs = derefEntry entry
  where derefEntry :: AddressSetEntry -> [AddressBookEntry]
        derefEntry (AddressReference name) = concat . map (flip deref $ xs) $ derefName name
        derefEntry (AddressSetReference name) = concat . map (flip deref $ xs) $ derefName name
        derefName :: Text -> [AddressBookEntry]
        derefName name = lookupDefault [] name xs
deref x _ = [x]

addressBookParser :: Parser AddressBook
addressBookParser = many $ addressBookEntryParser <* endOfLine

addressBookEntryParser :: Parser AddressBookEntry
addressBookEntryParser =
     addressBookEntryAddressDNSParser
 <|> addressBookEntryAddressParser
 <|> addressBookEntryAddressSetParser

addressBookEntryAddressDNSParser :: Parser AddressBookEntry
addressBookEntryAddressDNSParser = do
  _ <- string "set security zones security-zone untrust address-book address "
  n <- takeTill (==' ')
  _ <- string " dns-name "
  dns <- takeTill isEndOfLine
  return $ AddressDNS (toLower n) (toLower dns)

addressBookEntryAddressParser :: Parser AddressBookEntry
addressBookEntryAddressParser = do
  _ <- string "set security zones security-zone untrust address-book address "
  n <- takeTill (==' ')
  _ <- char ' '
  c <- cidrParser
  return $ Address (toLower n) c

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
  return $ AddressSet (toLower n) e

addressSetEntryParser :: Parser AddressSetEntry
addressSetEntryParser =
      addressSetEntryAddressParser
  <|> addressSetEntryAddressSetParser

addressSetEntryAddressParser :: Parser AddressSetEntry
addressSetEntryAddressParser = do
  _ <- string " address "
  n <- takeTill isEndOfLine
  return $ AddressReference (toLower n)

addressSetEntryAddressSetParser :: Parser AddressSetEntry
addressSetEntryAddressSetParser = do
  _ <- string " address-set "
  n <- takeTill isEndOfLine
  return $ AddressSetReference (toLower n)
