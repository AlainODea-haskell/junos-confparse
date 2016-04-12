{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text (parseOnly)
import Data.Text.IO as T
import Lib (addressBookParser)
import Data.HashMap.Strict (empty, insert, lookup)

main :: IO ()
main = T.readFile logFile >>= print . parseOnly addressBookParser
  where logFile = "address-book-example.txt"


{-
Next Steps
==========
- Use Data.HashMap.Strict as a dictionary for load/store
  of address-book to enable deref
- Implement recursive deref

Thoughts:
One pass to get initial mapping
Second pass to replace refs with actual addresses
Any deref would recurse until addresses
  Essentially any address-set should be replaced with a list of its contents
  A new data structure to represent this is NOT required, it's still
  [AddressBookEntry]
  Recursion would take any AddressSet and replace with contents
  How do we do this reliably?

Example:
set security zones security-zone untrust address-book address Net_10.0.0.1/16 10.0.0.1/16
set security zones security-zone untrust address-book address-set AcmeWidgetsCo address Net_10.0.0.1/16
set security zones security-zone untrust address-book address-set PartnerNetworks address-set AcmeWidgetsCo

Parse pass:
Right
[
Address "Net_10.0.0.1/16" (CIDR (IP 10 0 0 1) 16),
AddressSet "AcmeWidgetsCo" (AddressReference "Net_10.0.0.1/16"),
AddressSet "PartnerNetworks" (AddressSetReference "AcmeWidgetsCo")
]

Build lookup table:
fromList
[
("Net_10.0.0.1/16",[Address "Net_10.0.0.1/16" (CIDR (IP 10 0 0 1) 16)]),
("AcmeWidgetsCo",[AddressSet "AcmeWidgetsCo" (AddressReference "Net_10.0.0.1/16")]),
("PartnerNetworks",[AddressSet "PartnerNetworks" (AddressSetReference "AcmeWidgetsCo")])
]
 -NOTE: may require Address and AddressSet lookup tables if JUNOS has separate
  namespaces for them (that seems pathological)
 -If it doesn't, then the AddressReference and AddressSetReference can collapse
  into one

Deref:
Address "Net_10.0.0.1/16" (CIDR (IP 10 0 0 1) 16))
identity []
[Address "Net_10.0.0.1/16" (CIDR (IP 10 0 0 1) 16))]

AddressSet "AcmeWidgetsCo" (AddressReference "Net_10.0.0.1/16")
deref "Net_10.0.0.1/16"
[Address "Net_10.0.0.1/16" (CIDR (IP 10 0 0 1) 16))]

AddressSet "PartnerNetworks" (AddressSetReference "AcmeWidgetsCo")
deref "AcmeWidgetsCo"
AddressSet "AcmeWidgetsCo" (AddressReference "Net_10.0.0.1/16")
deref "Net_10.0.0.1/16"
[Address "Net_10.0.0.1/16" (CIDR (IP 10 0 0 1) 16))]

names :: [AddressBookEntry] -> HashMap Text [AddressBookEntry]
names = foldr g
  where g x@(Address name _) = insert name x
        g x@(AddressSet name _) = insert name x

If I was feeling particularly clever I could optimize this by populating
a lookup of the previous derefs, thus dramatically cutting down
computation.

deref :: AddressBookEntry -> HashMap Text [AddressBookEntry] -> [AddressBookEntry]
deref (AddressSet _ entry) names = derefEntry entry
  where derefEntry :: AddressSetEntry -> [AddressBookEntry]
        derefEntry (AddressReference name) = map deref $ derefName name
        derefEntry (AddressSetReference name) = map deref $ derefName name
        derefName :: Text -> [AddressBookEntry]
        derefName name = lookup name names
deref x = x

-- really wish I knew Reader monad right now
-- ugly hack is closure scope
-}
