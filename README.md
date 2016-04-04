junos-confparse
===============
Configuration parser for JUNOS SRX Config files

Building
--------
stack build

Running
-------
./.stack-work/install/x86_64-osx/lts-5.10/7.10.3/bin/junos-confparse-exe

Description
-----------
Intended as a library, not a command, so the command doesn't do much
useful.

Caveats
-------
This is also incomplete and likely has significant functional gaps.
Right now it can parse the address book and nothing else.  It may
choke on more complex address books, but can easily parse the almost
8,000-entry address book I need it for.

I should have a test suite for this and don't.  Tests would be easy for
a parser like this so I will eventually add them.
