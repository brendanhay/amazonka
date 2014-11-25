TOP     := ..
NAME    := amazonka-core
VERSION := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' amazonka-core.cabal)

include $(TOP)/share/include.mk

.PHONY: test

test:
	cabal install --enable-tests
