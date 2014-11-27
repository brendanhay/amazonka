TOP     := ..
NAME    := amazonka-core
VERSION := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' amazonka-core.cabal)

include $(TOP)/share/library.mk

.PHONY: test

test:
	cabal install --enable-tests --only-dep && \
 cabal configure --enable-tests && \
 cabal build && \
 cabal test
