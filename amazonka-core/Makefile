SHELL := /usr/bin/env bash
FLAGS := -j --disable-documentation --disable-library-coverage

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: add-sources
	cabal install $(FLAGS) --only-dependencies

clean:
	-cabal clean

doc:
	cabal haddock

add-sources: cabal.sandbox.config

cabal.sandbox.config:
	cabal sandbox init --sandbox=../.cabal-sandbox
