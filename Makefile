SHELL := /usr/bin/env bash
FLAGS := -j --disable-documentation --disable-library-coverage

.PHONY: test lint doc

all: build

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: cabal.sandbox.config add-sources
	cabal install $(FLAGS)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox vendor
	cabal clean

test:
	cabal test --test-options=\
	"--num-threads=4 +RTS -N4 -RTS --quickcheck-tests=20 --quickcheck-max-size=50"

lint:
	hlint src

doc:
	cabal haddock

cabal.sandbox.config:
	cabal sandbox init

add-sources: cabal.sandbox.config
	cabal sandbox add-source ../ede
