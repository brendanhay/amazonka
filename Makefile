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
	cabal install --enable-tests --flags="-f-colors"

lint:
	hlint src

doc:
	cabal haddock

cabal.sandbox.config:
	cabal sandbox init

add-sources: cabal.sandbox.config
	cabal sandbox add-source ../ed-e
