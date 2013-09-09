SHELL := /usr/bin/env bash
FLAGS := -j --disable-documentation --disable-library-coverage
DEPS  := vendor/http-streams

.PHONY: test lint doc

all: build

build: cabal.sandbox.config
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: $(DEPS) add-sources
	cabal install $(FLAGS)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox
	cabal clean

test:
	cabal install --enable-tests $(FLAGS)

lint:
	hlint src

doc:
	cabal haddock

cabal.sandbox.config:
	cabal sandbox init

add-sources: cabal.sandbox.config
	cabal sandbox add-source ../hexpat-pickle-generic
	cabal sandbox add-source ../querystring-pickle
	cabal sandbox add-source vendor/http-streams


vendor/%:
	git clone git@github.com:brendanhay/$*.git $@
