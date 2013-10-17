SHELL := /usr/bin/env bash
FLAGS := -j --disable-documentation --disable-library-coverage
DEPS  := vendor/http-streams

.PHONY: test lint doc

all: build

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: $(DEPS) cabal.sandbox.config add-sources
	cabal install $(FLAGS)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox vendor
	cabal clean

test:
	cabal install --enable-tests

lint:
	hlint src

doc:
	cabal haddock

cabal.sandbox.config:
	cabal sandbox init

add-sources: cabal.sandbox.config
	cabal sandbox add-source vendor/http-streams

vendor/http-streams:
	git clone git@github.com:afcowie/http-streams.git $@

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@
