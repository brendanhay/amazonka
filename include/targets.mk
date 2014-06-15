.PHONY: def-test

default: build

def-build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

def-install: cabal.sandbox.config
	cabal install $(FLAGS) && \
	 cabal install $(FLAGS) --only-dependencies --enable-tests

def-test:
	cabal configure --enable-tests && \
	 cabal build

def-clean:
	cabal clean

doc:
	cabal haddock

cabal.sandbox.config:
	ln -fs $(TOP)/cabal.sandbox.config
