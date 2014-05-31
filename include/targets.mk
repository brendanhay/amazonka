.PHONY: test

default: build

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: cabal.sandbox.config
	cabal install $(FLAGS) && \
	 cabal install $(FLAGS) --only-dependencies --enable-tests

test:
	cabal configure --enable-tests && \
	 cabal build

clean:
	cabal clean

doc:
	cabal haddock

cabal.sandbox.config:
	ln -fs $(TOP)/cabal.sandbox.config
