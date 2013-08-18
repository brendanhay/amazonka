.PHONY: test lint doc

all: build

build: .conf
	cabal-dev build

install:
	cabal-dev install -j \
	 --disable-documentation \
	 --disable-library-coverage

clean:
	-rm -rf .conf dist
	cabal-dev clean

test:
	rm -f .conf && cabal-dev install --enable-tests

lint:
	hlint src

doc:
	cabal-dev haddock

.conf:
	cabal-dev configure && touch $@
