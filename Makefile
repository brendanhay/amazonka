.PHONY: test lint doc

all: build

build: .conf
	cabal-dev build

install:
	cabal-meta install --dev -j \
	 --disable-documentation \
	 --disable-library-coverage

clean:
	-rm -rf .conf dist
	cabal-dev clean

test:
	cabal-dev configure --enable-tests && cabal-dev test

lint:
	hlint src

doc:
	cabal-dev haddock

.conf:
	cabal-dev configure && touch $@

