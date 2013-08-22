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
	rm -rf cabal-dev/share/x86_64-osx-ghc-7.6.3/aws-haskell-0.1.0/test && \
	 mkdir -p cabal-dev/share/x86_64-osx-ghc-7.6.3/aws-haskell-0.1.0/test && \
	 cp -rf test/response cabal-dev/share/x86_64-osx-ghc-7.6.3/aws-haskell-0.1.0/test/ && \
	 rm -f .conf && cabal-dev install --enable-tests

lint:
	hlint src

doc:
	cabal-dev haddock

.conf:
	cabal-dev configure && touch $@

