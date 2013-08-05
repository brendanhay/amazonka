all: build

build: .conf
	cabal-dev build

install:
	cabal-dev install -j \
	 --disable-documentation \
	 --disable-library-coverage

clean:
	-rm -rf .conf bin dist .shelly
	cabal-dev clean

lint:
	hlint src

.conf:
	cabal-dev configure && touch .conf
