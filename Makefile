all: build

build: .conf
	cabal-dev build

install:
	cabal-meta install \
	 --dev \
	 --disable-documentation \
	 --disable-library-coverage

clean:
	-rm -rf .conf bin dist .shelly
	cabal-dev clean

lint:
	hlint src

.conf:
	cabal-dev configure && touch .conf
