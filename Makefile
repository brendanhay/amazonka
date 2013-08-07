all: build

build: .conf
	cabal-dev build

install: vendor/aeson
	cabal-dev install -j \
	 --disable-documentation \
	 --disable-library-coverage

clean:
	-rm -rf .conf bin dist .shelly vendor
	cabal-dev clean

lint:
	hlint src

.conf:
	cabal-dev configure && touch .conf

vendor/aeson:
	git clone git@github.com:bos/aeson.git $@
