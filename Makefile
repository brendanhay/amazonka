include include/common.mk

build: gen
	make -C amazonka $@

gen:
	make -C amazonka-gen $@

install: cabal.sandbox.config
	make -C amazonka-gen $@
	make -C amazonka $@

cabal.sandbox.config:
	cabal sandbox init
