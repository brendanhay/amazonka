DEPS := $(wildcard amazonka-*)

.PHONY: install clean

build: $(addprefix build-,$(DEPS)) build-amazonka

build-%:
	make -C $* build

install: cabal.sandbox.config $(addprefix install-,$(DEPS)) install-amazonka

install-%:
	make -C $* install

clean: $(addprefix clean-,$(DEPS)) clean-amazonka
#	rm -rf .cabal-sandbox cabal.sandbox.config

clean-%:
	make -C $* clean

cabal.sandbox.config:
	cabal sandbox init
