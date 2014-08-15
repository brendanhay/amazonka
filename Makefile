DEPS := $(wildcard lib/amazonka-*)

.PHONY: install clean

build: $(addprefix build-,$(DEPS))

build-lib/%:
	make -C lib/$* build

install: cabal.sandbox.config $(addprefix install-,$(DEPS))

install-lib/%:
	make -C lib/$* install

clean: $(addprefix clean-,$(DEPS))
	rm -rf .cabal-sandbox cabal.sandbox.config

clean-lib/%:
	make -C lib/$* clean

cabal.sandbox.config:
	cabal sandbox init && \
 cabal sandbox add-source amazonka-core
