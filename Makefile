DEPS := $(wildcard amazonka-*)

.PHONY: install full-clean clean

build: $(addprefix build-,$(DEPS)) build-amazonka

build-%:
	@make -C $* build

install: cabal.sandbox.config $(addprefix install-,$(DEPS)) install-amazonka

install-%:
	@make -C $* install

full-clean: clean
	rm -rf cabal.sandbox.config .cabal-sandbox

clean: $(addprefix clean-,$(DEPS)) clean-amazonka

clean-%:
	@make -C $* clean

sdist: sdist-core sdist-amazonka $(addprefix sdist-,$(DEPS))

sdist-%:
	@make -C $* sdist

upload: upload-core upload-amazonka $(addprefix upload-,$(DEPS))

upload-%:
	@make -C $* upload

cabal.sandbox.config:
	cabal sandbox init

travis:
	@make -C core test && make install && make
