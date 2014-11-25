DEPS := $(wildcard amazonka-*)

.PHONY: install clean

build: $(addprefix build-,$(DEPS)) build-amazonka

build-%:
	@make -C $* build

install: cabal.sandbox.config $(addprefix install-,$(DEPS)) install-amazonka

install-%:
	@make -C $* install

clean: $(addprefix clean-,$(DEPS)) clean-amazonka

clean-%:
	@make -C $* clean

sdist: $(addprefix sdist-,$(DEPS)) sdist-amazonka sdist-core

sdist-%:
	@make -C $* sdist

upload: $(addprefix upload-,$(DEPS)) upload-amazonka upload-core

upload-%:
	@make -C $* upload

cabal.sandbox.config:
	cabal sandbox init

test:
	@make -C core test
