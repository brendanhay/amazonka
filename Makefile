DEPS := $(wildcard ./lib/amazonka-*)

.PHONY: install clean

install: $(addprefix install-,$(DEPS))

install-%:
	make -C $* install

clean: $(addprefix clean-,$(DEPS))

clean-%:
	make -C $* clean
