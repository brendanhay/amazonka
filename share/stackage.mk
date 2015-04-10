ifndef TOP
$(error variable TOP is not set)
endif

CABAL_CONFIG     := $(TOP)/share/cabal.config
STACKAGE_NIGHTLY ?= $(shell date -d "yesterday 13:00 " '+%Y-%m-%d')

cabal.config: $(CABAL_CONFIG)
	ln -fs $(TOP)/share/cabal.config $@

$(CABAL_CONFIG):
	curl -s \
 https://www.stackage.org/snapshot/nightly-$(STACKAGE_NIGHTLY)/cabal.config > $(TOP)/share/cabal.config
