TOP := ..

include $(TOP)/include/common.mk

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install:
	cabal install $(FLAGS) --only-dependencies

clean:
	-cabal clean
	rm -rf gen amazonka.cabal

doc:
	cabal haddock
