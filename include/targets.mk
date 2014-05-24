default:

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: cabal.sandbox.config
	cabal install $(FLAGS)

clean:
	cabal clean

cabal.sandbox.config:
	ln -fs $(TOP)/cabal.sandbox.config
