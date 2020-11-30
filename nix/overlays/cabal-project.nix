final: prev: {

cabalProject = prev.haskell-nix.cabalProject {
  compiler-nix-name = "ghc8102";

  src = prev.haskell-nix.cleanSourceHaskell {
    name = "amazonka";
    src = ../..;
  };
};

}
