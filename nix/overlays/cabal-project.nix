{ ghcVersion }:

final: prev: {
  cabalProject = prev.haskell-nix.cabalProject {
    compiler-nix-name = ghcVersion;

    src = prev.haskell-nix.cleanSourceHaskell {
      name = "amazonka";
      src = ../..;
    };
  };
}
