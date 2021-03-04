{ ghcVersion }:

final: prev:

let

  compiler-nix-name = ghcVersion;

in {
  cabalProject = prev.haskell-nix.cabalProject {
    inherit compiler-nix-name;

    src = prev.haskell-nix.cleanSourceHaskell {
      name = "amazonka";
      src = ../..;
    };

    pkg-def-extras = [
      (hackage: {
        packages = {
          # Avoid a nonReinstallablePkgs issue when trying to obtain
          # cabal-fmt via `pkgs.haskell-nix.tool`.
          cabal-fmt = hackage.cabal-fmt."0.1.5.1".revisions.default;
        };
      })
    ];
  } // {
    inherit compiler-nix-name;
  };
}
