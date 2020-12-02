final: prev:

let

  haskell-nix = prev.haskell-nix // {
    toolPackageName = prev.haskell-nix.toolPackageName // {
      shellcheck = "ShellCheck";
    };
  };

  ghcVersion = "ghc8102";

  hackageTool = haskell-nix.tool ghcVersion;

  cabalProject = haskell-nix.cabalProject {
    compiler-nix-name = ghcVersion;

    src = haskell-nix.cleanSourceHaskell {
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
  };

in {
  inherit haskell-nix cabalProject;

  tools = {
    cabal-fmt = cabalProject.cabal-fmt.components.exes.cabal-fmt;
    cabal = hackageTool "cabal" "3.2.0.0";
    ormolu = hackageTool "ormolu" "0.1.3.0";
    shellcheck = hackageTool "shellcheck" "0.7.1";
  };
}
