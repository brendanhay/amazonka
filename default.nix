# The build system where packages will be _built_.
{ system ? builtins.currentSystem
  # The host system where packages will _run_.
, crossSystem ? null
  # Additional sources.json overrides.
, sources ? { }
  # Additional nixpkgs.config overrides.
, config ? { }
  # Additional nixpkgs.overlays.
, overlays ? [ ]
  # Overlays to apply to the last package set in cross compilation.
, crossOverlays ? [ ] }:

let

  pkgs = import ./nix/default.nix {
    inherit system sources config overlays crossOverlays;
  };

  compiler-nix-name = "ghc8102";

  hackageTool = pkgs.haskell-nix.tool compiler-nix-name;

  cabalProject = pkgs.haskell-nix.cabalProject {
    inherit compiler-nix-name;

    src = pkgs.haskell-nix.cleanSourceHaskell {
      name = "amazonka";
      src = ./.;
    };

    pkg-def-extras = [
      (hackage: {
        packages = {
          # Added as part of the package set due to Cabal dependency errors.
          cabal-fmt = hackage.cabal-fmt."0.1.5.1".revisions.default;
        };
      })
    ];
  };

  components = pkgs.libLocal.collectComponents cabalProject;

  tools = {
    cabal-fmt = cabalProject.cabal-fmt.components.exes.cabal-fmt;
    cabal = hackageTool "cabal" "3.2.0.0";
    ormolu = hackageTool "ormolu" "0.1.3.0";
    shellcheck = hackageTool "shellcheck" "0.7.1";
  };

in {
  inherit cabalProject;
  inherit (components) library checks exes;

  generate = pkgs.callPackage ./amazonka-gen/default.nix {
    inherit (tools) cabal-fmt ormolu;

    botocore = pkgs.sources.botocore;
    amazonka-gen = cabalProject.amazonka-gen.components.exes.amazonka-gen;

    libraryVersion = "0.0.0";
    clientVersion = "0.0.0";
    coreVersion = "0.0.0";
    models = [ "ec2" ];
  };

  shell = cabalProject.shellFor {
    withHoogle = true;
    exactDeps = true;

    packages = ps:
      with ps; [
        amazonka
        amazonka-core
        amazonka-test
        amazonka-gen
      ];

    # tools = { .. };

    buildInputs = [
      pkgs.nixfmt
      pkgs.shfmt

      tools.cabal
      # tools.cabal-fmt
      tools.ormolu
      tools.shellcheck

      # sources.json
      (import pkgs.sources.niv { }).niv
    ];
  };
}
