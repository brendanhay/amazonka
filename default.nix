# The build system where packages will be built.
{ system ? builtins.currentSystem
  # The host system where packages will run.
, crossSystem ? null
  # Additional sources.json overrides.
, sources ? { }
  # Additional nixpkgs.config overrides.
, config ? { }
  # Additional nixpkgs.overlays.
, overlays ? [ ]
  # Overlays to apply to the last package set in cross compilation.
, crossOverlays ? [ ]
  # The GHC version to use. (compiler-nix-name in haskell.nix)
, ghcVersion ? "ghc8102" }:

let

  pkgs = import ./nix/default.nix {
    inherit system sources config overlays crossOverlays ghcVersion;
  };

  inherit (pkgs) localLib localTools cabalProject;

  components = member:
    localLib.collectProjectComponents (n:
      member (builtins.elem n [
        "amazonka"
        "amazonka-core"
        "amazonka-test"
        "amazonka-gen"
      ])) cabalProject;

in cabalProject // {
  workflow = {
    core = components pkgs.lib.id;
    libs = components (core: !core);
  };

  shell = cabalProject.shellFor {
    exactDeps = true;
    withHoogle = false;

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

      localTools.cabal
      localTools.cabal-fmt
      localTools.ormolu
      localTools.shellcheck

      (import pkgs.sources.niv { }).niv
    ];
  };
}
