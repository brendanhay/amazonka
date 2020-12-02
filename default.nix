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
, crossOverlays ? [ ] }:

let

  pkgs = import ./nix/default.nix {
    inherit system sources config overlays crossOverlays;
  };

  inherit (pkgs) libLocal cabalProject tools;

  components = libLocal.collectProjectComponents cabalProject;

in {
  inherit cabalProject;
  inherit (components) library exes checks;

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

      tools.cabal
      tools.cabal-fmt
      tools.ormolu
      tools.shellcheck

      (import pkgs.sources.niv { }).niv
    ];
  };
}
