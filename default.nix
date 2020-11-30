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

  components = pkgs.libLocal.collectHaskellComponents pkgs.cabalProject;

in {
  inherit (pkgs) cabalProject;
  inherit (components) library checks exes;

  generate =
    pkgs.callPackage ./amazonka-gen/default.nix {
      botocore = pkgs.sources.botocore;
      amazonka = pkgs.cabalProject.amazonka;
      amazonka-core = pkgs.cabalProject.amazonka-core;
      amazonka-gen = pkgs.cabalProject.amazonka-gen;
      models = [
        "ec2" 
      ];
    };

  shell = pkgs.cabalProject.shellFor {
    withHoogle = true;
    exactDeps = true;

    packages = ps: 
      with ps; [
        amazonka
        amazonka-core
        amazonka-test
        amazonka-gen
      ];

      # pkgs.lib.attrValues ps;

    tools = {
      cabal = "3.2.0.0";
      ormolu = "0.1.3.0";
      shellcheck = "0.7.1";
    };

    buildInputs = [
      pkgs.nixfmt
      pkgs.shfmt

      # sources.json
      (import pkgs.sources.niv { }).niv
    ];
  };
}
