# The build system where packages will be built.
{ system ? builtins.currentSystem
  # Additional sources.json overrides.
, sources ? { }
  # Additional nixpkgs.config overrides.
, config ? { }
  # Additional nixpkgs.overlays.
, overlays ? [ ]
  # The GHC version to use. (compiler-nix-name in haskell.nix)
, ghcVersion ? "ghc8104" }:

let

  pkgs = import ./nix/default.nix {
    inherit system sources config overlays ghcVersion;
  };

  project = pkgs.haskell-nix.haskellLib.selectProjectPackages pkgs.cabalProject;
  libraries = pkgs.haskell-nix.haskellLib.collectComponents' "library" project;
  checks = builtins.mapAttrs (_: p: p.checks) project;

in project // {
  ci = { inherit libraries checks; };

  shell = pkgs.cabalProject.shellFor {
    exactDeps = true;
    withHoogle = false;

    packages = ps:
      with ps; [
        amazonka
        amazonka-core
        amazonka-test
        amazonka-gen
      ];

    tools = {
      cabal = "3.2.0.0";
      cabal-fmt = "0.1.5.1";
      shellcheck = "0.7.1";
    };

    buildInputs = [
      pkgs.nixfmt
      pkgs.shfmt

      (import pkgs.sources.niv { }).niv
    ];
  };
}
