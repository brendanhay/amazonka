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
, ghcVersion ? "ghc8104" }:

let

  pkgs = import ./nix/default.nix {
    inherit system sources config overlays crossOverlays ghcVersion;
  };

  projectPackages =
    pkgs.haskell-nix.haskellLib.selectProjectPackages pkgs.cabalProject;

in projectPackages // {
  ci = {
    "library" =
      pkgs.haskell-nix.haskellLib.collectComponents' "library" projectPackages;
    "checks" = builtins.mapAttrs (_: p: p.checks) projectPackages;
  };

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
