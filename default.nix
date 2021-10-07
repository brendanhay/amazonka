# The build system where packages will be built.
{ system ? builtins.currentSystem
  # Additional sources.json overrides.
, sources ? { }
  # Additional nixpkgs.config overrides.
, config ? { }
  # Additional nixpkgs.overlays.
, overlays ? [ ]
  # The GHC version to use. (compiler-nix-name in haskell.nix)
, ghcVersion ? "ghc8107" }:

let

  pkgs = import ./nix/default.nix {
    inherit system sources config overlays ghcVersion;
  };

  inherit (pkgs.haskell-nix) haskellLib;

  packages = haskellLib.selectProjectPackages pkgs.cabalProject;

in pkgs.cabalProject // {
  ci = {
    libraries = haskellLib.collectComponents' "library" packages;
    checks = builtins.mapAttrs (_: p: p.checks) packages;
  };

  shell = pkgs.cabalProject.shellFor {
    exactDeps = true;
    withHoogle = false;

    packages = ps: with ps; [ amazonka amazonka-test amazonka-gen ];

    tools = {
      cabal-fmt = {
        inherit (pkgs.cabalProject) index-state;
        version = "0.1.5.1";
      };
    };

    buildInputs = [
      pkgs.coreutils
      pkgs.nixfmt
      pkgs.ormolu
      pkgs.parallel
      pkgs.shellcheck
      pkgs.shfmt

      (import pkgs.sources.niv { }).niv
    ];

    shellHook = ''
      export BOTOCORE_PATH='${pkgs.sources.botocore}/botocore/data';
    '';
  };
}
