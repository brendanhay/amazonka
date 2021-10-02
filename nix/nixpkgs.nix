{
# The build system where packages will be _built_.
system ? builtins.currentSystem
  # Additional nixpkgs.config overrides.
, config ? { }
  # Additional nixpkgs.overlays.
, overlays ? [ ]
  # Source repositories.
, sources ? import ./sources.nix { }
  # The default version of GHC to use for shell tools.
, ghcVersion ? "ghc8107" }:

import sources.nixpkgs {
  inherit system config;

  overlays = overlays ++ [
    (final: prev: rec {
      inherit sources;

      # Set the top-level ghc attribute to the specified ghcVersion.
      # This attribute is used in shell.nix for cabal development + ghci.
      ghc = prev.haskell.compiler.${ghcVersion};

      bazel = let
        # Set the ghc toolchain used by bazel to the ghcVersion used by nix-shell.
        bazelrc = prev.writeText "amazonka-bazelrc" ''
          build --host_platform=//tools/platforms:${ghcVersion}
        '';
      in prev.writeScriptBin "bazel" ''
        #!${prev.bash}/bin/bash
        export JAVA_HOME="${prev.jdk11_headless.home}"
        exec ${prev.bazel_4}/bin/bazel --bazelrc "${bazelrc}" "$@"
      '';
    })
  ];
}
