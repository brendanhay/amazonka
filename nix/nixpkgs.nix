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

      # Set the default ghc to our specified ghcVersion.
      # This attribute is used in shell.nix for development/ghci.
      ghc = prev.haskell.compiler.${ghcVersion};

      # Override the haskellPackages attribute to match the ghc version,
      # so we don't potentially download multiple compiler versions when
      # using this attribute to obtain haskell tools like cabal-fmt.
      haskellPackages = prev.haskellPackages.override { inherit ghc; };

      cabal-fmt = haskellPackages.cabal-fmt;

      bazel = let
        # Default the ghc toolchain used by bazel to the same ghcVersion.
        bazelrc = prev.writeText "amazonka-bazelrc" ''
          build --host_platform=//tools/platforms:${ghcVersion}
        '';
      in prev.writeScriptBin "bazel" (''
        #!${prev.bash}/bin/bash
      '' + prev.lib.optionalString (prev.buildPlatform.libc == "glibc") ''
        export LOCALE_ARCHIVE="${prev.glibcLocales}/lib/locale/locale-archive"
      '' + ''
        exec ${prev.bazel_4}/bin/bazel --bazelrc "${bazelrc}" "$@"
      '');
    })
  ];
}
