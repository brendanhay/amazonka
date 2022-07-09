self: super: {
  ourHaskellPackages = self.haskellPackages.override {
    overrides = hsSelf: hsSuper:
      let
        inherit (self) lib;
        hsPkgsInDir = dir:
          let
            isHsPkg = n: t:
              let
                pkgDir = builtins.readDir (dir + "/${n}");
                isCabalFile = pkgFileName: pkgFileType:
                  pkgFileType == "regular" &&
                  lib.hasSuffix ".cabal" pkgFileName;
              in
              t == "directory" &&
              lib.filterAttrs isCabalFile pkgDir != { };
            hsPkgs = lib.filterAttrs isHsPkg (builtins.readDir dir);
            toHsPkg = n: _t: hsSelf.callCabal2nix n (dir + "/${n}") { };
          in
            lib.mapAttrs toHsPkg hsPkgs;
      in
      hsPkgsInDir ../lib //
      hsPkgsInDir ../lib/services //
      hsPkgsInDir ../tests;
  };
  amazonka-s3-test-app =
    self.haskell.lib.justStaticExecutables
      self.ourHaskellPackages.amazonka-s3-test-app;
}
