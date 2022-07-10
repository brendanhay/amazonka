self: super: {
  # haskellPackages extended/overridden with all the amazonka libraries from this repo.
  ourHaskellPackages = self.haskellPackages.override {
    overrides = hsSelf: hsSuper:
      let
        inherit (self) lib;
        # Function that returns an attribute set mapping all haskell packages in
        # the given directory to derivations for those packages.
        hsPkgsInDir = dir:
          let
            # Function that determines whether path $dir/$pkgDirName is a Haskell
            # package by checking if it's a directoy and contains a .cabal file.
            isHsPkg = pkgDirName: pkgDirType:
              let
                pkgDir = builtins.readDir (dir + "/${pkgDirName}");
                isCabalFile = pkgFileName: pkgFileType:
                  pkgFileType == "regular" &&
                  lib.hasSuffix ".cabal" pkgFileName;
              in
              pkgDirType == "directory" &&
              lib.filterAttrs isCabalFile pkgDir != { };
            hsPkgs = lib.filterAttrs isHsPkg (builtins.readDir dir);
            toHsPkg = pkgDirName: _pkgDirType:
              hsSelf.callCabal2nix pkgDirName (dir + "/${pkgDirName}") { };
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

  # Override MinIO to use the latest release.
  minio = super.callPackage ./minio { };
}
