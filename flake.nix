{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixpkgs-unstable";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    botocore = {
      url = "github:boto/botocore";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks, botocore }:
    flake-utils.lib.eachDefaultSystem (system:
      let

        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = self;
          hooks = {
            cabal-fmt.enable = true;
            nixpkgs-fmt.enable = true;
            ormolu.enable = true;
            shellcheck.enable = true;
            shfmt.enable = true;
          };
        };

        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };

        renameVersion = version: "ghc" + (pkgs.lib.replaceStrings [ "." ] [ "" ] version);

        supportedGHCs = {
          ghc902 = pkgs.haskell.packages."ghc902";
          ghc927 = pkgs.haskell.packages."ghc927";
          ghc944 = pkgs.haskell.packages."ghc944";
          ghc961 = pkgs.haskell.packages."ghc961";
        };

        ghc902 = supportedGHCs.ghc902;
        ghc927 = supportedGHCs.ghc927;
        ghc944 = supportedGHCs.ghc944;
        ghc961 = supportedGHCs.ghc961;

        mkDevShell = hsPkgs: pkgs.mkShell {
          name = "amazonka-${renameVersion hsPkgs.ghc.version}";

          buildInputs = [
            # Haskell Toolchain
            hsPkgs.ghc
            pkgs.cabal-install

            # Package Dependencies
            pkgs.gmp
            pkgs.ncurses
            pkgs.zlib

            # Development Tools
            pkgs.haskell-language-server
            pkgs.hlint
            pkgs.nixpkgs-fmt
            pkgs.ormolu
          ];

          shellHook = pre-commit.shellHook;
        };

        amazonka-gen =
          pkgs.haskellPackages.developPackage {
            root = ./gen;
            overrides = _hsFinal: hsPrev: {
              ede = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.dontHaddock hsPrev.ede);
            };
          };

        botocore-data = pkgs.stdenvNoCC.mkDerivation {
          name = "botocore";
          src = botocore;
          phases = [ "installPhase" ];
          installPhase = ''
            cp -R $src/botocore/data $out
          '';
        };

      in
      {
        apps = {
          gen = {
            type = "app";
            program = "${amazonka-gen}/bin/gen";
          };

          gen-configs = {
            type = "app";
            program = "${amazonka-gen}/bin/gen-configs";
          };
        };

        checks = {
          inherit pre-commit;
        };

        packages = {
          botocore = botocore-data;
          default = amazonka-gen;
        };

        devShells = {
          ghc902 = mkDevShell ghc902;
          ghc944 = mkDevShell ghc944;
          ghc961 = mkDevShell ghc961;
          default = mkDevShell ghc927;
        };
      });
}
