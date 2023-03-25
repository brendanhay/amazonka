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

        # The ghc compiler version patch level will be the latest
        # that is available in nixpkgs.
        supportedGHCs = {
          ghc80 = pkgs.haskell.packages."ghc80";
          ghc90 = pkgs.haskell.packages."ghc90";
          ghc92 = pkgs.haskell.packages."ghc92";
          ghc94 = pkgs.haskell.packages."ghc94";
          ghc96 = pkgs.haskell.packages."ghc96";
        };

        ghc80 = supportedGHCs.ghc80;
        ghc90 = supportedGHCs.ghc90;
        ghc92 = supportedGHCs.ghc92;
        ghc94 = supportedGHCs.ghc94;
        ghc96 = supportedGHCs.ghc96;

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
          ghc90 = mkDevShell ghc90;
          ghc94 = mkDevShell ghc94;
          ghc96 = mkDevShell ghc96;
          default = mkDevShell ghc92;
        };
      });
}
