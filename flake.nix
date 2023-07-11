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
      # Lock botocore until we https://github.com/issue/888 is fixed
      url = "github:boto/botocore/f14ab129706a99198d42eed78d75350ea61c48e9";
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
            prettier = {
              enable = true;
              files = "\\.json$";
            };
          };
        };

        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };

        # The ghc compiler version patch level will be the latest that is available in nixpkgs.
        ghc810 = pkgs.haskell.packages."ghc810";
        ghc90 = pkgs.haskell.packages."ghc90";
        ghc92 = pkgs.haskell.packages."ghc92";
        ghc94 = pkgs.haskell.packages."ghc94";
        ghc96 = pkgs.haskell.packages."ghc96";

        # The default ghc to use when entering `nix develop`.
        ghcDefault = ghc94;

        renameVersion = version: "ghc" + (pkgs.lib.replaceStrings [ "." ] [ "" ] version);

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
            pkgs.haskellPackages.cabal-fmt
            pkgs.haskell-language-server
            pkgs.hlint
            pkgs.nixpkgs-fmt

            # Releases
            pkgs.gh

            # This is regrettable (long build times), but ormolu
            # 0.5.0.1 generates really ugly pyramids of `Prelude.seq`
            # or `Prelude.hashWithSalt` applications when generating code.
            #
            # When this assertion fails, check if pkgs.ormolu is newer
            # than 0.7.0.0 and see if we can switch back to it.
            (
              assert (pkgs.ormolu.version == "0.5.0.1");
              pkgs.haskell.packages.ghc96.ormolu_0_7_0_0
            )

            pkgs.parallel
          ];

          shellHook = pre-commit.shellHook + ''
            export BOTOCORE=${botocore.outPath}
            echo "botocore: $BOTOCORE"
          '';
        };

        amazonka-gen =
          # Use ghc92 because we want hashable ==1.3.* for actual
          # generation and the ghc-bignum dep is inside a conditional,
          # so doJailbreak won't work.
          #
          # We need hashable-1.3 for generation because hashable >=1.4
          # uses a different hashing algorithm which breaks things by
          # causing the contents of `HashMap`s to be traversed in a
          # slightly different order. This matters when `Ptr`s are
          # used to resolve recursive shape references.
          ghc92.developPackage {
            root = ./gen;
            overrides = _hsFinal: hsPrev: with pkgs.haskell.lib; {
              ede = dontCheck (dontHaddock hsPrev.ede);
              hashable = hsPrev.callHackage "hashable" "1.3.5.0" { };
              pandoc = dontHaddock hsPrev.pandoc;
              string-qq = dontCheck hsPrev.string-qq;
            };
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
          default = amazonka-gen;
        };

        devShells = {
          ghc810 = mkDevShell ghc810;
          ghc90 = mkDevShell ghc90;
          ghc92 = mkDevShell ghc92;
          ghc94 = mkDevShell ghc94;
          ghc96 = mkDevShell ghc96;
          default = mkDevShell ghcDefault;
        };
      });
}
