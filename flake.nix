{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    botocore = {
      # Lock botocore until we fix https://github.com/issue/888
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
        ghc96 = pkgs.haskell.packages."ghc96";
        ghc98 = pkgs.haskell.packages."ghc98";
        ghc910 = pkgs.haskell.packages."ghc910";
        ghc912 = pkgs.haskell.packages."ghc912";
        ghc914 = pkgs.haskell.packages."ghc914";

        # The default ghc to use when entering `nix develop`.
        ghcDefault = ghc914;

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
            pkgs.pkg-config
            pkgs.zlib

            # Development Tools
            pkgs.haskellPackages.cabal-fmt
            pkgs.haskell-language-server
            pkgs.hlint
            pkgs.nixpkgs-fmt
            pkgs.ormolu

            # Releases
            pkgs.gh

            pkgs.parallel
          ];

          shellHook = pre-commit.shellHook + ''
            export BOTOCORE=${botocore.outPath}
            echo "botocore: $BOTOCORE"
          '';
        };

      in
      {
        checks = {
          inherit pre-commit;
        };

        devShells = {
          ghc96 = mkDevShell ghc96;
          ghc98 = mkDevShell ghc98;
          ghc910 = mkDevShell ghc910;
          ghc912 = mkDevShell ghc912;
          ghc914 = mkDevShell ghc914;
          default = mkDevShell ghcDefault;
        };
      });
}
