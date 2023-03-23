{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixpkgs-unstable";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    flake-utils.lib.eachDefaultSystem (system:
      let

        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = self;
          hooks = {
            nixpkgs-fmt.enable = true;
            shellcheck.enable = true;
            shfmt.enable = true;
          };
        };

        pkgs = import nixpkgs { inherit system; };

        renameVersion = version: "ghc" + (pkgs.lib.replaceStrings [ "." ] [ "" ] version);

        supportedGHCs =
          let
            ghcVersion = renameVersion pkgs.haskellPackages.ghc.version;
            cases = {
              ghc902 = pkgs.haskell.packages."ghc902";
              ghc926 = pkgs.haskell.packages."ghc926";
              ghc927 = pkgs.haskell.packages."ghc927";
              ghc944 = pkgs.haskell.packages."ghc944";
              ghc961 = pkgs.haskell.packages."ghc961";
            };
          in
          { default = cases."${ghcVersion}"; } // cases;

        ghc902 = supportedGHCs.ghc902;
        ghc926 = supportedGHCs.ghc926;
        ghc944 = supportedGHCs.ghc944;
        ghc961 = supportedGHCs.ghc961;
        ghcDefault = supportedGHCs.default;

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

      in
      {
        devShells = {
          ghc902 = mkDevShell ghc902;
          ghc926 = mkDevShell ghc926;
          ghc944 = mkDevShell ghc944;
          ghc961 = mkDevShell ghc961;
          default = mkDevShell ghcDefault;
        };

        checks = {
          inherit pre-commit;
        };
      });
}
