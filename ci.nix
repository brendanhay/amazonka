let
  sources = import ./nix/sources.nix;
  haskellNix = import sources."haskell.nix" { };
  nixpkgs = import haskellNix.sources.nixpkgs-2009 haskellNix.nixpkgsArgs;

  project = args@{ ... }:
    nixpkgs.haskell-nix.project ({
      src = nixpkgs.haskell-nix.haskellLib.cleanGit {
        name = "amazonka";
        src = ./.;
      };
    } // args);
in
{
  "cabal-865" = project { projectFileName = "cabal.project"; compiler-nix-name = "ghc865"; };
  "stack-865" = project { projectFileName = "stack-8.6.5.yaml"; };
  "stack-884" = project { projectFileName = "stack-8.8.4.yaml"; };
  "stack-8104" = project { projectFileName = "stack-8.10.4.yaml"; };
}
