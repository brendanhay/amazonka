final: prev:

let

  cabalProject = final.cabalProject;

  ghcVersion = cabalProject.compiler-nix-name;

  hackageTool = prev.haskell-nix.tool ghcVersion;

in {
  localTools = {
    cabal-fmt = cabalProject.cabal-fmt.components.exes.cabal-fmt;
    cabal = hackageTool "cabal" "3.2.0.0";
    ormolu = hackageTool "ormolu" "0.1.3.0";
    shellcheck = hackageTool "shellcheck" "0.7.1";
  };
}
