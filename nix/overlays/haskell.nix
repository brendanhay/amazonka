final: prev:

{
  haskell-nix = prev.haskell-nix // {
    toolPackageName = prev.haskell-nix.toolPackageName // {
      shellcheck = "ShellCheck";
    };
  };
}
