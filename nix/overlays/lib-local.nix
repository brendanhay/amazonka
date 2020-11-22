final: prev: {
  libLocal = {
    collectHaskellComponents = project:
      let

        # These functions pull out from the Haskell project either all the
        # components of a particular type, or all the checks.

        pkgs = prev.haskell-nix.haskellLib.selectProjectPackages project;

        collectChecks = _:
          prev.recurseIntoAttrs (builtins.mapAttrs (_: p: p.checks) pkgs);

        collectComponents = type:
          prev.haskell-nix.haskellLib.collectComponents' type pkgs;

        # Recompute the Haskell package set sliced by component type
      in builtins.mapAttrs (type: f: f type) {
        # These names must match the subcomponent: components.<name>.<...>
        "library" = collectComponents;
        "tests" = collectComponents;
        "benchmarks" = collectComponents;
        "exes" = collectComponents;
        "checks" = collectChecks;
      };
  };
}
