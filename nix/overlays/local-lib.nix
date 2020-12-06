final: prev: {
  localLib = {
    cleanGeneratedSource = { src, name ? null }:
      let
        clean = prev.lib.cleanSourceWith {
          inherit name;

          src = prev.lib.cleanSource src;
          filter = path: type:
            baseNameOf (toString path) != "gen"
            && prev.haskell-nix.haskellSourceFilter path type;
        };
      in if (builtins.typeOf src) == "path" then clean else src;

    collectProjectComponents = predicate: project:
      let

        # These functions pull out from the Haskell project either all the
        # components of a particular type, or all the checks.

        pkgs = prev.haskell-nix.haskellLib.selectProjectPackages project;

        # recurseIntoAttrs is used so we can (more) easily run checks on CI.
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
