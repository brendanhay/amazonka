final: prev:

let

  compiler-nix-name = "ghc8102";

  cabalProject = prev.haskell-nix.cabalProject {
    inherit compiler-nix-name;

    src = prev.haskell-nix.cleanSourceHaskell {
      name = "amazonka";
      src = ../..;
    };

    pkg-def-extras = [ ];

    modules = [{
      packages = {
        # haskell.nix sets dontStrip = true by default, causing large closure sizes.
        amazonka.dontStrip = false;
      };

      # Ensures that we can depend on the installed GHC's boot libraries
      # when using ghc-exactprint or similar libraries that depend on the GHC API.
      nonReinstallablePkgs = [
        "Cabal"
        "Win32"
        "array"
        "base"
        "binary"
        "bytestring"
        "containers"
        "deepseq"
        "directory"
        "exceptions"
        "filepath"
        "ghc"
        "ghc-boot"
        "ghc-boot-th"
        "ghc-compact"
        "ghc-heap"
        "ghc-prim"
        "ghci"
        "ghcjs-prim"
        "ghcjs-th"
        "haskeline"
        "hpc"
        "integer-gmp"
        "integer-simple"
        "mtl"
        "parsec"
        "pretty"
        "process"
        "rts"
        "stm"
        "template-haskell"
        "terminfo"
        "text"
        "time"
        "transformers"
        "unix"
        "xhtml"
      ];
    }];
  };

in {
  inherit cabalProject;

  haskell-nix = prev.haskell-nix // {
    toolPackageName = prev.haskell-nix.toolPackageName // {
      shellcheck = "ShellCheck";
    };
  };
}
