workspace(name = "amazonka")

load("//tools:repo.bzl", "versioned_http_archive")

#
# Botocore service definitions
#

# This can be auto-updated via ./bin/update-botocore.
versioned_http_archive(
    name = "botocore",
    build_file_content = """
exports_files(glob(["**/*.json"]))
""",
    strip_prefix = "botocore-{version}/botocore/data",
    url = "https://github.com/boto/botocore/archive/{version}.tar.gz",
    version = "f759d4eb138a5b969c23f586c6238d9e7c20cba0",
)

#
# Rule repositories
#

versioned_http_archive(
    name = "bazel_skylib",
    sha256 = "1dde365491125a3db70731e25658dfdd3bc5dbdfd11b840b3e987ecf043c7ca0",
    url = "https://github.com/bazelbuild/bazel-skylib/releases/download/{version}/bazel_skylib-{version}.tar.gz",
    version = "0.9.0",
)

versioned_http_archive(
    name = "rules_pkg",
    sha256 = "dd13c5581146da6abdee49a1a2605cd1dd8fb39bea9a870e0089aa4066b260b6",
    strip_prefix = "rules_pkg-{version}/pkg",
    url = "https://github.com/bazelbuild/rules_pkg/archive/{version}.tar.gz",
    version = "8d542763a3959db79175404758f46c7f3f385fa5",
)

versioned_http_archive(
    name = "rules_sh",
    sha256 = "83a065ba6469135a35786eb741e17d50f360ca92ab2897857475ab17c0d29931",
    strip_prefix = "rules_sh-{version}",
    url = "https://github.com/tweag/rules_sh/archive/v{version}.tar.gz",
    version = "0.2.0",
)

versioned_http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = "33fd540d0283cf9956d0a5a640acb1430c81539a84069114beaf9640c96d221a",
    strip_prefix = "rules_nixpkgs-{version}",
    url = "https://github.com/tweag/rules_nixpkgs/archive/{version}.tar.gz",
    version = "81f61c4b5afcf50665b7073f7fce4c1755b4b9a3",
)

versioned_http_archive(
    name = "rules_haskell",
    patch_args = ["-p1"],
    patches = ["//:third_party/rules_haskell/haddock_index.patch"],
    sha256 = "73941c142bf37df115817083c0834fbc3269c963c7049bfbc345a9c76162e918",
    strip_prefix = "rules_haskell-{version}",
    url = "https://github.com/tweag/rules_haskell/archive/{version}.tar.gz",
    version = "ea0e70ace2432a490d4ab4c4e54617612466e584",
)

versioned_http_archive(
    name = "com_google_protobuf",
    sha256 = "a79d19dcdf9139fa4b81206e318e33d245c4c9da1ffed21c87288ed4380426f9",
    strip_prefix = "protobuf-{version}",
    url = "https://github.com/protocolbuffers/protobuf/archive/v{version}.tar.gz",
    version = "3.11.4",
)

versioned_http_archive(
    name = "io_bazel_rules_go",
    sha256 = "69de5c704a05ff37862f7e0f5534d4f479418afc21806c887db544a316f3cb6b",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v{version}/rules_go-v{version}.tar.gz",
        "https://github.com/bazelbuild/rules_go/releases/download/v{version}/rules_go-v{version}.tar.gz",
    ],
    version = "0.27.0",
)

versioned_http_archive(
    name = "bazel_gazelle",
    sha256 = "62ca106be173579c0a167deb23358fdfe71ffa1e4cfdddf5582af26520f1c66f",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v{version}/bazel-gazelle-v{version}.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v{version}/bazel-gazelle-v{version}.tar.gz",
    ],
    version = "0.23.0",
)

versioned_http_archive(
    name = "com_github_bazelbuild_buildtools",
    sha256 = "143ef233b81286470a14d77e57352cec682a642831423af883e5744e110af642",
    strip_prefix = "buildtools-{version}",
    url = "https://github.com/bazelbuild/buildtools/archive/{version}.tar.gz",
    version = "d6daef01a1a2f41a4143a314bf1996bf351caa30",
)

versioned_http_archive(
    name = "io_tweag_gazelle_cabal",
    sha256 = "65712e680ebb9214b7fecec1c5e4a380df1c4840b508866a0f5a37a82f87a687",
    strip_prefix = "gazelle_cabal-{version}",
    url = "https://github.com/tweag/gazelle_cabal/archive/{version}.tar.gz",
    version = "fbf32ca7344f950e6a79017d80569e7b4b7b540b",
)

#
# Nixpkgs
#

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_local_repository",
    "nixpkgs_package",
)
load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_python_configure",
)

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//:nix/nixpkgs.nix",
    nix_file_deps = [
        "//:nix/sources.json",
        "//:nix/sources.nix",
    ],
)

nixpkgs_cc_configure(repository = "@nixpkgs")

nixpkgs_python_configure(repository = "@nixpkgs")

#
# POSIX + Shell
#

load("@rules_sh//sh:repositories.bzl", "rules_sh_dependencies")
load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_sh_posix_configure")
load("@rules_sh//sh:posix.bzl", "sh_posix_configure")

rules_sh_dependencies()

nixpkgs_sh_posix_configure(repository = "@nixpkgs")

sh_posix_configure()

#
# Go
#

load("@io_bazel_rules_go//go:deps.bzl", "go_rules_dependencies")
load("@io_tweag_rules_nixpkgs//nixpkgs:toolchains/go.bzl", "nixpkgs_go_configure")
load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")

nixpkgs_go_configure(repository = "@nixpkgs")

go_rules_dependencies()

gazelle_dependencies()

#
# Nixpkgs
#

nixpkgs_package(
    name = "nixpkgs_happy",
    attribute_path = "haskellPackages.happy",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "nixpkgs_alex",
    attribute_path = "haskellPackages.alex",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "nixpkgs_zlib",
    attribute_path = "zlib",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "nixpkgs_hugo",
    attribute_path = "hugo",
    repository = "@nixpkgs",
)

#
# System/C dependencies
#

nixpkgs_package(
    name = "zlib.dev",
    build_file_content = """
load("@rules_cc//cc:defs.bzl", "cc_library")
filegroup(
    name = "include",
    srcs = glob(["include/*.h"]),
    visibility = ["//visibility:public"],
)
cc_library(
    name = "zlib",
    srcs = ["@nixpkgs_zlib//:lib"],
    hdrs = [":include"],
    strip_include_prefix = "include",
    visibility = ["//visibility:public"],
    linkstatic = 1,
)
""",
    repository = "@nixpkgs",
)

#
# Haskell setup
#

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")
load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")
load("@io_tweag_gazelle_cabal//:defs.bzl", "gazelle_cabal_dependencies")

rules_haskell_dependencies()

# Register custom toolchain prior to rules_haskell own toolchain registration,
# as Bazel takes precedence into account when determining the toolchain to use
# thereby allowing us to override the builtin rules_haskell one.
#
# See: https://github.com/tweag/rules_haskell/issues/1602#issuecomment-938675602
register_toolchains("//tools/ghc:toolchain")

haskell_register_ghc_nixpkgs(
    name = "ghc921",
    attribute_path = "haskell.compiler.ghc921",
    haddock_flags = [
        "--no-warnings",
    ],
    repository = "@nixpkgs",
    version = "9.2.1",
)

haskell_register_ghc_nixpkgs(
    name = "ghc901",
    attribute_path = "haskell.compiler.ghc901",
    haddock_flags = [
        "--no-warnings",
    ],
    repository = "@nixpkgs",
    version = "9.0.1",
)

haskell_register_ghc_nixpkgs(
    name = "ghc8107",
    attribute_path = "haskell.compiler.ghc8107",
    haddock_flags = [
        "--no-warnings",
    ],
    repository = "@nixpkgs",
    version = "8.10.7",
)

haskell_register_ghc_nixpkgs(
    name = "ghc884",
    attribute_path = "haskell.compiler.ghc884",
    haddock_flags = [
        "--no-warnings",
    ],
    repository = "@nixpkgs",
    version = "8.8.4",
)

gazelle_cabal_dependencies()

stack_snapshot(
    name = "stackage",
    components = {
        "pandoc": [
            "lib",
            "exe",
        ],
    },
    extra_deps = {
        "zlib": ["@zlib.dev//:zlib"],
        "digest": ["@zlib.dev//:zlib"],
    },
    # The keep comments per package line prevents `bazel run //:gazelle-update-repos`
    # from pruning packages that are used by hand-written `haskell_library` rules,
    # such as ./gen and ./docs.
    packages = [
        "QuickCheck",
        "aeson",  # keep
        "attoparsec",  # keep
        "base",
        "bifunctors",
        "bytestring",  # keep
        "cabal-doctest",  # keep
        "case-insensitive",  # keep
        "comonad",  # keep
        "conduit",
        "conduit-extra",
        "containers",  # keep
        "cryptonite",
        "data-ordlist",
        "deepseq",
        "deriving-compat",  # keep
        "directory",
        "directory-tree",  # keep
        "ede",
        "ede-0.3.2.0",  # keep
        "errors",  # keep
        "exceptions",
        "filepath",  # keep
        "free",  # keep
        "generic-lens",
        "groom",
        "hashable",
        "hashable-1.3.4.1",  # keep
        "haskell-src-exts",  # keep
        "http-client",
        "http-conduit",
        "http-types",
        "ini",
        "lens",  # keep
        "memory",
        "mtl",  # keep
        "optparse-applicative",  # keep
        "pandoc",  # keep
        "path",  # keep
        "path-io",  # keep
        "process",
        "quickcheck-instances",
        "quickcheck-unicode",
        "regex-posix",
        "resourcet",
        "retry",
        "scientific",  # keep
        "semigroups",
        "tasty",
        "tasty-hunit",
        "tasty-quickcheck",
        "template-haskell",
        "temporary",
        "text",  # keep
        "time",  # keep
        "transformers",  # keep
        "unexceptionalio",  # keep
        "unliftio",  # keep
        "unordered-containers",  # keep
        "uuid",
        "vector",
        "xml-conduit",
        "xml-types",
        "yaml",
    ],
    setup_deps = {
        "xml-conduit": ["@stackage//:cabal-doctest"],
    },
    snapshot = "lts-18.23",
    stack_snapshot_json = "//:third_party/stackage-snapshot.json",
    tools = [
        "@nixpkgs_alex//:bin/alex",
        "@nixpkgs_happy//:bin/happy",
    ],
)
