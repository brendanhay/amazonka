workspace(name = "amazonka")

load("//tools:repo.bzl", "versioned_http_archive")

#
# Botocore service definitions
#

# This can be auto-updated via ./scripts/update-botocore.
versioned_http_archive(
    name = "botocore",
    build_file_content = """
exports_files(glob(["**/*.json"]))
""",
    strip_prefix = "botocore-{version}/botocore/data",
    url = "https://github.com/boto/botocore/archive/{version}.tar.gz",
    version = "dfda41c08e3ed5354dce9f958b6db06e6cce99ed",
)

#
# Rule repositories
#

versioned_http_archive(
    name = "bazel_skylib",
    sha256 = "74d544d96f4a5bb630d465ca8bbcfe231e3594e5aae57e1edbf17a6eb3ca2506",
    url = "https://github.com/bazelbuild/bazel-skylib/releases/download/{version}/bazel-skylib-{version}.tar.gz",
    version = "1.3.0",
)

versioned_http_archive(
    name = "rules_pkg",
    sha256 = "8a298e832762eda1830597d64fe7db58178aa84cd5926d76d5b744d6558941c2",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_pkg/releases/download/{version}/rules_pkg-{version}.tar.gz",
        "https://github.com/bazelbuild/rules_pkg/releases/download/{version}/rules_pkg-{version}.tar.gz",
    ],
    version = "0.7.0",
)

versioned_http_archive(
    name = "rules_cc",
    sha256 = "af6cc82d87db94585bceeda2561cb8a9d55ad435318ccb4ddfee18a43580fb5d",
    strip_prefix = "rules_cc-{version}",
    urls = ["https://github.com/bazelbuild/rules_cc/releases/download/{version}/rules_cc-{version}.tar.gz"],
    version = "0.0.4",
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
    sha256 = "b01f170580f646ee3cde1ea4c117d00e561afaf3c59eda604cf09194a824ff10",
    strip_prefix = "rules_nixpkgs-{version}",
    url = "https://github.com/tweag/rules_nixpkgs/archive/refs/tags/v{version}.tar.gz",
    version = "0.9.0",
)

versioned_http_archive(
    name = "rules_haskell",
    patch_args = ["-p1"],
    patches = ["//:third_party/rules_haskell/haddock_index.patch"],
    sha256 = "aba3c16015a2363b16e2f867bdc5c792fa71c68cb97d8fe95fddc41e409d6ba8",
    strip_prefix = "rules_haskell-{version}",
    url = "https://github.com/tweag/rules_haskell/archive/refs/tags/v{version}.tar.gz",
    version = "0.15",
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
    sha256 = "099a9fb96a376ccbbb7d291ed4ecbdfd42f6bc822ab77ae6f1b5cb9e914e94fa",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v{version}/rules_go-v{version}.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v{version}/rules_go-v{version}.zip",
    ],
    version = "0.35.0",
)

versioned_http_archive(
    name = "bazel_gazelle",
    sha256 = "efbbba6ac1a4fd342d5122cbdfdb82aeb2cf2862e35022c752eaddffada7c3f3",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v{version}/bazel-gazelle-v{version}.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v{version}/bazel-gazelle-v{version}.tar.gz",
    ],
    version = "0.27.0",
)

versioned_http_archive(
    name = "com_github_bazelbuild_buildtools",
    sha256 = "e3bb0dc8b0274ea1aca75f1f8c0c835adbe589708ea89bf698069d0790701ea3",
    strip_prefix = "buildtools-{version}",
    url = "https://github.com/bazelbuild/buildtools/archive/refs/tags/{version}.tar.gz",
    version = "5.1.0",
)

versioned_http_archive(
    name = "io_tweag_gazelle_cabal",
    sha256 = "7a4f321a7634839cbdbd99ab09fd987cd6e5868a1b55388595fcb5f127221615",
    strip_prefix = "gazelle_cabal-{version}",
    url = "https://github.com/tweag/gazelle_cabal/archive/{version}.tar.gz",
    version = "a9ee2c26de58055c4ab9d97ea9d7e420445b1488",
)

#
# rules_pkg
#

load("@rules_pkg//pkg:deps.bzl", "rules_pkg_dependencies")

rules_pkg_dependencies()

#
# Nixpkgs
#

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:repositories.bzl",
    "rules_nixpkgs_dependencies",
)

rules_nixpkgs_dependencies()

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_local_repository",
    "nixpkgs_package",
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

load("@rules_cc//cc:repositories.bzl", "rules_cc_dependencies")

rules_cc_dependencies()

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
    local_snapshot = "//:third_party/stackage-snapshot.yaml",
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
        "ede",  # keep
        "errors",  # keep
        "exceptions",
        "filepath",  # keep
        "free",  # keep
        "generic-lens",
        "groom",
        "hashable",  # keep
        "haskell-src-exts",  # keep
        "http-client",
        "http-conduit",
        "http-types",
        "ini",
        "json",  # keep
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
    stack_snapshot_json = "//:third_party/stackage-snapshot.json",
    tools = [
        "@nixpkgs_alex//:bin/alex",
        "@nixpkgs_happy//:bin/happy",
    ],
)
