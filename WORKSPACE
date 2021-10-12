workspace(name = "amazonka")

#
# Rule repositories
#

load("//tools:repo.bzl", "versioned_http_archive")

versioned_http_archive(
    name = "bazel_skylib",
    sha256 = "1dde365491125a3db70731e25658dfdd3bc5dbdfd11b840b3e987ecf043c7ca0",
    url = "https://github.com/bazelbuild/bazel-skylib/releases/download/{version}/bazel_skylib-{version}.tar.gz",
    version = "0.9.0",
)

versioned_http_archive(
    name = "rules_sh",
    sha256 = "83a065ba6469135a35786eb741e17d50f360ca92ab2897857475ab17c0d29931",
    strip_prefix = "rules_sh-{version}",
    url = "https://github.com/tweag/rules_sh/archive/v{version}.tar.gz",
    version = "0.2.0",
)

versioned_http_archive(
    name = "rules_pkg",
    strip_prefix = "rules_pkg-{version}/pkg",
    url = "https://github.com/bazelbuild/rules_pkg/archive/{version}.tar.gz",
    version = "8d542763a3959db79175404758f46c7f3f385fa5",
    sha256 = "dd13c5581146da6abdee49a1a2605cd1dd8fb39bea9a870e0089aa4066b260b6"
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
    strip_prefix = "gazelle_cabal-{version}",
    url = "https://github.com/brendanhay/gazelle_cabal/archive/{version}.tar.gz",
    version = "41a3f831806d5ed8b875f6f34fd133017e156e12",
    sha256 = "04c4b76c5fb3d8a8ffb283be8b35e73226bd5d985d3e0c07c83a737b146456f1"
)

versioned_http_archive(
    name = "build_stack_rules_hugo",
    patch_args = ["-p1"],
    patches = ["//third_party/rules_hugo:hugo_site_serve.patch"],
    sha256 = "a8f0ac7ba4b71f88e33899df25c222d79e42d18c561d677f91a16aa71bfdf05f",
    strip_prefix = "rules_hugo-{version}",
    url = "https://github.com/stackb/rules_hugo/archive/{version}.tar.gz",
    version = "2927451ff7fff708292eb7eb68ca278457c5dd78",
)

versioned_http_archive(
    name = "hugo_theme_learn",
    build_file_content = """
filegroup(
    name = "files",
    srcs = glob(["**/*"]),
    visibility = ["//visibility:public"],
)
""",
    sha256 = "902bf754260400fb09d3282cd615b79e62072c6ea034d38505ab30ea20aec7c6",
    strip_prefix = "hugo-theme-learn-{version}",
    url = "https://github.com/matcornic/hugo-theme-learn/archive/{version}.tar.gz",
    version = "d198cbe65f064575df1ab02415980d6e44363bf9",
)

#
# Botocore service definitions
#

versioned_http_archive(
    name = "botocore",
    build_file_content = """
exports_files(glob(["**/*.json"]))
""",
    sha256 = "1e8ab0f11f0df6b1d3bc8f5708150606becc08f20e38da1785a8ab7c0b8c232a",
    strip_prefix = "botocore-{version}/botocore/data",
    url = "https://github.com/boto/botocore/archive/{version}.tar.gz",
    version = "f1d41183e0fad31301ad7331a8962e3af6359a22",
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
load("@rules_haskell//tools:repositories.bzl", "rules_haskell_worker_dependencies")
load("@io_tweag_gazelle_cabal//:defs.bzl", "gazelle_cabal_dependencies")

rules_haskell_dependencies()

rules_haskell_worker_dependencies()

# Register custom toolchain prior to rules_haskell toolchains, as Bazel
# takes precedence into account when determining the # toolchain to use.
register_toolchains("//tools/ghc:toolchain")

haskell_register_ghc_nixpkgs(
    name = "ghc865",
    attribute_path = "haskell.compiler.ghc865",
    repository = "@nixpkgs",
    version = "8.6.5",
)

haskell_register_ghc_nixpkgs(
    name = "ghc884",
    attribute_path = "haskell.compiler.ghc884",
    repository = "@nixpkgs",
    version = "8.8.4",
)

haskell_register_ghc_nixpkgs(
    name = "ghc8107",
    attribute_path = "haskell.compiler.ghc8107",
    repository = "@nixpkgs",
    version = "8.10.7",
)

gazelle_cabal_dependencies()

stack_snapshot(
    name = "stackage",
    extra_deps = {
        "zlib": ["@zlib.dev//:zlib"],
        "digest": ["@zlib.dev//:zlib"],
    },
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
        "filepath",  # keep
        "free",  # keep
        "ghc-prim",
        "groom",
        "hashable",  # keep
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
        "quickcheck-unicode",
        "resourcet",
        "retry",
        "scientific",  # keep
        "tagged",
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
        "unliftio-core",
        "unordered-containers",  # keep
        "xml-conduit",
        "xml-types",
        "yaml",
    ],
    setup_deps = {
        "xml-conduit": ["@stackage//:cabal-doctest"],
    },
    snapshot = "lts-18.10",
    stack_snapshot_json = "//:stackage-snapshot.json",
    tools = [
        "@nixpkgs_alex//:bin/alex",
        "@nixpkgs_happy//:bin/happy",
    ],
)
