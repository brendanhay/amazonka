# [Amazonka]

[![MPL2][license-badge]][license]
[![Hackage][hackage-badge]][hackage]
[![Build][build-badge]][actions]

[license]: https://opensource.org/licenses/MPL-2.0
[actions]: https://github.com/brendanhay/amazonka/actions
[license-badge]: https://img.shields.io/badge/license-MPL%202.0-blue.svg
[build-badge]: https://github.com/brendanhay/amazonka/workflows/build/badge.svg
[hackage]: http://hackage.haskell.org/package/amazonka
[hackage-badge]: https://img.shields.io/hackage/v/amazonka.svg
[amazonka]: https://www.brendanhay.nz/amazonka
[nix]: https://nixos.org/nix/
[bazel]: https://bazel.build
[bazel's label]: https://docs.bazel.build/versions/4.1.0/build-ref.html#labels
[ghc]: https://www.haskell.org/ghc/
[direnv]: https://direnv.net
[direnv wiki]: https://github.com/direnv/direnv/wiki
[hermetic]: https://sre.google/sre-book/release-engineering/#hermetic-builds-nqslhnid
[lorri]: https://github.com/nix-community/lorri
[botocore]: https://github.com/boto/botocore

An Amazon Web Services SDK for Haskell with support for most public services. Parts of the code contained in this repository are auto-generated and automatically kept up to date with Amazon's latest service APIs.

- You can find the latest Haddock documentation for each respective library on the [Amazonka] website.
- A release changelog can be found in [lib/amazonka/CHANGELOG.md](lib/amazonka/CHANGELOG.md).
- For problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).

* [License](#license)
* [Directory Layout](#directory-layout)
* [Supported Platforms and GHC Versions](#supported-platforms-and-ghc-versions)
* [Getting Started](#getting-started)
* [Building the Project](#building-the-project)
* [Building the Documentation](#building-the-documentation)
* [Running the Code Generator](#running-the-code-generator)
* [Code Formatting](#code-formatting)
* [Third Party Packages](#third-party-packages)

## License

Amazonka is licensed under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

The AWS service descriptions are licensed under Apache 2.0. Source files derived from the service descriptions contain an additional licensing clause in their header.

## Directory Layout

This repository is organised into the following directory structure:

- [`lib/amazonka`](lib/amazonka): The main library containing setup, authentication, and send logic. This will be your primary dependency.
- `lib/service/amazonka-*`: A library per supported Amazon Web Service, you'll need to add a dependency on each selected service library.
- [`lib/amazonka-core`](lib/amazonka-core): The `amazonka-core` library upon which each of the services depends.
- [`lib/amazonka-test`](lib/amazonka-test): Common test functionality.
- [`examples`](examples): Basic examples for using the service libraries.
- [`configs`](configs): Service configuration, templates, and assets used by the code generator.
- [`docs`](docs): The website documentation and related build code.
- [`gen`](gen): The code and configuration generators.
- [`nix`](nix): Nix configuration code for toolchain packages.
- [`scripts`](scripts): Scripts to manage the project, such as the release lifecycle.
- [`tools`](tools): Custom bazel rules.
- [`third_party`](third_party): Third party packages and patches.

## Supported Platforms and GHC Versions

GHC versions `8.8.4` and `8.10.7` are officially supported and tested on NixOS, Ubuntu, and macOS. GHC `8.6.5` may also work, but is not tested by our continuous integration pipeline.

## Getting Started

This repository is built using a combination of [Nix] and your choice of [Bazel] or Cabal. If you're just using Amazonka as a git dependency in your Cabal or Stack project, you can skip these steps. But if you plan on contributing to the codebase - welcome, read on!

### 1. Clone this repository

```
git clone git@github.com:brendanhay/amazonka.git
cd amazonka
```

### 2. Setup Nix

Building the code in this repository requires various development dependencies (e.g. [Nix], [Bazel], [GHC].)

The [Nix] package manager is used to obtain and build the other dependencies in a [hermetic] environment. You can install Nix by following the [official installation instructions](https://nixos.org/guides/install-nix.html):

```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
```

Once Nix is setup, you can enable the [cache](https://amazonka.cachix.org) to avoid building dependencies:

```bash
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use amazonka
```

### 3. Enter a Nix Shell

The build tools are installed and activated upon entering a [Nix] shell, which is achieved by running the following command in the root of the repository:

```bash
nix-shell
```

You can also enter a shell and explicitly specify the GHC version:

```bash
nix-shell --argstr ghcVersion 884
```

Optionally, if you have [Direnv] and [lorri] installed you can use the provided [.envrc](.envrc) instead, which will also add the [scripts](scripts) directory to your `PATH`. You can extend this by adding your own uncommitted `.envrc.local` file. See the [Direnv Wiki] for various recipes.

## Building the Project

> The following commands assume you're already in a nix-shell outlined in the previous step.

### Cabal

If you're familiar with Cabal, you can build `amazonka-*` packages via:

```bash
cabal build amazonka amazonka-s3
```

Or the entire project (which will take a very long time!):

```bash
cabal build all
```

### Bazel

Alternatively, if you plan on contributing to the project or want to perform code generation, you will need to familiarise yourself with [Bazel]. You can build packages by specifying one or more targets using [Bazel's label] syntax:

```bash
bazel build //lib/amazonka //lib/services/amazonka-s3
```

Or build all Haskell libraries in the project using the `...` wildcard:

```bash
bazel build //lib/...
```

To view what targets are available in the workspace:

```bash
bazel query //...
```

> By default, the `bazel` command will use the same GHC version as the Nix shell's `ghcVersion` argument. You can choose a different GHC version using `nix-shell --argstr ghcVersion 884` - which is just a synonym for `bazel build --//tools/ghc:version=884`.

## Building the Documentation

The [docs](docs) Bazel package contains the Haddock target and Hugo static site definition and markdown content. To build the site locally, run:

```bash
bazel build //docs:bundle
```

Alternatively, you can serve the documentation site locally on `http://localhost:1313` by running:

```bash
bazel run //docs:serve
```

## Running the Code Generator

The [gen](gen) Bazel package contains code generators for synthesising Haskell data types, packages, and configuration from the botocore service definitions.

[scripts/generate](scripts/generate) will run the code generator for all services configured in [config/services](config/services), for example:

```bash
./scripts/generate
```

Or, you can selectively run the generator on one or more services:

```bash
./scripts/generate ec2 s3 iam
```

To update the [botocore](botocore) service definitions used by the generator, you can run:

```bash
./scripts/update-botocore
```

[scripts/generate-configs](scripts/generate-configs) will run the config generator to produce placeholder [config/serivces](config/services) configurations for the version of botocore pinned in the [WORKSPACE](WORKSPACE).

To generate any missing service configurations:

```bash
./scripts/generate-configs
```

Service configurations generated in this way are intended as examples only and the resulting `configs/services/<name>.json:libraryName` (Haskell package name) and `configs/annexes/<name>.json:serviceAbbreviation` (Haskell package namespace) should be manually verified and curated as necessary.

For pull requests which affect generated output please _do not include_ the regenerated `amazonka-*` packages, only commit updates to the build rules, documentation, generator, and related configuration. This ensures the Continuous Integration process is the single source of truth for the generated code and reduces noise in pull requests, keeping them reviewable and focused on actual generator code/logic changes.

## Code Formatting

Please use `./scripts/format` frequently - it's OK, I hate 2 spaces too, we're in this together.

## Third Party Packages

When naming an additional library which provides supplemental functionality to `amazonka`, if you want to use the `amazonka-*` namespace, then please consider prefixing your package names with `amazonka-contrib-*`. For example, [amazonka-contrib-rds-utils](https://hackage.haskell.org/package/amazonka-contrib-rds-utils).

This minimises potential future collisions with auto-generated package names and new AWS service and product releases.
