# [Amazonka]

[![MPL2][license-badge]][license]
[![Build][build-badge]][actions]
[![Generate][generate-badge]][actions]
[![Documentation][documentation-badge]][actions]
[![Hackage][hackage-badge]][hackage]
[![Cachix][cachix-badge]][cachix]

[license]: https://opensource.org/licenses/MPL-2.0
[actions]: https://github.com/brendanhay/amazonka/actions
[license-badge]: https://img.shields.io/badge/license-MPL%202.0-blue.svg
[build-badge]: https://github.com/brendanhay/amazonka/workflows/build/badge.svg
[generate-badge]: https://github.com/brendanhay/amazonka/workflows/generate/badge.svg
[documentation-badge]: https://github.com/brendanhay/amazonka/workflows/documentation/badge.svg
[hackage]: http://hackage.haskell.org/package/amazonka
[hackage-badge]: https://img.shields.io/hackage/v/amazonka.svg
[cachix]: https://amazonka.cachix.org
[cachix-badge]: https://img.shields.io/badge/cachix-amazonka-purple.svg

[Amazonka]: https://brendanhay.github.io/amazonka
[Nix]: https://nixos.org/nix/
[Bazel]: https://bazel.build
[Bazel labels]: https://docs.bazel.build/versions/4.1.0/build-ref.html#labels
[GHC]: https://www.haskell.org/ghc/
[Direnv]: https://direnv.net
[Direnv Wiki]: https://github.com/direnv/direnv/wiki
[Cachix]: https://docs.cachix.org/
[hermetic]: https://sre.google/sre-book/release-engineering/#hermetic-builds-nqslhnid
[lorri]: https://github.com/nix-community/lorri

An Amazon Web Services SDK for Haskell with support for most public services. Parts of the code contained in this repository are auto-generated and automatically kept up to date with Amazon's latest service APIs.

* You can find the latest Haddock documentation for each respective library on the [Amazonka] website.
* A release changelog can be found in [amazonka/CHANGELOG.md](amazonka/CHANGELOG.md).
* For problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).

- [License](#license)
- [Supported Platforms and GHC Versions](#supported-platforms-and-ghc-versions)
- [Contributing](#contributing)
    - [Getting Started](#getting-started)
    - [Running the Generator](#running-the-generator)
    - [Code Formatting](#code-formatting)
    - [Directory Layout](#directory-layout)
    - [Third Party Package Naming](#third-party-package-naming)

## Licence

Amazonka is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0. Source files subject to this contain an additional licensing clause in their header.

## Supported Platforms and GHC Versions

GHC versions `8.8.4` and `8.10.7` are officially supported and tested on NixOS, Ubuntu, and macOS. GHC `8.6.5` may also work, but is not tested by our continuous integration pipeline.

## Contributing

This repository is built using a combination of [Nix] and your choice of [Bazel] or Cabal. If you're just using Amazonka as a git dependency in your Cabal or Stack project, you can skip these steps. But if you plan on contributing to the codebase - welcome, read on!

### Getting Started

#### 1. Clone this repository

```
git clone git@github.com:brendanhay/amazonka.git
cd amazonka
```

#### 2. Setup the Development Toolchain

Building the code in this repository requires various development dependencies (e.g. [Nix], [Bazel], [GHC].)

The [Nix] package manager is used to obtain and build the other dependencies in a [hermetic] environment. You can install Nix by following the [official installation instructions](https://nixos.org/guides/install-nix.html).

#### 3. Activate Build Caching

Finally, [Nix] and [Bazel] ideally need to use Amazonka's shared build caches to ensure we don't unnecessarily rebuild artefacts that have already been built on continuous integration.

You can install and configure [Cachix] via:

```bash
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use amazonka
```

The Bazel remote cache is automatically configured in the provided [.bazelrc](.bazelrc).

#### 4. Enter a Development Shell

The build toolchain is activated by entering a [Nix] shell, which will arrange your search `PATH` to point to the necessary tooling.

You can enter a `nix-shell` manually by running the following command in the root of the repository:

```bash
nix-shell
```

Optionally, If you have [Direnv] and [lorri] installed you can use the provided [.envrc](.envrc) instead, which will also add the [scripts](scripts) directory to your `PATH`. You can extend this by adding your own uncommitted `.envrc.local` file. See the [Direnv Wiki] for various recipes.

After installing [Direnv], you'll need to run the following command once:

```bash
direnv allow
```

#### 5. Building

> The following commands assume you're already in a nix-shell.

If you're already familiar with (and prefer) Cabal, you can build the `amazonka-*` packages via:

```bash
cabal build amazonka
```

Alternatively, if you are a contributor or plan on performing code generation you will need to familiarise yourself with [Bazel]. You can build the entire workspace using a wildcard label:

```bash
bazel build //...
```

To view what targets are available in the workspace:

```bash
bazel query //...
```

See [Bazel labels] for more information.

### Running the Code Generator

The [gen](gen) Bazel package contains code generators for synthesising Haskell data types, packages, and configuration from the botocore service definitions.

[scripts/generate](scripts/generate) will run the code generator for all services configured in [config/services](config/services), for example:

```bash
./scripts/generate
```

Or, you can selectively run the generator on one or more services:

```
./scripts/generate ec2 s3 iam
```

[scripts/generate-configs](scripts/generate-configs) will run the config generator to produce placeholder [config/serivces](config/services) configurations for the version of botocore pinned in the [WORKSPACE](WORKSPACE).

To generate any missing service configurations:

```bash
./scripts/generate-configs
```

Service configurations generated in this way are intended as examples only and the resulting `configs/services/<name>.json:libraryName` (Haskell package name) and `configs/annexes/<name>.json:serviceAbbreviation` (Haskell package namespace) should be manually verified and curated as necessary.

For pull requests which affect generated output please _do not include_ the regenerated `amazonka-*` packages, only commit updates to the build rules, documentation, generator, and related configuration. This ensures the Continuous Integration process is the single source of truth for the generated code and reduces noise in pull requests, keeping them reviewable and focused on actual generator code/logic changes.

### Code Formatting

Please use `./scripts/format` frequently - it's OK, I hate 2 spaces too, we're in this together.

### Directory Layout

This repository is organised into the following directory structure:

* `amazonka-*`: Data types for each of the individual Amazon Web Service libraries.
* `amazonka-*/test`: Tests and fixtures for each respective library.
* [`amazonka`](amazonka): Actual operational logic, you'll need to import this to send requests etc.
* [`examples`](examples): The `amazonka-examples` library containing basic examples.
* [`test`](test): The `amazonka-test` library containing common test functionality.
* [`docs`](docs): The documentation website and related build code.
* [`gen`](gen): The code and config generation binaries.
* [`config`](config): Service configuration, templates, and assets used by the code generator.
* [`scripts`](scripts): Convenient scripts to manage the release lifecycle of the service libraries.
* [`nix`](nix): Nix configuration code for the toolchain packages.
* [`tools`](tools): Custom bazel rules.
* [`third_party`](third_party): Third party bazel packages and patches.

### Third Party Package Naming

It is often desirable to provide supplemental functionality to `amazonka` as an additional library, for example providing S3 encryption via a package such as `amazonka-s3-encryption`.

Authors of these packages should carefully consider package naming and preferably do not prefix the package with `amazonka-*` to avoid potential future collisions with generated package names.
