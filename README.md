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
[nixpkgs]: https://github.com/NixOS/nixpkgs/
[ghc]: https://www.haskell.org/ghc/
[direnv]: https://direnv.net
[direnv wiki]: https://github.com/direnv/direnv/wiki
[botocore]: https://github.com/boto/botocore
[cabal]: https://www.haskell.org/cabal/

An Amazon Web Services SDK for Haskell with support for most public services. Parts of the code contained in this repository are auto-generated and automatically kept up to date with Amazon's latest service APIs.

- You can find the latest Haddock documentation for each respective library on the [Amazonka] website.
- A release changelog can be found in [lib/amazonka/CHANGELOG.md](lib/amazonka/CHANGELOG.md).
- For problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).

## Table of Contents

* [License](#license)
* [Directory Layout](#directory-layout)
* [Supported Platforms and GHC Versions](#supported-platforms-and-ghc-versions)
* [Getting Started](#getting-started)
* [Building the Project](#building-the-project)
* [Running the Code Generator](#running-the-code-generator)
* [Amazonka as a Git Dependency](#amazonka-as-a-git-dependency)
* [Code Formatting](#code-formatting)
* [Third Party Packages](#third-party-packages)

## License

Amazonka is licensed under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

The AWS service descriptions are licensed under Apache 2.0. Source files derived from the service descriptions contain an additional licensing clause in their header.

## Directory Layout

This repository is organised under the following directory structure:

- [`lib/amazonka`](lib/amazonka): The main library containing setup, authentication, and send logic. This will be your primary dependency.
- `lib/service/amazonka-*`: A library per supported Amazon Web Service, you'll need to add a dependency on each selected service library.
- [`lib/amazonka-core`](lib/amazonka-core): The `amazonka-core` library upon which each of the services depends.
- [`lib/amazonka-test`](lib/amazonka-test): Common test functionality.
- [`examples`](examples): Basic examples for using the service libraries.
- [`configs`](configs): Service configuration, templates, and assets used by the code generator.
- [`docs`](docs): The documentation website.
- [`gen`](gen): The code and configuration generators.
- [`scripts`](scripts): Scripts to manage the project, such as the release lifecycle.

## Supported Platforms and GHC Versions

GHC versions `9.0.*`, `9.4.*` and `9.6.*` are officially supported and tested on NixOS, Ubuntu, and macOS. Earlier or later versions of GHC may also work, but only the aforementioned versions are tested by our [continuous integration pipeline](./.github/workflows/build.yml).

## Getting Started

This repository is built using a combination of [Nix] and [Cabal]. If you're just using Amazonka as a git dependency in your Cabal or Stack project, you can skip the following steps and read [Amazonka as a Git dependency](#amazonka-as-a-git-dependency). But if you plan on contributing to the codebase - welcome, read on!

### 1. Clone this repository

```
git clone git@github.com:brendanhay/amazonka.git
cd amazonka
```

### 2. Setup Nix

Building the code in this repository requires various development dependencies that are obtained and built via the [Nix] package manager in a reproducible and hermetic environment. You can install Nix by following the [official installation instructions](https://nixos.org/guides/install-nix.html):

```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
```

Once Nix is setup, you can enable the [cache](https://amazonka.cachix.org) to avoid building dependencies:

```bash
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use amazonka
```

A [flake.nix](./flake.nix) is provided which will require your [Nix] configuration to enable [flake support](https://wiki.nixos.org/wiki/Flakes) if you haven't done so already.

Edit either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` and add:

```
experimental-features = nix-command flakes
```

If the Nix installation is in multi-user mode, don’t forget to restart the nix-daemon.

### 3. Enter a Nix Shell

The build tools are installed and activated upon entering a [Nix] shell, which is achieved by running the following command in the root of the repository:

```bash
nix develop
```

You can also enter a shell and by specifying one of the GHC versions declared by `flake.nix`, which can be inspected by running:

```bash
nix flake show
...
└───<system>
    ├───default: development environment 'amazonka-ghc944'
    ├───ghc810:  development environment 'amazonka-ghc8107'
    ├───ghc90:   development environment 'amazonka-ghc902'
    ├───ghc92:   development environment 'amazonka-ghc927'
    ├───ghc94:   development environment 'amazonka-ghc944'
    └───ghc96:   development environment 'amazonka-ghc961'
```

And then running `nix develop` for the desired version from the attribute list above:

``` bash
nix develop '.#ghc90'
```

> Note: the naming pattern for shells follows the GHC major versions available in [nixpkgs]. This means the minor versions will increment automatically as the [flake.lock](./flake.lock) is updated.

If you have [Direnv] installed you can use the provided [.envrc](.envrc) to automatically enter the default `nix develop`, which will also add the [scripts](scripts) directory to your `PATH`. You can extend this by adding your own uncommitted `.envrc.local` file. See the [Direnv Wiki] for various recipes.

## Building the Project

> The following commands assume you're already in a nix shell outlined in the previous step.

Once you've entered a [Nix] shell you can build `amazonka-*` packages via:

```bash
cabal build amazonka amazonka-s3
```

Or the entire project (which will take a very long time!):

```bash
cabal build all
```

## Running the Code Generator

The [gen](gen) package contain a code generator for synthesising Haskell data types, packages, and configuration from the [botocore] service definitions.

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

[scripts/generate-configs](scripts/generate-configs) will run the config generator to produce placeholder [config/services](config/services) configurations for any [botocore] services.

To generate configurations for any new/missing services:

```bash
./scripts/generate-configs
```

Service configurations generated in this way are intended as examples only and the resulting `configs/services/<name>.json:libraryName` (Haskell package name) and `configs/annexes/<name>.json:serviceAbbreviation` (Haskell package namespace) should be manually verified and curated as necessary.

For pull requests which affect generated output please _do not include_ the regenerated `amazonka-*` packages, only commit updates to the build rules, documentation, generator, and related configuration. This is to make code review more manageable by focusing pertinent changes such as configuration and logic changes in pull requests and designates the maintainers and Continuous Integration as the source of truth for the generated code.

## Amazonka as a Git Dependency

If there are as-yet-unreleased features or fixes that have yet to make it to Hackage, you can use the `main` (or another) development branch by declaring Amazonka as a Git dependency by following the Cabal or Stack instructions below.

> Note: `amazonka-core` is a required dependency of the main `amazonka` package, in addition to `amazonka-sts` and `amazonka-sso` for `sts:AssumeRoleWithWebIdentity` and SSO via AWS IAM Identity Center, respectively. These required dependencies can then be supplemented by any additional service libraries you use from `lib/services/amazonka-<service>`.

### Cabal

To add Amazonka as a Git dependency to your Cabal project, you will need to add a [`source-repository-package`](https://cabal.readthedocs.io/en/3.4/cabal-project.html#specifying-packages-from-remote-version-control-locations) section for `amazonka` to your `cabal.project` file:

```
-- For amazonka
-- Multiple subdirs in a single `source-repository-package` stanza are supported by cabal-install >= 3.2.0.0.
source-repository-package
    type: git
    location: https://github.com/brendanhay/amazonka
    tag: <current revision of the `main` branch>
    subdir: lib/amazonka lib/amazonka-core lib/services/amazonka-sso lib/services/amazonka-sts lib/services/amazonka-<service>
```

### Stack

Stack users should add an [`extra-deps:`](https://docs.haskellstack.org/en/stable/pantry/) stanza to their `stack.yaml`:

```yaml
extra-deps:
- github: brendanhay/amazonka
  commit: <current revision of the `main` branch>
  subdirs:
  - lib/amazonka
  - lib/amazonka-core
  - lib/services/amazonka-sso
  - lib/services/amazonka-sts
  - lib/services/amazonka-<service>
```

### Haskell.nix + Stack

Stack users who also use `haskell.nix` will need to configure `haskell.nix` to fetch Amazonka commits from a specific git branch/rev by using the `branchMap` parameter:

```nix
pkgs.haskell-nix.project {
  branchMap = {
    "https://github.com/brendanhay/amazonka" = {
      "<git-commit-sha>" = "main";
    };
  };

  ...
}
```

## Code Formatting

Provided automatically by the pre-commit hooks configured in `flake.nix` --- please use `nix develop`.

## Third Party Packages

When naming an additional library which provides supplemental functionality to `amazonka`, if you want to use the `amazonka-*` namespace, then please consider prefixing your package names with `amazonka-contrib-*`. For example, [amazonka-contrib-rds-utils](https://hackage.haskell.org/package/amazonka-contrib-rds-utils).

This minimises potential future collisions with auto-generated package names and new AWS service and product releases.
