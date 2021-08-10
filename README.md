# Amazonka

[![MPL2][license-badge]][license]
[![Build][build-badge]][build]
[![Hackage][hackage-badge]][hackage]
[![Nix][nix-badge]][nix]
[![Cachix][cachix-badge]][cachix]

[license]: https://opensource.org/licenses/MPL-2.0
[license-badge]: https://img.shields.io/badge/license-MPL%202.0-blue.svg
[build]: https://github.com/brendanhay/amazonka/actions
[build-badge]: https://github.com/brendanhay/amazonka/workflows/build/badge.svg
[hackage]: http://hackage.haskell.org/package/amazonka
[hackage-badge]: https://img.shields.io/hackage/v/amazonka.svg
[nix]: https://nixos.org
[nix-badge]: https://img.shields.io/badge/builtwith-nix-purple.svg
[cachix]: https://amazonka.cachix.org
[cachix-badge]: https://img.shields.io/badge/cachix-amazonka-purple.svg

* [Description](#description)
* [Documentation](#documentation)
* [Organisation](#organisation)
* [Change Log](#change-log)
* [Contribute](#contribute)
    - [Package Names](#package-names)
* [Licence](#licence)


## Description

A comprehensive Amazon Web Services SDK for Haskell supporting all of the
publicly available services.

Parts of the code contained in this repository are auto-generated and
automatically kept up to date with Amazon's latest service APIs.

An introductory blog post detailing some of the motivation and design decisions
can be found [here](http://brendanhay.nz/amazonka-comprehensive-haskell-aws-client).


## Documentation

You can find the latest stable release documentation for each respective library
on Hackage under the [AWS section](http://hackage.haskell.org/packages/#cat:AWS).


## Organisation

This repository is organised into the following directory structure:

* [`amazonka`](amazonka): Actual operational logic, you'll need to import this to send requests etc.
* `amazonka-*`: Data types for each of the individual Amazon Web Service libraries.
* `amazonka-*/test`: Tests and fixtures for each respective library.
* [`core`](core): The `amazonka-core` library upon which each of the services depends.
* [`examples`](examples): A currently sparse collection of examples for the various services.
* [`gen`](gen): The code generation binary, along with configuration, templates, and assets.
* [`scripts`](scripts): CI scripts to manage the release lifecycle of the service libraries.
* [`share`](share): Makefile plumbing common to all service libraries
* [`test`](test): The `amazonka-test` library containing common test functionality.


## Change Log

A change log for the entire project can be found under [`amazonka/CHANGELOG.md`](amazonka/CHANGELOG.md).


## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).

## Package Naming

It is often desirable to provide supplemental functionality to `amazonka` as an additional library, for example providing S3 encryption via a package such as `amazonka-s3-encryption`.

I ask that authors of these packages carefully consider package naming and preferably do not prefix the package with `amazonka-*` to avoid potential collisions with generated package names.


## Licence

Amazonka is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
