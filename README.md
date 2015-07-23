# Amazonka

[![Build Status](https://travis-ci.org/brendanhay/amazonka.svg?branch=develop)](https://travis-ci.org/brendanhay/amazonka)
[![Hackage Version](https://img.shields.io/hackage/v/amazonka.svg)](http://hackage.haskell.org/package/amazonka)

* [Description](#description)
* [Organisation](#organisation)
* [Documentation](#documentation)
* [Contribute](#contribute)
* [Licence](#licence)


## Description

A comprehensive Amazon Web Services SDK for Haskell supporting all of the
publicly available services.

Many Parts of the code contained in this repository are auto-generated and
automatically kept up to date with Amazon's latest service APIs.

An introductory blog post detailing some of the motivation and design decisions
can be found [here](http://brendanhay.nz/amazonka-comprehensive-haskell-aws-client.html).


## Documentation

You can find the latest stable release documentation for each respective library
on Hackage under the [AWS section](http://hackage.haskell.org/packages/#cat:AWS).

Haddock documentation which is built by CI from the `develop` branch
can be found [here](http://brendanhay.nz/amazonka-doc).


## Organisation

This repository is organised into the following directory structure:

* [`amazonka`](amazonka): Actual operational logic, you'll need to import this to send requests etc.
* `amazonka-*`: Data types for each of the individual Amazon Web Service libraries.
* `amazonka-*/test`: Tests and fixtures for each respective library.
* [`core`](core): The `amazonka-core` library upon which each of the services depends.
* [`gen`](gen): Code, templates, and assets for the `amazonka-gen` executable.
* [`script`](script): CI scripts to manage the release lifecycle of the service libraries.
* [`share`](share): Makefile plumbing common to all service libraries
* [`test`](test): The `amazonka-test` library containing common test functionality.


## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).


## Licence

Amazonka is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
