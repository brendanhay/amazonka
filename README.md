# Amazonka

[![Build Status](https://travis-ci.org/brendanhay/amazonka.svg?branch=develop)](https://travis-ci.org/brendanhay/amazonka)

* [Description](#description)
* [Organisation](#organisation)
* [Usage](#usage)
* [Contribute](#contribute)
* [Licence](#licence)

## Description

A comprehensive Amazon Web Services SDK for Haskell supporting all of the currently
available services.

An introductory blog post detailing some of the motivation and design decisions
can be found [here](http://brendanhay.github.io/amazonka-comprehensive-haskell-aws-client.html).


## Organisation

This repository is organised into the following directory structure:

* [`amazonka`](amazonka): Monad transformer and send/receive/paginate/presign logic.
* `amazonka-*`: Each of the individually supported Amazon Web Service libraries.
* [`core`](core): The `amazonka-core` library upon which each of the services depends.
* [`gen`](gen): Code, templates, and assets for the `amazonka-gen` executable.
* [`examples`](examples): An example project which can be loaded using `cabal repl`.
* [`script`](script): Scripts to manage the release and life-cycle of the service libraries.
* [`share`](share): Makefile plumbing common to all service libraries


## Usage

You will typically add an `amazonka` dependency in your project's cabal file,
and any additional services you wish to use.

For example the `build-depends` section of a cabal file which utilises EC2 and
S3 might look like:

```
build-depends:
      amazonka
    , amazonka-ec2
    , amazonka-s3
    , base
```

### Transformer

### Credentials

### Sending Requests

### Pagination

### Presigned URLs

### Asynchronous Actions

## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).


## Licence

Amazonka is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).
