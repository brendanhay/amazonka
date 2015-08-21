# Amazon Simple Systems Management Service SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.0.2`


## Description

Amazon EC2 Simple Systems Manager (SSM) enables you to configure and
manage your EC2 instances. You can create a configuration document and
then associate it with one or more running instances.

You can use a configuration document to automate the following tasks for
your Windows instances:

-   Join an AWS Directory

-   Install, repair, or uninstall software using an MSI package

-   Run PowerShell scripts

-   Configure CloudWatch Logs to monitor applications and systems

Note that configuration documents are not supported on Linux instances.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-ssm)
and the [AWS API Reference](http://docs.aws.amazon.com/ssm/latest/APIReference/Welcome.html).

The types from this library are intended to be used with [amazonka](http://hackage.haskell.org/package/amazonka),
which provides mechanisms for specifying AuthN/AuthZ information and sending requests.

Use of lenses is required for constructing and manipulating types.
This is due to the amount of nesting of AWS types and transparency regarding
de/serialisation into more palatable Haskell values.
The provided lenses should be compatible with any of the major lens libraries
[lens](http://hackage.haskell.org/package/lens) or [lens-family-core](http://hackage.haskell.org/package/lens-family-core).

## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).

> _Note:_ this library is an auto-generated Haskell package. Please see `amazonka-gen` for more information.


## Licence

`amazonka-ssm` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
