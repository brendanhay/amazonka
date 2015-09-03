# Amazon Redshift SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

Amazon Redshift __Overview__

This is an interface reference for Amazon Redshift. It contains
documentation for one of the programming or command line interfaces you
can use to manage Amazon Redshift clusters. Note that Amazon Redshift is
asynchronous, which means that some interfaces may require techniques,
such as polling or asynchronous callback handlers, to determine when a
command has been applied. In this reference, the parameter descriptions
indicate whether a change is applied immediately, on the next instance
reboot, or during the next maintenance window. For a summary of the
Amazon Redshift cluster management interfaces, go to
<http://docs.aws.amazon.com/redshift/latest/mgmt/using-aws-sdk.html Using the Amazon Redshift Management Interfaces>.

Amazon Redshift manages all the work of setting up, operating, and
scaling a data warehouse: provisioning capacity, monitoring and backing
up the cluster, and applying patches and upgrades to the Amazon Redshift
engine. You can focus on using your data to acquire new insights for
your business and customers.

If you are a first-time user of Amazon Redshift, we recommend that you
begin by reading the The
<http://docs.aws.amazon.com/redshift/latest/gsg/getting-started.html Amazon Redshift Getting Started Guide>

If you are a database developer, the
<http://docs.aws.amazon.com/redshift/latest/dg/welcome.html Amazon Redshift Database Developer Guide>
explains how to design, build, query, and maintain the databases that
make up your data warehouse.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-redshift)
and the [AWS API Reference](http://docs.aws.amazon.com/redshift/latest/APIReference/Welcome.html).

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

`amazonka-redshift` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
