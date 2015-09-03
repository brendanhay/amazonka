# Amazon Elastic Load Balancing SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

Elastic Load Balancing

Elastic Load Balancing distributes incoming traffic across your EC2
instances.

For information about the features of Elastic Load Balancing, see
<http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/elastic-load-balancing.html What Is Elastic Load Balancing?>
in the /Elastic Load Balancing Developer Guide/.

For information about the AWS regions supported by Elastic Load
Balancing, see
<http://docs.aws.amazon.com/general/latest/gr/rande.html#elb_region Regions and Endpoints - Elastic Load Balancing>
in the /Amazon Web Services General Reference/.

All Elastic Load Balancing operations are /idempotent/, which means that
they complete at most one time. If you repeat an operation, it succeeds
with a 200 OK response code.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-elb)
and the [AWS API Reference](http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/Welcome.html).

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

`amazonka-elb` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
