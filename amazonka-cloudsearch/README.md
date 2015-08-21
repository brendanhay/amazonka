# Amazon CloudSearch SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.0.2`


## Description

Amazon CloudSearch Configuration Service

You use the Amazon CloudSearch configuration service to create,
configure, and manage search domains. Configuration service requests are
submitted using the AWS Query protocol. AWS Query requests are HTTP or
HTTPS requests submitted via HTTP GET or POST with a query parameter
named Action.

The endpoint for configuration service requests is region-specific:
cloudsearch./region/.amazonaws.com. For example,
cloudsearch.us-east-1.amazonaws.com. For a current list of supported
regions and endpoints, see
<http://docs.aws.amazon.com/general/latest/gr/rande.html#cloudsearch_region Regions and Endpoints>.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-cloudsearch)
and the [AWS API Reference](http://docs.aws.amazon.com/cloudsearch/latest/developerguide/what-is-cloudsearch.html).

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

`amazonka-cloudsearch` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
