# Amazon DynamoDB Streams SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.4.4`


## Description

Amazon DynamoDB

Amazon DynamoDB Streams provides API actions for accessing streams and processing stream records. To learn more about application development with Streams, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Streams.html Capturing Table Activity with DynamoDB Streams> in the Amazon DynamoDB Developer Guide.

The following are short descriptions of each low-level DynamoDB Streams action:

-   /DescribeStream/ - Returns detailed information about a particular stream.

-   /GetRecords/ - Retrieves the stream records from within a shard.

-   /GetShardIterator/ - Returns information on how to retrieve the streams record from a shard with a given shard ID.

-   /ListStreams/ - Returns a list of all the streams associated with the current AWS account and endpoint.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-dynamodb-streams)
and the [AWS API Reference](https://aws.amazon.com/documentation/).

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

`amazonka-dynamodb-streams` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
