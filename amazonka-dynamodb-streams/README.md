# Amazon DynamoDB Streams SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

Amazon DynamoDB Streams

This is the Amazon DynamoDB Streams API Reference. This guide describes
the low-level API actions for accessing streams and processing stream
records. For information about application development with DynamoDB
Streams, see the
<http://docs.aws.amazon.com/amazondynamodb/latest/developerguide//Streams.html Amazon DynamoDB Developer Guide>.

Note that this document is intended for use with the following DynamoDB
documentation:

-   <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ Amazon DynamoDB Developer Guide>

-   <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/ Amazon DynamoDB API Reference>

The following are short descriptions of each low-level DynamoDB Streams
API action, organized by function.

-   /DescribeStream/ - Returns detailed information about a particular
    stream.

-   /GetRecords/ - Retrieves the stream records from within a shard.

-   /GetShardIterator/ - Returns information on how to retrieve the
    streams record from a shard with a given shard ID.

-   /ListStreams/ - Returns a list of all the streams associated with
    the current AWS account and endpoint.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-dynamodb-streams)
and the [AWS API Reference](http://dynamodb-preview.s3-website-us-west-2.amazonaws.com/docs/streams-api/Welcome.html).

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
