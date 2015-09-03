# Amazon DynamoDB SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

Amazon DynamoDB

__Overview__

This is the Amazon DynamoDB API Reference. This guide provides
descriptions and samples of the low-level DynamoDB API. For information
about DynamoDB application development, see the
<http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ Amazon DynamoDB Developer Guide>.

Instead of making the requests to the low-level DynamoDB API directly
from your application, we recommend that you use the AWS Software
Development Kits (SDKs). The easy-to-use libraries in the AWS SDKs make
it unnecessary to call the low-level DynamoDB API directly from your
application. The libraries take care of request authentication,
serialization, and connection management. For more information, see
<http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/UsingAWSSDK.html Using the AWS SDKs with DynamoDB>
in the /Amazon DynamoDB Developer Guide/.

If you decide to code against the low-level DynamoDB API directly, you
will need to write the necessary code to authenticate your requests. For
more information on signing your requests, see
<http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API.html Using the DynamoDB API>
in the /Amazon DynamoDB Developer Guide/.

The following are short descriptions of each low-level API action,
organized by function.

__Managing Tables__

-   /CreateTable/ - Creates a table with user-specified provisioned
    throughput settings. You must designate one attribute as the hash
    primary key for the table; you can optionally designate a second
    attribute as the range primary key. DynamoDB creates indexes on
    these key attributes for fast data access. Optionally, you can
    create one or more secondary indexes, which provide fast data access
    using non-key attributes.

-   /DescribeTable/ - Returns metadata for a table, such as table size,
    status, and index information.

-   /UpdateTable/ - Modifies the provisioned throughput settings for a
    table. Optionally, you can modify the provisioned throughput
    settings for global secondary indexes on the table.

-   /ListTables/ - Returns a list of all tables associated with the
    current AWS account and endpoint.

-   /DeleteTable/ - Deletes a table and all of its indexes.

For conceptual information about managing tables, see
<http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html Working with Tables>
in the /Amazon DynamoDB Developer Guide/.

__Reading Data__

-   /GetItem/ - Returns a set of attributes for the item that has a
    given primary key. By default, /GetItem/ performs an eventually
    consistent read; however, applications can request a strongly
    consistent read instead.

-   /BatchGetItem/ - Performs multiple /GetItem/ requests for data items
    using their primary keys, from one table or multiple tables. The
    response from /BatchGetItem/ has a size limit of 16 MB and returns a
    maximum of 100 items. Both eventually consistent and strongly
    consistent reads can be used.

-   /Query/ - Returns one or more items from a table or a secondary
    index. You must provide a specific hash key value. You can narrow
    the scope of the query using comparison operators against a range
    key value, or on the index key. /Query/ supports either eventual or
    strong consistency. A single response has a size limit of 1 MB.

-   /Scan/ - Reads every item in a table; the result set is eventually
    consistent. You can limit the number of items returned by filtering
    the data attributes, using conditional expressions. /Scan/ can be
    used to enable ad-hoc querying of a table against non-key
    attributes; however, since this is a full table scan without using
    an index, /Scan/ should not be used for any application query use
    case that requires predictable performance.

For conceptual information about reading data, see
<http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithItems.html Working with Items>
and
<http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Query and Scan Operations>
in the /Amazon DynamoDB Developer Guide/.

__Modifying Data__

-   /PutItem/ - Creates a new item, or replaces an existing item with a
    new item (including all the attributes). By default, if an item in
    the table already exists with the same primary key, the new item
    completely replaces the existing item. You can use conditional
    operators to replace an item only if its attribute values match
    certain conditions, or to insert a new item only if that item
    doesn\'t already exist.

-   /UpdateItem/ - Modifies the attributes of an existing item. You can
    also use conditional operators to perform an update only if the
    item\'s attribute values match certain conditions.

-   /DeleteItem/ - Deletes an item in a table by primary key. You can
    use conditional operators to perform a delete an item only if the
    item\'s attribute values match certain conditions.

-   /BatchWriteItem/ - Performs multiple /PutItem/ and /DeleteItem/
    requests across multiple tables in a single request. A failure of
    any request(s) in the batch will not cause the entire
    /BatchWriteItem/ operation to fail. Supports batches of up to 25
    items to put or delete, with a maximum total request size of 16 MB.

For conceptual information about modifying data, see
<http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithItems.html Working with Items>
and
<http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Query and Scan Operations>
in the /Amazon DynamoDB Developer Guide/.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-dynamodb)
and the [AWS API Reference](http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/Welcome.html).

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

`amazonka-dynamodb` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
