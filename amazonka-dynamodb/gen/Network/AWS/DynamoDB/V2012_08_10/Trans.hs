{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.DynamoDB.V2012_08_10.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon DynamoDB is a fully managed NoSQL database service that provides
-- fast and predictable performance with seamless scalability. You can use
-- Amazon DynamoDB to create a database table that can store and retrieve any
-- amount of data, and serve any level of request traffic. Amazon DynamoDB
-- automatically spreads the data and traffic for the table over a sufficient
-- number of servers to handle the request capacity specified by the customer
-- and the amount of data stored, while maintaining consistent and fast
-- performance.
--
-- The 'State' operator variants from 'Control.Lens' such as '.=' can be
-- used to modify any additional request parameters before sending.
module Network.AWS.DynamoDB.V2012_08_10.Trans
    (
    -- * BatchGetItem
      batchGetItem
    -- * BatchWriteItem
    , batchWriteItem
    -- * CreateTable
    , createTable
    -- * DeleteItem
    , deleteItem
    -- * DeleteTable
    , deleteTable
    -- * DescribeTable
    , describeTable
    -- * GetItem
    , getItem
    -- * ListTables
    , listTables
    -- * PutItem
    , putItem
    -- * Query
    , query
    -- * Scan
    , scan
    -- * UpdateItem
    , updateItem
    -- * UpdateTable
    , updateTable

    -- * Re-exported
    , module AWS
    , module Network.AWS.DynamoDB.V2012_08_10
    -- ** Lenses
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.DynamoDB.V2012_08_10

-- | The BatchGetItem operation returns the attributes of one or more items from
-- one or more tables. You identify requested items by primary key. A single
-- operation can retrieve up to 1 MB of data, which can contain as many as 100
-- items. BatchGetItem will return a partial result if the response size limit
-- is exceeded, the table's provisioned throughput is exceeded, or an internal
-- processing failure occurs. If a partial result is returned, the operation
-- returns a value for UnprocessedKeys. You can use this value to retry the
-- operation starting with the next item to get. For example, if you ask to
-- retrieve 100 items, but each individual item is 50 KB in size, the system
-- returns 20 items (1 MB) and an appropriate UnprocessedKeys value so you can
-- get the next page of results. If desired, your application can include its
-- own logic to assemble the pages of results into one dataset. If no items
-- can be processed because of insufficient provisioned throughput on each of
-- the tables involved in the request, BatchGetItem throws
-- ProvisionedThroughputExceededException. By default, BatchGetItem performs
-- eventually consistent reads on every table in the request. If you want
-- strongly consistent reads instead, you can set ConsistentRead to true for
-- any or all tables. In order to minimize response latency, BatchGetItem
-- retrieves items in parallel. When designing your application, keep in mind
-- that DynamoDB does not return attributes in any particular order. To help
-- parse the response by item, include the primary key values for the items in
-- your request in the AttributesToGet parameter. If a requested item does not
-- exist, it is not returned in the result. Requests for nonexistent items
-- consume the minimum read capacity units according to the type of read. For
-- more information, see Capacity Units Calculations in the Amazon DynamoDB
-- Developer Guide. Retrieve Items From Multiple Tables The following sample
-- requests attributes from two different tables. { "Responses": { "Forum": [
-- { "Name":{ "S":"Amazon DynamoDB" }, "Threads":{ "N":"5" }, "Messages":{
-- "N":"19" }, "Views":{ "N":"35" } }, { "Name":{ "S":"Amazon RDS" },
-- "Threads":{ "N":"8" }, "Messages":{ "N":"32" }, "Views":{ "N":"38" } }, {
-- "Name":{ "S":"Amazon Redshift" }, "Threads":{ "N":"12" }, "Messages":{
-- "N":"55" }, "Views":{ "N":"47" } } ] "Thread": [ { "Tags":{
-- "SS":["Reads","MultipleUsers"] }, "Message":{ "S":"How many users can read
-- a single data item at a time? Are there any limits?" } } ] },
-- "UnprocessedKeys": { }, "ConsumedCapacity": [ { "TableName": "Forum",
-- "CapacityUnits": 3 }, { "TableName": "Thread", "CapacityUnits": 1 } ] }.
--
-- See: 'Network.AWS.DynamoDB.V2012_08_10.BatchGetItem'
batchGetItem :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
             => Map Text KeysAndAttributes -- ^ 'bgiRequestItems'
             -> State BatchGetItem a
             -> m BatchGetItemResponse
batchGetItem p1 s =
    send $ (mkBatchGetItem p1) &~ s

-- | The BatchWriteItem operation puts or deletes multiple items in one or more
-- tables. A single call to BatchWriteItem can write up to 1 MB of data, which
-- can comprise as many as 25 put or delete requests. Individual items to be
-- written can be as large as 64 KB. BatchWriteItem cannot update items. To
-- update items, use the UpdateItem API. The individual PutItem and DeleteItem
-- operations specified in BatchWriteItem are atomic; however BatchWriteItem
-- as a whole is not. If any requested operations fail because the table's
-- provisioned throughput is exceeded or an internal processing failure
-- occurs, the failed operations are returned in the UnprocessedItems response
-- parameter. You can investigate and optionally resend the requests.
-- Typically, you would call BatchWriteItem in a loop. Each iteration would
-- check for unprocessed items and submit a new BatchWriteItem request with
-- those unprocessed items until all items have been processed. To write one
-- item, you can use the PutItem operation; to delete one item, you can use
-- the DeleteItem operation. With BatchWriteItem, you can efficiently write or
-- delete large amounts of data, such as from Amazon Elastic MapReduce (EMR),
-- or copy data from another database into DynamoDB. In order to improve
-- performance with these large-scale operations, BatchWriteItem does not
-- behave in the same way as individual PutItem and DeleteItem calls would For
-- example, you cannot specify conditions on individual put and delete
-- requests, and BatchWriteItem does not return deleted items in the response.
-- If you use a programming language that supports concurrency, such as Java,
-- you can use threads to write items in parallel. Your application must
-- include the necessary logic to manage the threads. With languages that
-- don't support threading, such as PHP, BatchWriteItem will write or delete
-- the specified items one at a time. In both situations, BatchWriteItem
-- provides an alternative where the API performs the specified put and delete
-- operations in parallel, giving you the power of the thread pool approach
-- without having to introduce complexity into your application. Parallel
-- processing reduces latency, but each specified put and delete request
-- consumes the same number of write capacity units whether it is processed in
-- parallel or not. Delete operations on nonexistent items consume one write
-- capacity unit. If one or more of the following is true, DynamoDB rejects
-- the entire batch write operation: One or more tables specified in the
-- BatchWriteItem request does not exist. Primary key attributes specified on
-- an item in the request do not match those in the corresponding table's
-- primary key schema. You try to perform multiple operations on the same item
-- in the same BatchWriteItem request. For example, you cannot put and delete
-- the same item in the same BatchWriteItem request. The total request size
-- exceeds 1 MB. Any individual item in a batch exceeds 64 KB. Multiple
-- Operations on One Table This example writes several items to the Forum
-- table. The response shows that the final put operation failed, possibly
-- because the application exceeded the provisioned throughput on the table.
-- The UnprocessedItems object shows the unsuccessful put request. The
-- application can call BatchWriteItem again to address such unprocessed
-- requests. { "UnprocessedItems": { "Forum": [ { "PutRequest": { "Item": {
-- "Name": { "S": "Amazon ElastiCache" }, "Category": { "S": "Amazon Web
-- Services" } } } } ] }, "ConsumedCapacity": [ { "TableName": "Forum",
-- "CapacityUnits": 3 } ] }.
--
-- See: 'Network.AWS.DynamoDB.V2012_08_10.BatchWriteItem'
batchWriteItem :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
               => Map Text (List1 WriteRequest) -- ^ 'bwiRequestItems'
               -> State BatchWriteItem a
               -> m BatchWriteItemResponse
batchWriteItem p1 s =
    send $ (mkBatchWriteItem p1) &~ s

-- | The CreateTable operation adds a new table to your account. In an AWS
-- account, table names must be unique within each region. That is, you can
-- have two tables with same name if you create the tables in different
-- regions. CreateTable is an asynchronous operation. Upon receiving a
-- CreateTable request, DynamoDB immediately returns a response with a
-- TableStatus of CREATING. After the table is created, DynamoDB sets the
-- TableStatus to ACTIVE. You can perform read and write operations only on an
-- ACTIVE table. If you want to create multiple tables with secondary indexes
-- on them, you must create them sequentially. Only one table with secondary
-- indexes can be in the CREATING state at any given time. You can use the
-- DescribeTable API to check the table status. Create a Table This example
-- creates a table named Thread. The table primary key consists of ForumName
-- (hash) and Subject (range). A local secondary index is also created; its
-- key consists of ForumName (hash) and LastPostDateTime (range). {
-- "TableDescription": { "AttributeDefinitions": [ { "AttributeName":
-- "ForumName", "AttributeType": "S" }, { "AttributeName": "LastPostDateTime",
-- "AttributeType": "S" }, { "AttributeName": "Subject", "AttributeType": "S"
-- } ], "CreationDateTime": 1.36372808007E9, "ItemCount": 0, "KeySchema": [ {
-- "AttributeName": "ForumName", "KeyType": "HASH" }, { "AttributeName":
-- "Subject", "KeyType": "RANGE" } ], "LocalSecondaryIndexes": [ {
-- "IndexName": "LastPostIndex", "IndexSizeBytes": 0, "ItemCount": 0,
-- "KeySchema": [ { "AttributeName": "ForumName", "KeyType": "HASH" }, {
-- "AttributeName": "LastPostDateTime", "KeyType": "RANGE" } ], "Projection":
-- { "ProjectionType": "KEYS_ONLY" } } ], "ProvisionedThroughput": {
-- "NumberOfDecreasesToday": 0, "ReadCapacityUnits": 5, "WriteCapacityUnits":
-- 5 }, "TableName": "Thread", "TableSizeBytes": 0, "TableStatus": "CREATING"
-- } }.
--
-- See: 'Network.AWS.DynamoDB.V2012_08_10.CreateTable'
createTable :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
            => [AttributeDefinition] -- ^ 'ctAttributeDefinitions'
            -> Text -- ^ 'ctTableName'
            -> List1 KeySchemaElement -- ^ 'ctKeySchema'
            -> ProvisionedThroughput -- ^ 'ctProvisionedThroughput'
            -> State CreateTable a
            -> m CreateTableResponse
createTable p1 p2 p3 p6 s =
    send $ (mkCreateTable p1 p2 p3 p6) &~ s

-- | Deletes a single item in a table by primary key. You can perform a
-- conditional delete operation that deletes the item if it exists, or if it
-- has an expected attribute value. In addition to deleting an item, you can
-- also return the item's attribute values in the same operation, using the
-- ReturnValues parameter. Unless you specify conditions, the DeleteItem is an
-- idempotent operation; running it multiple times on the same item or
-- attribute does not result in an error response. Conditional deletes are
-- useful for only deleting items if specific conditions are met. If those
-- conditions are met, DynamoDB performs the delete. Otherwise, the item is
-- not deleted. Delete an Item This example deletes an item from the Thread
-- table, but only if that item does not have an attribute named Replies.
-- Because ReturnValues is set to ALL_OLD, the response contains the item as
-- it appeared before the delete. { "Attributes": { "LastPostedBy": { "S":
-- "fred@example.com" }, "ForumName": { "S": "Amazon DynamoDB" },
-- "LastPostDateTime": { "S": "201303201023" }, "Tags": { "SS":
-- ["Update","Multiple Items","HelpMe"] }, "Subject": { "S": "How do I update
-- multiple items?" }, "Message": { "S": "I want to update multiple items in a
-- single API call. What's the best way to do that?" } } }.
--
-- See: 'Network.AWS.DynamoDB.V2012_08_10.DeleteItem'
deleteItem :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
           => Text -- ^ 'diTableName'
           -> Map Text AttributeValue -- ^ 'diKey'
           -> State DeleteItem a
           -> m DeleteItemResponse
deleteItem p1 p2 s =
    send $ (mkDeleteItem p1 p2) &~ s

-- | The DeleteTable operation deletes a table and all of its items. After a
-- DeleteTable request, the specified table is in the DELETING state until
-- DynamoDB completes the deletion. If the table is in the ACTIVE state, you
-- can delete it. If a table is in CREATING or UPDATING states, then DynamoDB
-- returns a ResourceInUseException. If the specified table does not exist,
-- DynamoDB returns a ResourceNotFoundException. If table is already in the
-- DELETING state, no error is returned. DynamoDB might continue to accept
-- data read and write operations, such as GetItem and PutItem, on a table in
-- the DELETING state until the table deletion is complete. When you delete a
-- table, any indexes on that table are also deleted. Use the DescribeTable
-- API to check the status of the table. Delete a Table This example deletes
-- the Reply table. { "TableDescription": { "ItemCount": 0,
-- "ProvisionedThroughput": { "NumberOfDecreasesToday": 0,
-- "ReadCapacityUnits": 5, "WriteCapacityUnits": 5 }, "TableName": "Reply",
-- "TableSizeBytes": 0, "TableStatus": "DELETING" } }.
--
-- See: 'Network.AWS.DynamoDB.V2012_08_10.DeleteTable'
deleteTable :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
            => Text -- ^ 'dtTableName'
            -> State DeleteTable a
            -> m DeleteTableResponse
deleteTable p1 s =
    send $ (mkDeleteTable p1) &~ s

-- | Returns information about the table, including the current status of the
-- table, when it was created, the primary key schema, and any indexes on the
-- table. Describe a Table This example describes the Thread table. { "Table":
-- { "AttributeDefinitions": [ { "AttributeName": "ForumName",
-- "AttributeType": "S" }, { "AttributeName": "LastPostDateTime",
-- "AttributeType": "S" }, { "AttributeName": "Subject", "AttributeType": "S"
-- } ], "CreationDateTime": 1.363729002358E9, "ItemCount": 0, "KeySchema": [ {
-- "AttributeName": "ForumName", "KeyType": "HASH" }, { "AttributeName":
-- "Subject", "KeyType": "RANGE" } ], "LocalSecondaryIndexes": [ {
-- "IndexName": "LastPostIndex", "IndexSizeBytes": 0, "ItemCount": 0,
-- "KeySchema": [ { "AttributeName": "ForumName", "KeyType": "HASH" }, {
-- "AttributeName": "LastPostDateTime", "KeyType": "RANGE" } ], "Projection":
-- { "ProjectionType": "KEYS_ONLY" } } ], "ProvisionedThroughput": {
-- "NumberOfDecreasesToday": 0, "ReadCapacityUnits": 5, "WriteCapacityUnits":
-- 5 }, "TableName": "Thread", "TableSizeBytes": 0, "TableStatus": "ACTIVE" }
-- }.
--
-- See: 'Network.AWS.DynamoDB.V2012_08_10.DescribeTable'
describeTable :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
              => Text -- ^ 'dt1TableName'
              -> State DescribeTable a
              -> m DescribeTableResponse
describeTable p1 s =
    send $ (mkDescribeTable p1) &~ s

-- | The GetItem operation returns a set of attributes for the item with the
-- given primary key. If there is no matching item, GetItem does not return
-- any data. GetItem provides an eventually consistent read by default. If
-- your application requires a strongly consistent read, set ConsistentRead to
-- true. Although a strongly consistent read might take more time than an
-- eventually consistent read, it always returns the last updated value.
-- Retrieve Item Attributes This example retrieves three attributes from the
-- Thread table. In the response, the ConsumedCapacityUnits value is 1,
-- because ConsistentRead is set to true. If ConsistentRead had been set to
-- false (or not specified) for the same request, an eventually consistent
-- read would have been used and ConsumedCapacityUnits would have been 0.5. {
-- "ConsumedCapacity": { "CapacityUnits": 1, "TableName": "Thread" }, "Item":
-- { "Tags": { "SS": ["Update","Multiple Items","HelpMe"] },
-- "LastPostDateTime": { "S": "201303190436" }, "Message": { "S": "I want to
-- update multiple items in a single API call. What's the best way to do
-- that?" } } }.
--
-- See: 'Network.AWS.DynamoDB.V2012_08_10.GetItem'
getItem :: ( MonadCatch m
           , MonadResource m
           , MonadError AWS.Error m
           , MonadReader Env m
           )
        => Text -- ^ 'giTableName'
        -> Map Text AttributeValue -- ^ 'giKey'
        -> State GetItem a
        -> m GetItemResponse
getItem p1 p2 s =
    send $ (mkGetItem p1 p2) &~ s

-- | Returns an array of all the tables associated with the current account and
-- endpoint. List Tables This example requests a list of tables, starting with
-- a table named comp2 and ending after three table names have been returned.
-- { "LastEvaluatedTableName": "Thread", "TableNames":
-- ["Forum","Reply","Thread"] }.
--
-- See: 'Network.AWS.DynamoDB.V2012_08_10.ListTables'
listTables :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env (ResumableSource m)
              )
           => State ListTables a
           -> ResumableSource m ListTablesResponse
listTables s =
    paginate (mkListTables &~ s)

-- | Creates a new item, or replaces an old item with a new item. If an item
-- already exists in the specified table with the same primary key, the new
-- item completely replaces the existing item. You can perform a conditional
-- put (insert a new item if one with the specified primary key doesn't
-- exist), or replace an existing item if it has certain attribute values. In
-- addition to putting an item, you can also return the item's attribute
-- values in the same operation, using the ReturnValues parameter. When you
-- add an item, the primary key attribute(s) are the only required attributes.
-- Attribute values cannot be null. String and binary type attributes must
-- have lengths greater than zero. Set type attributes cannot be empty.
-- Requests with empty values will be rejected with a ValidationException. You
-- can request that PutItem return either a copy of the old item (before the
-- update) or a copy of the new item (after the update). For more information,
-- see the ReturnValues description. To prevent a new item from replacing an
-- existing item, use a conditional put operation with Exists set to false for
-- the primary key attribute, or attributes. For more information about using
-- this API, see Working with Items in the Amazon DynamoDB Developer Guide.
-- Put an Item This example puts a new item into the Thread table. To prevent
-- this new item from overwriting an existing item, "Exists" is set to false
-- for the primary key attributes. { }.
--
-- See: 'Network.AWS.DynamoDB.V2012_08_10.PutItem'
putItem :: ( MonadCatch m
           , MonadResource m
           , MonadError AWS.Error m
           , MonadReader Env m
           )
        => Text -- ^ 'piTableName'
        -> Map Text AttributeValue -- ^ 'piItem'
        -> State PutItem a
        -> m PutItemResponse
putItem p1 p2 s =
    send $ (mkPutItem p1 p2) &~ s

-- | A Query operation directly accesses items from a table using the table
-- primary key, or from an index using the index key. You must provide a
-- specific hash key value. You can narrow the scope of the query by using
-- comparison operators on the range key value, or on the index key. You can
-- use the ScanIndexForward parameter to get results in forward or reverse
-- order, by range key or by index key. Queries that do not return results
-- consume the minimum read capacity units according to the type of read. If
-- the total number of items meeting the query criteria exceeds the result set
-- size limit of 1 MB, the query stops and results are returned to the user
-- with a LastEvaluatedKey to continue the query in a subsequent operation.
-- Unlike a Scan operation, a Query operation never returns an empty result
-- set and a LastEvaluatedKey. The LastEvaluatedKey is only provided if the
-- results exceed 1 MB, or if you have used Limit. You can query a table, a
-- local secondary index (LSI), or a global secondary index (GSI). For a query
-- on a table or on an LSI, you can set ConsistentRead to true and obtain a
-- strongly consistent result. GSIs support eventually consistent reads only,
-- so do not specify ConsistentRead when querying a GSI. Retrieving a Range of
-- Items This example queries the Thread table for postings between two dates.
-- There is an index on LastPostDateTime to facilitate fast lookups on this
-- attribute. All of the attributes will be returned. Any attributes that are
-- not projected into the index will cause DynamoDB to fetch those attributes
-- from the Thread table; this fetching occurs automatically. { "Count":`17 }.
--
-- See: 'Network.AWS.DynamoDB.V2012_08_10.Query'
query :: ( MonadCatch m
         , MonadResource m
         , MonadError AWS.Error m
         , MonadReader Env (ResumableSource m)
         )
      => Text -- ^ 'qTableName'
      -> State Query a
      -> ResumableSource m QueryResponse
query p1 s =
    paginate $ (mkQuery p1) &~ s

-- | The Scan operation returns one or more items and item attributes by
-- accessing every item in the table. To have DynamoDB return fewer items, you
-- can provide a ScanFilter. If the total number of scanned items exceeds the
-- maximum data set size limit of 1 MB, the scan stops and results are
-- returned to the user with a LastEvaluatedKey to continue the scan in a
-- subsequent operation. The results also include the number of items
-- exceeding the limit. A scan can result in no table data meeting the filter
-- criteria. The result set is eventually consistent. By default, Scan
-- operations proceed sequentially; however, for faster performance on large
-- tables, applications can request a parallel Scan by specifying the Segment
-- and TotalSegments parameters. For more information, see Parallel Scan in
-- the Amazon DynamoDB Developer Guide. Returning All Items This example
-- returns all of the items in a table. No scan filter is applied. {
-- "ConsumedCapacity": { "CapacityUnits": 0.5, "TableName": "Reply" },
-- "Count": 2, "Items": [ { "PostedBy": { "S": "joe@example.com" },
-- "ReplyDateTime": { "S": "20130320115336" }, "Id": { "S": "Amazon
-- DynamoDB#How do I update multiple items?" }, "Message": { "S": "Have you
-- looked at the BatchWriteItem API?" } }, { "PostedBy": { "S":
-- "joe@example.com" }, "ReplyDateTime": { "S": "20130320115347" }, "Id": {
-- "S": "Amazon DynamoDB#How do I update multiple items?" }, "Message": { "S":
-- "BatchWriteItem is documented in the Amazon DynamoDB API Reference." } } ],
-- "ScannedCount": 4 }.
--
-- See: 'Network.AWS.DynamoDB.V2012_08_10.Scan'
scan :: ( MonadCatch m
        , MonadResource m
        , MonadError AWS.Error m
        , MonadReader Env (ResumableSource m)
        )
     => Text -- ^ 'sTableName'
     -> State Scan a
     -> ResumableSource m ScanResponse
scan p1 s =
    paginate $ (mkScan p1) &~ s

-- | Edits an existing item's attributes, or inserts a new item if it does not
-- already exist. You can put, delete, or add attribute values. You can also
-- perform a conditional update (insert a new attribute name-value pair if it
-- doesn't exist, or replace an existing name-value pair if it has certain
-- expected attribute values). In addition to updating an item, you can also
-- return the item's attribute values in the same operation, using the
-- ReturnValues parameter. Conditional Update This example updates the Thread
-- table, changing the LastPostedBy attribute-but only if LastPostedBy is
-- currently "fred@example.com". All of the item's attributes, as they appear
-- after the update, are returned in the response. { }.
--
-- See: 'Network.AWS.DynamoDB.V2012_08_10.UpdateItem'
updateItem :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
           => Text -- ^ 'uiTableName'
           -> Map Text AttributeValue -- ^ 'uiKey'
           -> State UpdateItem a
           -> m UpdateItemResponse
updateItem p1 p2 s =
    send $ (mkUpdateItem p1 p2) &~ s

-- | Updates the provisioned throughput for the given table. Setting the
-- throughput for a table helps you manage performance and is part of the
-- provisioned throughput feature of DynamoDB. The provisioned throughput
-- values can be upgraded or downgraded based on the maximums and minimums
-- listed in the Limits section in the Amazon DynamoDB Developer Guide. The
-- table must be in the ACTIVE state for this operation to succeed.
-- UpdateTable is an asynchronous operation; while executing the operation,
-- the table is in the UPDATING state. While the table is in the UPDATING
-- state, the table still has the provisioned throughput from before the call.
-- The new provisioned throughput setting is in effect only when the table
-- returns to the ACTIVE state after the UpdateTable operation. You cannot
-- add, modify or delete indexes using UpdateTable. Indexes can only be
-- defined at table creation time. Modify Provisioned Write Throughput This
-- example changes both the provisioned read and write throughput of the
-- Thread table to 10 capacity units. { "TableDescription": {
-- "AttributeDefinitions": [ { "AttributeName": "ForumName", "AttributeType":
-- "S" }, { "AttributeName": "LastPostDateTime", "AttributeType": "S" }, {
-- "AttributeName": "Subject", "AttributeType": "S" } ], "CreationDateTime":
-- 1.363801528686E9, "ItemCount": 0, "KeySchema": [ { "AttributeName":
-- "ForumName", "KeyType": "HASH" }, { "AttributeName": "Subject", "KeyType":
-- "RANGE" } ], "LocalSecondaryIndexes": [ { "IndexName": "LastPostIndex",
-- "IndexSizeBytes": 0, "ItemCount": 0, "KeySchema": [ { "AttributeName":
-- "ForumName", "KeyType": "HASH" }, { "AttributeName": "LastPostDateTime",
-- "KeyType": "RANGE" } ], "Projection": { "ProjectionType": "KEYS_ONLY" } }
-- ], "ProvisionedThroughput": { "LastIncreaseDateTime": 1.363801701282E9,
-- "NumberOfDecreasesToday": 0, "ReadCapacityUnits": 5, "WriteCapacityUnits":
-- 5 }, "TableName": "Thread", "TableSizeBytes": 0, "TableStatus": "UPDATING"
-- } }.
--
-- See: 'Network.AWS.DynamoDB.V2012_08_10.UpdateTable'
updateTable :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
            => Text -- ^ 'utTableName'
            -> State UpdateTable a
            -> m UpdateTableResponse
updateTable p1 s =
    send $ (mkUpdateTable p1) &~ s
