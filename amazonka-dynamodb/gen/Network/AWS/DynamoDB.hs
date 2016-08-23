{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon DynamoDB
--
-- This is the Amazon DynamoDB API Reference. This guide provides descriptions of the low-level DynamoDB API.
--
-- This guide is intended for use with the following DynamoDB documentation:
--
-- -   <http://docs.aws.amazon.com/amazondynamodb/latest/gettingstartedguide/ Amazon DynamoDB Getting Started Guide> - provides hands-on exercises that help you learn the basics of working with DynamoDB. /If you are new to DynamoDB, we recommend that you begin with the Getting Started Guide./
--
-- -   <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ Amazon DynamoDB Developer Guide> - contains detailed information about DynamoDB concepts, usage, and best practices.
--
-- -   <http://docs.aws.amazon.com/dynamodbstreams/latest/APIReference/ Amazon DynamoDB Streams API Reference> - provides descriptions and samples of the DynamoDB Streams API. (For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Streams.html Capturing Table Activity with DynamoDB Streams> in the Amazon DynamoDB Developer Guide.)
--
-- Instead of making the requests to the low-level DynamoDB API directly from your application, we recommend that you use the AWS Software Development Kits (SDKs). The easy-to-use libraries in the AWS SDKs make it unnecessary to call the low-level DynamoDB API directly from your application. The libraries take care of request authentication, serialization, and connection management. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/UsingAWSSDK.html Using the AWS SDKs with DynamoDB> in the Amazon DynamoDB Developer Guide.
--
-- If you decide to code against the low-level DynamoDB API directly, you will need to write the necessary code to authenticate your requests. For more information on signing your requests, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API.html Using the DynamoDB API> in the /Amazon DynamoDB Developer Guide/.
--
-- The following are short descriptions of each low-level API action, organized by function.
--
-- __Managing Tables__
--
-- -   /CreateTable/ - Creates a table with user-specified provisioned throughput settings. You must define a primary key for the table - either a simple primary key (partition key), or a composite primary key (partition key and sort key). Optionally, you can create one or more secondary indexes, which provide fast data access using non-key attributes.
--
-- -   /DescribeTable/ - Returns metadata for a table, such as table size, status, and index information.
--
-- -   /UpdateTable/ - Modifies the provisioned throughput settings for a table. Optionally, you can modify the provisioned throughput settings for global secondary indexes on the table.
--
-- -   /ListTables/ - Returns a list of all tables associated with the current AWS account and endpoint.
--
-- -   /DeleteTable/ - Deletes a table and all of its indexes.
--
-- For conceptual information about managing tables, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html Working with Tables> in the /Amazon DynamoDB Developer Guide/.
--
-- __Reading Data__
--
-- -   /GetItem/ - Returns a set of attributes for the item that has a given primary key. By default, /GetItem/ performs an eventually consistent read; however, applications can request a strongly consistent read instead.
--
-- -   /BatchGetItem/ - Performs multiple /GetItem/ requests for data items using their primary keys, from one table or multiple tables. The response from /BatchGetItem/ has a size limit of 16 MB and returns a maximum of 100 items. Both eventually consistent and strongly consistent reads can be used.
--
-- -   /Query/ - Returns one or more items from a table or a secondary index. You must provide a specific value for the partition key. You can narrow the scope of the query using comparison operators against a sort key value, or on the index key. /Query/ supports either eventual or strong consistency. A single response has a size limit of 1 MB.
--
-- -   /Scan/ - Reads every item in a table; the result set is eventually consistent. You can limit the number of items returned by filtering the data attributes, using conditional expressions. /Scan/ can be used to enable ad-hoc querying of a table against non-key attributes; however, since this is a full table scan without using an index, /Scan/ should not be used for any application query use case that requires predictable performance.
--
-- For conceptual information about reading data, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithItems.html Working with Items> and <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Query and Scan Operations> in the /Amazon DynamoDB Developer Guide/.
--
-- __Modifying Data__
--
-- -   /PutItem/ - Creates a new item, or replaces an existing item with a new item (including all the attributes). By default, if an item in the table already exists with the same primary key, the new item completely replaces the existing item. You can use conditional operators to replace an item only if its attribute values match certain conditions, or to insert a new item only if that item doesn\'t already exist.
--
-- -   /UpdateItem/ - Modifies the attributes of an existing item. You can also use conditional operators to perform an update only if the item\'s attribute values match certain conditions.
--
-- -   /DeleteItem/ - Deletes an item in a table by primary key. You can use conditional operators to perform a delete an item only if the item\'s attribute values match certain conditions.
--
-- -   /BatchWriteItem/ - Performs multiple /PutItem/ and /DeleteItem/ requests across multiple tables in a single request. A failure of any request(s) in the batch will not cause the entire /BatchWriteItem/ operation to fail. Supports batches of up to 25 items to put or delete, with a maximum total request size of 16 MB.
--
-- For conceptual information about modifying data, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithItems.html Working with Items> and <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Query and Scan Operations> in the /Amazon DynamoDB Developer Guide/.
module Network.AWS.DynamoDB
    (
    -- * Service Configuration
      dynamoDB

    -- * Errors
    -- $errors

    -- ** ProvisionedThroughputExceededException
    , _ProvisionedThroughputExceededException

    -- ** ConditionalCheckFailedException
    , _ConditionalCheckFailedException

    -- ** ItemCollectionSizeLimitExceededException
    , _ItemCollectionSizeLimitExceededException

    -- ** InternalServerError
    , _InternalServerError

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- ** TableNotExists
    , tableNotExists

    -- ** TableExists
    , tableExists

    -- * Operations
    -- $operations

    -- ** PutItem
    , module Network.AWS.DynamoDB.PutItem

    -- ** DeleteItem
    , module Network.AWS.DynamoDB.DeleteItem

    -- ** UpdateItem
    , module Network.AWS.DynamoDB.UpdateItem

    -- ** DeleteTable
    , module Network.AWS.DynamoDB.DeleteTable

    -- ** UpdateTable
    , module Network.AWS.DynamoDB.UpdateTable

    -- ** BatchGetItem
    , module Network.AWS.DynamoDB.BatchGetItem

    -- ** DescribeTable
    , module Network.AWS.DynamoDB.DescribeTable

    -- ** DescribeLimits
    , module Network.AWS.DynamoDB.DescribeLimits

    -- ** GetItem
    , module Network.AWS.DynamoDB.GetItem

    -- ** BatchWriteItem
    , module Network.AWS.DynamoDB.BatchWriteItem

    -- ** ListTables (Paginated)
    , module Network.AWS.DynamoDB.ListTables

    -- ** Scan (Paginated)
    , module Network.AWS.DynamoDB.Scan

    -- ** Query (Paginated)
    , module Network.AWS.DynamoDB.Query

    -- ** CreateTable
    , module Network.AWS.DynamoDB.CreateTable

    -- * Types

    -- ** AttributeAction
    , AttributeAction (..)

    -- ** ComparisonOperator
    , ComparisonOperator (..)

    -- ** ConditionalOperator
    , ConditionalOperator (..)

    -- ** IndexStatus
    , IndexStatus (..)

    -- ** KeyType
    , KeyType (..)

    -- ** ProjectionType
    , ProjectionType (..)

    -- ** ReturnConsumedCapacity
    , ReturnConsumedCapacity (..)

    -- ** ReturnItemCollectionMetrics
    , ReturnItemCollectionMetrics (..)

    -- ** ReturnValue
    , ReturnValue (..)

    -- ** ScalarAttributeType
    , ScalarAttributeType (..)

    -- ** Select
    , Select (..)

    -- ** StreamViewType
    , StreamViewType (..)

    -- ** TableStatus
    , TableStatus (..)

    -- ** AttributeDefinition
    , AttributeDefinition
    , attributeDefinition
    , adAttributeName
    , adAttributeType

    -- ** AttributeValue
    , AttributeValue
    , attributeValue
    , avL
    , avNS
    , avM
    , avNULL
    , avN
    , avBS
    , avB
    , avSS
    , avS
    , avBOOL

    -- ** AttributeValueUpdate
    , AttributeValueUpdate
    , attributeValueUpdate
    , avuValue
    , avuAction

    -- ** Capacity
    , Capacity
    , capacity
    , cCapacityUnits

    -- ** Condition
    , Condition
    , condition
    , cAttributeValueList
    , cComparisonOperator

    -- ** ConsumedCapacity
    , ConsumedCapacity
    , consumedCapacity
    , ccGlobalSecondaryIndexes
    , ccCapacityUnits
    , ccLocalSecondaryIndexes
    , ccTable
    , ccTableName

    -- ** CreateGlobalSecondaryIndexAction
    , CreateGlobalSecondaryIndexAction
    , createGlobalSecondaryIndexAction
    , cgsiaIndexName
    , cgsiaKeySchema
    , cgsiaProjection
    , cgsiaProvisionedThroughput

    -- ** DeleteGlobalSecondaryIndexAction
    , DeleteGlobalSecondaryIndexAction
    , deleteGlobalSecondaryIndexAction
    , dgsiaIndexName

    -- ** DeleteRequest
    , DeleteRequest
    , deleteRequest
    , drKey

    -- ** ExpectedAttributeValue
    , ExpectedAttributeValue
    , expectedAttributeValue
    , eavAttributeValueList
    , eavExists
    , eavValue
    , eavComparisonOperator

    -- ** GlobalSecondaryIndex
    , GlobalSecondaryIndex
    , globalSecondaryIndex
    , gsiIndexName
    , gsiKeySchema
    , gsiProjection
    , gsiProvisionedThroughput

    -- ** GlobalSecondaryIndexDescription
    , GlobalSecondaryIndexDescription
    , globalSecondaryIndexDescription
    , gsidBackfilling
    , gsidIndexSizeBytes
    , gsidIndexStatus
    , gsidProvisionedThroughput
    , gsidIndexARN
    , gsidKeySchema
    , gsidProjection
    , gsidItemCount
    , gsidIndexName

    -- ** GlobalSecondaryIndexUpdate
    , GlobalSecondaryIndexUpdate
    , globalSecondaryIndexUpdate
    , gsiuCreate
    , gsiuDelete
    , gsiuUpdate

    -- ** ItemCollectionMetrics
    , ItemCollectionMetrics
    , itemCollectionMetrics
    , icmItemCollectionKey
    , icmSizeEstimateRangeGB

    -- ** KeySchemaElement
    , KeySchemaElement
    , keySchemaElement
    , kseAttributeName
    , kseKeyType

    -- ** KeysAndAttributes
    , KeysAndAttributes
    , keysAndAttributes
    , kaaProjectionExpression
    , kaaAttributesToGet
    , kaaExpressionAttributeNames
    , kaaConsistentRead
    , kaaKeys

    -- ** LocalSecondaryIndex
    , LocalSecondaryIndex
    , localSecondaryIndex
    , lsiIndexName
    , lsiKeySchema
    , lsiProjection

    -- ** LocalSecondaryIndexDescription
    , LocalSecondaryIndexDescription
    , localSecondaryIndexDescription
    , lsidIndexSizeBytes
    , lsidIndexARN
    , lsidKeySchema
    , lsidProjection
    , lsidItemCount
    , lsidIndexName

    -- ** Projection
    , Projection
    , projection
    , pProjectionType
    , pNonKeyAttributes

    -- ** ProvisionedThroughput
    , ProvisionedThroughput
    , provisionedThroughput
    , ptReadCapacityUnits
    , ptWriteCapacityUnits

    -- ** ProvisionedThroughputDescription
    , ProvisionedThroughputDescription
    , provisionedThroughputDescription
    , ptdReadCapacityUnits
    , ptdLastDecreaseDateTime
    , ptdWriteCapacityUnits
    , ptdNumberOfDecreasesToday
    , ptdLastIncreaseDateTime

    -- ** PutRequest
    , PutRequest
    , putRequest
    , prItem

    -- ** StreamSpecification
    , StreamSpecification
    , streamSpecification
    , ssStreamViewType
    , ssStreamEnabled

    -- ** TableDescription
    , TableDescription
    , tableDescription
    , tdTableSizeBytes
    , tdAttributeDefinitions
    , tdLatestStreamARN
    , tdProvisionedThroughput
    , tdTableStatus
    , tdTableARN
    , tdKeySchema
    , tdGlobalSecondaryIndexes
    , tdLatestStreamLabel
    , tdLocalSecondaryIndexes
    , tdCreationDateTime
    , tdItemCount
    , tdTableName
    , tdStreamSpecification

    -- ** UpdateGlobalSecondaryIndexAction
    , UpdateGlobalSecondaryIndexAction
    , updateGlobalSecondaryIndexAction
    , ugsiaIndexName
    , ugsiaProvisionedThroughput

    -- ** WriteRequest
    , WriteRequest
    , writeRequest
    , wrDeleteRequest
    , wrPutRequest
    ) where

import           Network.AWS.DynamoDB.BatchGetItem
import           Network.AWS.DynamoDB.BatchWriteItem
import           Network.AWS.DynamoDB.CreateTable
import           Network.AWS.DynamoDB.DeleteItem
import           Network.AWS.DynamoDB.DeleteTable
import           Network.AWS.DynamoDB.DescribeLimits
import           Network.AWS.DynamoDB.DescribeTable
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.ListTables
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Query
import           Network.AWS.DynamoDB.Scan
import           Network.AWS.DynamoDB.Types
import           Network.AWS.DynamoDB.Types.Product
import           Network.AWS.DynamoDB.UpdateItem
import           Network.AWS.DynamoDB.UpdateTable
import           Network.AWS.DynamoDB.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'DynamoDB'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
