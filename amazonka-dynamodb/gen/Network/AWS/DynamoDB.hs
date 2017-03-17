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
-- __Amazon DynamoDB__
--
-- Amazon DynamoDB is a fully managed NoSQL database service that provides fast and predictable performance with seamless scalability. DynamoDB lets you offload the administrative burdens of operating and scaling a distributed database, so that you don't have to worry about hardware provisioning, setup and configuration, replication, software patching, or cluster scaling.
--
-- With DynamoDB, you can create database tables that can store and retrieve any amount of data, and serve any level of request traffic. You can scale up or scale down your tables' throughput capacity without downtime or performance degradation, and use the AWS Management Console to monitor resource utilization and performance metrics.
--
-- DynamoDB automatically spreads the data and traffic for your tables over a sufficient number of servers to handle your throughput and storage requirements, while maintaining consistent and fast performance. All of your data is stored on solid state disks (SSDs) and automatically replicated across multiple Availability Zones in an AWS region, providing built-in high availability and data durability.
--
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

    -- ** ListTagsOfResource
    , module Network.AWS.DynamoDB.ListTagsOfResource

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

    -- ** DescribeTimeToLive
    , module Network.AWS.DynamoDB.DescribeTimeToLive

    -- ** TagResource
    , module Network.AWS.DynamoDB.TagResource

    -- ** UntagResource
    , module Network.AWS.DynamoDB.UntagResource

    -- ** UpdateTimeToLive
    , module Network.AWS.DynamoDB.UpdateTimeToLive

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

    -- ** TimeToLiveStatus
    , TimeToLiveStatus (..)

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

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** TimeToLiveDescription
    , TimeToLiveDescription
    , timeToLiveDescription
    , ttldTimeToLiveStatus
    , ttldAttributeName

    -- ** TimeToLiveSpecification
    , TimeToLiveSpecification
    , timeToLiveSpecification
    , ttlsEnabled
    , ttlsAttributeName

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
import           Network.AWS.DynamoDB.DescribeTimeToLive
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.ListTables
import           Network.AWS.DynamoDB.ListTagsOfResource
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Query
import           Network.AWS.DynamoDB.Scan
import           Network.AWS.DynamoDB.TagResource
import           Network.AWS.DynamoDB.Types
import           Network.AWS.DynamoDB.UntagResource
import           Network.AWS.DynamoDB.UpdateItem
import           Network.AWS.DynamoDB.UpdateTable
import           Network.AWS.DynamoDB.UpdateTimeToLive
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
