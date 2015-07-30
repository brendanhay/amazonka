{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types
    (
    -- * Service
      DynamoDB

    -- * Errors
    , _ProvisionedThroughputExceededException
    , _ConditionalCheckFailedException
    , _ItemCollectionSizeLimitExceededException
    , _InternalServerError
    , _ResourceNotFoundException
    , _ResourceInUseException
    , _LimitExceededException

    -- * AttributeAction
    , AttributeAction (..)

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * ConditionalOperator
    , ConditionalOperator (..)

    -- * IndexStatus
    , IndexStatus (..)

    -- * KeyType
    , KeyType (..)

    -- * ProjectionType
    , ProjectionType (..)

    -- * ReturnConsumedCapacity
    , ReturnConsumedCapacity (..)

    -- * ReturnItemCollectionMetrics
    , ReturnItemCollectionMetrics (..)

    -- * ReturnValue
    , ReturnValue (..)

    -- * ScalarAttributeType
    , ScalarAttributeType (..)

    -- * Select
    , Select (..)

    -- * StreamViewType
    , StreamViewType (..)

    -- * TableStatus
    , TableStatus (..)

    -- * AttributeDefinition
    , AttributeDefinition
    , attributeDefinition
    , adAttributeName
    , adAttributeType

    -- * AttributeValue
    , AttributeValue
    , attributeValue
    , avL
    , avM
    , avNS
    , avNULL
    , avN
    , avBS
    , avB
    , avSS
    , avS
    , avBOOL

    -- * AttributeValueUpdate
    , AttributeValueUpdate
    , attributeValueUpdate
    , avuValue
    , avuAction

    -- * Capacity
    , Capacity
    , capacity
    , cCapacityUnits

    -- * Condition
    , Condition
    , condition
    , cAttributeValueList
    , cComparisonOperator

    -- * ConsumedCapacity
    , ConsumedCapacity
    , consumedCapacity
    , ccCapacityUnits
    , ccGlobalSecondaryIndexes
    , ccLocalSecondaryIndexes
    , ccTable
    , ccTableName

    -- * CreateGlobalSecondaryIndexAction
    , CreateGlobalSecondaryIndexAction
    , createGlobalSecondaryIndexAction
    , cgsiaIndexName
    , cgsiaKeySchema
    , cgsiaProjection
    , cgsiaProvisionedThroughput

    -- * DeleteGlobalSecondaryIndexAction
    , DeleteGlobalSecondaryIndexAction
    , deleteGlobalSecondaryIndexAction
    , dgsiaIndexName

    -- * DeleteRequest
    , DeleteRequest
    , deleteRequest
    , drKey

    -- * ExpectedAttributeValue
    , ExpectedAttributeValue
    , expectedAttributeValue
    , eavAttributeValueList
    , eavExists
    , eavValue
    , eavComparisonOperator

    -- * GlobalSecondaryIndex
    , GlobalSecondaryIndex
    , globalSecondaryIndex
    , gsiIndexName
    , gsiKeySchema
    , gsiProjection
    , gsiProvisionedThroughput

    -- * GlobalSecondaryIndexDescription
    , GlobalSecondaryIndexDescription
    , globalSecondaryIndexDescription
    , gsidBackfilling
    , gsidProvisionedThroughput
    , gsidIndexStatus
    , gsidIndexSizeBytes
    , gsidIndexARN
    , gsidKeySchema
    , gsidProjection
    , gsidItemCount
    , gsidIndexName

    -- * GlobalSecondaryIndexUpdate
    , GlobalSecondaryIndexUpdate
    , globalSecondaryIndexUpdate
    , gsiuCreate
    , gsiuDelete
    , gsiuUpdate

    -- * ItemCollectionMetrics
    , ItemCollectionMetrics
    , itemCollectionMetrics
    , icmItemCollectionKey
    , icmSizeEstimateRangeGB

    -- * KeySchemaElement
    , KeySchemaElement
    , keySchemaElement
    , kseAttributeName
    , kseKeyType

    -- * KeysAndAttributes
    , KeysAndAttributes
    , keysAndAttributes
    , kaaProjectionExpression
    , kaaConsistentRead
    , kaaExpressionAttributeNames
    , kaaAttributesToGet
    , kaaKeys

    -- * LocalSecondaryIndex
    , LocalSecondaryIndex
    , localSecondaryIndex
    , lsiIndexName
    , lsiKeySchema
    , lsiProjection

    -- * LocalSecondaryIndexDescription
    , LocalSecondaryIndexDescription
    , localSecondaryIndexDescription
    , lsidIndexSizeBytes
    , lsidIndexARN
    , lsidKeySchema
    , lsidProjection
    , lsidItemCount
    , lsidIndexName

    -- * Projection
    , Projection
    , projection
    , pProjectionType
    , pNonKeyAttributes

    -- * ProvisionedThroughput
    , ProvisionedThroughput
    , provisionedThroughput
    , ptReadCapacityUnits
    , ptWriteCapacityUnits

    -- * ProvisionedThroughputDescription
    , ProvisionedThroughputDescription
    , provisionedThroughputDescription
    , ptdReadCapacityUnits
    , ptdLastDecreaseDateTime
    , ptdWriteCapacityUnits
    , ptdNumberOfDecreasesToday
    , ptdLastIncreaseDateTime

    -- * PutRequest
    , PutRequest
    , putRequest
    , prItem

    -- * StreamSpecification
    , StreamSpecification
    , streamSpecification
    , ssStreamEnabled
    , ssStreamViewType

    -- * TableDescription
    , TableDescription
    , tableDescription
    , tdProvisionedThroughput
    , tdAttributeDefinitions
    , tdLatestStreamARN
    , tdTableSizeBytes
    , tdTableStatus
    , tdTableARN
    , tdKeySchema
    , tdLatestStreamLabel
    , tdGlobalSecondaryIndexes
    , tdLocalSecondaryIndexes
    , tdCreationDateTime
    , tdItemCount
    , tdTableName
    , tdStreamSpecification

    -- * UpdateGlobalSecondaryIndexAction
    , UpdateGlobalSecondaryIndexAction
    , updateGlobalSecondaryIndexAction
    , ugsiaIndexName
    , ugsiaProvisionedThroughput

    -- * WriteRequest
    , WriteRequest
    , writeRequest
    , wrPutRequest
    , wrDeleteRequest
    ) where

import           Network.AWS.DynamoDB.Types.Product
import           Network.AWS.DynamoDB.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2012-08-10@ of the Amazon DynamoDB SDK.
data DynamoDB

instance AWSService DynamoDB where
    type Sg DynamoDB = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "DynamoDB"
            , _svcPrefix = "dynamodb"
            , _svcVersion = "2012-08-10"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has
               (hasCode "ProvisionedThroughputExceededException" .
                hasStatus 400)
               e =
              Just "throughput_exceeded"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Your request rate is too high. The AWS SDKs for DynamoDB automatically
-- retry requests that receive this exception. Your request is eventually
-- successful, unless your retry queue is too large to finish. Reduce the
-- frequency of requests and use exponential backoff. For more information,
-- go to
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html#APIRetries Error Retries and Exponential Backoff>
-- in the /Amazon DynamoDB Developer Guide/.
_ProvisionedThroughputExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_ProvisionedThroughputExceededException =
    _ServiceError . hasCode "ProvisionedThroughputExceededException"

-- | A condition specified in the operation could not be evaluated.
_ConditionalCheckFailedException :: AWSError a => Getting (First ServiceError) a ServiceError
_ConditionalCheckFailedException =
    _ServiceError . hasCode "ConditionalCheckFailedException"

-- | An item collection is too large. This exception is only returned for
-- tables that have one or more local secondary indexes.
_ItemCollectionSizeLimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_ItemCollectionSizeLimitExceededException =
    _ServiceError . hasCode "ItemCollectionSizeLimitExceededException"

-- | An error occurred on the server side.
_InternalServerError :: AWSError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _ServiceError . hasCode "InternalServerError"

-- | The operation tried to access a nonexistent table or index. The resource
-- might not be specified correctly, or its status might not be @ACTIVE@.
_ResourceNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasCode "ResourceNotFoundException"

-- | The operation conflicts with the resource\'s availability. For example,
-- you attempted to recreate an existing table, or tried to delete a table
-- currently in the @CREATING@ state.
_ResourceInUseException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException = _ServiceError . hasCode "ResourceInUseException"

-- | The number of concurrent table requests (cumulative number of tables in
-- the @CREATING@, @DELETING@ or @UPDATING@ state) exceeds the maximum
-- allowed of 10.
--
-- Also, for tables with secondary indexes, only one of those tables can be
-- in the @CREATING@ state at any point in time. Do not attempt to create
-- more than one such table simultaneously.
--
-- The total limit of tables in the @ACTIVE@ state is 250.
_LimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"
