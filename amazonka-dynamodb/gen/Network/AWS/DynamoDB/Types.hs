{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.Types
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
module Network.AWS.DynamoDB.Types
    (
    -- * Service
      DynamoDB
    -- ** Errors
    , DynamoDBError (..)
    , _ConditionalCheckFailedException
    , _DynamoDBClient
    , _DynamoDBSerializer
    , _DynamoDBService
    , _InternalServerError
    , _ItemCollectionSizeLimitExceededException
    , _LimitExceededException
    , _ProvisionedThroughputExceededException
    , _ResourceInUseException
    , _ResourceNotFoundException

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

    -- * TableStatus
    , TableStatus (..)

    -- * Capacity
    , Capacity
    , capacity
    , cCapacityUnits

    -- * DeleteRequest
    , DeleteRequest
    , deleteRequest
    , drKey

    -- * GlobalSecondaryIndexUpdate
    , GlobalSecondaryIndexUpdate
    , globalSecondaryIndexUpdate
    , gsiuUpdate

    -- * PutRequest
    , PutRequest
    , putRequest
    , prItem

    -- * AttributeDefinition
    , AttributeDefinition
    , attributeDefinition
    , adAttributeName
    , adAttributeType

    -- * AttributeValue
    , AttributeValue
    , attributeValue
    , avS
    , avN
    , avB
    , avSS
    , avNS
    , avBS

    -- * AttributeValueUpdate
    , AttributeValueUpdate
    , attributeValueUpdate
    , avuValue
    , avuAction

    -- * Condition
    , Condition
    , condition
    , c1AttributeValueList
    , c1ComparisonOperator

    -- * ConsumedCapacity
    , ConsumedCapacity
    , consumedCapacity
    , ccTableName
    , ccCapacityUnits
    , ccTable
    , ccLocalSecondaryIndexes
    , ccGlobalSecondaryIndexes

    -- * ExpectedAttributeValue
    , ExpectedAttributeValue
    , expectedAttributeValue
    , eavValue
    , eavExists
    , eavComparisonOperator
    , eavAttributeValueList

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
    , gsidIndexName
    , gsidKeySchema
    , gsidProjection
    , gsidIndexStatus
    , gsidProvisionedThroughput
    , gsidIndexSizeBytes
    , gsidItemCount

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
    , kaaKeys
    , kaaAttributesToGet
    , kaaConsistentRead

    -- * LocalSecondaryIndex
    , LocalSecondaryIndex
    , localSecondaryIndex
    , lsiIndexName
    , lsiKeySchema
    , lsiProjection

    -- * LocalSecondaryIndexDescription
    , LocalSecondaryIndexDescription
    , localSecondaryIndexDescription
    , lsidIndexName
    , lsidKeySchema
    , lsidProjection
    , lsidIndexSizeBytes
    , lsidItemCount

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
    , ptdLastIncreaseDateTime
    , ptdLastDecreaseDateTime
    , ptdNumberOfDecreasesToday
    , ptdReadCapacityUnits
    , ptdWriteCapacityUnits

    -- * TableDescription
    , TableDescription
    , tableDescription
    , tdAttributeDefinitions
    , tdTableName
    , tdKeySchema
    , tdTableStatus
    , tdCreationDateTime
    , tdProvisionedThroughput
    , tdTableSizeBytes
    , tdItemCount
    , tdLocalSecondaryIndexes
    , tdGlobalSecondaryIndexes

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

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2012-08-10@) of the
-- @Amazon DynamoDB@ service.
data DynamoDB deriving (Typeable)

instance AWSService DynamoDB where
    type Sg DynamoDB = V4
    type Er DynamoDB = DynamoDBError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "dynamodb"
        , _svcVersion  = "2012-08-10"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'DynamoDB' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data DynamoDBError
      -- | A condition specified in the operation could not be evaluated.
    = ConditionalCheckFailedException
        { _ccfeMessage :: Maybe Text
        }
    | DynamoDBClient HttpException
    | DynamoDBSerializer String
    | DynamoDBService String
      -- | An error occurred on the server side.
    | InternalServerError
        { _iseMessage :: Maybe Text
        }
      -- | An item collection is too large. This exception is only returned
      -- for tables that have one or more local secondary indexes.
    | ItemCollectionSizeLimitExceededException
        { _icsleeMessage :: Maybe Text
        }
      -- | The number of concurrent table requests (cumulative number of
      -- tables in the CREATING, DELETING or UPDATING state) exceeds the
      -- maximum allowed of 10. Also, for tables with secondary indexes,
      -- only one of those tables can be in the CREATING state at any
      -- point in time. Do not attempt to create more than one such table
      -- simultaneously. The total limit of tables in the ACTIVE state is
      -- 250.
    | LimitExceededException
        { _leeMessage :: Maybe Text
        }
      -- | The request rate is too high, or the request is too large, for
      -- the available throughput to accommodate. The AWS SDKs
      -- automatically retry requests that receive this exception;
      -- therefore, your request will eventually succeed, unless the
      -- request is too large or your retry queue is too large to finish.
      -- Reduce the frequency of requests by using the strategies listed
      -- in Error Retries and Exponential Backoff in the Amazon DynamoDB
      -- Developer Guide.
    | ProvisionedThroughputExceededException
        { _pteeMessage :: Maybe Text
        }
      -- | The operation conflicts with the resource's availability. For
      -- example, you attempted to recreate an existing table, or tried to
      -- delete a table currently in the CREATING state.
    | ResourceInUseException
        { _riueMessage :: Maybe Text
        }
      -- | The operation tried to access a nonexistent table or index. The
      -- resource may not be specified correctly, or its status may not be
      -- ACTIVE.
    | ResourceNotFoundException
        { _rnfeMessage :: Maybe Text
        }
      deriving (Show, Typeable, Generic)

instance AWSError DynamoDBError where
    awsError = const "DynamoDBError"

instance AWSServiceError DynamoDBError where
    serviceError    = DynamoDBService
    clientError     = DynamoDBClient
    serializerError = DynamoDBSerializer

instance Exception DynamoDBError

-- | A condition specified in the operation could not be evaluated.
--
-- See: 'ConditionalCheckFailedException'
_ConditionalCheckFailedException :: Prism' DynamoDBError (Maybe Text)
_ConditionalCheckFailedException = prism
    ConditionalCheckFailedException
    (\case
        ConditionalCheckFailedException p1 -> Right p1
        x -> Left x)

-- | See: 'DynamoDBClient'
_DynamoDBClient :: Prism' DynamoDBError HttpException
_DynamoDBClient = prism
    DynamoDBClient
    (\case
        DynamoDBClient p1 -> Right p1
        x -> Left x)

-- | See: 'DynamoDBSerializer'
_DynamoDBSerializer :: Prism' DynamoDBError String
_DynamoDBSerializer = prism
    DynamoDBSerializer
    (\case
        DynamoDBSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'DynamoDBService'
_DynamoDBService :: Prism' DynamoDBError String
_DynamoDBService = prism
    DynamoDBService
    (\case
        DynamoDBService p1 -> Right p1
        x -> Left x)

-- | An error occurred on the server side.
--
-- See: 'InternalServerError'
_InternalServerError :: Prism' DynamoDBError (Maybe Text)
_InternalServerError = prism
    InternalServerError
    (\case
        InternalServerError p1 -> Right p1
        x -> Left x)

-- | An item collection is too large. This exception is only returned for tables
-- that have one or more local secondary indexes.
--
-- See: 'ItemCollectionSizeLimitExceededException'
_ItemCollectionSizeLimitExceededException :: Prism' DynamoDBError (Maybe Text)
_ItemCollectionSizeLimitExceededException = prism
    ItemCollectionSizeLimitExceededException
    (\case
        ItemCollectionSizeLimitExceededException p1 -> Right p1
        x -> Left x)

-- | The number of concurrent table requests (cumulative number of tables in the
-- CREATING, DELETING or UPDATING state) exceeds the maximum allowed of 10.
-- Also, for tables with secondary indexes, only one of those tables can be in
-- the CREATING state at any point in time. Do not attempt to create more than
-- one such table simultaneously. The total limit of tables in the ACTIVE
-- state is 250.
--
-- See: 'LimitExceededException'
_LimitExceededException :: Prism' DynamoDBError (Maybe Text)
_LimitExceededException = prism
    LimitExceededException
    (\case
        LimitExceededException p1 -> Right p1
        x -> Left x)

-- | The request rate is too high, or the request is too large, for the
-- available throughput to accommodate. The AWS SDKs automatically retry
-- requests that receive this exception; therefore, your request will
-- eventually succeed, unless the request is too large or your retry queue is
-- too large to finish. Reduce the frequency of requests by using the
-- strategies listed in Error Retries and Exponential Backoff in the Amazon
-- DynamoDB Developer Guide.
--
-- See: 'ProvisionedThroughputExceededException'
_ProvisionedThroughputExceededException :: Prism' DynamoDBError (Maybe Text)
_ProvisionedThroughputExceededException = prism
    ProvisionedThroughputExceededException
    (\case
        ProvisionedThroughputExceededException p1 -> Right p1
        x -> Left x)

-- | The operation conflicts with the resource's availability. For example, you
-- attempted to recreate an existing table, or tried to delete a table
-- currently in the CREATING state.
--
-- See: 'ResourceInUseException'
_ResourceInUseException :: Prism' DynamoDBError (Maybe Text)
_ResourceInUseException = prism
    ResourceInUseException
    (\case
        ResourceInUseException p1 -> Right p1
        x -> Left x)

-- | The operation tried to access a nonexistent table or index. The resource
-- may not be specified correctly, or its status may not be ACTIVE.
--
-- See: 'ResourceNotFoundException'
_ResourceNotFoundException :: Prism' DynamoDBError (Maybe Text)
_ResourceNotFoundException = prism
    ResourceNotFoundException
    (\case
        ResourceNotFoundException p1 -> Right p1
        x -> Left x)

data AttributeAction
    = AttributeActionAdd -- ^ ADD
    | AttributeActionDelete -- ^ DELETE
    | AttributeActionPut -- ^ PUT
      deriving (Eq, Ord, Show, Generic)

instance Hashable AttributeAction

instance FromText AttributeAction where
    parser = match "ADD" AttributeActionAdd
         <|> match "DELETE" AttributeActionDelete
         <|> match "PUT" AttributeActionPut

instance ToText AttributeAction where
    toText AttributeActionAdd = "ADD"
    toText AttributeActionDelete = "DELETE"
    toText AttributeActionPut = "PUT"

instance ToByteString AttributeAction where
    toBS AttributeActionAdd = "ADD"
    toBS AttributeActionDelete = "DELETE"
    toBS AttributeActionPut = "PUT"

instance ToHeader AttributeAction where
    toHeader k = toHeader k . toBS

instance ToQuery AttributeAction where
    toQuery = toQuery . toBS

instance FromJSON AttributeAction

instance ToJSON AttributeAction

data ComparisonOperator
    = ComparisonOperatorBeginsWith -- ^ BEGINS_WITH
    | ComparisonOperatorBetween -- ^ BETWEEN
    | ComparisonOperatorContains -- ^ CONTAINS
    | ComparisonOperatorEq -- ^ EQ
    | ComparisonOperatorGe -- ^ GE
    | ComparisonOperatorGt -- ^ GT
    | ComparisonOperatorIn -- ^ IN
    | ComparisonOperatorLe -- ^ LE
    | ComparisonOperatorLt -- ^ LT
    | ComparisonOperatorNe -- ^ NE
    | ComparisonOperatorNotContains -- ^ NOT_CONTAINS
    | ComparisonOperatorNotNull -- ^ NOT_NULL
    | ComparisonOperatorNull -- ^ NULL
      deriving (Eq, Ord, Show, Generic)

instance Hashable ComparisonOperator

instance FromText ComparisonOperator where
    parser = match "BEGINS_WITH" ComparisonOperatorBeginsWith
         <|> match "BETWEEN" ComparisonOperatorBetween
         <|> match "CONTAINS" ComparisonOperatorContains
         <|> match "EQ" ComparisonOperatorEq
         <|> match "GE" ComparisonOperatorGe
         <|> match "GT" ComparisonOperatorGt
         <|> match "IN" ComparisonOperatorIn
         <|> match "LE" ComparisonOperatorLe
         <|> match "LT" ComparisonOperatorLt
         <|> match "NE" ComparisonOperatorNe
         <|> match "NOT_CONTAINS" ComparisonOperatorNotContains
         <|> match "NOT_NULL" ComparisonOperatorNotNull
         <|> match "NULL" ComparisonOperatorNull

instance ToText ComparisonOperator where
    toText ComparisonOperatorBeginsWith = "BEGINS_WITH"
    toText ComparisonOperatorBetween = "BETWEEN"
    toText ComparisonOperatorContains = "CONTAINS"
    toText ComparisonOperatorEq = "EQ"
    toText ComparisonOperatorGe = "GE"
    toText ComparisonOperatorGt = "GT"
    toText ComparisonOperatorIn = "IN"
    toText ComparisonOperatorLe = "LE"
    toText ComparisonOperatorLt = "LT"
    toText ComparisonOperatorNe = "NE"
    toText ComparisonOperatorNotContains = "NOT_CONTAINS"
    toText ComparisonOperatorNotNull = "NOT_NULL"
    toText ComparisonOperatorNull = "NULL"

instance ToByteString ComparisonOperator where
    toBS ComparisonOperatorBeginsWith = "BEGINS_WITH"
    toBS ComparisonOperatorBetween = "BETWEEN"
    toBS ComparisonOperatorContains = "CONTAINS"
    toBS ComparisonOperatorEq = "EQ"
    toBS ComparisonOperatorGe = "GE"
    toBS ComparisonOperatorGt = "GT"
    toBS ComparisonOperatorIn = "IN"
    toBS ComparisonOperatorLe = "LE"
    toBS ComparisonOperatorLt = "LT"
    toBS ComparisonOperatorNe = "NE"
    toBS ComparisonOperatorNotContains = "NOT_CONTAINS"
    toBS ComparisonOperatorNotNull = "NOT_NULL"
    toBS ComparisonOperatorNull = "NULL"

instance ToHeader ComparisonOperator where
    toHeader k = toHeader k . toBS

instance ToQuery ComparisonOperator where
    toQuery = toQuery . toBS

instance FromJSON ComparisonOperator

instance ToJSON ComparisonOperator

data ConditionalOperator
    = ConditionalOperatorAnd -- ^ AND
    | ConditionalOperatorOr -- ^ OR
      deriving (Eq, Ord, Show, Generic)

instance Hashable ConditionalOperator

instance FromText ConditionalOperator where
    parser = match "AND" ConditionalOperatorAnd
         <|> match "OR" ConditionalOperatorOr

instance ToText ConditionalOperator where
    toText ConditionalOperatorAnd = "AND"
    toText ConditionalOperatorOr = "OR"

instance ToByteString ConditionalOperator where
    toBS ConditionalOperatorAnd = "AND"
    toBS ConditionalOperatorOr = "OR"

instance ToHeader ConditionalOperator where
    toHeader k = toHeader k . toBS

instance ToQuery ConditionalOperator where
    toQuery = toQuery . toBS

instance ToJSON ConditionalOperator

data IndexStatus
    = IndexStatusActive -- ^ ACTIVE
    | IndexStatusCreating -- ^ CREATING
    | IndexStatusDeleting -- ^ DELETING
    | IndexStatusUpdating -- ^ UPDATING
      deriving (Eq, Ord, Show, Generic)

instance Hashable IndexStatus

instance FromText IndexStatus where
    parser = match "ACTIVE" IndexStatusActive
         <|> match "CREATING" IndexStatusCreating
         <|> match "DELETING" IndexStatusDeleting
         <|> match "UPDATING" IndexStatusUpdating

instance ToText IndexStatus where
    toText IndexStatusActive = "ACTIVE"
    toText IndexStatusCreating = "CREATING"
    toText IndexStatusDeleting = "DELETING"
    toText IndexStatusUpdating = "UPDATING"

instance ToByteString IndexStatus where
    toBS IndexStatusActive = "ACTIVE"
    toBS IndexStatusCreating = "CREATING"
    toBS IndexStatusDeleting = "DELETING"
    toBS IndexStatusUpdating = "UPDATING"

instance ToHeader IndexStatus where
    toHeader k = toHeader k . toBS

instance ToQuery IndexStatus where
    toQuery = toQuery . toBS

instance FromJSON IndexStatus

instance ToJSON IndexStatus

data KeyType
    = KeyTypeHash -- ^ HASH
    | KeyTypeRange -- ^ RANGE
      deriving (Eq, Ord, Show, Generic)

instance Hashable KeyType

instance FromText KeyType where
    parser = match "HASH" KeyTypeHash
         <|> match "RANGE" KeyTypeRange

instance ToText KeyType where
    toText KeyTypeHash = "HASH"
    toText KeyTypeRange = "RANGE"

instance ToByteString KeyType where
    toBS KeyTypeHash = "HASH"
    toBS KeyTypeRange = "RANGE"

instance ToHeader KeyType where
    toHeader k = toHeader k . toBS

instance ToQuery KeyType where
    toQuery = toQuery . toBS

instance FromJSON KeyType

instance ToJSON KeyType

data ProjectionType
    = ProjectionTypeAll -- ^ ALL
    | ProjectionTypeInclude -- ^ INCLUDE
    | ProjectionTypeKeysOnly -- ^ KEYS_ONLY
      deriving (Eq, Ord, Show, Generic)

instance Hashable ProjectionType

instance FromText ProjectionType where
    parser = match "ALL" ProjectionTypeAll
         <|> match "INCLUDE" ProjectionTypeInclude
         <|> match "KEYS_ONLY" ProjectionTypeKeysOnly

instance ToText ProjectionType where
    toText ProjectionTypeAll = "ALL"
    toText ProjectionTypeInclude = "INCLUDE"
    toText ProjectionTypeKeysOnly = "KEYS_ONLY"

instance ToByteString ProjectionType where
    toBS ProjectionTypeAll = "ALL"
    toBS ProjectionTypeInclude = "INCLUDE"
    toBS ProjectionTypeKeysOnly = "KEYS_ONLY"

instance ToHeader ProjectionType where
    toHeader k = toHeader k . toBS

instance ToQuery ProjectionType where
    toQuery = toQuery . toBS

instance FromJSON ProjectionType

instance ToJSON ProjectionType

data ReturnConsumedCapacity
    = ReturnConsumedCapacityIndexes -- ^ INDEXES
    | ReturnConsumedCapacityNone -- ^ NONE
    | ReturnConsumedCapacityTotal -- ^ TOTAL
      deriving (Eq, Ord, Show, Generic)

instance Hashable ReturnConsumedCapacity

instance FromText ReturnConsumedCapacity where
    parser = match "INDEXES" ReturnConsumedCapacityIndexes
         <|> match "NONE" ReturnConsumedCapacityNone
         <|> match "TOTAL" ReturnConsumedCapacityTotal

instance ToText ReturnConsumedCapacity where
    toText ReturnConsumedCapacityIndexes = "INDEXES"
    toText ReturnConsumedCapacityNone = "NONE"
    toText ReturnConsumedCapacityTotal = "TOTAL"

instance ToByteString ReturnConsumedCapacity where
    toBS ReturnConsumedCapacityIndexes = "INDEXES"
    toBS ReturnConsumedCapacityNone = "NONE"
    toBS ReturnConsumedCapacityTotal = "TOTAL"

instance ToHeader ReturnConsumedCapacity where
    toHeader k = toHeader k . toBS

instance ToQuery ReturnConsumedCapacity where
    toQuery = toQuery . toBS

instance ToJSON ReturnConsumedCapacity

data ReturnItemCollectionMetrics
    = ReturnItemCollectionMetricsNone -- ^ NONE
    | ReturnItemCollectionMetricsSize -- ^ SIZE
      deriving (Eq, Ord, Show, Generic)

instance Hashable ReturnItemCollectionMetrics

instance FromText ReturnItemCollectionMetrics where
    parser = match "NONE" ReturnItemCollectionMetricsNone
         <|> match "SIZE" ReturnItemCollectionMetricsSize

instance ToText ReturnItemCollectionMetrics where
    toText ReturnItemCollectionMetricsNone = "NONE"
    toText ReturnItemCollectionMetricsSize = "SIZE"

instance ToByteString ReturnItemCollectionMetrics where
    toBS ReturnItemCollectionMetricsNone = "NONE"
    toBS ReturnItemCollectionMetricsSize = "SIZE"

instance ToHeader ReturnItemCollectionMetrics where
    toHeader k = toHeader k . toBS

instance ToQuery ReturnItemCollectionMetrics where
    toQuery = toQuery . toBS

instance ToJSON ReturnItemCollectionMetrics

data ReturnValue
    = ReturnValueAllNew -- ^ ALL_NEW
    | ReturnValueAllOld -- ^ ALL_OLD
    | ReturnValueNone -- ^ NONE
    | ReturnValueUpdatedNew -- ^ UPDATED_NEW
    | ReturnValueUpdatedOld -- ^ UPDATED_OLD
      deriving (Eq, Ord, Show, Generic)

instance Hashable ReturnValue

instance FromText ReturnValue where
    parser = match "ALL_NEW" ReturnValueAllNew
         <|> match "ALL_OLD" ReturnValueAllOld
         <|> match "NONE" ReturnValueNone
         <|> match "UPDATED_NEW" ReturnValueUpdatedNew
         <|> match "UPDATED_OLD" ReturnValueUpdatedOld

instance ToText ReturnValue where
    toText ReturnValueAllNew = "ALL_NEW"
    toText ReturnValueAllOld = "ALL_OLD"
    toText ReturnValueNone = "NONE"
    toText ReturnValueUpdatedNew = "UPDATED_NEW"
    toText ReturnValueUpdatedOld = "UPDATED_OLD"

instance ToByteString ReturnValue where
    toBS ReturnValueAllNew = "ALL_NEW"
    toBS ReturnValueAllOld = "ALL_OLD"
    toBS ReturnValueNone = "NONE"
    toBS ReturnValueUpdatedNew = "UPDATED_NEW"
    toBS ReturnValueUpdatedOld = "UPDATED_OLD"

instance ToHeader ReturnValue where
    toHeader k = toHeader k . toBS

instance ToQuery ReturnValue where
    toQuery = toQuery . toBS

instance ToJSON ReturnValue

data ScalarAttributeType
    = ScalarAttributeTypeB -- ^ B
    | ScalarAttributeTypeN -- ^ N
    | ScalarAttributeTypeS -- ^ S
      deriving (Eq, Ord, Show, Generic)

instance Hashable ScalarAttributeType

instance FromText ScalarAttributeType where
    parser = match "B" ScalarAttributeTypeB
         <|> match "N" ScalarAttributeTypeN
         <|> match "S" ScalarAttributeTypeS

instance ToText ScalarAttributeType where
    toText ScalarAttributeTypeB = "B"
    toText ScalarAttributeTypeN = "N"
    toText ScalarAttributeTypeS = "S"

instance ToByteString ScalarAttributeType where
    toBS ScalarAttributeTypeB = "B"
    toBS ScalarAttributeTypeN = "N"
    toBS ScalarAttributeTypeS = "S"

instance ToHeader ScalarAttributeType where
    toHeader k = toHeader k . toBS

instance ToQuery ScalarAttributeType where
    toQuery = toQuery . toBS

instance FromJSON ScalarAttributeType

instance ToJSON ScalarAttributeType

data Select
    = SelectAllAttributes -- ^ ALL_ATTRIBUTES
    | SelectAllProjectedAttributes -- ^ ALL_PROJECTED_ATTRIBUTES
    | SelectCount -- ^ COUNT
    | SelectSpecificAttributes -- ^ SPECIFIC_ATTRIBUTES
      deriving (Eq, Ord, Show, Generic)

instance Hashable Select

instance FromText Select where
    parser = match "ALL_ATTRIBUTES" SelectAllAttributes
         <|> match "ALL_PROJECTED_ATTRIBUTES" SelectAllProjectedAttributes
         <|> match "COUNT" SelectCount
         <|> match "SPECIFIC_ATTRIBUTES" SelectSpecificAttributes

instance ToText Select where
    toText SelectAllAttributes = "ALL_ATTRIBUTES"
    toText SelectAllProjectedAttributes = "ALL_PROJECTED_ATTRIBUTES"
    toText SelectCount = "COUNT"
    toText SelectSpecificAttributes = "SPECIFIC_ATTRIBUTES"

instance ToByteString Select where
    toBS SelectAllAttributes = "ALL_ATTRIBUTES"
    toBS SelectAllProjectedAttributes = "ALL_PROJECTED_ATTRIBUTES"
    toBS SelectCount = "COUNT"
    toBS SelectSpecificAttributes = "SPECIFIC_ATTRIBUTES"

instance ToHeader Select where
    toHeader k = toHeader k . toBS

instance ToQuery Select where
    toQuery = toQuery . toBS

instance ToJSON Select

data TableStatus
    = TableStatusActive -- ^ ACTIVE
    | TableStatusCreating -- ^ CREATING
    | TableStatusDeleting -- ^ DELETING
    | TableStatusUpdating -- ^ UPDATING
      deriving (Eq, Ord, Show, Generic)

instance Hashable TableStatus

instance FromText TableStatus where
    parser = match "ACTIVE" TableStatusActive
         <|> match "CREATING" TableStatusCreating
         <|> match "DELETING" TableStatusDeleting
         <|> match "UPDATING" TableStatusUpdating

instance ToText TableStatus where
    toText TableStatusActive = "ACTIVE"
    toText TableStatusCreating = "CREATING"
    toText TableStatusDeleting = "DELETING"
    toText TableStatusUpdating = "UPDATING"

instance ToByteString TableStatus where
    toBS TableStatusActive = "ACTIVE"
    toBS TableStatusCreating = "CREATING"
    toBS TableStatusDeleting = "DELETING"
    toBS TableStatusUpdating = "UPDATING"

instance ToHeader TableStatus where
    toHeader k = toHeader k . toBS

instance ToQuery TableStatus where
    toQuery = toQuery . toBS

instance FromJSON TableStatus

-- | The amount of throughput consumed on the table affected by the operation.
newtype Capacity = Capacity
    { _cCapacityUnits :: Maybe Double
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Capacity' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CapacityUnits ::@ @Maybe Double@
--
capacity :: Capacity
capacity = Capacity
    { _cCapacityUnits = Nothing
    }

-- | The total number of capacity units consumed on a table or an index.
cCapacityUnits :: Lens' Capacity (Maybe Double)
cCapacityUnits = lens _cCapacityUnits (\s a -> s { _cCapacityUnits = a })

instance FromJSON Capacity

instance ToJSON Capacity

-- | A request to perform a DeleteItem operation.
newtype DeleteRequest = DeleteRequest
    { _drKey :: Map Text AttributeValue
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DeleteRequest' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @Map Text AttributeValue@
--
deleteRequest :: Map Text AttributeValue -- ^ 'drKey'
              -> DeleteRequest
deleteRequest p1 = DeleteRequest
    { _drKey = p1
    }

-- | A map of attribute name to attribute values, representing the primary key
-- of the item to delete. All of the table's primary key attributes must be
-- specified, and their data types must match those of the table's key schema.
drKey :: Lens' DeleteRequest (Map Text AttributeValue)
drKey = lens _drKey (\s a -> s { _drKey = a })

instance FromJSON DeleteRequest

instance ToJSON DeleteRequest

-- | Represents the new provisioned throughput settings to apply to a global
-- secondary index.
newtype GlobalSecondaryIndexUpdate = GlobalSecondaryIndexUpdate
    { _gsiuUpdate :: Maybe UpdateGlobalSecondaryIndexAction
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'GlobalSecondaryIndexUpdate' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Update ::@ @Maybe UpdateGlobalSecondaryIndexAction@
--
globalSecondaryIndexUpdate :: GlobalSecondaryIndexUpdate
globalSecondaryIndexUpdate = GlobalSecondaryIndexUpdate
    { _gsiuUpdate = Nothing
    }

-- | The name of a global secondary index, along with the updated provisioned
-- throughput settings that are to be applied to that index.
gsiuUpdate :: Lens' GlobalSecondaryIndexUpdate (Maybe UpdateGlobalSecondaryIndexAction)
gsiuUpdate = lens _gsiuUpdate (\s a -> s { _gsiuUpdate = a })

instance ToJSON GlobalSecondaryIndexUpdate

-- | A request to perform a PutItem operation.
newtype PutRequest = PutRequest
    { _prItem :: Map Text AttributeValue
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PutRequest' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Item ::@ @Map Text AttributeValue@
--
putRequest :: Map Text AttributeValue -- ^ 'prItem'
           -> PutRequest
putRequest p1 = PutRequest
    { _prItem = p1
    }

-- | A map of attribute name to attribute values, representing the primary key
-- of an item to be processed by PutItem. All of the table's primary key
-- attributes must be specified, and their data types must match those of the
-- table's key schema. If any attributes are present in the item which are
-- part of an index key schema for the table, their types must match the index
-- key schema.
prItem :: Lens' PutRequest (Map Text AttributeValue)
prItem = lens _prItem (\s a -> s { _prItem = a })

instance FromJSON PutRequest

instance ToJSON PutRequest

-- | Represents an attribute for describing the key schema for the table and
-- indexes.
data AttributeDefinition = AttributeDefinition
    { _adAttributeName :: Text
    , _adAttributeType :: ScalarAttributeType
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AttributeDefinition' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttributeName ::@ @Text@
--
-- * @AttributeType ::@ @ScalarAttributeType@
--
attributeDefinition :: Text -- ^ 'adAttributeName'
                    -> ScalarAttributeType -- ^ 'adAttributeType'
                    -> AttributeDefinition
attributeDefinition p1 p2 = AttributeDefinition
    { _adAttributeName = p1
    , _adAttributeType = p2
    }

-- | A name for the attribute.
adAttributeName :: Lens' AttributeDefinition Text
adAttributeName = lens _adAttributeName (\s a -> s { _adAttributeName = a })

-- | The data type for the attribute.
adAttributeType :: Lens' AttributeDefinition ScalarAttributeType
adAttributeType = lens _adAttributeType (\s a -> s { _adAttributeType = a })

instance FromJSON AttributeDefinition

instance ToJSON AttributeDefinition

-- | Represents the data for an attribute. You can set one, and only one, of the
-- elements.
data AttributeValue = AttributeValue
    { _avS :: Maybe Text
    , _avN :: Maybe Text
    , _avB :: Maybe Base64
    , _avSS :: [Text]
    , _avNS :: [Text]
    , _avBS :: [Base64]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AttributeValue' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @S ::@ @Maybe Text@
--
-- * @N ::@ @Maybe Text@
--
-- * @B ::@ @Maybe Base64@
--
-- * @SS ::@ @[Text]@
--
-- * @NS ::@ @[Text]@
--
-- * @BS ::@ @[Base64]@
--
attributeValue :: AttributeValue
attributeValue = AttributeValue
    { _avS = Nothing
    , _avN = Nothing
    , _avB = Nothing
    , _avSS = mempty
    , _avNS = mempty
    , _avBS = mempty
    }

-- | A String data type.
avS :: Lens' AttributeValue (Maybe Text)
avS = lens _avS (\s a -> s { _avS = a })

-- | A Number data type.
avN :: Lens' AttributeValue (Maybe Text)
avN = lens _avN (\s a -> s { _avN = a })

-- | A Binary data type.
avB :: Lens' AttributeValue (Maybe Base64)
avB = lens _avB (\s a -> s { _avB = a })

-- | A String set data type.
avSS :: Lens' AttributeValue [Text]
avSS = lens _avSS (\s a -> s { _avSS = a })

-- | Number set data type.
avNS :: Lens' AttributeValue [Text]
avNS = lens _avNS (\s a -> s { _avNS = a })

-- | A Binary set data type.
avBS :: Lens' AttributeValue [Base64]
avBS = lens _avBS (\s a -> s { _avBS = a })

instance FromJSON AttributeValue

instance ToJSON AttributeValue

-- | For the UpdateItem operation, represents the attributes to be modified,the
-- action to perform on each, and the new value for each. You cannot use
-- UpdateItem to update any primary key attributes. Instead, you will need to
-- delete the item, and then use PutItem to create a new item with new
-- attributes. Attribute values cannot be null; string and binary type
-- attributes must have lengths greater than zero; and set type attributes
-- must not be empty. Requests with empty values will be rejected with a
-- ValidationException.
data AttributeValueUpdate = AttributeValueUpdate
    { _avuValue :: Maybe AttributeValue
    , _avuAction :: Maybe AttributeAction
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AttributeValueUpdate' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Value ::@ @Maybe AttributeValue@
--
-- * @Action ::@ @Maybe AttributeAction@
--
attributeValueUpdate :: AttributeValueUpdate
attributeValueUpdate = AttributeValueUpdate
    { _avuValue = Nothing
    , _avuAction = Nothing
    }

-- | Represents the data for an attribute. You can set one, and only one, of the
-- elements.
avuValue :: Lens' AttributeValueUpdate (Maybe AttributeValue)
avuValue = lens _avuValue (\s a -> s { _avuValue = a })

-- | Specifies how to perform the update. Valid values are PUT, DELETE, and ADD.
-- The behavior depends on whether the specified primary key already exists in
-- the table. If an item with the specified Key is found in the table: PUT -
-- Adds the specified attribute to the item. If the attribute already exists,
-- it is replaced by the new value. DELETE - If no value is specified, the
-- attribute and its value are removed from the item. The data type of the
-- specified value must match the existing value's data type. If a set of
-- values is specified, then those values are subtracted from the old set. For
-- example, if the attribute value was the set [a,b,c] and the DELETE action
-- specified [a,c], then the final attribute value would be [b]. Specifying an
-- empty set is an error. ADD - If the attribute does not already exist, then
-- the attribute and its values are added to the item. If the attribute does
-- exist, then the behavior of ADD depends on the data type of the attribute:
-- If the existing attribute is a number, and if Value is also a number, then
-- the Value is mathematically added to the existing attribute. If Value is a
-- negative number, then it is subtracted from the existing attribute. If you
-- use ADD to increment or decrement a number value for an item that doesn't
-- exist before the update, DynamoDB uses 0 as the initial value. In addition,
-- if you use ADD to update an existing item, and intend to increment or
-- decrement an attribute value which does not yet exist, DynamoDB uses 0 as
-- the initial value. For example, suppose that the item you want to update
-- does not yet have an attribute named itemcount, but you decide to ADD the
-- number 3 to this attribute anyway, even though it currently does not exist.
-- DynamoDB will create the itemcount attribute, set its initial value to 0,
-- and finally add 3 to it. The result will be a new itemcount attribute in
-- the item, with a value of 3. If the existing data type is a set, and if the
-- Value is also a set, then the Value is added to the existing set. (This is
-- a set operation, not mathematical addition.) For example, if the attribute
-- value was the set [1,2], and the ADD action specified [3], then the final
-- attribute value would be [1,2,3]. An error occurs if an Add action is
-- specified for a set attribute and the attribute type specified does not
-- match the existing set type. Both sets must have the same primitive data
-- type. For example, if the existing data type is a set of strings, the Value
-- must also be a set of strings. The same holds true for number sets and
-- binary sets. This action is only valid for an existing attribute whose data
-- type is number or is a set. Do not use ADD for any other data types. If no
-- item with the specified Key is found: PUT - DynamoDB creates a new item
-- with the specified primary key, and then adds the attribute. DELETE -
-- Nothing happens; there is no attribute to delete. ADD - DynamoDB creates an
-- item with the supplied primary key and number (or set of numbers) for the
-- attribute value. The only data types allowed are number and number set; no
-- other data types can be specified.
avuAction :: Lens' AttributeValueUpdate (Maybe AttributeAction)
avuAction = lens _avuAction (\s a -> s { _avuAction = a })

instance ToJSON AttributeValueUpdate

-- | Represents a selection criteria for a Query or Scan operation. For a Query
-- operation, the condition specifies the key attributes to use when querying
-- a table or an index. For a Scan operation, the condition is used to
-- evaluate the scan results and return only the desired values. Multiple
-- conditions are "ANDed" together. In other words, all of the conditions must
-- be met to be included in the output.
data Condition = Condition
    { _c1AttributeValueList :: [AttributeValue]
    , _c1ComparisonOperator :: ComparisonOperator
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Condition' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttributeValueList ::@ @[AttributeValue]@
--
-- * @ComparisonOperator ::@ @ComparisonOperator@
--
condition :: ComparisonOperator -- ^ 'c1ComparisonOperator'
          -> Condition
condition p2 = Condition
    { _c1AttributeValueList = mempty
    , _c1ComparisonOperator = p2
    }

-- | One or more values to evaluate against the supplied attribute. This list
-- contains exactly one value, except for a BETWEEN or IN comparison, in which
-- case the list contains two values. For type Number, value comparisons are
-- numeric. String value comparisons for greater than, equals, or less than
-- are based on ASCII character code values. For example, a is greater than A,
-- and aa is greater than B. For a list of code values, see
-- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters. For Binary,
-- DynamoDB treats each byte of the binary data as unsigned when it compares
-- binary values, for example when evaluating query expressions.
c1AttributeValueList :: Lens' Condition [AttributeValue]
c1AttributeValueList =
    lens _c1AttributeValueList (\s a -> s { _c1AttributeValueList = a })

-- | A comparator for evaluating attributes. For example, equals, greater than,
-- less than, etc. Valid comparison operators for Query: EQ | LE | LT | GE |
-- GT | BEGINS_WITH | BETWEEN Valid comparison operators for Scan: EQ | NE |
-- LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH
-- | IN | BETWEEN For information on specifying data types in JSON, see JSON
-- Data Format in the Amazon DynamoDB Developer Guide. The following are
-- descriptions of each comparison operator. EQ : Equal. AttributeValueList
-- can contain only one AttributeValue of type String, Number, or Binary (not
-- a set). If an item contains an AttributeValue of a different type than the
-- one specified in the request, the value does not match. For example,
-- {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not equal
-- {"NS":["6", "2", "1"]}. NE : Not equal. AttributeValueList can contain only
-- one AttributeValue of type String, Number, or Binary (not a set). If an
-- item contains an AttributeValue of a different type than the one specified
-- in the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not equal {"NS":["6", "2", "1"]}. LE
-- : Less than or equal. AttributeValueList can contain only one
-- AttributeValue of type String, Number, or Binary (not a set). If an item
-- contains an AttributeValue of a different type than the one specified in
-- the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
-- "1"]}. LT : Less than. AttributeValueList can contain only one
-- AttributeValue of type String, Number, or Binary (not a set). If an item
-- contains an AttributeValue of a different type than the one specified in
-- the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
-- "1"]}. GE : Greater than or equal. AttributeValueList can contain only one
-- AttributeValue of type String, Number, or Binary (not a set). If an item
-- contains an AttributeValue of a different type than the one specified in
-- the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
-- "1"]}. GT : Greater than. AttributeValueList can contain only one
-- AttributeValue of type String, Number, or Binary (not a set). If an item
-- contains an AttributeValue of a different type than the one specified in
-- the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
-- "1"]}. NOT_NULL : The attribute exists. NULL : The attribute does not
-- exist. CONTAINS : checks for a subsequence, or value in a set.
-- AttributeValueList can contain only one AttributeValue of type String,
-- Number, or Binary (not a set). If the target attribute of the comparison is
-- a String, then the operation checks for a substring match. If the target
-- attribute of the comparison is Binary, then the operation looks for a
-- subsequence of the target that matches the input. If the target attribute
-- of the comparison is a set ("SS", "NS", or "BS"), then the operation checks
-- for a member of the set (not as a substring). NOT_CONTAINS : checks for
-- absence of a subsequence, or absence of a value in a set.
-- AttributeValueList can contain only one AttributeValue of type String,
-- Number, or Binary (not a set). If the target attribute of the comparison is
-- a String, then the operation checks for the absence of a substring match.
-- If the target attribute of the comparison is Binary, then the operation
-- checks for the absence of a subsequence of the target that matches the
-- input. If the target attribute of the comparison is a set ("SS", "NS", or
-- "BS"), then the operation checks for the absence of a member of the set
-- (not as a substring). BEGINS_WITH : checks for a prefix. AttributeValueList
-- can contain only one AttributeValue of type String or Binary (not a Number
-- or a set). The target attribute of the comparison must be a String or
-- Binary (not a Number or a set). IN : checks for exact matches.
-- AttributeValueList can contain more than one AttributeValue of type String,
-- Number, or Binary (not a set). The target attribute of the comparison must
-- be of the same type and exact value to match. A String never matches a
-- String set. BETWEEN : Greater than or equal to the first value, and less
-- than or equal to the second value. AttributeValueList must contain two
-- AttributeValue elements of the same type, either String, Number, or Binary
-- (not a set). A target attribute matches if the target value is greater
-- than, or equal to, the first element and less than, or equal to, the second
-- element. If an item contains an AttributeValue of a different type than the
-- one specified in the request, the value does not match. For example,
-- {"S":"6"} does not compare to {"N":"6"}. Also, {"N":"6"} does not compare
-- to {"NS":["6", "2", "1"]}.
c1ComparisonOperator :: Lens' Condition ComparisonOperator
c1ComparisonOperator =
    lens _c1ComparisonOperator (\s a -> s { _c1ComparisonOperator = a })

instance ToJSON Condition

-- | Represents the capacity units consumed by an operation. The data returned
-- includes the total provisioned throughput consumed, along with statistics
-- for the table and any indexes involved in the operation. ConsumedCapacity
-- is only returned if it was asked for in the request. For more information,
-- see Provisioned Throughput in the Amazon DynamoDB Developer Guide.
data ConsumedCapacity = ConsumedCapacity
    { _ccTableName :: Maybe Text
    , _ccCapacityUnits :: Maybe Double
    , _ccTable :: Maybe Capacity
    , _ccLocalSecondaryIndexes :: Map Text Capacity
    , _ccGlobalSecondaryIndexes :: Map Text Capacity
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ConsumedCapacity' data type.
--
-- 'ConsumedCapacity' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TableName ::@ @Maybe Text@
--
-- * @CapacityUnits ::@ @Maybe Double@
--
-- * @Table ::@ @Maybe Capacity@
--
-- * @LocalSecondaryIndexes ::@ @Map Text Capacity@
--
-- * @GlobalSecondaryIndexes ::@ @Map Text Capacity@
--
consumedCapacity :: ConsumedCapacity
consumedCapacity = ConsumedCapacity
    { _ccTableName = Nothing
    , _ccCapacityUnits = Nothing
    , _ccTable = Nothing
    , _ccLocalSecondaryIndexes = mempty
    , _ccGlobalSecondaryIndexes = mempty
    }

-- | The name of the table that was affected by the operation.
ccTableName :: Lens' ConsumedCapacity (Maybe Text)
ccTableName = lens _ccTableName (\s a -> s { _ccTableName = a })

-- | The total number of capacity units consumed by the operation.
ccCapacityUnits :: Lens' ConsumedCapacity (Maybe Double)
ccCapacityUnits = lens _ccCapacityUnits (\s a -> s { _ccCapacityUnits = a })

-- | The amount of throughput consumed on the table affected by the operation.
ccTable :: Lens' ConsumedCapacity (Maybe Capacity)
ccTable = lens _ccTable (\s a -> s { _ccTable = a })

-- | The amount of throughput consumed on each local index affected by the
-- operation.
ccLocalSecondaryIndexes :: Lens' ConsumedCapacity (Map Text Capacity)
ccLocalSecondaryIndexes =
    lens _ccLocalSecondaryIndexes
         (\s a -> s { _ccLocalSecondaryIndexes = a })

-- | The amount of throughput consumed on each global index affected by the
-- operation.
ccGlobalSecondaryIndexes :: Lens' ConsumedCapacity (Map Text Capacity)
ccGlobalSecondaryIndexes =
    lens _ccGlobalSecondaryIndexes
         (\s a -> s { _ccGlobalSecondaryIndexes = a })

instance FromJSON ConsumedCapacity

-- | Represents an attribute value used with conditional DeleteItem, PutItem or
-- UpdateItem operations. DynamoDB will check to see if the attribute value
-- already exists; or if the attribute exists and has a particular value
-- before updating it.
data ExpectedAttributeValue = ExpectedAttributeValue
    { _eavValue :: Maybe AttributeValue
    , _eavExists :: Maybe Bool
    , _eavComparisonOperator :: Maybe ComparisonOperator
    , _eavAttributeValueList :: [AttributeValue]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ExpectedAttributeValue' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Value ::@ @Maybe AttributeValue@
--
-- * @Exists ::@ @Maybe Bool@
--
-- * @ComparisonOperator ::@ @Maybe ComparisonOperator@
--
-- * @AttributeValueList ::@ @[AttributeValue]@
--
expectedAttributeValue :: ExpectedAttributeValue
expectedAttributeValue = ExpectedAttributeValue
    { _eavValue = Nothing
    , _eavExists = Nothing
    , _eavComparisonOperator = Nothing
    , _eavAttributeValueList = mempty
    }

-- | Represents the data for an attribute. You can set one, and only one, of the
-- elements.
eavValue :: Lens' ExpectedAttributeValue (Maybe AttributeValue)
eavValue = lens _eavValue (\s a -> s { _eavValue = a })

-- | Causes DynamoDB to evaluate the value before attempting a conditional
-- operation: If Exists is true, DynamoDB will check to see if that attribute
-- value already exists in the table. If it is found, then the operation
-- succeeds. If it is not found, the operation fails with a
-- ConditionalCheckFailedException. If Exists is false, DynamoDB assumes that
-- the attribute value does not exist in the table. If in fact the value does
-- not exist, then the assumption is valid and the operation succeeds. If the
-- value is found, despite the assumption that it does not exist, the
-- operation fails with a ConditionalCheckFailedException. The default setting
-- for Exists is true. If you supply a Value all by itself, DynamoDB assumes
-- the attribute exists: You don't have to set Exists to true, because it is
-- implied. DynamoDB returns a ValidationException if: Exists is true but
-- there is no Value to check. (You expect a value to exist, but don't specify
-- what that value is.) Exists is false but you also specify a Value. (You
-- cannot expect an attribute to have a value, while also expecting it not to
-- exist.) If you specify more than one condition for Exists, then all of the
-- conditions must evaluate to true. (In other words, the conditions are ANDed
-- together.) Otherwise, the conditional operation will fail.
eavExists :: Lens' ExpectedAttributeValue (Maybe Bool)
eavExists = lens _eavExists (\s a -> s { _eavExists = a })

eavComparisonOperator :: Lens' ExpectedAttributeValue (Maybe ComparisonOperator)
eavComparisonOperator =
    lens _eavComparisonOperator (\s a -> s { _eavComparisonOperator = a })

eavAttributeValueList :: Lens' ExpectedAttributeValue [AttributeValue]
eavAttributeValueList =
    lens _eavAttributeValueList (\s a -> s { _eavAttributeValueList = a })

instance ToJSON ExpectedAttributeValue

-- | Represents a global secondary index.
data GlobalSecondaryIndex = GlobalSecondaryIndex
    { _gsiIndexName :: Text
    , _gsiKeySchema :: List1 KeySchemaElement
    , _gsiProjection :: Projection
    , _gsiProvisionedThroughput :: ProvisionedThroughput
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'GlobalSecondaryIndex' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IndexName ::@ @Text@
--
-- * @KeySchema ::@ @List1 KeySchemaElement@
--
-- * @Projection ::@ @Projection@
--
-- * @ProvisionedThroughput ::@ @ProvisionedThroughput@
--
globalSecondaryIndex :: Text -- ^ 'gsiIndexName'
                     -> List1 KeySchemaElement -- ^ 'gsiKeySchema'
                     -> Projection -- ^ 'gsiProjection'
                     -> ProvisionedThroughput -- ^ 'gsiProvisionedThroughput'
                     -> GlobalSecondaryIndex
globalSecondaryIndex p1 p2 p3 p4 = GlobalSecondaryIndex
    { _gsiIndexName = p1
    , _gsiKeySchema = p2
    , _gsiProjection = p3
    , _gsiProvisionedThroughput = p4
    }

-- | The name of the global secondary index. The name must be unique among all
-- other indexes on this table.
gsiIndexName :: Lens' GlobalSecondaryIndex Text
gsiIndexName = lens _gsiIndexName (\s a -> s { _gsiIndexName = a })

-- | The complete key schema for a global secondary index, which consists of one
-- or more pairs of attribute names and key types (HASH or RANGE).
gsiKeySchema :: Lens' GlobalSecondaryIndex (List1 KeySchemaElement)
gsiKeySchema = lens _gsiKeySchema (\s a -> s { _gsiKeySchema = a })

-- | Represents attributes that are copied (projected) from the table into an
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected.
gsiProjection :: Lens' GlobalSecondaryIndex Projection
gsiProjection = lens _gsiProjection (\s a -> s { _gsiProjection = a })

-- | Represents the provisioned throughput settings for a specified table or
-- index. The settings can be modified using the UpdateTable operation. For
-- current minimum and maximum provisioned throughput values, see Limits in
-- the Amazon DynamoDB Developer Guide.
gsiProvisionedThroughput :: Lens' GlobalSecondaryIndex ProvisionedThroughput
gsiProvisionedThroughput =
    lens _gsiProvisionedThroughput
         (\s a -> s { _gsiProvisionedThroughput = a })

instance ToJSON GlobalSecondaryIndex

-- | Represents the properties of a global secondary index.
data GlobalSecondaryIndexDescription = GlobalSecondaryIndexDescription
    { _gsidIndexName :: Maybe Text
    , _gsidKeySchema :: Maybe (List1 KeySchemaElement)
    , _gsidProjection :: Maybe Projection
    , _gsidIndexStatus :: Maybe IndexStatus
    , _gsidProvisionedThroughput :: Maybe ProvisionedThroughputDescription
    , _gsidIndexSizeBytes :: Maybe Integer
    , _gsidItemCount :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'GlobalSecondaryIndexDescription' data type.
--
-- 'GlobalSecondaryIndexDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IndexName ::@ @Maybe Text@
--
-- * @KeySchema ::@ @Maybe (List1 KeySchemaElement)@
--
-- * @Projection ::@ @Maybe Projection@
--
-- * @IndexStatus ::@ @Maybe IndexStatus@
--
-- * @ProvisionedThroughput ::@ @Maybe ProvisionedThroughputDescription@
--
-- * @IndexSizeBytes ::@ @Maybe Integer@
--
-- * @ItemCount ::@ @Maybe Integer@
--
globalSecondaryIndexDescription :: GlobalSecondaryIndexDescription
globalSecondaryIndexDescription = GlobalSecondaryIndexDescription
    { _gsidIndexName = Nothing
    , _gsidKeySchema = Nothing
    , _gsidProjection = Nothing
    , _gsidIndexStatus = Nothing
    , _gsidProvisionedThroughput = Nothing
    , _gsidIndexSizeBytes = Nothing
    , _gsidItemCount = Nothing
    }

-- | The name of the global secondary index.
gsidIndexName :: Lens' GlobalSecondaryIndexDescription (Maybe Text)
gsidIndexName = lens _gsidIndexName (\s a -> s { _gsidIndexName = a })

-- | The complete key schema for the global secondary index, consisting of one
-- or more pairs of attribute names and key types (HASH or RANGE).
gsidKeySchema :: Lens' GlobalSecondaryIndexDescription (Maybe (List1 KeySchemaElement))
gsidKeySchema = lens _gsidKeySchema (\s a -> s { _gsidKeySchema = a })

-- | Represents attributes that are copied (projected) from the table into an
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected.
gsidProjection :: Lens' GlobalSecondaryIndexDescription (Maybe Projection)
gsidProjection = lens _gsidProjection (\s a -> s { _gsidProjection = a })

-- | The current state of the global secondary index: CREATING - The index is
-- being created, as the result of a CreateTable or UpdateTable operation.
-- UPDATING - The index is being updated, as the result of a CreateTable or
-- UpdateTable operation. DELETING - The index is being deleted, as the result
-- of a DeleteTable operation. ACTIVE - The index is ready for use.
gsidIndexStatus :: Lens' GlobalSecondaryIndexDescription (Maybe IndexStatus)
gsidIndexStatus = lens _gsidIndexStatus (\s a -> s { _gsidIndexStatus = a })

-- | Represents the provisioned throughput settings for the table, consisting of
-- read and write capacity units, along with data about increases and
-- decreases.
gsidProvisionedThroughput :: Lens' GlobalSecondaryIndexDescription (Maybe ProvisionedThroughputDescription)
gsidProvisionedThroughput =
    lens _gsidProvisionedThroughput
         (\s a -> s { _gsidProvisionedThroughput = a })

-- | The total size of the specified index, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be reflected
-- in this value.
gsidIndexSizeBytes :: Lens' GlobalSecondaryIndexDescription (Maybe Integer)
gsidIndexSizeBytes =
    lens _gsidIndexSizeBytes (\s a -> s { _gsidIndexSizeBytes = a })

-- | The number of items in the specified index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
gsidItemCount :: Lens' GlobalSecondaryIndexDescription (Maybe Integer)
gsidItemCount = lens _gsidItemCount (\s a -> s { _gsidItemCount = a })

instance FromJSON GlobalSecondaryIndexDescription

-- | Information about item collections, if any, that were affected by the
-- operation. ItemCollectionMetrics is only returned if it was asked for in
-- the request. If the table does not have any local secondary indexes, this
-- information is not returned in the response.
data ItemCollectionMetrics = ItemCollectionMetrics
    { _icmItemCollectionKey :: Map Text AttributeValue
    , _icmSizeEstimateRangeGB :: [Double]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ItemCollectionMetrics' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ItemCollectionKey ::@ @Map Text AttributeValue@
--
-- * @SizeEstimateRangeGB ::@ @[Double]@
--
itemCollectionMetrics :: ItemCollectionMetrics
itemCollectionMetrics = ItemCollectionMetrics
    { _icmItemCollectionKey = mempty
    , _icmSizeEstimateRangeGB = mempty
    }

-- | The hash key value of the item collection. This is the same as the hash key
-- of the item.
icmItemCollectionKey :: Lens' ItemCollectionMetrics (Map Text AttributeValue)
icmItemCollectionKey =
    lens _icmItemCollectionKey (\s a -> s { _icmItemCollectionKey = a })

-- | An estimate of item collection size, measured in gigabytes. This is a
-- two-element array containing a lower bound and an upper bound for the
-- estimate. The estimate includes the size of all the items in the table,
-- plus the size of all attributes projected into all of the local secondary
-- indexes on that table. Use this estimate to measure whether a local
-- secondary index is approaching its size limit. The estimate is subject to
-- change over time; therefore, do not rely on the precision or accuracy of
-- the estimate.
icmSizeEstimateRangeGB :: Lens' ItemCollectionMetrics [Double]
icmSizeEstimateRangeGB =
    lens _icmSizeEstimateRangeGB (\s a -> s { _icmSizeEstimateRangeGB = a })

instance FromJSON ItemCollectionMetrics

instance ToJSON ItemCollectionMetrics

-- | Represents a single element of a key schema. A key schema specifies the
-- attributes that make up the primary key of a table, or the key attributes
-- of an index.
data KeySchemaElement = KeySchemaElement
    { _kseAttributeName :: Text
    , _kseKeyType :: KeyType
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'KeySchemaElement' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttributeName ::@ @Text@
--
-- * @KeyType ::@ @KeyType@
--
keySchemaElement :: Text -- ^ 'kseAttributeName'
                 -> KeyType -- ^ 'kseKeyType'
                 -> KeySchemaElement
keySchemaElement p1 p2 = KeySchemaElement
    { _kseAttributeName = p1
    , _kseKeyType = p2
    }

-- | The name of a key attribute.
kseAttributeName :: Lens' KeySchemaElement Text
kseAttributeName =
    lens _kseAttributeName (\s a -> s { _kseAttributeName = a })

-- | The attribute data, consisting of the data type and the attribute value
-- itself.
kseKeyType :: Lens' KeySchemaElement KeyType
kseKeyType = lens _kseKeyType (\s a -> s { _kseKeyType = a })

instance FromJSON KeySchemaElement

instance ToJSON KeySchemaElement

-- | Represents a set of primary keys and, for each key, the attributes to
-- retrieve from the table.
data KeysAndAttributes = KeysAndAttributes
    { _kaaKeys :: List1 (Map Text AttributeValue)
    , _kaaAttributesToGet :: Maybe (List1 Text)
    , _kaaConsistentRead :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'KeysAndAttributes' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Keys ::@ @List1 (Map Text AttributeValue)@
--
-- * @AttributesToGet ::@ @Maybe (List1 Text)@
--
-- * @ConsistentRead ::@ @Maybe Bool@
--
keysAndAttributes :: List1 (Map Text AttributeValue) -- ^ 'kaaKeys'
                  -> KeysAndAttributes
keysAndAttributes p1 = KeysAndAttributes
    { _kaaKeys = p1
    , _kaaAttributesToGet = Nothing
    , _kaaConsistentRead = Nothing
    }

-- | The primary key attribute values that define the items and the attributes
-- associated with the items.
kaaKeys :: Lens' KeysAndAttributes (List1 (Map Text AttributeValue))
kaaKeys = lens _kaaKeys (\s a -> s { _kaaKeys = a })

-- | One or more attributes to retrieve from the table or index. If no attribute
-- names are specified then all attributes will be returned. If any of the
-- specified attributes are not found, they will not appear in the result.
kaaAttributesToGet :: Lens' KeysAndAttributes (Maybe (List1 Text))
kaaAttributesToGet =
    lens _kaaAttributesToGet (\s a -> s { _kaaAttributesToGet = a })

-- | The consistency of a read operation. If set to true, then a strongly
-- consistent read is used; otherwise, an eventually consistent read is used.
kaaConsistentRead :: Lens' KeysAndAttributes (Maybe Bool)
kaaConsistentRead =
    lens _kaaConsistentRead (\s a -> s { _kaaConsistentRead = a })

instance FromJSON KeysAndAttributes

instance ToJSON KeysAndAttributes

-- | Represents a local secondary index.
data LocalSecondaryIndex = LocalSecondaryIndex
    { _lsiIndexName :: Text
    , _lsiKeySchema :: List1 KeySchemaElement
    , _lsiProjection :: Projection
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LocalSecondaryIndex' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IndexName ::@ @Text@
--
-- * @KeySchema ::@ @List1 KeySchemaElement@
--
-- * @Projection ::@ @Projection@
--
localSecondaryIndex :: Text -- ^ 'lsiIndexName'
                    -> List1 KeySchemaElement -- ^ 'lsiKeySchema'
                    -> Projection -- ^ 'lsiProjection'
                    -> LocalSecondaryIndex
localSecondaryIndex p1 p2 p3 = LocalSecondaryIndex
    { _lsiIndexName = p1
    , _lsiKeySchema = p2
    , _lsiProjection = p3
    }

-- | The name of the local secondary index. The name must be unique among all
-- other indexes on this table.
lsiIndexName :: Lens' LocalSecondaryIndex Text
lsiIndexName = lens _lsiIndexName (\s a -> s { _lsiIndexName = a })

-- | The complete key schema for the local secondary index, consisting of one or
-- more pairs of attribute names and key types (HASH or RANGE).
lsiKeySchema :: Lens' LocalSecondaryIndex (List1 KeySchemaElement)
lsiKeySchema = lens _lsiKeySchema (\s a -> s { _lsiKeySchema = a })

-- | Represents attributes that are copied (projected) from the table into an
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected.
lsiProjection :: Lens' LocalSecondaryIndex Projection
lsiProjection = lens _lsiProjection (\s a -> s { _lsiProjection = a })

instance ToJSON LocalSecondaryIndex

-- | Represents the properties of a local secondary index.
data LocalSecondaryIndexDescription = LocalSecondaryIndexDescription
    { _lsidIndexName :: Maybe Text
    , _lsidKeySchema :: Maybe (List1 KeySchemaElement)
    , _lsidProjection :: Maybe Projection
    , _lsidIndexSizeBytes :: Maybe Integer
    , _lsidItemCount :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LocalSecondaryIndexDescription' data type.
--
-- 'LocalSecondaryIndexDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IndexName ::@ @Maybe Text@
--
-- * @KeySchema ::@ @Maybe (List1 KeySchemaElement)@
--
-- * @Projection ::@ @Maybe Projection@
--
-- * @IndexSizeBytes ::@ @Maybe Integer@
--
-- * @ItemCount ::@ @Maybe Integer@
--
localSecondaryIndexDescription :: LocalSecondaryIndexDescription
localSecondaryIndexDescription = LocalSecondaryIndexDescription
    { _lsidIndexName = Nothing
    , _lsidKeySchema = Nothing
    , _lsidProjection = Nothing
    , _lsidIndexSizeBytes = Nothing
    , _lsidItemCount = Nothing
    }

-- | Represents the name of the local secondary index.
lsidIndexName :: Lens' LocalSecondaryIndexDescription (Maybe Text)
lsidIndexName = lens _lsidIndexName (\s a -> s { _lsidIndexName = a })

-- | The complete index key schema, which consists of one or more pairs of
-- attribute names and key types (HASH or RANGE).
lsidKeySchema :: Lens' LocalSecondaryIndexDescription (Maybe (List1 KeySchemaElement))
lsidKeySchema = lens _lsidKeySchema (\s a -> s { _lsidKeySchema = a })

-- | Represents attributes that are copied (projected) from the table into an
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected.
lsidProjection :: Lens' LocalSecondaryIndexDescription (Maybe Projection)
lsidProjection = lens _lsidProjection (\s a -> s { _lsidProjection = a })

-- | The total size of the specified index, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be reflected
-- in this value.
lsidIndexSizeBytes :: Lens' LocalSecondaryIndexDescription (Maybe Integer)
lsidIndexSizeBytes =
    lens _lsidIndexSizeBytes (\s a -> s { _lsidIndexSizeBytes = a })

-- | The number of items in the specified index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
lsidItemCount :: Lens' LocalSecondaryIndexDescription (Maybe Integer)
lsidItemCount = lens _lsidItemCount (\s a -> s { _lsidItemCount = a })

instance FromJSON LocalSecondaryIndexDescription

-- | Represents attributes that are copied (projected) from the table into an
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected.
data Projection = Projection
    { _pProjectionType :: Maybe ProjectionType
    , _pNonKeyAttributes :: Maybe (List1 Text)
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Projection' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ProjectionType ::@ @Maybe ProjectionType@
--
-- * @NonKeyAttributes ::@ @Maybe (List1 Text)@
--
projection :: Projection
projection = Projection
    { _pProjectionType = Nothing
    , _pNonKeyAttributes = Nothing
    }

-- | The set of attributes that are projected into the index: KEYS_ONLY - Only
-- the index and primary keys are projected into the index. INCLUDE - Only the
-- specified table attributes are projected into the index. The list of
-- projected attributes are in NonKeyAttributes. ALL - All of the table
-- attributes are projected into the index.
pProjectionType :: Lens' Projection (Maybe ProjectionType)
pProjectionType = lens _pProjectionType (\s a -> s { _pProjectionType = a })

-- | Represents the non-key attribute names which will be projected into the
-- index. For local secondary indexes, the total count of NonKeyAttributes
-- summed across all of the local secondary indexes, must not exceed 20. If
-- you project the same attribute into two different indexes, this counts as
-- two distinct attributes when determining the total.
pNonKeyAttributes :: Lens' Projection (Maybe (List1 Text))
pNonKeyAttributes =
    lens _pNonKeyAttributes (\s a -> s { _pNonKeyAttributes = a })

instance FromJSON Projection

instance ToJSON Projection

-- | Represents the provisioned throughput settings for a specified table or
-- index. The settings can be modified using the UpdateTable operation. For
-- current minimum and maximum provisioned throughput values, see Limits in
-- the Amazon DynamoDB Developer Guide.
data ProvisionedThroughput = ProvisionedThroughput
    { _ptReadCapacityUnits :: !Integer
    , _ptWriteCapacityUnits :: !Integer
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ProvisionedThroughput' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReadCapacityUnits ::@ @Integer@
--
-- * @WriteCapacityUnits ::@ @Integer@
--
provisionedThroughput :: Integer -- ^ 'ptReadCapacityUnits'
                      -> Integer -- ^ 'ptWriteCapacityUnits'
                      -> ProvisionedThroughput
provisionedThroughput p1 p2 = ProvisionedThroughput
    { _ptReadCapacityUnits = p1
    , _ptWriteCapacityUnits = p2
    }

-- | The maximum number of strongly consistent reads consumed per second before
-- DynamoDB returns a ThrottlingException. For more information, see
-- Specifying Read and Write Requirements in the Amazon DynamoDB Developer
-- Guide.
ptReadCapacityUnits :: Lens' ProvisionedThroughput Integer
ptReadCapacityUnits =
    lens _ptReadCapacityUnits (\s a -> s { _ptReadCapacityUnits = a })

-- | The maximum number of writes consumed per second before DynamoDB returns a
-- ThrottlingException. For more information, see Specifying Read and Write
-- Requirements in the Amazon DynamoDB Developer Guide.
ptWriteCapacityUnits :: Lens' ProvisionedThroughput Integer
ptWriteCapacityUnits =
    lens _ptWriteCapacityUnits (\s a -> s { _ptWriteCapacityUnits = a })

instance FromJSON ProvisionedThroughput

instance ToJSON ProvisionedThroughput

-- | The provisioned throughput settings for the table, consisting of read and
-- write capacity units, along with data about increases and decreases.
data ProvisionedThroughputDescription = ProvisionedThroughputDescription
    { _ptdLastIncreaseDateTime :: Maybe ISO8601
    , _ptdLastDecreaseDateTime :: Maybe ISO8601
    , _ptdNumberOfDecreasesToday :: Maybe Integer
    , _ptdReadCapacityUnits :: Maybe Integer
    , _ptdWriteCapacityUnits :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ProvisionedThroughputDescription' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LastIncreaseDateTime ::@ @Maybe ISO8601@
--
-- * @LastDecreaseDateTime ::@ @Maybe ISO8601@
--
-- * @NumberOfDecreasesToday ::@ @Maybe Integer@
--
-- * @ReadCapacityUnits ::@ @Maybe Integer@
--
-- * @WriteCapacityUnits ::@ @Maybe Integer@
--
provisionedThroughputDescription :: ProvisionedThroughputDescription
provisionedThroughputDescription = ProvisionedThroughputDescription
    { _ptdLastIncreaseDateTime = Nothing
    , _ptdLastDecreaseDateTime = Nothing
    , _ptdNumberOfDecreasesToday = Nothing
    , _ptdReadCapacityUnits = Nothing
    , _ptdWriteCapacityUnits = Nothing
    }

-- | The date and time of the last provisioned throughput increase for this
-- table.
ptdLastIncreaseDateTime :: Lens' ProvisionedThroughputDescription (Maybe ISO8601)
ptdLastIncreaseDateTime =
    lens _ptdLastIncreaseDateTime
         (\s a -> s { _ptdLastIncreaseDateTime = a })

-- | The date and time of the last provisioned throughput decrease for this
-- table.
ptdLastDecreaseDateTime :: Lens' ProvisionedThroughputDescription (Maybe ISO8601)
ptdLastDecreaseDateTime =
    lens _ptdLastDecreaseDateTime
         (\s a -> s { _ptdLastDecreaseDateTime = a })

-- | The number of provisioned throughput decreases for this table during this
-- UTC calendar day. For current maximums on provisioned throughput decreases,
-- see Limits in the Amazon DynamoDB Developer Guide.
ptdNumberOfDecreasesToday :: Lens' ProvisionedThroughputDescription (Maybe Integer)
ptdNumberOfDecreasesToday =
    lens _ptdNumberOfDecreasesToday
         (\s a -> s { _ptdNumberOfDecreasesToday = a })

-- | The maximum number of strongly consistent reads consumed per second before
-- DynamoDB returns a ThrottlingException. Eventually consistent reads require
-- less effort than strongly consistent reads, so a setting of 50
-- ReadCapacityUnits per second provides 100 eventually consistent
-- ReadCapacityUnits per second.
ptdReadCapacityUnits :: Lens' ProvisionedThroughputDescription (Maybe Integer)
ptdReadCapacityUnits =
    lens _ptdReadCapacityUnits (\s a -> s { _ptdReadCapacityUnits = a })

-- | The maximum number of writes consumed per second before DynamoDB returns a
-- ThrottlingException.
ptdWriteCapacityUnits :: Lens' ProvisionedThroughputDescription (Maybe Integer)
ptdWriteCapacityUnits =
    lens _ptdWriteCapacityUnits (\s a -> s { _ptdWriteCapacityUnits = a })

instance FromJSON ProvisionedThroughputDescription

instance ToJSON ProvisionedThroughputDescription

-- | Represents the properties of a table.
data TableDescription = TableDescription
    { _tdAttributeDefinitions :: [AttributeDefinition]
    , _tdTableName :: Maybe Text
    , _tdKeySchema :: Maybe (List1 KeySchemaElement)
    , _tdTableStatus :: Maybe TableStatus
    , _tdCreationDateTime :: Maybe ISO8601
    , _tdProvisionedThroughput :: Maybe ProvisionedThroughputDescription
    , _tdTableSizeBytes :: Maybe Integer
    , _tdItemCount :: Maybe Integer
    , _tdLocalSecondaryIndexes :: [LocalSecondaryIndexDescription]
    , _tdGlobalSecondaryIndexes :: [GlobalSecondaryIndexDescription]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TableDescription' data type.
--
-- 'TableDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttributeDefinitions ::@ @[AttributeDefinition]@
--
-- * @TableName ::@ @Maybe Text@
--
-- * @KeySchema ::@ @Maybe (List1 KeySchemaElement)@
--
-- * @TableStatus ::@ @Maybe TableStatus@
--
-- * @CreationDateTime ::@ @Maybe ISO8601@
--
-- * @ProvisionedThroughput ::@ @Maybe ProvisionedThroughputDescription@
--
-- * @TableSizeBytes ::@ @Maybe Integer@
--
-- * @ItemCount ::@ @Maybe Integer@
--
-- * @LocalSecondaryIndexes ::@ @[LocalSecondaryIndexDescription]@
--
-- * @GlobalSecondaryIndexes ::@ @[GlobalSecondaryIndexDescription]@
--
tableDescription :: TableDescription
tableDescription = TableDescription
    { _tdAttributeDefinitions = mempty
    , _tdTableName = Nothing
    , _tdKeySchema = Nothing
    , _tdTableStatus = Nothing
    , _tdCreationDateTime = Nothing
    , _tdProvisionedThroughput = Nothing
    , _tdTableSizeBytes = Nothing
    , _tdItemCount = Nothing
    , _tdLocalSecondaryIndexes = mempty
    , _tdGlobalSecondaryIndexes = mempty
    }

-- | An array of AttributeDefinition objects. Each of these objects describes
-- one attribute in the table and index key schema. Each AttributeDefinition
-- object in this array is composed of: AttributeName - The name of the
-- attribute. AttributeType - The data type for the attribute.
tdAttributeDefinitions :: Lens' TableDescription [AttributeDefinition]
tdAttributeDefinitions =
    lens _tdAttributeDefinitions (\s a -> s { _tdAttributeDefinitions = a })

-- | The name of the table.
tdTableName :: Lens' TableDescription (Maybe Text)
tdTableName = lens _tdTableName (\s a -> s { _tdTableName = a })

-- | The primary key structure for the table. Each KeySchemaElement consists of:
-- AttributeName - The name of the attribute. KeyType - The key type for the
-- attribute. Can be either HASH or RANGE. For more information about primary
-- keys, see Primary Key in the Amazon DynamoDB Developer Guide.
tdKeySchema :: Lens' TableDescription (Maybe (List1 KeySchemaElement))
tdKeySchema = lens _tdKeySchema (\s a -> s { _tdKeySchema = a })

-- | The current state of the table: CREATING - The table is being created, as
-- the result of a CreateTable operation. UPDATING - The table is being
-- updated, as the result of an UpdateTable operation. DELETING - The table is
-- being deleted, as the result of a DeleteTable operation. ACTIVE - The table
-- is ready for use.
tdTableStatus :: Lens' TableDescription (Maybe TableStatus)
tdTableStatus = lens _tdTableStatus (\s a -> s { _tdTableStatus = a })

-- | The date and time when the table was created, in UNIX epoch time format.
tdCreationDateTime :: Lens' TableDescription (Maybe ISO8601)
tdCreationDateTime =
    lens _tdCreationDateTime (\s a -> s { _tdCreationDateTime = a })

-- | The provisioned throughput settings for the table, consisting of read and
-- write capacity units, along with data about increases and decreases.
tdProvisionedThroughput :: Lens' TableDescription (Maybe ProvisionedThroughputDescription)
tdProvisionedThroughput =
    lens _tdProvisionedThroughput
         (\s a -> s { _tdProvisionedThroughput = a })

-- | The total size of the specified table, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be reflected
-- in this value.
tdTableSizeBytes :: Lens' TableDescription (Maybe Integer)
tdTableSizeBytes =
    lens _tdTableSizeBytes (\s a -> s { _tdTableSizeBytes = a })

-- | The number of items in the specified table. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
tdItemCount :: Lens' TableDescription (Maybe Integer)
tdItemCount = lens _tdItemCount (\s a -> s { _tdItemCount = a })

-- | Represents one or more local secondary indexes on the table. Each index is
-- scoped to a given hash key value. Tables with one or more local secondary
-- indexes are subject to an item collection size limit, where the amount of
-- data within a given item collection cannot exceed 10 GB. Each element is
-- composed of: IndexName - The name of the local secondary index. KeySchema -
-- Specifies the complete index key schema. The attribute names in the key
-- schema must be between 1 and 255 characters (inclusive). The key schema
-- must begin with the same hash key attribute as the table. Projection -
-- Specifies attributes that are copied (projected) from the table into the
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected. Each attribute specification
-- is composed of: ProjectionType - One of the following: KEYS_ONLY - Only the
-- index and primary keys are projected into the index. INCLUDE - Only the
-- specified table attributes are projected into the index. The list of
-- projected attributes are in NonKeyAttributes. ALL - All of the table
-- attributes are projected into the index. NonKeyAttributes - A list of one
-- or more non-key attribute names that are projected into the secondary
-- index. The total count of attributes specified in NonKeyAttributes, summed
-- across all of the secondary indexes, must not exceed 20. If you project the
-- same attribute into two different indexes, this counts as two distinct
-- attributes when determining the total. IndexSizeBytes - Represents the
-- total size of the index, in bytes. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value. ItemCount - Represents the number of items in the index.
-- DynamoDB updates this value approximately every six hours. Recent changes
-- might not be reflected in this value. If the table is in the DELETING
-- state, no information about indexes will be returned.
tdLocalSecondaryIndexes :: Lens' TableDescription [LocalSecondaryIndexDescription]
tdLocalSecondaryIndexes =
    lens _tdLocalSecondaryIndexes
         (\s a -> s { _tdLocalSecondaryIndexes = a })

-- | The global secondary indexes, if any, on the table. Each index is scoped to
-- a given hash key value. Each element is composed of: IndexName - The name
-- of the global secondary index. IndexSizeBytes - The total size of the
-- global secondary index, in bytes. DynamoDB updates this value approximately
-- every six hours. Recent changes might not be reflected in this value.
-- IndexStatus - The current status of the global secondary index: CREATING -
-- The index is being created. UPDATING - The index is being updated. DELETING
-- - The index is being deleted. ACTIVE - The index is ready for use.
-- ItemCount - The number of items in the global secondary index. DynamoDB
-- updates this value approximately every six hours. Recent changes might not
-- be reflected in this value. KeySchema - Specifies the complete index key
-- schema. The attribute names in the key schema must be between 1 and 255
-- characters (inclusive). The key schema must begin with the same hash key
-- attribute as the table. Projection - Specifies attributes that are copied
-- (projected) from the table into the index. These are in addition to the
-- primary key attributes and index key attributes, which are automatically
-- projected. Each attribute specification is composed of: ProjectionType -
-- One of the following: KEYS_ONLY - Only the index and primary keys are
-- projected into the index. INCLUDE - Only the specified table attributes are
-- projected into the index. The list of projected attributes are in
-- NonKeyAttributes. ALL - All of the table attributes are projected into the
-- index. NonKeyAttributes - A list of one or more non-key attribute names
-- that are projected into the secondary index. The total count of attributes
-- specified in NonKeyAttributes, summed across all of the secondary indexes,
-- must not exceed 20. If you project the same attribute into two different
-- indexes, this counts as two distinct attributes when determining the total.
-- ProvisionedThroughput - The provisioned throughput settings for the global
-- secondary index, consisting of read and write capacity units, along with
-- data about increases and decreases. If the table is in the DELETING state,
-- no information about indexes will be returned.
tdGlobalSecondaryIndexes :: Lens' TableDescription [GlobalSecondaryIndexDescription]
tdGlobalSecondaryIndexes =
    lens _tdGlobalSecondaryIndexes
         (\s a -> s { _tdGlobalSecondaryIndexes = a })

instance FromJSON TableDescription

-- | The name of a global secondary index, along with the updated provisioned
-- throughput settings that are to be applied to that index.
data UpdateGlobalSecondaryIndexAction = UpdateGlobalSecondaryIndexAction
    { _ugsiaIndexName :: Text
    , _ugsiaProvisionedThroughput :: ProvisionedThroughput
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'UpdateGlobalSecondaryIndexAction' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IndexName ::@ @Text@
--
-- * @ProvisionedThroughput ::@ @ProvisionedThroughput@
--
updateGlobalSecondaryIndexAction :: Text -- ^ 'ugsiaIndexName'
                                 -> ProvisionedThroughput -- ^ 'ugsiaProvisionedThroughput'
                                 -> UpdateGlobalSecondaryIndexAction
updateGlobalSecondaryIndexAction p1 p2 = UpdateGlobalSecondaryIndexAction
    { _ugsiaIndexName = p1
    , _ugsiaProvisionedThroughput = p2
    }

-- | The name of the global secondary index to be updated.
ugsiaIndexName :: Lens' UpdateGlobalSecondaryIndexAction Text
ugsiaIndexName = lens _ugsiaIndexName (\s a -> s { _ugsiaIndexName = a })

-- | Represents the provisioned throughput settings for a specified table or
-- index. The settings can be modified using the UpdateTable operation. For
-- current minimum and maximum provisioned throughput values, see Limits in
-- the Amazon DynamoDB Developer Guide.
ugsiaProvisionedThroughput :: Lens' UpdateGlobalSecondaryIndexAction ProvisionedThroughput
ugsiaProvisionedThroughput =
    lens _ugsiaProvisionedThroughput
         (\s a -> s { _ugsiaProvisionedThroughput = a })

instance FromJSON UpdateGlobalSecondaryIndexAction

instance ToJSON UpdateGlobalSecondaryIndexAction

-- | Represents an operation to perform - either DeleteItem or PutItem. You can
-- only specify one of these operations, not both, in a single WriteRequest.
-- If you do need to perform both of these operations, you will need to
-- specify two separate WriteRequest objects.
data WriteRequest = WriteRequest
    { _wrPutRequest :: Maybe PutRequest
    , _wrDeleteRequest :: Maybe DeleteRequest
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WriteRequest' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PutRequest ::@ @Maybe PutRequest@
--
-- * @DeleteRequest ::@ @Maybe DeleteRequest@
--
writeRequest :: WriteRequest
writeRequest = WriteRequest
    { _wrPutRequest = Nothing
    , _wrDeleteRequest = Nothing
    }

-- | A request to perform a PutItem operation.
wrPutRequest :: Lens' WriteRequest (Maybe PutRequest)
wrPutRequest = lens _wrPutRequest (\s a -> s { _wrPutRequest = a })

-- | A request to perform a DeleteItem operation.
wrDeleteRequest :: Lens' WriteRequest (Maybe DeleteRequest)
wrDeleteRequest = lens _wrDeleteRequest (\s a -> s { _wrDeleteRequest = a })

instance FromJSON WriteRequest

instance ToJSON WriteRequest
