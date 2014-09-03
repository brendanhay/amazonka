{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.V2012_08_10.Types
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
module Network.AWS.DynamoDB.V2012_08_10.Types
    (
    -- * Service
      DynamoDB
    -- ** Errors
    , Er (..)

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
    , Capacity (..)
    , cCapacityUnits

    -- * DeleteRequest
    , DeleteRequest (..)
    , drKey

    -- * GlobalSecondaryIndexUpdate
    , GlobalSecondaryIndexUpdate (..)
    , gsiuUpdate

    -- * PutRequest
    , PutRequest (..)
    , prItem

    -- * AttributeDefinition
    , AttributeDefinition (..)
    , aeAttributeName
    , aeAttributeType

    -- * AttributeValue
    , AttributeValue (..)
    , avS
    , avN
    , avB
    , avSS
    , avNS
    , avBS

    -- * AttributeValueUpdate
    , AttributeValueUpdate (..)
    , avuValue
    , avuAction

    -- * Condition
    , Condition (..)
    , cnAttributeValueList
    , cnComparisonOperator

    -- * ConsumedCapacity
    , ConsumedCapacity (..)
    , ccTableName
    , ccCapacityUnits
    , ccTable
    , ccLocalSecondaryIndexes
    , ccGlobalSecondaryIndexes

    -- * ExpectedAttributeValue
    , ExpectedAttributeValue (..)
    , eavValue
    , eavExists
    , eavComparisonOperator
    , eavAttributeValueList

    -- * GlobalSecondaryIndex
    , GlobalSecondaryIndex (..)
    , gsiIndexName
    , gsiKeySchema
    , gsiProjection
    , gsiProvisionedThroughput

    -- * GlobalSecondaryIndexDescription
    , GlobalSecondaryIndexDescription (..)
    , gsidIndexName
    , gsidKeySchema
    , gsidProjection
    , gsidIndexStatus
    , gsidProvisionedThroughput
    , gsidIndexSizeBytes
    , gsidItemCount

    -- * ItemCollectionMetrics
    , ItemCollectionMetrics (..)
    , icmItemCollectionKey
    , icmSizeEstimateRangeGB

    -- * KeySchemaElement
    , KeySchemaElement (..)
    , kseAttributeName
    , kseKeyType

    -- * KeysAndAttributes
    , KeysAndAttributes (..)
    , kaaKeys
    , kaaAttributesToGet
    , kaaConsistentRead

    -- * LocalSecondaryIndex
    , LocalSecondaryIndex (..)
    , lsiIndexName
    , lsiKeySchema
    , lsiProjection

    -- * LocalSecondaryIndexDescription
    , LocalSecondaryIndexDescription (..)
    , lsidIndexName
    , lsidKeySchema
    , lsidProjection
    , lsidIndexSizeBytes
    , lsidItemCount

    -- * Projection
    , Projection (..)
    , pProjectionType
    , pNonKeyAttributes

    -- * ProvisionedThroughput
    , ProvisionedThroughput (..)
    , pvReadCapacityUnits
    , pvWriteCapacityUnits

    -- * ProvisionedThroughputDescription
    , ProvisionedThroughputDescription (..)
    , ptdLastIncreaseDateTime
    , ptdLastDecreaseDateTime
    , ptdNumberOfDecreasesToday
    , ptdReadCapacityUnits
    , ptdWriteCapacityUnits

    -- * TableDescription
    , TableDescription (..)
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
    , UpdateGlobalSecondaryIndexAction (..)
    , ugsiaIndexName
    , ugsiaProvisionedThroughput

    -- * WriteRequest
    , WriteRequest (..)
    , wsPutRequest
    , wsDeleteRequest

    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2012-08-10@) of the
-- @Amazon DynamoDB@ service.
data DynamoDB deriving (Typeable)

instance AWSService DynamoDB where
    type Sg DynamoDB = V4
    data Er DynamoDB
        = ConditionalCheckFailedException
            { _ccfeMessage :: Maybe Text
            }
        | DynamoDBClient HttpException
        | DynamoDBSerializer String
        | DynamoDBService String
        | InternalServerError
            { _iseMessage :: Maybe Text
            }
        | ItemCollectionSizeLimitExceededException
            { _icsleeMessage :: Maybe Text
            }
        | LimitExceededException
            { _leeMessage :: Maybe Text
            }
        | ProvisionedThroughputExceededException
            { _pteeMessage :: Maybe Text
            }
        | ResourceInUseException
            { _riueMessage :: Maybe Text
            }
        | ResourceNotFoundException
            { _rnfeMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "dynamodb"
        , _svcVersion  = "2012-08-10"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er DynamoDB)
deriving instance Generic (Er DynamoDB)

instance AWSError (Er DynamoDB) where
    awsError = const "DynamoDBError"

instance AWSServiceError (Er DynamoDB) where
    serviceError    = DynamoDBService
    clientError     = DynamoDBClient
    serializerError = DynamoDBSerializer

instance Exception (Er DynamoDB)

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
data AttributeAction
    = AttributeActionAdd -- ^ ADD
    | AttributeActionDelete -- ^ DELETE
    | AttributeActionPut -- ^ PUT
      deriving (Eq, Show, Generic)

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
      deriving (Eq, Show, Generic)

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
      deriving (Eq, Show, Generic)

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

-- | The current state of the global secondary index: CREATING - The index is
-- being created, as the result of a CreateTable or UpdateTable operation.
-- UPDATING - The index is being updated, as the result of a CreateTable or
-- UpdateTable operation. DELETING - The index is being deleted, as the result
-- of a DeleteTable operation. ACTIVE - The index is ready for use.
data IndexStatus
    = IndexStatusActive -- ^ ACTIVE
    | IndexStatusCreating -- ^ CREATING
    | IndexStatusDeleting -- ^ DELETING
    | IndexStatusUpdating -- ^ UPDATING
      deriving (Eq, Show, Generic)

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

-- | The attribute data, consisting of the data type and the attribute value
-- itself.
data KeyType
    = KeyTypeHash -- ^ HASH
    | KeyTypeRange -- ^ RANGE
      deriving (Eq, Show, Generic)

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

-- | The set of attributes that are projected into the index: KEYS_ONLY - Only
-- the index and primary keys are projected into the index. INCLUDE - Only the
-- specified table attributes are projected into the index. The list of
-- projected attributes are in NonKeyAttributes. ALL - All of the table
-- attributes are projected into the index.
data ProjectionType
    = ProjectionTypeAll -- ^ ALL
    | ProjectionTypeInclude -- ^ INCLUDE
    | ProjectionTypeKeysOnly -- ^ KEYS_ONLY
      deriving (Eq, Show, Generic)

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

-- | If set to TOTAL, the response includes ConsumedCapacity data for tables and
-- indexes. If set to INDEXES, the repsonse includes ConsumedCapacity for
-- indexes. If set to NONE (the default), ConsumedCapacity is not included in
-- the response.
data ReturnConsumedCapacity
    = ReturnConsumedCapacityIndexes -- ^ INDEXES
    | ReturnConsumedCapacityNone -- ^ NONE
    | ReturnConsumedCapacityTotal -- ^ TOTAL
      deriving (Eq, Show, Generic)

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

-- | If set to SIZE, statistics about item collections, if any, that were
-- modified during the operation are returned in the response. If set to NONE
-- (the default), no statistics are returned.
data ReturnItemCollectionMetrics
    = ReturnItemCollectionMetricsNone -- ^ NONE
    | ReturnItemCollectionMetricsSize -- ^ SIZE
      deriving (Eq, Show, Generic)

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

-- | Use ReturnValues if you want to get the item attributes as they appeared
-- before they were deleted. For DeleteItem, the valid values are: NONE - If
-- ReturnValues is not specified, or if its value is NONE, then nothing is
-- returned. (This is the default for ReturnValues.) ALL_OLD - The content of
-- the old item is returned.
data ReturnValue
    = ReturnValueAllNew -- ^ ALL_NEW
    | ReturnValueAllOld -- ^ ALL_OLD
    | ReturnValueNone -- ^ NONE
    | ReturnValueUpdatedNew -- ^ UPDATED_NEW
    | ReturnValueUpdatedOld -- ^ UPDATED_OLD
      deriving (Eq, Show, Generic)

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

-- | The data type for the attribute.
data ScalarAttributeType
    = ScalarAttributeTypeB -- ^ B
    | ScalarAttributeTypeN -- ^ N
    | ScalarAttributeTypeS -- ^ S
      deriving (Eq, Show, Generic)

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

-- | The attributes to be returned in the result. You can retrieve all item
-- attributes, specific item attributes, the count of matching items, or in
-- the case of an index, some or all of the attributes projected into the
-- index. ALL_ATTRIBUTES: Returns all of the item attributes from the
-- specified table or index. If you are querying a local secondary index, then
-- for each matching item in the index DynamoDB will fetch the entire item
-- from the parent table. If the index is configured to project all item
-- attributes, then all of the data can be obtained from the local secondary
-- index, and no fetching is required.. ALL_PROJECTED_ATTRIBUTES: Allowed only
-- when querying an index. Retrieves all attributes which have been projected
-- into the index. If the index is configured to project all attributes, this
-- is equivalent to specifying ALL_ATTRIBUTES. COUNT: Returns the number of
-- matching items, rather than the matching items themselves.
-- SPECIFIC_ATTRIBUTES : Returns only the attributes listed in
-- AttributesToGet. This is equivalent to specifying AttributesToGet without
-- specifying any value for Select. If you are querying a local secondary
-- index and request only attributes that are projected into that index, the
-- operation will read only the index and not the table. If any of the
-- requested attributes are not projected into the local secondary index,
-- DynamoDB will fetch each of these attributes from the parent table. This
-- extra fetching incurs additional throughput cost and latency. If you are
-- querying a global secondary index, you can only request attributes that are
-- projected into the index. Global secondary index queries cannot fetch
-- attributes from the parent table. If neither Select nor AttributesToGet are
-- specified, DynamoDB defaults to ALL_ATTRIBUTES when accessing a table, and
-- ALL_PROJECTED_ATTRIBUTES when accessing an index. You cannot use both
-- Select and AttributesToGet together in a single request, unless the value
-- for Select is SPECIFIC_ATTRIBUTES. (This usage is equivalent to specifying
-- AttributesToGet without any value for Select.).
data Select
    = SelectAllAttributes -- ^ ALL_ATTRIBUTES
    | SelectAllProjectedAttributes -- ^ ALL_PROJECTED_ATTRIBUTES
    | SelectCount -- ^ COUNT
    | SelectSpecificAttributes -- ^ SPECIFIC_ATTRIBUTES
      deriving (Eq, Show, Generic)

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

-- | The current state of the table: CREATING - The table is being created, as
-- the result of a CreateTable operation. UPDATING - The table is being
-- updated, as the result of an UpdateTable operation. DELETING - The table is
-- being deleted, as the result of a DeleteTable operation. ACTIVE - The table
-- is ready for use.
data TableStatus
    = TableStatusActive -- ^ ACTIVE
    | TableStatusCreating -- ^ CREATING
    | TableStatusDeleting -- ^ DELETING
    | TableStatusUpdating -- ^ UPDATING
      deriving (Eq, Show, Generic)

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
      -- ^ The total number of capacity units consumed on a table or an
      -- index.
    } deriving (Show, Generic)

-- | The total number of capacity units consumed on a table or an index.
cCapacityUnits
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> Capacity
    -> f Capacity
cCapacityUnits f x =
    (\y -> x { _cCapacityUnits = y })
       <$> f (_cCapacityUnits x)
{-# INLINE cCapacityUnits #-}

instance FromJSON Capacity

instance ToJSON Capacity

-- | A request to perform a DeleteItem operation.
newtype DeleteRequest = DeleteRequest
    { _drKey :: Map Text AttributeValue
      -- ^ A map of attribute name to attribute values, representing the
      -- primary key of the item to delete. All of the table's primary key
      -- attributes must be specified, and their data types must match
      -- those of the table's key schema.
    } deriving (Show, Generic)

-- | A map of attribute name to attribute values, representing the primary key
-- of the item to delete. All of the table's primary key attributes must be
-- specified, and their data types must match those of the table's key schema.
drKey
    :: Functor f
    => (Map Text AttributeValue
    -> f (Map Text AttributeValue))
    -> DeleteRequest
    -> f DeleteRequest
drKey f x =
    (\y -> x { _drKey = y })
       <$> f (_drKey x)
{-# INLINE drKey #-}

instance FromJSON DeleteRequest

instance ToJSON DeleteRequest

-- | Represents the new provisioned throughput settings to apply to a global
-- secondary index.
newtype GlobalSecondaryIndexUpdate = GlobalSecondaryIndexUpdate
    { _gsiuUpdate :: Maybe UpdateGlobalSecondaryIndexAction
      -- ^ The name of a global secondary index, along with the updated
      -- provisioned throughput settings that are to be applied to that
      -- index.
    } deriving (Show, Generic)

-- | The name of a global secondary index, along with the updated provisioned
-- throughput settings that are to be applied to that index.
gsiuUpdate
    :: Functor f
    => (Maybe UpdateGlobalSecondaryIndexAction
    -> f (Maybe UpdateGlobalSecondaryIndexAction))
    -> GlobalSecondaryIndexUpdate
    -> f GlobalSecondaryIndexUpdate
gsiuUpdate f x =
    (\y -> x { _gsiuUpdate = y })
       <$> f (_gsiuUpdate x)
{-# INLINE gsiuUpdate #-}

instance ToJSON GlobalSecondaryIndexUpdate

-- | A request to perform a PutItem operation.
newtype PutRequest = PutRequest
    { _prItem :: Map Text AttributeValue
      -- ^ A map of attribute name to attribute values, representing the
      -- primary key of an item to be processed by PutItem. All of the
      -- table's primary key attributes must be specified, and their data
      -- types must match those of the table's key schema. If any
      -- attributes are present in the item which are part of an index key
      -- schema for the table, their types must match the index key
      -- schema.
    } deriving (Show, Generic)

-- | A map of attribute name to attribute values, representing the primary key
-- of an item to be processed by PutItem. All of the table's primary key
-- attributes must be specified, and their data types must match those of the
-- table's key schema. If any attributes are present in the item which are
-- part of an index key schema for the table, their types must match the index
-- key schema.
prItem
    :: Functor f
    => (Map Text AttributeValue
    -> f (Map Text AttributeValue))
    -> PutRequest
    -> f PutRequest
prItem f x =
    (\y -> x { _prItem = y })
       <$> f (_prItem x)
{-# INLINE prItem #-}

instance FromJSON PutRequest

instance ToJSON PutRequest

-- | Represents an attribute for describing the key schema for the table and
-- indexes.
data AttributeDefinition = AttributeDefinition
    { _aeAttributeName :: Text
      -- ^ A name for the attribute.
    , _aeAttributeType :: ScalarAttributeType
      -- ^ The data type for the attribute.
    } deriving (Show, Generic)

-- | A name for the attribute.
aeAttributeName
    :: Functor f
    => (Text
    -> f (Text))
    -> AttributeDefinition
    -> f AttributeDefinition
aeAttributeName f x =
    (\y -> x { _aeAttributeName = y })
       <$> f (_aeAttributeName x)
{-# INLINE aeAttributeName #-}

-- | The data type for the attribute.
aeAttributeType
    :: Functor f
    => (ScalarAttributeType
    -> f (ScalarAttributeType))
    -> AttributeDefinition
    -> f AttributeDefinition
aeAttributeType f x =
    (\y -> x { _aeAttributeType = y })
       <$> f (_aeAttributeType x)
{-# INLINE aeAttributeType #-}

instance FromJSON AttributeDefinition

instance ToJSON AttributeDefinition

-- | Represents the data for an attribute. You can set one, and only one, of the
-- elements.
data AttributeValue = AttributeValue
    { _avS :: Maybe Text
      -- ^ A String data type.
    , _avN :: Maybe Text
      -- ^ A Number data type.
    , _avB :: Maybe Base64
      -- ^ A Binary data type.
    , _avSS :: [Text]
      -- ^ A String set data type.
    , _avNS :: [Text]
      -- ^ Number set data type.
    , _avBS :: [Base64]
      -- ^ A Binary set data type.
    } deriving (Show, Generic)

-- | A String data type.
avS
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AttributeValue
    -> f AttributeValue
avS f x =
    (\y -> x { _avS = y })
       <$> f (_avS x)
{-# INLINE avS #-}

-- | A Number data type.
avN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AttributeValue
    -> f AttributeValue
avN f x =
    (\y -> x { _avN = y })
       <$> f (_avN x)
{-# INLINE avN #-}

-- | A Binary data type.
avB
    :: Functor f
    => (Maybe Base64
    -> f (Maybe Base64))
    -> AttributeValue
    -> f AttributeValue
avB f x =
    (\y -> x { _avB = y })
       <$> f (_avB x)
{-# INLINE avB #-}

-- | A String set data type.
avSS
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> AttributeValue
    -> f AttributeValue
avSS f x =
    (\y -> x { _avSS = y })
       <$> f (_avSS x)
{-# INLINE avSS #-}

-- | Number set data type.
avNS
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> AttributeValue
    -> f AttributeValue
avNS f x =
    (\y -> x { _avNS = y })
       <$> f (_avNS x)
{-# INLINE avNS #-}

-- | A Binary set data type.
avBS
    :: Functor f
    => ([Base64]
    -> f ([Base64]))
    -> AttributeValue
    -> f AttributeValue
avBS f x =
    (\y -> x { _avBS = y })
       <$> f (_avBS x)
{-# INLINE avBS #-}

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
      -- ^ Represents the data for an attribute. You can set one, and only
      -- one, of the elements.
    , _avuAction :: Maybe AttributeAction
      -- ^ Specifies how to perform the update. Valid values are PUT,
      -- DELETE, and ADD. The behavior depends on whether the specified
      -- primary key already exists in the table. If an item with the
      -- specified Key is found in the table: PUT - Adds the specified
      -- attribute to the item. If the attribute already exists, it is
      -- replaced by the new value. DELETE - If no value is specified, the
      -- attribute and its value are removed from the item. The data type
      -- of the specified value must match the existing value's data type.
      -- If a set of values is specified, then those values are subtracted
      -- from the old set. For example, if the attribute value was the set
      -- [a,b,c] and the DELETE action specified [a,c], then the final
      -- attribute value would be [b]. Specifying an empty set is an
      -- error. ADD - If the attribute does not already exist, then the
      -- attribute and its values are added to the item. If the attribute
      -- does exist, then the behavior of ADD depends on the data type of
      -- the attribute: If the existing attribute is a number, and if
      -- Value is also a number, then the Value is mathematically added to
      -- the existing attribute. If Value is a negative number, then it is
      -- subtracted from the existing attribute. If you use ADD to
      -- increment or decrement a number value for an item that doesn't
      -- exist before the update, DynamoDB uses 0 as the initial value. In
      -- addition, if you use ADD to update an existing item, and intend
      -- to increment or decrement an attribute value which does not yet
      -- exist, DynamoDB uses 0 as the initial value. For example, suppose
      -- that the item you want to update does not yet have an attribute
      -- named itemcount, but you decide to ADD the number 3 to this
      -- attribute anyway, even though it currently does not exist.
      -- DynamoDB will create the itemcount attribute, set its initial
      -- value to 0, and finally add 3 to it. The result will be a new
      -- itemcount attribute in the item, with a value of 3. If the
      -- existing data type is a set, and if the Value is also a set, then
      -- the Value is added to the existing set. (This is a set operation,
      -- not mathematical addition.) For example, if the attribute value
      -- was the set [1,2], and the ADD action specified [3], then the
      -- final attribute value would be [1,2,3]. An error occurs if an Add
      -- action is specified for a set attribute and the attribute type
      -- specified does not match the existing set type. Both sets must
      -- have the same primitive data type. For example, if the existing
      -- data type is a set of strings, the Value must also be a set of
      -- strings. The same holds true for number sets and binary sets.
      -- This action is only valid for an existing attribute whose data
      -- type is number or is a set. Do not use ADD for any other data
      -- types. If no item with the specified Key is found: PUT - DynamoDB
      -- creates a new item with the specified primary key, and then adds
      -- the attribute. DELETE - Nothing happens; there is no attribute to
      -- delete. ADD - DynamoDB creates an item with the supplied primary
      -- key and number (or set of numbers) for the attribute value. The
      -- only data types allowed are number and number set; no other data
      -- types can be specified.
    } deriving (Show, Generic)

-- | Represents the data for an attribute. You can set one, and only one, of the
-- elements.
avuValue
    :: Functor f
    => (Maybe AttributeValue
    -> f (Maybe AttributeValue))
    -> AttributeValueUpdate
    -> f AttributeValueUpdate
avuValue f x =
    (\y -> x { _avuValue = y })
       <$> f (_avuValue x)
{-# INLINE avuValue #-}

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
avuAction
    :: Functor f
    => (Maybe AttributeAction
    -> f (Maybe AttributeAction))
    -> AttributeValueUpdate
    -> f AttributeValueUpdate
avuAction f x =
    (\y -> x { _avuAction = y })
       <$> f (_avuAction x)
{-# INLINE avuAction #-}

instance ToJSON AttributeValueUpdate

-- | Represents a selection criteria for a Query or Scan operation. For a Query
-- operation, the condition specifies the key attributes to use when querying
-- a table or an index. For a Scan operation, the condition is used to
-- evaluate the scan results and return only the desired values. Multiple
-- conditions are "ANDed" together. In other words, all of the conditions must
-- be met to be included in the output.
data Condition = Condition
    { _cnAttributeValueList :: [AttributeValue]
      -- ^ One or more values to evaluate against the supplied attribute.
      -- This list contains exactly one value, except for a BETWEEN or IN
      -- comparison, in which case the list contains two values. For type
      -- Number, value comparisons are numeric. String value comparisons
      -- for greater than, equals, or less than are based on ASCII
      -- character code values. For example, a is greater than A, and aa
      -- is greater than B. For a list of code values, see
      -- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters.
      -- For Binary, DynamoDB treats each byte of the binary data as
      -- unsigned when it compares binary values, for example when
      -- evaluating query expressions.
    , _cnComparisonOperator :: ComparisonOperator
      -- ^ A comparator for evaluating attributes. For example, equals,
      -- greater than, less than, etc. Valid comparison operators for
      -- Query: EQ | LE | LT | GE | GT | BEGINS_WITH | BETWEEN Valid
      -- comparison operators for Scan: EQ | NE | LE | LT | GE | GT |
      -- NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN |
      -- BETWEEN For information on specifying data types in JSON, see
      -- JSON Data Format in the Amazon DynamoDB Developer Guide. The
      -- following are descriptions of each comparison operator. EQ :
      -- Equal. AttributeValueList can contain only one AttributeValue of
      -- type String, Number, or Binary (not a set). If an item contains
      -- an AttributeValue of a different type than the one specified in
      -- the request, the value does not match. For example, {"S":"6"}
      -- does not equal {"N":"6"}. Also, {"N":"6"} does not equal
      -- {"NS":["6", "2", "1"]}. NE : Not equal. AttributeValueList can
      -- contain only one AttributeValue of type String, Number, or Binary
      -- (not a set). If an item contains an AttributeValue of a different
      -- type than the one specified in the request, the value does not
      -- match. For example, {"S":"6"} does not equal {"N":"6"}. Also,
      -- {"N":"6"} does not equal {"NS":["6", "2", "1"]}. LE : Less than
      -- or equal. AttributeValueList can contain only one AttributeValue
      -- of type String, Number, or Binary (not a set). If an item
      -- contains an AttributeValue of a different type than the one
      -- specified in the request, the value does not match. For example,
      -- {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not
      -- compare to {"NS":["6", "2", "1"]}. LT : Less than.
      -- AttributeValueList can contain only one AttributeValue of type
      -- String, Number, or Binary (not a set). If an item contains an
      -- AttributeValue of a different type than the one specified in the
      -- request, the value does not match. For example, {"S":"6"} does
      -- not equal {"N":"6"}. Also, {"N":"6"} does not compare to
      -- {"NS":["6", "2", "1"]}. GE : Greater than or equal.
      -- AttributeValueList can contain only one AttributeValue of type
      -- String, Number, or Binary (not a set). If an item contains an
      -- AttributeValue of a different type than the one specified in the
      -- request, the value does not match. For example, {"S":"6"} does
      -- not equal {"N":"6"}. Also, {"N":"6"} does not compare to
      -- {"NS":["6", "2", "1"]}. GT : Greater than. AttributeValueList can
      -- contain only one AttributeValue of type String, Number, or Binary
      -- (not a set). If an item contains an AttributeValue of a different
      -- type than the one specified in the request, the value does not
      -- match. For example, {"S":"6"} does not equal {"N":"6"}. Also,
      -- {"N":"6"} does not compare to {"NS":["6", "2", "1"]}. NOT_NULL :
      -- The attribute exists. NULL : The attribute does not exist.
      -- CONTAINS : checks for a subsequence, or value in a set.
      -- AttributeValueList can contain only one AttributeValue of type
      -- String, Number, or Binary (not a set). If the target attribute of
      -- the comparison is a String, then the operation checks for a
      -- substring match. If the target attribute of the comparison is
      -- Binary, then the operation looks for a subsequence of the target
      -- that matches the input. If the target attribute of the comparison
      -- is a set ("SS", "NS", or "BS"), then the operation checks for a
      -- member of the set (not as a substring). NOT_CONTAINS : checks for
      -- absence of a subsequence, or absence of a value in a set.
      -- AttributeValueList can contain only one AttributeValue of type
      -- String, Number, or Binary (not a set). If the target attribute of
      -- the comparison is a String, then the operation checks for the
      -- absence of a substring match. If the target attribute of the
      -- comparison is Binary, then the operation checks for the absence
      -- of a subsequence of the target that matches the input. If the
      -- target attribute of the comparison is a set ("SS", "NS", or
      -- "BS"), then the operation checks for the absence of a member of
      -- the set (not as a substring). BEGINS_WITH : checks for a prefix.
      -- AttributeValueList can contain only one AttributeValue of type
      -- String or Binary (not a Number or a set). The target attribute of
      -- the comparison must be a String or Binary (not a Number or a
      -- set). IN : checks for exact matches. AttributeValueList can
      -- contain more than one AttributeValue of type String, Number, or
      -- Binary (not a set). The target attribute of the comparison must
      -- be of the same type and exact value to match. A String never
      -- matches a String set. BETWEEN : Greater than or equal to the
      -- first value, and less than or equal to the second value.
      -- AttributeValueList must contain two AttributeValue elements of
      -- the same type, either String, Number, or Binary (not a set). A
      -- target attribute matches if the target value is greater than, or
      -- equal to, the first element and less than, or equal to, the
      -- second element. If an item contains an AttributeValue of a
      -- different type than the one specified in the request, the value
      -- does not match. For example, {"S":"6"} does not compare to
      -- {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
      -- "1"]}.
    } deriving (Show, Generic)

-- | One or more values to evaluate against the supplied attribute. This list
-- contains exactly one value, except for a BETWEEN or IN comparison, in which
-- case the list contains two values. For type Number, value comparisons are
-- numeric. String value comparisons for greater than, equals, or less than
-- are based on ASCII character code values. For example, a is greater than A,
-- and aa is greater than B. For a list of code values, see
-- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters. For Binary,
-- DynamoDB treats each byte of the binary data as unsigned when it compares
-- binary values, for example when evaluating query expressions.
cnAttributeValueList
    :: Functor f
    => ([AttributeValue]
    -> f ([AttributeValue]))
    -> Condition
    -> f Condition
cnAttributeValueList f x =
    (\y -> x { _cnAttributeValueList = y })
       <$> f (_cnAttributeValueList x)
{-# INLINE cnAttributeValueList #-}

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
cnComparisonOperator
    :: Functor f
    => (ComparisonOperator
    -> f (ComparisonOperator))
    -> Condition
    -> f Condition
cnComparisonOperator f x =
    (\y -> x { _cnComparisonOperator = y })
       <$> f (_cnComparisonOperator x)
{-# INLINE cnComparisonOperator #-}

instance ToJSON Condition

-- | Represents the capacity units consumed by an operation. The data returned
-- includes the total provisioned throughput consumed, along with statistics
-- for the table and any indexes involved in the operation. ConsumedCapacity
-- is only returned if it was asked for in the request. For more information,
-- see Provisioned Throughput in the Amazon DynamoDB Developer Guide.
data ConsumedCapacity = ConsumedCapacity
    { _ccTableName :: Maybe Text
      -- ^ The name of the table that was affected by the operation.
    , _ccCapacityUnits :: Maybe Double
      -- ^ The total number of capacity units consumed by the operation.
    , _ccTable :: Maybe Capacity
      -- ^ The amount of throughput consumed on the table affected by the
      -- operation.
    , _ccLocalSecondaryIndexes :: Map Text Capacity
      -- ^ The amount of throughput consumed on each local index affected by
      -- the operation.
    , _ccGlobalSecondaryIndexes :: Map Text Capacity
      -- ^ The amount of throughput consumed on each global index affected
      -- by the operation.
    } deriving (Show, Generic)

-- | The name of the table that was affected by the operation.
ccTableName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ConsumedCapacity
    -> f ConsumedCapacity
ccTableName f x =
    (\y -> x { _ccTableName = y })
       <$> f (_ccTableName x)
{-# INLINE ccTableName #-}

-- | The total number of capacity units consumed by the operation.
ccCapacityUnits
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ConsumedCapacity
    -> f ConsumedCapacity
ccCapacityUnits f x =
    (\y -> x { _ccCapacityUnits = y })
       <$> f (_ccCapacityUnits x)
{-# INLINE ccCapacityUnits #-}

-- | The amount of throughput consumed on the table affected by the operation.
ccTable
    :: Functor f
    => (Maybe Capacity
    -> f (Maybe Capacity))
    -> ConsumedCapacity
    -> f ConsumedCapacity
ccTable f x =
    (\y -> x { _ccTable = y })
       <$> f (_ccTable x)
{-# INLINE ccTable #-}

-- | The amount of throughput consumed on each local index affected by the
-- operation.
ccLocalSecondaryIndexes
    :: Functor f
    => (Map Text Capacity
    -> f (Map Text Capacity))
    -> ConsumedCapacity
    -> f ConsumedCapacity
ccLocalSecondaryIndexes f x =
    (\y -> x { _ccLocalSecondaryIndexes = y })
       <$> f (_ccLocalSecondaryIndexes x)
{-# INLINE ccLocalSecondaryIndexes #-}

-- | The amount of throughput consumed on each global index affected by the
-- operation.
ccGlobalSecondaryIndexes
    :: Functor f
    => (Map Text Capacity
    -> f (Map Text Capacity))
    -> ConsumedCapacity
    -> f ConsumedCapacity
ccGlobalSecondaryIndexes f x =
    (\y -> x { _ccGlobalSecondaryIndexes = y })
       <$> f (_ccGlobalSecondaryIndexes x)
{-# INLINE ccGlobalSecondaryIndexes #-}

instance FromJSON ConsumedCapacity

-- | Represents an attribute value used with conditional DeleteItem, PutItem or
-- UpdateItem operations. DynamoDB will check to see if the attribute value
-- already exists; or if the attribute exists and has a particular value
-- before updating it.
data ExpectedAttributeValue = ExpectedAttributeValue
    { _eavValue :: Maybe AttributeValue
      -- ^ Represents the data for an attribute. You can set one, and only
      -- one, of the elements.
    , _eavExists :: Maybe Bool
      -- ^ Causes DynamoDB to evaluate the value before attempting a
      -- conditional operation: If Exists is true, DynamoDB will check to
      -- see if that attribute value already exists in the table. If it is
      -- found, then the operation succeeds. If it is not found, the
      -- operation fails with a ConditionalCheckFailedException. If Exists
      -- is false, DynamoDB assumes that the attribute value does not
      -- exist in the table. If in fact the value does not exist, then the
      -- assumption is valid and the operation succeeds. If the value is
      -- found, despite the assumption that it does not exist, the
      -- operation fails with a ConditionalCheckFailedException. The
      -- default setting for Exists is true. If you supply a Value all by
      -- itself, DynamoDB assumes the attribute exists: You don't have to
      -- set Exists to true, because it is implied. DynamoDB returns a
      -- ValidationException if: Exists is true but there is no Value to
      -- check. (You expect a value to exist, but don't specify what that
      -- value is.) Exists is false but you also specify a Value. (You
      -- cannot expect an attribute to have a value, while also expecting
      -- it not to exist.) If you specify more than one condition for
      -- Exists, then all of the conditions must evaluate to true. (In
      -- other words, the conditions are ANDed together.) Otherwise, the
      -- conditional operation will fail.
    , _eavComparisonOperator :: Maybe ComparisonOperator
    , _eavAttributeValueList :: [AttributeValue]
    } deriving (Show, Generic)

-- | Represents the data for an attribute. You can set one, and only one, of the
-- elements.
eavValue
    :: Functor f
    => (Maybe AttributeValue
    -> f (Maybe AttributeValue))
    -> ExpectedAttributeValue
    -> f ExpectedAttributeValue
eavValue f x =
    (\y -> x { _eavValue = y })
       <$> f (_eavValue x)
{-# INLINE eavValue #-}

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
eavExists
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> ExpectedAttributeValue
    -> f ExpectedAttributeValue
eavExists f x =
    (\y -> x { _eavExists = y })
       <$> f (_eavExists x)
{-# INLINE eavExists #-}

eavComparisonOperator
    :: Functor f
    => (Maybe ComparisonOperator
    -> f (Maybe ComparisonOperator))
    -> ExpectedAttributeValue
    -> f ExpectedAttributeValue
eavComparisonOperator f x =
    (\y -> x { _eavComparisonOperator = y })
       <$> f (_eavComparisonOperator x)
{-# INLINE eavComparisonOperator #-}

eavAttributeValueList
    :: Functor f
    => ([AttributeValue]
    -> f ([AttributeValue]))
    -> ExpectedAttributeValue
    -> f ExpectedAttributeValue
eavAttributeValueList f x =
    (\y -> x { _eavAttributeValueList = y })
       <$> f (_eavAttributeValueList x)
{-# INLINE eavAttributeValueList #-}

instance ToJSON ExpectedAttributeValue

-- | Represents a global secondary index.
data GlobalSecondaryIndex = GlobalSecondaryIndex
    { _gsiIndexName :: Text
      -- ^ The name of the global secondary index. The name must be unique
      -- among all other indexes on this table.
    , _gsiKeySchema :: [KeySchemaElement]
      -- ^ The complete key schema for a global secondary index, which
      -- consists of one or more pairs of attribute names and key types
      -- (HASH or RANGE).
    , _gsiProjection :: Projection
      -- ^ Represents attributes that are copied (projected) from the table
      -- into an index. These are in addition to the primary key
      -- attributes and index key attributes, which are automatically
      -- projected.
    , _gsiProvisionedThroughput :: ProvisionedThroughput
      -- ^ Represents the provisioned throughput settings for a specified
      -- table or index. The settings can be modified using the
      -- UpdateTable operation. For current minimum and maximum
      -- provisioned throughput values, see Limits in the Amazon DynamoDB
      -- Developer Guide.
    } deriving (Show, Generic)

-- | The name of the global secondary index. The name must be unique among all
-- other indexes on this table.
gsiIndexName
    :: Functor f
    => (Text
    -> f (Text))
    -> GlobalSecondaryIndex
    -> f GlobalSecondaryIndex
gsiIndexName f x =
    (\y -> x { _gsiIndexName = y })
       <$> f (_gsiIndexName x)
{-# INLINE gsiIndexName #-}

-- | The complete key schema for a global secondary index, which consists of one
-- or more pairs of attribute names and key types (HASH or RANGE).
gsiKeySchema
    :: Functor f
    => ([KeySchemaElement]
    -> f ([KeySchemaElement]))
    -> GlobalSecondaryIndex
    -> f GlobalSecondaryIndex
gsiKeySchema f x =
    (\y -> x { _gsiKeySchema = y })
       <$> f (_gsiKeySchema x)
{-# INLINE gsiKeySchema #-}

-- | Represents attributes that are copied (projected) from the table into an
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected.
gsiProjection
    :: Functor f
    => (Projection
    -> f (Projection))
    -> GlobalSecondaryIndex
    -> f GlobalSecondaryIndex
gsiProjection f x =
    (\y -> x { _gsiProjection = y })
       <$> f (_gsiProjection x)
{-# INLINE gsiProjection #-}

-- | Represents the provisioned throughput settings for a specified table or
-- index. The settings can be modified using the UpdateTable operation. For
-- current minimum and maximum provisioned throughput values, see Limits in
-- the Amazon DynamoDB Developer Guide.
gsiProvisionedThroughput
    :: Functor f
    => (ProvisionedThroughput
    -> f (ProvisionedThroughput))
    -> GlobalSecondaryIndex
    -> f GlobalSecondaryIndex
gsiProvisionedThroughput f x =
    (\y -> x { _gsiProvisionedThroughput = y })
       <$> f (_gsiProvisionedThroughput x)
{-# INLINE gsiProvisionedThroughput #-}

instance ToJSON GlobalSecondaryIndex

-- | Represents the properties of a global secondary index.
data GlobalSecondaryIndexDescription = GlobalSecondaryIndexDescription
    { _gsidIndexName :: Maybe Text
      -- ^ The name of the global secondary index.
    , _gsidKeySchema :: Maybe [KeySchemaElement]
      -- ^ The complete key schema for the global secondary index,
      -- consisting of one or more pairs of attribute names and key types
      -- (HASH or RANGE).
    , _gsidProjection :: Maybe Projection
      -- ^ Represents attributes that are copied (projected) from the table
      -- into an index. These are in addition to the primary key
      -- attributes and index key attributes, which are automatically
      -- projected.
    , _gsidIndexStatus :: Maybe IndexStatus
      -- ^ The current state of the global secondary index: CREATING - The
      -- index is being created, as the result of a CreateTable or
      -- UpdateTable operation. UPDATING - The index is being updated, as
      -- the result of a CreateTable or UpdateTable operation. DELETING -
      -- The index is being deleted, as the result of a DeleteTable
      -- operation. ACTIVE - The index is ready for use.
    , _gsidProvisionedThroughput :: Maybe ProvisionedThroughputDescription
      -- ^ Represents the provisioned throughput settings for the table,
      -- consisting of read and write capacity units, along with data
      -- about increases and decreases.
    , _gsidIndexSizeBytes :: Maybe Integer
      -- ^ The total size of the specified index, in bytes. DynamoDB updates
      -- this value approximately every six hours. Recent changes might
      -- not be reflected in this value.
    , _gsidItemCount :: Maybe Integer
      -- ^ The number of items in the specified index. DynamoDB updates this
      -- value approximately every six hours. Recent changes might not be
      -- reflected in this value.
    } deriving (Show, Generic)

-- | The name of the global secondary index.
gsidIndexName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GlobalSecondaryIndexDescription
    -> f GlobalSecondaryIndexDescription
gsidIndexName f x =
    (\y -> x { _gsidIndexName = y })
       <$> f (_gsidIndexName x)
{-# INLINE gsidIndexName #-}

-- | The complete key schema for the global secondary index, consisting of one
-- or more pairs of attribute names and key types (HASH or RANGE).
gsidKeySchema
    :: Functor f
    => (Maybe [KeySchemaElement]
    -> f (Maybe [KeySchemaElement]))
    -> GlobalSecondaryIndexDescription
    -> f GlobalSecondaryIndexDescription
gsidKeySchema f x =
    (\y -> x { _gsidKeySchema = y })
       <$> f (_gsidKeySchema x)
{-# INLINE gsidKeySchema #-}

-- | Represents attributes that are copied (projected) from the table into an
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected.
gsidProjection
    :: Functor f
    => (Maybe Projection
    -> f (Maybe Projection))
    -> GlobalSecondaryIndexDescription
    -> f GlobalSecondaryIndexDescription
gsidProjection f x =
    (\y -> x { _gsidProjection = y })
       <$> f (_gsidProjection x)
{-# INLINE gsidProjection #-}

-- | The current state of the global secondary index: CREATING - The index is
-- being created, as the result of a CreateTable or UpdateTable operation.
-- UPDATING - The index is being updated, as the result of a CreateTable or
-- UpdateTable operation. DELETING - The index is being deleted, as the result
-- of a DeleteTable operation. ACTIVE - The index is ready for use.
gsidIndexStatus
    :: Functor f
    => (Maybe IndexStatus
    -> f (Maybe IndexStatus))
    -> GlobalSecondaryIndexDescription
    -> f GlobalSecondaryIndexDescription
gsidIndexStatus f x =
    (\y -> x { _gsidIndexStatus = y })
       <$> f (_gsidIndexStatus x)
{-# INLINE gsidIndexStatus #-}

-- | Represents the provisioned throughput settings for the table, consisting of
-- read and write capacity units, along with data about increases and
-- decreases.
gsidProvisionedThroughput
    :: Functor f
    => (Maybe ProvisionedThroughputDescription
    -> f (Maybe ProvisionedThroughputDescription))
    -> GlobalSecondaryIndexDescription
    -> f GlobalSecondaryIndexDescription
gsidProvisionedThroughput f x =
    (\y -> x { _gsidProvisionedThroughput = y })
       <$> f (_gsidProvisionedThroughput x)
{-# INLINE gsidProvisionedThroughput #-}

-- | The total size of the specified index, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be reflected
-- in this value.
gsidIndexSizeBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> GlobalSecondaryIndexDescription
    -> f GlobalSecondaryIndexDescription
gsidIndexSizeBytes f x =
    (\y -> x { _gsidIndexSizeBytes = y })
       <$> f (_gsidIndexSizeBytes x)
{-# INLINE gsidIndexSizeBytes #-}

-- | The number of items in the specified index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
gsidItemCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> GlobalSecondaryIndexDescription
    -> f GlobalSecondaryIndexDescription
gsidItemCount f x =
    (\y -> x { _gsidItemCount = y })
       <$> f (_gsidItemCount x)
{-# INLINE gsidItemCount #-}

instance FromJSON GlobalSecondaryIndexDescription

-- | Information about item collections, if any, that were affected by the
-- operation. ItemCollectionMetrics is only returned if it was asked for in
-- the request. If the table does not have any local secondary indexes, this
-- information is not returned in the response.
data ItemCollectionMetrics = ItemCollectionMetrics
    { _icmItemCollectionKey :: Map Text AttributeValue
      -- ^ The hash key value of the item collection. This is the same as
      -- the hash key of the item.
    , _icmSizeEstimateRangeGB :: [Double]
      -- ^ An estimate of item collection size, measured in gigabytes. This
      -- is a two-element array containing a lower bound and an upper
      -- bound for the estimate. The estimate includes the size of all the
      -- items in the table, plus the size of all attributes projected
      -- into all of the local secondary indexes on that table. Use this
      -- estimate to measure whether a local secondary index is
      -- approaching its size limit. The estimate is subject to change
      -- over time; therefore, do not rely on the precision or accuracy of
      -- the estimate.
    } deriving (Show, Generic)

-- | The hash key value of the item collection. This is the same as the hash key
-- of the item.
icmItemCollectionKey
    :: Functor f
    => (Map Text AttributeValue
    -> f (Map Text AttributeValue))
    -> ItemCollectionMetrics
    -> f ItemCollectionMetrics
icmItemCollectionKey f x =
    (\y -> x { _icmItemCollectionKey = y })
       <$> f (_icmItemCollectionKey x)
{-# INLINE icmItemCollectionKey #-}

-- | An estimate of item collection size, measured in gigabytes. This is a
-- two-element array containing a lower bound and an upper bound for the
-- estimate. The estimate includes the size of all the items in the table,
-- plus the size of all attributes projected into all of the local secondary
-- indexes on that table. Use this estimate to measure whether a local
-- secondary index is approaching its size limit. The estimate is subject to
-- change over time; therefore, do not rely on the precision or accuracy of
-- the estimate.
icmSizeEstimateRangeGB
    :: Functor f
    => ([Double]
    -> f ([Double]))
    -> ItemCollectionMetrics
    -> f ItemCollectionMetrics
icmSizeEstimateRangeGB f x =
    (\y -> x { _icmSizeEstimateRangeGB = y })
       <$> f (_icmSizeEstimateRangeGB x)
{-# INLINE icmSizeEstimateRangeGB #-}

instance FromJSON ItemCollectionMetrics

instance ToJSON ItemCollectionMetrics

-- | Represents a single element of a key schema. A key schema specifies the
-- attributes that make up the primary key of a table, or the key attributes
-- of an index.
data KeySchemaElement = KeySchemaElement
    { _kseAttributeName :: Text
      -- ^ The name of a key attribute.
    , _kseKeyType :: KeyType
      -- ^ The attribute data, consisting of the data type and the attribute
      -- value itself.
    } deriving (Show, Generic)

-- | The name of a key attribute.
kseAttributeName
    :: Functor f
    => (Text
    -> f (Text))
    -> KeySchemaElement
    -> f KeySchemaElement
kseAttributeName f x =
    (\y -> x { _kseAttributeName = y })
       <$> f (_kseAttributeName x)
{-# INLINE kseAttributeName #-}

-- | The attribute data, consisting of the data type and the attribute value
-- itself.
kseKeyType
    :: Functor f
    => (KeyType
    -> f (KeyType))
    -> KeySchemaElement
    -> f KeySchemaElement
kseKeyType f x =
    (\y -> x { _kseKeyType = y })
       <$> f (_kseKeyType x)
{-# INLINE kseKeyType #-}

instance FromJSON KeySchemaElement

instance ToJSON KeySchemaElement

-- | Represents a set of primary keys and, for each key, the attributes to
-- retrieve from the table.
data KeysAndAttributes = KeysAndAttributes
    { _kaaKeys :: [Map Text AttributeValue]
      -- ^ The primary key attribute values that define the items and the
      -- attributes associated with the items.
    , _kaaAttributesToGet :: Maybe [Text]
      -- ^ One or more attributes to retrieve from the table or index. If no
      -- attribute names are specified then all attributes will be
      -- returned. If any of the specified attributes are not found, they
      -- will not appear in the result.
    , _kaaConsistentRead :: Maybe Bool
      -- ^ The consistency of a read operation. If set to true, then a
      -- strongly consistent read is used; otherwise, an eventually
      -- consistent read is used.
    } deriving (Show, Generic)

-- | The primary key attribute values that define the items and the attributes
-- associated with the items.
kaaKeys
    :: Functor f
    => ([Map Text AttributeValue]
    -> f ([Map Text AttributeValue]))
    -> KeysAndAttributes
    -> f KeysAndAttributes
kaaKeys f x =
    (\y -> x { _kaaKeys = y })
       <$> f (_kaaKeys x)
{-# INLINE kaaKeys #-}

-- | One or more attributes to retrieve from the table or index. If no attribute
-- names are specified then all attributes will be returned. If any of the
-- specified attributes are not found, they will not appear in the result.
kaaAttributesToGet
    :: Functor f
    => (Maybe [Text]
    -> f (Maybe [Text]))
    -> KeysAndAttributes
    -> f KeysAndAttributes
kaaAttributesToGet f x =
    (\y -> x { _kaaAttributesToGet = y })
       <$> f (_kaaAttributesToGet x)
{-# INLINE kaaAttributesToGet #-}

-- | The consistency of a read operation. If set to true, then a strongly
-- consistent read is used; otherwise, an eventually consistent read is used.
kaaConsistentRead
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> KeysAndAttributes
    -> f KeysAndAttributes
kaaConsistentRead f x =
    (\y -> x { _kaaConsistentRead = y })
       <$> f (_kaaConsistentRead x)
{-# INLINE kaaConsistentRead #-}

instance FromJSON KeysAndAttributes

instance ToJSON KeysAndAttributes

-- | Represents a local secondary index.
data LocalSecondaryIndex = LocalSecondaryIndex
    { _lsiIndexName :: Text
      -- ^ The name of the local secondary index. The name must be unique
      -- among all other indexes on this table.
    , _lsiKeySchema :: [KeySchemaElement]
      -- ^ The complete key schema for the local secondary index, consisting
      -- of one or more pairs of attribute names and key types (HASH or
      -- RANGE).
    , _lsiProjection :: Projection
      -- ^ Represents attributes that are copied (projected) from the table
      -- into an index. These are in addition to the primary key
      -- attributes and index key attributes, which are automatically
      -- projected.
    } deriving (Show, Generic)

-- | The name of the local secondary index. The name must be unique among all
-- other indexes on this table.
lsiIndexName
    :: Functor f
    => (Text
    -> f (Text))
    -> LocalSecondaryIndex
    -> f LocalSecondaryIndex
lsiIndexName f x =
    (\y -> x { _lsiIndexName = y })
       <$> f (_lsiIndexName x)
{-# INLINE lsiIndexName #-}

-- | The complete key schema for the local secondary index, consisting of one or
-- more pairs of attribute names and key types (HASH or RANGE).
lsiKeySchema
    :: Functor f
    => ([KeySchemaElement]
    -> f ([KeySchemaElement]))
    -> LocalSecondaryIndex
    -> f LocalSecondaryIndex
lsiKeySchema f x =
    (\y -> x { _lsiKeySchema = y })
       <$> f (_lsiKeySchema x)
{-# INLINE lsiKeySchema #-}

-- | Represents attributes that are copied (projected) from the table into an
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected.
lsiProjection
    :: Functor f
    => (Projection
    -> f (Projection))
    -> LocalSecondaryIndex
    -> f LocalSecondaryIndex
lsiProjection f x =
    (\y -> x { _lsiProjection = y })
       <$> f (_lsiProjection x)
{-# INLINE lsiProjection #-}

instance ToJSON LocalSecondaryIndex

-- | Represents the properties of a local secondary index.
data LocalSecondaryIndexDescription = LocalSecondaryIndexDescription
    { _lsidIndexName :: Maybe Text
      -- ^ Represents the name of the local secondary index.
    , _lsidKeySchema :: Maybe [KeySchemaElement]
      -- ^ The complete index key schema, which consists of one or more
      -- pairs of attribute names and key types (HASH or RANGE).
    , _lsidProjection :: Maybe Projection
      -- ^ Represents attributes that are copied (projected) from the table
      -- into an index. These are in addition to the primary key
      -- attributes and index key attributes, which are automatically
      -- projected.
    , _lsidIndexSizeBytes :: Maybe Integer
      -- ^ The total size of the specified index, in bytes. DynamoDB updates
      -- this value approximately every six hours. Recent changes might
      -- not be reflected in this value.
    , _lsidItemCount :: Maybe Integer
      -- ^ The number of items in the specified index. DynamoDB updates this
      -- value approximately every six hours. Recent changes might not be
      -- reflected in this value.
    } deriving (Show, Generic)

-- | Represents the name of the local secondary index.
lsidIndexName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LocalSecondaryIndexDescription
    -> f LocalSecondaryIndexDescription
lsidIndexName f x =
    (\y -> x { _lsidIndexName = y })
       <$> f (_lsidIndexName x)
{-# INLINE lsidIndexName #-}

-- | The complete index key schema, which consists of one or more pairs of
-- attribute names and key types (HASH or RANGE).
lsidKeySchema
    :: Functor f
    => (Maybe [KeySchemaElement]
    -> f (Maybe [KeySchemaElement]))
    -> LocalSecondaryIndexDescription
    -> f LocalSecondaryIndexDescription
lsidKeySchema f x =
    (\y -> x { _lsidKeySchema = y })
       <$> f (_lsidKeySchema x)
{-# INLINE lsidKeySchema #-}

-- | Represents attributes that are copied (projected) from the table into an
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected.
lsidProjection
    :: Functor f
    => (Maybe Projection
    -> f (Maybe Projection))
    -> LocalSecondaryIndexDescription
    -> f LocalSecondaryIndexDescription
lsidProjection f x =
    (\y -> x { _lsidProjection = y })
       <$> f (_lsidProjection x)
{-# INLINE lsidProjection #-}

-- | The total size of the specified index, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be reflected
-- in this value.
lsidIndexSizeBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> LocalSecondaryIndexDescription
    -> f LocalSecondaryIndexDescription
lsidIndexSizeBytes f x =
    (\y -> x { _lsidIndexSizeBytes = y })
       <$> f (_lsidIndexSizeBytes x)
{-# INLINE lsidIndexSizeBytes #-}

-- | The number of items in the specified index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
lsidItemCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> LocalSecondaryIndexDescription
    -> f LocalSecondaryIndexDescription
lsidItemCount f x =
    (\y -> x { _lsidItemCount = y })
       <$> f (_lsidItemCount x)
{-# INLINE lsidItemCount #-}

instance FromJSON LocalSecondaryIndexDescription

-- | Represents attributes that are copied (projected) from the table into an
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected.
data Projection = Projection
    { _pProjectionType :: Maybe ProjectionType
      -- ^ The set of attributes that are projected into the index:
      -- KEYS_ONLY - Only the index and primary keys are projected into
      -- the index. INCLUDE - Only the specified table attributes are
      -- projected into the index. The list of projected attributes are in
      -- NonKeyAttributes. ALL - All of the table attributes are projected
      -- into the index.
    , _pNonKeyAttributes :: Maybe [Text]
      -- ^ Represents the non-key attribute names which will be projected
      -- into the index. For local secondary indexes, the total count of
      -- NonKeyAttributes summed across all of the local secondary
      -- indexes, must not exceed 20. If you project the same attribute
      -- into two different indexes, this counts as two distinct
      -- attributes when determining the total.
    } deriving (Show, Generic)

-- | The set of attributes that are projected into the index: KEYS_ONLY - Only
-- the index and primary keys are projected into the index. INCLUDE - Only the
-- specified table attributes are projected into the index. The list of
-- projected attributes are in NonKeyAttributes. ALL - All of the table
-- attributes are projected into the index.
pProjectionType
    :: Functor f
    => (Maybe ProjectionType
    -> f (Maybe ProjectionType))
    -> Projection
    -> f Projection
pProjectionType f x =
    (\y -> x { _pProjectionType = y })
       <$> f (_pProjectionType x)
{-# INLINE pProjectionType #-}

-- | Represents the non-key attribute names which will be projected into the
-- index. For local secondary indexes, the total count of NonKeyAttributes
-- summed across all of the local secondary indexes, must not exceed 20. If
-- you project the same attribute into two different indexes, this counts as
-- two distinct attributes when determining the total.
pNonKeyAttributes
    :: Functor f
    => (Maybe [Text]
    -> f (Maybe [Text]))
    -> Projection
    -> f Projection
pNonKeyAttributes f x =
    (\y -> x { _pNonKeyAttributes = y })
       <$> f (_pNonKeyAttributes x)
{-# INLINE pNonKeyAttributes #-}

instance FromJSON Projection

instance ToJSON Projection

-- | Represents the provisioned throughput settings for a specified table or
-- index. The settings can be modified using the UpdateTable operation. For
-- current minimum and maximum provisioned throughput values, see Limits in
-- the Amazon DynamoDB Developer Guide.
data ProvisionedThroughput = ProvisionedThroughput
    { _pvReadCapacityUnits :: Integer
      -- ^ The maximum number of strongly consistent reads consumed per
      -- second before DynamoDB returns a ThrottlingException. For more
      -- information, see Specifying Read and Write Requirements in the
      -- Amazon DynamoDB Developer Guide.
    , _pvWriteCapacityUnits :: Integer
      -- ^ The maximum number of writes consumed per second before DynamoDB
      -- returns a ThrottlingException. For more information, see
      -- Specifying Read and Write Requirements in the Amazon DynamoDB
      -- Developer Guide.
    } deriving (Show, Generic)

-- | The maximum number of strongly consistent reads consumed per second before
-- DynamoDB returns a ThrottlingException. For more information, see
-- Specifying Read and Write Requirements in the Amazon DynamoDB Developer
-- Guide.
pvReadCapacityUnits
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ProvisionedThroughput
    -> f ProvisionedThroughput
pvReadCapacityUnits f x =
    (\y -> x { _pvReadCapacityUnits = y })
       <$> f (_pvReadCapacityUnits x)
{-# INLINE pvReadCapacityUnits #-}

-- | The maximum number of writes consumed per second before DynamoDB returns a
-- ThrottlingException. For more information, see Specifying Read and Write
-- Requirements in the Amazon DynamoDB Developer Guide.
pvWriteCapacityUnits
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ProvisionedThroughput
    -> f ProvisionedThroughput
pvWriteCapacityUnits f x =
    (\y -> x { _pvWriteCapacityUnits = y })
       <$> f (_pvWriteCapacityUnits x)
{-# INLINE pvWriteCapacityUnits #-}

instance FromJSON ProvisionedThroughput

instance ToJSON ProvisionedThroughput

-- | The provisioned throughput settings for the table, consisting of read and
-- write capacity units, along with data about increases and decreases.
data ProvisionedThroughputDescription = ProvisionedThroughputDescription
    { _ptdLastIncreaseDateTime :: Maybe ISO8601
      -- ^ The date and time of the last provisioned throughput increase for
      -- this table.
    , _ptdLastDecreaseDateTime :: Maybe ISO8601
      -- ^ The date and time of the last provisioned throughput decrease for
      -- this table.
    , _ptdNumberOfDecreasesToday :: Maybe Integer
      -- ^ The number of provisioned throughput decreases for this table
      -- during this UTC calendar day. For current maximums on provisioned
      -- throughput decreases, see Limits in the Amazon DynamoDB Developer
      -- Guide.
    , _ptdReadCapacityUnits :: Maybe Integer
      -- ^ The maximum number of strongly consistent reads consumed per
      -- second before DynamoDB returns a ThrottlingException. Eventually
      -- consistent reads require less effort than strongly consistent
      -- reads, so a setting of 50 ReadCapacityUnits per second provides
      -- 100 eventually consistent ReadCapacityUnits per second.
    , _ptdWriteCapacityUnits :: Maybe Integer
      -- ^ The maximum number of writes consumed per second before DynamoDB
      -- returns a ThrottlingException.
    } deriving (Show, Generic)

-- | The date and time of the last provisioned throughput increase for this
-- table.
ptdLastIncreaseDateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ProvisionedThroughputDescription
    -> f ProvisionedThroughputDescription
ptdLastIncreaseDateTime f x =
    (\y -> x { _ptdLastIncreaseDateTime = y })
       <$> f (_ptdLastIncreaseDateTime x)
{-# INLINE ptdLastIncreaseDateTime #-}

-- | The date and time of the last provisioned throughput decrease for this
-- table.
ptdLastDecreaseDateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ProvisionedThroughputDescription
    -> f ProvisionedThroughputDescription
ptdLastDecreaseDateTime f x =
    (\y -> x { _ptdLastDecreaseDateTime = y })
       <$> f (_ptdLastDecreaseDateTime x)
{-# INLINE ptdLastDecreaseDateTime #-}

-- | The number of provisioned throughput decreases for this table during this
-- UTC calendar day. For current maximums on provisioned throughput decreases,
-- see Limits in the Amazon DynamoDB Developer Guide.
ptdNumberOfDecreasesToday
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ProvisionedThroughputDescription
    -> f ProvisionedThroughputDescription
ptdNumberOfDecreasesToday f x =
    (\y -> x { _ptdNumberOfDecreasesToday = y })
       <$> f (_ptdNumberOfDecreasesToday x)
{-# INLINE ptdNumberOfDecreasesToday #-}

-- | The maximum number of strongly consistent reads consumed per second before
-- DynamoDB returns a ThrottlingException. Eventually consistent reads require
-- less effort than strongly consistent reads, so a setting of 50
-- ReadCapacityUnits per second provides 100 eventually consistent
-- ReadCapacityUnits per second.
ptdReadCapacityUnits
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ProvisionedThroughputDescription
    -> f ProvisionedThroughputDescription
ptdReadCapacityUnits f x =
    (\y -> x { _ptdReadCapacityUnits = y })
       <$> f (_ptdReadCapacityUnits x)
{-# INLINE ptdReadCapacityUnits #-}

-- | The maximum number of writes consumed per second before DynamoDB returns a
-- ThrottlingException.
ptdWriteCapacityUnits
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ProvisionedThroughputDescription
    -> f ProvisionedThroughputDescription
ptdWriteCapacityUnits f x =
    (\y -> x { _ptdWriteCapacityUnits = y })
       <$> f (_ptdWriteCapacityUnits x)
{-# INLINE ptdWriteCapacityUnits #-}

instance FromJSON ProvisionedThroughputDescription

instance ToJSON ProvisionedThroughputDescription

-- | Represents the properties of a table.
data TableDescription = TableDescription
    { _tdAttributeDefinitions :: [AttributeDefinition]
      -- ^ An array of AttributeDefinition objects. Each of these objects
      -- describes one attribute in the table and index key schema. Each
      -- AttributeDefinition object in this array is composed of:
      -- AttributeName - The name of the attribute. AttributeType - The
      -- data type for the attribute.
    , _tdTableName :: Maybe Text
      -- ^ The name of the table.
    , _tdKeySchema :: Maybe [KeySchemaElement]
      -- ^ The primary key structure for the table. Each KeySchemaElement
      -- consists of: AttributeName - The name of the attribute. KeyType -
      -- The key type for the attribute. Can be either HASH or RANGE. For
      -- more information about primary keys, see Primary Key in the
      -- Amazon DynamoDB Developer Guide.
    , _tdTableStatus :: Maybe TableStatus
      -- ^ The current state of the table: CREATING - The table is being
      -- created, as the result of a CreateTable operation. UPDATING - The
      -- table is being updated, as the result of an UpdateTable
      -- operation. DELETING - The table is being deleted, as the result
      -- of a DeleteTable operation. ACTIVE - The table is ready for use.
    , _tdCreationDateTime :: Maybe ISO8601
      -- ^ The date and time when the table was created, in UNIX epoch time
      -- format.
    , _tdProvisionedThroughput :: Maybe ProvisionedThroughputDescription
      -- ^ The provisioned throughput settings for the table, consisting of
      -- read and write capacity units, along with data about increases
      -- and decreases.
    , _tdTableSizeBytes :: Maybe Integer
      -- ^ The total size of the specified table, in bytes. DynamoDB updates
      -- this value approximately every six hours. Recent changes might
      -- not be reflected in this value.
    , _tdItemCount :: Maybe Integer
      -- ^ The number of items in the specified table. DynamoDB updates this
      -- value approximately every six hours. Recent changes might not be
      -- reflected in this value.
    , _tdLocalSecondaryIndexes :: [LocalSecondaryIndexDescription]
      -- ^ Represents one or more local secondary indexes on the table. Each
      -- index is scoped to a given hash key value. Tables with one or
      -- more local secondary indexes are subject to an item collection
      -- size limit, where the amount of data within a given item
      -- collection cannot exceed 10 GB. Each element is composed of:
      -- IndexName - The name of the local secondary index. KeySchema -
      -- Specifies the complete index key schema. The attribute names in
      -- the key schema must be between 1 and 255 characters (inclusive).
      -- The key schema must begin with the same hash key attribute as the
      -- table. Projection - Specifies attributes that are copied
      -- (projected) from the table into the index. These are in addition
      -- to the primary key attributes and index key attributes, which are
      -- automatically projected. Each attribute specification is composed
      -- of: ProjectionType - One of the following: KEYS_ONLY - Only the
      -- index and primary keys are projected into the index. INCLUDE -
      -- Only the specified table attributes are projected into the index.
      -- The list of projected attributes are in NonKeyAttributes. ALL -
      -- All of the table attributes are projected into the index.
      -- NonKeyAttributes - A list of one or more non-key attribute names
      -- that are projected into the secondary index. The total count of
      -- attributes specified in NonKeyAttributes, summed across all of
      -- the secondary indexes, must not exceed 20. If you project the
      -- same attribute into two different indexes, this counts as two
      -- distinct attributes when determining the total. IndexSizeBytes -
      -- Represents the total size of the index, in bytes. DynamoDB
      -- updates this value approximately every six hours. Recent changes
      -- might not be reflected in this value. ItemCount - Represents the
      -- number of items in the index. DynamoDB updates this value
      -- approximately every six hours. Recent changes might not be
      -- reflected in this value. If the table is in the DELETING state,
      -- no information about indexes will be returned.
    , _tdGlobalSecondaryIndexes :: [GlobalSecondaryIndexDescription]
      -- ^ The global secondary indexes, if any, on the table. Each index is
      -- scoped to a given hash key value. Each element is composed of:
      -- IndexName - The name of the global secondary index.
      -- IndexSizeBytes - The total size of the global secondary index, in
      -- bytes. DynamoDB updates this value approximately every six hours.
      -- Recent changes might not be reflected in this value. IndexStatus
      -- - The current status of the global secondary index: CREATING -
      -- The index is being created. UPDATING - The index is being
      -- updated. DELETING - The index is being deleted. ACTIVE - The
      -- index is ready for use. ItemCount - The number of items in the
      -- global secondary index. DynamoDB updates this value approximately
      -- every six hours. Recent changes might not be reflected in this
      -- value. KeySchema - Specifies the complete index key schema. The
      -- attribute names in the key schema must be between 1 and 255
      -- characters (inclusive). The key schema must begin with the same
      -- hash key attribute as the table. Projection - Specifies
      -- attributes that are copied (projected) from the table into the
      -- index. These are in addition to the primary key attributes and
      -- index key attributes, which are automatically projected. Each
      -- attribute specification is composed of: ProjectionType - One of
      -- the following: KEYS_ONLY - Only the index and primary keys are
      -- projected into the index. INCLUDE - Only the specified table
      -- attributes are projected into the index. The list of projected
      -- attributes are in NonKeyAttributes. ALL - All of the table
      -- attributes are projected into the index. NonKeyAttributes - A
      -- list of one or more non-key attribute names that are projected
      -- into the secondary index. The total count of attributes specified
      -- in NonKeyAttributes, summed across all of the secondary indexes,
      -- must not exceed 20. If you project the same attribute into two
      -- different indexes, this counts as two distinct attributes when
      -- determining the total. ProvisionedThroughput - The provisioned
      -- throughput settings for the global secondary index, consisting of
      -- read and write capacity units, along with data about increases
      -- and decreases. If the table is in the DELETING state, no
      -- information about indexes will be returned.
    } deriving (Show, Generic)

-- | An array of AttributeDefinition objects. Each of these objects describes
-- one attribute in the table and index key schema. Each AttributeDefinition
-- object in this array is composed of: AttributeName - The name of the
-- attribute. AttributeType - The data type for the attribute.
tdAttributeDefinitions
    :: Functor f
    => ([AttributeDefinition]
    -> f ([AttributeDefinition]))
    -> TableDescription
    -> f TableDescription
tdAttributeDefinitions f x =
    (\y -> x { _tdAttributeDefinitions = y })
       <$> f (_tdAttributeDefinitions x)
{-# INLINE tdAttributeDefinitions #-}

-- | The name of the table.
tdTableName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TableDescription
    -> f TableDescription
tdTableName f x =
    (\y -> x { _tdTableName = y })
       <$> f (_tdTableName x)
{-# INLINE tdTableName #-}

-- | The primary key structure for the table. Each KeySchemaElement consists of:
-- AttributeName - The name of the attribute. KeyType - The key type for the
-- attribute. Can be either HASH or RANGE. For more information about primary
-- keys, see Primary Key in the Amazon DynamoDB Developer Guide.
tdKeySchema
    :: Functor f
    => (Maybe [KeySchemaElement]
    -> f (Maybe [KeySchemaElement]))
    -> TableDescription
    -> f TableDescription
tdKeySchema f x =
    (\y -> x { _tdKeySchema = y })
       <$> f (_tdKeySchema x)
{-# INLINE tdKeySchema #-}

-- | The current state of the table: CREATING - The table is being created, as
-- the result of a CreateTable operation. UPDATING - The table is being
-- updated, as the result of an UpdateTable operation. DELETING - The table is
-- being deleted, as the result of a DeleteTable operation. ACTIVE - The table
-- is ready for use.
tdTableStatus
    :: Functor f
    => (Maybe TableStatus
    -> f (Maybe TableStatus))
    -> TableDescription
    -> f TableDescription
tdTableStatus f x =
    (\y -> x { _tdTableStatus = y })
       <$> f (_tdTableStatus x)
{-# INLINE tdTableStatus #-}

-- | The date and time when the table was created, in UNIX epoch time format.
tdCreationDateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> TableDescription
    -> f TableDescription
tdCreationDateTime f x =
    (\y -> x { _tdCreationDateTime = y })
       <$> f (_tdCreationDateTime x)
{-# INLINE tdCreationDateTime #-}

-- | The provisioned throughput settings for the table, consisting of read and
-- write capacity units, along with data about increases and decreases.
tdProvisionedThroughput
    :: Functor f
    => (Maybe ProvisionedThroughputDescription
    -> f (Maybe ProvisionedThroughputDescription))
    -> TableDescription
    -> f TableDescription
tdProvisionedThroughput f x =
    (\y -> x { _tdProvisionedThroughput = y })
       <$> f (_tdProvisionedThroughput x)
{-# INLINE tdProvisionedThroughput #-}

-- | The total size of the specified table, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be reflected
-- in this value.
tdTableSizeBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> TableDescription
    -> f TableDescription
tdTableSizeBytes f x =
    (\y -> x { _tdTableSizeBytes = y })
       <$> f (_tdTableSizeBytes x)
{-# INLINE tdTableSizeBytes #-}

-- | The number of items in the specified table. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
tdItemCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> TableDescription
    -> f TableDescription
tdItemCount f x =
    (\y -> x { _tdItemCount = y })
       <$> f (_tdItemCount x)
{-# INLINE tdItemCount #-}

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
tdLocalSecondaryIndexes
    :: Functor f
    => ([LocalSecondaryIndexDescription]
    -> f ([LocalSecondaryIndexDescription]))
    -> TableDescription
    -> f TableDescription
tdLocalSecondaryIndexes f x =
    (\y -> x { _tdLocalSecondaryIndexes = y })
       <$> f (_tdLocalSecondaryIndexes x)
{-# INLINE tdLocalSecondaryIndexes #-}

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
tdGlobalSecondaryIndexes
    :: Functor f
    => ([GlobalSecondaryIndexDescription]
    -> f ([GlobalSecondaryIndexDescription]))
    -> TableDescription
    -> f TableDescription
tdGlobalSecondaryIndexes f x =
    (\y -> x { _tdGlobalSecondaryIndexes = y })
       <$> f (_tdGlobalSecondaryIndexes x)
{-# INLINE tdGlobalSecondaryIndexes #-}

instance FromJSON TableDescription

-- | The name of a global secondary index, along with the updated provisioned
-- throughput settings that are to be applied to that index.
data UpdateGlobalSecondaryIndexAction = UpdateGlobalSecondaryIndexAction
    { _ugsiaIndexName :: Text
      -- ^ The name of the global secondary index to be updated.
    , _ugsiaProvisionedThroughput :: ProvisionedThroughput
      -- ^ Represents the provisioned throughput settings for a specified
      -- table or index. The settings can be modified using the
      -- UpdateTable operation. For current minimum and maximum
      -- provisioned throughput values, see Limits in the Amazon DynamoDB
      -- Developer Guide.
    } deriving (Show, Generic)

-- | The name of the global secondary index to be updated.
ugsiaIndexName
    :: Functor f
    => (Text
    -> f (Text))
    -> UpdateGlobalSecondaryIndexAction
    -> f UpdateGlobalSecondaryIndexAction
ugsiaIndexName f x =
    (\y -> x { _ugsiaIndexName = y })
       <$> f (_ugsiaIndexName x)
{-# INLINE ugsiaIndexName #-}

-- | Represents the provisioned throughput settings for a specified table or
-- index. The settings can be modified using the UpdateTable operation. For
-- current minimum and maximum provisioned throughput values, see Limits in
-- the Amazon DynamoDB Developer Guide.
ugsiaProvisionedThroughput
    :: Functor f
    => (ProvisionedThroughput
    -> f (ProvisionedThroughput))
    -> UpdateGlobalSecondaryIndexAction
    -> f UpdateGlobalSecondaryIndexAction
ugsiaProvisionedThroughput f x =
    (\y -> x { _ugsiaProvisionedThroughput = y })
       <$> f (_ugsiaProvisionedThroughput x)
{-# INLINE ugsiaProvisionedThroughput #-}

instance FromJSON UpdateGlobalSecondaryIndexAction

instance ToJSON UpdateGlobalSecondaryIndexAction

-- | Represents an operation to perform - either DeleteItem or PutItem. You can
-- only specify one of these operations, not both, in a single WriteRequest.
-- If you do need to perform both of these operations, you will need to
-- specify two separate WriteRequest objects.
data WriteRequest = WriteRequest
    { _wsPutRequest :: Maybe PutRequest
      -- ^ A request to perform a PutItem operation.
    , _wsDeleteRequest :: Maybe DeleteRequest
      -- ^ A request to perform a DeleteItem operation.
    } deriving (Show, Generic)

-- | A request to perform a PutItem operation.
wsPutRequest
    :: Functor f
    => (Maybe PutRequest
    -> f (Maybe PutRequest))
    -> WriteRequest
    -> f WriteRequest
wsPutRequest f x =
    (\y -> x { _wsPutRequest = y })
       <$> f (_wsPutRequest x)
{-# INLINE wsPutRequest #-}

-- | A request to perform a DeleteItem operation.
wsDeleteRequest
    :: Functor f
    => (Maybe DeleteRequest
    -> f (Maybe DeleteRequest))
    -> WriteRequest
    -> f WriteRequest
wsDeleteRequest f x =
    (\y -> x { _wsDeleteRequest = y })
       <$> f (_wsDeleteRequest x)
{-# INLINE wsDeleteRequest #-}

instance FromJSON WriteRequest

instance ToJSON WriteRequest
