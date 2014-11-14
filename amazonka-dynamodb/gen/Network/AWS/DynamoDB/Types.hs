{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.DynamoDB.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.DynamoDB.Types
    (
    -- * Service
      DynamoDB
    -- ** Error
    , JSONError

    -- * WriteRequest
    , WriteRequest
    , writeRequest
    , wDeleteRequest
    , wPutRequest

    -- * ProvisionedThroughputDescription
    , ProvisionedThroughputDescription
    , provisionedThroughputDescription
    , ptdLastDecreaseDateTime
    , ptdLastIncreaseDateTime
    , ptdNumberOfDecreasesToday
    , ptdReadCapacityUnits
    , ptdWriteCapacityUnits

    -- * KeyType
    , KeyType (..)

    -- * AttributeValue
    , AttributeValue
    , attributeValue
    , avB
    , avBOOL
    , avBS
    , avL
    , avM
    , avN
    , avNS
    , avNULL
    , avS
    , avSS

    -- * IndexStatus
    , IndexStatus (..)

    -- * ProvisionedThroughput
    , ProvisionedThroughput
    , provisionedThroughput
    , ptReadCapacityUnits
    , ptWriteCapacityUnits

    -- * TableStatus
    , TableStatus (..)

    -- * ProjectionType
    , ProjectionType (..)

    -- * TableDescription
    , TableDescription
    , tableDescription
    , tdAttributeDefinitions
    , tdCreationDateTime
    , tdGlobalSecondaryIndexes
    , tdItemCount
    , tdKeySchema
    , tdLocalSecondaryIndexes
    , tdProvisionedThroughput
    , tdTableName
    , tdTableSizeBytes
    , tdTableStatus

    -- * KeysAndAttributes
    , KeysAndAttributes
    , keysAndAttributes
    , kaaAttributesToGet
    , kaaConsistentRead
    , kaaExpressionAttributeNames
    , kaaKeys
    , kaaProjectionExpression

    -- * ReturnConsumedCapacity
    , ReturnConsumedCapacity (..)

    -- * ReturnItemCollectionMetrics
    , ReturnItemCollectionMetrics (..)

    -- * AttributeValueUpdate
    , AttributeValueUpdate
    , attributeValueUpdate
    , avuAction
    , avuValue

    -- * ExpectedAttributeValue
    , ExpectedAttributeValue
    , expectedAttributeValue
    , eavAttributeValueList
    , eavComparisonOperator
    , eavExists
    , eavValue

    -- * AttributeDefinition
    , AttributeDefinition
    , attributeDefinition
    , adAttributeName
    , adAttributeType

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * ReturnValue
    , ReturnValue (..)

    -- * LocalSecondaryIndex
    , LocalSecondaryIndex
    , localSecondaryIndex
    , lsiIndexName
    , lsiKeySchema
    , lsiProjection

    -- * GlobalSecondaryIndexDescription
    , GlobalSecondaryIndexDescription
    , globalSecondaryIndexDescription
    , gsidIndexName
    , gsidIndexSizeBytes
    , gsidIndexStatus
    , gsidItemCount
    , gsidKeySchema
    , gsidProjection
    , gsidProvisionedThroughput

    -- * ItemCollectionMetrics
    , ItemCollectionMetrics
    , itemCollectionMetrics
    , icmItemCollectionKey
    , icmSizeEstimateRangeGB

    -- * Capacity
    , Capacity
    , capacity
    , cCapacityUnits

    -- * ConsumedCapacity
    , ConsumedCapacity
    , consumedCapacity
    , ccCapacityUnits
    , ccGlobalSecondaryIndexes
    , ccLocalSecondaryIndexes
    , ccTable
    , ccTableName

    -- * GlobalSecondaryIndex
    , GlobalSecondaryIndex
    , globalSecondaryIndex
    , gsiIndexName
    , gsiKeySchema
    , gsiProjection
    , gsiProvisionedThroughput

    -- * LocalSecondaryIndexDescription
    , LocalSecondaryIndexDescription
    , localSecondaryIndexDescription
    , lsidIndexName
    , lsidIndexSizeBytes
    , lsidItemCount
    , lsidKeySchema
    , lsidProjection

    -- * AttributeAction
    , AttributeAction (..)

    -- * ScalarAttributeType
    , ScalarAttributeType (..)

    -- * Projection
    , Projection
    , projection
    , pNonKeyAttributes
    , pProjectionType

    -- * Select
    , Select (..)

    -- * KeySchemaElement
    , KeySchemaElement
    , keySchemaElement
    , kseAttributeName
    , kseKeyType

    -- * DeleteRequest
    , DeleteRequest
    , deleteRequest
    , dKey

    -- * UpdateGlobalSecondaryIndexAction
    , UpdateGlobalSecondaryIndexAction
    , updateGlobalSecondaryIndexAction
    , ugsiaIndexName
    , ugsiaProvisionedThroughput

    -- * PutRequest
    , PutRequest
    , putRequest
    , pItem

    -- * Condition
    , Condition
    , condition
    , cAttributeValueList
    , cComparisonOperator

    -- * ConditionalOperator
    , ConditionalOperator (..)

    -- * GlobalSecondaryIndexUpdate
    , GlobalSecondaryIndexUpdate
    , globalSecondaryIndexUpdate
    , gsiuUpdate
    ) where

import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4
import qualified GHC.Exts

-- | Supported version (@2012-08-10@) of the Amazon DynamoDB.
data DynamoDB deriving (Typeable)

instance AWSService DynamoDB where
    type Sg DynamoDB = V4
    type Er DynamoDB = JSONError

    service = Service
        { _svcEndpoint = regional
        , _svcAbbrev   = "DynamoDB"
        , _svcPrefix   = "dynamodb"
        , _svcVersion  = "2012-08-10"
        , _svcTarget   = Nothing
        }

    handle = jsonError alwaysFail

data WriteRequest = WriteRequest
    { _wDeleteRequest :: Maybe DeleteRequest
    , _wPutRequest    :: Maybe PutRequest
    } deriving (Eq, Show, Generic)

-- | 'WriteRequest' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wDeleteRequest' @::@ 'Maybe' 'DeleteRequest'
--
-- * 'wPutRequest' @::@ 'Maybe' 'PutRequest'
--
writeRequest :: WriteRequest
writeRequest = WriteRequest
    { _wPutRequest    = Nothing
    , _wDeleteRequest = Nothing
    }

-- | A request to perform a DeleteItem operation.
wDeleteRequest :: Lens' WriteRequest (Maybe DeleteRequest)
wDeleteRequest = lens _wDeleteRequest (\s a -> s { _wDeleteRequest = a })

-- | A request to perform a PutItem operation.
wPutRequest :: Lens' WriteRequest (Maybe PutRequest)
wPutRequest = lens _wPutRequest (\s a -> s { _wPutRequest = a })

instance FromJSON WriteRequest

instance ToJSON WriteRequest

data ProvisionedThroughputDescription = ProvisionedThroughputDescription
    { _ptdLastDecreaseDateTime   :: Maybe RFC822
    , _ptdLastIncreaseDateTime   :: Maybe RFC822
    , _ptdNumberOfDecreasesToday :: Maybe Natural
    , _ptdReadCapacityUnits      :: Maybe Natural
    , _ptdWriteCapacityUnits     :: Maybe Natural
    } deriving (Eq, Ord, Show, Generic)

-- | 'ProvisionedThroughputDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptdLastDecreaseDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'ptdLastIncreaseDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'ptdNumberOfDecreasesToday' @::@ 'Maybe' 'Natural'
--
-- * 'ptdReadCapacityUnits' @::@ 'Maybe' 'Natural'
--
-- * 'ptdWriteCapacityUnits' @::@ 'Maybe' 'Natural'
--
provisionedThroughputDescription :: ProvisionedThroughputDescription
provisionedThroughputDescription = ProvisionedThroughputDescription
    { _ptdLastIncreaseDateTime   = Nothing
    , _ptdLastDecreaseDateTime   = Nothing
    , _ptdNumberOfDecreasesToday = Nothing
    , _ptdReadCapacityUnits      = Nothing
    , _ptdWriteCapacityUnits     = Nothing
    }

-- | The date and time of the last provisioned throughput decrease for this
-- table.
ptdLastDecreaseDateTime :: Lens' ProvisionedThroughputDescription (Maybe UTCTime)
ptdLastDecreaseDateTime =
    lens _ptdLastDecreaseDateTime (\s a -> s { _ptdLastDecreaseDateTime = a })
        . mapping _Time

-- | The date and time of the last provisioned throughput increase for this
-- table.
ptdLastIncreaseDateTime :: Lens' ProvisionedThroughputDescription (Maybe UTCTime)
ptdLastIncreaseDateTime =
    lens _ptdLastIncreaseDateTime (\s a -> s { _ptdLastIncreaseDateTime = a })
        . mapping _Time

-- | The number of provisioned throughput decreases for this table during this
-- UTC calendar day. For current maximums on provisioned throughput
-- decreases, see Limits in the Amazon DynamoDB Developer Guide.
ptdNumberOfDecreasesToday :: Lens' ProvisionedThroughputDescription (Maybe Natural)
ptdNumberOfDecreasesToday =
    lens _ptdNumberOfDecreasesToday
        (\s a -> s { _ptdNumberOfDecreasesToday = a })

-- | The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a ThrottlingException. Eventually consistent
-- reads require less effort than strongly consistent reads, so a setting of
-- 50 ReadCapacityUnits per second provides 100 eventually consistent
-- ReadCapacityUnits per second.
ptdReadCapacityUnits :: Lens' ProvisionedThroughputDescription (Maybe Natural)
ptdReadCapacityUnits =
    lens _ptdReadCapacityUnits (\s a -> s { _ptdReadCapacityUnits = a })

-- | The maximum number of writes consumed per second before DynamoDB returns
-- a ThrottlingException.
ptdWriteCapacityUnits :: Lens' ProvisionedThroughputDescription (Maybe Natural)
ptdWriteCapacityUnits =
    lens _ptdWriteCapacityUnits (\s a -> s { _ptdWriteCapacityUnits = a })

instance FromJSON ProvisionedThroughputDescription

instance ToJSON ProvisionedThroughputDescription

data KeyType
    = Hash  -- ^ HASH
    | Range -- ^ RANGE
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable KeyType

instance FromText KeyType where
    parser = match "HASH"  Hash
         <|> match "RANGE" Range

instance ToText KeyType where
    toText = \case
        Hash  -> "HASH"
        Range -> "RANGE"

instance FromJSON KeyType

instance ToJSON KeyType

data AttributeValue = AttributeValue
    { _avB    :: Maybe Base64
    , _avBOOL :: Maybe Bool
    , _avBS   :: [Base64]
    , _avL    :: [AttributeValue]
    , _avM    :: Map Text AttributeValue
    , _avN    :: Maybe Text
    , _avNS   :: [Text]
    , _avNULL :: Maybe Bool
    , _avS    :: Maybe Text
    , _avSS   :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'AttributeValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avB' @::@ 'Maybe' 'Base64'
--
-- * 'avBOOL' @::@ 'Maybe' 'Bool'
--
-- * 'avBS' @::@ ['Base64']
--
-- * 'avL' @::@ ['AttributeValue']
--
-- * 'avM' @::@ 'HashMap' 'Text' 'AttributeValue'
--
-- * 'avN' @::@ 'Maybe' 'Text'
--
-- * 'avNS' @::@ ['Text']
--
-- * 'avNULL' @::@ 'Maybe' 'Bool'
--
-- * 'avS' @::@ 'Maybe' 'Text'
--
-- * 'avSS' @::@ ['Text']
--
attributeValue :: AttributeValue
attributeValue = AttributeValue
    { _avS    = Nothing
    , _avN    = Nothing
    , _avB    = Nothing
    , _avSS   = mempty
    , _avNS   = mempty
    , _avBS   = mempty
    , _avM    = mempty
    , _avL    = mempty
    , _avNULL = Nothing
    , _avBOOL = Nothing
    }

-- | A Binary data type.
avB :: Lens' AttributeValue (Maybe Base64)
avB = lens _avB (\s a -> s { _avB = a })

-- | A Boolean data type.
avBOOL :: Lens' AttributeValue (Maybe Bool)
avBOOL = lens _avBOOL (\s a -> s { _avBOOL = a })

-- | A Binary Set data type.
avBS :: Lens' AttributeValue [Base64]
avBS = lens _avBS (\s a -> s { _avBS = a })

-- | A List of attribute values.
avL :: Lens' AttributeValue [AttributeValue]
avL = lens _avL (\s a -> s { _avL = a })

-- | A Map of attribute values.
avM :: Lens' AttributeValue (HashMap Text AttributeValue)
avM = lens _avM (\s a -> s { _avM = a })
    . _Map

-- | A Number data type.
avN :: Lens' AttributeValue (Maybe Text)
avN = lens _avN (\s a -> s { _avN = a })

-- | A Number Set data type.
avNS :: Lens' AttributeValue [Text]
avNS = lens _avNS (\s a -> s { _avNS = a })

-- | A Null data type.
avNULL :: Lens' AttributeValue (Maybe Bool)
avNULL = lens _avNULL (\s a -> s { _avNULL = a })

-- | A String data type.
avS :: Lens' AttributeValue (Maybe Text)
avS = lens _avS (\s a -> s { _avS = a })

-- | A String Set data type.
avSS :: Lens' AttributeValue [Text]
avSS = lens _avSS (\s a -> s { _avSS = a })

instance FromJSON AttributeValue

instance ToJSON AttributeValue

data IndexStatus
    = Active   -- ^ ACTIVE
    | Creating -- ^ CREATING
    | Deleting -- ^ DELETING
    | Updating -- ^ UPDATING
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable IndexStatus

instance FromText IndexStatus where
    parser = match "ACTIVE"   Active
         <|> match "CREATING" Creating
         <|> match "DELETING" Deleting
         <|> match "UPDATING" Updating

instance ToText IndexStatus where
    toText = \case
        Active   -> "ACTIVE"
        Creating -> "CREATING"
        Deleting -> "DELETING"
        Updating -> "UPDATING"

instance FromJSON IndexStatus

instance ToJSON IndexStatus

data ProvisionedThroughput = ProvisionedThroughput
    { _ptReadCapacityUnits  :: Natural
    , _ptWriteCapacityUnits :: Natural
    } deriving (Eq, Ord, Show, Generic)

-- | 'ProvisionedThroughput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptReadCapacityUnits' @::@ 'Natural'
--
-- * 'ptWriteCapacityUnits' @::@ 'Natural'
--
provisionedThroughput :: Natural -- ^ 'ptReadCapacityUnits'
                      -> Natural -- ^ 'ptWriteCapacityUnits'
                      -> ProvisionedThroughput
provisionedThroughput p1 p2 = ProvisionedThroughput
    { _ptReadCapacityUnits  = p1
    , _ptWriteCapacityUnits = p2
    }

-- | The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a ThrottlingException. For more information, see
-- Specifying Read and Write Requirements in the Amazon DynamoDB Developer
-- Guide.
ptReadCapacityUnits :: Lens' ProvisionedThroughput Natural
ptReadCapacityUnits =
    lens _ptReadCapacityUnits (\s a -> s { _ptReadCapacityUnits = a })

-- | The maximum number of writes consumed per second before DynamoDB returns
-- a ThrottlingException. For more information, see Specifying Read and
-- Write Requirements in the Amazon DynamoDB Developer Guide.
ptWriteCapacityUnits :: Lens' ProvisionedThroughput Natural
ptWriteCapacityUnits =
    lens _ptWriteCapacityUnits (\s a -> s { _ptWriteCapacityUnits = a })

instance FromJSON ProvisionedThroughput

instance ToJSON ProvisionedThroughput

data TableStatus
    = TSActive   -- ^ ACTIVE
    | TSCreating -- ^ CREATING
    | TSDeleting -- ^ DELETING
    | TSUpdating -- ^ UPDATING
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable TableStatus

instance FromText TableStatus where
    parser = match "ACTIVE"   TSActive
         <|> match "CREATING" TSCreating
         <|> match "DELETING" TSDeleting
         <|> match "UPDATING" TSUpdating

instance ToText TableStatus where
    toText = \case
        TSActive   -> "ACTIVE"
        TSCreating -> "CREATING"
        TSDeleting -> "DELETING"
        TSUpdating -> "UPDATING"

instance FromJSON TableStatus

instance ToJSON TableStatus

data ProjectionType
    = All      -- ^ ALL
    | Include  -- ^ INCLUDE
    | KeysOnly -- ^ KEYS_ONLY
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ProjectionType

instance FromText ProjectionType where
    parser = match "ALL"       All
         <|> match "INCLUDE"   Include
         <|> match "KEYS_ONLY" KeysOnly

instance ToText ProjectionType where
    toText = \case
        All      -> "ALL"
        Include  -> "INCLUDE"
        KeysOnly -> "KEYS_ONLY"

instance FromJSON ProjectionType

instance ToJSON ProjectionType

data TableDescription = TableDescription
    { _tdAttributeDefinitions   :: [AttributeDefinition]
    , _tdCreationDateTime       :: Maybe RFC822
    , _tdGlobalSecondaryIndexes :: [GlobalSecondaryIndexDescription]
    , _tdItemCount              :: Maybe Integer
    , _tdKeySchema              :: List1 KeySchemaElement
    , _tdLocalSecondaryIndexes  :: [LocalSecondaryIndexDescription]
    , _tdProvisionedThroughput  :: Maybe ProvisionedThroughputDescription
    , _tdTableName              :: Maybe Text
    , _tdTableSizeBytes         :: Maybe Integer
    , _tdTableStatus            :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'TableDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tdAttributeDefinitions' @::@ ['AttributeDefinition']
--
-- * 'tdCreationDateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'tdGlobalSecondaryIndexes' @::@ ['GlobalSecondaryIndexDescription']
--
-- * 'tdItemCount' @::@ 'Maybe' 'Integer'
--
-- * 'tdKeySchema' @::@ 'NonEmpty' 'KeySchemaElement'
--
-- * 'tdLocalSecondaryIndexes' @::@ ['LocalSecondaryIndexDescription']
--
-- * 'tdProvisionedThroughput' @::@ 'Maybe' 'ProvisionedThroughputDescription'
--
-- * 'tdTableName' @::@ 'Maybe' 'Text'
--
-- * 'tdTableSizeBytes' @::@ 'Maybe' 'Integer'
--
-- * 'tdTableStatus' @::@ 'Maybe' 'Text'
--
tableDescription :: NonEmpty KeySchemaElement -- ^ 'tdKeySchema'
                 -> TableDescription
tableDescription p1 = TableDescription
    { _tdKeySchema              = withIso _List1 (const id) p1
    , _tdAttributeDefinitions   = mempty
    , _tdTableName              = Nothing
    , _tdTableStatus            = Nothing
    , _tdCreationDateTime       = Nothing
    , _tdProvisionedThroughput  = Nothing
    , _tdTableSizeBytes         = Nothing
    , _tdItemCount              = Nothing
    , _tdLocalSecondaryIndexes  = mempty
    , _tdGlobalSecondaryIndexes = mempty
    }

-- | An array of AttributeDefinition objects. Each of these objects describes
-- one attribute in the table and index key schema. Each AttributeDefinition
-- object in this array is composed of: AttributeName - The name of the
-- attribute. AttributeType - The data type for the attribute.
tdAttributeDefinitions :: Lens' TableDescription [AttributeDefinition]
tdAttributeDefinitions =
    lens _tdAttributeDefinitions (\s a -> s { _tdAttributeDefinitions = a })

-- | The date and time when the table was created, in UNIX epoch time format.
tdCreationDateTime :: Lens' TableDescription (Maybe UTCTime)
tdCreationDateTime =
    lens _tdCreationDateTime (\s a -> s { _tdCreationDateTime = a })
        . mapping _Time

-- | The global secondary indexes, if any, on the table. Each index is scoped
-- to a given hash key value. Each element is composed of: IndexName - The
-- name of the global secondary index. IndexSizeBytes - The total size of
-- the global secondary index, in bytes. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value. IndexStatus - The current status of the global secondary
-- index: CREATING - The index is being created. UPDATING - The index is
-- being updated. DELETING - The index is being deleted. ACTIVE - The index
-- is ready for use. ItemCount - The number of items in the global secondary
-- index. DynamoDB updates this value approximately every six hours. Recent
-- changes might not be reflected in this value. KeySchema - Specifies the
-- complete index key schema. The attribute names in the key schema must be
-- between 1 and 255 characters (inclusive). The key schema must begin with
-- the same hash key attribute as the table. Projection - Specifies
-- attributes that are copied (projected) from the table into the index.
-- These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected. Each attribute
-- specification is composed of: ProjectionType - One of the following:
-- KEYS_ONLY - Only the index and primary keys are projected into the index.
-- INCLUDE - Only the specified table attributes are projected into the
-- index. The list of projected attributes are in NonKeyAttributes. ALL -
-- All of the table attributes are projected into the index.
-- NonKeyAttributes - A list of one or more non-key attribute names that are
-- projected into the secondary index. The total count of attributes
-- specified in NonKeyAttributes, summed across all of the secondary
-- indexes, must not exceed 20. If you project the same attribute into two
-- different indexes, this counts as two distinct attributes when
-- determining the total. ProvisionedThroughput - The provisioned throughput
-- settings for the global secondary index, consisting of read and write
-- capacity units, along with data about increases and decreases. If the
-- table is in the DELETING state, no information about indexes will be
-- returned.
tdGlobalSecondaryIndexes :: Lens' TableDescription [GlobalSecondaryIndexDescription]
tdGlobalSecondaryIndexes =
    lens _tdGlobalSecondaryIndexes
        (\s a -> s { _tdGlobalSecondaryIndexes = a })

-- | The number of items in the specified table. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
tdItemCount :: Lens' TableDescription (Maybe Integer)
tdItemCount = lens _tdItemCount (\s a -> s { _tdItemCount = a })

-- | The primary key structure for the table. Each KeySchemaElement consists
-- of: AttributeName - The name of the attribute. KeyType - The key type for
-- the attribute. Can be either HASH or RANGE. For more information about
-- primary keys, see Primary Key in the Amazon DynamoDB Developer Guide.
tdKeySchema :: Lens' TableDescription (NonEmpty KeySchemaElement)
tdKeySchema = lens _tdKeySchema (\s a -> s { _tdKeySchema = a })
    . _List1

-- | Represents one or more local secondary indexes on the table. Each index
-- is scoped to a given hash key value. Tables with one or more local
-- secondary indexes are subject to an item collection size limit, where the
-- amount of data within a given item collection cannot exceed 10 GB. Each
-- element is composed of: IndexName - The name of the local secondary
-- index. KeySchema - Specifies the complete index key schema. The attribute
-- names in the key schema must be between 1 and 255 characters (inclusive).
-- The key schema must begin with the same hash key attribute as the table.
-- Projection - Specifies attributes that are copied (projected) from the
-- table into the index. These are in addition to the primary key attributes
-- and index key attributes, which are automatically projected. Each
-- attribute specification is composed of: ProjectionType - One of the
-- following: KEYS_ONLY - Only the index and primary keys are projected into
-- the index. INCLUDE - Only the specified table attributes are projected
-- into the index. The list of projected attributes are in NonKeyAttributes.
-- ALL - All of the table attributes are projected into the index.
-- NonKeyAttributes - A list of one or more non-key attribute names that are
-- projected into the secondary index. The total count of attributes
-- specified in NonKeyAttributes, summed across all of the secondary
-- indexes, must not exceed 20. If you project the same attribute into two
-- different indexes, this counts as two distinct attributes when
-- determining the total. IndexSizeBytes - Represents the total size of the
-- index, in bytes. DynamoDB updates this value approximately every six
-- hours. Recent changes might not be reflected in this value. ItemCount -
-- Represents the number of items in the index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value. If the table is in the DELETING state, no information about
-- indexes will be returned.
tdLocalSecondaryIndexes :: Lens' TableDescription [LocalSecondaryIndexDescription]
tdLocalSecondaryIndexes =
    lens _tdLocalSecondaryIndexes (\s a -> s { _tdLocalSecondaryIndexes = a })

-- | The provisioned throughput settings for the table, consisting of read and
-- write capacity units, along with data about increases and decreases.
tdProvisionedThroughput :: Lens' TableDescription (Maybe ProvisionedThroughputDescription)
tdProvisionedThroughput =
    lens _tdProvisionedThroughput (\s a -> s { _tdProvisionedThroughput = a })

-- | The name of the table.
tdTableName :: Lens' TableDescription (Maybe Text)
tdTableName = lens _tdTableName (\s a -> s { _tdTableName = a })

-- | The total size of the specified table, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be
-- reflected in this value.
tdTableSizeBytes :: Lens' TableDescription (Maybe Integer)
tdTableSizeBytes = lens _tdTableSizeBytes (\s a -> s { _tdTableSizeBytes = a })

-- | The current state of the table: CREATING - The table is being created, as
-- the result of a CreateTable operation. UPDATING - The table is being
-- updated, as the result of an UpdateTable operation. DELETING - The table
-- is being deleted, as the result of a DeleteTable operation. ACTIVE - The
-- table is ready for use.
tdTableStatus :: Lens' TableDescription (Maybe Text)
tdTableStatus = lens _tdTableStatus (\s a -> s { _tdTableStatus = a })

instance FromJSON TableDescription

instance ToJSON TableDescription

data KeysAndAttributes = KeysAndAttributes
    { _kaaAttributesToGet          :: List1 Text
    , _kaaConsistentRead           :: Maybe Bool
    , _kaaExpressionAttributeNames :: Map Text Text
    , _kaaKeys                     :: List1 (Map Text AttributeValue)
    , _kaaProjectionExpression     :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'KeysAndAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kaaAttributesToGet' @::@ 'NonEmpty' 'Text'
--
-- * 'kaaConsistentRead' @::@ 'Maybe' 'Bool'
--
-- * 'kaaExpressionAttributeNames' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'kaaKeys' @::@ 'NonEmpty' ('HashMap' 'Text' 'AttributeValue')
--
-- * 'kaaProjectionExpression' @::@ 'Maybe' 'Text'
--
keysAndAttributes :: NonEmpty (HashMap Text AttributeValue) -- ^ 'kaaKeys'
                  -> NonEmpty Text -- ^ 'kaaAttributesToGet'
                  -> KeysAndAttributes
keysAndAttributes p1 p2 = KeysAndAttributes
    { _kaaKeys                     = withIso _List1 (const id) p1
    , _kaaAttributesToGet          = withIso _List1 (const id) p2
    , _kaaConsistentRead           = Nothing
    , _kaaProjectionExpression     = Nothing
    , _kaaExpressionAttributeNames = mempty
    }

-- | One or more attributes to retrieve from the table or index. If no
-- attribute names are specified then all attributes will be returned. If
-- any of the specified attributes are not found, they will not appear in
-- the result.
kaaAttributesToGet :: Lens' KeysAndAttributes (NonEmpty Text)
kaaAttributesToGet =
    lens _kaaAttributesToGet (\s a -> s { _kaaAttributesToGet = a })
        . _List1

-- | The consistency of a read operation. If set to true, then a strongly
-- consistent read is used; otherwise, an eventually consistent read is
-- used.
kaaConsistentRead :: Lens' KeysAndAttributes (Maybe Bool)
kaaConsistentRead =
    lens _kaaConsistentRead (\s a -> s { _kaaConsistentRead = a })

-- | One or more substitution tokens for simplifying complex expressions. The
-- following are some use cases for an ExpressionAttributeNames value: To
-- shorten an attribute name that is very long or unwieldy in an expression.
-- To create a placeholder for repeating occurrences of an attribute name in
-- an expression. To prevent special characters in an attribute name from
-- being misinterpreted in an expression. Use the # character in an
-- expression to dereference an attribute name. For example, consider the
-- following expression: order.customerInfo.LastName = "Smith" OR
-- order.customerInfo.LastName = "Jones" Now suppose that you specified the
-- following for ExpressionAttributeNames:
-- {"n":"order.customerInfo.LastName"} The expression can now be simplified
-- as follows: #n = "Smith" OR #n = "Jones".
kaaExpressionAttributeNames :: Lens' KeysAndAttributes (HashMap Text Text)
kaaExpressionAttributeNames =
    lens _kaaExpressionAttributeNames
        (\s a -> s { _kaaExpressionAttributeNames = a })
            . _Map

-- | The primary key attribute values that define the items and the attributes
-- associated with the items.
kaaKeys :: Lens' KeysAndAttributes (NonEmpty (HashMap Text AttributeValue))
kaaKeys = lens _kaaKeys (\s a -> s { _kaaKeys = a })
    . _List1

-- | One or more attributes to retrieve from the table. These attributes can
-- include scalars, sets, or elements of a JSON document. The attributes in
-- the expression must be separated by commas. If no attribute names are
-- specified, then all attributes will be returned. If any of the requested
-- attributes are not found, they will not appear in the result.
kaaProjectionExpression :: Lens' KeysAndAttributes (Maybe Text)
kaaProjectionExpression =
    lens _kaaProjectionExpression (\s a -> s { _kaaProjectionExpression = a })

instance FromJSON KeysAndAttributes

instance ToJSON KeysAndAttributes

data ReturnConsumedCapacity
    = Indexes -- ^ INDEXES
    | None    -- ^ NONE
    | Total   -- ^ TOTAL
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ReturnConsumedCapacity

instance FromText ReturnConsumedCapacity where
    parser = match "INDEXES" Indexes
         <|> match "NONE"    None
         <|> match "TOTAL"   Total

instance ToText ReturnConsumedCapacity where
    toText = \case
        Indexes -> "INDEXES"
        None    -> "NONE"
        Total   -> "TOTAL"

instance FromJSON ReturnConsumedCapacity

instance ToJSON ReturnConsumedCapacity

data ReturnItemCollectionMetrics
    = RICMNone -- ^ NONE
    | RICMSize -- ^ SIZE
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ReturnItemCollectionMetrics

instance FromText ReturnItemCollectionMetrics where
    parser = match "NONE" RICMNone
         <|> match "SIZE" RICMSize

instance ToText ReturnItemCollectionMetrics where
    toText = \case
        RICMNone -> "NONE"
        RICMSize -> "SIZE"

instance FromJSON ReturnItemCollectionMetrics

instance ToJSON ReturnItemCollectionMetrics

data AttributeValueUpdate = AttributeValueUpdate
    { _avuAction :: Maybe Text
    , _avuValue  :: Maybe AttributeValue
    } deriving (Eq, Show, Generic)

-- | 'AttributeValueUpdate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avuAction' @::@ 'Maybe' 'Text'
--
-- * 'avuValue' @::@ 'Maybe' 'AttributeValue'
--
attributeValueUpdate :: AttributeValueUpdate
attributeValueUpdate = AttributeValueUpdate
    { _avuValue  = Nothing
    , _avuAction = Nothing
    }

-- | Specifies how to perform the update. Valid values are PUT (default),
-- DELETE, and ADD. The behavior depends on whether the specified primary
-- key already exists in the table. If an item with the specified Key is
-- found in the table: PUT - Adds the specified attribute to the item. If
-- the attribute already exists, it is replaced by the new value. DELETE -
-- If no value is specified, the attribute and its value are removed from
-- the item. The data type of the specified value must match the existing
-- value's data type. If a set of values is specified, then those values are
-- subtracted from the old set. For example, if the attribute value was the
-- set [a,b,c] and the DELETE action specified [a,c], then the final
-- attribute value would be [b]. Specifying an empty set is an error. ADD -
-- If the attribute does not already exist, then the attribute and its
-- values are added to the item. If the attribute does exist, then the
-- behavior of ADD depends on the data type of the attribute: If the
-- existing attribute is a number, and if Value is also a number, then the
-- Value is mathematically added to the existing attribute. If Value is a
-- negative number, then it is subtracted from the existing attribute. If
-- the existing data type is a set, and if the Value is also a set, then the
-- Value is added to the existing set. (This is a set operation, not
-- mathematical addition.) For example, if the attribute value was the set
-- [1,2], and the ADD action specified [3], then the final attribute value
-- would be [1,2,3]. An error occurs if an Add action is specified for a set
-- attribute and the attribute type specified does not match the existing
-- set type. Both sets must have the same primitive data type. For example,
-- if the existing data type is a set of strings, the Value must also be a
-- set of strings. The same holds true for number sets and binary sets. This
-- action is only valid for an existing attribute whose data type is number
-- or is a set. Do not use ADD for any other data types. If no item with the
-- specified Key is found: PUT - DynamoDB creates a new item with the
-- specified primary key, and then adds the attribute. DELETE - Nothing
-- happens; there is no attribute to delete. ADD - DynamoDB creates an item
-- with the supplied primary key and number (or set of numbers) for the
-- attribute value. The only data types allowed are number and number set;
-- no other data types can be specified.
avuAction :: Lens' AttributeValueUpdate (Maybe Text)
avuAction = lens _avuAction (\s a -> s { _avuAction = a })

avuValue :: Lens' AttributeValueUpdate (Maybe AttributeValue)
avuValue = lens _avuValue (\s a -> s { _avuValue = a })

instance FromJSON AttributeValueUpdate

instance ToJSON AttributeValueUpdate

data ExpectedAttributeValue = ExpectedAttributeValue
    { _eavAttributeValueList :: [AttributeValue]
    , _eavComparisonOperator :: Maybe Text
    , _eavExists             :: Maybe Bool
    , _eavValue              :: Maybe AttributeValue
    } deriving (Eq, Show, Generic)

-- | 'ExpectedAttributeValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eavAttributeValueList' @::@ ['AttributeValue']
--
-- * 'eavComparisonOperator' @::@ 'Maybe' 'Text'
--
-- * 'eavExists' @::@ 'Maybe' 'Bool'
--
-- * 'eavValue' @::@ 'Maybe' 'AttributeValue'
--
expectedAttributeValue :: ExpectedAttributeValue
expectedAttributeValue = ExpectedAttributeValue
    { _eavValue              = Nothing
    , _eavExists             = Nothing
    , _eavComparisonOperator = Nothing
    , _eavAttributeValueList = mempty
    }

-- | One or more values to evaluate against the supplied attribute. The number
-- of values in the list depends on the ComparisonOperator being used. For
-- type Number, value comparisons are numeric. String value comparisons for
-- greater than, equals, or less than are based on ASCII character code
-- values. For example, a is greater than A, and aa is greater than B. For a
-- list of code values, see
-- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters. For
-- Binary, DynamoDB treats each byte of the binary data as unsigned when it
-- compares binary values, for example when evaluating query expressions.
-- For information on specifying data types in JSON, see JSON Data Format in
-- the Amazon DynamoDB Developer Guide.
eavAttributeValueList :: Lens' ExpectedAttributeValue [AttributeValue]
eavAttributeValueList =
    lens _eavAttributeValueList (\s a -> s { _eavAttributeValueList = a })

-- | A comparator for evaluating attributes in the AttributeValueList. For
-- example, equals, greater than, less than, etc. The following comparison
-- operators are available: EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL |
-- CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN The following are
-- descriptions of each comparison operator. EQ : Equal. EQ is supported for
-- all datatypes, including lists and maps. AttributeValueList can contain
-- only one AttributeValue element of type String, Number, Binary, String
-- Set, Number Set, or Binary Set. If an item contains an AttributeValue
-- element of a different type than the one specified in the request, the
-- value does not match. For example, {"S":"6"} does not equal {"N":"6"}.
-- Also, {"N":"6"} does not equal {"NS":["6", "2", "1"]}. NE : Not equal. NE
-- is supported for all datatypes, including lists and maps.
-- AttributeValueList can contain only one AttributeValue of type String,
-- Number, Binary, String Set, Number Set, or Binary Set. If an item
-- contains an AttributeValue of a different type than the one specified in
-- the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not equal {"NS":["6", "2", "1"]}.
-- LE : Less than or equal. AttributeValueList can contain only one
-- AttributeValue element of type String, Number, or Binary (not a set
-- type). If an item contains an AttributeValue element of a different type
-- than the one specified in the request, the value does not match. For
-- example, {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not
-- compare to {"NS":["6", "2", "1"]}. LT : Less than. AttributeValueList can
-- contain only one AttributeValue of type String, Number, or Binary (not a
-- set type). If an item contains an AttributeValue element of a different
-- type than the one specified in the request, the value does not match. For
-- example, {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not
-- compare to {"NS":["6", "2", "1"]}. GE : Greater than or equal.
-- AttributeValueList can contain only one AttributeValue element of type
-- String, Number, or Binary (not a set type). If an item contains an
-- AttributeValue element of a different type than the one specified in the
-- request, the value does not match. For example, {"S":"6"} does not equal
-- {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2", "1"]}. GT
-- : Greater than. AttributeValueList can contain only one AttributeValue
-- element of type String, Number, or Binary (not a set type). If an item
-- contains an AttributeValue element of a different type than the one
-- specified in the request, the value does not match. For example,
-- {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not compare to
-- {"NS":["6", "2", "1"]}. NOT_NULL : The attribute exists. NOT_NULL is
-- supported for all datatypes, including lists and maps. NULL : The
-- attribute does not exist. NULL is supported for all datatypes, including
-- lists and maps. CONTAINS : Checks for a subsequence, or value in a set.
-- AttributeValueList can contain only one AttributeValue element of type
-- String, Number, or Binary (not a set type). If the target attribute of
-- the comparison is of type String, then the operator checks for a
-- substring match. If the target attribute of the comparison is of type
-- Binary, then the operator looks for a subsequence of the target that
-- matches the input. If the target attribute of the comparison is a set
-- ("SS", "NS", or "BS"), then the operator evaluates to true if it finds an
-- exact match with any member of the set. CONTAINS is supported for lists:
-- When evaluating "a CONTAINS b", "a" can be a list; however, "b" cannot be
-- a set, a map, or a list. NOT_CONTAINS : Checks for absence of a
-- subsequence, or absence of a value in a set. AttributeValueList can
-- contain only one AttributeValue element of type String, Number, or Binary
-- (not a set type). If the target attribute of the comparison is a String,
-- then the operator checks for the absence of a substring match. If the
-- target attribute of the comparison is Binary, then the operator checks
-- for the absence of a subsequence of the target that matches the input. If
-- the target attribute of the comparison is a set ("SS", "NS", or "BS"),
-- then the operator evaluates to true if it does not find an exact match
-- with any member of the set. NOT_CONTAINS is supported for lists: When
-- evaluating "a NOT CONTAINS b", "a" can be a list; however, "b" cannot be
-- a set, a map, or a list. BEGINS_WITH : Checks for a prefix.
-- AttributeValueList can contain only one AttributeValue of type String or
-- Binary (not a Number or a set type). The target attribute of the
-- comparison must be of type String or Binary (not a Number or a set type).
-- IN : Checks for matching elements within two sets. AttributeValueList can
-- contain one or more AttributeValue elements of type String, Number, or
-- Binary (not a set type). These attributes are compared against an
-- existing set type attribute of an item. If any elements of the input set
-- are present in the item attribute, the expression evaluates to true.
-- BETWEEN : Greater than or equal to the first value, and less than or
-- equal to the second value. AttributeValueList must contain two
-- AttributeValue elements of the same type, either String, Number, or
-- Binary (not a set type). A target attribute matches if the target value
-- is greater than, or equal to, the first element and less than, or equal
-- to, the second element. If an item contains an AttributeValue element of
-- a different type than the one specified in the request, the value does
-- not match. For example, {"S":"6"} does not compare to {"N":"6"}. Also,
-- {"N":"6"} does not compare to {"NS":["6", "2", "1"]}.
eavComparisonOperator :: Lens' ExpectedAttributeValue (Maybe Text)
eavComparisonOperator =
    lens _eavComparisonOperator (\s a -> s { _eavComparisonOperator = a })

-- | Causes DynamoDB to evaluate the value before attempting a conditional
-- operation: If Exists is true, DynamoDB will check to see if that
-- attribute value already exists in the table. If it is found, then the
-- operation succeeds. If it is not found, the operation fails with a
-- ConditionalCheckFailedException. If Exists is false, DynamoDB assumes
-- that the attribute value does not exist in the table. If in fact the
-- value does not exist, then the assumption is valid and the operation
-- succeeds. If the value is found, despite the assumption that it does not
-- exist, the operation fails with a ConditionalCheckFailedException. The
-- default setting for Exists is true. If you supply a Value all by itself,
-- DynamoDB assumes the attribute exists: You don't have to set Exists to
-- true, because it is implied. DynamoDB returns a ValidationException if:
-- Exists is true but there is no Value to check. (You expect a value to
-- exist, but don't specify what that value is.) Exists is false but you
-- also specify a Value. (You cannot expect an attribute to have a value,
-- while also expecting it not to exist.).
eavExists :: Lens' ExpectedAttributeValue (Maybe Bool)
eavExists = lens _eavExists (\s a -> s { _eavExists = a })

eavValue :: Lens' ExpectedAttributeValue (Maybe AttributeValue)
eavValue = lens _eavValue (\s a -> s { _eavValue = a })

instance FromJSON ExpectedAttributeValue

instance ToJSON ExpectedAttributeValue

data AttributeDefinition = AttributeDefinition
    { _adAttributeName :: Text
    , _adAttributeType :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AttributeDefinition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adAttributeName' @::@ 'Text'
--
-- * 'adAttributeType' @::@ 'Text'
--
attributeDefinition :: Text -- ^ 'adAttributeName'
                    -> Text -- ^ 'adAttributeType'
                    -> AttributeDefinition
attributeDefinition p1 p2 = AttributeDefinition
    { _adAttributeName = p1
    , _adAttributeType = p2
    }

-- | A name for the attribute.
adAttributeName :: Lens' AttributeDefinition Text
adAttributeName = lens _adAttributeName (\s a -> s { _adAttributeName = a })

-- | The data type for the attribute.
adAttributeType :: Lens' AttributeDefinition Text
adAttributeType = lens _adAttributeType (\s a -> s { _adAttributeType = a })

instance FromJSON AttributeDefinition

instance ToJSON AttributeDefinition

data ComparisonOperator
    = BeginsWith  -- ^ BEGINS_WITH
    | Between     -- ^ BETWEEN
    | Contains    -- ^ CONTAINS
    | Eq          -- ^ EQ
    | Ge          -- ^ GE
    | Gt          -- ^ GT
    | In          -- ^ IN
    | Le          -- ^ LE
    | Lt          -- ^ LT
    | Ne          -- ^ NE
    | NotContains -- ^ NOT_CONTAINS
    | NotNull     -- ^ NOT_NULL
    | Null        -- ^ NULL
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ComparisonOperator

instance FromText ComparisonOperator where
    parser = match "BEGINS_WITH"  BeginsWith
         <|> match "BETWEEN"      Between
         <|> match "CONTAINS"     Contains
         <|> match "EQ"           Eq
         <|> match "GE"           Ge
         <|> match "GT"           Gt
         <|> match "IN"           In
         <|> match "LE"           Le
         <|> match "LT"           Lt
         <|> match "NE"           Ne
         <|> match "NOT_CONTAINS" NotContains
         <|> match "NOT_NULL"     NotNull
         <|> match "NULL"         Null

instance ToText ComparisonOperator where
    toText = \case
        BeginsWith  -> "BEGINS_WITH"
        Between     -> "BETWEEN"
        Contains    -> "CONTAINS"
        Eq          -> "EQ"
        Ge          -> "GE"
        Gt          -> "GT"
        In          -> "IN"
        Le          -> "LE"
        Lt          -> "LT"
        Ne          -> "NE"
        NotContains -> "NOT_CONTAINS"
        NotNull     -> "NOT_NULL"
        Null        -> "NULL"

instance FromJSON ComparisonOperator

instance ToJSON ComparisonOperator

data ReturnValue
    = RVAllNew     -- ^ ALL_NEW
    | RVAllOld     -- ^ ALL_OLD
    | RVNone       -- ^ NONE
    | RVUpdatedNew -- ^ UPDATED_NEW
    | RVUpdatedOld -- ^ UPDATED_OLD
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ReturnValue

instance FromText ReturnValue where
    parser = match "ALL_NEW"     RVAllNew
         <|> match "ALL_OLD"     RVAllOld
         <|> match "NONE"        RVNone
         <|> match "UPDATED_NEW" RVUpdatedNew
         <|> match "UPDATED_OLD" RVUpdatedOld

instance ToText ReturnValue where
    toText = \case
        RVAllNew     -> "ALL_NEW"
        RVAllOld     -> "ALL_OLD"
        RVNone       -> "NONE"
        RVUpdatedNew -> "UPDATED_NEW"
        RVUpdatedOld -> "UPDATED_OLD"

instance FromJSON ReturnValue

instance ToJSON ReturnValue

data LocalSecondaryIndex = LocalSecondaryIndex
    { _lsiIndexName  :: Text
    , _lsiKeySchema  :: List1 KeySchemaElement
    , _lsiProjection :: Projection
    } deriving (Eq, Show, Generic)

-- | 'LocalSecondaryIndex' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsiIndexName' @::@ 'Text'
--
-- * 'lsiKeySchema' @::@ 'NonEmpty' 'KeySchemaElement'
--
-- * 'lsiProjection' @::@ 'Projection'
--
localSecondaryIndex :: Text -- ^ 'lsiIndexName'
                    -> NonEmpty KeySchemaElement -- ^ 'lsiKeySchema'
                    -> Projection -- ^ 'lsiProjection'
                    -> LocalSecondaryIndex
localSecondaryIndex p1 p2 p3 = LocalSecondaryIndex
    { _lsiIndexName  = p1
    , _lsiKeySchema  = withIso _List1 (const id) p2
    , _lsiProjection = p3
    }

-- | The name of the local secondary index. The name must be unique among all
-- other indexes on this table.
lsiIndexName :: Lens' LocalSecondaryIndex Text
lsiIndexName = lens _lsiIndexName (\s a -> s { _lsiIndexName = a })

-- | The complete key schema for the local secondary index, consisting of one
-- or more pairs of attribute names and key types (HASH or RANGE).
lsiKeySchema :: Lens' LocalSecondaryIndex (NonEmpty KeySchemaElement)
lsiKeySchema = lens _lsiKeySchema (\s a -> s { _lsiKeySchema = a })
    . _List1

lsiProjection :: Lens' LocalSecondaryIndex Projection
lsiProjection = lens _lsiProjection (\s a -> s { _lsiProjection = a })

instance FromJSON LocalSecondaryIndex

instance ToJSON LocalSecondaryIndex

data GlobalSecondaryIndexDescription = GlobalSecondaryIndexDescription
    { _gsidIndexName             :: Maybe Text
    , _gsidIndexSizeBytes        :: Maybe Integer
    , _gsidIndexStatus           :: Maybe Text
    , _gsidItemCount             :: Maybe Integer
    , _gsidKeySchema             :: List1 KeySchemaElement
    , _gsidProjection            :: Maybe Projection
    , _gsidProvisionedThroughput :: Maybe ProvisionedThroughputDescription
    } deriving (Eq, Show, Generic)

-- | 'GlobalSecondaryIndexDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsidIndexName' @::@ 'Maybe' 'Text'
--
-- * 'gsidIndexSizeBytes' @::@ 'Maybe' 'Integer'
--
-- * 'gsidIndexStatus' @::@ 'Maybe' 'Text'
--
-- * 'gsidItemCount' @::@ 'Maybe' 'Integer'
--
-- * 'gsidKeySchema' @::@ 'NonEmpty' 'KeySchemaElement'
--
-- * 'gsidProjection' @::@ 'Maybe' 'Projection'
--
-- * 'gsidProvisionedThroughput' @::@ 'Maybe' 'ProvisionedThroughputDescription'
--
globalSecondaryIndexDescription :: NonEmpty KeySchemaElement -- ^ 'gsidKeySchema'
                                -> GlobalSecondaryIndexDescription
globalSecondaryIndexDescription p1 = GlobalSecondaryIndexDescription
    { _gsidKeySchema             = withIso _List1 (const id) p1
    , _gsidIndexName             = Nothing
    , _gsidProjection            = Nothing
    , _gsidIndexStatus           = Nothing
    , _gsidProvisionedThroughput = Nothing
    , _gsidIndexSizeBytes        = Nothing
    , _gsidItemCount             = Nothing
    }

-- | The name of the global secondary index.
gsidIndexName :: Lens' GlobalSecondaryIndexDescription (Maybe Text)
gsidIndexName = lens _gsidIndexName (\s a -> s { _gsidIndexName = a })

-- | The total size of the specified index, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be
-- reflected in this value.
gsidIndexSizeBytes :: Lens' GlobalSecondaryIndexDescription (Maybe Integer)
gsidIndexSizeBytes =
    lens _gsidIndexSizeBytes (\s a -> s { _gsidIndexSizeBytes = a })

-- | The current state of the global secondary index: CREATING - The index is
-- being created, as the result of a CreateTable or UpdateTable operation.
-- UPDATING - The index is being updated, as the result of a CreateTable or
-- UpdateTable operation. DELETING - The index is being deleted, as the
-- result of a DeleteTable operation. ACTIVE - The index is ready for use.
gsidIndexStatus :: Lens' GlobalSecondaryIndexDescription (Maybe Text)
gsidIndexStatus = lens _gsidIndexStatus (\s a -> s { _gsidIndexStatus = a })

-- | The number of items in the specified index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
gsidItemCount :: Lens' GlobalSecondaryIndexDescription (Maybe Integer)
gsidItemCount = lens _gsidItemCount (\s a -> s { _gsidItemCount = a })

-- | The complete key schema for the global secondary index, consisting of one
-- or more pairs of attribute names and key types (HASH or RANGE).
gsidKeySchema :: Lens' GlobalSecondaryIndexDescription (NonEmpty KeySchemaElement)
gsidKeySchema = lens _gsidKeySchema (\s a -> s { _gsidKeySchema = a })
    . _List1

gsidProjection :: Lens' GlobalSecondaryIndexDescription (Maybe Projection)
gsidProjection = lens _gsidProjection (\s a -> s { _gsidProjection = a })

gsidProvisionedThroughput :: Lens' GlobalSecondaryIndexDescription (Maybe ProvisionedThroughputDescription)
gsidProvisionedThroughput =
    lens _gsidProvisionedThroughput
        (\s a -> s { _gsidProvisionedThroughput = a })

instance FromJSON GlobalSecondaryIndexDescription

instance ToJSON GlobalSecondaryIndexDescription

data ItemCollectionMetrics = ItemCollectionMetrics
    { _icmItemCollectionKey   :: Map Text AttributeValue
    , _icmSizeEstimateRangeGB :: [Double]
    } deriving (Eq, Show, Generic)

-- | 'ItemCollectionMetrics' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'icmItemCollectionKey' @::@ 'HashMap' 'Text' 'AttributeValue'
--
-- * 'icmSizeEstimateRangeGB' @::@ ['Double']
--
itemCollectionMetrics :: ItemCollectionMetrics
itemCollectionMetrics = ItemCollectionMetrics
    { _icmItemCollectionKey   = mempty
    , _icmSizeEstimateRangeGB = mempty
    }

-- | The hash key value of the item collection. This value is the same as the
-- hash key of the item.
icmItemCollectionKey :: Lens' ItemCollectionMetrics (HashMap Text AttributeValue)
icmItemCollectionKey =
    lens _icmItemCollectionKey (\s a -> s { _icmItemCollectionKey = a })
        . _Map

-- | An estimate of item collection size, in gigabytes. This value is a
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

newtype Capacity = Capacity
    { _cCapacityUnits :: Maybe Double
    } deriving (Eq, Ord, Show, Generic)

-- | 'Capacity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cCapacityUnits' @::@ 'Maybe' 'Double'
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

data ConsumedCapacity = ConsumedCapacity
    { _ccCapacityUnits          :: Maybe Double
    , _ccGlobalSecondaryIndexes :: Map Text Capacity
    , _ccLocalSecondaryIndexes  :: Map Text Capacity
    , _ccTable                  :: Maybe Capacity
    , _ccTableName              :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ConsumedCapacity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccCapacityUnits' @::@ 'Maybe' 'Double'
--
-- * 'ccGlobalSecondaryIndexes' @::@ 'HashMap' 'Text' 'Capacity'
--
-- * 'ccLocalSecondaryIndexes' @::@ 'HashMap' 'Text' 'Capacity'
--
-- * 'ccTable' @::@ 'Maybe' 'Capacity'
--
-- * 'ccTableName' @::@ 'Maybe' 'Text'
--
consumedCapacity :: ConsumedCapacity
consumedCapacity = ConsumedCapacity
    { _ccTableName              = Nothing
    , _ccCapacityUnits          = Nothing
    , _ccTable                  = Nothing
    , _ccLocalSecondaryIndexes  = mempty
    , _ccGlobalSecondaryIndexes = mempty
    }

-- | The total number of capacity units consumed by the operation.
ccCapacityUnits :: Lens' ConsumedCapacity (Maybe Double)
ccCapacityUnits = lens _ccCapacityUnits (\s a -> s { _ccCapacityUnits = a })

-- | The amount of throughput consumed on each global index affected by the
-- operation.
ccGlobalSecondaryIndexes :: Lens' ConsumedCapacity (HashMap Text Capacity)
ccGlobalSecondaryIndexes =
    lens _ccGlobalSecondaryIndexes
        (\s a -> s { _ccGlobalSecondaryIndexes = a })
            . _Map

-- | The amount of throughput consumed on each local index affected by the
-- operation.
ccLocalSecondaryIndexes :: Lens' ConsumedCapacity (HashMap Text Capacity)
ccLocalSecondaryIndexes =
    lens _ccLocalSecondaryIndexes (\s a -> s { _ccLocalSecondaryIndexes = a })
        . _Map

-- | The amount of throughput consumed on the table affected by the operation.
ccTable :: Lens' ConsumedCapacity (Maybe Capacity)
ccTable = lens _ccTable (\s a -> s { _ccTable = a })

-- | The name of the table that was affected by the operation.
ccTableName :: Lens' ConsumedCapacity (Maybe Text)
ccTableName = lens _ccTableName (\s a -> s { _ccTableName = a })

instance FromJSON ConsumedCapacity

instance ToJSON ConsumedCapacity

data GlobalSecondaryIndex = GlobalSecondaryIndex
    { _gsiIndexName             :: Text
    , _gsiKeySchema             :: List1 KeySchemaElement
    , _gsiProjection            :: Projection
    , _gsiProvisionedThroughput :: ProvisionedThroughput
    } deriving (Eq, Show, Generic)

-- | 'GlobalSecondaryIndex' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsiIndexName' @::@ 'Text'
--
-- * 'gsiKeySchema' @::@ 'NonEmpty' 'KeySchemaElement'
--
-- * 'gsiProjection' @::@ 'Projection'
--
-- * 'gsiProvisionedThroughput' @::@ 'ProvisionedThroughput'
--
globalSecondaryIndex :: Text -- ^ 'gsiIndexName'
                     -> NonEmpty KeySchemaElement -- ^ 'gsiKeySchema'
                     -> Projection -- ^ 'gsiProjection'
                     -> ProvisionedThroughput -- ^ 'gsiProvisionedThroughput'
                     -> GlobalSecondaryIndex
globalSecondaryIndex p1 p2 p3 p4 = GlobalSecondaryIndex
    { _gsiIndexName             = p1
    , _gsiKeySchema             = withIso _List1 (const id) p2
    , _gsiProjection            = p3
    , _gsiProvisionedThroughput = p4
    }

-- | The name of the global secondary index. The name must be unique among all
-- other indexes on this table.
gsiIndexName :: Lens' GlobalSecondaryIndex Text
gsiIndexName = lens _gsiIndexName (\s a -> s { _gsiIndexName = a })

-- | The complete key schema for a global secondary index, which consists of
-- one or more pairs of attribute names and key types (HASH or RANGE).
gsiKeySchema :: Lens' GlobalSecondaryIndex (NonEmpty KeySchemaElement)
gsiKeySchema = lens _gsiKeySchema (\s a -> s { _gsiKeySchema = a })
    . _List1

gsiProjection :: Lens' GlobalSecondaryIndex Projection
gsiProjection = lens _gsiProjection (\s a -> s { _gsiProjection = a })

gsiProvisionedThroughput :: Lens' GlobalSecondaryIndex ProvisionedThroughput
gsiProvisionedThroughput =
    lens _gsiProvisionedThroughput
        (\s a -> s { _gsiProvisionedThroughput = a })

instance FromJSON GlobalSecondaryIndex

instance ToJSON GlobalSecondaryIndex

data LocalSecondaryIndexDescription = LocalSecondaryIndexDescription
    { _lsidIndexName      :: Maybe Text
    , _lsidIndexSizeBytes :: Maybe Integer
    , _lsidItemCount      :: Maybe Integer
    , _lsidKeySchema      :: List1 KeySchemaElement
    , _lsidProjection     :: Maybe Projection
    } deriving (Eq, Show, Generic)

-- | 'LocalSecondaryIndexDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsidIndexName' @::@ 'Maybe' 'Text'
--
-- * 'lsidIndexSizeBytes' @::@ 'Maybe' 'Integer'
--
-- * 'lsidItemCount' @::@ 'Maybe' 'Integer'
--
-- * 'lsidKeySchema' @::@ 'NonEmpty' 'KeySchemaElement'
--
-- * 'lsidProjection' @::@ 'Maybe' 'Projection'
--
localSecondaryIndexDescription :: NonEmpty KeySchemaElement -- ^ 'lsidKeySchema'
                               -> LocalSecondaryIndexDescription
localSecondaryIndexDescription p1 = LocalSecondaryIndexDescription
    { _lsidKeySchema      = withIso _List1 (const id) p1
    , _lsidIndexName      = Nothing
    , _lsidProjection     = Nothing
    , _lsidIndexSizeBytes = Nothing
    , _lsidItemCount      = Nothing
    }

-- | Represents the name of the local secondary index.
lsidIndexName :: Lens' LocalSecondaryIndexDescription (Maybe Text)
lsidIndexName = lens _lsidIndexName (\s a -> s { _lsidIndexName = a })

-- | The total size of the specified index, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be
-- reflected in this value.
lsidIndexSizeBytes :: Lens' LocalSecondaryIndexDescription (Maybe Integer)
lsidIndexSizeBytes =
    lens _lsidIndexSizeBytes (\s a -> s { _lsidIndexSizeBytes = a })

-- | The number of items in the specified index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
lsidItemCount :: Lens' LocalSecondaryIndexDescription (Maybe Integer)
lsidItemCount = lens _lsidItemCount (\s a -> s { _lsidItemCount = a })

-- | The complete index key schema, which consists of one or more pairs of
-- attribute names and key types (HASH or RANGE).
lsidKeySchema :: Lens' LocalSecondaryIndexDescription (NonEmpty KeySchemaElement)
lsidKeySchema = lens _lsidKeySchema (\s a -> s { _lsidKeySchema = a })
    . _List1

lsidProjection :: Lens' LocalSecondaryIndexDescription (Maybe Projection)
lsidProjection = lens _lsidProjection (\s a -> s { _lsidProjection = a })

instance FromJSON LocalSecondaryIndexDescription

instance ToJSON LocalSecondaryIndexDescription

data AttributeAction
    = Add    -- ^ ADD
    | Delete -- ^ DELETE
    | Put    -- ^ PUT
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable AttributeAction

instance FromText AttributeAction where
    parser = match "ADD"    Add
         <|> match "DELETE" Delete
         <|> match "PUT"    Put

instance ToText AttributeAction where
    toText = \case
        Add    -> "ADD"
        Delete -> "DELETE"
        Put    -> "PUT"

instance FromJSON AttributeAction

instance ToJSON AttributeAction

data ScalarAttributeType
    = B -- ^ B
    | N -- ^ N
    | S -- ^ S
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ScalarAttributeType

instance FromText ScalarAttributeType where
    parser = match "B" B
         <|> match "N" N
         <|> match "S" S

instance ToText ScalarAttributeType where
    toText = \case
        B -> "B"
        N -> "N"
        S -> "S"

instance FromJSON ScalarAttributeType

instance ToJSON ScalarAttributeType

data Projection = Projection
    { _pNonKeyAttributes :: List1 Text
    , _pProjectionType   :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Projection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pNonKeyAttributes' @::@ 'NonEmpty' 'Text'
--
-- * 'pProjectionType' @::@ 'Maybe' 'Text'
--
projection :: NonEmpty Text -- ^ 'pNonKeyAttributes'
           -> Projection
projection p1 = Projection
    { _pNonKeyAttributes = withIso _List1 (const id) p1
    , _pProjectionType   = Nothing
    }

-- | Represents the non-key attribute names which will be projected into the
-- index. For local secondary indexes, the total count of NonKeyAttributes
-- summed across all of the local secondary indexes, must not exceed 20. If
-- you project the same attribute into two different indexes, this counts as
-- two distinct attributes when determining the total.
pNonKeyAttributes :: Lens' Projection (NonEmpty Text)
pNonKeyAttributes =
    lens _pNonKeyAttributes (\s a -> s { _pNonKeyAttributes = a })
        . _List1

-- | The set of attributes that are projected into the index: KEYS_ONLY - Only
-- the index and primary keys are projected into the index. INCLUDE - Only
-- the specified table attributes are projected into the index. The list of
-- projected attributes are in NonKeyAttributes. ALL - All of the table
-- attributes are projected into the index.
pProjectionType :: Lens' Projection (Maybe Text)
pProjectionType = lens _pProjectionType (\s a -> s { _pProjectionType = a })

instance FromJSON Projection

instance ToJSON Projection

data Select
    = AllAttributes          -- ^ ALL_ATTRIBUTES
    | AllProjectedAttributes -- ^ ALL_PROJECTED_ATTRIBUTES
    | Count                  -- ^ COUNT
    | SpecificAttributes     -- ^ SPECIFIC_ATTRIBUTES
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable Select

instance FromText Select where
    parser = match "ALL_ATTRIBUTES"           AllAttributes
         <|> match "ALL_PROJECTED_ATTRIBUTES" AllProjectedAttributes
         <|> match "COUNT"                    Count
         <|> match "SPECIFIC_ATTRIBUTES"      SpecificAttributes

instance ToText Select where
    toText = \case
        AllAttributes          -> "ALL_ATTRIBUTES"
        AllProjectedAttributes -> "ALL_PROJECTED_ATTRIBUTES"
        Count                  -> "COUNT"
        SpecificAttributes     -> "SPECIFIC_ATTRIBUTES"

instance FromJSON Select

instance ToJSON Select

data KeySchemaElement = KeySchemaElement
    { _kseAttributeName :: Text
    , _kseKeyType       :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'KeySchemaElement' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kseAttributeName' @::@ 'Text'
--
-- * 'kseKeyType' @::@ 'Text'
--
keySchemaElement :: Text -- ^ 'kseAttributeName'
                 -> Text -- ^ 'kseKeyType'
                 -> KeySchemaElement
keySchemaElement p1 p2 = KeySchemaElement
    { _kseAttributeName = p1
    , _kseKeyType       = p2
    }

-- | The name of a key attribute.
kseAttributeName :: Lens' KeySchemaElement Text
kseAttributeName = lens _kseAttributeName (\s a -> s { _kseAttributeName = a })

-- | The attribute data, consisting of the data type and the attribute value
-- itself.
kseKeyType :: Lens' KeySchemaElement Text
kseKeyType = lens _kseKeyType (\s a -> s { _kseKeyType = a })

instance FromJSON KeySchemaElement

instance ToJSON KeySchemaElement

newtype DeleteRequest = DeleteRequest
    { _dKey :: Map Text AttributeValue
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

-- | 'DeleteRequest' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dKey' @::@ 'HashMap' 'Text' 'AttributeValue'
--
deleteRequest :: DeleteRequest
deleteRequest = DeleteRequest
    { _dKey = mempty
    }

-- | A map of attribute name to attribute values, representing the primary key
-- of the item to delete. All of the table's primary key attributes must be
-- specified, and their data types must match those of the table's key
-- schema.
dKey :: Lens' DeleteRequest (HashMap Text AttributeValue)
dKey = lens _dKey (\s a -> s { _dKey = a })
    . _Map

instance FromJSON DeleteRequest

instance ToJSON DeleteRequest

data UpdateGlobalSecondaryIndexAction = UpdateGlobalSecondaryIndexAction
    { _ugsiaIndexName             :: Text
    , _ugsiaProvisionedThroughput :: ProvisionedThroughput
    } deriving (Eq, Show, Generic)

-- | 'UpdateGlobalSecondaryIndexAction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ugsiaIndexName' @::@ 'Text'
--
-- * 'ugsiaProvisionedThroughput' @::@ 'ProvisionedThroughput'
--
updateGlobalSecondaryIndexAction :: Text -- ^ 'ugsiaIndexName'
                                 -> ProvisionedThroughput -- ^ 'ugsiaProvisionedThroughput'
                                 -> UpdateGlobalSecondaryIndexAction
updateGlobalSecondaryIndexAction p1 p2 = UpdateGlobalSecondaryIndexAction
    { _ugsiaIndexName             = p1
    , _ugsiaProvisionedThroughput = p2
    }

-- | The name of the global secondary index to be updated.
ugsiaIndexName :: Lens' UpdateGlobalSecondaryIndexAction Text
ugsiaIndexName = lens _ugsiaIndexName (\s a -> s { _ugsiaIndexName = a })

ugsiaProvisionedThroughput :: Lens' UpdateGlobalSecondaryIndexAction ProvisionedThroughput
ugsiaProvisionedThroughput =
    lens _ugsiaProvisionedThroughput
        (\s a -> s { _ugsiaProvisionedThroughput = a })

instance FromJSON UpdateGlobalSecondaryIndexAction

instance ToJSON UpdateGlobalSecondaryIndexAction

newtype PutRequest = PutRequest
    { _pItem :: Map Text AttributeValue
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

-- | 'PutRequest' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pItem' @::@ 'HashMap' 'Text' 'AttributeValue'
--
putRequest :: PutRequest
putRequest = PutRequest
    { _pItem = mempty
    }

-- | A map of attribute name to attribute values, representing the primary key
-- of an item to be processed by PutItem. All of the table's primary key
-- attributes must be specified, and their data types must match those of
-- the table's key schema. If any attributes are present in the item which
-- are part of an index key schema for the table, their types must match the
-- index key schema.
pItem :: Lens' PutRequest (HashMap Text AttributeValue)
pItem = lens _pItem (\s a -> s { _pItem = a })
    . _Map

instance FromJSON PutRequest

instance ToJSON PutRequest

data Condition = Condition
    { _cAttributeValueList :: [AttributeValue]
    , _cComparisonOperator :: Text
    } deriving (Eq, Show, Generic)

-- | 'Condition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cAttributeValueList' @::@ ['AttributeValue']
--
-- * 'cComparisonOperator' @::@ 'Text'
--
condition :: Text -- ^ 'cComparisonOperator'
          -> Condition
condition p1 = Condition
    { _cComparisonOperator = p1
    , _cAttributeValueList = mempty
    }

-- | One or more values to evaluate against the supplied attribute. The number
-- of values in the list depends on the ComparisonOperator being used. For
-- type Number, value comparisons are numeric. String value comparisons for
-- greater than, equals, or less than are based on ASCII character code
-- values. For example, a is greater than A, and aa is greater than B. For a
-- list of code values, see
-- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters. For
-- Binary, DynamoDB treats each byte of the binary data as unsigned when it
-- compares binary values, for example when evaluating query expressions.
cAttributeValueList :: Lens' Condition [AttributeValue]
cAttributeValueList =
    lens _cAttributeValueList (\s a -> s { _cAttributeValueList = a })

-- | A comparator for evaluating attributes. For example, equals, greater
-- than, less than, etc. The following comparison operators are available:
-- EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS |
-- BEGINS_WITH | IN | BETWEEN The following are descriptions of each
-- comparison operator. EQ : Equal. EQ is supported for all datatypes,
-- including lists and maps. AttributeValueList can contain only one
-- AttributeValue element of type String, Number, Binary, String Set, Number
-- Set, or Binary Set. If an item contains an AttributeValue element of a
-- different type than the one specified in the request, the value does not
-- match. For example, {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"}
-- does not equal {"NS":["6", "2", "1"]}. NE : Not equal. NE is supported
-- for all datatypes, including lists and maps. AttributeValueList can
-- contain only one AttributeValue of type String, Number, Binary, String
-- Set, Number Set, or Binary Set. If an item contains an AttributeValue of
-- a different type than the one specified in the request, the value does
-- not match. For example, {"S":"6"} does not equal {"N":"6"}. Also,
-- {"N":"6"} does not equal {"NS":["6", "2", "1"]}. LE : Less than or equal.
-- AttributeValueList can contain only one AttributeValue element of type
-- String, Number, or Binary (not a set type). If an item contains an
-- AttributeValue element of a different type than the one specified in the
-- request, the value does not match. For example, {"S":"6"} does not equal
-- {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2", "1"]}. LT
-- : Less than. AttributeValueList can contain only one AttributeValue of
-- type String, Number, or Binary (not a set type). If an item contains an
-- AttributeValue element of a different type than the one specified in the
-- request, the value does not match. For example, {"S":"6"} does not equal
-- {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2", "1"]}. GE
-- : Greater than or equal. AttributeValueList can contain only one
-- AttributeValue element of type String, Number, or Binary (not a set
-- type). If an item contains an AttributeValue element of a different type
-- than the one specified in the request, the value does not match. For
-- example, {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not
-- compare to {"NS":["6", "2", "1"]}. GT : Greater than. AttributeValueList
-- can contain only one AttributeValue element of type String, Number, or
-- Binary (not a set type). If an item contains an AttributeValue element of
-- a different type than the one specified in the request, the value does
-- not match. For example, {"S":"6"} does not equal {"N":"6"}. Also,
-- {"N":"6"} does not compare to {"NS":["6", "2", "1"]}. NOT_NULL : The
-- attribute exists. NOT_NULL is supported for all datatypes, including
-- lists and maps. NULL : The attribute does not exist. NULL is supported
-- for all datatypes, including lists and maps. CONTAINS : Checks for a
-- subsequence, or value in a set. AttributeValueList can contain only one
-- AttributeValue element of type String, Number, or Binary (not a set
-- type). If the target attribute of the comparison is of type String, then
-- the operator checks for a substring match. If the target attribute of the
-- comparison is of type Binary, then the operator looks for a subsequence
-- of the target that matches the input. If the target attribute of the
-- comparison is a set ("SS", "NS", or "BS"), then the operator evaluates to
-- true if it finds an exact match with any member of the set. CONTAINS is
-- supported for lists: When evaluating "a CONTAINS b", "a" can be a list;
-- however, "b" cannot be a set, a map, or a list. NOT_CONTAINS : Checks for
-- absence of a subsequence, or absence of a value in a set.
-- AttributeValueList can contain only one AttributeValue element of type
-- String, Number, or Binary (not a set type). If the target attribute of
-- the comparison is a String, then the operator checks for the absence of a
-- substring match. If the target attribute of the comparison is Binary,
-- then the operator checks for the absence of a subsequence of the target
-- that matches the input. If the target attribute of the comparison is a
-- set ("SS", "NS", or "BS"), then the operator evaluates to true if it does
-- not find an exact match with any member of the set. NOT_CONTAINS is
-- supported for lists: When evaluating "a NOT CONTAINS b", "a" can be a
-- list; however, "b" cannot be a set, a map, or a list. BEGINS_WITH :
-- Checks for a prefix. AttributeValueList can contain only one
-- AttributeValue of type String or Binary (not a Number or a set type). The
-- target attribute of the comparison must be of type String or Binary (not
-- a Number or a set type). IN : Checks for matching elements within two
-- sets. AttributeValueList can contain one or more AttributeValue elements
-- of type String, Number, or Binary (not a set type). These attributes are
-- compared against an existing set type attribute of an item. If any
-- elements of the input set are present in the item attribute, the
-- expression evaluates to true. BETWEEN : Greater than or equal to the
-- first value, and less than or equal to the second value.
-- AttributeValueList must contain two AttributeValue elements of the same
-- type, either String, Number, or Binary (not a set type). A target
-- attribute matches if the target value is greater than, or equal to, the
-- first element and less than, or equal to, the second element. If an item
-- contains an AttributeValue element of a different type than the one
-- specified in the request, the value does not match. For example,
-- {"S":"6"} does not compare to {"N":"6"}. Also, {"N":"6"} does not compare
-- to {"NS":["6", "2", "1"]} For usage examples of AttributeValueList and
-- ComparisonOperator, see Legacy Conditional Parameters in the Amazon
-- DynamoDB Developer Guide.
cComparisonOperator :: Lens' Condition Text
cComparisonOperator =
    lens _cComparisonOperator (\s a -> s { _cComparisonOperator = a })

instance FromJSON Condition

instance ToJSON Condition

data ConditionalOperator
    = And -- ^ AND
    | Or  -- ^ OR
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ConditionalOperator

instance FromText ConditionalOperator where
    parser = match "AND" And
         <|> match "OR"  Or

instance ToText ConditionalOperator where
    toText = \case
        And -> "AND"
        Or  -> "OR"

instance FromJSON ConditionalOperator

instance ToJSON ConditionalOperator

newtype GlobalSecondaryIndexUpdate = GlobalSecondaryIndexUpdate
    { _gsiuUpdate :: Maybe UpdateGlobalSecondaryIndexAction
    } deriving (Eq, Show, Generic)

-- | 'GlobalSecondaryIndexUpdate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsiuUpdate' @::@ 'Maybe' 'UpdateGlobalSecondaryIndexAction'
--
globalSecondaryIndexUpdate :: GlobalSecondaryIndexUpdate
globalSecondaryIndexUpdate = GlobalSecondaryIndexUpdate
    { _gsiuUpdate = Nothing
    }

-- | The name of a global secondary index, along with the updated provisioned
-- throughput settings that are to be applied to that index.
gsiuUpdate :: Lens' GlobalSecondaryIndexUpdate (Maybe UpdateGlobalSecondaryIndexAction)
gsiuUpdate = lens _gsiuUpdate (\s a -> s { _gsiuUpdate = a })

instance FromJSON GlobalSecondaryIndexUpdate

instance ToJSON GlobalSecondaryIndexUpdate
