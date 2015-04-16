{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

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
    , gsidBackfilling
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

    -- * CreateGlobalSecondaryIndexAction
    , CreateGlobalSecondaryIndexAction
    , createGlobalSecondaryIndexAction
    , cgsiaIndexName
    , cgsiaKeySchema
    , cgsiaProjection
    , cgsiaProvisionedThroughput

    -- * Select
    , Select (..)

    -- * KeySchemaElement
    , KeySchemaElement
    , keySchemaElement
    , kseAttributeName
    , kseKeyType

    -- * DeleteGlobalSecondaryIndexAction
    , DeleteGlobalSecondaryIndexAction
    , deleteGlobalSecondaryIndexAction
    , dgsiaIndexName

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
    , gsiuCreate
    , gsiuDelete
    , gsiuUpdate
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2012-08-10@ of the Amazon DynamoDB service.
data DynamoDB

instance AWSService DynamoDB where
    type Sg DynamoDB = V4
    type Er DynamoDB = JSONError

    service = service'
      where
        service' :: Service DynamoDB
        service' = Service
            { _svcAbbrev       = "DynamoDB"
            , _svcPrefix       = "dynamodb"
            , _svcVersion      = "2012-08-10"
            , _svcTargetPrefix = Just "DynamoDB_20120810"
            , _svcJSONVersion  = Just "1.0"
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry DynamoDB
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 10
            , _retryCheck    = check
            }

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 400 && Just "ThrottlingException" == e = True -- Throttling
            | s == 400 && Just "ProvisionedThroughputExceededException" == e = True -- Throughput Exceeded
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

data WriteRequest = WriteRequest
    { _wDeleteRequest :: Maybe DeleteRequest
    , _wPutRequest    :: Maybe PutRequest
    } deriving (Eq, Read, Show)

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

-- | A request to perform a /DeleteItem/ operation.
wDeleteRequest :: Lens' WriteRequest (Maybe DeleteRequest)
wDeleteRequest = lens _wDeleteRequest (\s a -> s { _wDeleteRequest = a })

-- | A request to perform a /PutItem/ operation.
wPutRequest :: Lens' WriteRequest (Maybe PutRequest)
wPutRequest = lens _wPutRequest (\s a -> s { _wPutRequest = a })

instance FromJSON WriteRequest where
    parseJSON = withObject "WriteRequest" $ \o -> WriteRequest
        <$> o .:? "DeleteRequest"
        <*> o .:? "PutRequest"

instance ToJSON WriteRequest where
    toJSON WriteRequest{..} = object
        [ "PutRequest"    .= _wPutRequest
        , "DeleteRequest" .= _wDeleteRequest
        ]

data ProvisionedThroughputDescription = ProvisionedThroughputDescription
    { _ptdLastDecreaseDateTime   :: Maybe POSIX
    , _ptdLastIncreaseDateTime   :: Maybe POSIX
    , _ptdNumberOfDecreasesToday :: Maybe Nat
    , _ptdReadCapacityUnits      :: Maybe Nat
    , _ptdWriteCapacityUnits     :: Maybe Nat
    } deriving (Eq, Ord, Read, Show)

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

-- | The date and time of the last provisioned throughput decrease for this table.
ptdLastDecreaseDateTime :: Lens' ProvisionedThroughputDescription (Maybe UTCTime)
ptdLastDecreaseDateTime =
    lens _ptdLastDecreaseDateTime (\s a -> s { _ptdLastDecreaseDateTime = a })
        . mapping _Time

-- | The date and time of the last provisioned throughput increase for this table.
ptdLastIncreaseDateTime :: Lens' ProvisionedThroughputDescription (Maybe UTCTime)
ptdLastIncreaseDateTime =
    lens _ptdLastIncreaseDateTime (\s a -> s { _ptdLastIncreaseDateTime = a })
        . mapping _Time

-- | The number of provisioned throughput decreases for this table during this UTC
-- calendar day. For current maximums on provisioned throughput decreases, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/.
ptdNumberOfDecreasesToday :: Lens' ProvisionedThroughputDescription (Maybe Natural)
ptdNumberOfDecreasesToday =
    lens _ptdNumberOfDecreasesToday
        (\s a -> s { _ptdNumberOfDecreasesToday = a })
            . mapping _Nat

-- | The maximum number of strongly consistent reads consumed per second before
-- DynamoDB returns a /ThrottlingException/. Eventually consistent reads require
-- less effort than strongly consistent reads, so a setting of 50 /ReadCapacityUnits/ per second provides 100 eventually consistent /ReadCapacityUnits/ per second.
ptdReadCapacityUnits :: Lens' ProvisionedThroughputDescription (Maybe Natural)
ptdReadCapacityUnits =
    lens _ptdReadCapacityUnits (\s a -> s { _ptdReadCapacityUnits = a })
        . mapping _Nat

-- | The maximum number of writes consumed per second before DynamoDB returns a /ThrottlingException/.
ptdWriteCapacityUnits :: Lens' ProvisionedThroughputDescription (Maybe Natural)
ptdWriteCapacityUnits =
    lens _ptdWriteCapacityUnits (\s a -> s { _ptdWriteCapacityUnits = a })
        . mapping _Nat

instance FromJSON ProvisionedThroughputDescription where
    parseJSON = withObject "ProvisionedThroughputDescription" $ \o -> ProvisionedThroughputDescription
        <$> o .:? "LastDecreaseDateTime"
        <*> o .:? "LastIncreaseDateTime"
        <*> o .:? "NumberOfDecreasesToday"
        <*> o .:? "ReadCapacityUnits"
        <*> o .:? "WriteCapacityUnits"

instance ToJSON ProvisionedThroughputDescription where
    toJSON ProvisionedThroughputDescription{..} = object
        [ "LastIncreaseDateTime"   .= _ptdLastIncreaseDateTime
        , "LastDecreaseDateTime"   .= _ptdLastDecreaseDateTime
        , "NumberOfDecreasesToday" .= _ptdNumberOfDecreasesToday
        , "ReadCapacityUnits"      .= _ptdReadCapacityUnits
        , "WriteCapacityUnits"     .= _ptdWriteCapacityUnits
        ]

data KeyType
    = Hash  -- ^ HASH
    | Range -- ^ RANGE
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable KeyType

instance FromText KeyType where
    parser = takeLowerText >>= \case
        "hash"  -> pure Hash
        "range" -> pure Range
        e       -> fail $
            "Failure parsing KeyType from " ++ show e

instance ToText KeyType where
    toText = \case
        Hash  -> "HASH"
        Range -> "RANGE"

instance ToByteString KeyType
instance ToHeader     KeyType
instance ToQuery      KeyType

instance FromJSON KeyType where
    parseJSON = parseJSONText "KeyType"

instance ToJSON KeyType where
    toJSON = toJSONText

data AttributeValue = AttributeValue
    { _avB    :: Maybe Base64
    , _avBOOL :: Maybe Bool
    , _avBS   :: List "BS" Base64
    , _avL    :: List "L" AttributeValue
    , _avM    :: Map Text AttributeValue
    , _avN    :: Maybe Text
    , _avNS   :: List "NS" Text
    , _avNULL :: Maybe Bool
    , _avS    :: Maybe Text
    , _avSS   :: List "SS" Text
    } deriving (Eq, Read, Show)

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
avBS = lens _avBS (\s a -> s { _avBS = a }) . _List

-- | A List of attribute values.
avL :: Lens' AttributeValue [AttributeValue]
avL = lens _avL (\s a -> s { _avL = a }) . _List

-- | A Map of attribute values.
avM :: Lens' AttributeValue (HashMap Text AttributeValue)
avM = lens _avM (\s a -> s { _avM = a }) . _Map

-- | A Number data type.
avN :: Lens' AttributeValue (Maybe Text)
avN = lens _avN (\s a -> s { _avN = a })

-- | A Number Set data type.
avNS :: Lens' AttributeValue [Text]
avNS = lens _avNS (\s a -> s { _avNS = a }) . _List

-- | A Null data type.
avNULL :: Lens' AttributeValue (Maybe Bool)
avNULL = lens _avNULL (\s a -> s { _avNULL = a })

-- | A String data type.
avS :: Lens' AttributeValue (Maybe Text)
avS = lens _avS (\s a -> s { _avS = a })

-- | A String Set data type.
avSS :: Lens' AttributeValue [Text]
avSS = lens _avSS (\s a -> s { _avSS = a }) . _List

instance FromJSON AttributeValue where
    parseJSON = withObject "AttributeValue" $ \o -> AttributeValue
        <$> o .:? "B"
        <*> o .:? "BOOL"
        <*> o .:? "BS" .!= mempty
        <*> o .:? "L" .!= mempty
        <*> o .:? "M" .!= mempty
        <*> o .:? "N"
        <*> o .:? "NS" .!= mempty
        <*> o .:? "NULL"
        <*> o .:? "S"
        <*> o .:? "SS" .!= mempty

instance ToJSON AttributeValue where
    toJSON AttributeValue{..} = object
        [ "S"    .= _avS
        , "N"    .= _avN
        , "B"    .= _avB
        , "SS"   .= _avSS
        , "NS"   .= _avNS
        , "BS"   .= _avBS
        , "M"    .= _avM
        , "L"    .= _avL
        , "NULL" .= _avNULL
        , "BOOL" .= _avBOOL
        ]

data IndexStatus
    = Active   -- ^ ACTIVE
    | Creating -- ^ CREATING
    | Deleting -- ^ DELETING
    | Updating -- ^ UPDATING
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable IndexStatus

instance FromText IndexStatus where
    parser = takeLowerText >>= \case
        "active"   -> pure Active
        "creating" -> pure Creating
        "deleting" -> pure Deleting
        "updating" -> pure Updating
        e          -> fail $
            "Failure parsing IndexStatus from " ++ show e

instance ToText IndexStatus where
    toText = \case
        Active   -> "ACTIVE"
        Creating -> "CREATING"
        Deleting -> "DELETING"
        Updating -> "UPDATING"

instance ToByteString IndexStatus
instance ToHeader     IndexStatus
instance ToQuery      IndexStatus

instance FromJSON IndexStatus where
    parseJSON = parseJSONText "IndexStatus"

instance ToJSON IndexStatus where
    toJSON = toJSONText

data ProvisionedThroughput = ProvisionedThroughput
    { _ptReadCapacityUnits  :: Nat
    , _ptWriteCapacityUnits :: Nat
    } deriving (Eq, Ord, Read, Show)

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
    { _ptReadCapacityUnits  = withIso _Nat (const id) p1
    , _ptWriteCapacityUnits = withIso _Nat (const id) p2
    }

-- | The maximum number of strongly consistent reads consumed per second before
-- DynamoDB returns a /ThrottlingException/. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput SpecifyingRead and Write Requirements> in the /Amazon DynamoDB Developer Guide/.
ptReadCapacityUnits :: Lens' ProvisionedThroughput Natural
ptReadCapacityUnits =
    lens _ptReadCapacityUnits (\s a -> s { _ptReadCapacityUnits = a })
        . _Nat

-- | The maximum number of writes consumed per second before DynamoDB returns a /ThrottlingException/. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/.
ptWriteCapacityUnits :: Lens' ProvisionedThroughput Natural
ptWriteCapacityUnits =
    lens _ptWriteCapacityUnits (\s a -> s { _ptWriteCapacityUnits = a })
        . _Nat

instance FromJSON ProvisionedThroughput where
    parseJSON = withObject "ProvisionedThroughput" $ \o -> ProvisionedThroughput
        <$> o .:  "ReadCapacityUnits"
        <*> o .:  "WriteCapacityUnits"

instance ToJSON ProvisionedThroughput where
    toJSON ProvisionedThroughput{..} = object
        [ "ReadCapacityUnits"  .= _ptReadCapacityUnits
        , "WriteCapacityUnits" .= _ptWriteCapacityUnits
        ]

data TableStatus
    = TSActive   -- ^ ACTIVE
    | TSCreating -- ^ CREATING
    | TSDeleting -- ^ DELETING
    | TSUpdating -- ^ UPDATING
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable TableStatus

instance FromText TableStatus where
    parser = takeLowerText >>= \case
        "active"   -> pure TSActive
        "creating" -> pure TSCreating
        "deleting" -> pure TSDeleting
        "updating" -> pure TSUpdating
        e          -> fail $
            "Failure parsing TableStatus from " ++ show e

instance ToText TableStatus where
    toText = \case
        TSActive   -> "ACTIVE"
        TSCreating -> "CREATING"
        TSDeleting -> "DELETING"
        TSUpdating -> "UPDATING"

instance ToByteString TableStatus
instance ToHeader     TableStatus
instance ToQuery      TableStatus

instance FromJSON TableStatus where
    parseJSON = parseJSONText "TableStatus"

instance ToJSON TableStatus where
    toJSON = toJSONText

data ProjectionType
    = All      -- ^ ALL
    | Include  -- ^ INCLUDE
    | KeysOnly -- ^ KEYS_ONLY
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ProjectionType

instance FromText ProjectionType where
    parser = takeLowerText >>= \case
        "all"       -> pure All
        "include"   -> pure Include
        "keys_only" -> pure KeysOnly
        e           -> fail $
            "Failure parsing ProjectionType from " ++ show e

instance ToText ProjectionType where
    toText = \case
        All      -> "ALL"
        Include  -> "INCLUDE"
        KeysOnly -> "KEYS_ONLY"

instance ToByteString ProjectionType
instance ToHeader     ProjectionType
instance ToQuery      ProjectionType

instance FromJSON ProjectionType where
    parseJSON = parseJSONText "ProjectionType"

instance ToJSON ProjectionType where
    toJSON = toJSONText

data TableDescription = TableDescription
    { _tdAttributeDefinitions   :: List "AttributeDefinitions" AttributeDefinition
    , _tdCreationDateTime       :: POSIX
    , _tdGlobalSecondaryIndexes :: List "GlobalSecondaryIndexes" GlobalSecondaryIndexDescription
    , _tdItemCount              :: Integer
    , _tdKeySchema              :: List1 "KeySchema" KeySchemaElement
    , _tdLocalSecondaryIndexes  :: List "LocalSecondaryIndexes" LocalSecondaryIndexDescription
    , _tdProvisionedThroughput  :: ProvisionedThroughputDescription
    , _tdTableName              :: Text
    , _tdTableSizeBytes         :: Integer
    , _tdTableStatus            :: TableStatus
    } deriving (Eq, Read, Show)

-- | 'TableDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tdAttributeDefinitions' @::@ ['AttributeDefinition']
--
-- * 'tdCreationDateTime' @::@ 'UTCTime'
--
-- * 'tdGlobalSecondaryIndexes' @::@ ['GlobalSecondaryIndexDescription']
--
-- * 'tdItemCount' @::@ 'Integer'
--
-- * 'tdKeySchema' @::@ 'NonEmpty' 'KeySchemaElement'
--
-- * 'tdLocalSecondaryIndexes' @::@ ['LocalSecondaryIndexDescription']
--
-- * 'tdProvisionedThroughput' @::@ 'ProvisionedThroughputDescription'
--
-- * 'tdTableName' @::@ 'Text'
--
-- * 'tdTableSizeBytes' @::@ 'Integer'
--
-- * 'tdTableStatus' @::@ 'TableStatus'
--
tableDescription :: Text -- ^ 'tdTableName'
                 -> NonEmpty KeySchemaElement -- ^ 'tdKeySchema'
                 -> TableStatus -- ^ 'tdTableStatus'
                 -> UTCTime -- ^ 'tdCreationDateTime'
                 -> ProvisionedThroughputDescription -- ^ 'tdProvisionedThroughput'
                 -> Integer -- ^ 'tdTableSizeBytes'
                 -> Integer -- ^ 'tdItemCount'
                 -> TableDescription
tableDescription p1 p2 p3 p4 p5 p6 p7 = TableDescription
    { _tdTableName              = p1
    , _tdKeySchema              = withIso _List1 (const id) p2
    , _tdTableStatus            = p3
    , _tdCreationDateTime       = withIso _Time (const id) p4
    , _tdProvisionedThroughput  = p5
    , _tdTableSizeBytes         = p6
    , _tdItemCount              = p7
    , _tdAttributeDefinitions   = mempty
    , _tdLocalSecondaryIndexes  = mempty
    , _tdGlobalSecondaryIndexes = mempty
    }

-- | An array of /AttributeDefinition/ objects. Each of these objects describes one
-- attribute in the table and index key schema.
--
-- Each /AttributeDefinition/ object in this array is composed of:
--
-- /AttributeName/ - The name of the attribute.
--
-- /AttributeType/ - The data type for the attribute.
--
--
tdAttributeDefinitions :: Lens' TableDescription [AttributeDefinition]
tdAttributeDefinitions =
    lens _tdAttributeDefinitions (\s a -> s { _tdAttributeDefinitions = a })
        . _List

-- | The date and time when the table was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
tdCreationDateTime :: Lens' TableDescription UTCTime
tdCreationDateTime =
    lens _tdCreationDateTime (\s a -> s { _tdCreationDateTime = a })
        . _Time

-- | The global secondary indexes, if any, on the table. Each index is scoped to a
-- given hash key value. Each element is composed of:
--
-- /Backfilling/ - If true, then the index is currently in the backfilling
-- phase. Backfilling occurs only when a new global secondary index is added to
-- the table; it is the process by which DynamoDB populates the new index with
-- data from the table. (This attribute does not appear for indexes that were
-- created during a /CreateTable/ operation.)
--
-- /IndexName/ - The name of the global secondary index.
--
-- /IndexSizeBytes/ - The total size of the global secondary index, in bytes.
-- DynamoDB updates this value approximately every six hours. Recent changes
-- might not be reflected in this value.
--
-- /IndexStatus/ - The current status of the global secondary index:
--
-- /CREATING/ - The index is being created.
--
-- /UPDATING/ - The index is being updated.
--
-- /DELETING/ - The index is being deleted.
--
-- /ACTIVE/ - The index is ready for use.
--
-- /ItemCount/ - The number of items in the global secondary index. DynamoDB
-- updates this value approximately every six hours. Recent changes might not be
-- reflected in this value.
--
-- /KeySchema/ - Specifies the complete index key schema. The attribute names
-- in the key schema must be between 1 and 255 characters (inclusive). The key
-- schema must begin with the same hash key attribute as the table.
--
-- /Projection/ - Specifies attributes that are copied (projected) from the
-- table into the index. These are in addition to the primary key attributes and
-- index key attributes, which are automatically projected. Each attribute
-- specification is composed of:
--
-- /ProjectionType/ - One of the following:
--
-- 'KEYS_ONLY' - Only the index and primary keys are projected into the index.
--
-- 'INCLUDE' - Only the specified table attributes are projected into the
-- index. The list of projected attributes are in /NonKeyAttributes/.
--
-- 'ALL' - All of the table attributes are projected into the index.
--
-- /NonKeyAttributes/ - A list of one or more non-key attribute names that
-- are projected into the secondary index. The total count of attributes
-- provided in /NonKeyAttributes/, summed across all of the secondary indexes,
-- must not exceed 20. If you project the same attribute into two different
-- indexes, this counts as two distinct attributes when determining the total.
--
-- /ProvisionedThroughput/ - The provisioned throughput settings for the
-- global secondary index, consisting of read and write capacity units, along
-- with data about increases and decreases.
--
-- If the table is in the 'DELETING' state, no information about indexes will
-- be returned.
tdGlobalSecondaryIndexes :: Lens' TableDescription [GlobalSecondaryIndexDescription]
tdGlobalSecondaryIndexes =
    lens _tdGlobalSecondaryIndexes
        (\s a -> s { _tdGlobalSecondaryIndexes = a })
            . _List

-- | The number of items in the specified table. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in this
-- value.
tdItemCount :: Lens' TableDescription Integer
tdItemCount = lens _tdItemCount (\s a -> s { _tdItemCount = a })

-- | The primary key structure for the table. Each /KeySchemaElement/ consists of:
--
-- /AttributeName/ - The name of the attribute.
--
-- /KeyType/ - The key type for the attribute. Can be either 'HASH' or 'RANGE'.
--
-- For more information about primary keys, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html#DataModelPrimaryKey Primary Key> in the /AmazonDynamoDB Developer Guide/.
tdKeySchema :: Lens' TableDescription (NonEmpty KeySchemaElement)
tdKeySchema = lens _tdKeySchema (\s a -> s { _tdKeySchema = a }) . _List1

-- | Represents one or more local secondary indexes on the table. Each index is
-- scoped to a given hash key value. Tables with one or more local secondary
-- indexes are subject to an item collection size limit, where the amount of
-- data within a given item collection cannot exceed 10 GB. Each element is
-- composed of:
--
-- /IndexName/ - The name of the local secondary index.
--
-- /KeySchema/ - Specifies the complete index key schema. The attribute names
-- in the key schema must be between 1 and 255 characters (inclusive). The key
-- schema must begin with the same hash key attribute as the table.
--
-- /Projection/ - Specifies attributes that are copied (projected) from the
-- table into the index. These are in addition to the primary key attributes and
-- index key attributes, which are automatically projected. Each attribute
-- specification is composed of:
--
-- /ProjectionType/ - One of the following:
--
-- 'KEYS_ONLY' - Only the index and primary keys are projected into the index.
--
-- 'INCLUDE' - Only the specified table attributes are projected into the
-- index. The list of projected attributes are in /NonKeyAttributes/.
--
-- 'ALL' - All of the table attributes are projected into the index.
--
-- /NonKeyAttributes/ - A list of one or more non-key attribute names that
-- are projected into the secondary index. The total count of attributes
-- provided in /NonKeyAttributes/, summed across all of the secondary indexes,
-- must not exceed 20. If you project the same attribute into two different
-- indexes, this counts as two distinct attributes when determining the total.
--
-- /IndexSizeBytes/ - Represents the total size of the index, in bytes.
-- DynamoDB updates this value approximately every six hours. Recent changes
-- might not be reflected in this value.
--
-- /ItemCount/ - Represents the number of items in the index. DynamoDB updates
-- this value approximately every six hours. Recent changes might not be
-- reflected in this value.
--
-- If the table is in the 'DELETING' state, no information about indexes will
-- be returned.
tdLocalSecondaryIndexes :: Lens' TableDescription [LocalSecondaryIndexDescription]
tdLocalSecondaryIndexes =
    lens _tdLocalSecondaryIndexes (\s a -> s { _tdLocalSecondaryIndexes = a })
        . _List

-- | The provisioned throughput settings for the table, consisting of read and
-- write capacity units, along with data about increases and decreases.
tdProvisionedThroughput :: Lens' TableDescription ProvisionedThroughputDescription
tdProvisionedThroughput =
    lens _tdProvisionedThroughput (\s a -> s { _tdProvisionedThroughput = a })

-- | The name of the table.
tdTableName :: Lens' TableDescription Text
tdTableName = lens _tdTableName (\s a -> s { _tdTableName = a })

-- | The total size of the specified table, in bytes. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in this
-- value.
tdTableSizeBytes :: Lens' TableDescription Integer
tdTableSizeBytes = lens _tdTableSizeBytes (\s a -> s { _tdTableSizeBytes = a })

-- | The current state of the table:
--
-- /CREATING/ - The table is being created.
--
-- /UPDATING/ - The table is being updated.
--
-- /DELETING/ - The table is being deleted.
--
-- /ACTIVE/ - The table is ready for use.
--
--
tdTableStatus :: Lens' TableDescription TableStatus
tdTableStatus = lens _tdTableStatus (\s a -> s { _tdTableStatus = a })

instance FromJSON TableDescription where
    parseJSON = withObject "TableDescription" $ \o -> TableDescription
        <$> o .:? "AttributeDefinitions" .!= mempty
        <*> o .:  "CreationDateTime"
        <*> o .:? "GlobalSecondaryIndexes" .!= mempty
        <*> o .:  "ItemCount"
        <*> o .:  "KeySchema"
        <*> o .:? "LocalSecondaryIndexes" .!= mempty
        <*> o .:  "ProvisionedThroughput"
        <*> o .:  "TableName"
        <*> o .:  "TableSizeBytes"
        <*> o .:  "TableStatus"

instance ToJSON TableDescription where
    toJSON TableDescription{..} = object
        [ "AttributeDefinitions"   .= _tdAttributeDefinitions
        , "TableName"              .= _tdTableName
        , "KeySchema"              .= _tdKeySchema
        , "TableStatus"            .= _tdTableStatus
        , "CreationDateTime"       .= _tdCreationDateTime
        , "ProvisionedThroughput"  .= _tdProvisionedThroughput
        , "TableSizeBytes"         .= _tdTableSizeBytes
        , "ItemCount"              .= _tdItemCount
        , "LocalSecondaryIndexes"  .= _tdLocalSecondaryIndexes
        , "GlobalSecondaryIndexes" .= _tdGlobalSecondaryIndexes
        ]

data KeysAndAttributes = KeysAndAttributes
    { _kaaAttributesToGet          :: List1 "AttributesToGet" Text
    , _kaaConsistentRead           :: Maybe Bool
    , _kaaExpressionAttributeNames :: Map Text Text
    , _kaaKeys                     :: List1 "Keys" (Map Text AttributeValue)
    , _kaaProjectionExpression     :: Maybe Text
    } deriving (Eq, Read, Show)

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

-- | One or more attributes to retrieve from the table or index. If no attribute
-- names are specified then all attributes will be returned. If any of the
-- specified attributes are not found, they will not appear in the result.
kaaAttributesToGet :: Lens' KeysAndAttributes (NonEmpty Text)
kaaAttributesToGet =
    lens _kaaAttributesToGet (\s a -> s { _kaaAttributesToGet = a })
        . _List1

-- | The consistency of a read operation. If set to 'true', then a strongly
-- consistent read is used; otherwise, an eventually consistent read is used.
kaaConsistentRead :: Lens' KeysAndAttributes (Maybe Bool)
kaaConsistentRead =
    lens _kaaConsistentRead (\s a -> s { _kaaConsistentRead = a })

-- | One or more substitution tokens for attribute names in an expression. The
-- following are some use cases for using /ExpressionAttributeNames/:
--
-- To access an attribute whose name conflicts with a DynamoDB reserved word.
--
-- To create a placeholder for repeating occurrences of an attribute name in
-- an expression.
--
-- To prevent special characters in an attribute name from being
-- misinterpreted in an expression.
--
-- Use the # character in an expression to dereference an attribute name. For
-- example, consider the following attribute name:
--
-- 'Percentile'
--
-- The name of this attribute conflicts with a reserved word, so it cannot be
-- used directly in an expression. (For the complete list of reserved words, go
-- to <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/). To work around
-- this, you could specify the following for /ExpressionAttributeNames/:
--
-- '{"#P":"Percentile"}'
--
-- You could then use this substitution in an expression, as in this example:
--
-- '#P = :val'
--
-- Tokens that begin with the : character are /expression attribute values/,
-- which are placeholders for the actual value at runtime.
--
-- For more information on expression attribute names, go to <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing ItemAttributes> in the /Amazon DynamoDB Developer Guide/.
kaaExpressionAttributeNames :: Lens' KeysAndAttributes (HashMap Text Text)
kaaExpressionAttributeNames =
    lens _kaaExpressionAttributeNames
        (\s a -> s { _kaaExpressionAttributeNames = a })
            . _Map

-- | The primary key attribute values that define the items and the attributes
-- associated with the items.
kaaKeys :: Lens' KeysAndAttributes (NonEmpty (HashMap Text AttributeValue))
kaaKeys = lens _kaaKeys (\s a -> s { _kaaKeys = a }) . _List1

-- | A string that identifies one or more attributes to retrieve from the table.
-- These attributes can include scalars, sets, or elements of a JSON document.
-- The attributes in the /ProjectionExpression/ must be separated by commas.
--
-- If no attribute names are specified, then all attributes will be returned.
-- If any of the requested attributes are not found, they will not appear in the
-- result.
--
-- For more information, go to <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDBDeveloper Guide/.
kaaProjectionExpression :: Lens' KeysAndAttributes (Maybe Text)
kaaProjectionExpression =
    lens _kaaProjectionExpression (\s a -> s { _kaaProjectionExpression = a })

instance FromJSON KeysAndAttributes where
    parseJSON = withObject "KeysAndAttributes" $ \o -> KeysAndAttributes
        <$> o .:  "AttributesToGet"
        <*> o .:? "ConsistentRead"
        <*> o .:? "ExpressionAttributeNames" .!= mempty
        <*> o .:  "Keys"
        <*> o .:? "ProjectionExpression"

instance ToJSON KeysAndAttributes where
    toJSON KeysAndAttributes{..} = object
        [ "Keys"                     .= _kaaKeys
        , "AttributesToGet"          .= _kaaAttributesToGet
        , "ConsistentRead"           .= _kaaConsistentRead
        , "ProjectionExpression"     .= _kaaProjectionExpression
        , "ExpressionAttributeNames" .= _kaaExpressionAttributeNames
        ]

data ReturnConsumedCapacity
    = Indexes -- ^ INDEXES
    | None    -- ^ NONE
    | Total   -- ^ TOTAL
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ReturnConsumedCapacity

instance FromText ReturnConsumedCapacity where
    parser = takeLowerText >>= \case
        "indexes" -> pure Indexes
        "none"    -> pure None
        "total"   -> pure Total
        e         -> fail $
            "Failure parsing ReturnConsumedCapacity from " ++ show e

instance ToText ReturnConsumedCapacity where
    toText = \case
        Indexes -> "INDEXES"
        None    -> "NONE"
        Total   -> "TOTAL"

instance ToByteString ReturnConsumedCapacity
instance ToHeader     ReturnConsumedCapacity
instance ToQuery      ReturnConsumedCapacity

instance FromJSON ReturnConsumedCapacity where
    parseJSON = parseJSONText "ReturnConsumedCapacity"

instance ToJSON ReturnConsumedCapacity where
    toJSON = toJSONText

data ReturnItemCollectionMetrics
    = RICMNone -- ^ NONE
    | RICMSize -- ^ SIZE
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ReturnItemCollectionMetrics

instance FromText ReturnItemCollectionMetrics where
    parser = takeLowerText >>= \case
        "none" -> pure RICMNone
        "size" -> pure RICMSize
        e      -> fail $
            "Failure parsing ReturnItemCollectionMetrics from " ++ show e

instance ToText ReturnItemCollectionMetrics where
    toText = \case
        RICMNone -> "NONE"
        RICMSize -> "SIZE"

instance ToByteString ReturnItemCollectionMetrics
instance ToHeader     ReturnItemCollectionMetrics
instance ToQuery      ReturnItemCollectionMetrics

instance FromJSON ReturnItemCollectionMetrics where
    parseJSON = parseJSONText "ReturnItemCollectionMetrics"

instance ToJSON ReturnItemCollectionMetrics where
    toJSON = toJSONText

data AttributeValueUpdate = AttributeValueUpdate
    { _avuAction :: Maybe AttributeAction
    , _avuValue  :: Maybe AttributeValue
    } deriving (Eq, Read, Show)

-- | 'AttributeValueUpdate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avuAction' @::@ 'Maybe' 'AttributeAction'
--
-- * 'avuValue' @::@ 'Maybe' 'AttributeValue'
--
attributeValueUpdate :: AttributeValueUpdate
attributeValueUpdate = AttributeValueUpdate
    { _avuValue  = Nothing
    , _avuAction = Nothing
    }

-- | Specifies how to perform the update. Valid values are 'PUT' (default), 'DELETE',
-- and 'ADD'. The behavior depends on whether the specified primary key already
-- exists in the table.
--
-- If an item with the specified /Key/ is found in the table:
--
-- 'PUT' - Adds the specified attribute to the item. If the attribute already
-- exists, it is replaced by the new value.
--
-- 'DELETE' - If no value is specified, the attribute and its value are removed
-- from the item. The data type of the specified value must match the existing
-- value's data type.
--
-- If a /set/ of values is specified, then those values are subtracted from the
-- old set. For example, if the attribute value was the set '[a,b,c]' and the /DELETE/ action specified '[a,c]', then the final attribute value would be '[b]'.
-- Specifying an empty set is an error.
--
-- 'ADD' - If the attribute does not already exist, then the attribute and its
-- values are added to the item. If the attribute does exist, then the behavior
-- of 'ADD' depends on the data type of the attribute:
--
-- If the existing attribute is a number, and if /Value/ is also a number, then
-- the /Value/ is mathematically added to the existing attribute. If /Value/ is a
-- negative number, then it is subtracted from the existing attribute.
--
-- If you use 'ADD' to increment or decrement a number value for an item that
-- doesn't exist before the update, DynamoDB uses 0 as the initial value.
--
-- In addition, if you use 'ADD' to update an existing item, and intend to
-- increment or decrement an attribute value which does not yet exist, DynamoDB
-- uses '0' as the initial value. For example, suppose that the item you want to
-- update does not yet have an attribute named /itemcount/, but you decide to 'ADD'
-- the number '3' to this attribute anyway, even though it currently does not
-- exist. DynamoDB will create the /itemcount/ attribute, set its initial value to '0', and finally add '3' to it. The result will be a new /itemcount/ attribute in
-- the item, with a value of '3'.
--
-- If the existing data type is a set, and if the /Value/ is also a set, then
-- the /Value/ is added to the existing set. (This is a /set/ operation, not
-- mathematical addition.) For example, if the attribute value was the set '[1,2]', and the
-- 'ADD' action specified '[3]', then the final attribute value would be '[1,2,3]'. An
-- error occurs if an Add action is specified for a set attribute and the
-- attribute type specified does not match the existing set type.
--
-- Both sets must have the same primitive data type. For example, if the
-- existing data type is a set of strings, the /Value/ must also be a set of
-- strings. The same holds true for number sets and binary sets.
--
-- This action is only valid for an existing attribute whose data type is
-- number or is a set. Do not use 'ADD' for any other data types.
--
-- If no item with the specified /Key/ is found:
--
-- 'PUT' - DynamoDB creates a new item with the specified primary key, and then
-- adds the attribute.
--
-- 'DELETE' - Nothing happens; there is no attribute to delete.
--
-- 'ADD' - DynamoDB creates an item with the supplied primary key and number
-- (or set of numbers) for the attribute value. The only data types allowed are
-- number and number set; no other data types can be specified.
--
--
avuAction :: Lens' AttributeValueUpdate (Maybe AttributeAction)
avuAction = lens _avuAction (\s a -> s { _avuAction = a })

avuValue :: Lens' AttributeValueUpdate (Maybe AttributeValue)
avuValue = lens _avuValue (\s a -> s { _avuValue = a })

instance FromJSON AttributeValueUpdate where
    parseJSON = withObject "AttributeValueUpdate" $ \o -> AttributeValueUpdate
        <$> o .:? "Action"
        <*> o .:? "Value"

instance ToJSON AttributeValueUpdate where
    toJSON AttributeValueUpdate{..} = object
        [ "Value"  .= _avuValue
        , "Action" .= _avuAction
        ]

data ExpectedAttributeValue = ExpectedAttributeValue
    { _eavAttributeValueList :: List "AttributeValueList" AttributeValue
    , _eavComparisonOperator :: Maybe ComparisonOperator
    , _eavExists             :: Maybe Bool
    , _eavValue              :: Maybe AttributeValue
    } deriving (Eq, Read, Show)

-- | 'ExpectedAttributeValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eavAttributeValueList' @::@ ['AttributeValue']
--
-- * 'eavComparisonOperator' @::@ 'Maybe' 'ComparisonOperator'
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

-- | One or more values to evaluate against the supplied attribute. The number of
-- values in the list depends on the /ComparisonOperator/ being used.
--
-- For type Number, value comparisons are numeric.
--
-- String value comparisons for greater than, equals, or less than are based on
-- ASCII character code values. For example, 'a' is greater than 'A', and 'a' is
-- greater than 'B'. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
--
-- For Binary, DynamoDB treats each byte of the binary data as unsigned when it
-- compares binary values.
--
-- For information on specifying data types in JSON, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataFormat.html JSON Data Format> in
-- the /Amazon DynamoDB Developer Guide/.
eavAttributeValueList :: Lens' ExpectedAttributeValue [AttributeValue]
eavAttributeValueList =
    lens _eavAttributeValueList (\s a -> s { _eavAttributeValueList = a })
        . _List

-- | A comparator for evaluating attributes in the /AttributeValueList/. For
-- example, equals, greater than, less than, etc.
--
-- The following comparison operators are available:
--
-- 'EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS |BEGINS_WITH | IN | BETWEEN'
--
-- The following are descriptions of each comparison operator.
--
-- 'EQ' : Equal. 'EQ' is supported for all datatypes, including lists and maps.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, Binary, String Set, Number Set, or Binary Set. If an item
-- contains an /AttributeValue/ element of a different type than the one provided
-- in the request, the value does not match. For example, '{"S":"6"}' does not
-- equal '{"N":"6"}'. Also, '{"N":"6"}' does not equal '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'NE' : Not equal. 'NE' is supported for all datatypes, including lists and
-- maps.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ of type String,
-- Number, Binary, String Set, Number Set, or Binary Set. If an item contains an /AttributeValue/ of a different type than the one provided in the request, the
-- value does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not equal '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'LE' : Less than or equal.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'LT' : Less than.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ of type String,
-- Number, or Binary (not a set type). If an item contains an /AttributeValue/
-- element of a different type than the one provided in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'GE' : Greater than or equal.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'GT' : Greater than.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'NOT_NULL' : The attribute exists. 'NOT_NULL' is supported for all datatypes,
-- including lists and maps.
--
-- This operator tests for the existence of an attribute, not its data type. If
-- the data type of attribute "'a'" is null, and you evaluate it using 'NOT_NULL',
-- the result is a Boolean /true/. This result is because the attribute "'a'"
-- exists; its data type is not relevant to the 'NOT_NULL' comparison operator.
--
-- 'NULL' : The attribute does not exist. 'NULL' is supported for all datatypes,
-- including lists and maps.
--
-- This operator tests for the nonexistence of an attribute, not its data type.
-- If the data type of attribute "'a'" is null, and you evaluate it using 'NULL',
-- the result is a Boolean /false/. This is because the attribute "'a'" exists; its
-- data type is not relevant to the 'NULL' comparison operator.
--
-- 'CONTAINS' : Checks for a subsequence, or value in a set.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If the target attribute of the
-- comparison is of type String, then the operator checks for a substring match.
-- If the target attribute of the comparison is of type Binary, then the
-- operator looks for a subsequence of the target that matches the input. If the
-- target attribute of the comparison is a set ("'SS'", "'NS'", or "'BS'"), then the
-- operator evaluates to true if it finds an exact match with any member of the
-- set.
--
-- CONTAINS is supported for lists: When evaluating "'a CONTAINS b'", "'a'" can be
-- a list; however, "'b'" cannot be a set, a map, or a list.
--
-- 'NOT_CONTAINS' : Checks for absence of a subsequence, or absence of a value
-- in a set.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If the target attribute of the
-- comparison is a String, then the operator checks for the absence of a
-- substring match. If the target attribute of the comparison is Binary, then
-- the operator checks for the absence of a subsequence of the target that
-- matches the input. If the target attribute of the comparison is a set ("'SS'", "'NS'", or "'BS'"), then the operator evaluates to true if it /does not/ find an
-- exact match with any member of the set.
--
-- NOT_CONTAINS is supported for lists: When evaluating "'a NOT CONTAINS b'", "'a'"
-- can be a list; however, "'b'" cannot be a set, a map, or a list.
--
-- 'BEGINS_WITH' : Checks for a prefix.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ of type String or
-- Binary (not a Number or a set type). The target attribute of the comparison
-- must be of type String or Binary (not a Number or a set type).
--
--
--
-- 'IN' : Checks for matching elements within two sets.
--
-- /AttributeValueList/ can contain one or more /AttributeValue/ elements of type
-- String, Number, or Binary (not a set type). These attributes are compared
-- against an existing set type attribute of an item. If any elements of the
-- input set are present in the item attribute, the expression evaluates to true.
--
-- 'BETWEEN' : Greater than or equal to the first value, and less than or equal
-- to the second value.
--
-- /AttributeValueList/ must contain two /AttributeValue/ elements of the same
-- type, either String, Number, or Binary (not a set type). A target attribute
-- matches if the target value is greater than, or equal to, the first element
-- and less than, or equal to, the second element. If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value
-- does not match. For example, '{"S":"6"}' does not compare to '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'
--
--
eavComparisonOperator :: Lens' ExpectedAttributeValue (Maybe ComparisonOperator)
eavComparisonOperator =
    lens _eavComparisonOperator (\s a -> s { _eavComparisonOperator = a })

-- | Causes DynamoDB to evaluate the value before attempting a conditional
-- operation:
--
-- If /Exists/ is 'true', DynamoDB will check to see if that attribute value
-- already exists in the table. If it is found, then the operation succeeds. If
-- it is not found, the operation fails with a /ConditionalCheckFailedException/.
--
-- If /Exists/ is 'false', DynamoDB assumes that the attribute value does not
-- exist in the table. If in fact the value does not exist, then the assumption
-- is valid and the operation succeeds. If the value is found, despite the
-- assumption that it does not exist, the operation fails with a /ConditionalCheckFailedException/.
--
-- The default setting for /Exists/ is 'true'. If you supply a /Value/ all by
-- itself, DynamoDB assumes the attribute exists: You don't have to set /Exists/
-- to 'true', because it is implied.
--
-- DynamoDB returns a /ValidationException/ if:
--
-- /Exists/ is 'true' but there is no /Value/ to check. (You expect a value to
-- exist, but don't specify what that value is.)
--
-- /Exists/ is 'false' but you also provide a /Value/. (You cannot expect an
-- attribute to have a value, while also expecting it not to exist.)
--
--
eavExists :: Lens' ExpectedAttributeValue (Maybe Bool)
eavExists = lens _eavExists (\s a -> s { _eavExists = a })

eavValue :: Lens' ExpectedAttributeValue (Maybe AttributeValue)
eavValue = lens _eavValue (\s a -> s { _eavValue = a })

instance FromJSON ExpectedAttributeValue where
    parseJSON = withObject "ExpectedAttributeValue" $ \o -> ExpectedAttributeValue
        <$> o .:? "AttributeValueList" .!= mempty
        <*> o .:? "ComparisonOperator"
        <*> o .:? "Exists"
        <*> o .:? "Value"

instance ToJSON ExpectedAttributeValue where
    toJSON ExpectedAttributeValue{..} = object
        [ "Value"              .= _eavValue
        , "Exists"             .= _eavExists
        , "ComparisonOperator" .= _eavComparisonOperator
        , "AttributeValueList" .= _eavAttributeValueList
        ]

data AttributeDefinition = AttributeDefinition
    { _adAttributeName :: Text
    , _adAttributeType :: ScalarAttributeType
    } deriving (Eq, Read, Show)

-- | 'AttributeDefinition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adAttributeName' @::@ 'Text'
--
-- * 'adAttributeType' @::@ 'ScalarAttributeType'
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

instance FromJSON AttributeDefinition where
    parseJSON = withObject "AttributeDefinition" $ \o -> AttributeDefinition
        <$> o .:  "AttributeName"
        <*> o .:  "AttributeType"

instance ToJSON AttributeDefinition where
    toJSON AttributeDefinition{..} = object
        [ "AttributeName" .= _adAttributeName
        , "AttributeType" .= _adAttributeType
        ]

data ComparisonOperator
    = BeginsWith  -- ^ BEGINS_WITH
    | Between     -- ^ BETWEEN
    | Contains    -- ^ CONTAINS
    | Eq          -- ^ EQ
    | Ge          -- ^ GE
    | Gt          -- ^ GT
    | In'         -- ^ IN
    | Le          -- ^ LE
    | Lt          -- ^ LT
    | Ne          -- ^ NE
    | NotContains -- ^ NOT_CONTAINS
    | NotNull     -- ^ NOT_NULL
    | Null        -- ^ NULL
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ComparisonOperator

instance FromText ComparisonOperator where
    parser = takeLowerText >>= \case
        "begins_with"  -> pure BeginsWith
        "between"      -> pure Between
        "contains"     -> pure Contains
        "eq"           -> pure Eq
        "ge"           -> pure Ge
        "gt"           -> pure Gt
        "in"           -> pure In'
        "le"           -> pure Le
        "lt"           -> pure Lt
        "ne"           -> pure Ne
        "not_contains" -> pure NotContains
        "not_null"     -> pure NotNull
        "null"         -> pure Null
        e              -> fail $
            "Failure parsing ComparisonOperator from " ++ show e

instance ToText ComparisonOperator where
    toText = \case
        BeginsWith  -> "BEGINS_WITH"
        Between     -> "BETWEEN"
        Contains    -> "CONTAINS"
        Eq          -> "EQ"
        Ge          -> "GE"
        Gt          -> "GT"
        In'         -> "IN"
        Le          -> "LE"
        Lt          -> "LT"
        Ne          -> "NE"
        NotContains -> "NOT_CONTAINS"
        NotNull     -> "NOT_NULL"
        Null        -> "NULL"

instance ToByteString ComparisonOperator
instance ToHeader     ComparisonOperator
instance ToQuery      ComparisonOperator

instance FromJSON ComparisonOperator where
    parseJSON = parseJSONText "ComparisonOperator"

instance ToJSON ComparisonOperator where
    toJSON = toJSONText

data ReturnValue
    = RVAllNew     -- ^ ALL_NEW
    | RVAllOld     -- ^ ALL_OLD
    | RVNone       -- ^ NONE
    | RVUpdatedNew -- ^ UPDATED_NEW
    | RVUpdatedOld -- ^ UPDATED_OLD
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ReturnValue

instance FromText ReturnValue where
    parser = takeLowerText >>= \case
        "all_new"     -> pure RVAllNew
        "all_old"     -> pure RVAllOld
        "none"        -> pure RVNone
        "updated_new" -> pure RVUpdatedNew
        "updated_old" -> pure RVUpdatedOld
        e             -> fail $
            "Failure parsing ReturnValue from " ++ show e

instance ToText ReturnValue where
    toText = \case
        RVAllNew     -> "ALL_NEW"
        RVAllOld     -> "ALL_OLD"
        RVNone       -> "NONE"
        RVUpdatedNew -> "UPDATED_NEW"
        RVUpdatedOld -> "UPDATED_OLD"

instance ToByteString ReturnValue
instance ToHeader     ReturnValue
instance ToQuery      ReturnValue

instance FromJSON ReturnValue where
    parseJSON = parseJSONText "ReturnValue"

instance ToJSON ReturnValue where
    toJSON = toJSONText

data LocalSecondaryIndex = LocalSecondaryIndex
    { _lsiIndexName  :: Text
    , _lsiKeySchema  :: List1 "KeySchema" KeySchemaElement
    , _lsiProjection :: Projection
    } deriving (Eq, Read, Show)

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

-- | The complete key schema for the local secondary index, consisting of one or
-- more pairs of attribute names and key types ('HASH' or 'RANGE').
lsiKeySchema :: Lens' LocalSecondaryIndex (NonEmpty KeySchemaElement)
lsiKeySchema = lens _lsiKeySchema (\s a -> s { _lsiKeySchema = a }) . _List1

lsiProjection :: Lens' LocalSecondaryIndex Projection
lsiProjection = lens _lsiProjection (\s a -> s { _lsiProjection = a })

instance FromJSON LocalSecondaryIndex where
    parseJSON = withObject "LocalSecondaryIndex" $ \o -> LocalSecondaryIndex
        <$> o .:  "IndexName"
        <*> o .:  "KeySchema"
        <*> o .:  "Projection"

instance ToJSON LocalSecondaryIndex where
    toJSON LocalSecondaryIndex{..} = object
        [ "IndexName"  .= _lsiIndexName
        , "KeySchema"  .= _lsiKeySchema
        , "Projection" .= _lsiProjection
        ]

data GlobalSecondaryIndexDescription = GlobalSecondaryIndexDescription
    { _gsidBackfilling           :: Maybe Bool
    , _gsidIndexName             :: Maybe Text
    , _gsidIndexSizeBytes        :: Maybe Integer
    , _gsidIndexStatus           :: Maybe IndexStatus
    , _gsidItemCount             :: Maybe Integer
    , _gsidKeySchema             :: List1 "KeySchema" KeySchemaElement
    , _gsidProjection            :: Maybe Projection
    , _gsidProvisionedThroughput :: Maybe ProvisionedThroughputDescription
    } deriving (Eq, Read, Show)

-- | 'GlobalSecondaryIndexDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsidBackfilling' @::@ 'Maybe' 'Bool'
--
-- * 'gsidIndexName' @::@ 'Maybe' 'Text'
--
-- * 'gsidIndexSizeBytes' @::@ 'Maybe' 'Integer'
--
-- * 'gsidIndexStatus' @::@ 'Maybe' 'IndexStatus'
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
    , _gsidBackfilling           = Nothing
    , _gsidProvisionedThroughput = Nothing
    , _gsidIndexSizeBytes        = Nothing
    , _gsidItemCount             = Nothing
    }

-- | Indicates whether the index is currently backfilling. /Backfilling/ is the
-- process of reading items from the table and determining whether they can be
-- added to the index. (Not all items will qualify: For example, a hash key
-- attribute cannot have any duplicates.) If an item can be added to the index,
-- DynamoDB will do so. After all items have been processed, the backfilling
-- operation is complete and /Backfilling/ is false.
--
-- For indexes that were created during a /CreateTable/ operation, the /Backfilling/
-- attribute does not appear in the /DescribeTable/ output.
--
gsidBackfilling :: Lens' GlobalSecondaryIndexDescription (Maybe Bool)
gsidBackfilling = lens _gsidBackfilling (\s a -> s { _gsidBackfilling = a })

-- | The name of the global secondary index.
gsidIndexName :: Lens' GlobalSecondaryIndexDescription (Maybe Text)
gsidIndexName = lens _gsidIndexName (\s a -> s { _gsidIndexName = a })

-- | The total size of the specified index, in bytes. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in this
-- value.
gsidIndexSizeBytes :: Lens' GlobalSecondaryIndexDescription (Maybe Integer)
gsidIndexSizeBytes =
    lens _gsidIndexSizeBytes (\s a -> s { _gsidIndexSizeBytes = a })

-- | The current state of the global secondary index:
--
-- /CREATING/ - The index is being created.
--
-- /UPDATING/ - The index is being updated.
--
-- /DELETING/ - The index is being deleted.
--
-- /ACTIVE/ - The index is ready for use.
--
--
gsidIndexStatus :: Lens' GlobalSecondaryIndexDescription (Maybe IndexStatus)
gsidIndexStatus = lens _gsidIndexStatus (\s a -> s { _gsidIndexStatus = a })

-- | The number of items in the specified index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in this
-- value.
gsidItemCount :: Lens' GlobalSecondaryIndexDescription (Maybe Integer)
gsidItemCount = lens _gsidItemCount (\s a -> s { _gsidItemCount = a })

-- | The complete key schema for the global secondary index, consisting of one or
-- more pairs of attribute names and key types ('HASH' or 'RANGE').
gsidKeySchema :: Lens' GlobalSecondaryIndexDescription (NonEmpty KeySchemaElement)
gsidKeySchema = lens _gsidKeySchema (\s a -> s { _gsidKeySchema = a }) . _List1

gsidProjection :: Lens' GlobalSecondaryIndexDescription (Maybe Projection)
gsidProjection = lens _gsidProjection (\s a -> s { _gsidProjection = a })

gsidProvisionedThroughput :: Lens' GlobalSecondaryIndexDescription (Maybe ProvisionedThroughputDescription)
gsidProvisionedThroughput =
    lens _gsidProvisionedThroughput
        (\s a -> s { _gsidProvisionedThroughput = a })

instance FromJSON GlobalSecondaryIndexDescription where
    parseJSON = withObject "GlobalSecondaryIndexDescription" $ \o -> GlobalSecondaryIndexDescription
        <$> o .:? "Backfilling"
        <*> o .:? "IndexName"
        <*> o .:? "IndexSizeBytes"
        <*> o .:? "IndexStatus"
        <*> o .:? "ItemCount"
        <*> o .:  "KeySchema"
        <*> o .:? "Projection"
        <*> o .:? "ProvisionedThroughput"

instance ToJSON GlobalSecondaryIndexDescription where
    toJSON GlobalSecondaryIndexDescription{..} = object
        [ "IndexName"             .= _gsidIndexName
        , "KeySchema"             .= _gsidKeySchema
        , "Projection"            .= _gsidProjection
        , "IndexStatus"           .= _gsidIndexStatus
        , "Backfilling"           .= _gsidBackfilling
        , "ProvisionedThroughput" .= _gsidProvisionedThroughput
        , "IndexSizeBytes"        .= _gsidIndexSizeBytes
        , "ItemCount"             .= _gsidItemCount
        ]

data ItemCollectionMetrics = ItemCollectionMetrics
    { _icmItemCollectionKey   :: Map Text AttributeValue
    , _icmSizeEstimateRangeGB :: List "SizeEstimateRangeGB" Double
    } deriving (Eq, Read, Show)

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

-- | The hash key value of the item collection. This value is the same as the hash
-- key of the item.
icmItemCollectionKey :: Lens' ItemCollectionMetrics (HashMap Text AttributeValue)
icmItemCollectionKey =
    lens _icmItemCollectionKey (\s a -> s { _icmItemCollectionKey = a })
        . _Map

-- | An estimate of item collection size, in gigabytes. This value is a
-- two-element array containing a lower bound and an upper bound for the
-- estimate. The estimate includes the size of all the items in the table, plus
-- the size of all attributes projected into all of the local secondary indexes
-- on that table. Use this estimate to measure whether a local secondary index
-- is approaching its size limit.
--
-- The estimate is subject to change over time; therefore, do not rely on the
-- precision or accuracy of the estimate.
icmSizeEstimateRangeGB :: Lens' ItemCollectionMetrics [Double]
icmSizeEstimateRangeGB =
    lens _icmSizeEstimateRangeGB (\s a -> s { _icmSizeEstimateRangeGB = a })
        . _List

instance FromJSON ItemCollectionMetrics where
    parseJSON = withObject "ItemCollectionMetrics" $ \o -> ItemCollectionMetrics
        <$> o .:? "ItemCollectionKey" .!= mempty
        <*> o .:? "SizeEstimateRangeGB" .!= mempty

instance ToJSON ItemCollectionMetrics where
    toJSON ItemCollectionMetrics{..} = object
        [ "ItemCollectionKey"   .= _icmItemCollectionKey
        , "SizeEstimateRangeGB" .= _icmSizeEstimateRangeGB
        ]

newtype Capacity = Capacity
    { _cCapacityUnits :: Maybe Double
    } deriving (Eq, Ord, Read, Show)

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

instance FromJSON Capacity where
    parseJSON = withObject "Capacity" $ \o -> Capacity
        <$> o .:? "CapacityUnits"

instance ToJSON Capacity where
    toJSON Capacity{..} = object
        [ "CapacityUnits" .= _cCapacityUnits
        ]

data ConsumedCapacity = ConsumedCapacity
    { _ccCapacityUnits          :: Maybe Double
    , _ccGlobalSecondaryIndexes :: Map Text Capacity
    , _ccLocalSecondaryIndexes  :: Map Text Capacity
    , _ccTable                  :: Maybe Capacity
    , _ccTableName              :: Maybe Text
    } deriving (Eq, Read, Show)

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

instance FromJSON ConsumedCapacity where
    parseJSON = withObject "ConsumedCapacity" $ \o -> ConsumedCapacity
        <$> o .:? "CapacityUnits"
        <*> o .:? "GlobalSecondaryIndexes" .!= mempty
        <*> o .:? "LocalSecondaryIndexes" .!= mempty
        <*> o .:? "Table"
        <*> o .:? "TableName"

instance ToJSON ConsumedCapacity where
    toJSON ConsumedCapacity{..} = object
        [ "TableName"              .= _ccTableName
        , "CapacityUnits"          .= _ccCapacityUnits
        , "Table"                  .= _ccTable
        , "LocalSecondaryIndexes"  .= _ccLocalSecondaryIndexes
        , "GlobalSecondaryIndexes" .= _ccGlobalSecondaryIndexes
        ]

data GlobalSecondaryIndex = GlobalSecondaryIndex
    { _gsiIndexName             :: Text
    , _gsiKeySchema             :: List1 "KeySchema" KeySchemaElement
    , _gsiProjection            :: Projection
    , _gsiProvisionedThroughput :: ProvisionedThroughput
    } deriving (Eq, Read, Show)

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

-- | The complete key schema for a global secondary index, which consists of one
-- or more pairs of attribute names and key types ('HASH' or 'RANGE').
gsiKeySchema :: Lens' GlobalSecondaryIndex (NonEmpty KeySchemaElement)
gsiKeySchema = lens _gsiKeySchema (\s a -> s { _gsiKeySchema = a }) . _List1

gsiProjection :: Lens' GlobalSecondaryIndex Projection
gsiProjection = lens _gsiProjection (\s a -> s { _gsiProjection = a })

gsiProvisionedThroughput :: Lens' GlobalSecondaryIndex ProvisionedThroughput
gsiProvisionedThroughput =
    lens _gsiProvisionedThroughput
        (\s a -> s { _gsiProvisionedThroughput = a })

instance FromJSON GlobalSecondaryIndex where
    parseJSON = withObject "GlobalSecondaryIndex" $ \o -> GlobalSecondaryIndex
        <$> o .:  "IndexName"
        <*> o .:  "KeySchema"
        <*> o .:  "Projection"
        <*> o .:  "ProvisionedThroughput"

instance ToJSON GlobalSecondaryIndex where
    toJSON GlobalSecondaryIndex{..} = object
        [ "IndexName"             .= _gsiIndexName
        , "KeySchema"             .= _gsiKeySchema
        , "Projection"            .= _gsiProjection
        , "ProvisionedThroughput" .= _gsiProvisionedThroughput
        ]

data LocalSecondaryIndexDescription = LocalSecondaryIndexDescription
    { _lsidIndexName      :: Maybe Text
    , _lsidIndexSizeBytes :: Maybe Integer
    , _lsidItemCount      :: Maybe Integer
    , _lsidKeySchema      :: List1 "KeySchema" KeySchemaElement
    , _lsidProjection     :: Maybe Projection
    } deriving (Eq, Read, Show)

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

-- | The total size of the specified index, in bytes. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in this
-- value.
lsidIndexSizeBytes :: Lens' LocalSecondaryIndexDescription (Maybe Integer)
lsidIndexSizeBytes =
    lens _lsidIndexSizeBytes (\s a -> s { _lsidIndexSizeBytes = a })

-- | The number of items in the specified index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in this
-- value.
lsidItemCount :: Lens' LocalSecondaryIndexDescription (Maybe Integer)
lsidItemCount = lens _lsidItemCount (\s a -> s { _lsidItemCount = a })

-- | The complete index key schema, which consists of one or more pairs of
-- attribute names and key types ('HASH' or 'RANGE').
lsidKeySchema :: Lens' LocalSecondaryIndexDescription (NonEmpty KeySchemaElement)
lsidKeySchema = lens _lsidKeySchema (\s a -> s { _lsidKeySchema = a }) . _List1

lsidProjection :: Lens' LocalSecondaryIndexDescription (Maybe Projection)
lsidProjection = lens _lsidProjection (\s a -> s { _lsidProjection = a })

instance FromJSON LocalSecondaryIndexDescription where
    parseJSON = withObject "LocalSecondaryIndexDescription" $ \o -> LocalSecondaryIndexDescription
        <$> o .:? "IndexName"
        <*> o .:? "IndexSizeBytes"
        <*> o .:? "ItemCount"
        <*> o .:  "KeySchema"
        <*> o .:? "Projection"

instance ToJSON LocalSecondaryIndexDescription where
    toJSON LocalSecondaryIndexDescription{..} = object
        [ "IndexName"      .= _lsidIndexName
        , "KeySchema"      .= _lsidKeySchema
        , "Projection"     .= _lsidProjection
        , "IndexSizeBytes" .= _lsidIndexSizeBytes
        , "ItemCount"      .= _lsidItemCount
        ]

data AttributeAction
    = Add     -- ^ ADD
    | Delete' -- ^ DELETE
    | Put     -- ^ PUT
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable AttributeAction

instance FromText AttributeAction where
    parser = takeLowerText >>= \case
        "add"    -> pure Add
        "delete" -> pure Delete'
        "put"    -> pure Put
        e        -> fail $
            "Failure parsing AttributeAction from " ++ show e

instance ToText AttributeAction where
    toText = \case
        Add     -> "ADD"
        Delete' -> "DELETE"
        Put     -> "PUT"

instance ToByteString AttributeAction
instance ToHeader     AttributeAction
instance ToQuery      AttributeAction

instance FromJSON AttributeAction where
    parseJSON = parseJSONText "AttributeAction"

instance ToJSON AttributeAction where
    toJSON = toJSONText

data ScalarAttributeType
    = B -- ^ B
    | N -- ^ N
    | S -- ^ S
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ScalarAttributeType

instance FromText ScalarAttributeType where
    parser = takeLowerText >>= \case
        "b" -> pure B
        "n" -> pure N
        "s" -> pure S
        e   -> fail $
            "Failure parsing ScalarAttributeType from " ++ show e

instance ToText ScalarAttributeType where
    toText = \case
        B -> "B"
        N -> "N"
        S -> "S"

instance ToByteString ScalarAttributeType
instance ToHeader     ScalarAttributeType
instance ToQuery      ScalarAttributeType

instance FromJSON ScalarAttributeType where
    parseJSON = parseJSONText "ScalarAttributeType"

instance ToJSON ScalarAttributeType where
    toJSON = toJSONText

data Projection = Projection
    { _pNonKeyAttributes :: List1 "NonKeyAttributes" Text
    , _pProjectionType   :: Maybe ProjectionType
    } deriving (Eq, Read, Show)

-- | 'Projection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pNonKeyAttributes' @::@ 'NonEmpty' 'Text'
--
-- * 'pProjectionType' @::@ 'Maybe' 'ProjectionType'
--
projection :: NonEmpty Text -- ^ 'pNonKeyAttributes'
           -> Projection
projection p1 = Projection
    { _pNonKeyAttributes = withIso _List1 (const id) p1
    , _pProjectionType   = Nothing
    }

-- | Represents the non-key attribute names which will be projected into the index.
--
-- For local secondary indexes, the total count of /NonKeyAttributes/ summed
-- across all of the local secondary indexes, must not exceed 20. If you project
-- the same attribute into two different indexes, this counts as two distinct
-- attributes when determining the total.
pNonKeyAttributes :: Lens' Projection (NonEmpty Text)
pNonKeyAttributes =
    lens _pNonKeyAttributes (\s a -> s { _pNonKeyAttributes = a })
        . _List1

-- | The set of attributes that are projected into the index:
--
-- 'KEYS_ONLY' - Only the index and primary keys are projected into the index.
--
-- 'INCLUDE' - Only the specified table attributes are projected into the
-- index. The list of projected attributes are in /NonKeyAttributes/.
--
-- 'ALL' - All of the table attributes are projected into the index.
--
--
pProjectionType :: Lens' Projection (Maybe ProjectionType)
pProjectionType = lens _pProjectionType (\s a -> s { _pProjectionType = a })

instance FromJSON Projection where
    parseJSON = withObject "Projection" $ \o -> Projection
        <$> o .:  "NonKeyAttributes"
        <*> o .:? "ProjectionType"

instance ToJSON Projection where
    toJSON Projection{..} = object
        [ "ProjectionType"   .= _pProjectionType
        , "NonKeyAttributes" .= _pNonKeyAttributes
        ]

data CreateGlobalSecondaryIndexAction = CreateGlobalSecondaryIndexAction
    { _cgsiaIndexName             :: Text
    , _cgsiaKeySchema             :: List1 "KeySchema" KeySchemaElement
    , _cgsiaProjection            :: Projection
    , _cgsiaProvisionedThroughput :: ProvisionedThroughput
    } deriving (Eq, Read, Show)

-- | 'CreateGlobalSecondaryIndexAction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cgsiaIndexName' @::@ 'Text'
--
-- * 'cgsiaKeySchema' @::@ 'NonEmpty' 'KeySchemaElement'
--
-- * 'cgsiaProjection' @::@ 'Projection'
--
-- * 'cgsiaProvisionedThroughput' @::@ 'ProvisionedThroughput'
--
createGlobalSecondaryIndexAction :: Text -- ^ 'cgsiaIndexName'
                                 -> NonEmpty KeySchemaElement -- ^ 'cgsiaKeySchema'
                                 -> Projection -- ^ 'cgsiaProjection'
                                 -> ProvisionedThroughput -- ^ 'cgsiaProvisionedThroughput'
                                 -> CreateGlobalSecondaryIndexAction
createGlobalSecondaryIndexAction p1 p2 p3 p4 = CreateGlobalSecondaryIndexAction
    { _cgsiaIndexName             = p1
    , _cgsiaKeySchema             = withIso _List1 (const id) p2
    , _cgsiaProjection            = p3
    , _cgsiaProvisionedThroughput = p4
    }

-- | The name of the global secondary index to be created.
cgsiaIndexName :: Lens' CreateGlobalSecondaryIndexAction Text
cgsiaIndexName = lens _cgsiaIndexName (\s a -> s { _cgsiaIndexName = a })

-- | The key schema for the global secondary index.
cgsiaKeySchema :: Lens' CreateGlobalSecondaryIndexAction (NonEmpty KeySchemaElement)
cgsiaKeySchema = lens _cgsiaKeySchema (\s a -> s { _cgsiaKeySchema = a }) . _List1

cgsiaProjection :: Lens' CreateGlobalSecondaryIndexAction Projection
cgsiaProjection = lens _cgsiaProjection (\s a -> s { _cgsiaProjection = a })

cgsiaProvisionedThroughput :: Lens' CreateGlobalSecondaryIndexAction ProvisionedThroughput
cgsiaProvisionedThroughput =
    lens _cgsiaProvisionedThroughput
        (\s a -> s { _cgsiaProvisionedThroughput = a })

instance FromJSON CreateGlobalSecondaryIndexAction where
    parseJSON = withObject "CreateGlobalSecondaryIndexAction" $ \o -> CreateGlobalSecondaryIndexAction
        <$> o .:  "IndexName"
        <*> o .:  "KeySchema"
        <*> o .:  "Projection"
        <*> o .:  "ProvisionedThroughput"

instance ToJSON CreateGlobalSecondaryIndexAction where
    toJSON CreateGlobalSecondaryIndexAction{..} = object
        [ "IndexName"             .= _cgsiaIndexName
        , "KeySchema"             .= _cgsiaKeySchema
        , "Projection"            .= _cgsiaProjection
        , "ProvisionedThroughput" .= _cgsiaProvisionedThroughput
        ]

data Select
    = AllAttributes          -- ^ ALL_ATTRIBUTES
    | AllProjectedAttributes -- ^ ALL_PROJECTED_ATTRIBUTES
    | Count                  -- ^ COUNT
    | SpecificAttributes     -- ^ SPECIFIC_ATTRIBUTES
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable Select

instance FromText Select where
    parser = takeLowerText >>= \case
        "all_attributes"           -> pure AllAttributes
        "all_projected_attributes" -> pure AllProjectedAttributes
        "count"                    -> pure Count
        "specific_attributes"      -> pure SpecificAttributes
        e                          -> fail $
            "Failure parsing Select from " ++ show e

instance ToText Select where
    toText = \case
        AllAttributes          -> "ALL_ATTRIBUTES"
        AllProjectedAttributes -> "ALL_PROJECTED_ATTRIBUTES"
        Count                  -> "COUNT"
        SpecificAttributes     -> "SPECIFIC_ATTRIBUTES"

instance ToByteString Select
instance ToHeader     Select
instance ToQuery      Select

instance FromJSON Select where
    parseJSON = parseJSONText "Select"

instance ToJSON Select where
    toJSON = toJSONText

data KeySchemaElement = KeySchemaElement
    { _kseAttributeName :: Text
    , _kseKeyType       :: KeyType
    } deriving (Eq, Read, Show)

-- | 'KeySchemaElement' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kseAttributeName' @::@ 'Text'
--
-- * 'kseKeyType' @::@ 'KeyType'
--
keySchemaElement :: Text -- ^ 'kseAttributeName'
                 -> KeyType -- ^ 'kseKeyType'
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
kseKeyType :: Lens' KeySchemaElement KeyType
kseKeyType = lens _kseKeyType (\s a -> s { _kseKeyType = a })

instance FromJSON KeySchemaElement where
    parseJSON = withObject "KeySchemaElement" $ \o -> KeySchemaElement
        <$> o .:  "AttributeName"
        <*> o .:  "KeyType"

instance ToJSON KeySchemaElement where
    toJSON KeySchemaElement{..} = object
        [ "AttributeName" .= _kseAttributeName
        , "KeyType"       .= _kseKeyType
        ]

newtype DeleteGlobalSecondaryIndexAction = DeleteGlobalSecondaryIndexAction
    { _dgsiaIndexName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteGlobalSecondaryIndexAction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgsiaIndexName' @::@ 'Text'
--
deleteGlobalSecondaryIndexAction :: Text -- ^ 'dgsiaIndexName'
                                 -> DeleteGlobalSecondaryIndexAction
deleteGlobalSecondaryIndexAction p1 = DeleteGlobalSecondaryIndexAction
    { _dgsiaIndexName = p1
    }

-- | The name of the global secondary index to be deleted.
dgsiaIndexName :: Lens' DeleteGlobalSecondaryIndexAction Text
dgsiaIndexName = lens _dgsiaIndexName (\s a -> s { _dgsiaIndexName = a })

instance FromJSON DeleteGlobalSecondaryIndexAction where
    parseJSON = withObject "DeleteGlobalSecondaryIndexAction" $ \o -> DeleteGlobalSecondaryIndexAction
        <$> o .:  "IndexName"

instance ToJSON DeleteGlobalSecondaryIndexAction where
    toJSON DeleteGlobalSecondaryIndexAction{..} = object
        [ "IndexName" .= _dgsiaIndexName
        ]

newtype DeleteRequest = DeleteRequest
    { _dKey :: Map Text AttributeValue
    } deriving (Eq, Read, Show, Monoid, Semigroup)

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

-- | A map of attribute name to attribute values, representing the primary key of
-- the item to delete. All of the table's primary key attributes must be
-- specified, and their data types must match those of the table's key schema.
dKey :: Lens' DeleteRequest (HashMap Text AttributeValue)
dKey = lens _dKey (\s a -> s { _dKey = a }) . _Map

instance FromJSON DeleteRequest where
    parseJSON = withObject "DeleteRequest" $ \o -> DeleteRequest
        <$> o .:? "Key" .!= mempty

instance ToJSON DeleteRequest where
    toJSON DeleteRequest{..} = object
        [ "Key" .= _dKey
        ]

data UpdateGlobalSecondaryIndexAction = UpdateGlobalSecondaryIndexAction
    { _ugsiaIndexName             :: Text
    , _ugsiaProvisionedThroughput :: ProvisionedThroughput
    } deriving (Eq, Read, Show)

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

instance FromJSON UpdateGlobalSecondaryIndexAction where
    parseJSON = withObject "UpdateGlobalSecondaryIndexAction" $ \o -> UpdateGlobalSecondaryIndexAction
        <$> o .:  "IndexName"
        <*> o .:  "ProvisionedThroughput"

instance ToJSON UpdateGlobalSecondaryIndexAction where
    toJSON UpdateGlobalSecondaryIndexAction{..} = object
        [ "IndexName"             .= _ugsiaIndexName
        , "ProvisionedThroughput" .= _ugsiaProvisionedThroughput
        ]

newtype PutRequest = PutRequest
    { _pItem :: Map Text AttributeValue
    } deriving (Eq, Read, Show, Monoid, Semigroup)

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

-- | A map of attribute name to attribute values, representing the primary key of
-- an item to be processed by /PutItem/. All of the table's primary key attributes
-- must be specified, and their data types must match those of the table's key
-- schema. If any attributes are present in the item which are part of an index
-- key schema for the table, their types must match the index key schema.
pItem :: Lens' PutRequest (HashMap Text AttributeValue)
pItem = lens _pItem (\s a -> s { _pItem = a }) . _Map

instance FromJSON PutRequest where
    parseJSON = withObject "PutRequest" $ \o -> PutRequest
        <$> o .:? "Item" .!= mempty

instance ToJSON PutRequest where
    toJSON PutRequest{..} = object
        [ "Item" .= _pItem
        ]

data Condition = Condition
    { _cAttributeValueList :: List "AttributeValueList" AttributeValue
    , _cComparisonOperator :: ComparisonOperator
    } deriving (Eq, Read, Show)

-- | 'Condition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cAttributeValueList' @::@ ['AttributeValue']
--
-- * 'cComparisonOperator' @::@ 'ComparisonOperator'
--
condition :: ComparisonOperator -- ^ 'cComparisonOperator'
          -> Condition
condition p1 = Condition
    { _cComparisonOperator = p1
    , _cAttributeValueList = mempty
    }

-- | One or more values to evaluate against the supplied attribute. The number of
-- values in the list depends on the /ComparisonOperator/ being used.
--
-- For type Number, value comparisons are numeric.
--
-- String value comparisons for greater than, equals, or less than are based on
-- ASCII character code values. For example, 'a' is greater than 'A', and 'a' is
-- greater than 'B'. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
--
-- For Binary, DynamoDB treats each byte of the binary data as unsigned when it
-- compares binary values.
cAttributeValueList :: Lens' Condition [AttributeValue]
cAttributeValueList =
    lens _cAttributeValueList (\s a -> s { _cAttributeValueList = a })
        . _List

-- | A comparator for evaluating attributes. For example, equals, greater than,
-- less than, etc.
--
-- The following comparison operators are available:
--
-- 'EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS |BEGINS_WITH | IN | BETWEEN'
--
-- The following are descriptions of each comparison operator.
--
-- 'EQ' : Equal. 'EQ' is supported for all datatypes, including lists and maps.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, Binary, String Set, Number Set, or Binary Set. If an item
-- contains an /AttributeValue/ element of a different type than the one provided
-- in the request, the value does not match. For example, '{"S":"6"}' does not
-- equal '{"N":"6"}'. Also, '{"N":"6"}' does not equal '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'NE' : Not equal. 'NE' is supported for all datatypes, including lists and
-- maps.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ of type String,
-- Number, Binary, String Set, Number Set, or Binary Set. If an item contains an /AttributeValue/ of a different type than the one provided in the request, the
-- value does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not equal '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'LE' : Less than or equal.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'LT' : Less than.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ of type String,
-- Number, or Binary (not a set type). If an item contains an /AttributeValue/
-- element of a different type than the one provided in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'GE' : Greater than or equal.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'GT' : Greater than.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'NOT_NULL' : The attribute exists. 'NOT_NULL' is supported for all datatypes,
-- including lists and maps.
--
-- This operator tests for the existence of an attribute, not its data type. If
-- the data type of attribute "'a'" is null, and you evaluate it using 'NOT_NULL',
-- the result is a Boolean /true/. This result is because the attribute "'a'"
-- exists; its data type is not relevant to the 'NOT_NULL' comparison operator.
--
-- 'NULL' : The attribute does not exist. 'NULL' is supported for all datatypes,
-- including lists and maps.
--
-- This operator tests for the nonexistence of an attribute, not its data type.
-- If the data type of attribute "'a'" is null, and you evaluate it using 'NULL',
-- the result is a Boolean /false/. This is because the attribute "'a'" exists; its
-- data type is not relevant to the 'NULL' comparison operator.
--
-- 'CONTAINS' : Checks for a subsequence, or value in a set.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If the target attribute of the
-- comparison is of type String, then the operator checks for a substring match.
-- If the target attribute of the comparison is of type Binary, then the
-- operator looks for a subsequence of the target that matches the input. If the
-- target attribute of the comparison is a set ("'SS'", "'NS'", or "'BS'"), then the
-- operator evaluates to true if it finds an exact match with any member of the
-- set.
--
-- CONTAINS is supported for lists: When evaluating "'a CONTAINS b'", "'a'" can be
-- a list; however, "'b'" cannot be a set, a map, or a list.
--
-- 'NOT_CONTAINS' : Checks for absence of a subsequence, or absence of a value
-- in a set.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If the target attribute of the
-- comparison is a String, then the operator checks for the absence of a
-- substring match. If the target attribute of the comparison is Binary, then
-- the operator checks for the absence of a subsequence of the target that
-- matches the input. If the target attribute of the comparison is a set ("'SS'", "'NS'", or "'BS'"), then the operator evaluates to true if it /does not/ find an
-- exact match with any member of the set.
--
-- NOT_CONTAINS is supported for lists: When evaluating "'a NOT CONTAINS b'", "'a'"
-- can be a list; however, "'b'" cannot be a set, a map, or a list.
--
-- 'BEGINS_WITH' : Checks for a prefix.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ of type String or
-- Binary (not a Number or a set type). The target attribute of the comparison
-- must be of type String or Binary (not a Number or a set type).
--
--
--
-- 'IN' : Checks for matching elements within two sets.
--
-- /AttributeValueList/ can contain one or more /AttributeValue/ elements of type
-- String, Number, or Binary (not a set type). These attributes are compared
-- against an existing set type attribute of an item. If any elements of the
-- input set are present in the item attribute, the expression evaluates to true.
--
-- 'BETWEEN' : Greater than or equal to the first value, and less than or equal
-- to the second value.
--
-- /AttributeValueList/ must contain two /AttributeValue/ elements of the same
-- type, either String, Number, or Binary (not a set type). A target attribute
-- matches if the target value is greater than, or equal to, the first element
-- and less than, or equal to, the second element. If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value
-- does not match. For example, '{"S":"6"}' does not compare to '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'
--
-- For usage examples of /AttributeValueList/ and /ComparisonOperator/, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/.
cComparisonOperator :: Lens' Condition ComparisonOperator
cComparisonOperator =
    lens _cComparisonOperator (\s a -> s { _cComparisonOperator = a })

instance FromJSON Condition where
    parseJSON = withObject "Condition" $ \o -> Condition
        <$> o .:? "AttributeValueList" .!= mempty
        <*> o .:  "ComparisonOperator"

instance ToJSON Condition where
    toJSON Condition{..} = object
        [ "AttributeValueList" .= _cAttributeValueList
        , "ComparisonOperator" .= _cComparisonOperator
        ]

data ConditionalOperator
    = And -- ^ AND
    | Or  -- ^ OR
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ConditionalOperator

instance FromText ConditionalOperator where
    parser = takeLowerText >>= \case
        "and" -> pure And
        "or"  -> pure Or
        e     -> fail $
            "Failure parsing ConditionalOperator from " ++ show e

instance ToText ConditionalOperator where
    toText = \case
        And -> "AND"
        Or  -> "OR"

instance ToByteString ConditionalOperator
instance ToHeader     ConditionalOperator
instance ToQuery      ConditionalOperator

instance FromJSON ConditionalOperator where
    parseJSON = parseJSONText "ConditionalOperator"

instance ToJSON ConditionalOperator where
    toJSON = toJSONText

data GlobalSecondaryIndexUpdate = GlobalSecondaryIndexUpdate
    { _gsiuCreate :: Maybe CreateGlobalSecondaryIndexAction
    , _gsiuDelete :: Maybe DeleteGlobalSecondaryIndexAction
    , _gsiuUpdate :: Maybe UpdateGlobalSecondaryIndexAction
    } deriving (Eq, Read, Show)

-- | 'GlobalSecondaryIndexUpdate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsiuCreate' @::@ 'Maybe' 'CreateGlobalSecondaryIndexAction'
--
-- * 'gsiuDelete' @::@ 'Maybe' 'DeleteGlobalSecondaryIndexAction'
--
-- * 'gsiuUpdate' @::@ 'Maybe' 'UpdateGlobalSecondaryIndexAction'
--
globalSecondaryIndexUpdate :: GlobalSecondaryIndexUpdate
globalSecondaryIndexUpdate = GlobalSecondaryIndexUpdate
    { _gsiuUpdate = Nothing
    , _gsiuCreate = Nothing
    , _gsiuDelete = Nothing
    }

-- | The parameters required for creating a global secondary index on an existing
-- table:
--
-- 'IndexName '
--
-- 'KeySchema '
--
-- 'AttributeDefinitions '
--
-- 'Projection '
--
-- 'ProvisionedThroughput '
--
--
gsiuCreate :: Lens' GlobalSecondaryIndexUpdate (Maybe CreateGlobalSecondaryIndexAction)
gsiuCreate = lens _gsiuCreate (\s a -> s { _gsiuCreate = a })

-- | The name of an existing global secondary index to be removed.
gsiuDelete :: Lens' GlobalSecondaryIndexUpdate (Maybe DeleteGlobalSecondaryIndexAction)
gsiuDelete = lens _gsiuDelete (\s a -> s { _gsiuDelete = a })

-- | The name of an existing global secondary index, along with new provisioned
-- throughput settings to be applied to that index.
gsiuUpdate :: Lens' GlobalSecondaryIndexUpdate (Maybe UpdateGlobalSecondaryIndexAction)
gsiuUpdate = lens _gsiuUpdate (\s a -> s { _gsiuUpdate = a })

instance FromJSON GlobalSecondaryIndexUpdate where
    parseJSON = withObject "GlobalSecondaryIndexUpdate" $ \o -> GlobalSecondaryIndexUpdate
        <$> o .:? "Create"
        <*> o .:? "Delete"
        <*> o .:? "Update"

instance ToJSON GlobalSecondaryIndexUpdate where
    toJSON GlobalSecondaryIndexUpdate{..} = object
        [ "Update" .= _gsiuUpdate
        , "Create" .= _gsiuCreate
        , "Delete" .= _gsiuDelete
        ]
