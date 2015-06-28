{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DynamoDB.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    , capCapacityUnits

    -- * Condition
    , Condition
    , condition
    , conAttributeValueList
    , conComparisonOperator

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
    , lsidKeySchema
    , lsidProjection
    , lsidItemCount
    , lsidIndexName

    -- * Projection
    , Projection
    , projection
    , proProjectionType
    , proNonKeyAttributes

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

    -- * TableDescription
    , TableDescription
    , tableDescription
    , tdProvisionedThroughput
    , tdAttributeDefinitions
    , tdTableSizeBytes
    , tdTableStatus
    , tdKeySchema
    , tdGlobalSecondaryIndexes
    , tdLocalSecondaryIndexes
    , tdCreationDateTime
    , tdItemCount
    , tdTableName

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
            , _svcTimeout = 80000000
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

-- | The request rate is too high, or the request is too large, for the
-- available throughput to accommodate. The AWS SDKs automatically retry
-- requests that receive this exception; therefore, your request will
-- eventually succeed, unless the request is too large or your retry queue
-- is too large to finish. Reduce the frequency of requests by using the
-- strategies listed in
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

data AttributeAction
    = Add
    | Put
    | Delete
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText AttributeAction where
    parser = takeLowerText >>= \case
        "ADD" -> pure Add
        "DELETE" -> pure Delete
        "PUT" -> pure Put
        e -> fail ("Failure parsing AttributeAction from " ++ show e)

instance ToText AttributeAction where
    toText = \case
        Add -> "ADD"
        Delete -> "DELETE"
        Put -> "PUT"

instance Hashable AttributeAction
instance ToQuery AttributeAction
instance ToHeader AttributeAction

instance ToJSON AttributeAction where
    toJSON = toJSONText

data ComparisonOperator
    = GE
    | EQ'
    | NE
    | Null
    | NotContains
    | GT'
    | LT'
    | IN
    | Between
    | Contains
    | BeginsWith
    | NotNull
    | LE
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ComparisonOperator where
    parser = takeLowerText >>= \case
        "BEGINS_WITH" -> pure BeginsWith
        "BETWEEN" -> pure Between
        "CONTAINS" -> pure Contains
        "EQ" -> pure EQ'
        "GE" -> pure GE
        "GT" -> pure GT'
        "IN" -> pure IN
        "LE" -> pure LE
        "LT" -> pure LT'
        "NE" -> pure NE
        "NOT_CONTAINS" -> pure NotContains
        "NOT_NULL" -> pure NotNull
        "NULL" -> pure Null
        e -> fail ("Failure parsing ComparisonOperator from " ++ show e)

instance ToText ComparisonOperator where
    toText = \case
        BeginsWith -> "BEGINS_WITH"
        Between -> "BETWEEN"
        Contains -> "CONTAINS"
        EQ' -> "EQ"
        GE -> "GE"
        GT' -> "GT"
        IN -> "IN"
        LE -> "LE"
        LT' -> "LT"
        NE -> "NE"
        NotContains -> "NOT_CONTAINS"
        NotNull -> "NOT_NULL"
        Null -> "NULL"

instance Hashable ComparisonOperator
instance ToQuery ComparisonOperator
instance ToHeader ComparisonOperator

instance ToJSON ComparisonOperator where
    toJSON = toJSONText

data ConditionalOperator
    = And
    | OR
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ConditionalOperator where
    parser = takeLowerText >>= \case
        "AND" -> pure And
        "OR" -> pure OR
        e -> fail ("Failure parsing ConditionalOperator from " ++ show e)

instance ToText ConditionalOperator where
    toText = \case
        And -> "AND"
        OR -> "OR"

instance Hashable ConditionalOperator
instance ToQuery ConditionalOperator
instance ToHeader ConditionalOperator

instance ToJSON ConditionalOperator where
    toJSON = toJSONText

data IndexStatus
    = ISUpdating
    | ISDeleting
    | ISCreating
    | ISActive
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText IndexStatus where
    parser = takeLowerText >>= \case
        "ACTIVE" -> pure ISActive
        "CREATING" -> pure ISCreating
        "DELETING" -> pure ISDeleting
        "UPDATING" -> pure ISUpdating
        e -> fail ("Failure parsing IndexStatus from " ++ show e)

instance ToText IndexStatus where
    toText = \case
        ISActive -> "ACTIVE"
        ISCreating -> "CREATING"
        ISDeleting -> "DELETING"
        ISUpdating -> "UPDATING"

instance Hashable IndexStatus
instance ToQuery IndexStatus
instance ToHeader IndexStatus

instance FromJSON IndexStatus where
    parseJSON = parseJSONText "IndexStatus"

data KeyType
    = Hash
    | Range
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText KeyType where
    parser = takeLowerText >>= \case
        "HASH" -> pure Hash
        "RANGE" -> pure Range
        e -> fail ("Failure parsing KeyType from " ++ show e)

instance ToText KeyType where
    toText = \case
        Hash -> "HASH"
        Range -> "RANGE"

instance Hashable KeyType
instance ToQuery KeyType
instance ToHeader KeyType

instance ToJSON KeyType where
    toJSON = toJSONText

instance FromJSON KeyType where
    parseJSON = parseJSONText "KeyType"

data ProjectionType
    = Include
    | All
    | KeysOnly
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ProjectionType where
    parser = takeLowerText >>= \case
        "ALL" -> pure All
        "INCLUDE" -> pure Include
        "KEYS_ONLY" -> pure KeysOnly
        e -> fail ("Failure parsing ProjectionType from " ++ show e)

instance ToText ProjectionType where
    toText = \case
        All -> "ALL"
        Include -> "INCLUDE"
        KeysOnly -> "KEYS_ONLY"

instance Hashable ProjectionType
instance ToQuery ProjectionType
instance ToHeader ProjectionType

instance ToJSON ProjectionType where
    toJSON = toJSONText

instance FromJSON ProjectionType where
    parseJSON = parseJSONText "ProjectionType"

-- | A value that if set to @TOTAL@, the response includes /ConsumedCapacity/
-- data for tables and indexes. If set to @INDEXES@, the response includes
-- /ConsumedCapacity/ for indexes. If set to @NONE@ (the default),
-- /ConsumedCapacity/ is not included in the response.
data ReturnConsumedCapacity
    = RCCNone
    | RCCIndexes
    | RCCTotal
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ReturnConsumedCapacity where
    parser = takeLowerText >>= \case
        "INDEXES" -> pure RCCIndexes
        "NONE" -> pure RCCNone
        "TOTAL" -> pure RCCTotal
        e -> fail ("Failure parsing ReturnConsumedCapacity from " ++ show e)

instance ToText ReturnConsumedCapacity where
    toText = \case
        RCCIndexes -> "INDEXES"
        RCCNone -> "NONE"
        RCCTotal -> "TOTAL"

instance Hashable ReturnConsumedCapacity
instance ToQuery ReturnConsumedCapacity
instance ToHeader ReturnConsumedCapacity

instance ToJSON ReturnConsumedCapacity where
    toJSON = toJSONText

data ReturnItemCollectionMetrics
    = RICMNone
    | RICMSize
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ReturnItemCollectionMetrics where
    parser = takeLowerText >>= \case
        "NONE" -> pure RICMNone
        "SIZE" -> pure RICMSize
        e -> fail ("Failure parsing ReturnItemCollectionMetrics from " ++ show e)

instance ToText ReturnItemCollectionMetrics where
    toText = \case
        RICMNone -> "NONE"
        RICMSize -> "SIZE"

instance Hashable ReturnItemCollectionMetrics
instance ToQuery ReturnItemCollectionMetrics
instance ToHeader ReturnItemCollectionMetrics

instance ToJSON ReturnItemCollectionMetrics where
    toJSON = toJSONText

data ReturnValue
    = UpdatedOld
    | None
    | AllNew
    | AllOld
    | UpdatedNew
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ReturnValue where
    parser = takeLowerText >>= \case
        "ALL_NEW" -> pure AllNew
        "ALL_OLD" -> pure AllOld
        "NONE" -> pure None
        "UPDATED_NEW" -> pure UpdatedNew
        "UPDATED_OLD" -> pure UpdatedOld
        e -> fail ("Failure parsing ReturnValue from " ++ show e)

instance ToText ReturnValue where
    toText = \case
        AllNew -> "ALL_NEW"
        AllOld -> "ALL_OLD"
        None -> "NONE"
        UpdatedNew -> "UPDATED_NEW"
        UpdatedOld -> "UPDATED_OLD"

instance Hashable ReturnValue
instance ToQuery ReturnValue
instance ToHeader ReturnValue

instance ToJSON ReturnValue where
    toJSON = toJSONText

data ScalarAttributeType
    = N
    | B
    | S
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ScalarAttributeType where
    parser = takeLowerText >>= \case
        "B" -> pure B
        "N" -> pure N
        "S" -> pure S
        e -> fail ("Failure parsing ScalarAttributeType from " ++ show e)

instance ToText ScalarAttributeType where
    toText = \case
        B -> "B"
        N -> "N"
        S -> "S"

instance Hashable ScalarAttributeType
instance ToQuery ScalarAttributeType
instance ToHeader ScalarAttributeType

instance ToJSON ScalarAttributeType where
    toJSON = toJSONText

instance FromJSON ScalarAttributeType where
    parseJSON = parseJSONText "ScalarAttributeType"

data Select
    = Count
    | AllAttributes
    | SpecificAttributes
    | AllProjectedAttributes
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText Select where
    parser = takeLowerText >>= \case
        "ALL_ATTRIBUTES" -> pure AllAttributes
        "ALL_PROJECTED_ATTRIBUTES" -> pure AllProjectedAttributes
        "COUNT" -> pure Count
        "SPECIFIC_ATTRIBUTES" -> pure SpecificAttributes
        e -> fail ("Failure parsing Select from " ++ show e)

instance ToText Select where
    toText = \case
        AllAttributes -> "ALL_ATTRIBUTES"
        AllProjectedAttributes -> "ALL_PROJECTED_ATTRIBUTES"
        Count -> "COUNT"
        SpecificAttributes -> "SPECIFIC_ATTRIBUTES"

instance Hashable Select
instance ToQuery Select
instance ToHeader Select

instance ToJSON Select where
    toJSON = toJSONText

data TableStatus
    = Deleting
    | Updating
    | Creating
    | Active
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText TableStatus where
    parser = takeLowerText >>= \case
        "ACTIVE" -> pure Active
        "CREATING" -> pure Creating
        "DELETING" -> pure Deleting
        "UPDATING" -> pure Updating
        e -> fail ("Failure parsing TableStatus from " ++ show e)

instance ToText TableStatus where
    toText = \case
        Active -> "ACTIVE"
        Creating -> "CREATING"
        Deleting -> "DELETING"
        Updating -> "UPDATING"

instance Hashable TableStatus
instance ToQuery TableStatus
instance ToHeader TableStatus

instance FromJSON TableStatus where
    parseJSON = parseJSONText "TableStatus"

-- | Represents an attribute for describing the key schema for the table and
-- indexes.
--
-- /See:/ 'attributeDefinition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adAttributeName'
--
-- * 'adAttributeType'
data AttributeDefinition = AttributeDefinition'
    { _adAttributeName :: !Text
    , _adAttributeType :: !ScalarAttributeType
    } deriving (Eq,Read,Show)

-- | 'AttributeDefinition' smart constructor.
attributeDefinition :: Text -> ScalarAttributeType -> AttributeDefinition
attributeDefinition pAttributeName pAttributeType =
    AttributeDefinition'
    { _adAttributeName = pAttributeName
    , _adAttributeType = pAttributeType
    }

-- | A name for the attribute.
adAttributeName :: Lens' AttributeDefinition Text
adAttributeName = lens _adAttributeName (\ s a -> s{_adAttributeName = a});

-- | The data type for the attribute.
adAttributeType :: Lens' AttributeDefinition ScalarAttributeType
adAttributeType = lens _adAttributeType (\ s a -> s{_adAttributeType = a});

instance FromJSON AttributeDefinition where
        parseJSON
          = withObject "AttributeDefinition"
              (\ x ->
                 AttributeDefinition' <$>
                   (x .: "AttributeName") <*> (x .: "AttributeType"))

instance ToJSON AttributeDefinition where
        toJSON AttributeDefinition'{..}
          = object
              ["AttributeName" .= _adAttributeName,
               "AttributeType" .= _adAttributeType]

-- | Represents the data for an attribute. You can set one, and only one, of
-- the elements.
--
-- Each attribute in an item is a name-value pair. An attribute can be
-- single-valued or multi-valued set. For example, a book item can have
-- title and authors attributes. Each book has one title but can have many
-- authors. The multi-valued attribute is a set; duplicate values are not
-- allowed.
--
-- /See:/ 'attributeValue' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avL'
--
-- * 'avM'
--
-- * 'avNS'
--
-- * 'avNULL'
--
-- * 'avN'
--
-- * 'avBS'
--
-- * 'avB'
--
-- * 'avSS'
--
-- * 'avS'
--
-- * 'avBOOL'
data AttributeValue = AttributeValue'
    { _avL    :: !(Maybe [AttributeValue])
    , _avM    :: !(Maybe (Map Text AttributeValue))
    , _avNS   :: !(Maybe [Text])
    , _avNULL :: !(Maybe Bool)
    , _avN    :: !(Maybe Text)
    , _avBS   :: !(Maybe [Base64])
    , _avB    :: !(Maybe Base64)
    , _avSS   :: !(Maybe [Text])
    , _avS    :: !(Maybe Text)
    , _avBOOL :: !(Maybe Bool)
    } deriving (Eq,Show)

-- | 'AttributeValue' smart constructor.
attributeValue :: AttributeValue
attributeValue =
    AttributeValue'
    { _avL = Nothing
    , _avM = Nothing
    , _avNS = Nothing
    , _avNULL = Nothing
    , _avN = Nothing
    , _avBS = Nothing
    , _avB = Nothing
    , _avSS = Nothing
    , _avS = Nothing
    , _avBOOL = Nothing
    }

-- | A List of attribute values.
avL :: Lens' AttributeValue [AttributeValue]
avL = lens _avL (\ s a -> s{_avL = a}) . _Default;

-- | A Map of attribute values.
avM :: Lens' AttributeValue (HashMap Text AttributeValue)
avM = lens _avM (\ s a -> s{_avM = a}) . _Default . _Map;

-- | A Number Set data type.
avNS :: Lens' AttributeValue [Text]
avNS = lens _avNS (\ s a -> s{_avNS = a}) . _Default;

-- | A Null data type.
avNULL :: Lens' AttributeValue (Maybe Bool)
avNULL = lens _avNULL (\ s a -> s{_avNULL = a});

-- | A Number data type.
avN :: Lens' AttributeValue (Maybe Text)
avN = lens _avN (\ s a -> s{_avN = a});

-- | A Binary Set data type.
avBS :: Lens' AttributeValue [Base64]
avBS = lens _avBS (\ s a -> s{_avBS = a}) . _Default;

-- | A Binary data type.
avB :: Lens' AttributeValue (Maybe Base64)
avB = lens _avB (\ s a -> s{_avB = a});

-- | A String Set data type.
avSS :: Lens' AttributeValue [Text]
avSS = lens _avSS (\ s a -> s{_avSS = a}) . _Default;

-- | A String data type.
avS :: Lens' AttributeValue (Maybe Text)
avS = lens _avS (\ s a -> s{_avS = a});

-- | A Boolean data type.
avBOOL :: Lens' AttributeValue (Maybe Bool)
avBOOL = lens _avBOOL (\ s a -> s{_avBOOL = a});

instance FromJSON AttributeValue where
        parseJSON
          = withObject "AttributeValue"
              (\ x ->
                 AttributeValue' <$>
                   (x .:? "L" .!= mempty) <*> (x .:? "M" .!= mempty) <*>
                     (x .:? "NS" .!= mempty)
                     <*> (x .:? "NULL")
                     <*> (x .:? "N")
                     <*> (x .:? "BS" .!= mempty)
                     <*> (x .:? "B")
                     <*> (x .:? "SS" .!= mempty)
                     <*> (x .:? "S")
                     <*> (x .:? "BOOL"))

instance ToJSON AttributeValue where
        toJSON AttributeValue'{..}
          = object
              ["L" .= _avL, "M" .= _avM, "NS" .= _avNS,
               "NULL" .= _avNULL, "N" .= _avN, "BS" .= _avBS,
               "B" .= _avB, "SS" .= _avSS, "S" .= _avS,
               "BOOL" .= _avBOOL]

-- | For the /UpdateItem/ operation, represents the attributes to be
-- modified, the action to perform on each, and the new value for each.
--
-- You cannot use /UpdateItem/ to update any primary key attributes.
-- Instead, you will need to delete the item, and then use /PutItem/ to
-- create a new item with new attributes.
--
-- Attribute values cannot be null; string and binary type attributes must
-- have lengths greater than zero; and set type attributes must not be
-- empty. Requests with empty values will be rejected with a
-- /ValidationException/ exception.
--
-- /See:/ 'attributeValueUpdate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avuValue'
--
-- * 'avuAction'
data AttributeValueUpdate = AttributeValueUpdate'
    { _avuValue  :: !(Maybe AttributeValue)
    , _avuAction :: !(Maybe AttributeAction)
    } deriving (Eq,Show)

-- | 'AttributeValueUpdate' smart constructor.
attributeValueUpdate :: AttributeValueUpdate
attributeValueUpdate =
    AttributeValueUpdate'
    { _avuValue = Nothing
    , _avuAction = Nothing
    }

-- | FIXME: Undocumented member.
avuValue :: Lens' AttributeValueUpdate (Maybe AttributeValue)
avuValue = lens _avuValue (\ s a -> s{_avuValue = a});

-- | Specifies how to perform the update. Valid values are @PUT@ (default),
-- @DELETE@, and @ADD@. The behavior depends on whether the specified
-- primary key already exists in the table.
--
-- __If an item with the specified /Key/ is found in the table:__
--
-- -   @PUT@ - Adds the specified attribute to the item. If the attribute
--     already exists, it is replaced by the new value.
--
-- -   @DELETE@ - If no value is specified, the attribute and its value are
--     removed from the item. The data type of the specified value must
--     match the existing value\'s data type.
--
--     If a /set/ of values is specified, then those values are subtracted
--     from the old set. For example, if the attribute value was the set
--     @[a,b,c]@ and the /DELETE/ action specified @[a,c]@, then the final
--     attribute value would be @[b]@. Specifying an empty set is an error.
--
-- -   @ADD@ - If the attribute does not already exist, then the attribute
--     and its values are added to the item. If the attribute does exist,
--     then the behavior of @ADD@ depends on the data type of the
--     attribute:
--
--     -   If the existing attribute is a number, and if /Value/ is also a
--         number, then the /Value/ is mathematically added to the existing
--         attribute. If /Value/ is a negative number, then it is
--         subtracted from the existing attribute.
--
--         If you use @ADD@ to increment or decrement a number value for an
--         item that doesn\'t exist before the update, DynamoDB uses 0 as
--         the initial value.
--
--         In addition, if you use @ADD@ to update an existing item, and
--         intend to increment or decrement an attribute value which does
--         not yet exist, DynamoDB uses @0@ as the initial value. For
--         example, suppose that the item you want to update does not yet
--         have an attribute named /itemcount/, but you decide to @ADD@ the
--         number @3@ to this attribute anyway, even though it currently
--         does not exist. DynamoDB will create the /itemcount/ attribute,
--         set its initial value to @0@, and finally add @3@ to it. The
--         result will be a new /itemcount/ attribute in the item, with a
--         value of @3@.
--
--     -   If the existing data type is a set, and if the /Value/ is also a
--         set, then the /Value/ is added to the existing set. (This is a
--         /set/ operation, not mathematical addition.) For example, if the
--         attribute value was the set @[1,2]@, and the @ADD@ action
--         specified @[3]@, then the final attribute value would be
--         @[1,2,3]@. An error occurs if an Add action is specified for a
--         set attribute and the attribute type specified does not match
--         the existing set type.
--
--         Both sets must have the same primitive data type. For example,
--         if the existing data type is a set of strings, the /Value/ must
--         also be a set of strings. The same holds true for number sets
--         and binary sets.
--
--     This action is only valid for an existing attribute whose data type
--     is number or is a set. Do not use @ADD@ for any other data types.
--
-- __If no item with the specified /Key/ is found:__
--
-- -   @PUT@ - DynamoDB creates a new item with the specified primary key,
--     and then adds the attribute.
--
-- -   @DELETE@ - Nothing happens; there is no attribute to delete.
--
-- -   @ADD@ - DynamoDB creates an item with the supplied primary key and
--     number (or set of numbers) for the attribute value. The only data
--     types allowed are number and number set; no other data types can be
--     specified.
--
avuAction :: Lens' AttributeValueUpdate (Maybe AttributeAction)
avuAction = lens _avuAction (\ s a -> s{_avuAction = a});

instance ToJSON AttributeValueUpdate where
        toJSON AttributeValueUpdate'{..}
          = object
              ["Value" .= _avuValue, "Action" .= _avuAction]

-- | Represents the amount of provisioned throughput capacity consumed on a
-- table or an index.
--
-- /See:/ 'capacity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'capCapacityUnits'
newtype Capacity = Capacity'
    { _capCapacityUnits :: Maybe Double
    } deriving (Eq,Read,Show)

-- | 'Capacity' smart constructor.
capacity :: Capacity
capacity =
    Capacity'
    { _capCapacityUnits = Nothing
    }

-- | The total number of capacity units consumed on a table or an index.
capCapacityUnits :: Lens' Capacity (Maybe Double)
capCapacityUnits = lens _capCapacityUnits (\ s a -> s{_capCapacityUnits = a});

instance FromJSON Capacity where
        parseJSON
          = withObject "Capacity"
              (\ x -> Capacity' <$> (x .:? "CapacityUnits"))

-- | Represents the selection criteria for a /Query/ or /Scan/ operation:
--
-- -   For a /Query/ operation, /Condition/ is used for specifying the
--     /KeyConditions/ to use when querying a table or an index. For
--     /KeyConditions/, only the following comparison operators are
--     supported:
--
--     @EQ | LE | LT | GE | GT | BEGINS_WITH | BETWEEN@
--
--     /Condition/ is also used in a /QueryFilter/, which evaluates the
--     query results and returns only the desired values.
--
-- -   For a /Scan/ operation, /Condition/ is used in a /ScanFilter/, which
--     evaluates the scan results and returns only the desired values.
--
--
-- /See:/ 'condition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'conAttributeValueList'
--
-- * 'conComparisonOperator'
data Condition = Condition'
    { _conAttributeValueList :: !(Maybe [AttributeValue])
    , _conComparisonOperator :: !ComparisonOperator
    } deriving (Eq,Show)

-- | 'Condition' smart constructor.
condition :: ComparisonOperator -> Condition
condition pComparisonOperator =
    Condition'
    { _conAttributeValueList = Nothing
    , _conComparisonOperator = pComparisonOperator
    }

-- | One or more values to evaluate against the supplied attribute. The
-- number of values in the list depends on the /ComparisonOperator/ being
-- used.
--
-- For type Number, value comparisons are numeric.
--
-- String value comparisons for greater than, equals, or less than are
-- based on ASCII character code values. For example, @a@ is greater than
-- @A@, and @a@ is greater than @B@. For a list of code values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
--
-- For Binary, DynamoDB treats each byte of the binary data as unsigned
-- when it compares binary values.
conAttributeValueList :: Lens' Condition [AttributeValue]
conAttributeValueList = lens _conAttributeValueList (\ s a -> s{_conAttributeValueList = a}) . _Default;

-- | A comparator for evaluating attributes. For example, equals, greater
-- than, less than, etc.
--
-- The following comparison operators are available:
--
-- @EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN@
--
-- The following are descriptions of each comparison operator.
--
-- -   @EQ@ : Equal. @EQ@ is supported for all datatypes, including lists
--     and maps.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ element
--     of type String, Number, Binary, String Set, Number Set, or Binary
--     Set. If an item contains an /AttributeValue/ element of a different
--     type than the one provided in the request, the value does not match.
--     For example, @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not equal @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @NE@ : Not equal. @NE@ is supported for all datatypes, including
--     lists and maps.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ of type
--     String, Number, Binary, String Set, Number Set, or Binary Set. If an
--     item contains an /AttributeValue/ of a different type than the one
--     provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not equal @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @LE@ : Less than or equal.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ element
--     of type String, Number, or Binary (not a set type). If an item
--     contains an /AttributeValue/ element of a different type than the
--     one provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @LT@ : Less than.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ of type
--     String, Number, or Binary (not a set type). If an item contains an
--     /AttributeValue/ element of a different type than the one provided
--     in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @GE@ : Greater than or equal.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ element
--     of type String, Number, or Binary (not a set type). If an item
--     contains an /AttributeValue/ element of a different type than the
--     one provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @GT@ : Greater than.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ element
--     of type String, Number, or Binary (not a set type). If an item
--     contains an /AttributeValue/ element of a different type than the
--     one provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @NOT_NULL@ : The attribute exists. @NOT_NULL@ is supported for all
--     datatypes, including lists and maps.
--
--     This operator tests for the existence of an attribute, not its data
--     type. If the data type of attribute \"@a@\" is null, and you
--     evaluate it using @NOT_NULL@, the result is a Boolean /true/. This
--     result is because the attribute \"@a@\" exists; its data type is not
--     relevant to the @NOT_NULL@ comparison operator.
--
-- -   @NULL@ : The attribute does not exist. @NULL@ is supported for all
--     datatypes, including lists and maps.
--
--     This operator tests for the nonexistence of an attribute, not its
--     data type. If the data type of attribute \"@a@\" is null, and you
--     evaluate it using @NULL@, the result is a Boolean /false/. This is
--     because the attribute \"@a@\" exists; its data type is not relevant
--     to the @NULL@ comparison operator.
--
-- -   @CONTAINS@ : Checks for a subsequence, or value in a set.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ element
--     of type String, Number, or Binary (not a set type). If the target
--     attribute of the comparison is of type String, then the operator
--     checks for a substring match. If the target attribute of the
--     comparison is of type Binary, then the operator looks for a
--     subsequence of the target that matches the input. If the target
--     attribute of the comparison is a set (\"@SS@\", \"@NS@\", or
--     \"@BS@\"), then the operator evaluates to true if it finds an exact
--     match with any member of the set.
--
--     CONTAINS is supported for lists: When evaluating \"@a CONTAINS b@\",
--     \"@a@\" can be a list; however, \"@b@\" cannot be a set, a map, or a
--     list.
--
-- -   @NOT_CONTAINS@ : Checks for absence of a subsequence, or absence of
--     a value in a set.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ element
--     of type String, Number, or Binary (not a set type). If the target
--     attribute of the comparison is a String, then the operator checks
--     for the absence of a substring match. If the target attribute of the
--     comparison is Binary, then the operator checks for the absence of a
--     subsequence of the target that matches the input. If the target
--     attribute of the comparison is a set (\"@SS@\", \"@NS@\", or
--     \"@BS@\"), then the operator evaluates to true if it /does not/ find
--     an exact match with any member of the set.
--
--     NOT_CONTAINS is supported for lists: When evaluating
--     \"@a NOT CONTAINS b@\", \"@a@\" can be a list; however, \"@b@\"
--     cannot be a set, a map, or a list.
--
-- -   @BEGINS_WITH@ : Checks for a prefix.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ of type
--     String or Binary (not a Number or a set type). The target attribute
--     of the comparison must be of type String or Binary (not a Number or
--     a set type).
--
-- -   @IN@ : Checks for matching elements within two sets.
--
--     /AttributeValueList/ can contain one or more /AttributeValue/
--     elements of type String, Number, or Binary (not a set type). These
--     attributes are compared against an existing set type attribute of an
--     item. If any elements of the input set are present in the item
--     attribute, the expression evaluates to true.
--
-- -   @BETWEEN@ : Greater than or equal to the first value, and less than
--     or equal to the second value.
--
--     /AttributeValueList/ must contain two /AttributeValue/ elements of
--     the same type, either String, Number, or Binary (not a set type). A
--     target attribute matches if the target value is greater than, or
--     equal to, the first element and less than, or equal to, the second
--     element. If an item contains an /AttributeValue/ element of a
--     different type than the one provided in the request, the value does
--     not match. For example, @{\"S\":\"6\"}@ does not compare to
--     @{\"N\":\"6\"}@. Also, @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@
--
-- For usage examples of /AttributeValueList/ and /ComparisonOperator/, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters>
-- in the /Amazon DynamoDB Developer Guide/.
conComparisonOperator :: Lens' Condition ComparisonOperator
conComparisonOperator = lens _conComparisonOperator (\ s a -> s{_conComparisonOperator = a});

instance ToJSON Condition where
        toJSON Condition'{..}
          = object
              ["AttributeValueList" .= _conAttributeValueList,
               "ComparisonOperator" .= _conComparisonOperator]

-- | The capacity units consumed by an operation. The data returned includes
-- the total provisioned throughput consumed, along with statistics for the
-- table and any indexes involved in the operation. /ConsumedCapacity/ is
-- only returned if the request asked for it. For more information, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- /See:/ 'consumedCapacity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccCapacityUnits'
--
-- * 'ccGlobalSecondaryIndexes'
--
-- * 'ccLocalSecondaryIndexes'
--
-- * 'ccTable'
--
-- * 'ccTableName'
data ConsumedCapacity = ConsumedCapacity'
    { _ccCapacityUnits          :: !(Maybe Double)
    , _ccGlobalSecondaryIndexes :: !(Maybe (Map Text Capacity))
    , _ccLocalSecondaryIndexes  :: !(Maybe (Map Text Capacity))
    , _ccTable                  :: !(Maybe Capacity)
    , _ccTableName              :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ConsumedCapacity' smart constructor.
consumedCapacity :: ConsumedCapacity
consumedCapacity =
    ConsumedCapacity'
    { _ccCapacityUnits = Nothing
    , _ccGlobalSecondaryIndexes = Nothing
    , _ccLocalSecondaryIndexes = Nothing
    , _ccTable = Nothing
    , _ccTableName = Nothing
    }

-- | The total number of capacity units consumed by the operation.
ccCapacityUnits :: Lens' ConsumedCapacity (Maybe Double)
ccCapacityUnits = lens _ccCapacityUnits (\ s a -> s{_ccCapacityUnits = a});

-- | The amount of throughput consumed on each global index affected by the
-- operation.
ccGlobalSecondaryIndexes :: Lens' ConsumedCapacity (HashMap Text Capacity)
ccGlobalSecondaryIndexes = lens _ccGlobalSecondaryIndexes (\ s a -> s{_ccGlobalSecondaryIndexes = a}) . _Default . _Map;

-- | The amount of throughput consumed on each local index affected by the
-- operation.
ccLocalSecondaryIndexes :: Lens' ConsumedCapacity (HashMap Text Capacity)
ccLocalSecondaryIndexes = lens _ccLocalSecondaryIndexes (\ s a -> s{_ccLocalSecondaryIndexes = a}) . _Default . _Map;

-- | The amount of throughput consumed on the table affected by the
-- operation.
ccTable :: Lens' ConsumedCapacity (Maybe Capacity)
ccTable = lens _ccTable (\ s a -> s{_ccTable = a});

-- | The name of the table that was affected by the operation.
ccTableName :: Lens' ConsumedCapacity (Maybe Text)
ccTableName = lens _ccTableName (\ s a -> s{_ccTableName = a});

instance FromJSON ConsumedCapacity where
        parseJSON
          = withObject "ConsumedCapacity"
              (\ x ->
                 ConsumedCapacity' <$>
                   (x .:? "CapacityUnits") <*>
                     (x .:? "GlobalSecondaryIndexes" .!= mempty)
                     <*> (x .:? "LocalSecondaryIndexes" .!= mempty)
                     <*> (x .:? "Table")
                     <*> (x .:? "TableName"))

-- | Represents a new global secondary index to be added to an existing
-- table.
--
-- /See:/ 'createGlobalSecondaryIndexAction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cgsiaIndexName'
--
-- * 'cgsiaKeySchema'
--
-- * 'cgsiaProjection'
--
-- * 'cgsiaProvisionedThroughput'
data CreateGlobalSecondaryIndexAction = CreateGlobalSecondaryIndexAction'
    { _cgsiaIndexName             :: !Text
    , _cgsiaKeySchema             :: !(List1 KeySchemaElement)
    , _cgsiaProjection            :: !Projection
    , _cgsiaProvisionedThroughput :: !ProvisionedThroughput
    } deriving (Eq,Read,Show)

-- | 'CreateGlobalSecondaryIndexAction' smart constructor.
createGlobalSecondaryIndexAction :: Text -> NonEmpty KeySchemaElement -> Projection -> ProvisionedThroughput -> CreateGlobalSecondaryIndexAction
createGlobalSecondaryIndexAction pIndexName pKeySchema pProjection pProvisionedThroughput =
    CreateGlobalSecondaryIndexAction'
    { _cgsiaIndexName = pIndexName
    , _cgsiaKeySchema = _List1 # pKeySchema
    , _cgsiaProjection = pProjection
    , _cgsiaProvisionedThroughput = pProvisionedThroughput
    }

-- | The name of the global secondary index to be created.
cgsiaIndexName :: Lens' CreateGlobalSecondaryIndexAction Text
cgsiaIndexName = lens _cgsiaIndexName (\ s a -> s{_cgsiaIndexName = a});

-- | The key schema for the global secondary index.
cgsiaKeySchema :: Lens' CreateGlobalSecondaryIndexAction (NonEmpty KeySchemaElement)
cgsiaKeySchema = lens _cgsiaKeySchema (\ s a -> s{_cgsiaKeySchema = a}) . _List1;

-- | FIXME: Undocumented member.
cgsiaProjection :: Lens' CreateGlobalSecondaryIndexAction Projection
cgsiaProjection = lens _cgsiaProjection (\ s a -> s{_cgsiaProjection = a});

-- | FIXME: Undocumented member.
cgsiaProvisionedThroughput :: Lens' CreateGlobalSecondaryIndexAction ProvisionedThroughput
cgsiaProvisionedThroughput = lens _cgsiaProvisionedThroughput (\ s a -> s{_cgsiaProvisionedThroughput = a});

instance ToJSON CreateGlobalSecondaryIndexAction
         where
        toJSON CreateGlobalSecondaryIndexAction'{..}
          = object
              ["IndexName" .= _cgsiaIndexName,
               "KeySchema" .= _cgsiaKeySchema,
               "Projection" .= _cgsiaProjection,
               "ProvisionedThroughput" .=
                 _cgsiaProvisionedThroughput]

-- | Represents a global secondary index to be deleted from an existing
-- table.
--
-- /See:/ 'deleteGlobalSecondaryIndexAction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgsiaIndexName'
newtype DeleteGlobalSecondaryIndexAction = DeleteGlobalSecondaryIndexAction'
    { _dgsiaIndexName :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteGlobalSecondaryIndexAction' smart constructor.
deleteGlobalSecondaryIndexAction :: Text -> DeleteGlobalSecondaryIndexAction
deleteGlobalSecondaryIndexAction pIndexName =
    DeleteGlobalSecondaryIndexAction'
    { _dgsiaIndexName = pIndexName
    }

-- | The name of the global secondary index to be deleted.
dgsiaIndexName :: Lens' DeleteGlobalSecondaryIndexAction Text
dgsiaIndexName = lens _dgsiaIndexName (\ s a -> s{_dgsiaIndexName = a});

instance ToJSON DeleteGlobalSecondaryIndexAction
         where
        toJSON DeleteGlobalSecondaryIndexAction'{..}
          = object ["IndexName" .= _dgsiaIndexName]

-- | Represents a request to perform a /DeleteItem/ operation on an item.
--
-- /See:/ 'deleteRequest' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drKey'
newtype DeleteRequest = DeleteRequest'
    { _drKey :: Map Text AttributeValue
    } deriving (Eq,Show)

-- | 'DeleteRequest' smart constructor.
deleteRequest :: DeleteRequest
deleteRequest =
    DeleteRequest'
    { _drKey = mempty
    }

-- | A map of attribute name to attribute values, representing the primary
-- key of the item to delete. All of the table\'s primary key attributes
-- must be specified, and their data types must match those of the table\'s
-- key schema.
drKey :: Lens' DeleteRequest (HashMap Text AttributeValue)
drKey = lens _drKey (\ s a -> s{_drKey = a}) . _Map;

instance FromJSON DeleteRequest where
        parseJSON
          = withObject "DeleteRequest"
              (\ x -> DeleteRequest' <$> (x .:? "Key" .!= mempty))

instance ToJSON DeleteRequest where
        toJSON DeleteRequest'{..} = object ["Key" .= _drKey]

-- | Represents a condition to be compared with an attribute value. This
-- condition can be used with /DeleteItem/, /PutItem/ or /UpdateItem/
-- operations; if the comparison evaluates to true, the operation succeeds;
-- if not, the operation fails. You can use /ExpectedAttributeValue/ in one
-- of two different ways:
--
-- -   Use /AttributeValueList/ to specify one or more values to compare
--     against an attribute. Use /ComparisonOperator/ to specify how you
--     want to perform the comparison. If the comparison evaluates to true,
--     then the conditional operation succeeds.
--
-- -   Use /Value/ to specify a value that DynamoDB will compare against an
--     attribute. If the values match, then /ExpectedAttributeValue/
--     evaluates to true and the conditional operation succeeds.
--     Optionally, you can also set /Exists/ to false, indicating that you
--     /do not/ expect to find the attribute value in the table. In this
--     case, the conditional operation succeeds only if the comparison
--     evaluates to false.
--
-- /Value/ and /Exists/ are incompatible with /AttributeValueList/ and
-- /ComparisonOperator/. Note that if you use both sets of parameters at
-- once, DynamoDB will return a /ValidationException/ exception.
--
-- /See:/ 'expectedAttributeValue' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eavAttributeValueList'
--
-- * 'eavExists'
--
-- * 'eavValue'
--
-- * 'eavComparisonOperator'
data ExpectedAttributeValue = ExpectedAttributeValue'
    { _eavAttributeValueList :: !(Maybe [AttributeValue])
    , _eavExists             :: !(Maybe Bool)
    , _eavValue              :: !(Maybe AttributeValue)
    , _eavComparisonOperator :: !(Maybe ComparisonOperator)
    } deriving (Eq,Show)

-- | 'ExpectedAttributeValue' smart constructor.
expectedAttributeValue :: ExpectedAttributeValue
expectedAttributeValue =
    ExpectedAttributeValue'
    { _eavAttributeValueList = Nothing
    , _eavExists = Nothing
    , _eavValue = Nothing
    , _eavComparisonOperator = Nothing
    }

-- | One or more values to evaluate against the supplied attribute. The
-- number of values in the list depends on the /ComparisonOperator/ being
-- used.
--
-- For type Number, value comparisons are numeric.
--
-- String value comparisons for greater than, equals, or less than are
-- based on ASCII character code values. For example, @a@ is greater than
-- @A@, and @a@ is greater than @B@. For a list of code values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
--
-- For Binary, DynamoDB treats each byte of the binary data as unsigned
-- when it compares binary values.
--
-- For information on specifying data types in JSON, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataFormat.html JSON Data Format>
-- in the /Amazon DynamoDB Developer Guide/.
eavAttributeValueList :: Lens' ExpectedAttributeValue [AttributeValue]
eavAttributeValueList = lens _eavAttributeValueList (\ s a -> s{_eavAttributeValueList = a}) . _Default;

-- | Causes DynamoDB to evaluate the value before attempting a conditional
-- operation:
--
-- -   If /Exists/ is @true@, DynamoDB will check to see if that attribute
--     value already exists in the table. If it is found, then the
--     operation succeeds. If it is not found, the operation fails with a
--     /ConditionalCheckFailedException/.
--
-- -   If /Exists/ is @false@, DynamoDB assumes that the attribute value
--     does not exist in the table. If in fact the value does not exist,
--     then the assumption is valid and the operation succeeds. If the
--     value is found, despite the assumption that it does not exist, the
--     operation fails with a /ConditionalCheckFailedException/.
--
-- The default setting for /Exists/ is @true@. If you supply a /Value/ all
-- by itself, DynamoDB assumes the attribute exists: You don\'t have to set
-- /Exists/ to @true@, because it is implied.
--
-- DynamoDB returns a /ValidationException/ if:
--
-- -   /Exists/ is @true@ but there is no /Value/ to check. (You expect a
--     value to exist, but don\'t specify what that value is.)
--
-- -   /Exists/ is @false@ but you also provide a /Value/. (You cannot
--     expect an attribute to have a value, while also expecting it not to
--     exist.)
--
eavExists :: Lens' ExpectedAttributeValue (Maybe Bool)
eavExists = lens _eavExists (\ s a -> s{_eavExists = a});

-- | FIXME: Undocumented member.
eavValue :: Lens' ExpectedAttributeValue (Maybe AttributeValue)
eavValue = lens _eavValue (\ s a -> s{_eavValue = a});

-- | A comparator for evaluating attributes in the /AttributeValueList/. For
-- example, equals, greater than, less than, etc.
--
-- The following comparison operators are available:
--
-- @EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN@
--
-- The following are descriptions of each comparison operator.
--
-- -   @EQ@ : Equal. @EQ@ is supported for all datatypes, including lists
--     and maps.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ element
--     of type String, Number, Binary, String Set, Number Set, or Binary
--     Set. If an item contains an /AttributeValue/ element of a different
--     type than the one provided in the request, the value does not match.
--     For example, @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not equal @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @NE@ : Not equal. @NE@ is supported for all datatypes, including
--     lists and maps.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ of type
--     String, Number, Binary, String Set, Number Set, or Binary Set. If an
--     item contains an /AttributeValue/ of a different type than the one
--     provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not equal @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @LE@ : Less than or equal.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ element
--     of type String, Number, or Binary (not a set type). If an item
--     contains an /AttributeValue/ element of a different type than the
--     one provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @LT@ : Less than.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ of type
--     String, Number, or Binary (not a set type). If an item contains an
--     /AttributeValue/ element of a different type than the one provided
--     in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @GE@ : Greater than or equal.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ element
--     of type String, Number, or Binary (not a set type). If an item
--     contains an /AttributeValue/ element of a different type than the
--     one provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @GT@ : Greater than.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ element
--     of type String, Number, or Binary (not a set type). If an item
--     contains an /AttributeValue/ element of a different type than the
--     one provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @NOT_NULL@ : The attribute exists. @NOT_NULL@ is supported for all
--     datatypes, including lists and maps.
--
--     This operator tests for the existence of an attribute, not its data
--     type. If the data type of attribute \"@a@\" is null, and you
--     evaluate it using @NOT_NULL@, the result is a Boolean /true/. This
--     result is because the attribute \"@a@\" exists; its data type is not
--     relevant to the @NOT_NULL@ comparison operator.
--
-- -   @NULL@ : The attribute does not exist. @NULL@ is supported for all
--     datatypes, including lists and maps.
--
--     This operator tests for the nonexistence of an attribute, not its
--     data type. If the data type of attribute \"@a@\" is null, and you
--     evaluate it using @NULL@, the result is a Boolean /false/. This is
--     because the attribute \"@a@\" exists; its data type is not relevant
--     to the @NULL@ comparison operator.
--
-- -   @CONTAINS@ : Checks for a subsequence, or value in a set.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ element
--     of type String, Number, or Binary (not a set type). If the target
--     attribute of the comparison is of type String, then the operator
--     checks for a substring match. If the target attribute of the
--     comparison is of type Binary, then the operator looks for a
--     subsequence of the target that matches the input. If the target
--     attribute of the comparison is a set (\"@SS@\", \"@NS@\", or
--     \"@BS@\"), then the operator evaluates to true if it finds an exact
--     match with any member of the set.
--
--     CONTAINS is supported for lists: When evaluating \"@a CONTAINS b@\",
--     \"@a@\" can be a list; however, \"@b@\" cannot be a set, a map, or a
--     list.
--
-- -   @NOT_CONTAINS@ : Checks for absence of a subsequence, or absence of
--     a value in a set.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ element
--     of type String, Number, or Binary (not a set type). If the target
--     attribute of the comparison is a String, then the operator checks
--     for the absence of a substring match. If the target attribute of the
--     comparison is Binary, then the operator checks for the absence of a
--     subsequence of the target that matches the input. If the target
--     attribute of the comparison is a set (\"@SS@\", \"@NS@\", or
--     \"@BS@\"), then the operator evaluates to true if it /does not/ find
--     an exact match with any member of the set.
--
--     NOT_CONTAINS is supported for lists: When evaluating
--     \"@a NOT CONTAINS b@\", \"@a@\" can be a list; however, \"@b@\"
--     cannot be a set, a map, or a list.
--
-- -   @BEGINS_WITH@ : Checks for a prefix.
--
--     /AttributeValueList/ can contain only one /AttributeValue/ of type
--     String or Binary (not a Number or a set type). The target attribute
--     of the comparison must be of type String or Binary (not a Number or
--     a set type).
--
-- -   @IN@ : Checks for matching elements within two sets.
--
--     /AttributeValueList/ can contain one or more /AttributeValue/
--     elements of type String, Number, or Binary (not a set type). These
--     attributes are compared against an existing set type attribute of an
--     item. If any elements of the input set are present in the item
--     attribute, the expression evaluates to true.
--
-- -   @BETWEEN@ : Greater than or equal to the first value, and less than
--     or equal to the second value.
--
--     /AttributeValueList/ must contain two /AttributeValue/ elements of
--     the same type, either String, Number, or Binary (not a set type). A
--     target attribute matches if the target value is greater than, or
--     equal to, the first element and less than, or equal to, the second
--     element. If an item contains an /AttributeValue/ element of a
--     different type than the one provided in the request, the value does
--     not match. For example, @{\"S\":\"6\"}@ does not compare to
--     @{\"N\":\"6\"}@. Also, @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@
--
eavComparisonOperator :: Lens' ExpectedAttributeValue (Maybe ComparisonOperator)
eavComparisonOperator = lens _eavComparisonOperator (\ s a -> s{_eavComparisonOperator = a});

instance ToJSON ExpectedAttributeValue where
        toJSON ExpectedAttributeValue'{..}
          = object
              ["AttributeValueList" .= _eavAttributeValueList,
               "Exists" .= _eavExists, "Value" .= _eavValue,
               "ComparisonOperator" .= _eavComparisonOperator]

-- | Represents the properties of a global secondary index.
--
-- /See:/ 'globalSecondaryIndex' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsiIndexName'
--
-- * 'gsiKeySchema'
--
-- * 'gsiProjection'
--
-- * 'gsiProvisionedThroughput'
data GlobalSecondaryIndex = GlobalSecondaryIndex'
    { _gsiIndexName             :: !Text
    , _gsiKeySchema             :: !(List1 KeySchemaElement)
    , _gsiProjection            :: !Projection
    , _gsiProvisionedThroughput :: !ProvisionedThroughput
    } deriving (Eq,Read,Show)

-- | 'GlobalSecondaryIndex' smart constructor.
globalSecondaryIndex :: Text -> NonEmpty KeySchemaElement -> Projection -> ProvisionedThroughput -> GlobalSecondaryIndex
globalSecondaryIndex pIndexName pKeySchema pProjection pProvisionedThroughput =
    GlobalSecondaryIndex'
    { _gsiIndexName = pIndexName
    , _gsiKeySchema = _List1 # pKeySchema
    , _gsiProjection = pProjection
    , _gsiProvisionedThroughput = pProvisionedThroughput
    }

-- | The name of the global secondary index. The name must be unique among
-- all other indexes on this table.
gsiIndexName :: Lens' GlobalSecondaryIndex Text
gsiIndexName = lens _gsiIndexName (\ s a -> s{_gsiIndexName = a});

-- | The complete key schema for a global secondary index, which consists of
-- one or more pairs of attribute names and key types (@HASH@ or @RANGE@).
gsiKeySchema :: Lens' GlobalSecondaryIndex (NonEmpty KeySchemaElement)
gsiKeySchema = lens _gsiKeySchema (\ s a -> s{_gsiKeySchema = a}) . _List1;

-- | FIXME: Undocumented member.
gsiProjection :: Lens' GlobalSecondaryIndex Projection
gsiProjection = lens _gsiProjection (\ s a -> s{_gsiProjection = a});

-- | FIXME: Undocumented member.
gsiProvisionedThroughput :: Lens' GlobalSecondaryIndex ProvisionedThroughput
gsiProvisionedThroughput = lens _gsiProvisionedThroughput (\ s a -> s{_gsiProvisionedThroughput = a});

instance ToJSON GlobalSecondaryIndex where
        toJSON GlobalSecondaryIndex'{..}
          = object
              ["IndexName" .= _gsiIndexName,
               "KeySchema" .= _gsiKeySchema,
               "Projection" .= _gsiProjection,
               "ProvisionedThroughput" .= _gsiProvisionedThroughput]

-- | Represents the properties of a global secondary index.
--
-- /See:/ 'globalSecondaryIndexDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsidBackfilling'
--
-- * 'gsidProvisionedThroughput'
--
-- * 'gsidIndexStatus'
--
-- * 'gsidIndexSizeBytes'
--
-- * 'gsidKeySchema'
--
-- * 'gsidProjection'
--
-- * 'gsidItemCount'
--
-- * 'gsidIndexName'
data GlobalSecondaryIndexDescription = GlobalSecondaryIndexDescription'
    { _gsidBackfilling           :: !(Maybe Bool)
    , _gsidProvisionedThroughput :: !(Maybe ProvisionedThroughputDescription)
    , _gsidIndexStatus           :: !(Maybe IndexStatus)
    , _gsidIndexSizeBytes        :: !(Maybe Integer)
    , _gsidKeySchema             :: !(Maybe (List1 KeySchemaElement))
    , _gsidProjection            :: !(Maybe Projection)
    , _gsidItemCount             :: !(Maybe Integer)
    , _gsidIndexName             :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'GlobalSecondaryIndexDescription' smart constructor.
globalSecondaryIndexDescription :: GlobalSecondaryIndexDescription
globalSecondaryIndexDescription =
    GlobalSecondaryIndexDescription'
    { _gsidBackfilling = Nothing
    , _gsidProvisionedThroughput = Nothing
    , _gsidIndexStatus = Nothing
    , _gsidIndexSizeBytes = Nothing
    , _gsidKeySchema = Nothing
    , _gsidProjection = Nothing
    , _gsidItemCount = Nothing
    , _gsidIndexName = Nothing
    }

-- | Indicates whether the index is currently backfilling. /Backfilling/ is
-- the process of reading items from the table and determining whether they
-- can be added to the index. (Not all items will qualify: For example, a
-- hash key attribute cannot have any duplicates.) If an item can be added
-- to the index, DynamoDB will do so. After all items have been processed,
-- the backfilling operation is complete and /Backfilling/ is false.
--
-- For indexes that were created during a /CreateTable/ operation, the
-- /Backfilling/ attribute does not appear in the /DescribeTable/ output.
gsidBackfilling :: Lens' GlobalSecondaryIndexDescription (Maybe Bool)
gsidBackfilling = lens _gsidBackfilling (\ s a -> s{_gsidBackfilling = a});

-- | FIXME: Undocumented member.
gsidProvisionedThroughput :: Lens' GlobalSecondaryIndexDescription (Maybe ProvisionedThroughputDescription)
gsidProvisionedThroughput = lens _gsidProvisionedThroughput (\ s a -> s{_gsidProvisionedThroughput = a});

-- | The current state of the global secondary index:
--
-- -   /CREATING/ - The index is being created.
--
-- -   /UPDATING/ - The index is being updated.
--
-- -   /DELETING/ - The index is being deleted.
--
-- -   /ACTIVE/ - The index is ready for use.
--
gsidIndexStatus :: Lens' GlobalSecondaryIndexDescription (Maybe IndexStatus)
gsidIndexStatus = lens _gsidIndexStatus (\ s a -> s{_gsidIndexStatus = a});

-- | The total size of the specified index, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be
-- reflected in this value.
gsidIndexSizeBytes :: Lens' GlobalSecondaryIndexDescription (Maybe Integer)
gsidIndexSizeBytes = lens _gsidIndexSizeBytes (\ s a -> s{_gsidIndexSizeBytes = a});

-- | The complete key schema for the global secondary index, consisting of
-- one or more pairs of attribute names and key types (@HASH@ or @RANGE@).
gsidKeySchema :: Lens' GlobalSecondaryIndexDescription (Maybe (NonEmpty KeySchemaElement))
gsidKeySchema = lens _gsidKeySchema (\ s a -> s{_gsidKeySchema = a}) . mapping _List1;

-- | FIXME: Undocumented member.
gsidProjection :: Lens' GlobalSecondaryIndexDescription (Maybe Projection)
gsidProjection = lens _gsidProjection (\ s a -> s{_gsidProjection = a});

-- | The number of items in the specified index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
gsidItemCount :: Lens' GlobalSecondaryIndexDescription (Maybe Integer)
gsidItemCount = lens _gsidItemCount (\ s a -> s{_gsidItemCount = a});

-- | The name of the global secondary index.
gsidIndexName :: Lens' GlobalSecondaryIndexDescription (Maybe Text)
gsidIndexName = lens _gsidIndexName (\ s a -> s{_gsidIndexName = a});

instance FromJSON GlobalSecondaryIndexDescription
         where
        parseJSON
          = withObject "GlobalSecondaryIndexDescription"
              (\ x ->
                 GlobalSecondaryIndexDescription' <$>
                   (x .:? "Backfilling") <*>
                     (x .:? "ProvisionedThroughput")
                     <*> (x .:? "IndexStatus")
                     <*> (x .:? "IndexSizeBytes")
                     <*> (x .:? "KeySchema")
                     <*> (x .:? "Projection")
                     <*> (x .:? "ItemCount")
                     <*> (x .:? "IndexName"))

-- | Represents one of the following:
--
-- -   A new global secondary index to be added to an existing table.
--
-- -   New provisioned throughput parameters for an existing global
--     secondary index.
--
-- -   An existing global secondary index to be removed from an existing
--     table.
--
--
-- /See:/ 'globalSecondaryIndexUpdate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsiuCreate'
--
-- * 'gsiuDelete'
--
-- * 'gsiuUpdate'
data GlobalSecondaryIndexUpdate = GlobalSecondaryIndexUpdate'
    { _gsiuCreate :: !(Maybe CreateGlobalSecondaryIndexAction)
    , _gsiuDelete :: !(Maybe DeleteGlobalSecondaryIndexAction)
    , _gsiuUpdate :: !(Maybe UpdateGlobalSecondaryIndexAction)
    } deriving (Eq,Read,Show)

-- | 'GlobalSecondaryIndexUpdate' smart constructor.
globalSecondaryIndexUpdate :: GlobalSecondaryIndexUpdate
globalSecondaryIndexUpdate =
    GlobalSecondaryIndexUpdate'
    { _gsiuCreate = Nothing
    , _gsiuDelete = Nothing
    , _gsiuUpdate = Nothing
    }

-- | The parameters required for creating a global secondary index on an
-- existing table:
--
-- -   @IndexName @
--
-- -   @KeySchema @
--
-- -   @AttributeDefinitions @
--
-- -   @Projection @
--
-- -   @ProvisionedThroughput @
--
gsiuCreate :: Lens' GlobalSecondaryIndexUpdate (Maybe CreateGlobalSecondaryIndexAction)
gsiuCreate = lens _gsiuCreate (\ s a -> s{_gsiuCreate = a});

-- | The name of an existing global secondary index to be removed.
gsiuDelete :: Lens' GlobalSecondaryIndexUpdate (Maybe DeleteGlobalSecondaryIndexAction)
gsiuDelete = lens _gsiuDelete (\ s a -> s{_gsiuDelete = a});

-- | The name of an existing global secondary index, along with new
-- provisioned throughput settings to be applied to that index.
gsiuUpdate :: Lens' GlobalSecondaryIndexUpdate (Maybe UpdateGlobalSecondaryIndexAction)
gsiuUpdate = lens _gsiuUpdate (\ s a -> s{_gsiuUpdate = a});

instance ToJSON GlobalSecondaryIndexUpdate where
        toJSON GlobalSecondaryIndexUpdate'{..}
          = object
              ["Create" .= _gsiuCreate, "Delete" .= _gsiuDelete,
               "Update" .= _gsiuUpdate]

-- | Information about item collections, if any, that were affected by the
-- operation. /ItemCollectionMetrics/ is only returned if the request asked
-- for it. If the table does not have any local secondary indexes, this
-- information is not returned in the response.
--
-- /See:/ 'itemCollectionMetrics' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'icmItemCollectionKey'
--
-- * 'icmSizeEstimateRangeGB'
data ItemCollectionMetrics = ItemCollectionMetrics'
    { _icmItemCollectionKey   :: !(Maybe (Map Text AttributeValue))
    , _icmSizeEstimateRangeGB :: !(Maybe [Double])
    } deriving (Eq,Show)

-- | 'ItemCollectionMetrics' smart constructor.
itemCollectionMetrics :: ItemCollectionMetrics
itemCollectionMetrics =
    ItemCollectionMetrics'
    { _icmItemCollectionKey = Nothing
    , _icmSizeEstimateRangeGB = Nothing
    }

-- | The hash key value of the item collection. This value is the same as the
-- hash key of the item.
icmItemCollectionKey :: Lens' ItemCollectionMetrics (HashMap Text AttributeValue)
icmItemCollectionKey = lens _icmItemCollectionKey (\ s a -> s{_icmItemCollectionKey = a}) . _Default . _Map;

-- | An estimate of item collection size, in gigabytes. This value is a
-- two-element array containing a lower bound and an upper bound for the
-- estimate. The estimate includes the size of all the items in the table,
-- plus the size of all attributes projected into all of the local
-- secondary indexes on that table. Use this estimate to measure whether a
-- local secondary index is approaching its size limit.
--
-- The estimate is subject to change over time; therefore, do not rely on
-- the precision or accuracy of the estimate.
icmSizeEstimateRangeGB :: Lens' ItemCollectionMetrics [Double]
icmSizeEstimateRangeGB = lens _icmSizeEstimateRangeGB (\ s a -> s{_icmSizeEstimateRangeGB = a}) . _Default;

instance FromJSON ItemCollectionMetrics where
        parseJSON
          = withObject "ItemCollectionMetrics"
              (\ x ->
                 ItemCollectionMetrics' <$>
                   (x .:? "ItemCollectionKey" .!= mempty) <*>
                     (x .:? "SizeEstimateRangeGB" .!= mempty))

-- | Represents /a single element/ of a key schema. A key schema specifies
-- the attributes that make up the primary key of a table, or the key
-- attributes of an index.
--
-- A /KeySchemaElement/ represents exactly one attribute of the primary
-- key. For example, a hash type primary key would be represented by one
-- /KeySchemaElement/. A hash-and-range type primary key would require one
-- /KeySchemaElement/ for the hash attribute, and another
-- /KeySchemaElement/ for the range attribute.
--
-- /See:/ 'keySchemaElement' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kseAttributeName'
--
-- * 'kseKeyType'
data KeySchemaElement = KeySchemaElement'
    { _kseAttributeName :: !Text
    , _kseKeyType       :: !KeyType
    } deriving (Eq,Read,Show)

-- | 'KeySchemaElement' smart constructor.
keySchemaElement :: Text -> KeyType -> KeySchemaElement
keySchemaElement pAttributeName pKeyType =
    KeySchemaElement'
    { _kseAttributeName = pAttributeName
    , _kseKeyType = pKeyType
    }

-- | The name of a key attribute.
kseAttributeName :: Lens' KeySchemaElement Text
kseAttributeName = lens _kseAttributeName (\ s a -> s{_kseAttributeName = a});

-- | The attribute data, consisting of the data type and the attribute value
-- itself.
kseKeyType :: Lens' KeySchemaElement KeyType
kseKeyType = lens _kseKeyType (\ s a -> s{_kseKeyType = a});

instance FromJSON KeySchemaElement where
        parseJSON
          = withObject "KeySchemaElement"
              (\ x ->
                 KeySchemaElement' <$>
                   (x .: "AttributeName") <*> (x .: "KeyType"))

instance ToJSON KeySchemaElement where
        toJSON KeySchemaElement'{..}
          = object
              ["AttributeName" .= _kseAttributeName,
               "KeyType" .= _kseKeyType]

-- | Represents a set of primary keys and, for each key, the attributes to
-- retrieve from the table.
--
-- For each primary key, you must provide /all/ of the key attributes. For
-- example, with a hash type primary key, you only need to provide the hash
-- attribute. For a hash-and-range type primary key, you must provide
-- /both/ the hash attribute and the range attribute.
--
-- /See:/ 'keysAndAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kaaProjectionExpression'
--
-- * 'kaaConsistentRead'
--
-- * 'kaaExpressionAttributeNames'
--
-- * 'kaaAttributesToGet'
--
-- * 'kaaKeys'
data KeysAndAttributes = KeysAndAttributes'
    { _kaaProjectionExpression     :: !(Maybe Text)
    , _kaaConsistentRead           :: !(Maybe Bool)
    , _kaaExpressionAttributeNames :: !(Maybe (Map Text Text))
    , _kaaAttributesToGet          :: !(Maybe (List1 Text))
    , _kaaKeys                     :: !(List1 (Map Text AttributeValue))
    } deriving (Eq,Show)

-- | 'KeysAndAttributes' smart constructor.
keysAndAttributes :: NonEmpty (HashMap Text AttributeValue) -> KeysAndAttributes
keysAndAttributes pKeys =
    KeysAndAttributes'
    { _kaaProjectionExpression = Nothing
    , _kaaConsistentRead = Nothing
    , _kaaExpressionAttributeNames = Nothing
    , _kaaAttributesToGet = Nothing
    , _kaaKeys = _List1 # pKeys
    }

-- | A string that identifies one or more attributes to retrieve from the
-- table. These attributes can include scalars, sets, or elements of a JSON
-- document. The attributes in the /ProjectionExpression/ must be separated
-- by commas.
--
-- If no attribute names are specified, then all attributes will be
-- returned. If any of the requested attributes are not found, they will
-- not appear in the result.
--
-- For more information, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- /ProjectionExpression/ replaces the legacy /AttributesToGet/ parameter.
kaaProjectionExpression :: Lens' KeysAndAttributes (Maybe Text)
kaaProjectionExpression = lens _kaaProjectionExpression (\ s a -> s{_kaaProjectionExpression = a});

-- | The consistency of a read operation. If set to @true@, then a strongly
-- consistent read is used; otherwise, an eventually consistent read is
-- used.
kaaConsistentRead :: Lens' KeysAndAttributes (Maybe Bool)
kaaConsistentRead = lens _kaaConsistentRead (\ s a -> s{_kaaConsistentRead = a});

-- | One or more substitution tokens for attribute names in an expression.
-- The following are some use cases for using /ExpressionAttributeNames/:
--
-- -   To access an attribute whose name conflicts with a DynamoDB reserved
--     word.
--
-- -   To create a placeholder for repeating occurrences of an attribute
--     name in an expression.
--
-- -   To prevent special characters in an attribute name from being
--     misinterpreted in an expression.
--
-- Use the __#__ character in an expression to dereference an attribute
-- name. For example, consider the following attribute name:
--
-- -   @Percentile@
--
-- The name of this attribute conflicts with a reserved word, so it cannot
-- be used directly in an expression. (For the complete list of reserved
-- words, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
-- in the /Amazon DynamoDB Developer Guide/). To work around this, you
-- could specify the following for /ExpressionAttributeNames/:
--
-- -   @{\"#P\":\"Percentile\"}@
--
-- You could then use this substitution in an expression, as in this
-- example:
--
-- -   @#P = :val@
--
-- Tokens that begin with the __:__ character are /expression attribute
-- values/, which are placeholders for the actual value at runtime.
--
-- For more information on expression attribute names, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ExpressionPlaceholders.html Using Placeholders for Attribute Names and Values>
-- in the /Amazon DynamoDB Developer Guide/.
kaaExpressionAttributeNames :: Lens' KeysAndAttributes (HashMap Text Text)
kaaExpressionAttributeNames = lens _kaaExpressionAttributeNames (\ s a -> s{_kaaExpressionAttributeNames = a}) . _Default . _Map;

-- | One or more attributes to retrieve from the table or index. If no
-- attribute names are specified then all attributes will be returned. If
-- any of the specified attributes are not found, they will not appear in
-- the result.
kaaAttributesToGet :: Lens' KeysAndAttributes (Maybe (NonEmpty Text))
kaaAttributesToGet = lens _kaaAttributesToGet (\ s a -> s{_kaaAttributesToGet = a}) . mapping _List1;

-- | The primary key attribute values that define the items and the
-- attributes associated with the items.
kaaKeys :: Lens' KeysAndAttributes (NonEmpty (HashMap Text AttributeValue))
kaaKeys = lens _kaaKeys (\ s a -> s{_kaaKeys = a}) . _List1;

instance FromJSON KeysAndAttributes where
        parseJSON
          = withObject "KeysAndAttributes"
              (\ x ->
                 KeysAndAttributes' <$>
                   (x .:? "ProjectionExpression") <*>
                     (x .:? "ConsistentRead")
                     <*> (x .:? "ExpressionAttributeNames" .!= mempty)
                     <*> (x .:? "AttributesToGet")
                     <*> (x .: "Keys"))

instance ToJSON KeysAndAttributes where
        toJSON KeysAndAttributes'{..}
          = object
              ["ProjectionExpression" .= _kaaProjectionExpression,
               "ConsistentRead" .= _kaaConsistentRead,
               "ExpressionAttributeNames" .=
                 _kaaExpressionAttributeNames,
               "AttributesToGet" .= _kaaAttributesToGet,
               "Keys" .= _kaaKeys]

-- | Represents the properties of a local secondary index.
--
-- /See:/ 'localSecondaryIndex' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsiIndexName'
--
-- * 'lsiKeySchema'
--
-- * 'lsiProjection'
data LocalSecondaryIndex = LocalSecondaryIndex'
    { _lsiIndexName  :: !Text
    , _lsiKeySchema  :: !(List1 KeySchemaElement)
    , _lsiProjection :: !Projection
    } deriving (Eq,Read,Show)

-- | 'LocalSecondaryIndex' smart constructor.
localSecondaryIndex :: Text -> NonEmpty KeySchemaElement -> Projection -> LocalSecondaryIndex
localSecondaryIndex pIndexName pKeySchema pProjection =
    LocalSecondaryIndex'
    { _lsiIndexName = pIndexName
    , _lsiKeySchema = _List1 # pKeySchema
    , _lsiProjection = pProjection
    }

-- | The name of the local secondary index. The name must be unique among all
-- other indexes on this table.
lsiIndexName :: Lens' LocalSecondaryIndex Text
lsiIndexName = lens _lsiIndexName (\ s a -> s{_lsiIndexName = a});

-- | The complete key schema for the local secondary index, consisting of one
-- or more pairs of attribute names and key types (@HASH@ or @RANGE@).
lsiKeySchema :: Lens' LocalSecondaryIndex (NonEmpty KeySchemaElement)
lsiKeySchema = lens _lsiKeySchema (\ s a -> s{_lsiKeySchema = a}) . _List1;

-- | FIXME: Undocumented member.
lsiProjection :: Lens' LocalSecondaryIndex Projection
lsiProjection = lens _lsiProjection (\ s a -> s{_lsiProjection = a});

instance ToJSON LocalSecondaryIndex where
        toJSON LocalSecondaryIndex'{..}
          = object
              ["IndexName" .= _lsiIndexName,
               "KeySchema" .= _lsiKeySchema,
               "Projection" .= _lsiProjection]

-- | Represents the properties of a local secondary index.
--
-- /See:/ 'localSecondaryIndexDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsidIndexSizeBytes'
--
-- * 'lsidKeySchema'
--
-- * 'lsidProjection'
--
-- * 'lsidItemCount'
--
-- * 'lsidIndexName'
data LocalSecondaryIndexDescription = LocalSecondaryIndexDescription'
    { _lsidIndexSizeBytes :: !(Maybe Integer)
    , _lsidKeySchema      :: !(Maybe (List1 KeySchemaElement))
    , _lsidProjection     :: !(Maybe Projection)
    , _lsidItemCount      :: !(Maybe Integer)
    , _lsidIndexName      :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'LocalSecondaryIndexDescription' smart constructor.
localSecondaryIndexDescription :: LocalSecondaryIndexDescription
localSecondaryIndexDescription =
    LocalSecondaryIndexDescription'
    { _lsidIndexSizeBytes = Nothing
    , _lsidKeySchema = Nothing
    , _lsidProjection = Nothing
    , _lsidItemCount = Nothing
    , _lsidIndexName = Nothing
    }

-- | The total size of the specified index, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be
-- reflected in this value.
lsidIndexSizeBytes :: Lens' LocalSecondaryIndexDescription (Maybe Integer)
lsidIndexSizeBytes = lens _lsidIndexSizeBytes (\ s a -> s{_lsidIndexSizeBytes = a});

-- | The complete index key schema, which consists of one or more pairs of
-- attribute names and key types (@HASH@ or @RANGE@).
lsidKeySchema :: Lens' LocalSecondaryIndexDescription (Maybe (NonEmpty KeySchemaElement))
lsidKeySchema = lens _lsidKeySchema (\ s a -> s{_lsidKeySchema = a}) . mapping _List1;

-- | FIXME: Undocumented member.
lsidProjection :: Lens' LocalSecondaryIndexDescription (Maybe Projection)
lsidProjection = lens _lsidProjection (\ s a -> s{_lsidProjection = a});

-- | The number of items in the specified index. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
lsidItemCount :: Lens' LocalSecondaryIndexDescription (Maybe Integer)
lsidItemCount = lens _lsidItemCount (\ s a -> s{_lsidItemCount = a});

-- | Represents the name of the local secondary index.
lsidIndexName :: Lens' LocalSecondaryIndexDescription (Maybe Text)
lsidIndexName = lens _lsidIndexName (\ s a -> s{_lsidIndexName = a});

instance FromJSON LocalSecondaryIndexDescription
         where
        parseJSON
          = withObject "LocalSecondaryIndexDescription"
              (\ x ->
                 LocalSecondaryIndexDescription' <$>
                   (x .:? "IndexSizeBytes") <*> (x .:? "KeySchema") <*>
                     (x .:? "Projection")
                     <*> (x .:? "ItemCount")
                     <*> (x .:? "IndexName"))

-- | Represents attributes that are copied (projected) from the table into an
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected.
--
-- /See:/ 'projection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'proProjectionType'
--
-- * 'proNonKeyAttributes'
data Projection = Projection'
    { _proProjectionType   :: !(Maybe ProjectionType)
    , _proNonKeyAttributes :: !(Maybe (List1 Text))
    } deriving (Eq,Read,Show)

-- | 'Projection' smart constructor.
projection :: Projection
projection =
    Projection'
    { _proProjectionType = Nothing
    , _proNonKeyAttributes = Nothing
    }

-- | The set of attributes that are projected into the index:
--
-- -   @KEYS_ONLY@ - Only the index and primary keys are projected into the
--     index.
--
-- -   @INCLUDE@ - Only the specified table attributes are projected into
--     the index. The list of projected attributes are in
--     /NonKeyAttributes/.
--
-- -   @ALL@ - All of the table attributes are projected into the index.
--
proProjectionType :: Lens' Projection (Maybe ProjectionType)
proProjectionType = lens _proProjectionType (\ s a -> s{_proProjectionType = a});

-- | Represents the non-key attribute names which will be projected into the
-- index.
--
-- For local secondary indexes, the total count of /NonKeyAttributes/
-- summed across all of the local secondary indexes, must not exceed 20. If
-- you project the same attribute into two different indexes, this counts
-- as two distinct attributes when determining the total.
proNonKeyAttributes :: Lens' Projection (Maybe (NonEmpty Text))
proNonKeyAttributes = lens _proNonKeyAttributes (\ s a -> s{_proNonKeyAttributes = a}) . mapping _List1;

instance FromJSON Projection where
        parseJSON
          = withObject "Projection"
              (\ x ->
                 Projection' <$>
                   (x .:? "ProjectionType") <*>
                     (x .:? "NonKeyAttributes"))

instance ToJSON Projection where
        toJSON Projection'{..}
          = object
              ["ProjectionType" .= _proProjectionType,
               "NonKeyAttributes" .= _proNonKeyAttributes]

-- | Represents the provisioned throughput settings for a specified table or
-- index. The settings can be modified using the /UpdateTable/ operation.
--
-- For current minimum and maximum provisioned throughput values, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- /See:/ 'provisionedThroughput' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptReadCapacityUnits'
--
-- * 'ptWriteCapacityUnits'
data ProvisionedThroughput = ProvisionedThroughput'
    { _ptReadCapacityUnits  :: !Nat
    , _ptWriteCapacityUnits :: !Nat
    } deriving (Eq,Read,Show)

-- | 'ProvisionedThroughput' smart constructor.
provisionedThroughput :: Natural -> Natural -> ProvisionedThroughput
provisionedThroughput pReadCapacityUnits pWriteCapacityUnits =
    ProvisionedThroughput'
    { _ptReadCapacityUnits = _Nat # pReadCapacityUnits
    , _ptWriteCapacityUnits = _Nat # pWriteCapacityUnits
    }

-- | The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a /ThrottlingException/. For more information,
-- see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
ptReadCapacityUnits :: Lens' ProvisionedThroughput Natural
ptReadCapacityUnits = lens _ptReadCapacityUnits (\ s a -> s{_ptReadCapacityUnits = a}) . _Nat;

-- | The maximum number of writes consumed per second before DynamoDB returns
-- a /ThrottlingException/. For more information, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
ptWriteCapacityUnits :: Lens' ProvisionedThroughput Natural
ptWriteCapacityUnits = lens _ptWriteCapacityUnits (\ s a -> s{_ptWriteCapacityUnits = a}) . _Nat;

instance ToJSON ProvisionedThroughput where
        toJSON ProvisionedThroughput'{..}
          = object
              ["ReadCapacityUnits" .= _ptReadCapacityUnits,
               "WriteCapacityUnits" .= _ptWriteCapacityUnits]

-- | Represents the provisioned throughput settings for the table, consisting
-- of read and write capacity units, along with data about increases and
-- decreases.
--
-- /See:/ 'provisionedThroughputDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptdReadCapacityUnits'
--
-- * 'ptdLastDecreaseDateTime'
--
-- * 'ptdWriteCapacityUnits'
--
-- * 'ptdNumberOfDecreasesToday'
--
-- * 'ptdLastIncreaseDateTime'
data ProvisionedThroughputDescription = ProvisionedThroughputDescription'
    { _ptdReadCapacityUnits      :: !(Maybe Nat)
    , _ptdLastDecreaseDateTime   :: !(Maybe POSIX)
    , _ptdWriteCapacityUnits     :: !(Maybe Nat)
    , _ptdNumberOfDecreasesToday :: !(Maybe Nat)
    , _ptdLastIncreaseDateTime   :: !(Maybe POSIX)
    } deriving (Eq,Read,Show)

-- | 'ProvisionedThroughputDescription' smart constructor.
provisionedThroughputDescription :: ProvisionedThroughputDescription
provisionedThroughputDescription =
    ProvisionedThroughputDescription'
    { _ptdReadCapacityUnits = Nothing
    , _ptdLastDecreaseDateTime = Nothing
    , _ptdWriteCapacityUnits = Nothing
    , _ptdNumberOfDecreasesToday = Nothing
    , _ptdLastIncreaseDateTime = Nothing
    }

-- | The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a /ThrottlingException/. Eventually consistent
-- reads require less effort than strongly consistent reads, so a setting
-- of 50 /ReadCapacityUnits/ per second provides 100 eventually consistent
-- /ReadCapacityUnits/ per second.
ptdReadCapacityUnits :: Lens' ProvisionedThroughputDescription (Maybe Natural)
ptdReadCapacityUnits = lens _ptdReadCapacityUnits (\ s a -> s{_ptdReadCapacityUnits = a}) . mapping _Nat;

-- | The date and time of the last provisioned throughput decrease for this
-- table.
ptdLastDecreaseDateTime :: Lens' ProvisionedThroughputDescription (Maybe UTCTime)
ptdLastDecreaseDateTime = lens _ptdLastDecreaseDateTime (\ s a -> s{_ptdLastDecreaseDateTime = a}) . mapping _Time;

-- | The maximum number of writes consumed per second before DynamoDB returns
-- a /ThrottlingException/.
ptdWriteCapacityUnits :: Lens' ProvisionedThroughputDescription (Maybe Natural)
ptdWriteCapacityUnits = lens _ptdWriteCapacityUnits (\ s a -> s{_ptdWriteCapacityUnits = a}) . mapping _Nat;

-- | The number of provisioned throughput decreases for this table during
-- this UTC calendar day. For current maximums on provisioned throughput
-- decreases, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits>
-- in the /Amazon DynamoDB Developer Guide/.
ptdNumberOfDecreasesToday :: Lens' ProvisionedThroughputDescription (Maybe Natural)
ptdNumberOfDecreasesToday = lens _ptdNumberOfDecreasesToday (\ s a -> s{_ptdNumberOfDecreasesToday = a}) . mapping _Nat;

-- | The date and time of the last provisioned throughput increase for this
-- table.
ptdLastIncreaseDateTime :: Lens' ProvisionedThroughputDescription (Maybe UTCTime)
ptdLastIncreaseDateTime = lens _ptdLastIncreaseDateTime (\ s a -> s{_ptdLastIncreaseDateTime = a}) . mapping _Time;

instance FromJSON ProvisionedThroughputDescription
         where
        parseJSON
          = withObject "ProvisionedThroughputDescription"
              (\ x ->
                 ProvisionedThroughputDescription' <$>
                   (x .:? "ReadCapacityUnits") <*>
                     (x .:? "LastDecreaseDateTime")
                     <*> (x .:? "WriteCapacityUnits")
                     <*> (x .:? "NumberOfDecreasesToday")
                     <*> (x .:? "LastIncreaseDateTime"))

-- | Represents a request to perform a /PutItem/ operation on an item.
--
-- /See:/ 'putRequest' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prItem'
newtype PutRequest = PutRequest'
    { _prItem :: Map Text AttributeValue
    } deriving (Eq,Show)

-- | 'PutRequest' smart constructor.
putRequest :: PutRequest
putRequest =
    PutRequest'
    { _prItem = mempty
    }

-- | A map of attribute name to attribute values, representing the primary
-- key of an item to be processed by /PutItem/. All of the table\'s primary
-- key attributes must be specified, and their data types must match those
-- of the table\'s key schema. If any attributes are present in the item
-- which are part of an index key schema for the table, their types must
-- match the index key schema.
prItem :: Lens' PutRequest (HashMap Text AttributeValue)
prItem = lens _prItem (\ s a -> s{_prItem = a}) . _Map;

instance FromJSON PutRequest where
        parseJSON
          = withObject "PutRequest"
              (\ x -> PutRequest' <$> (x .:? "Item" .!= mempty))

instance ToJSON PutRequest where
        toJSON PutRequest'{..} = object ["Item" .= _prItem]

-- | Represents the properties of a table.
--
-- /See:/ 'tableDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tdProvisionedThroughput'
--
-- * 'tdAttributeDefinitions'
--
-- * 'tdTableSizeBytes'
--
-- * 'tdTableStatus'
--
-- * 'tdKeySchema'
--
-- * 'tdGlobalSecondaryIndexes'
--
-- * 'tdLocalSecondaryIndexes'
--
-- * 'tdCreationDateTime'
--
-- * 'tdItemCount'
--
-- * 'tdTableName'
data TableDescription = TableDescription'
    { _tdProvisionedThroughput  :: !(Maybe ProvisionedThroughputDescription)
    , _tdAttributeDefinitions   :: !(Maybe [AttributeDefinition])
    , _tdTableSizeBytes         :: !(Maybe Integer)
    , _tdTableStatus            :: !(Maybe TableStatus)
    , _tdKeySchema              :: !(Maybe (List1 KeySchemaElement))
    , _tdGlobalSecondaryIndexes :: !(Maybe [GlobalSecondaryIndexDescription])
    , _tdLocalSecondaryIndexes  :: !(Maybe [LocalSecondaryIndexDescription])
    , _tdCreationDateTime       :: !(Maybe POSIX)
    , _tdItemCount              :: !(Maybe Integer)
    , _tdTableName              :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'TableDescription' smart constructor.
tableDescription :: TableDescription
tableDescription =
    TableDescription'
    { _tdProvisionedThroughput = Nothing
    , _tdAttributeDefinitions = Nothing
    , _tdTableSizeBytes = Nothing
    , _tdTableStatus = Nothing
    , _tdKeySchema = Nothing
    , _tdGlobalSecondaryIndexes = Nothing
    , _tdLocalSecondaryIndexes = Nothing
    , _tdCreationDateTime = Nothing
    , _tdItemCount = Nothing
    , _tdTableName = Nothing
    }

-- | The provisioned throughput settings for the table, consisting of read
-- and write capacity units, along with data about increases and decreases.
tdProvisionedThroughput :: Lens' TableDescription (Maybe ProvisionedThroughputDescription)
tdProvisionedThroughput = lens _tdProvisionedThroughput (\ s a -> s{_tdProvisionedThroughput = a});

-- | An array of /AttributeDefinition/ objects. Each of these objects
-- describes one attribute in the table and index key schema.
--
-- Each /AttributeDefinition/ object in this array is composed of:
--
-- -   /AttributeName/ - The name of the attribute.
--
-- -   /AttributeType/ - The data type for the attribute.
--
tdAttributeDefinitions :: Lens' TableDescription [AttributeDefinition]
tdAttributeDefinitions = lens _tdAttributeDefinitions (\ s a -> s{_tdAttributeDefinitions = a}) . _Default;

-- | The total size of the specified table, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be
-- reflected in this value.
tdTableSizeBytes :: Lens' TableDescription (Maybe Integer)
tdTableSizeBytes = lens _tdTableSizeBytes (\ s a -> s{_tdTableSizeBytes = a});

-- | The current state of the table:
--
-- -   /CREATING/ - The table is being created.
--
-- -   /UPDATING/ - The table is being updated.
--
-- -   /DELETING/ - The table is being deleted.
--
-- -   /ACTIVE/ - The table is ready for use.
--
tdTableStatus :: Lens' TableDescription (Maybe TableStatus)
tdTableStatus = lens _tdTableStatus (\ s a -> s{_tdTableStatus = a});

-- | The primary key structure for the table. Each /KeySchemaElement/
-- consists of:
--
-- -   /AttributeName/ - The name of the attribute.
--
-- -   /KeyType/ - The key type for the attribute. Can be either @HASH@ or
--     @RANGE@.
--
-- For more information about primary keys, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html#DataModelPrimaryKey Primary Key>
-- in the /Amazon DynamoDB Developer Guide/.
tdKeySchema :: Lens' TableDescription (Maybe (NonEmpty KeySchemaElement))
tdKeySchema = lens _tdKeySchema (\ s a -> s{_tdKeySchema = a}) . mapping _List1;

-- | The global secondary indexes, if any, on the table. Each index is scoped
-- to a given hash key value. Each element is composed of:
--
-- -   /Backfilling/ - If true, then the index is currently in the
--     backfilling phase. Backfilling occurs only when a new global
--     secondary index is added to the table; it is the process by which
--     DynamoDB populates the new index with data from the table. (This
--     attribute does not appear for indexes that were created during a
--     /CreateTable/ operation.)
--
-- -   /IndexName/ - The name of the global secondary index.
--
-- -   /IndexSizeBytes/ - The total size of the global secondary index, in
--     bytes. DynamoDB updates this value approximately every six hours.
--     Recent changes might not be reflected in this value.
--
-- -   /IndexStatus/ - The current status of the global secondary index:
--
--     -   /CREATING/ - The index is being created.
--
--     -   /UPDATING/ - The index is being updated.
--
--     -   /DELETING/ - The index is being deleted.
--
--     -   /ACTIVE/ - The index is ready for use.
--
-- -   /ItemCount/ - The number of items in the global secondary index.
--     DynamoDB updates this value approximately every six hours. Recent
--     changes might not be reflected in this value.
--
-- -   /KeySchema/ - Specifies the complete index key schema. The attribute
--     names in the key schema must be between 1 and 255 characters
--     (inclusive). The key schema must begin with the same hash key
--     attribute as the table.
--
-- -   /Projection/ - Specifies attributes that are copied (projected) from
--     the table into the index. These are in addition to the primary key
--     attributes and index key attributes, which are automatically
--     projected. Each attribute specification is composed of:
--
--     -   /ProjectionType/ - One of the following:
--
--         -   @KEYS_ONLY@ - Only the index and primary keys are projected
--             into the index.
--
--         -   @INCLUDE@ - Only the specified table attributes are
--             projected into the index. The list of projected attributes
--             are in /NonKeyAttributes/.
--
--         -   @ALL@ - All of the table attributes are projected into the
--             index.
--
--     -   /NonKeyAttributes/ - A list of one or more non-key attribute
--         names that are projected into the secondary index. The total
--         count of attributes provided in /NonKeyAttributes/, summed
--         across all of the secondary indexes, must not exceed 20. If you
--         project the same attribute into two different indexes, this
--         counts as two distinct attributes when determining the total.
--
-- -   /ProvisionedThroughput/ - The provisioned throughput settings for
--     the global secondary index, consisting of read and write capacity
--     units, along with data about increases and decreases.
--
-- If the table is in the @DELETING@ state, no information about indexes
-- will be returned.
tdGlobalSecondaryIndexes :: Lens' TableDescription [GlobalSecondaryIndexDescription]
tdGlobalSecondaryIndexes = lens _tdGlobalSecondaryIndexes (\ s a -> s{_tdGlobalSecondaryIndexes = a}) . _Default;

-- | Represents one or more local secondary indexes on the table. Each index
-- is scoped to a given hash key value. Tables with one or more local
-- secondary indexes are subject to an item collection size limit, where
-- the amount of data within a given item collection cannot exceed 10 GB.
-- Each element is composed of:
--
-- -   /IndexName/ - The name of the local secondary index.
--
-- -   /KeySchema/ - Specifies the complete index key schema. The attribute
--     names in the key schema must be between 1 and 255 characters
--     (inclusive). The key schema must begin with the same hash key
--     attribute as the table.
--
-- -   /Projection/ - Specifies attributes that are copied (projected) from
--     the table into the index. These are in addition to the primary key
--     attributes and index key attributes, which are automatically
--     projected. Each attribute specification is composed of:
--
--     -   /ProjectionType/ - One of the following:
--
--         -   @KEYS_ONLY@ - Only the index and primary keys are projected
--             into the index.
--
--         -   @INCLUDE@ - Only the specified table attributes are
--             projected into the index. The list of projected attributes
--             are in /NonKeyAttributes/.
--
--         -   @ALL@ - All of the table attributes are projected into the
--             index.
--
--     -   /NonKeyAttributes/ - A list of one or more non-key attribute
--         names that are projected into the secondary index. The total
--         count of attributes provided in /NonKeyAttributes/, summed
--         across all of the secondary indexes, must not exceed 20. If you
--         project the same attribute into two different indexes, this
--         counts as two distinct attributes when determining the total.
--
-- -   /IndexSizeBytes/ - Represents the total size of the index, in bytes.
--     DynamoDB updates this value approximately every six hours. Recent
--     changes might not be reflected in this value.
--
-- -   /ItemCount/ - Represents the number of items in the index. DynamoDB
--     updates this value approximately every six hours. Recent changes
--     might not be reflected in this value.
--
-- If the table is in the @DELETING@ state, no information about indexes
-- will be returned.
tdLocalSecondaryIndexes :: Lens' TableDescription [LocalSecondaryIndexDescription]
tdLocalSecondaryIndexes = lens _tdLocalSecondaryIndexes (\ s a -> s{_tdLocalSecondaryIndexes = a}) . _Default;

-- | The date and time when the table was created, in
-- <http://www.epochconverter.com/ UNIX epoch time> format.
tdCreationDateTime :: Lens' TableDescription (Maybe UTCTime)
tdCreationDateTime = lens _tdCreationDateTime (\ s a -> s{_tdCreationDateTime = a}) . mapping _Time;

-- | The number of items in the specified table. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
tdItemCount :: Lens' TableDescription (Maybe Integer)
tdItemCount = lens _tdItemCount (\ s a -> s{_tdItemCount = a});

-- | The name of the table.
tdTableName :: Lens' TableDescription (Maybe Text)
tdTableName = lens _tdTableName (\ s a -> s{_tdTableName = a});

instance FromJSON TableDescription where
        parseJSON
          = withObject "TableDescription"
              (\ x ->
                 TableDescription' <$>
                   (x .:? "ProvisionedThroughput") <*>
                     (x .:? "AttributeDefinitions" .!= mempty)
                     <*> (x .:? "TableSizeBytes")
                     <*> (x .:? "TableStatus")
                     <*> (x .:? "KeySchema")
                     <*> (x .:? "GlobalSecondaryIndexes" .!= mempty)
                     <*> (x .:? "LocalSecondaryIndexes" .!= mempty)
                     <*> (x .:? "CreationDateTime")
                     <*> (x .:? "ItemCount")
                     <*> (x .:? "TableName"))

-- | Represents the new provisioned throughput settings to be applied to a
-- global secondary index.
--
-- /See:/ 'updateGlobalSecondaryIndexAction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ugsiaIndexName'
--
-- * 'ugsiaProvisionedThroughput'
data UpdateGlobalSecondaryIndexAction = UpdateGlobalSecondaryIndexAction'
    { _ugsiaIndexName             :: !Text
    , _ugsiaProvisionedThroughput :: !ProvisionedThroughput
    } deriving (Eq,Read,Show)

-- | 'UpdateGlobalSecondaryIndexAction' smart constructor.
updateGlobalSecondaryIndexAction :: Text -> ProvisionedThroughput -> UpdateGlobalSecondaryIndexAction
updateGlobalSecondaryIndexAction pIndexName pProvisionedThroughput =
    UpdateGlobalSecondaryIndexAction'
    { _ugsiaIndexName = pIndexName
    , _ugsiaProvisionedThroughput = pProvisionedThroughput
    }

-- | The name of the global secondary index to be updated.
ugsiaIndexName :: Lens' UpdateGlobalSecondaryIndexAction Text
ugsiaIndexName = lens _ugsiaIndexName (\ s a -> s{_ugsiaIndexName = a});

-- | FIXME: Undocumented member.
ugsiaProvisionedThroughput :: Lens' UpdateGlobalSecondaryIndexAction ProvisionedThroughput
ugsiaProvisionedThroughput = lens _ugsiaProvisionedThroughput (\ s a -> s{_ugsiaProvisionedThroughput = a});

instance ToJSON UpdateGlobalSecondaryIndexAction
         where
        toJSON UpdateGlobalSecondaryIndexAction'{..}
          = object
              ["IndexName" .= _ugsiaIndexName,
               "ProvisionedThroughput" .=
                 _ugsiaProvisionedThroughput]

-- | Represents an operation to perform - either /DeleteItem/ or /PutItem/.
-- You can only request one of these operations, not both, in a single
-- /WriteRequest/. If you do need to perform both of these operations, you
-- will need to provide two separate /WriteRequest/ objects.
--
-- /See:/ 'writeRequest' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wrPutRequest'
--
-- * 'wrDeleteRequest'
data WriteRequest = WriteRequest'
    { _wrPutRequest    :: !(Maybe PutRequest)
    , _wrDeleteRequest :: !(Maybe DeleteRequest)
    } deriving (Eq,Show)

-- | 'WriteRequest' smart constructor.
writeRequest :: WriteRequest
writeRequest =
    WriteRequest'
    { _wrPutRequest = Nothing
    , _wrDeleteRequest = Nothing
    }

-- | A request to perform a /PutItem/ operation.
wrPutRequest :: Lens' WriteRequest (Maybe PutRequest)
wrPutRequest = lens _wrPutRequest (\ s a -> s{_wrPutRequest = a});

-- | A request to perform a /DeleteItem/ operation.
wrDeleteRequest :: Lens' WriteRequest (Maybe DeleteRequest)
wrDeleteRequest = lens _wrDeleteRequest (\ s a -> s{_wrDeleteRequest = a});

instance FromJSON WriteRequest where
        parseJSON
          = withObject "WriteRequest"
              (\ x ->
                 WriteRequest' <$>
                   (x .:? "PutRequest") <*> (x .:? "DeleteRequest"))

instance ToJSON WriteRequest where
        toJSON WriteRequest'{..}
          = object
              ["PutRequest" .= _wrPutRequest,
               "DeleteRequest" .= _wrDeleteRequest]
