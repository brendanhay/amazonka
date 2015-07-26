{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.Sum where

import           Network.AWS.Prelude

data AttributeAction
    = Add
    | Put
    | Delete
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText AttributeAction where
    parser = takeLowerText >>= \case
        "add" -> pure Add
        "delete" -> pure Delete
        "put" -> pure Put
        e -> fromTextError $ "Failure parsing AttributeAction from value: '" <> e
           <> "'. Accepted values: add, delete, put"

instance ToText AttributeAction where
    toText = \case
        Add -> "add"
        Delete -> "delete"
        Put -> "put"

instance Hashable     AttributeAction
instance ToByteString AttributeAction
instance ToPath       AttributeAction
instance ToQuery      AttributeAction
instance ToHeader     AttributeAction

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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ComparisonOperator where
    parser = takeLowerText >>= \case
        "begins_with" -> pure BeginsWith
        "between" -> pure Between
        "contains" -> pure Contains
        "eq" -> pure EQ'
        "ge" -> pure GE
        "gt" -> pure GT'
        "in" -> pure IN
        "le" -> pure LE
        "lt" -> pure LT'
        "ne" -> pure NE
        "not_contains" -> pure NotContains
        "not_null" -> pure NotNull
        "null" -> pure Null
        e -> fromTextError $ "Failure parsing ComparisonOperator from value: '" <> e
           <> "'. Accepted values: begins_with, between, contains, eq, ge, gt, in, le, lt, ne, not_contains, not_null, null"

instance ToText ComparisonOperator where
    toText = \case
        BeginsWith -> "begins_with"
        Between -> "between"
        Contains -> "contains"
        EQ' -> "eq"
        GE -> "ge"
        GT' -> "gt"
        IN -> "in"
        LE -> "le"
        LT' -> "lt"
        NE -> "ne"
        NotContains -> "not_contains"
        NotNull -> "not_null"
        Null -> "null"

instance Hashable     ComparisonOperator
instance ToByteString ComparisonOperator
instance ToPath       ComparisonOperator
instance ToQuery      ComparisonOperator
instance ToHeader     ComparisonOperator

instance ToJSON ComparisonOperator where
    toJSON = toJSONText

data ConditionalOperator
    = And
    | OR
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ConditionalOperator where
    parser = takeLowerText >>= \case
        "and" -> pure And
        "or" -> pure OR
        e -> fromTextError $ "Failure parsing ConditionalOperator from value: '" <> e
           <> "'. Accepted values: and, or"

instance ToText ConditionalOperator where
    toText = \case
        And -> "and"
        OR -> "or"

instance Hashable     ConditionalOperator
instance ToByteString ConditionalOperator
instance ToPath       ConditionalOperator
instance ToQuery      ConditionalOperator
instance ToHeader     ConditionalOperator

instance ToJSON ConditionalOperator where
    toJSON = toJSONText

data IndexStatus
    = ISUpdating
    | ISDeleting
    | ISCreating
    | ISActive
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText IndexStatus where
    parser = takeLowerText >>= \case
        "active" -> pure ISActive
        "creating" -> pure ISCreating
        "deleting" -> pure ISDeleting
        "updating" -> pure ISUpdating
        e -> fromTextError $ "Failure parsing IndexStatus from value: '" <> e
           <> "'. Accepted values: active, creating, deleting, updating"

instance ToText IndexStatus where
    toText = \case
        ISActive -> "active"
        ISCreating -> "creating"
        ISDeleting -> "deleting"
        ISUpdating -> "updating"

instance Hashable     IndexStatus
instance ToByteString IndexStatus
instance ToPath       IndexStatus
instance ToQuery      IndexStatus
instance ToHeader     IndexStatus

instance FromJSON IndexStatus where
    parseJSON = parseJSONText "IndexStatus"

data KeyType
    = Hash
    | Range
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText KeyType where
    parser = takeLowerText >>= \case
        "hash" -> pure Hash
        "range" -> pure Range
        e -> fromTextError $ "Failure parsing KeyType from value: '" <> e
           <> "'. Accepted values: hash, range"

instance ToText KeyType where
    toText = \case
        Hash -> "hash"
        Range -> "range"

instance Hashable     KeyType
instance ToByteString KeyType
instance ToPath       KeyType
instance ToQuery      KeyType
instance ToHeader     KeyType

instance ToJSON KeyType where
    toJSON = toJSONText

instance FromJSON KeyType where
    parseJSON = parseJSONText "KeyType"

data ProjectionType
    = Include
    | All
    | KeysOnly
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ProjectionType where
    parser = takeLowerText >>= \case
        "all" -> pure All
        "include" -> pure Include
        "keys_only" -> pure KeysOnly
        e -> fromTextError $ "Failure parsing ProjectionType from value: '" <> e
           <> "'. Accepted values: all, include, keys_only"

instance ToText ProjectionType where
    toText = \case
        All -> "all"
        Include -> "include"
        KeysOnly -> "keys_only"

instance Hashable     ProjectionType
instance ToByteString ProjectionType
instance ToPath       ProjectionType
instance ToQuery      ProjectionType
instance ToHeader     ProjectionType

instance ToJSON ProjectionType where
    toJSON = toJSONText

instance FromJSON ProjectionType where
    parseJSON = parseJSONText "ProjectionType"

-- | Determines the level of detail about provisioned throughput consumption
-- that is returned in the response:
--
-- -   /INDEXES/ - The response includes the aggregate /ConsumedCapacity/
--     for the operation, together with /ConsumedCapacity/ for each table
--     and secondary index that was accessed.
--
--     Note that some operations, such as /GetItem/ and /BatchGetItem/, do
--     not access any indexes at all. In these cases, specifying /INDEXES/
--     will only return /ConsumedCapacity/ information for table(s).
--
-- -   /TOTAL/ - The response includes only the aggregate
--     /ConsumedCapacity/ for the operation.
--
-- -   /NONE/ - No /ConsumedCapacity/ details are included in the response.
--
data ReturnConsumedCapacity
    = RCCNone
    | RCCIndexes
    | RCCTotal
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ReturnConsumedCapacity where
    parser = takeLowerText >>= \case
        "indexes" -> pure RCCIndexes
        "none" -> pure RCCNone
        "total" -> pure RCCTotal
        e -> fromTextError $ "Failure parsing ReturnConsumedCapacity from value: '" <> e
           <> "'. Accepted values: indexes, none, total"

instance ToText ReturnConsumedCapacity where
    toText = \case
        RCCIndexes -> "indexes"
        RCCNone -> "none"
        RCCTotal -> "total"

instance Hashable     ReturnConsumedCapacity
instance ToByteString ReturnConsumedCapacity
instance ToPath       ReturnConsumedCapacity
instance ToQuery      ReturnConsumedCapacity
instance ToHeader     ReturnConsumedCapacity

instance ToJSON ReturnConsumedCapacity where
    toJSON = toJSONText

data ReturnItemCollectionMetrics
    = RICMNone
    | RICMSize
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ReturnItemCollectionMetrics where
    parser = takeLowerText >>= \case
        "none" -> pure RICMNone
        "size" -> pure RICMSize
        e -> fromTextError $ "Failure parsing ReturnItemCollectionMetrics from value: '" <> e
           <> "'. Accepted values: none, size"

instance ToText ReturnItemCollectionMetrics where
    toText = \case
        RICMNone -> "none"
        RICMSize -> "size"

instance Hashable     ReturnItemCollectionMetrics
instance ToByteString ReturnItemCollectionMetrics
instance ToPath       ReturnItemCollectionMetrics
instance ToQuery      ReturnItemCollectionMetrics
instance ToHeader     ReturnItemCollectionMetrics

instance ToJSON ReturnItemCollectionMetrics where
    toJSON = toJSONText

data ReturnValue
    = UpdatedOld
    | None
    | AllNew
    | AllOld
    | UpdatedNew
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ReturnValue where
    parser = takeLowerText >>= \case
        "all_new" -> pure AllNew
        "all_old" -> pure AllOld
        "none" -> pure None
        "updated_new" -> pure UpdatedNew
        "updated_old" -> pure UpdatedOld
        e -> fromTextError $ "Failure parsing ReturnValue from value: '" <> e
           <> "'. Accepted values: all_new, all_old, none, updated_new, updated_old"

instance ToText ReturnValue where
    toText = \case
        AllNew -> "all_new"
        AllOld -> "all_old"
        None -> "none"
        UpdatedNew -> "updated_new"
        UpdatedOld -> "updated_old"

instance Hashable     ReturnValue
instance ToByteString ReturnValue
instance ToPath       ReturnValue
instance ToQuery      ReturnValue
instance ToHeader     ReturnValue

instance ToJSON ReturnValue where
    toJSON = toJSONText

data ScalarAttributeType
    = N
    | B
    | S
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ScalarAttributeType where
    parser = takeLowerText >>= \case
        "b" -> pure B
        "n" -> pure N
        "s" -> pure S
        e -> fromTextError $ "Failure parsing ScalarAttributeType from value: '" <> e
           <> "'. Accepted values: b, n, s"

instance ToText ScalarAttributeType where
    toText = \case
        B -> "b"
        N -> "n"
        S -> "s"

instance Hashable     ScalarAttributeType
instance ToByteString ScalarAttributeType
instance ToPath       ScalarAttributeType
instance ToQuery      ScalarAttributeType
instance ToHeader     ScalarAttributeType

instance ToJSON ScalarAttributeType where
    toJSON = toJSONText

instance FromJSON ScalarAttributeType where
    parseJSON = parseJSONText "ScalarAttributeType"

data Select
    = Count
    | AllAttributes
    | SpecificAttributes
    | AllProjectedAttributes
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Select where
    parser = takeLowerText >>= \case
        "all_attributes" -> pure AllAttributes
        "all_projected_attributes" -> pure AllProjectedAttributes
        "count" -> pure Count
        "specific_attributes" -> pure SpecificAttributes
        e -> fromTextError $ "Failure parsing Select from value: '" <> e
           <> "'. Accepted values: all_attributes, all_projected_attributes, count, specific_attributes"

instance ToText Select where
    toText = \case
        AllAttributes -> "all_attributes"
        AllProjectedAttributes -> "all_projected_attributes"
        Count -> "count"
        SpecificAttributes -> "specific_attributes"

instance Hashable     Select
instance ToByteString Select
instance ToPath       Select
instance ToQuery      Select
instance ToHeader     Select

instance ToJSON Select where
    toJSON = toJSONText

data StreamViewType
    = SVTNewImage
    | SVTNewAndOldImages
    | SVTOldImage
    | SVTKeysOnly
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StreamViewType where
    parser = takeLowerText >>= \case
        "keys_only" -> pure SVTKeysOnly
        "new_and_old_images" -> pure SVTNewAndOldImages
        "new_image" -> pure SVTNewImage
        "old_image" -> pure SVTOldImage
        e -> fromTextError $ "Failure parsing StreamViewType from value: '" <> e
           <> "'. Accepted values: keys_only, new_and_old_images, new_image, old_image"

instance ToText StreamViewType where
    toText = \case
        SVTKeysOnly -> "keys_only"
        SVTNewAndOldImages -> "new_and_old_images"
        SVTNewImage -> "new_image"
        SVTOldImage -> "old_image"

instance Hashable     StreamViewType
instance ToByteString StreamViewType
instance ToPath       StreamViewType
instance ToQuery      StreamViewType
instance ToHeader     StreamViewType

instance ToJSON StreamViewType where
    toJSON = toJSONText

instance FromJSON StreamViewType where
    parseJSON = parseJSONText "StreamViewType"

data TableStatus
    = Deleting
    | Updating
    | Creating
    | Active
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText TableStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "creating" -> pure Creating
        "deleting" -> pure Deleting
        "updating" -> pure Updating
        e -> fromTextError $ "Failure parsing TableStatus from value: '" <> e
           <> "'. Accepted values: active, creating, deleting, updating"

instance ToText TableStatus where
    toText = \case
        Active -> "active"
        Creating -> "creating"
        Deleting -> "deleting"
        Updating -> "updating"

instance Hashable     TableStatus
instance ToByteString TableStatus
instance ToPath       TableStatus
instance ToQuery      TableStatus
instance ToHeader     TableStatus

instance FromJSON TableStatus where
    parseJSON = parseJSONText "TableStatus"
