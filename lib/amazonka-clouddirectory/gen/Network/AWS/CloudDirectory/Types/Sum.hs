{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.Sum where

import Network.AWS.Prelude

data BatchReadExceptionType
  = AccessDeniedException
  | CannotListParentOfRootException
  | DirectoryNotEnabledException
  | FacetValidationException
  | InternalServiceException
  | InvalidARNException
  | InvalidNextTokenException
  | LimitExceededException
  | NotIndexException
  | NotNodeException
  | NotPolicyException
  | ResourceNotFoundException
  | ValidationException
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BatchReadExceptionType where
    parser = takeLowerText >>= \case
        "accessdeniedexception" -> pure AccessDeniedException
        "cannotlistparentofrootexception" -> pure CannotListParentOfRootException
        "directorynotenabledexception" -> pure DirectoryNotEnabledException
        "facetvalidationexception" -> pure FacetValidationException
        "internalserviceexception" -> pure InternalServiceException
        "invalidarnexception" -> pure InvalidARNException
        "invalidnexttokenexception" -> pure InvalidNextTokenException
        "limitexceededexception" -> pure LimitExceededException
        "notindexexception" -> pure NotIndexException
        "notnodeexception" -> pure NotNodeException
        "notpolicyexception" -> pure NotPolicyException
        "resourcenotfoundexception" -> pure ResourceNotFoundException
        "validationexception" -> pure ValidationException
        e -> fromTextError $ "Failure parsing BatchReadExceptionType from value: '" <> e
           <> "'. Accepted values: accessdeniedexception, cannotlistparentofrootexception, directorynotenabledexception, facetvalidationexception, internalserviceexception, invalidarnexception, invalidnexttokenexception, limitexceededexception, notindexexception, notnodeexception, notpolicyexception, resourcenotfoundexception, validationexception"

instance ToText BatchReadExceptionType where
    toText = \case
        AccessDeniedException -> "AccessDeniedException"
        CannotListParentOfRootException -> "CannotListParentOfRootException"
        DirectoryNotEnabledException -> "DirectoryNotEnabledException"
        FacetValidationException -> "FacetValidationException"
        InternalServiceException -> "InternalServiceException"
        InvalidARNException -> "InvalidArnException"
        InvalidNextTokenException -> "InvalidNextTokenException"
        LimitExceededException -> "LimitExceededException"
        NotIndexException -> "NotIndexException"
        NotNodeException -> "NotNodeException"
        NotPolicyException -> "NotPolicyException"
        ResourceNotFoundException -> "ResourceNotFoundException"
        ValidationException -> "ValidationException"

instance Hashable     BatchReadExceptionType
instance NFData       BatchReadExceptionType
instance ToByteString BatchReadExceptionType
instance ToQuery      BatchReadExceptionType
instance ToHeader     BatchReadExceptionType

instance FromJSON BatchReadExceptionType where
    parseJSON = parseJSONText "BatchReadExceptionType"

data ConsistencyLevel
  = Eventual
  | Serializable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConsistencyLevel where
    parser = takeLowerText >>= \case
        "eventual" -> pure Eventual
        "serializable" -> pure Serializable
        e -> fromTextError $ "Failure parsing ConsistencyLevel from value: '" <> e
           <> "'. Accepted values: eventual, serializable"

instance ToText ConsistencyLevel where
    toText = \case
        Eventual -> "EVENTUAL"
        Serializable -> "SERIALIZABLE"

instance Hashable     ConsistencyLevel
instance NFData       ConsistencyLevel
instance ToByteString ConsistencyLevel
instance ToQuery      ConsistencyLevel
instance ToHeader     ConsistencyLevel

instance ToJSON ConsistencyLevel where
    toJSON = toJSONText

data DirectoryState
  = Deleted
  | Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DirectoryState where
    parser = takeLowerText >>= \case
        "deleted" -> pure Deleted
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing DirectoryState from value: '" <> e
           <> "'. Accepted values: deleted, disabled, enabled"

instance ToText DirectoryState where
    toText = \case
        Deleted -> "DELETED"
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"

instance Hashable     DirectoryState
instance NFData       DirectoryState
instance ToByteString DirectoryState
instance ToQuery      DirectoryState
instance ToHeader     DirectoryState

instance ToJSON DirectoryState where
    toJSON = toJSONText

instance FromJSON DirectoryState where
    parseJSON = parseJSONText "DirectoryState"

data FacetAttributeType
  = Binary
  | Boolean
  | Datetime
  | Number
  | String
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FacetAttributeType where
    parser = takeLowerText >>= \case
        "binary" -> pure Binary
        "boolean" -> pure Boolean
        "datetime" -> pure Datetime
        "number" -> pure Number
        "string" -> pure String
        e -> fromTextError $ "Failure parsing FacetAttributeType from value: '" <> e
           <> "'. Accepted values: binary, boolean, datetime, number, string"

instance ToText FacetAttributeType where
    toText = \case
        Binary -> "BINARY"
        Boolean -> "BOOLEAN"
        Datetime -> "DATETIME"
        Number -> "NUMBER"
        String -> "STRING"

instance Hashable     FacetAttributeType
instance NFData       FacetAttributeType
instance ToByteString FacetAttributeType
instance ToQuery      FacetAttributeType
instance ToHeader     FacetAttributeType

instance ToJSON FacetAttributeType where
    toJSON = toJSONText

instance FromJSON FacetAttributeType where
    parseJSON = parseJSONText "FacetAttributeType"

data ObjectType
  = Index
  | LeafNode
  | Node
  | Policy
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ObjectType where
    parser = takeLowerText >>= \case
        "index" -> pure Index
        "leaf_node" -> pure LeafNode
        "node" -> pure Node
        "policy" -> pure Policy
        e -> fromTextError $ "Failure parsing ObjectType from value: '" <> e
           <> "'. Accepted values: index, leaf_node, node, policy"

instance ToText ObjectType where
    toText = \case
        Index -> "INDEX"
        LeafNode -> "LEAF_NODE"
        Node -> "NODE"
        Policy -> "POLICY"

instance Hashable     ObjectType
instance NFData       ObjectType
instance ToByteString ObjectType
instance ToQuery      ObjectType
instance ToHeader     ObjectType

instance ToJSON ObjectType where
    toJSON = toJSONText

instance FromJSON ObjectType where
    parseJSON = parseJSONText "ObjectType"

data RangeMode
  = Exclusive
  | First
  | Inclusive
  | Last
  | LastBeforeMissingValues
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RangeMode where
    parser = takeLowerText >>= \case
        "exclusive" -> pure Exclusive
        "first" -> pure First
        "inclusive" -> pure Inclusive
        "last" -> pure Last
        "last_before_missing_values" -> pure LastBeforeMissingValues
        e -> fromTextError $ "Failure parsing RangeMode from value: '" <> e
           <> "'. Accepted values: exclusive, first, inclusive, last, last_before_missing_values"

instance ToText RangeMode where
    toText = \case
        Exclusive -> "EXCLUSIVE"
        First -> "FIRST"
        Inclusive -> "INCLUSIVE"
        Last -> "LAST"
        LastBeforeMissingValues -> "LAST_BEFORE_MISSING_VALUES"

instance Hashable     RangeMode
instance NFData       RangeMode
instance ToByteString RangeMode
instance ToQuery      RangeMode
instance ToHeader     RangeMode

instance ToJSON RangeMode where
    toJSON = toJSONText

data RequiredAttributeBehavior
  = NotRequired
  | RequiredAlways
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RequiredAttributeBehavior where
    parser = takeLowerText >>= \case
        "not_required" -> pure NotRequired
        "required_always" -> pure RequiredAlways
        e -> fromTextError $ "Failure parsing RequiredAttributeBehavior from value: '" <> e
           <> "'. Accepted values: not_required, required_always"

instance ToText RequiredAttributeBehavior where
    toText = \case
        NotRequired -> "NOT_REQUIRED"
        RequiredAlways -> "REQUIRED_ALWAYS"

instance Hashable     RequiredAttributeBehavior
instance NFData       RequiredAttributeBehavior
instance ToByteString RequiredAttributeBehavior
instance ToQuery      RequiredAttributeBehavior
instance ToHeader     RequiredAttributeBehavior

instance ToJSON RequiredAttributeBehavior where
    toJSON = toJSONText

instance FromJSON RequiredAttributeBehavior where
    parseJSON = parseJSONText "RequiredAttributeBehavior"

data RuleType
  = BinaryLength
  | NumberComparison
  | StringFromSet
  | StringLength
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RuleType where
    parser = takeLowerText >>= \case
        "binary_length" -> pure BinaryLength
        "number_comparison" -> pure NumberComparison
        "string_from_set" -> pure StringFromSet
        "string_length" -> pure StringLength
        e -> fromTextError $ "Failure parsing RuleType from value: '" <> e
           <> "'. Accepted values: binary_length, number_comparison, string_from_set, string_length"

instance ToText RuleType where
    toText = \case
        BinaryLength -> "BINARY_LENGTH"
        NumberComparison -> "NUMBER_COMPARISON"
        StringFromSet -> "STRING_FROM_SET"
        StringLength -> "STRING_LENGTH"

instance Hashable     RuleType
instance NFData       RuleType
instance ToByteString RuleType
instance ToQuery      RuleType
instance ToHeader     RuleType

instance ToJSON RuleType where
    toJSON = toJSONText

instance FromJSON RuleType where
    parseJSON = parseJSONText "RuleType"

data UpdateActionType
  = CreateOrUpdate
  | Delete
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UpdateActionType where
    parser = takeLowerText >>= \case
        "create_or_update" -> pure CreateOrUpdate
        "delete" -> pure Delete
        e -> fromTextError $ "Failure parsing UpdateActionType from value: '" <> e
           <> "'. Accepted values: create_or_update, delete"

instance ToText UpdateActionType where
    toText = \case
        CreateOrUpdate -> "CREATE_OR_UPDATE"
        Delete -> "DELETE"

instance Hashable     UpdateActionType
instance NFData       UpdateActionType
instance ToByteString UpdateActionType
instance ToQuery      UpdateActionType
instance ToHeader     UpdateActionType

instance ToJSON UpdateActionType where
    toJSON = toJSONText
