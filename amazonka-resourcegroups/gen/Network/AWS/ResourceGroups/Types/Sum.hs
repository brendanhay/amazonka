{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroups.Types.Sum where

import Network.AWS.Prelude

data GroupFilterName =
  GFNResourceType
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GroupFilterName where
    parser = takeLowerText >>= \case
        "resource-type" -> pure GFNResourceType
        e -> fromTextError $ "Failure parsing GroupFilterName from value: '" <> e
           <> "'. Accepted values: resource-type"

instance ToText GroupFilterName where
    toText = \case
        GFNResourceType -> "resource-type"

instance Hashable     GroupFilterName
instance NFData       GroupFilterName
instance ToByteString GroupFilterName
instance ToQuery      GroupFilterName
instance ToHeader     GroupFilterName

instance ToJSON GroupFilterName where
    toJSON = toJSONText

data QueryErrorCode
  = CloudformationStackInactive
  | CloudformationStackNotExisting
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText QueryErrorCode where
    parser = takeLowerText >>= \case
        "cloudformation_stack_inactive" -> pure CloudformationStackInactive
        "cloudformation_stack_not_existing" -> pure CloudformationStackNotExisting
        e -> fromTextError $ "Failure parsing QueryErrorCode from value: '" <> e
           <> "'. Accepted values: cloudformation_stack_inactive, cloudformation_stack_not_existing"

instance ToText QueryErrorCode where
    toText = \case
        CloudformationStackInactive -> "CLOUDFORMATION_STACK_INACTIVE"
        CloudformationStackNotExisting -> "CLOUDFORMATION_STACK_NOT_EXISTING"

instance Hashable     QueryErrorCode
instance NFData       QueryErrorCode
instance ToByteString QueryErrorCode
instance ToQuery      QueryErrorCode
instance ToHeader     QueryErrorCode

instance FromJSON QueryErrorCode where
    parseJSON = parseJSONText "QueryErrorCode"

data QueryType
  = CloudformationStack10
  | TagFilters10
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText QueryType where
    parser = takeLowerText >>= \case
        "cloudformation_stack_1_0" -> pure CloudformationStack10
        "tag_filters_1_0" -> pure TagFilters10
        e -> fromTextError $ "Failure parsing QueryType from value: '" <> e
           <> "'. Accepted values: cloudformation_stack_1_0, tag_filters_1_0"

instance ToText QueryType where
    toText = \case
        CloudformationStack10 -> "CLOUDFORMATION_STACK_1_0"
        TagFilters10 -> "TAG_FILTERS_1_0"

instance Hashable     QueryType
instance NFData       QueryType
instance ToByteString QueryType
instance ToQuery      QueryType
instance ToHeader     QueryType

instance ToJSON QueryType where
    toJSON = toJSONText

instance FromJSON QueryType where
    parseJSON = parseJSONText "QueryType"

data ResourceFilterName =
  ResourceType
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceFilterName where
    parser = takeLowerText >>= \case
        "resource-type" -> pure ResourceType
        e -> fromTextError $ "Failure parsing ResourceFilterName from value: '" <> e
           <> "'. Accepted values: resource-type"

instance ToText ResourceFilterName where
    toText = \case
        ResourceType -> "resource-type"

instance Hashable     ResourceFilterName
instance NFData       ResourceFilterName
instance ToByteString ResourceFilterName
instance ToQuery      ResourceFilterName
instance ToHeader     ResourceFilterName

instance ToJSON ResourceFilterName where
    toJSON = toJSONText
