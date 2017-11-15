{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Sum
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.Sum where

import Network.AWS.Prelude

data ChangeTypeEnum
  = A
  | D
  | M
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeTypeEnum where
    parser = takeLowerText >>= \case
        "a" -> pure A
        "d" -> pure D
        "m" -> pure M
        e -> fromTextError $ "Failure parsing ChangeTypeEnum from value: '" <> e
           <> "'. Accepted values: a, d, m"

instance ToText ChangeTypeEnum where
    toText = \case
        A -> "A"
        D -> "D"
        M -> "M"

instance Hashable     ChangeTypeEnum
instance NFData       ChangeTypeEnum
instance ToByteString ChangeTypeEnum
instance ToQuery      ChangeTypeEnum
instance ToHeader     ChangeTypeEnum

instance FromJSON ChangeTypeEnum where
    parseJSON = parseJSONText "ChangeTypeEnum"

data OrderEnum
  = Ascending
  | Descending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OrderEnum where
    parser = takeLowerText >>= \case
        "ascending" -> pure Ascending
        "descending" -> pure Descending
        e -> fromTextError $ "Failure parsing OrderEnum from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText OrderEnum where
    toText = \case
        Ascending -> "ascending"
        Descending -> "descending"

instance Hashable     OrderEnum
instance NFData       OrderEnum
instance ToByteString OrderEnum
instance ToQuery      OrderEnum
instance ToHeader     OrderEnum

instance ToJSON OrderEnum where
    toJSON = toJSONText

data RepositoryTriggerEventEnum
  = All
  | CreateReference
  | DeleteReference
  | UpdateReference
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RepositoryTriggerEventEnum where
    parser = takeLowerText >>= \case
        "all" -> pure All
        "createreference" -> pure CreateReference
        "deletereference" -> pure DeleteReference
        "updatereference" -> pure UpdateReference
        e -> fromTextError $ "Failure parsing RepositoryTriggerEventEnum from value: '" <> e
           <> "'. Accepted values: all, createreference, deletereference, updatereference"

instance ToText RepositoryTriggerEventEnum where
    toText = \case
        All -> "all"
        CreateReference -> "createReference"
        DeleteReference -> "deleteReference"
        UpdateReference -> "updateReference"

instance Hashable     RepositoryTriggerEventEnum
instance NFData       RepositoryTriggerEventEnum
instance ToByteString RepositoryTriggerEventEnum
instance ToQuery      RepositoryTriggerEventEnum
instance ToHeader     RepositoryTriggerEventEnum

instance ToJSON RepositoryTriggerEventEnum where
    toJSON = toJSONText

instance FromJSON RepositoryTriggerEventEnum where
    parseJSON = parseJSONText "RepositoryTriggerEventEnum"

data SortByEnum
  = LastModifiedDate
  | RepositoryName
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SortByEnum where
    parser = takeLowerText >>= \case
        "lastmodifieddate" -> pure LastModifiedDate
        "repositoryname" -> pure RepositoryName
        e -> fromTextError $ "Failure parsing SortByEnum from value: '" <> e
           <> "'. Accepted values: lastmodifieddate, repositoryname"

instance ToText SortByEnum where
    toText = \case
        LastModifiedDate -> "lastModifiedDate"
        RepositoryName -> "repositoryName"

instance Hashable     SortByEnum
instance NFData       SortByEnum
instance ToByteString SortByEnum
instance ToQuery      SortByEnum
instance ToHeader     SortByEnum

instance ToJSON SortByEnum where
    toJSON = toJSONText
