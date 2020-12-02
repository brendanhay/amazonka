{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
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

data FileModeTypeEnum
  = Executable
  | Normal
  | Symlink
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FileModeTypeEnum where
    parser = takeLowerText >>= \case
        "executable" -> pure Executable
        "normal" -> pure Normal
        "symlink" -> pure Symlink
        e -> fromTextError $ "Failure parsing FileModeTypeEnum from value: '" <> e
           <> "'. Accepted values: executable, normal, symlink"

instance ToText FileModeTypeEnum where
    toText = \case
        Executable -> "EXECUTABLE"
        Normal -> "NORMAL"
        Symlink -> "SYMLINK"

instance Hashable     FileModeTypeEnum
instance NFData       FileModeTypeEnum
instance ToByteString FileModeTypeEnum
instance ToQuery      FileModeTypeEnum
instance ToHeader     FileModeTypeEnum

instance ToJSON FileModeTypeEnum where
    toJSON = toJSONText

data MergeOptionTypeEnum =
  FastForwardMerge
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MergeOptionTypeEnum where
    parser = takeLowerText >>= \case
        "fast_forward_merge" -> pure FastForwardMerge
        e -> fromTextError $ "Failure parsing MergeOptionTypeEnum from value: '" <> e
           <> "'. Accepted values: fast_forward_merge"

instance ToText MergeOptionTypeEnum where
    toText = \case
        FastForwardMerge -> "FAST_FORWARD_MERGE"

instance Hashable     MergeOptionTypeEnum
instance NFData       MergeOptionTypeEnum
instance ToByteString MergeOptionTypeEnum
instance ToQuery      MergeOptionTypeEnum
instance ToHeader     MergeOptionTypeEnum

instance ToJSON MergeOptionTypeEnum where
    toJSON = toJSONText

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

data PullRequestEventType
  = PullRequestCreated
  | PullRequestMergeStateChanged
  | PullRequestSourceReferenceUpdated
  | PullRequestStatusChanged
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PullRequestEventType where
    parser = takeLowerText >>= \case
        "pull_request_created" -> pure PullRequestCreated
        "pull_request_merge_state_changed" -> pure PullRequestMergeStateChanged
        "pull_request_source_reference_updated" -> pure PullRequestSourceReferenceUpdated
        "pull_request_status_changed" -> pure PullRequestStatusChanged
        e -> fromTextError $ "Failure parsing PullRequestEventType from value: '" <> e
           <> "'. Accepted values: pull_request_created, pull_request_merge_state_changed, pull_request_source_reference_updated, pull_request_status_changed"

instance ToText PullRequestEventType where
    toText = \case
        PullRequestCreated -> "PULL_REQUEST_CREATED"
        PullRequestMergeStateChanged -> "PULL_REQUEST_MERGE_STATE_CHANGED"
        PullRequestSourceReferenceUpdated -> "PULL_REQUEST_SOURCE_REFERENCE_UPDATED"
        PullRequestStatusChanged -> "PULL_REQUEST_STATUS_CHANGED"

instance Hashable     PullRequestEventType
instance NFData       PullRequestEventType
instance ToByteString PullRequestEventType
instance ToQuery      PullRequestEventType
instance ToHeader     PullRequestEventType

instance ToJSON PullRequestEventType where
    toJSON = toJSONText

instance FromJSON PullRequestEventType where
    parseJSON = parseJSONText "PullRequestEventType"

data PullRequestStatusEnum
  = Closed
  | Open
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PullRequestStatusEnum where
    parser = takeLowerText >>= \case
        "closed" -> pure Closed
        "open" -> pure Open
        e -> fromTextError $ "Failure parsing PullRequestStatusEnum from value: '" <> e
           <> "'. Accepted values: closed, open"

instance ToText PullRequestStatusEnum where
    toText = \case
        Closed -> "CLOSED"
        Open -> "OPEN"

instance Hashable     PullRequestStatusEnum
instance NFData       PullRequestStatusEnum
instance ToByteString PullRequestStatusEnum
instance ToQuery      PullRequestStatusEnum
instance ToHeader     PullRequestStatusEnum

instance ToJSON PullRequestStatusEnum where
    toJSON = toJSONText

instance FromJSON PullRequestStatusEnum where
    parseJSON = parseJSONText "PullRequestStatusEnum"

data RelativeFileVersionEnum
  = After
  | Before
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RelativeFileVersionEnum where
    parser = takeLowerText >>= \case
        "after" -> pure After
        "before" -> pure Before
        e -> fromTextError $ "Failure parsing RelativeFileVersionEnum from value: '" <> e
           <> "'. Accepted values: after, before"

instance ToText RelativeFileVersionEnum where
    toText = \case
        After -> "AFTER"
        Before -> "BEFORE"

instance Hashable     RelativeFileVersionEnum
instance NFData       RelativeFileVersionEnum
instance ToByteString RelativeFileVersionEnum
instance ToQuery      RelativeFileVersionEnum
instance ToHeader     RelativeFileVersionEnum

instance ToJSON RelativeFileVersionEnum where
    toJSON = toJSONText

instance FromJSON RelativeFileVersionEnum where
    parseJSON = parseJSONText "RelativeFileVersionEnum"

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
