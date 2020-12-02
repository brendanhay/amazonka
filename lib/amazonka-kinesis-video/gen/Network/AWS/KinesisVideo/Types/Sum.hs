{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideo.Types.Sum where

import Network.AWS.Prelude

data APIName
  = GetMedia
  | GetMediaForFragmentList
  | ListFragments
  | PutMedia
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText APIName where
    parser = takeLowerText >>= \case
        "get_media" -> pure GetMedia
        "get_media_for_fragment_list" -> pure GetMediaForFragmentList
        "list_fragments" -> pure ListFragments
        "put_media" -> pure PutMedia
        e -> fromTextError $ "Failure parsing APIName from value: '" <> e
           <> "'. Accepted values: get_media, get_media_for_fragment_list, list_fragments, put_media"

instance ToText APIName where
    toText = \case
        GetMedia -> "GET_MEDIA"
        GetMediaForFragmentList -> "GET_MEDIA_FOR_FRAGMENT_LIST"
        ListFragments -> "LIST_FRAGMENTS"
        PutMedia -> "PUT_MEDIA"

instance Hashable     APIName
instance NFData       APIName
instance ToByteString APIName
instance ToQuery      APIName
instance ToHeader     APIName

instance ToJSON APIName where
    toJSON = toJSONText

data ComparisonOperator =
  BeginsWith
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ComparisonOperator where
    parser = takeLowerText >>= \case
        "begins_with" -> pure BeginsWith
        e -> fromTextError $ "Failure parsing ComparisonOperator from value: '" <> e
           <> "'. Accepted values: begins_with"

instance ToText ComparisonOperator where
    toText = \case
        BeginsWith -> "BEGINS_WITH"

instance Hashable     ComparisonOperator
instance NFData       ComparisonOperator
instance ToByteString ComparisonOperator
instance ToQuery      ComparisonOperator
instance ToHeader     ComparisonOperator

instance ToJSON ComparisonOperator where
    toJSON = toJSONText

data StreamStatus
  = Active
  | Creating
  | Deleting
  | Updating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StreamStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "creating" -> pure Creating
        "deleting" -> pure Deleting
        "updating" -> pure Updating
        e -> fromTextError $ "Failure parsing StreamStatus from value: '" <> e
           <> "'. Accepted values: active, creating, deleting, updating"

instance ToText StreamStatus where
    toText = \case
        Active -> "ACTIVE"
        Creating -> "CREATING"
        Deleting -> "DELETING"
        Updating -> "UPDATING"

instance Hashable     StreamStatus
instance NFData       StreamStatus
instance ToByteString StreamStatus
instance ToQuery      StreamStatus
instance ToHeader     StreamStatus

instance FromJSON StreamStatus where
    parseJSON = parseJSONText "StreamStatus"

data UpdateDataRetentionOperation
  = DecreaseDataRetention
  | IncreaseDataRetention
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UpdateDataRetentionOperation where
    parser = takeLowerText >>= \case
        "decrease_data_retention" -> pure DecreaseDataRetention
        "increase_data_retention" -> pure IncreaseDataRetention
        e -> fromTextError $ "Failure parsing UpdateDataRetentionOperation from value: '" <> e
           <> "'. Accepted values: decrease_data_retention, increase_data_retention"

instance ToText UpdateDataRetentionOperation where
    toText = \case
        DecreaseDataRetention -> "DECREASE_DATA_RETENTION"
        IncreaseDataRetention -> "INCREASE_DATA_RETENTION"

instance Hashable     UpdateDataRetentionOperation
instance NFData       UpdateDataRetentionOperation
instance ToByteString UpdateDataRetentionOperation
instance ToQuery      UpdateDataRetentionOperation
instance ToHeader     UpdateDataRetentionOperation

instance ToJSON UpdateDataRetentionOperation where
    toJSON = toJSONText
