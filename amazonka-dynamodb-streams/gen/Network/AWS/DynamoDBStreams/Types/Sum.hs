{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDBStreams.Types.Sum where

import           Network.AWS.Prelude

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
instance ToQuery      KeyType
instance ToHeader     KeyType

instance FromJSON KeyType where
    parseJSON = parseJSONText "KeyType"

data OperationType
    = Insert
    | Modify
    | Remove
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText OperationType where
    parser = takeLowerText >>= \case
        "insert" -> pure Insert
        "modify" -> pure Modify
        "remove" -> pure Remove
        e -> fromTextError $ "Failure parsing OperationType from value: '" <> e
           <> "'. Accepted values: insert, modify, remove"

instance ToText OperationType where
    toText = \case
        Insert -> "insert"
        Modify -> "modify"
        Remove -> "remove"

instance Hashable     OperationType
instance ToByteString OperationType
instance ToQuery      OperationType
instance ToHeader     OperationType

instance FromJSON OperationType where
    parseJSON = parseJSONText "OperationType"

data ShardIteratorType
    = AfterSequenceNumber
    | AtSequenceNumber
    | Latest
    | TrimHorizon
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ShardIteratorType where
    parser = takeLowerText >>= \case
        "after_sequence_number" -> pure AfterSequenceNumber
        "at_sequence_number" -> pure AtSequenceNumber
        "latest" -> pure Latest
        "trim_horizon" -> pure TrimHorizon
        e -> fromTextError $ "Failure parsing ShardIteratorType from value: '" <> e
           <> "'. Accepted values: after_sequence_number, at_sequence_number, latest, trim_horizon"

instance ToText ShardIteratorType where
    toText = \case
        AfterSequenceNumber -> "after_sequence_number"
        AtSequenceNumber -> "at_sequence_number"
        Latest -> "latest"
        TrimHorizon -> "trim_horizon"

instance Hashable     ShardIteratorType
instance ToByteString ShardIteratorType
instance ToQuery      ShardIteratorType
instance ToHeader     ShardIteratorType

instance ToJSON ShardIteratorType where
    toJSON = toJSONText

data StreamStatus
    = Disabled
    | Disabling
    | Enabled
    | Enabling
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StreamStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "disabling" -> pure Disabling
        "enabled" -> pure Enabled
        "enabling" -> pure Enabling
        e -> fromTextError $ "Failure parsing StreamStatus from value: '" <> e
           <> "'. Accepted values: disabled, disabling, enabled, enabling"

instance ToText StreamStatus where
    toText = \case
        Disabled -> "disabled"
        Disabling -> "disabling"
        Enabled -> "enabled"
        Enabling -> "enabling"

instance Hashable     StreamStatus
instance ToByteString StreamStatus
instance ToQuery      StreamStatus
instance ToHeader     StreamStatus

instance FromJSON StreamStatus where
    parseJSON = parseJSONText "StreamStatus"

data StreamViewType
    = KeysOnly
    | NewAndOldImages
    | NewImage
    | OldImage
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StreamViewType where
    parser = takeLowerText >>= \case
        "keys_only" -> pure KeysOnly
        "new_and_old_images" -> pure NewAndOldImages
        "new_image" -> pure NewImage
        "old_image" -> pure OldImage
        e -> fromTextError $ "Failure parsing StreamViewType from value: '" <> e
           <> "'. Accepted values: keys_only, new_and_old_images, new_image, old_image"

instance ToText StreamViewType where
    toText = \case
        KeysOnly -> "keys_only"
        NewAndOldImages -> "new_and_old_images"
        NewImage -> "new_image"
        OldImage -> "old_image"

instance Hashable     StreamViewType
instance ToByteString StreamViewType
instance ToQuery      StreamViewType
instance ToHeader     StreamViewType

instance FromJSON StreamViewType where
    parseJSON = parseJSONText "StreamViewType"
