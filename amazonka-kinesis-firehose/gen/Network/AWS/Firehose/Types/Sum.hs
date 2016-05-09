{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.Sum where

import           Network.AWS.Prelude

data CompressionFormat
    = Gzip
    | Snappy
    | Uncompressed
    | Zip
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText CompressionFormat where
    parser = takeLowerText >>= \case
        "gzip" -> pure Gzip
        "snappy" -> pure Snappy
        "uncompressed" -> pure Uncompressed
        "zip" -> pure Zip
        e -> fromTextError $ "Failure parsing CompressionFormat from value: '" <> e
           <> "'. Accepted values: GZIP, Snappy, UNCOMPRESSED, ZIP"

instance ToText CompressionFormat where
    toText = \case
        Gzip -> "GZIP"
        Snappy -> "Snappy"
        Uncompressed -> "UNCOMPRESSED"
        Zip -> "ZIP"

instance Hashable     CompressionFormat
instance NFData       CompressionFormat
instance ToByteString CompressionFormat
instance ToQuery      CompressionFormat
instance ToHeader     CompressionFormat

instance ToJSON CompressionFormat where
    toJSON = toJSONText

instance FromJSON CompressionFormat where
    parseJSON = parseJSONText "CompressionFormat"

data DeliveryStreamStatus
    = Active
    | Creating
    | Deleting
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DeliveryStreamStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "creating" -> pure Creating
        "deleting" -> pure Deleting
        e -> fromTextError $ "Failure parsing DeliveryStreamStatus from value: '" <> e
           <> "'. Accepted values: ACTIVE, CREATING, DELETING"

instance ToText DeliveryStreamStatus where
    toText = \case
        Active -> "ACTIVE"
        Creating -> "CREATING"
        Deleting -> "DELETING"

instance Hashable     DeliveryStreamStatus
instance NFData       DeliveryStreamStatus
instance ToByteString DeliveryStreamStatus
instance ToQuery      DeliveryStreamStatus
instance ToHeader     DeliveryStreamStatus

instance FromJSON DeliveryStreamStatus where
    parseJSON = parseJSONText "DeliveryStreamStatus"

data ElasticsearchIndexRotationPeriod
    = NoRotation
    | OneDay
    | OneHour
    | OneMonth
    | OneWeek
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ElasticsearchIndexRotationPeriod where
    parser = takeLowerText >>= \case
        "norotation" -> pure NoRotation
        "oneday" -> pure OneDay
        "onehour" -> pure OneHour
        "onemonth" -> pure OneMonth
        "oneweek" -> pure OneWeek
        e -> fromTextError $ "Failure parsing ElasticsearchIndexRotationPeriod from value: '" <> e
           <> "'. Accepted values: NoRotation, OneDay, OneHour, OneMonth, OneWeek"

instance ToText ElasticsearchIndexRotationPeriod where
    toText = \case
        NoRotation -> "NoRotation"
        OneDay -> "OneDay"
        OneHour -> "OneHour"
        OneMonth -> "OneMonth"
        OneWeek -> "OneWeek"

instance Hashable     ElasticsearchIndexRotationPeriod
instance NFData       ElasticsearchIndexRotationPeriod
instance ToByteString ElasticsearchIndexRotationPeriod
instance ToQuery      ElasticsearchIndexRotationPeriod
instance ToHeader     ElasticsearchIndexRotationPeriod

instance ToJSON ElasticsearchIndexRotationPeriod where
    toJSON = toJSONText

instance FromJSON ElasticsearchIndexRotationPeriod where
    parseJSON = parseJSONText "ElasticsearchIndexRotationPeriod"

data ElasticsearchS3BackupMode
    = AllDocuments
    | FailedDocumentsOnly
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ElasticsearchS3BackupMode where
    parser = takeLowerText >>= \case
        "alldocuments" -> pure AllDocuments
        "faileddocumentsonly" -> pure FailedDocumentsOnly
        e -> fromTextError $ "Failure parsing ElasticsearchS3BackupMode from value: '" <> e
           <> "'. Accepted values: AllDocuments, FailedDocumentsOnly"

instance ToText ElasticsearchS3BackupMode where
    toText = \case
        AllDocuments -> "AllDocuments"
        FailedDocumentsOnly -> "FailedDocumentsOnly"

instance Hashable     ElasticsearchS3BackupMode
instance NFData       ElasticsearchS3BackupMode
instance ToByteString ElasticsearchS3BackupMode
instance ToQuery      ElasticsearchS3BackupMode
instance ToHeader     ElasticsearchS3BackupMode

instance ToJSON ElasticsearchS3BackupMode where
    toJSON = toJSONText

instance FromJSON ElasticsearchS3BackupMode where
    parseJSON = parseJSONText "ElasticsearchS3BackupMode"

data NoEncryptionConfig =
    NoEncryption
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText NoEncryptionConfig where
    parser = takeLowerText >>= \case
        "noencryption" -> pure NoEncryption
        e -> fromTextError $ "Failure parsing NoEncryptionConfig from value: '" <> e
           <> "'. Accepted values: NoEncryption"

instance ToText NoEncryptionConfig where
    toText = \case
        NoEncryption -> "NoEncryption"

instance Hashable     NoEncryptionConfig
instance NFData       NoEncryptionConfig
instance ToByteString NoEncryptionConfig
instance ToQuery      NoEncryptionConfig
instance ToHeader     NoEncryptionConfig

instance ToJSON NoEncryptionConfig where
    toJSON = toJSONText

instance FromJSON NoEncryptionConfig where
    parseJSON = parseJSONText "NoEncryptionConfig"
