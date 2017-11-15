{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Sum
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.Sum where

import Network.AWS.Prelude

data CompressionFormat
  = Gzip
  | Snappy
  | Uncompressed
  | Zip
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CompressionFormat where
    parser = takeLowerText >>= \case
        "gzip" -> pure Gzip
        "snappy" -> pure Snappy
        "uncompressed" -> pure Uncompressed
        "zip" -> pure Zip
        e -> fromTextError $ "Failure parsing CompressionFormat from value: '" <> e
           <> "'. Accepted values: gzip, snappy, uncompressed, zip"

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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeliveryStreamStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "creating" -> pure Creating
        "deleting" -> pure Deleting
        e -> fromTextError $ "Failure parsing DeliveryStreamStatus from value: '" <> e
           <> "'. Accepted values: active, creating, deleting"

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

data DeliveryStreamType
  = DirectPut
  | KinesisStreamAsSource
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeliveryStreamType where
    parser = takeLowerText >>= \case
        "directput" -> pure DirectPut
        "kinesisstreamassource" -> pure KinesisStreamAsSource
        e -> fromTextError $ "Failure parsing DeliveryStreamType from value: '" <> e
           <> "'. Accepted values: directput, kinesisstreamassource"

instance ToText DeliveryStreamType where
    toText = \case
        DirectPut -> "DirectPut"
        KinesisStreamAsSource -> "KinesisStreamAsSource"

instance Hashable     DeliveryStreamType
instance NFData       DeliveryStreamType
instance ToByteString DeliveryStreamType
instance ToQuery      DeliveryStreamType
instance ToHeader     DeliveryStreamType

instance ToJSON DeliveryStreamType where
    toJSON = toJSONText

instance FromJSON DeliveryStreamType where
    parseJSON = parseJSONText "DeliveryStreamType"

data ElasticsearchIndexRotationPeriod
  = NoRotation
  | OneDay
  | OneHour
  | OneMonth
  | OneWeek
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ElasticsearchIndexRotationPeriod where
    parser = takeLowerText >>= \case
        "norotation" -> pure NoRotation
        "oneday" -> pure OneDay
        "onehour" -> pure OneHour
        "onemonth" -> pure OneMonth
        "oneweek" -> pure OneWeek
        e -> fromTextError $ "Failure parsing ElasticsearchIndexRotationPeriod from value: '" <> e
           <> "'. Accepted values: norotation, oneday, onehour, onemonth, oneweek"

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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ElasticsearchS3BackupMode where
    parser = takeLowerText >>= \case
        "alldocuments" -> pure AllDocuments
        "faileddocumentsonly" -> pure FailedDocumentsOnly
        e -> fromTextError $ "Failure parsing ElasticsearchS3BackupMode from value: '" <> e
           <> "'. Accepted values: alldocuments, faileddocumentsonly"

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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NoEncryptionConfig where
    parser = takeLowerText >>= \case
        "noencryption" -> pure NoEncryption
        e -> fromTextError $ "Failure parsing NoEncryptionConfig from value: '" <> e
           <> "'. Accepted values: noencryption"

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

data ProcessorParameterName
  = LambdaARN
  | NumberOfRetries
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProcessorParameterName where
    parser = takeLowerText >>= \case
        "lambdaarn" -> pure LambdaARN
        "numberofretries" -> pure NumberOfRetries
        e -> fromTextError $ "Failure parsing ProcessorParameterName from value: '" <> e
           <> "'. Accepted values: lambdaarn, numberofretries"

instance ToText ProcessorParameterName where
    toText = \case
        LambdaARN -> "LambdaArn"
        NumberOfRetries -> "NumberOfRetries"

instance Hashable     ProcessorParameterName
instance NFData       ProcessorParameterName
instance ToByteString ProcessorParameterName
instance ToQuery      ProcessorParameterName
instance ToHeader     ProcessorParameterName

instance ToJSON ProcessorParameterName where
    toJSON = toJSONText

instance FromJSON ProcessorParameterName where
    parseJSON = parseJSONText "ProcessorParameterName"

data ProcessorType =
  Lambda
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProcessorType where
    parser = takeLowerText >>= \case
        "lambda" -> pure Lambda
        e -> fromTextError $ "Failure parsing ProcessorType from value: '" <> e
           <> "'. Accepted values: lambda"

instance ToText ProcessorType where
    toText = \case
        Lambda -> "Lambda"

instance Hashable     ProcessorType
instance NFData       ProcessorType
instance ToByteString ProcessorType
instance ToQuery      ProcessorType
instance ToHeader     ProcessorType

instance ToJSON ProcessorType where
    toJSON = toJSONText

instance FromJSON ProcessorType where
    parseJSON = parseJSONText "ProcessorType"

data RedshiftS3BackupMode
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RedshiftS3BackupMode where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing RedshiftS3BackupMode from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText RedshiftS3BackupMode where
    toText = \case
        Disabled -> "Disabled"
        Enabled -> "Enabled"

instance Hashable     RedshiftS3BackupMode
instance NFData       RedshiftS3BackupMode
instance ToByteString RedshiftS3BackupMode
instance ToQuery      RedshiftS3BackupMode
instance ToHeader     RedshiftS3BackupMode

instance ToJSON RedshiftS3BackupMode where
    toJSON = toJSONText

instance FromJSON RedshiftS3BackupMode where
    parseJSON = parseJSONText "RedshiftS3BackupMode"

data S3BackupMode
  = SBMDisabled
  | SBMEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText S3BackupMode where
    parser = takeLowerText >>= \case
        "disabled" -> pure SBMDisabled
        "enabled" -> pure SBMEnabled
        e -> fromTextError $ "Failure parsing S3BackupMode from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText S3BackupMode where
    toText = \case
        SBMDisabled -> "Disabled"
        SBMEnabled -> "Enabled"

instance Hashable     S3BackupMode
instance NFData       S3BackupMode
instance ToByteString S3BackupMode
instance ToQuery      S3BackupMode
instance ToHeader     S3BackupMode

instance ToJSON S3BackupMode where
    toJSON = toJSONText

instance FromJSON S3BackupMode where
    parseJSON = parseJSONText "S3BackupMode"
