{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
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

data HECEndpointType
  = Event
  | Raw
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HECEndpointType where
    parser = takeLowerText >>= \case
        "event" -> pure Event
        "raw" -> pure Raw
        e -> fromTextError $ "Failure parsing HECEndpointType from value: '" <> e
           <> "'. Accepted values: event, raw"

instance ToText HECEndpointType where
    toText = \case
        Event -> "Event"
        Raw -> "Raw"

instance Hashable     HECEndpointType
instance NFData       HECEndpointType
instance ToByteString HECEndpointType
instance ToQuery      HECEndpointType
instance ToHeader     HECEndpointType

instance ToJSON HECEndpointType where
    toJSON = toJSONText

instance FromJSON HECEndpointType where
    parseJSON = parseJSONText "HECEndpointType"

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

data OrcCompression
  = OCNone
  | OCSnappy
  | OCZlib
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OrcCompression where
    parser = takeLowerText >>= \case
        "none" -> pure OCNone
        "snappy" -> pure OCSnappy
        "zlib" -> pure OCZlib
        e -> fromTextError $ "Failure parsing OrcCompression from value: '" <> e
           <> "'. Accepted values: none, snappy, zlib"

instance ToText OrcCompression where
    toText = \case
        OCNone -> "NONE"
        OCSnappy -> "SNAPPY"
        OCZlib -> "ZLIB"

instance Hashable     OrcCompression
instance NFData       OrcCompression
instance ToByteString OrcCompression
instance ToQuery      OrcCompression
instance ToHeader     OrcCompression

instance ToJSON OrcCompression where
    toJSON = toJSONText

instance FromJSON OrcCompression where
    parseJSON = parseJSONText "OrcCompression"

data OrcFormatVersion
  = V011
  | V012
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OrcFormatVersion where
    parser = takeLowerText >>= \case
        "v0_11" -> pure V011
        "v0_12" -> pure V012
        e -> fromTextError $ "Failure parsing OrcFormatVersion from value: '" <> e
           <> "'. Accepted values: v0_11, v0_12"

instance ToText OrcFormatVersion where
    toText = \case
        V011 -> "V0_11"
        V012 -> "V0_12"

instance Hashable     OrcFormatVersion
instance NFData       OrcFormatVersion
instance ToByteString OrcFormatVersion
instance ToQuery      OrcFormatVersion
instance ToHeader     OrcFormatVersion

instance ToJSON OrcFormatVersion where
    toJSON = toJSONText

instance FromJSON OrcFormatVersion where
    parseJSON = parseJSONText "OrcFormatVersion"

data ParquetCompression
  = PCGzip
  | PCSnappy
  | PCUncompressed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ParquetCompression where
    parser = takeLowerText >>= \case
        "gzip" -> pure PCGzip
        "snappy" -> pure PCSnappy
        "uncompressed" -> pure PCUncompressed
        e -> fromTextError $ "Failure parsing ParquetCompression from value: '" <> e
           <> "'. Accepted values: gzip, snappy, uncompressed"

instance ToText ParquetCompression where
    toText = \case
        PCGzip -> "GZIP"
        PCSnappy -> "SNAPPY"
        PCUncompressed -> "UNCOMPRESSED"

instance Hashable     ParquetCompression
instance NFData       ParquetCompression
instance ToByteString ParquetCompression
instance ToQuery      ParquetCompression
instance ToHeader     ParquetCompression

instance ToJSON ParquetCompression where
    toJSON = toJSONText

instance FromJSON ParquetCompression where
    parseJSON = parseJSONText "ParquetCompression"

data ParquetWriterVersion
  = V1
  | V2
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ParquetWriterVersion where
    parser = takeLowerText >>= \case
        "v1" -> pure V1
        "v2" -> pure V2
        e -> fromTextError $ "Failure parsing ParquetWriterVersion from value: '" <> e
           <> "'. Accepted values: v1, v2"

instance ToText ParquetWriterVersion where
    toText = \case
        V1 -> "V1"
        V2 -> "V2"

instance Hashable     ParquetWriterVersion
instance NFData       ParquetWriterVersion
instance ToByteString ParquetWriterVersion
instance ToQuery      ParquetWriterVersion
instance ToHeader     ParquetWriterVersion

instance ToJSON ParquetWriterVersion where
    toJSON = toJSONText

instance FromJSON ParquetWriterVersion where
    parseJSON = parseJSONText "ParquetWriterVersion"

data ProcessorParameterName
  = BufferIntervalInSeconds
  | BufferSizeInMBs
  | LambdaARN
  | NumberOfRetries
  | RoleARN
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProcessorParameterName where
    parser = takeLowerText >>= \case
        "bufferintervalinseconds" -> pure BufferIntervalInSeconds
        "buffersizeinmbs" -> pure BufferSizeInMBs
        "lambdaarn" -> pure LambdaARN
        "numberofretries" -> pure NumberOfRetries
        "rolearn" -> pure RoleARN
        e -> fromTextError $ "Failure parsing ProcessorParameterName from value: '" <> e
           <> "'. Accepted values: bufferintervalinseconds, buffersizeinmbs, lambdaarn, numberofretries, rolearn"

instance ToText ProcessorParameterName where
    toText = \case
        BufferIntervalInSeconds -> "BufferIntervalInSeconds"
        BufferSizeInMBs -> "BufferSizeInMBs"
        LambdaARN -> "LambdaArn"
        NumberOfRetries -> "NumberOfRetries"
        RoleARN -> "RoleArn"

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

data SplunkS3BackupMode
  = AllEvents
  | FailedEventsOnly
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SplunkS3BackupMode where
    parser = takeLowerText >>= \case
        "allevents" -> pure AllEvents
        "failedeventsonly" -> pure FailedEventsOnly
        e -> fromTextError $ "Failure parsing SplunkS3BackupMode from value: '" <> e
           <> "'. Accepted values: allevents, failedeventsonly"

instance ToText SplunkS3BackupMode where
    toText = \case
        AllEvents -> "AllEvents"
        FailedEventsOnly -> "FailedEventsOnly"

instance Hashable     SplunkS3BackupMode
instance NFData       SplunkS3BackupMode
instance ToByteString SplunkS3BackupMode
instance ToQuery      SplunkS3BackupMode
instance ToHeader     SplunkS3BackupMode

instance ToJSON SplunkS3BackupMode where
    toJSON = toJSONText

instance FromJSON SplunkS3BackupMode where
    parseJSON = parseJSONText "SplunkS3BackupMode"
