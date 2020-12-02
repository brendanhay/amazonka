{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.Sum where

import Network.AWS.Prelude

data EncryptionType
  = KMS
  | None
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EncryptionType where
    parser = takeLowerText >>= \case
        "kms" -> pure KMS
        "none" -> pure None
        e -> fromTextError $ "Failure parsing EncryptionType from value: '" <> e
           <> "'. Accepted values: kms, none"

instance ToText EncryptionType where
    toText = \case
        KMS -> "KMS"
        None -> "NONE"

instance Hashable     EncryptionType
instance NFData       EncryptionType
instance ToByteString EncryptionType
instance ToQuery      EncryptionType
instance ToHeader     EncryptionType

instance ToJSON EncryptionType where
    toJSON = toJSONText

instance FromJSON EncryptionType where
    parseJSON = parseJSONText "EncryptionType"

data MetricsName
  = All
  | IncomingBytes
  | IncomingRecords
  | IteratorAgeMilliseconds
  | OutgoingBytes
  | OutgoingRecords
  | ReadProvisionedThroughputExceeded
  | WriteProvisionedThroughputExceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MetricsName where
    parser = takeLowerText >>= \case
        "all" -> pure All
        "incomingbytes" -> pure IncomingBytes
        "incomingrecords" -> pure IncomingRecords
        "iteratoragemilliseconds" -> pure IteratorAgeMilliseconds
        "outgoingbytes" -> pure OutgoingBytes
        "outgoingrecords" -> pure OutgoingRecords
        "readprovisionedthroughputexceeded" -> pure ReadProvisionedThroughputExceeded
        "writeprovisionedthroughputexceeded" -> pure WriteProvisionedThroughputExceeded
        e -> fromTextError $ "Failure parsing MetricsName from value: '" <> e
           <> "'. Accepted values: all, incomingbytes, incomingrecords, iteratoragemilliseconds, outgoingbytes, outgoingrecords, readprovisionedthroughputexceeded, writeprovisionedthroughputexceeded"

instance ToText MetricsName where
    toText = \case
        All -> "ALL"
        IncomingBytes -> "IncomingBytes"
        IncomingRecords -> "IncomingRecords"
        IteratorAgeMilliseconds -> "IteratorAgeMilliseconds"
        OutgoingBytes -> "OutgoingBytes"
        OutgoingRecords -> "OutgoingRecords"
        ReadProvisionedThroughputExceeded -> "ReadProvisionedThroughputExceeded"
        WriteProvisionedThroughputExceeded -> "WriteProvisionedThroughputExceeded"

instance Hashable     MetricsName
instance NFData       MetricsName
instance ToByteString MetricsName
instance ToQuery      MetricsName
instance ToHeader     MetricsName

instance ToJSON MetricsName where
    toJSON = toJSONText

instance FromJSON MetricsName where
    parseJSON = parseJSONText "MetricsName"

data ScalingType =
  UniformScaling
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScalingType where
    parser = takeLowerText >>= \case
        "uniform_scaling" -> pure UniformScaling
        e -> fromTextError $ "Failure parsing ScalingType from value: '" <> e
           <> "'. Accepted values: uniform_scaling"

instance ToText ScalingType where
    toText = \case
        UniformScaling -> "UNIFORM_SCALING"

instance Hashable     ScalingType
instance NFData       ScalingType
instance ToByteString ScalingType
instance ToQuery      ScalingType
instance ToHeader     ScalingType

instance ToJSON ScalingType where
    toJSON = toJSONText

data ShardIteratorType
  = AfterSequenceNumber
  | AtSequenceNumber
  | AtTimestamp
  | Latest
  | TrimHorizon
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ShardIteratorType where
    parser = takeLowerText >>= \case
        "after_sequence_number" -> pure AfterSequenceNumber
        "at_sequence_number" -> pure AtSequenceNumber
        "at_timestamp" -> pure AtTimestamp
        "latest" -> pure Latest
        "trim_horizon" -> pure TrimHorizon
        e -> fromTextError $ "Failure parsing ShardIteratorType from value: '" <> e
           <> "'. Accepted values: after_sequence_number, at_sequence_number, at_timestamp, latest, trim_horizon"

instance ToText ShardIteratorType where
    toText = \case
        AfterSequenceNumber -> "AFTER_SEQUENCE_NUMBER"
        AtSequenceNumber -> "AT_SEQUENCE_NUMBER"
        AtTimestamp -> "AT_TIMESTAMP"
        Latest -> "LATEST"
        TrimHorizon -> "TRIM_HORIZON"

instance Hashable     ShardIteratorType
instance NFData       ShardIteratorType
instance ToByteString ShardIteratorType
instance ToQuery      ShardIteratorType
instance ToHeader     ShardIteratorType

instance ToJSON ShardIteratorType where
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
