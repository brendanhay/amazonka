{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.Sum where

import Network.AWS.Prelude

data AuthMechanismValue
  = Default
  | MongodbCr
  | ScramSha1
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AuthMechanismValue where
    parser = takeLowerText >>= \case
        "default" -> pure Default
        "mongodb_cr" -> pure MongodbCr
        "scram_sha_1" -> pure ScramSha1
        e -> fromTextError $ "Failure parsing AuthMechanismValue from value: '" <> e
           <> "'. Accepted values: default, mongodb_cr, scram_sha_1"

instance ToText AuthMechanismValue where
    toText = \case
        Default -> "default"
        MongodbCr -> "mongodb_cr"
        ScramSha1 -> "scram_sha_1"

instance Hashable     AuthMechanismValue
instance NFData       AuthMechanismValue
instance ToByteString AuthMechanismValue
instance ToQuery      AuthMechanismValue
instance ToHeader     AuthMechanismValue

instance ToJSON AuthMechanismValue where
    toJSON = toJSONText

instance FromJSON AuthMechanismValue where
    parseJSON = parseJSONText "AuthMechanismValue"

data AuthTypeValue
  = NO
  | Password
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AuthTypeValue where
    parser = takeLowerText >>= \case
        "no" -> pure NO
        "password" -> pure Password
        e -> fromTextError $ "Failure parsing AuthTypeValue from value: '" <> e
           <> "'. Accepted values: no, password"

instance ToText AuthTypeValue where
    toText = \case
        NO -> "no"
        Password -> "password"

instance Hashable     AuthTypeValue
instance NFData       AuthTypeValue
instance ToByteString AuthTypeValue
instance ToQuery      AuthTypeValue
instance ToHeader     AuthTypeValue

instance ToJSON AuthTypeValue where
    toJSON = toJSONText

instance FromJSON AuthTypeValue where
    parseJSON = parseJSONText "AuthTypeValue"

data CompressionTypeValue
  = CTVGzip
  | CTVNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CompressionTypeValue where
    parser = takeLowerText >>= \case
        "gzip" -> pure CTVGzip
        "none" -> pure CTVNone
        e -> fromTextError $ "Failure parsing CompressionTypeValue from value: '" <> e
           <> "'. Accepted values: gzip, none"

instance ToText CompressionTypeValue where
    toText = \case
        CTVGzip -> "gzip"
        CTVNone -> "none"

instance Hashable     CompressionTypeValue
instance NFData       CompressionTypeValue
instance ToByteString CompressionTypeValue
instance ToQuery      CompressionTypeValue
instance ToHeader     CompressionTypeValue

instance ToJSON CompressionTypeValue where
    toJSON = toJSONText

instance FromJSON CompressionTypeValue where
    parseJSON = parseJSONText "CompressionTypeValue"

data DmsSSLModeValue
  = DSMVNone
  | DSMVRequire
  | DSMVVerifyCa
  | DSMVVerifyFull
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DmsSSLModeValue where
    parser = takeLowerText >>= \case
        "none" -> pure DSMVNone
        "require" -> pure DSMVRequire
        "verify-ca" -> pure DSMVVerifyCa
        "verify-full" -> pure DSMVVerifyFull
        e -> fromTextError $ "Failure parsing DmsSSLModeValue from value: '" <> e
           <> "'. Accepted values: none, require, verify-ca, verify-full"

instance ToText DmsSSLModeValue where
    toText = \case
        DSMVNone -> "none"
        DSMVRequire -> "require"
        DSMVVerifyCa -> "verify-ca"
        DSMVVerifyFull -> "verify-full"

instance Hashable     DmsSSLModeValue
instance NFData       DmsSSLModeValue
instance ToByteString DmsSSLModeValue
instance ToQuery      DmsSSLModeValue
instance ToHeader     DmsSSLModeValue

instance ToJSON DmsSSLModeValue where
    toJSON = toJSONText

instance FromJSON DmsSSLModeValue where
    parseJSON = parseJSONText "DmsSSLModeValue"

data MigrationTypeValue
  = Cdc
  | FullLoad
  | FullLoadAndCdc
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MigrationTypeValue where
    parser = takeLowerText >>= \case
        "cdc" -> pure Cdc
        "full-load" -> pure FullLoad
        "full-load-and-cdc" -> pure FullLoadAndCdc
        e -> fromTextError $ "Failure parsing MigrationTypeValue from value: '" <> e
           <> "'. Accepted values: cdc, full-load, full-load-and-cdc"

instance ToText MigrationTypeValue where
    toText = \case
        Cdc -> "cdc"
        FullLoad -> "full-load"
        FullLoadAndCdc -> "full-load-and-cdc"

instance Hashable     MigrationTypeValue
instance NFData       MigrationTypeValue
instance ToByteString MigrationTypeValue
instance ToQuery      MigrationTypeValue
instance ToHeader     MigrationTypeValue

instance ToJSON MigrationTypeValue where
    toJSON = toJSONText

instance FromJSON MigrationTypeValue where
    parseJSON = parseJSONText "MigrationTypeValue"

data NestingLevelValue
  = None
  | One
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NestingLevelValue where
    parser = takeLowerText >>= \case
        "none" -> pure None
        "one" -> pure One
        e -> fromTextError $ "Failure parsing NestingLevelValue from value: '" <> e
           <> "'. Accepted values: none, one"

instance ToText NestingLevelValue where
    toText = \case
        None -> "none"
        One -> "one"

instance Hashable     NestingLevelValue
instance NFData       NestingLevelValue
instance ToByteString NestingLevelValue
instance ToQuery      NestingLevelValue
instance ToHeader     NestingLevelValue

instance ToJSON NestingLevelValue where
    toJSON = toJSONText

instance FromJSON NestingLevelValue where
    parseJSON = parseJSONText "NestingLevelValue"

data RefreshSchemasStatusTypeValue
  = Failed
  | Refreshing
  | Successful
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RefreshSchemasStatusTypeValue where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "refreshing" -> pure Refreshing
        "successful" -> pure Successful
        e -> fromTextError $ "Failure parsing RefreshSchemasStatusTypeValue from value: '" <> e
           <> "'. Accepted values: failed, refreshing, successful"

instance ToText RefreshSchemasStatusTypeValue where
    toText = \case
        Failed -> "failed"
        Refreshing -> "refreshing"
        Successful -> "successful"

instance Hashable     RefreshSchemasStatusTypeValue
instance NFData       RefreshSchemasStatusTypeValue
instance ToByteString RefreshSchemasStatusTypeValue
instance ToQuery      RefreshSchemasStatusTypeValue
instance ToHeader     RefreshSchemasStatusTypeValue

instance FromJSON RefreshSchemasStatusTypeValue where
    parseJSON = parseJSONText "RefreshSchemasStatusTypeValue"

data ReplicationEndpointTypeValue
  = Source
  | Target
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReplicationEndpointTypeValue where
    parser = takeLowerText >>= \case
        "source" -> pure Source
        "target" -> pure Target
        e -> fromTextError $ "Failure parsing ReplicationEndpointTypeValue from value: '" <> e
           <> "'. Accepted values: source, target"

instance ToText ReplicationEndpointTypeValue where
    toText = \case
        Source -> "source"
        Target -> "target"

instance Hashable     ReplicationEndpointTypeValue
instance NFData       ReplicationEndpointTypeValue
instance ToByteString ReplicationEndpointTypeValue
instance ToQuery      ReplicationEndpointTypeValue
instance ToHeader     ReplicationEndpointTypeValue

instance ToJSON ReplicationEndpointTypeValue where
    toJSON = toJSONText

instance FromJSON ReplicationEndpointTypeValue where
    parseJSON = parseJSONText "ReplicationEndpointTypeValue"

data SourceType =
  ReplicationInstance
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SourceType where
    parser = takeLowerText >>= \case
        "replication-instance" -> pure ReplicationInstance
        e -> fromTextError $ "Failure parsing SourceType from value: '" <> e
           <> "'. Accepted values: replication-instance"

instance ToText SourceType where
    toText = \case
        ReplicationInstance -> "replication-instance"

instance Hashable     SourceType
instance NFData       SourceType
instance ToByteString SourceType
instance ToQuery      SourceType
instance ToHeader     SourceType

instance ToJSON SourceType where
    toJSON = toJSONText

instance FromJSON SourceType where
    parseJSON = parseJSONText "SourceType"

data StartReplicationTaskTypeValue
  = ReloadTarget
  | ResumeProcessing
  | StartReplication
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StartReplicationTaskTypeValue where
    parser = takeLowerText >>= \case
        "reload-target" -> pure ReloadTarget
        "resume-processing" -> pure ResumeProcessing
        "start-replication" -> pure StartReplication
        e -> fromTextError $ "Failure parsing StartReplicationTaskTypeValue from value: '" <> e
           <> "'. Accepted values: reload-target, resume-processing, start-replication"

instance ToText StartReplicationTaskTypeValue where
    toText = \case
        ReloadTarget -> "reload-target"
        ResumeProcessing -> "resume-processing"
        StartReplication -> "start-replication"

instance Hashable     StartReplicationTaskTypeValue
instance NFData       StartReplicationTaskTypeValue
instance ToByteString StartReplicationTaskTypeValue
instance ToQuery      StartReplicationTaskTypeValue
instance ToHeader     StartReplicationTaskTypeValue

instance ToJSON StartReplicationTaskTypeValue where
    toJSON = toJSONText
