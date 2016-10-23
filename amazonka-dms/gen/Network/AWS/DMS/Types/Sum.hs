{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.Sum where

import           Network.AWS.Prelude

data DmsSSLModeValue
    = None
    | Require
    | VerifyCa
    | VerifyFull
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DmsSSLModeValue where
    parser = takeLowerText >>= \case
        "none" -> pure None
        "require" -> pure Require
        "verify-ca" -> pure VerifyCa
        "verify-full" -> pure VerifyFull
        e -> fromTextError $ "Failure parsing DmsSSLModeValue from value: '" <> e
           <> "'. Accepted values: none, require, verify-ca, verify-full"

instance ToText DmsSSLModeValue where
    toText = \case
        None -> "none"
        Require -> "require"
        VerifyCa -> "verify-ca"
        VerifyFull -> "verify-full"

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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

data RefreshSchemasStatusTypeValue
    = Failed
    | Refreshing
    | Successful
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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

data StartReplicationTaskTypeValue
    = ReloadTarget
    | ResumeProcessing
    | StartReplication
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
