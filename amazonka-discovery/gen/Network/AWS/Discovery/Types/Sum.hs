{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.Sum where

import           Network.AWS.Prelude

data AgentStatus
    = Blacklisted
    | Healthy
    | Running
    | Shutdown
    | Unhealthy
    | Unknown
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText AgentStatus where
    parser = takeLowerText >>= \case
        "blacklisted" -> pure Blacklisted
        "healthy" -> pure Healthy
        "running" -> pure Running
        "shutdown" -> pure Shutdown
        "unhealthy" -> pure Unhealthy
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing AgentStatus from value: '" <> e
           <> "'. Accepted values: blacklisted, healthy, running, shutdown, unhealthy, unknown"

instance ToText AgentStatus where
    toText = \case
        Blacklisted -> "BLACKLISTED"
        Healthy -> "HEALTHY"
        Running -> "RUNNING"
        Shutdown -> "SHUTDOWN"
        Unhealthy -> "UNHEALTHY"
        Unknown -> "UNKNOWN"

instance Hashable     AgentStatus
instance NFData       AgentStatus
instance ToByteString AgentStatus
instance ToQuery      AgentStatus
instance ToHeader     AgentStatus

instance FromJSON AgentStatus where
    parseJSON = parseJSONText "AgentStatus"

data ConfigurationItemType
    = Connection
    | Process
    | Server
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ConfigurationItemType where
    parser = takeLowerText >>= \case
        "connection" -> pure Connection
        "process" -> pure Process
        "server" -> pure Server
        e -> fromTextError $ "Failure parsing ConfigurationItemType from value: '" <> e
           <> "'. Accepted values: connection, process, server"

instance ToText ConfigurationItemType where
    toText = \case
        Connection -> "CONNECTION"
        Process -> "PROCESS"
        Server -> "SERVER"

instance Hashable     ConfigurationItemType
instance NFData       ConfigurationItemType
instance ToByteString ConfigurationItemType
instance ToQuery      ConfigurationItemType
instance ToHeader     ConfigurationItemType

instance ToJSON ConfigurationItemType where
    toJSON = toJSONText

instance FromJSON ConfigurationItemType where
    parseJSON = parseJSONText "ConfigurationItemType"

data ExportStatus
    = Failed
    | InProgress
    | Succeeded
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ExportStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "in_progress" -> pure InProgress
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing ExportStatus from value: '" <> e
           <> "'. Accepted values: failed, in_progress, succeeded"

instance ToText ExportStatus where
    toText = \case
        Failed -> "FAILED"
        InProgress -> "IN_PROGRESS"
        Succeeded -> "SUCCEEDED"

instance Hashable     ExportStatus
instance NFData       ExportStatus
instance ToByteString ExportStatus
instance ToQuery      ExportStatus
instance ToHeader     ExportStatus

instance FromJSON ExportStatus where
    parseJSON = parseJSONText "ExportStatus"
