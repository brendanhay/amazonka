{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.Sum where

import           Network.AWS.Prelude

data AgentUpdateStatus
    = AUSFailed
    | AUSPending
    | AUSStaged
    | AUSStaging
    | AUSUpdated
    | AUSUpdating
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText AgentUpdateStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure AUSFailed
        "pending" -> pure AUSPending
        "staged" -> pure AUSStaged
        "staging" -> pure AUSStaging
        "updated" -> pure AUSUpdated
        "updating" -> pure AUSUpdating
        e -> fromTextError $ "Failure parsing AgentUpdateStatus from value: '" <> e
           <> "'. Accepted values: failed, pending, staged, staging, updated, updating"

instance ToText AgentUpdateStatus where
    toText = \case
        AUSFailed -> "FAILED"
        AUSPending -> "PENDING"
        AUSStaged -> "STAGED"
        AUSStaging -> "STAGING"
        AUSUpdated -> "UPDATED"
        AUSUpdating -> "UPDATING"

instance Hashable     AgentUpdateStatus
instance NFData       AgentUpdateStatus
instance ToByteString AgentUpdateStatus
instance ToQuery      AgentUpdateStatus
instance ToHeader     AgentUpdateStatus

instance FromJSON AgentUpdateStatus where
    parseJSON = parseJSONText "AgentUpdateStatus"

data DesiredStatus
    = Pending
    | Running
    | Stopped
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DesiredStatus where
    parser = takeLowerText >>= \case
        "pending" -> pure Pending
        "running" -> pure Running
        "stopped" -> pure Stopped
        e -> fromTextError $ "Failure parsing DesiredStatus from value: '" <> e
           <> "'. Accepted values: pending, running, stopped"

instance ToText DesiredStatus where
    toText = \case
        Pending -> "PENDING"
        Running -> "RUNNING"
        Stopped -> "STOPPED"

instance Hashable     DesiredStatus
instance NFData       DesiredStatus
instance ToByteString DesiredStatus
instance ToQuery      DesiredStatus
instance ToHeader     DesiredStatus

instance ToJSON DesiredStatus where
    toJSON = toJSONText

data LogDriver
    = AWSlogs
    | Fluentd
    | Gelf
    | JSONFile
    | Journald
    | Syslog
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText LogDriver where
    parser = takeLowerText >>= \case
        "awslogs" -> pure AWSlogs
        "fluentd" -> pure Fluentd
        "gelf" -> pure Gelf
        "json-file" -> pure JSONFile
        "journald" -> pure Journald
        "syslog" -> pure Syslog
        e -> fromTextError $ "Failure parsing LogDriver from value: '" <> e
           <> "'. Accepted values: awslogs, fluentd, gelf, json-file, journald, syslog"

instance ToText LogDriver where
    toText = \case
        AWSlogs -> "awslogs"
        Fluentd -> "fluentd"
        Gelf -> "gelf"
        JSONFile -> "json-file"
        Journald -> "journald"
        Syslog -> "syslog"

instance Hashable     LogDriver
instance NFData       LogDriver
instance ToByteString LogDriver
instance ToQuery      LogDriver
instance ToHeader     LogDriver

instance ToJSON LogDriver where
    toJSON = toJSONText

instance FromJSON LogDriver where
    parseJSON = parseJSONText "LogDriver"

data SortOrder
    = Asc
    | Desc
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText SortOrder where
    parser = takeLowerText >>= \case
        "asc" -> pure Asc
        "desc" -> pure Desc
        e -> fromTextError $ "Failure parsing SortOrder from value: '" <> e
           <> "'. Accepted values: asc, desc"

instance ToText SortOrder where
    toText = \case
        Asc -> "ASC"
        Desc -> "DESC"

instance Hashable     SortOrder
instance NFData       SortOrder
instance ToByteString SortOrder
instance ToQuery      SortOrder
instance ToHeader     SortOrder

instance ToJSON SortOrder where
    toJSON = toJSONText

data TaskDefinitionFamilyStatus
    = Active
    | All
    | Inactive
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText TaskDefinitionFamilyStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "all" -> pure All
        "inactive" -> pure Inactive
        e -> fromTextError $ "Failure parsing TaskDefinitionFamilyStatus from value: '" <> e
           <> "'. Accepted values: active, all, inactive"

instance ToText TaskDefinitionFamilyStatus where
    toText = \case
        Active -> "ACTIVE"
        All -> "ALL"
        Inactive -> "INACTIVE"

instance Hashable     TaskDefinitionFamilyStatus
instance NFData       TaskDefinitionFamilyStatus
instance ToByteString TaskDefinitionFamilyStatus
instance ToQuery      TaskDefinitionFamilyStatus
instance ToHeader     TaskDefinitionFamilyStatus

instance ToJSON TaskDefinitionFamilyStatus where
    toJSON = toJSONText

data TaskDefinitionStatus
    = TDSActive
    | TDSInactive
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText TaskDefinitionStatus where
    parser = takeLowerText >>= \case
        "active" -> pure TDSActive
        "inactive" -> pure TDSInactive
        e -> fromTextError $ "Failure parsing TaskDefinitionStatus from value: '" <> e
           <> "'. Accepted values: active, inactive"

instance ToText TaskDefinitionStatus where
    toText = \case
        TDSActive -> "ACTIVE"
        TDSInactive -> "INACTIVE"

instance Hashable     TaskDefinitionStatus
instance NFData       TaskDefinitionStatus
instance ToByteString TaskDefinitionStatus
instance ToQuery      TaskDefinitionStatus
instance ToHeader     TaskDefinitionStatus

instance ToJSON TaskDefinitionStatus where
    toJSON = toJSONText

instance FromJSON TaskDefinitionStatus where
    parseJSON = parseJSONText "TaskDefinitionStatus"

data TransportProtocol
    = TCP
    | Udp
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText TransportProtocol where
    parser = takeLowerText >>= \case
        "tcp" -> pure TCP
        "udp" -> pure Udp
        e -> fromTextError $ "Failure parsing TransportProtocol from value: '" <> e
           <> "'. Accepted values: tcp, udp"

instance ToText TransportProtocol where
    toText = \case
        TCP -> "tcp"
        Udp -> "udp"

instance Hashable     TransportProtocol
instance NFData       TransportProtocol
instance ToByteString TransportProtocol
instance ToQuery      TransportProtocol
instance ToHeader     TransportProtocol

instance ToJSON TransportProtocol where
    toJSON = toJSONText

instance FromJSON TransportProtocol where
    parseJSON = parseJSONText "TransportProtocol"

data UlimitName
    = CPU
    | Core
    | Data
    | Fsize
    | Locks
    | Memlock
    | Msgqueue
    | Nice
    | Nofile
    | Nproc
    | Rss
    | Rtprio
    | Rttime
    | Sigpending
    | Stack
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText UlimitName where
    parser = takeLowerText >>= \case
        "cpu" -> pure CPU
        "core" -> pure Core
        "data" -> pure Data
        "fsize" -> pure Fsize
        "locks" -> pure Locks
        "memlock" -> pure Memlock
        "msgqueue" -> pure Msgqueue
        "nice" -> pure Nice
        "nofile" -> pure Nofile
        "nproc" -> pure Nproc
        "rss" -> pure Rss
        "rtprio" -> pure Rtprio
        "rttime" -> pure Rttime
        "sigpending" -> pure Sigpending
        "stack" -> pure Stack
        e -> fromTextError $ "Failure parsing UlimitName from value: '" <> e
           <> "'. Accepted values: cpu, core, data, fsize, locks, memlock, msgqueue, nice, nofile, nproc, rss, rtprio, rttime, sigpending, stack"

instance ToText UlimitName where
    toText = \case
        CPU -> "cpu"
        Core -> "core"
        Data -> "data"
        Fsize -> "fsize"
        Locks -> "locks"
        Memlock -> "memlock"
        Msgqueue -> "msgqueue"
        Nice -> "nice"
        Nofile -> "nofile"
        Nproc -> "nproc"
        Rss -> "rss"
        Rtprio -> "rtprio"
        Rttime -> "rttime"
        Sigpending -> "sigpending"
        Stack -> "stack"

instance Hashable     UlimitName
instance NFData       UlimitName
instance ToByteString UlimitName
instance ToQuery      UlimitName
instance ToHeader     UlimitName

instance ToJSON UlimitName where
    toJSON = toJSONText

instance FromJSON UlimitName where
    parseJSON = parseJSONText "UlimitName"
