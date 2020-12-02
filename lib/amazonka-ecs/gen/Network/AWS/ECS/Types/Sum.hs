{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.Sum where

import Network.AWS.Prelude

data AgentUpdateStatus
  = AUSFailed
  | AUSPending
  | AUSStaged
  | AUSStaging
  | AUSUpdated
  | AUSUpdating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

data AssignPublicIP
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AssignPublicIP where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing AssignPublicIP from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText AssignPublicIP where
    toText = \case
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"

instance Hashable     AssignPublicIP
instance NFData       AssignPublicIP
instance ToByteString AssignPublicIP
instance ToQuery      AssignPublicIP
instance ToHeader     AssignPublicIP

instance ToJSON AssignPublicIP where
    toJSON = toJSONText

instance FromJSON AssignPublicIP where
    parseJSON = parseJSONText "AssignPublicIP"

data ClusterField =
  Statistics
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ClusterField where
    parser = takeLowerText >>= \case
        "statistics" -> pure Statistics
        e -> fromTextError $ "Failure parsing ClusterField from value: '" <> e
           <> "'. Accepted values: statistics"

instance ToText ClusterField where
    toText = \case
        Statistics -> "STATISTICS"

instance Hashable     ClusterField
instance NFData       ClusterField
instance ToByteString ClusterField
instance ToQuery      ClusterField
instance ToHeader     ClusterField

instance ToJSON ClusterField where
    toJSON = toJSONText

data Compatibility
  = CEC2
  | CFargate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Compatibility where
    parser = takeLowerText >>= \case
        "ec2" -> pure CEC2
        "fargate" -> pure CFargate
        e -> fromTextError $ "Failure parsing Compatibility from value: '" <> e
           <> "'. Accepted values: ec2, fargate"

instance ToText Compatibility where
    toText = \case
        CEC2 -> "EC2"
        CFargate -> "FARGATE"

instance Hashable     Compatibility
instance NFData       Compatibility
instance ToByteString Compatibility
instance ToQuery      Compatibility
instance ToHeader     Compatibility

instance ToJSON Compatibility where
    toJSON = toJSONText

instance FromJSON Compatibility where
    parseJSON = parseJSONText "Compatibility"

data Connectivity
  = Connected
  | Disconnected
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Connectivity where
    parser = takeLowerText >>= \case
        "connected" -> pure Connected
        "disconnected" -> pure Disconnected
        e -> fromTextError $ "Failure parsing Connectivity from value: '" <> e
           <> "'. Accepted values: connected, disconnected"

instance ToText Connectivity where
    toText = \case
        Connected -> "CONNECTED"
        Disconnected -> "DISCONNECTED"

instance Hashable     Connectivity
instance NFData       Connectivity
instance ToByteString Connectivity
instance ToQuery      Connectivity
instance ToHeader     Connectivity

instance FromJSON Connectivity where
    parseJSON = parseJSONText "Connectivity"

data ContainerInstanceStatus
  = Active
  | Draining
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ContainerInstanceStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "draining" -> pure Draining
        e -> fromTextError $ "Failure parsing ContainerInstanceStatus from value: '" <> e
           <> "'. Accepted values: active, draining"

instance ToText ContainerInstanceStatus where
    toText = \case
        Active -> "ACTIVE"
        Draining -> "DRAINING"

instance Hashable     ContainerInstanceStatus
instance NFData       ContainerInstanceStatus
instance ToByteString ContainerInstanceStatus
instance ToQuery      ContainerInstanceStatus
instance ToHeader     ContainerInstanceStatus

instance ToJSON ContainerInstanceStatus where
    toJSON = toJSONText

data DesiredStatus
  = Pending
  | Running
  | Stopped
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

data DeviceCgroupPermission
  = Mknod
  | Read
  | Write
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeviceCgroupPermission where
    parser = takeLowerText >>= \case
        "mknod" -> pure Mknod
        "read" -> pure Read
        "write" -> pure Write
        e -> fromTextError $ "Failure parsing DeviceCgroupPermission from value: '" <> e
           <> "'. Accepted values: mknod, read, write"

instance ToText DeviceCgroupPermission where
    toText = \case
        Mknod -> "mknod"
        Read -> "read"
        Write -> "write"

instance Hashable     DeviceCgroupPermission
instance NFData       DeviceCgroupPermission
instance ToByteString DeviceCgroupPermission
instance ToQuery      DeviceCgroupPermission
instance ToHeader     DeviceCgroupPermission

instance ToJSON DeviceCgroupPermission where
    toJSON = toJSONText

instance FromJSON DeviceCgroupPermission where
    parseJSON = parseJSONText "DeviceCgroupPermission"

data HealthStatus
  = Healthy
  | Unhealthy
  | Unknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HealthStatus where
    parser = takeLowerText >>= \case
        "healthy" -> pure Healthy
        "unhealthy" -> pure Unhealthy
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing HealthStatus from value: '" <> e
           <> "'. Accepted values: healthy, unhealthy, unknown"

instance ToText HealthStatus where
    toText = \case
        Healthy -> "HEALTHY"
        Unhealthy -> "UNHEALTHY"
        Unknown -> "UNKNOWN"

instance Hashable     HealthStatus
instance NFData       HealthStatus
instance ToByteString HealthStatus
instance ToQuery      HealthStatus
instance ToHeader     HealthStatus

instance FromJSON HealthStatus where
    parseJSON = parseJSONText "HealthStatus"

data LaunchType
  = EC2
  | Fargate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LaunchType where
    parser = takeLowerText >>= \case
        "ec2" -> pure EC2
        "fargate" -> pure Fargate
        e -> fromTextError $ "Failure parsing LaunchType from value: '" <> e
           <> "'. Accepted values: ec2, fargate"

instance ToText LaunchType where
    toText = \case
        EC2 -> "EC2"
        Fargate -> "FARGATE"

instance Hashable     LaunchType
instance NFData       LaunchType
instance ToByteString LaunchType
instance ToQuery      LaunchType
instance ToHeader     LaunchType

instance ToJSON LaunchType where
    toJSON = toJSONText

instance FromJSON LaunchType where
    parseJSON = parseJSONText "LaunchType"

data LogDriver
  = AWSlogs
  | Fluentd
  | Gelf
  | JSONFile
  | Journald
  | Splunk
  | Syslog
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LogDriver where
    parser = takeLowerText >>= \case
        "awslogs" -> pure AWSlogs
        "fluentd" -> pure Fluentd
        "gelf" -> pure Gelf
        "json-file" -> pure JSONFile
        "journald" -> pure Journald
        "splunk" -> pure Splunk
        "syslog" -> pure Syslog
        e -> fromTextError $ "Failure parsing LogDriver from value: '" <> e
           <> "'. Accepted values: awslogs, fluentd, gelf, json-file, journald, splunk, syslog"

instance ToText LogDriver where
    toText = \case
        AWSlogs -> "awslogs"
        Fluentd -> "fluentd"
        Gelf -> "gelf"
        JSONFile -> "json-file"
        Journald -> "journald"
        Splunk -> "splunk"
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

data NetworkMode
  = AWSvpc
  | Bridge
  | Host
  | None
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NetworkMode where
    parser = takeLowerText >>= \case
        "awsvpc" -> pure AWSvpc
        "bridge" -> pure Bridge
        "host" -> pure Host
        "none" -> pure None
        e -> fromTextError $ "Failure parsing NetworkMode from value: '" <> e
           <> "'. Accepted values: awsvpc, bridge, host, none"

instance ToText NetworkMode where
    toText = \case
        AWSvpc -> "awsvpc"
        Bridge -> "bridge"
        Host -> "host"
        None -> "none"

instance Hashable     NetworkMode
instance NFData       NetworkMode
instance ToByteString NetworkMode
instance ToQuery      NetworkMode
instance ToHeader     NetworkMode

instance ToJSON NetworkMode where
    toJSON = toJSONText

instance FromJSON NetworkMode where
    parseJSON = parseJSONText "NetworkMode"

data PlacementConstraintType
  = PCTDistinctInstance
  | PCTMemberOf
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlacementConstraintType where
    parser = takeLowerText >>= \case
        "distinctinstance" -> pure PCTDistinctInstance
        "memberof" -> pure PCTMemberOf
        e -> fromTextError $ "Failure parsing PlacementConstraintType from value: '" <> e
           <> "'. Accepted values: distinctinstance, memberof"

instance ToText PlacementConstraintType where
    toText = \case
        PCTDistinctInstance -> "distinctInstance"
        PCTMemberOf -> "memberOf"

instance Hashable     PlacementConstraintType
instance NFData       PlacementConstraintType
instance ToByteString PlacementConstraintType
instance ToQuery      PlacementConstraintType
instance ToHeader     PlacementConstraintType

instance ToJSON PlacementConstraintType where
    toJSON = toJSONText

instance FromJSON PlacementConstraintType where
    parseJSON = parseJSONText "PlacementConstraintType"

data PlacementStrategyType
  = Binpack
  | Random
  | Spread
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlacementStrategyType where
    parser = takeLowerText >>= \case
        "binpack" -> pure Binpack
        "random" -> pure Random
        "spread" -> pure Spread
        e -> fromTextError $ "Failure parsing PlacementStrategyType from value: '" <> e
           <> "'. Accepted values: binpack, random, spread"

instance ToText PlacementStrategyType where
    toText = \case
        Binpack -> "binpack"
        Random -> "random"
        Spread -> "spread"

instance Hashable     PlacementStrategyType
instance NFData       PlacementStrategyType
instance ToByteString PlacementStrategyType
instance ToQuery      PlacementStrategyType
instance ToHeader     PlacementStrategyType

instance ToJSON PlacementStrategyType where
    toJSON = toJSONText

instance FromJSON PlacementStrategyType where
    parseJSON = parseJSONText "PlacementStrategyType"

data SortOrder
  = Asc
  | Desc
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

data TargetType =
  ContainerInstance
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TargetType where
    parser = takeLowerText >>= \case
        "container-instance" -> pure ContainerInstance
        e -> fromTextError $ "Failure parsing TargetType from value: '" <> e
           <> "'. Accepted values: container-instance"

instance ToText TargetType where
    toText = \case
        ContainerInstance -> "container-instance"

instance Hashable     TargetType
instance NFData       TargetType
instance ToByteString TargetType
instance ToQuery      TargetType
instance ToHeader     TargetType

instance ToJSON TargetType where
    toJSON = toJSONText

instance FromJSON TargetType where
    parseJSON = parseJSONText "TargetType"

data TaskDefinitionFamilyStatus
  = TDFSActive
  | TDFSAll
  | TDFSInactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TaskDefinitionFamilyStatus where
    parser = takeLowerText >>= \case
        "active" -> pure TDFSActive
        "all" -> pure TDFSAll
        "inactive" -> pure TDFSInactive
        e -> fromTextError $ "Failure parsing TaskDefinitionFamilyStatus from value: '" <> e
           <> "'. Accepted values: active, all, inactive"

instance ToText TaskDefinitionFamilyStatus where
    toText = \case
        TDFSActive -> "ACTIVE"
        TDFSAll -> "ALL"
        TDFSInactive -> "INACTIVE"

instance Hashable     TaskDefinitionFamilyStatus
instance NFData       TaskDefinitionFamilyStatus
instance ToByteString TaskDefinitionFamilyStatus
instance ToQuery      TaskDefinitionFamilyStatus
instance ToHeader     TaskDefinitionFamilyStatus

instance ToJSON TaskDefinitionFamilyStatus where
    toJSON = toJSONText

data TaskDefinitionPlacementConstraintType =
  MemberOf
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TaskDefinitionPlacementConstraintType where
    parser = takeLowerText >>= \case
        "memberof" -> pure MemberOf
        e -> fromTextError $ "Failure parsing TaskDefinitionPlacementConstraintType from value: '" <> e
           <> "'. Accepted values: memberof"

instance ToText TaskDefinitionPlacementConstraintType where
    toText = \case
        MemberOf -> "memberOf"

instance Hashable     TaskDefinitionPlacementConstraintType
instance NFData       TaskDefinitionPlacementConstraintType
instance ToByteString TaskDefinitionPlacementConstraintType
instance ToQuery      TaskDefinitionPlacementConstraintType
instance ToHeader     TaskDefinitionPlacementConstraintType

instance ToJSON TaskDefinitionPlacementConstraintType where
    toJSON = toJSONText

instance FromJSON TaskDefinitionPlacementConstraintType where
    parseJSON = parseJSONText "TaskDefinitionPlacementConstraintType"

data TaskDefinitionStatus
  = TDSActive
  | TDSInactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
