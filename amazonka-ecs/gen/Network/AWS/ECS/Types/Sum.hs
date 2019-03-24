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

data ClusterField
  = Statistics
  | Tags
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ClusterField where
    parser = takeLowerText >>= \case
        "statistics" -> pure Statistics
        "tags" -> pure Tags
        e -> fromTextError $ "Failure parsing ClusterField from value: '" <> e
           <> "'. Accepted values: statistics, tags"

instance ToText ClusterField where
    toText = \case
        Statistics -> "STATISTICS"
        Tags -> "TAGS"

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

data ContainerCondition
  = Complete
  | Healthy
  | Start
  | Success
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ContainerCondition where
    parser = takeLowerText >>= \case
        "complete" -> pure Complete
        "healthy" -> pure Healthy
        "start" -> pure Start
        "success" -> pure Success
        e -> fromTextError $ "Failure parsing ContainerCondition from value: '" <> e
           <> "'. Accepted values: complete, healthy, start, success"

instance ToText ContainerCondition where
    toText = \case
        Complete -> "COMPLETE"
        Healthy -> "HEALTHY"
        Start -> "START"
        Success -> "SUCCESS"

instance Hashable     ContainerCondition
instance NFData       ContainerCondition
instance ToByteString ContainerCondition
instance ToQuery      ContainerCondition
instance ToHeader     ContainerCondition

instance ToJSON ContainerCondition where
    toJSON = toJSONText

instance FromJSON ContainerCondition where
    parseJSON = parseJSONText "ContainerCondition"

data ContainerInstanceField =
  CIFTags
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ContainerInstanceField where
    parser = takeLowerText >>= \case
        "tags" -> pure CIFTags
        e -> fromTextError $ "Failure parsing ContainerInstanceField from value: '" <> e
           <> "'. Accepted values: tags"

instance ToText ContainerInstanceField where
    toText = \case
        CIFTags -> "TAGS"

instance Hashable     ContainerInstanceField
instance NFData       ContainerInstanceField
instance ToByteString ContainerInstanceField
instance ToQuery      ContainerInstanceField
instance ToHeader     ContainerInstanceField

instance ToJSON ContainerInstanceField where
    toJSON = toJSONText

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

data DeploymentControllerType
  = CodeDeploy
  | Ecs
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeploymentControllerType where
    parser = takeLowerText >>= \case
        "code_deploy" -> pure CodeDeploy
        "ecs" -> pure Ecs
        e -> fromTextError $ "Failure parsing DeploymentControllerType from value: '" <> e
           <> "'. Accepted values: code_deploy, ecs"

instance ToText DeploymentControllerType where
    toText = \case
        CodeDeploy -> "CODE_DEPLOY"
        Ecs -> "ECS"

instance Hashable     DeploymentControllerType
instance NFData       DeploymentControllerType
instance ToByteString DeploymentControllerType
instance ToQuery      DeploymentControllerType
instance ToHeader     DeploymentControllerType

instance ToJSON DeploymentControllerType where
    toJSON = toJSONText

instance FromJSON DeploymentControllerType where
    parseJSON = parseJSONText "DeploymentControllerType"

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
  = HSHealthy
  | HSUnhealthy
  | HSUnknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HealthStatus where
    parser = takeLowerText >>= \case
        "healthy" -> pure HSHealthy
        "unhealthy" -> pure HSUnhealthy
        "unknown" -> pure HSUnknown
        e -> fromTextError $ "Failure parsing HealthStatus from value: '" <> e
           <> "'. Accepted values: healthy, unhealthy, unknown"

instance ToText HealthStatus where
    toText = \case
        HSHealthy -> "HEALTHY"
        HSUnhealthy -> "UNHEALTHY"
        HSUnknown -> "UNKNOWN"

instance Hashable     HealthStatus
instance NFData       HealthStatus
instance ToByteString HealthStatus
instance ToQuery      HealthStatus
instance ToHeader     HealthStatus

instance FromJSON HealthStatus where
    parseJSON = parseJSONText "HealthStatus"

data IPcMode
  = IMHost
  | IMNone
  | IMTask
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IPcMode where
    parser = takeLowerText >>= \case
        "host" -> pure IMHost
        "none" -> pure IMNone
        "task" -> pure IMTask
        e -> fromTextError $ "Failure parsing IPcMode from value: '" <> e
           <> "'. Accepted values: host, none, task"

instance ToText IPcMode where
    toText = \case
        IMHost -> "host"
        IMNone -> "none"
        IMTask -> "task"

instance Hashable     IPcMode
instance NFData       IPcMode
instance ToByteString IPcMode
instance ToQuery      IPcMode
instance ToHeader     IPcMode

instance ToJSON IPcMode where
    toJSON = toJSONText

instance FromJSON IPcMode where
    parseJSON = parseJSONText "IPcMode"

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

data PidMode
  = PMHost
  | PMTask
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PidMode where
    parser = takeLowerText >>= \case
        "host" -> pure PMHost
        "task" -> pure PMTask
        e -> fromTextError $ "Failure parsing PidMode from value: '" <> e
           <> "'. Accepted values: host, task"

instance ToText PidMode where
    toText = \case
        PMHost -> "host"
        PMTask -> "task"

instance Hashable     PidMode
instance NFData       PidMode
instance ToByteString PidMode
instance ToQuery      PidMode
instance ToHeader     PidMode

instance ToJSON PidMode where
    toJSON = toJSONText

instance FromJSON PidMode where
    parseJSON = parseJSONText "PidMode"

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

data PlatformDeviceType =
  Gpu
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlatformDeviceType where
    parser = takeLowerText >>= \case
        "gpu" -> pure Gpu
        e -> fromTextError $ "Failure parsing PlatformDeviceType from value: '" <> e
           <> "'. Accepted values: gpu"

instance ToText PlatformDeviceType where
    toText = \case
        Gpu -> "GPU"

instance Hashable     PlatformDeviceType
instance NFData       PlatformDeviceType
instance ToByteString PlatformDeviceType
instance ToQuery      PlatformDeviceType
instance ToHeader     PlatformDeviceType

instance ToJSON PlatformDeviceType where
    toJSON = toJSONText

data PropagateTags
  = Service
  | TaskDefinition
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PropagateTags where
    parser = takeLowerText >>= \case
        "service" -> pure Service
        "task_definition" -> pure TaskDefinition
        e -> fromTextError $ "Failure parsing PropagateTags from value: '" <> e
           <> "'. Accepted values: service, task_definition"

instance ToText PropagateTags where
    toText = \case
        Service -> "SERVICE"
        TaskDefinition -> "TASK_DEFINITION"

instance Hashable     PropagateTags
instance NFData       PropagateTags
instance ToByteString PropagateTags
instance ToQuery      PropagateTags
instance ToHeader     PropagateTags

instance ToJSON PropagateTags where
    toJSON = toJSONText

instance FromJSON PropagateTags where
    parseJSON = parseJSONText "PropagateTags"

data ProxyConfigurationType =
  Appmesh
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProxyConfigurationType where
    parser = takeLowerText >>= \case
        "appmesh" -> pure Appmesh
        e -> fromTextError $ "Failure parsing ProxyConfigurationType from value: '" <> e
           <> "'. Accepted values: appmesh"

instance ToText ProxyConfigurationType where
    toText = \case
        Appmesh -> "APPMESH"

instance Hashable     ProxyConfigurationType
instance NFData       ProxyConfigurationType
instance ToByteString ProxyConfigurationType
instance ToQuery      ProxyConfigurationType
instance ToHeader     ProxyConfigurationType

instance ToJSON ProxyConfigurationType where
    toJSON = toJSONText

instance FromJSON ProxyConfigurationType where
    parseJSON = parseJSONText "ProxyConfigurationType"

data ResourceType =
  RTGpu
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceType where
    parser = takeLowerText >>= \case
        "gpu" -> pure RTGpu
        e -> fromTextError $ "Failure parsing ResourceType from value: '" <> e
           <> "'. Accepted values: gpu"

instance ToText ResourceType where
    toText = \case
        RTGpu -> "GPU"

instance Hashable     ResourceType
instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance ToJSON ResourceType where
    toJSON = toJSONText

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"

data ScaleUnit =
  Percent
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScaleUnit where
    parser = takeLowerText >>= \case
        "percent" -> pure Percent
        e -> fromTextError $ "Failure parsing ScaleUnit from value: '" <> e
           <> "'. Accepted values: percent"

instance ToText ScaleUnit where
    toText = \case
        Percent -> "PERCENT"

instance Hashable     ScaleUnit
instance NFData       ScaleUnit
instance ToByteString ScaleUnit
instance ToQuery      ScaleUnit
instance ToHeader     ScaleUnit

instance FromJSON ScaleUnit where
    parseJSON = parseJSONText "ScaleUnit"

data SchedulingStrategy
  = Daemon
  | Replica
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SchedulingStrategy where
    parser = takeLowerText >>= \case
        "daemon" -> pure Daemon
        "replica" -> pure Replica
        e -> fromTextError $ "Failure parsing SchedulingStrategy from value: '" <> e
           <> "'. Accepted values: daemon, replica"

instance ToText SchedulingStrategy where
    toText = \case
        Daemon -> "DAEMON"
        Replica -> "REPLICA"

instance Hashable     SchedulingStrategy
instance NFData       SchedulingStrategy
instance ToByteString SchedulingStrategy
instance ToQuery      SchedulingStrategy
instance ToHeader     SchedulingStrategy

instance ToJSON SchedulingStrategy where
    toJSON = toJSONText

instance FromJSON SchedulingStrategy where
    parseJSON = parseJSONText "SchedulingStrategy"

data Scope
  = Shared
  | Task
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Scope where
    parser = takeLowerText >>= \case
        "shared" -> pure Shared
        "task" -> pure Task
        e -> fromTextError $ "Failure parsing Scope from value: '" <> e
           <> "'. Accepted values: shared, task"

instance ToText Scope where
    toText = \case
        Shared -> "shared"
        Task -> "task"

instance Hashable     Scope
instance NFData       Scope
instance ToByteString Scope
instance ToQuery      Scope
instance ToHeader     Scope

instance ToJSON Scope where
    toJSON = toJSONText

instance FromJSON Scope where
    parseJSON = parseJSONText "Scope"

data ServiceField =
  SFTags
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServiceField where
    parser = takeLowerText >>= \case
        "tags" -> pure SFTags
        e -> fromTextError $ "Failure parsing ServiceField from value: '" <> e
           <> "'. Accepted values: tags"

instance ToText ServiceField where
    toText = \case
        SFTags -> "TAGS"

instance Hashable     ServiceField
instance NFData       ServiceField
instance ToByteString ServiceField
instance ToQuery      ServiceField
instance ToHeader     ServiceField

instance ToJSON ServiceField where
    toJSON = toJSONText

data SettingName
  = ContainerInstanceLongARNFormat
  | ServiceLongARNFormat
  | TaskLongARNFormat
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SettingName where
    parser = takeLowerText >>= \case
        "containerinstancelongarnformat" -> pure ContainerInstanceLongARNFormat
        "servicelongarnformat" -> pure ServiceLongARNFormat
        "tasklongarnformat" -> pure TaskLongARNFormat
        e -> fromTextError $ "Failure parsing SettingName from value: '" <> e
           <> "'. Accepted values: containerinstancelongarnformat, servicelongarnformat, tasklongarnformat"

instance ToText SettingName where
    toText = \case
        ContainerInstanceLongARNFormat -> "containerInstanceLongArnFormat"
        ServiceLongARNFormat -> "serviceLongArnFormat"
        TaskLongARNFormat -> "taskLongArnFormat"

instance Hashable     SettingName
instance NFData       SettingName
instance ToByteString SettingName
instance ToQuery      SettingName
instance ToHeader     SettingName

instance ToJSON SettingName where
    toJSON = toJSONText

instance FromJSON SettingName where
    parseJSON = parseJSONText "SettingName"

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

data StabilityStatus
  = Stabilizing
  | SteadyState
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StabilityStatus where
    parser = takeLowerText >>= \case
        "stabilizing" -> pure Stabilizing
        "steady_state" -> pure SteadyState
        e -> fromTextError $ "Failure parsing StabilityStatus from value: '" <> e
           <> "'. Accepted values: stabilizing, steady_state"

instance ToText StabilityStatus where
    toText = \case
        Stabilizing -> "STABILIZING"
        SteadyState -> "STEADY_STATE"

instance Hashable     StabilityStatus
instance NFData       StabilityStatus
instance ToByteString StabilityStatus
instance ToQuery      StabilityStatus
instance ToHeader     StabilityStatus

instance FromJSON StabilityStatus where
    parseJSON = parseJSONText "StabilityStatus"

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

data TaskDefinitionField =
  TDFTags
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TaskDefinitionField where
    parser = takeLowerText >>= \case
        "tags" -> pure TDFTags
        e -> fromTextError $ "Failure parsing TaskDefinitionField from value: '" <> e
           <> "'. Accepted values: tags"

instance ToText TaskDefinitionField where
    toText = \case
        TDFTags -> "TAGS"

instance Hashable     TaskDefinitionField
instance NFData       TaskDefinitionField
instance ToByteString TaskDefinitionField
instance ToQuery      TaskDefinitionField
instance ToHeader     TaskDefinitionField

instance ToJSON TaskDefinitionField where
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

data TaskField =
  TFTags
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TaskField where
    parser = takeLowerText >>= \case
        "tags" -> pure TFTags
        e -> fromTextError $ "Failure parsing TaskField from value: '" <> e
           <> "'. Accepted values: tags"

instance ToText TaskField where
    toText = \case
        TFTags -> "TAGS"

instance Hashable     TaskField
instance NFData       TaskField
instance ToByteString TaskField
instance ToQuery      TaskField
instance ToHeader     TaskField

instance ToJSON TaskField where
    toJSON = toJSONText

data TaskStopCode
  = EssentialContainerExited
  | TaskFailedToStart
  | UserInitiated
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TaskStopCode where
    parser = takeLowerText >>= \case
        "essentialcontainerexited" -> pure EssentialContainerExited
        "taskfailedtostart" -> pure TaskFailedToStart
        "userinitiated" -> pure UserInitiated
        e -> fromTextError $ "Failure parsing TaskStopCode from value: '" <> e
           <> "'. Accepted values: essentialcontainerexited, taskfailedtostart, userinitiated"

instance ToText TaskStopCode where
    toText = \case
        EssentialContainerExited -> "EssentialContainerExited"
        TaskFailedToStart -> "TaskFailedToStart"
        UserInitiated -> "UserInitiated"

instance Hashable     TaskStopCode
instance NFData       TaskStopCode
instance ToByteString TaskStopCode
instance ToQuery      TaskStopCode
instance ToHeader     TaskStopCode

instance FromJSON TaskStopCode where
    parseJSON = parseJSONText "TaskStopCode"

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
