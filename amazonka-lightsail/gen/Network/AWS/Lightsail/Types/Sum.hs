{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Sum
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.Sum where

import Network.AWS.Prelude

data AccessDirection
  = Inbound
  | Outbound
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AccessDirection where
    parser = takeLowerText >>= \case
        "inbound" -> pure Inbound
        "outbound" -> pure Outbound
        e -> fromTextError $ "Failure parsing AccessDirection from value: '" <> e
           <> "'. Accepted values: inbound, outbound"

instance ToText AccessDirection where
    toText = \case
        Inbound -> "inbound"
        Outbound -> "outbound"

instance Hashable     AccessDirection
instance NFData       AccessDirection
instance ToByteString AccessDirection
instance ToQuery      AccessDirection
instance ToHeader     AccessDirection

instance FromJSON AccessDirection where
    parseJSON = parseJSONText "AccessDirection"

data BlueprintType
  = App
  | OS
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BlueprintType where
    parser = takeLowerText >>= \case
        "app" -> pure App
        "os" -> pure OS
        e -> fromTextError $ "Failure parsing BlueprintType from value: '" <> e
           <> "'. Accepted values: app, os"

instance ToText BlueprintType where
    toText = \case
        App -> "app"
        OS -> "os"

instance Hashable     BlueprintType
instance NFData       BlueprintType
instance ToByteString BlueprintType
instance ToQuery      BlueprintType
instance ToHeader     BlueprintType

instance FromJSON BlueprintType where
    parseJSON = parseJSONText "BlueprintType"

data InstanceAccessProtocol
  = Rdp
  | SSH
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceAccessProtocol where
    parser = takeLowerText >>= \case
        "rdp" -> pure Rdp
        "ssh" -> pure SSH
        e -> fromTextError $ "Failure parsing InstanceAccessProtocol from value: '" <> e
           <> "'. Accepted values: rdp, ssh"

instance ToText InstanceAccessProtocol where
    toText = \case
        Rdp -> "rdp"
        SSH -> "ssh"

instance Hashable     InstanceAccessProtocol
instance NFData       InstanceAccessProtocol
instance ToByteString InstanceAccessProtocol
instance ToQuery      InstanceAccessProtocol
instance ToHeader     InstanceAccessProtocol

instance ToJSON InstanceAccessProtocol where
    toJSON = toJSONText

instance FromJSON InstanceAccessProtocol where
    parseJSON = parseJSONText "InstanceAccessProtocol"

data InstanceMetricName
  = CPUUtilization
  | NetworkIn
  | NetworkOut
  | StatusCheckFailed
  | StatusCheckFailedInstance
  | StatusCheckFailedSystem
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceMetricName where
    parser = takeLowerText >>= \case
        "cpuutilization" -> pure CPUUtilization
        "networkin" -> pure NetworkIn
        "networkout" -> pure NetworkOut
        "statuscheckfailed" -> pure StatusCheckFailed
        "statuscheckfailed_instance" -> pure StatusCheckFailedInstance
        "statuscheckfailed_system" -> pure StatusCheckFailedSystem
        e -> fromTextError $ "Failure parsing InstanceMetricName from value: '" <> e
           <> "'. Accepted values: cpuutilization, networkin, networkout, statuscheckfailed, statuscheckfailed_instance, statuscheckfailed_system"

instance ToText InstanceMetricName where
    toText = \case
        CPUUtilization -> "CPUUtilization"
        NetworkIn -> "NetworkIn"
        NetworkOut -> "NetworkOut"
        StatusCheckFailed -> "StatusCheckFailed"
        StatusCheckFailedInstance -> "StatusCheckFailed_Instance"
        StatusCheckFailedSystem -> "StatusCheckFailed_System"

instance Hashable     InstanceMetricName
instance NFData       InstanceMetricName
instance ToByteString InstanceMetricName
instance ToQuery      InstanceMetricName
instance ToHeader     InstanceMetricName

instance ToJSON InstanceMetricName where
    toJSON = toJSONText

instance FromJSON InstanceMetricName where
    parseJSON = parseJSONText "InstanceMetricName"

data InstancePlatform
  = LinuxUnix
  | Windows
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstancePlatform where
    parser = takeLowerText >>= \case
        "linux_unix" -> pure LinuxUnix
        "windows" -> pure Windows
        e -> fromTextError $ "Failure parsing InstancePlatform from value: '" <> e
           <> "'. Accepted values: linux_unix, windows"

instance ToText InstancePlatform where
    toText = \case
        LinuxUnix -> "LINUX_UNIX"
        Windows -> "WINDOWS"

instance Hashable     InstancePlatform
instance NFData       InstancePlatform
instance ToByteString InstancePlatform
instance ToQuery      InstancePlatform
instance ToHeader     InstancePlatform

instance FromJSON InstancePlatform where
    parseJSON = parseJSONText "InstancePlatform"

data InstanceSnapshotState
  = Available
  | Error'
  | Pending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceSnapshotState where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "error" -> pure Error'
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing InstanceSnapshotState from value: '" <> e
           <> "'. Accepted values: available, error, pending"

instance ToText InstanceSnapshotState where
    toText = \case
        Available -> "available"
        Error' -> "error"
        Pending -> "pending"

instance Hashable     InstanceSnapshotState
instance NFData       InstanceSnapshotState
instance ToByteString InstanceSnapshotState
instance ToQuery      InstanceSnapshotState
instance ToHeader     InstanceSnapshotState

instance FromJSON InstanceSnapshotState where
    parseJSON = parseJSONText "InstanceSnapshotState"

data MetricStatistic
  = Average
  | Maximum
  | Minimum
  | SampleCount
  | Sum
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MetricStatistic where
    parser = takeLowerText >>= \case
        "average" -> pure Average
        "maximum" -> pure Maximum
        "minimum" -> pure Minimum
        "samplecount" -> pure SampleCount
        "sum" -> pure Sum
        e -> fromTextError $ "Failure parsing MetricStatistic from value: '" <> e
           <> "'. Accepted values: average, maximum, minimum, samplecount, sum"

instance ToText MetricStatistic where
    toText = \case
        Average -> "Average"
        Maximum -> "Maximum"
        Minimum -> "Minimum"
        SampleCount -> "SampleCount"
        Sum -> "Sum"

instance Hashable     MetricStatistic
instance NFData       MetricStatistic
instance ToByteString MetricStatistic
instance ToQuery      MetricStatistic
instance ToHeader     MetricStatistic

instance ToJSON MetricStatistic where
    toJSON = toJSONText

data MetricUnit
  = Bits
  | BitsSecond
  | Bytes
  | BytesSecond
  | Count
  | CountSecond
  | Gigabits
  | GigabitsSecond
  | Gigabytes
  | GigabytesSecond
  | Kilobits
  | KilobitsSecond
  | Kilobytes
  | KilobytesSecond
  | Megabits
  | MegabitsSecond
  | Megabytes
  | MegabytesSecond
  | Microseconds
  | Milliseconds
  | None
  | Percent
  | Seconds
  | Terabits
  | TerabitsSecond
  | Terabytes
  | TerabytesSecond
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MetricUnit where
    parser = takeLowerText >>= \case
        "bits" -> pure Bits
        "bits/second" -> pure BitsSecond
        "bytes" -> pure Bytes
        "bytes/second" -> pure BytesSecond
        "count" -> pure Count
        "count/second" -> pure CountSecond
        "gigabits" -> pure Gigabits
        "gigabits/second" -> pure GigabitsSecond
        "gigabytes" -> pure Gigabytes
        "gigabytes/second" -> pure GigabytesSecond
        "kilobits" -> pure Kilobits
        "kilobits/second" -> pure KilobitsSecond
        "kilobytes" -> pure Kilobytes
        "kilobytes/second" -> pure KilobytesSecond
        "megabits" -> pure Megabits
        "megabits/second" -> pure MegabitsSecond
        "megabytes" -> pure Megabytes
        "megabytes/second" -> pure MegabytesSecond
        "microseconds" -> pure Microseconds
        "milliseconds" -> pure Milliseconds
        "none" -> pure None
        "percent" -> pure Percent
        "seconds" -> pure Seconds
        "terabits" -> pure Terabits
        "terabits/second" -> pure TerabitsSecond
        "terabytes" -> pure Terabytes
        "terabytes/second" -> pure TerabytesSecond
        e -> fromTextError $ "Failure parsing MetricUnit from value: '" <> e
           <> "'. Accepted values: bits, bits/second, bytes, bytes/second, count, count/second, gigabits, gigabits/second, gigabytes, gigabytes/second, kilobits, kilobits/second, kilobytes, kilobytes/second, megabits, megabits/second, megabytes, megabytes/second, microseconds, milliseconds, none, percent, seconds, terabits, terabits/second, terabytes, terabytes/second"

instance ToText MetricUnit where
    toText = \case
        Bits -> "Bits"
        BitsSecond -> "Bits/Second"
        Bytes -> "Bytes"
        BytesSecond -> "Bytes/Second"
        Count -> "Count"
        CountSecond -> "Count/Second"
        Gigabits -> "Gigabits"
        GigabitsSecond -> "Gigabits/Second"
        Gigabytes -> "Gigabytes"
        GigabytesSecond -> "Gigabytes/Second"
        Kilobits -> "Kilobits"
        KilobitsSecond -> "Kilobits/Second"
        Kilobytes -> "Kilobytes"
        KilobytesSecond -> "Kilobytes/Second"
        Megabits -> "Megabits"
        MegabitsSecond -> "Megabits/Second"
        Megabytes -> "Megabytes"
        MegabytesSecond -> "Megabytes/Second"
        Microseconds -> "Microseconds"
        Milliseconds -> "Milliseconds"
        None -> "None"
        Percent -> "Percent"
        Seconds -> "Seconds"
        Terabits -> "Terabits"
        TerabitsSecond -> "Terabits/Second"
        Terabytes -> "Terabytes"
        TerabytesSecond -> "Terabytes/Second"

instance Hashable     MetricUnit
instance NFData       MetricUnit
instance ToByteString MetricUnit
instance ToQuery      MetricUnit
instance ToHeader     MetricUnit

instance ToJSON MetricUnit where
    toJSON = toJSONText

instance FromJSON MetricUnit where
    parseJSON = parseJSONText "MetricUnit"

data NetworkProtocol
  = All
  | TCP
  | Udp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NetworkProtocol where
    parser = takeLowerText >>= \case
        "all" -> pure All
        "tcp" -> pure TCP
        "udp" -> pure Udp
        e -> fromTextError $ "Failure parsing NetworkProtocol from value: '" <> e
           <> "'. Accepted values: all, tcp, udp"

instance ToText NetworkProtocol where
    toText = \case
        All -> "all"
        TCP -> "tcp"
        Udp -> "udp"

instance Hashable     NetworkProtocol
instance NFData       NetworkProtocol
instance ToByteString NetworkProtocol
instance ToQuery      NetworkProtocol
instance ToHeader     NetworkProtocol

instance ToJSON NetworkProtocol where
    toJSON = toJSONText

instance FromJSON NetworkProtocol where
    parseJSON = parseJSONText "NetworkProtocol"

data OperationStatus
  = Completed
  | Failed
  | NotStarted
  | Started
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OperationStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "failed" -> pure Failed
        "notstarted" -> pure NotStarted
        "started" -> pure Started
        e -> fromTextError $ "Failure parsing OperationStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, notstarted, started"

instance ToText OperationStatus where
    toText = \case
        Completed -> "Completed"
        Failed -> "Failed"
        NotStarted -> "NotStarted"
        Started -> "Started"

instance Hashable     OperationStatus
instance NFData       OperationStatus
instance ToByteString OperationStatus
instance ToQuery      OperationStatus
instance ToHeader     OperationStatus

instance FromJSON OperationStatus where
    parseJSON = parseJSONText "OperationStatus"

data OperationType
  = AllocateStaticIP
  | AttachStaticIP
  | CloseInstancePublicPorts
  | CreateDomain
  | CreateInstance
  | CreateInstanceSnapshot
  | CreateInstancesFromSnapshot
  | DeleteDomain
  | DeleteDomainEntry
  | DeleteInstance
  | DeleteInstanceSnapshot
  | DetachStaticIP
  | OpenInstancePublicPorts
  | PutInstancePublicPorts
  | RebootInstance
  | ReleaseStaticIP
  | StartInstance
  | StopInstance
  | UpdateDomainEntry
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OperationType where
    parser = takeLowerText >>= \case
        "allocatestaticip" -> pure AllocateStaticIP
        "attachstaticip" -> pure AttachStaticIP
        "closeinstancepublicports" -> pure CloseInstancePublicPorts
        "createdomain" -> pure CreateDomain
        "createinstance" -> pure CreateInstance
        "createinstancesnapshot" -> pure CreateInstanceSnapshot
        "createinstancesfromsnapshot" -> pure CreateInstancesFromSnapshot
        "deletedomain" -> pure DeleteDomain
        "deletedomainentry" -> pure DeleteDomainEntry
        "deleteinstance" -> pure DeleteInstance
        "deleteinstancesnapshot" -> pure DeleteInstanceSnapshot
        "detachstaticip" -> pure DetachStaticIP
        "openinstancepublicports" -> pure OpenInstancePublicPorts
        "putinstancepublicports" -> pure PutInstancePublicPorts
        "rebootinstance" -> pure RebootInstance
        "releasestaticip" -> pure ReleaseStaticIP
        "startinstance" -> pure StartInstance
        "stopinstance" -> pure StopInstance
        "updatedomainentry" -> pure UpdateDomainEntry
        e -> fromTextError $ "Failure parsing OperationType from value: '" <> e
           <> "'. Accepted values: allocatestaticip, attachstaticip, closeinstancepublicports, createdomain, createinstance, createinstancesnapshot, createinstancesfromsnapshot, deletedomain, deletedomainentry, deleteinstance, deleteinstancesnapshot, detachstaticip, openinstancepublicports, putinstancepublicports, rebootinstance, releasestaticip, startinstance, stopinstance, updatedomainentry"

instance ToText OperationType where
    toText = \case
        AllocateStaticIP -> "AllocateStaticIp"
        AttachStaticIP -> "AttachStaticIp"
        CloseInstancePublicPorts -> "CloseInstancePublicPorts"
        CreateDomain -> "CreateDomain"
        CreateInstance -> "CreateInstance"
        CreateInstanceSnapshot -> "CreateInstanceSnapshot"
        CreateInstancesFromSnapshot -> "CreateInstancesFromSnapshot"
        DeleteDomain -> "DeleteDomain"
        DeleteDomainEntry -> "DeleteDomainEntry"
        DeleteInstance -> "DeleteInstance"
        DeleteInstanceSnapshot -> "DeleteInstanceSnapshot"
        DetachStaticIP -> "DetachStaticIp"
        OpenInstancePublicPorts -> "OpenInstancePublicPorts"
        PutInstancePublicPorts -> "PutInstancePublicPorts"
        RebootInstance -> "RebootInstance"
        ReleaseStaticIP -> "ReleaseStaticIp"
        StartInstance -> "StartInstance"
        StopInstance -> "StopInstance"
        UpdateDomainEntry -> "UpdateDomainEntry"

instance Hashable     OperationType
instance NFData       OperationType
instance ToByteString OperationType
instance ToQuery      OperationType
instance ToHeader     OperationType

instance FromJSON OperationType where
    parseJSON = parseJSONText "OperationType"

data PortAccessType
  = Private
  | Public
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PortAccessType where
    parser = takeLowerText >>= \case
        "private" -> pure Private
        "public" -> pure Public
        e -> fromTextError $ "Failure parsing PortAccessType from value: '" <> e
           <> "'. Accepted values: private, public"

instance ToText PortAccessType where
    toText = \case
        Private -> "Private"
        Public -> "Public"

instance Hashable     PortAccessType
instance NFData       PortAccessType
instance ToByteString PortAccessType
instance ToQuery      PortAccessType
instance ToHeader     PortAccessType

instance FromJSON PortAccessType where
    parseJSON = parseJSONText "PortAccessType"

data PortState
  = Closed
  | Open
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PortState where
    parser = takeLowerText >>= \case
        "closed" -> pure Closed
        "open" -> pure Open
        e -> fromTextError $ "Failure parsing PortState from value: '" <> e
           <> "'. Accepted values: closed, open"

instance ToText PortState where
    toText = \case
        Closed -> "closed"
        Open -> "open"

instance Hashable     PortState
instance NFData       PortState
instance ToByteString PortState
instance ToQuery      PortState
instance ToHeader     PortState

instance FromJSON PortState where
    parseJSON = parseJSONText "PortState"

data RegionName
  = ApNortheast1
  | ApNortheast2
  | ApSouth1
  | ApSoutheast1
  | ApSoutheast2
  | EuCentral1
  | EuWest1
  | UsEast1
  | UsEast2
  | UsWest1
  | UsWest2
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RegionName where
    parser = takeLowerText >>= \case
        "ap-northeast-1" -> pure ApNortheast1
        "ap-northeast-2" -> pure ApNortheast2
        "ap-south-1" -> pure ApSouth1
        "ap-southeast-1" -> pure ApSoutheast1
        "ap-southeast-2" -> pure ApSoutheast2
        "eu-central-1" -> pure EuCentral1
        "eu-west-1" -> pure EuWest1
        "us-east-1" -> pure UsEast1
        "us-east-2" -> pure UsEast2
        "us-west-1" -> pure UsWest1
        "us-west-2" -> pure UsWest2
        e -> fromTextError $ "Failure parsing RegionName from value: '" <> e
           <> "'. Accepted values: ap-northeast-1, ap-northeast-2, ap-south-1, ap-southeast-1, ap-southeast-2, eu-central-1, eu-west-1, us-east-1, us-east-2, us-west-1, us-west-2"

instance ToText RegionName where
    toText = \case
        ApNortheast1 -> "ap-northeast-1"
        ApNortheast2 -> "ap-northeast-2"
        ApSouth1 -> "ap-south-1"
        ApSoutheast1 -> "ap-southeast-1"
        ApSoutheast2 -> "ap-southeast-2"
        EuCentral1 -> "eu-central-1"
        EuWest1 -> "eu-west-1"
        UsEast1 -> "us-east-1"
        UsEast2 -> "us-east-2"
        UsWest1 -> "us-west-1"
        UsWest2 -> "us-west-2"

instance Hashable     RegionName
instance NFData       RegionName
instance ToByteString RegionName
instance ToQuery      RegionName
instance ToHeader     RegionName

instance FromJSON RegionName where
    parseJSON = parseJSONText "RegionName"

data ResourceType
  = Domain
  | Instance
  | InstanceSnapshot
  | KeyPair
  | PeeredVPC
  | StaticIP
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceType where
    parser = takeLowerText >>= \case
        "domain" -> pure Domain
        "instance" -> pure Instance
        "instancesnapshot" -> pure InstanceSnapshot
        "keypair" -> pure KeyPair
        "peeredvpc" -> pure PeeredVPC
        "staticip" -> pure StaticIP
        e -> fromTextError $ "Failure parsing ResourceType from value: '" <> e
           <> "'. Accepted values: domain, instance, instancesnapshot, keypair, peeredvpc, staticip"

instance ToText ResourceType where
    toText = \case
        Domain -> "Domain"
        Instance -> "Instance"
        InstanceSnapshot -> "InstanceSnapshot"
        KeyPair -> "KeyPair"
        PeeredVPC -> "PeeredVpc"
        StaticIP -> "StaticIp"

instance Hashable     ResourceType
instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"
