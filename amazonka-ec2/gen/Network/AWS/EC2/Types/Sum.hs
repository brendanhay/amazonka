{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Sum where

import           Network.AWS.Prelude

data AccountAttributeName
    = DefaultVPC
    | SupportedPlatforms
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText AccountAttributeName where
    parser = takeLowerText >>= \case
        "default-vpc" -> pure DefaultVPC
        "supported-platforms" -> pure SupportedPlatforms
        e -> fromTextError $ "Failure parsing AccountAttributeName from value: '" <> e
           <> "'. Accepted values: default-vpc, supported-platforms"

instance ToText AccountAttributeName where
    toText = \case
        DefaultVPC -> "default-vpc"
        SupportedPlatforms -> "supported-platforms"

instance Hashable     AccountAttributeName
instance ToByteString AccountAttributeName
instance ToQuery      AccountAttributeName
instance ToHeader     AccountAttributeName

data AddressStatus
    = InClassic
    | InVPC
    | MoveInProgress
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText AddressStatus where
    parser = takeLowerText >>= \case
        "inclassic" -> pure InClassic
        "invpc" -> pure InVPC
        "moveinprogress" -> pure MoveInProgress
        e -> fromTextError $ "Failure parsing AddressStatus from value: '" <> e
           <> "'. Accepted values: InClassic, InVpc, MoveInProgress"

instance ToText AddressStatus where
    toText = \case
        InClassic -> "InClassic"
        InVPC -> "InVpc"
        MoveInProgress -> "MoveInProgress"

instance Hashable     AddressStatus
instance ToByteString AddressStatus
instance ToQuery      AddressStatus
instance ToHeader     AddressStatus

instance FromXML AddressStatus where
    parseXML = parseXMLText "AddressStatus"

data ArchitectureValues
    = I386
    | X86_64
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ArchitectureValues where
    parser = takeLowerText >>= \case
        "i386" -> pure I386
        "x86_64" -> pure X86_64
        e -> fromTextError $ "Failure parsing ArchitectureValues from value: '" <> e
           <> "'. Accepted values: i386, x86_64"

instance ToText ArchitectureValues where
    toText = \case
        I386 -> "i386"
        X86_64 -> "x86_64"

instance Hashable     ArchitectureValues
instance ToByteString ArchitectureValues
instance ToQuery      ArchitectureValues
instance ToHeader     ArchitectureValues

instance FromXML ArchitectureValues where
    parseXML = parseXMLText "ArchitectureValues"

data AttachmentStatus
    = Attached
    | Attaching
    | Detached
    | Detaching
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText AttachmentStatus where
    parser = takeLowerText >>= \case
        "attached" -> pure Attached
        "attaching" -> pure Attaching
        "detached" -> pure Detached
        "detaching" -> pure Detaching
        e -> fromTextError $ "Failure parsing AttachmentStatus from value: '" <> e
           <> "'. Accepted values: attached, attaching, detached, detaching"

instance ToText AttachmentStatus where
    toText = \case
        Attached -> "attached"
        Attaching -> "attaching"
        Detached -> "detached"
        Detaching -> "detaching"

instance Hashable     AttachmentStatus
instance ToByteString AttachmentStatus
instance ToQuery      AttachmentStatus
instance ToHeader     AttachmentStatus

instance FromXML AttachmentStatus where
    parseXML = parseXMLText "AttachmentStatus"

data AvailabilityZoneState =
    AZSAvailable
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText AvailabilityZoneState where
    parser = takeLowerText >>= \case
        "available" -> pure AZSAvailable
        e -> fromTextError $ "Failure parsing AvailabilityZoneState from value: '" <> e
           <> "'. Accepted values: available"

instance ToText AvailabilityZoneState where
    toText = \case
        AZSAvailable -> "available"

instance Hashable     AvailabilityZoneState
instance ToByteString AvailabilityZoneState
instance ToQuery      AvailabilityZoneState
instance ToHeader     AvailabilityZoneState

instance FromXML AvailabilityZoneState where
    parseXML = parseXMLText "AvailabilityZoneState"

data BatchState
    = BSActive
    | BSCancelled
    | BSCancelledRunning
    | BSCancelledTerminating
    | BSFailed
    | BSSubmitted
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText BatchState where
    parser = takeLowerText >>= \case
        "active" -> pure BSActive
        "cancelled" -> pure BSCancelled
        "cancelled_running" -> pure BSCancelledRunning
        "cancelled_terminating" -> pure BSCancelledTerminating
        "failed" -> pure BSFailed
        "submitted" -> pure BSSubmitted
        e -> fromTextError $ "Failure parsing BatchState from value: '" <> e
           <> "'. Accepted values: active, cancelled, cancelled_running, cancelled_terminating, failed, submitted"

instance ToText BatchState where
    toText = \case
        BSActive -> "active"
        BSCancelled -> "cancelled"
        BSCancelledRunning -> "cancelled_running"
        BSCancelledTerminating -> "cancelled_terminating"
        BSFailed -> "failed"
        BSSubmitted -> "submitted"

instance Hashable     BatchState
instance ToByteString BatchState
instance ToQuery      BatchState
instance ToHeader     BatchState

instance FromXML BatchState where
    parseXML = parseXMLText "BatchState"

data BundleTaskState
    = BTSBundling
    | BTSCancelling
    | BTSComplete
    | BTSFailed
    | BTSPending
    | BTSStoring
    | BTSWaitingForShutdown
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText BundleTaskState where
    parser = takeLowerText >>= \case
        "bundling" -> pure BTSBundling
        "cancelling" -> pure BTSCancelling
        "complete" -> pure BTSComplete
        "failed" -> pure BTSFailed
        "pending" -> pure BTSPending
        "storing" -> pure BTSStoring
        "waiting-for-shutdown" -> pure BTSWaitingForShutdown
        e -> fromTextError $ "Failure parsing BundleTaskState from value: '" <> e
           <> "'. Accepted values: bundling, cancelling, complete, failed, pending, storing, waiting-for-shutdown"

instance ToText BundleTaskState where
    toText = \case
        BTSBundling -> "bundling"
        BTSCancelling -> "cancelling"
        BTSComplete -> "complete"
        BTSFailed -> "failed"
        BTSPending -> "pending"
        BTSStoring -> "storing"
        BTSWaitingForShutdown -> "waiting-for-shutdown"

instance Hashable     BundleTaskState
instance ToByteString BundleTaskState
instance ToQuery      BundleTaskState
instance ToHeader     BundleTaskState

instance FromXML BundleTaskState where
    parseXML = parseXMLText "BundleTaskState"

data CancelBatchErrorCode
    = FleetRequestIdDoesNotExist
    | FleetRequestIdMalformed
    | FleetRequestNotInCancellableState
    | UnexpectedError
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText CancelBatchErrorCode where
    parser = takeLowerText >>= \case
        "fleetrequestiddoesnotexist" -> pure FleetRequestIdDoesNotExist
        "fleetrequestidmalformed" -> pure FleetRequestIdMalformed
        "fleetrequestnotincancellablestate" -> pure FleetRequestNotInCancellableState
        "unexpectederror" -> pure UnexpectedError
        e -> fromTextError $ "Failure parsing CancelBatchErrorCode from value: '" <> e
           <> "'. Accepted values: fleetRequestIdDoesNotExist, fleetRequestIdMalformed, fleetRequestNotInCancellableState, unexpectedError"

instance ToText CancelBatchErrorCode where
    toText = \case
        FleetRequestIdDoesNotExist -> "fleetRequestIdDoesNotExist"
        FleetRequestIdMalformed -> "fleetRequestIdMalformed"
        FleetRequestNotInCancellableState -> "fleetRequestNotInCancellableState"
        UnexpectedError -> "unexpectedError"

instance Hashable     CancelBatchErrorCode
instance ToByteString CancelBatchErrorCode
instance ToQuery      CancelBatchErrorCode
instance ToHeader     CancelBatchErrorCode

instance FromXML CancelBatchErrorCode where
    parseXML = parseXMLText "CancelBatchErrorCode"

data CancelSpotInstanceRequestState
    = CSIRSActive
    | CSIRSCancelled
    | CSIRSClosed
    | CSIRSCompleted
    | CSIRSOpen
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText CancelSpotInstanceRequestState where
    parser = takeLowerText >>= \case
        "active" -> pure CSIRSActive
        "cancelled" -> pure CSIRSCancelled
        "closed" -> pure CSIRSClosed
        "completed" -> pure CSIRSCompleted
        "open" -> pure CSIRSOpen
        e -> fromTextError $ "Failure parsing CancelSpotInstanceRequestState from value: '" <> e
           <> "'. Accepted values: active, cancelled, closed, completed, open"

instance ToText CancelSpotInstanceRequestState where
    toText = \case
        CSIRSActive -> "active"
        CSIRSCancelled -> "cancelled"
        CSIRSClosed -> "closed"
        CSIRSCompleted -> "completed"
        CSIRSOpen -> "open"

instance Hashable     CancelSpotInstanceRequestState
instance ToByteString CancelSpotInstanceRequestState
instance ToQuery      CancelSpotInstanceRequestState
instance ToHeader     CancelSpotInstanceRequestState

instance FromXML CancelSpotInstanceRequestState where
    parseXML = parseXMLText "CancelSpotInstanceRequestState"

data ContainerFormat =
    Ova
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ContainerFormat where
    parser = takeLowerText >>= \case
        "ova" -> pure Ova
        e -> fromTextError $ "Failure parsing ContainerFormat from value: '" <> e
           <> "'. Accepted values: ova"

instance ToText ContainerFormat where
    toText = \case
        Ova -> "ova"

instance Hashable     ContainerFormat
instance ToByteString ContainerFormat
instance ToQuery      ContainerFormat
instance ToHeader     ContainerFormat

instance FromXML ContainerFormat where
    parseXML = parseXMLText "ContainerFormat"

data ConversionTaskState
    = CTSActive
    | CTSCancelled
    | CTSCancelling
    | CTSCompleted
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ConversionTaskState where
    parser = takeLowerText >>= \case
        "active" -> pure CTSActive
        "cancelled" -> pure CTSCancelled
        "cancelling" -> pure CTSCancelling
        "completed" -> pure CTSCompleted
        e -> fromTextError $ "Failure parsing ConversionTaskState from value: '" <> e
           <> "'. Accepted values: active, cancelled, cancelling, completed"

instance ToText ConversionTaskState where
    toText = \case
        CTSActive -> "active"
        CTSCancelled -> "cancelled"
        CTSCancelling -> "cancelling"
        CTSCompleted -> "completed"

instance Hashable     ConversionTaskState
instance ToByteString ConversionTaskState
instance ToQuery      ConversionTaskState
instance ToHeader     ConversionTaskState

instance FromXML ConversionTaskState where
    parseXML = parseXMLText "ConversionTaskState"

data CurrencyCodeValues =
    Usd
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText CurrencyCodeValues where
    parser = takeLowerText >>= \case
        "usd" -> pure Usd
        e -> fromTextError $ "Failure parsing CurrencyCodeValues from value: '" <> e
           <> "'. Accepted values: USD"

instance ToText CurrencyCodeValues where
    toText = \case
        Usd -> "USD"

instance Hashable     CurrencyCodeValues
instance ToByteString CurrencyCodeValues
instance ToQuery      CurrencyCodeValues
instance ToHeader     CurrencyCodeValues

instance FromXML CurrencyCodeValues where
    parseXML = parseXMLText "CurrencyCodeValues"

data DatafeedSubscriptionState
    = DSSActive
    | DSSInactive
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DatafeedSubscriptionState where
    parser = takeLowerText >>= \case
        "active" -> pure DSSActive
        "inactive" -> pure DSSInactive
        e -> fromTextError $ "Failure parsing DatafeedSubscriptionState from value: '" <> e
           <> "'. Accepted values: Active, Inactive"

instance ToText DatafeedSubscriptionState where
    toText = \case
        DSSActive -> "Active"
        DSSInactive -> "Inactive"

instance Hashable     DatafeedSubscriptionState
instance ToByteString DatafeedSubscriptionState
instance ToQuery      DatafeedSubscriptionState
instance ToHeader     DatafeedSubscriptionState

instance FromXML DatafeedSubscriptionState where
    parseXML = parseXMLText "DatafeedSubscriptionState"

data DeviceType
    = EBS
    | InstanceStore
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DeviceType where
    parser = takeLowerText >>= \case
        "ebs" -> pure EBS
        "instance-store" -> pure InstanceStore
        e -> fromTextError $ "Failure parsing DeviceType from value: '" <> e
           <> "'. Accepted values: ebs, instance-store"

instance ToText DeviceType where
    toText = \case
        EBS -> "ebs"
        InstanceStore -> "instance-store"

instance Hashable     DeviceType
instance ToByteString DeviceType
instance ToQuery      DeviceType
instance ToHeader     DeviceType

instance FromXML DeviceType where
    parseXML = parseXMLText "DeviceType"

data DiskImageFormat
    = Raw
    | VHD
    | VMDK
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DiskImageFormat where
    parser = takeLowerText >>= \case
        "raw" -> pure Raw
        "vhd" -> pure VHD
        "vmdk" -> pure VMDK
        e -> fromTextError $ "Failure parsing DiskImageFormat from value: '" <> e
           <> "'. Accepted values: RAW, VHD, VMDK"

instance ToText DiskImageFormat where
    toText = \case
        Raw -> "RAW"
        VHD -> "VHD"
        VMDK -> "VMDK"

instance Hashable     DiskImageFormat
instance ToByteString DiskImageFormat
instance ToQuery      DiskImageFormat
instance ToHeader     DiskImageFormat

instance FromXML DiskImageFormat where
    parseXML = parseXMLText "DiskImageFormat"

data DomainType
    = DTStandard
    | DTVPC
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DomainType where
    parser = takeLowerText >>= \case
        "standard" -> pure DTStandard
        "vpc" -> pure DTVPC
        e -> fromTextError $ "Failure parsing DomainType from value: '" <> e
           <> "'. Accepted values: standard, vpc"

instance ToText DomainType where
    toText = \case
        DTStandard -> "standard"
        DTVPC -> "vpc"

instance Hashable     DomainType
instance ToByteString DomainType
instance ToQuery      DomainType
instance ToHeader     DomainType

instance FromXML DomainType where
    parseXML = parseXMLText "DomainType"

data EventCode
    = InstanceReboot
    | InstanceRetirement
    | InstanceStop
    | SystemMaintenance
    | SystemReboot
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText EventCode where
    parser = takeLowerText >>= \case
        "instance-reboot" -> pure InstanceReboot
        "instance-retirement" -> pure InstanceRetirement
        "instance-stop" -> pure InstanceStop
        "system-maintenance" -> pure SystemMaintenance
        "system-reboot" -> pure SystemReboot
        e -> fromTextError $ "Failure parsing EventCode from value: '" <> e
           <> "'. Accepted values: instance-reboot, instance-retirement, instance-stop, system-maintenance, system-reboot"

instance ToText EventCode where
    toText = \case
        InstanceReboot -> "instance-reboot"
        InstanceRetirement -> "instance-retirement"
        InstanceStop -> "instance-stop"
        SystemMaintenance -> "system-maintenance"
        SystemReboot -> "system-reboot"

instance Hashable     EventCode
instance ToByteString EventCode
instance ToQuery      EventCode
instance ToHeader     EventCode

instance FromXML EventCode where
    parseXML = parseXMLText "EventCode"

data EventType
    = Error'
    | FleetRequestChange
    | InstanceChange
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText EventType where
    parser = takeLowerText >>= \case
        "error" -> pure Error'
        "fleetrequestchange" -> pure FleetRequestChange
        "instancechange" -> pure InstanceChange
        e -> fromTextError $ "Failure parsing EventType from value: '" <> e
           <> "'. Accepted values: error, fleetRequestChange, instanceChange"

instance ToText EventType where
    toText = \case
        Error' -> "error"
        FleetRequestChange -> "fleetRequestChange"
        InstanceChange -> "instanceChange"

instance Hashable     EventType
instance ToByteString EventType
instance ToQuery      EventType
instance ToHeader     EventType

instance FromXML EventType where
    parseXML = parseXMLText "EventType"

data ExportEnvironment
    = Citrix
    | Microsoft
    | VMware
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ExportEnvironment where
    parser = takeLowerText >>= \case
        "citrix" -> pure Citrix
        "microsoft" -> pure Microsoft
        "vmware" -> pure VMware
        e -> fromTextError $ "Failure parsing ExportEnvironment from value: '" <> e
           <> "'. Accepted values: citrix, microsoft, vmware"

instance ToText ExportEnvironment where
    toText = \case
        Citrix -> "citrix"
        Microsoft -> "microsoft"
        VMware -> "vmware"

instance Hashable     ExportEnvironment
instance ToByteString ExportEnvironment
instance ToQuery      ExportEnvironment
instance ToHeader     ExportEnvironment

instance FromXML ExportEnvironment where
    parseXML = parseXMLText "ExportEnvironment"

data ExportTaskState
    = ETSActive
    | ETSCancelled
    | ETSCancelling
    | ETSCompleted
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ExportTaskState where
    parser = takeLowerText >>= \case
        "active" -> pure ETSActive
        "cancelled" -> pure ETSCancelled
        "cancelling" -> pure ETSCancelling
        "completed" -> pure ETSCompleted
        e -> fromTextError $ "Failure parsing ExportTaskState from value: '" <> e
           <> "'. Accepted values: active, cancelled, cancelling, completed"

instance ToText ExportTaskState where
    toText = \case
        ETSActive -> "active"
        ETSCancelled -> "cancelled"
        ETSCancelling -> "cancelling"
        ETSCompleted -> "completed"

instance Hashable     ExportTaskState
instance ToByteString ExportTaskState
instance ToQuery      ExportTaskState
instance ToHeader     ExportTaskState

instance FromXML ExportTaskState where
    parseXML = parseXMLText "ExportTaskState"

data FlowLogsResourceType
    = FLRTNetworkInterface
    | FLRTSubnet
    | FLRTVPC
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText FlowLogsResourceType where
    parser = takeLowerText >>= \case
        "networkinterface" -> pure FLRTNetworkInterface
        "subnet" -> pure FLRTSubnet
        "vpc" -> pure FLRTVPC
        e -> fromTextError $ "Failure parsing FlowLogsResourceType from value: '" <> e
           <> "'. Accepted values: NetworkInterface, Subnet, VPC"

instance ToText FlowLogsResourceType where
    toText = \case
        FLRTNetworkInterface -> "NetworkInterface"
        FLRTSubnet -> "Subnet"
        FLRTVPC -> "VPC"

instance Hashable     FlowLogsResourceType
instance ToByteString FlowLogsResourceType
instance ToQuery      FlowLogsResourceType
instance ToHeader     FlowLogsResourceType

data GatewayType =
    IPsec_1
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText GatewayType where
    parser = takeLowerText >>= \case
        "ipsec.1" -> pure IPsec_1
        e -> fromTextError $ "Failure parsing GatewayType from value: '" <> e
           <> "'. Accepted values: ipsec.1"

instance ToText GatewayType where
    toText = \case
        IPsec_1 -> "ipsec.1"

instance Hashable     GatewayType
instance ToByteString GatewayType
instance ToQuery      GatewayType
instance ToHeader     GatewayType

instance FromXML GatewayType where
    parseXML = parseXMLText "GatewayType"

data HypervisorType
    = Ovm
    | Xen
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText HypervisorType where
    parser = takeLowerText >>= \case
        "ovm" -> pure Ovm
        "xen" -> pure Xen
        e -> fromTextError $ "Failure parsing HypervisorType from value: '" <> e
           <> "'. Accepted values: ovm, xen"

instance ToText HypervisorType where
    toText = \case
        Ovm -> "ovm"
        Xen -> "xen"

instance Hashable     HypervisorType
instance ToByteString HypervisorType
instance ToQuery      HypervisorType
instance ToHeader     HypervisorType

instance FromXML HypervisorType where
    parseXML = parseXMLText "HypervisorType"

data ImageAttributeName
    = BlockDeviceMapping
    | Description
    | Kernel
    | LaunchPermission
    | ProductCodes
    | RAMDisk
    | SRIOVNetSupport
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ImageAttributeName where
    parser = takeLowerText >>= \case
        "blockdevicemapping" -> pure BlockDeviceMapping
        "description" -> pure Description
        "kernel" -> pure Kernel
        "launchpermission" -> pure LaunchPermission
        "productcodes" -> pure ProductCodes
        "ramdisk" -> pure RAMDisk
        "sriovnetsupport" -> pure SRIOVNetSupport
        e -> fromTextError $ "Failure parsing ImageAttributeName from value: '" <> e
           <> "'. Accepted values: blockDeviceMapping, description, kernel, launchPermission, productCodes, ramdisk, sriovNetSupport"

instance ToText ImageAttributeName where
    toText = \case
        BlockDeviceMapping -> "blockDeviceMapping"
        Description -> "description"
        Kernel -> "kernel"
        LaunchPermission -> "launchPermission"
        ProductCodes -> "productCodes"
        RAMDisk -> "ramdisk"
        SRIOVNetSupport -> "sriovNetSupport"

instance Hashable     ImageAttributeName
instance ToByteString ImageAttributeName
instance ToQuery      ImageAttributeName
instance ToHeader     ImageAttributeName

data ImageState
    = ISAvailable
    | ISDeregistered
    | ISError'
    | ISFailed
    | ISInvalid
    | ISPending
    | ISTransient
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ImageState where
    parser = takeLowerText >>= \case
        "available" -> pure ISAvailable
        "deregistered" -> pure ISDeregistered
        "error" -> pure ISError'
        "failed" -> pure ISFailed
        "invalid" -> pure ISInvalid
        "pending" -> pure ISPending
        "transient" -> pure ISTransient
        e -> fromTextError $ "Failure parsing ImageState from value: '" <> e
           <> "'. Accepted values: available, deregistered, error, failed, invalid, pending, transient"

instance ToText ImageState where
    toText = \case
        ISAvailable -> "available"
        ISDeregistered -> "deregistered"
        ISError' -> "error"
        ISFailed -> "failed"
        ISInvalid -> "invalid"
        ISPending -> "pending"
        ISTransient -> "transient"

instance Hashable     ImageState
instance ToByteString ImageState
instance ToQuery      ImageState
instance ToHeader     ImageState

instance FromXML ImageState where
    parseXML = parseXMLText "ImageState"

data ImageTypeValues
    = ITVKernel
    | ITVMachine
    | ITVRAMDisk
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ImageTypeValues where
    parser = takeLowerText >>= \case
        "kernel" -> pure ITVKernel
        "machine" -> pure ITVMachine
        "ramdisk" -> pure ITVRAMDisk
        e -> fromTextError $ "Failure parsing ImageTypeValues from value: '" <> e
           <> "'. Accepted values: kernel, machine, ramdisk"

instance ToText ImageTypeValues where
    toText = \case
        ITVKernel -> "kernel"
        ITVMachine -> "machine"
        ITVRAMDisk -> "ramdisk"

instance Hashable     ImageTypeValues
instance ToByteString ImageTypeValues
instance ToQuery      ImageTypeValues
instance ToHeader     ImageTypeValues

instance FromXML ImageTypeValues where
    parseXML = parseXMLText "ImageTypeValues"

data InstanceAttributeName
    = IANBlockDeviceMapping
    | IANDisableAPITermination
    | IANEBSOptimized
    | IANGroupSet
    | IANInstanceInitiatedShutdownBehavior
    | IANInstanceType
    | IANKernel
    | IANProductCodes
    | IANRAMDisk
    | IANRootDeviceName
    | IANSRIOVNetSupport
    | IANSourceDestCheck
    | IANUserData
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText InstanceAttributeName where
    parser = takeLowerText >>= \case
        "blockdevicemapping" -> pure IANBlockDeviceMapping
        "disableapitermination" -> pure IANDisableAPITermination
        "ebsoptimized" -> pure IANEBSOptimized
        "groupset" -> pure IANGroupSet
        "instanceinitiatedshutdownbehavior" -> pure IANInstanceInitiatedShutdownBehavior
        "instancetype" -> pure IANInstanceType
        "kernel" -> pure IANKernel
        "productcodes" -> pure IANProductCodes
        "ramdisk" -> pure IANRAMDisk
        "rootdevicename" -> pure IANRootDeviceName
        "sriovnetsupport" -> pure IANSRIOVNetSupport
        "sourcedestcheck" -> pure IANSourceDestCheck
        "userdata" -> pure IANUserData
        e -> fromTextError $ "Failure parsing InstanceAttributeName from value: '" <> e
           <> "'. Accepted values: blockDeviceMapping, disableApiTermination, ebsOptimized, groupSet, instanceInitiatedShutdownBehavior, instanceType, kernel, productCodes, ramdisk, rootDeviceName, sriovNetSupport, sourceDestCheck, userData"

instance ToText InstanceAttributeName where
    toText = \case
        IANBlockDeviceMapping -> "blockDeviceMapping"
        IANDisableAPITermination -> "disableApiTermination"
        IANEBSOptimized -> "ebsOptimized"
        IANGroupSet -> "groupSet"
        IANInstanceInitiatedShutdownBehavior -> "instanceInitiatedShutdownBehavior"
        IANInstanceType -> "instanceType"
        IANKernel -> "kernel"
        IANProductCodes -> "productCodes"
        IANRAMDisk -> "ramdisk"
        IANRootDeviceName -> "rootDeviceName"
        IANSRIOVNetSupport -> "sriovNetSupport"
        IANSourceDestCheck -> "sourceDestCheck"
        IANUserData -> "userData"

instance Hashable     InstanceAttributeName
instance ToByteString InstanceAttributeName
instance ToQuery      InstanceAttributeName
instance ToHeader     InstanceAttributeName

data InstanceLifecycleType =
    Spot
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText InstanceLifecycleType where
    parser = takeLowerText >>= \case
        "spot" -> pure Spot
        e -> fromTextError $ "Failure parsing InstanceLifecycleType from value: '" <> e
           <> "'. Accepted values: spot"

instance ToText InstanceLifecycleType where
    toText = \case
        Spot -> "spot"

instance Hashable     InstanceLifecycleType
instance ToByteString InstanceLifecycleType
instance ToQuery      InstanceLifecycleType
instance ToHeader     InstanceLifecycleType

instance FromXML InstanceLifecycleType where
    parseXML = parseXMLText "InstanceLifecycleType"

data InstanceStateName
    = ISNPending
    | ISNRunning
    | ISNShuttingDown
    | ISNStopped
    | ISNStopping
    | ISNTerminated
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText InstanceStateName where
    parser = takeLowerText >>= \case
        "pending" -> pure ISNPending
        "running" -> pure ISNRunning
        "shutting-down" -> pure ISNShuttingDown
        "stopped" -> pure ISNStopped
        "stopping" -> pure ISNStopping
        "terminated" -> pure ISNTerminated
        e -> fromTextError $ "Failure parsing InstanceStateName from value: '" <> e
           <> "'. Accepted values: pending, running, shutting-down, stopped, stopping, terminated"

instance ToText InstanceStateName where
    toText = \case
        ISNPending -> "pending"
        ISNRunning -> "running"
        ISNShuttingDown -> "shutting-down"
        ISNStopped -> "stopped"
        ISNStopping -> "stopping"
        ISNTerminated -> "terminated"

instance Hashable     InstanceStateName
instance ToByteString InstanceStateName
instance ToQuery      InstanceStateName
instance ToHeader     InstanceStateName

instance FromXML InstanceStateName where
    parseXML = parseXMLText "InstanceStateName"

data InstanceType
    = C1_Medium
    | C1_XLarge
    | C3_2XLarge
    | C3_4XLarge
    | C3_8XLarge
    | C3_Large
    | C3_XLarge
    | C4_2XLarge
    | C4_4XLarge
    | C4_8XLarge
    | C4_Large
    | C4_XLarge
    | CC1_4XLarge
    | CC2_8XLarge
    | CG1_4XLarge
    | CR1_8XLarge
    | D2_2XLarge
    | D2_4XLarge
    | D2_8XLarge
    | D2_XLarge
    | G2_2XLarge
    | HI1_4XLarge
    | HS1_8XLarge
    | I2_2XLarge
    | I2_4XLarge
    | I2_8XLarge
    | I2_XLarge
    | M1_Large
    | M1_Medium
    | M1_Small
    | M1_XLarge
    | M2_2XLarge
    | M2_4XLarge
    | M2_XLarge
    | M3_2XLarge
    | M3_Large
    | M3_Medium
    | M3_XLarge
    | M4_10XLarge
    | M4_2XLarge
    | M4_4XLarge
    | M4_Large
    | M4_XLarge
    | R3_2XLarge
    | R3_4XLarge
    | R3_8XLarge
    | R3_Large
    | R3_XLarge
    | T1_Micro
    | T2_Large
    | T2_Medium
    | T2_Micro
    | T2_Small
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText InstanceType where
    parser = takeLowerText >>= \case
        "c1.medium" -> pure C1_Medium
        "c1.xlarge" -> pure C1_XLarge
        "c3.2xlarge" -> pure C3_2XLarge
        "c3.4xlarge" -> pure C3_4XLarge
        "c3.8xlarge" -> pure C3_8XLarge
        "c3.large" -> pure C3_Large
        "c3.xlarge" -> pure C3_XLarge
        "c4.2xlarge" -> pure C4_2XLarge
        "c4.4xlarge" -> pure C4_4XLarge
        "c4.8xlarge" -> pure C4_8XLarge
        "c4.large" -> pure C4_Large
        "c4.xlarge" -> pure C4_XLarge
        "cc1.4xlarge" -> pure CC1_4XLarge
        "cc2.8xlarge" -> pure CC2_8XLarge
        "cg1.4xlarge" -> pure CG1_4XLarge
        "cr1.8xlarge" -> pure CR1_8XLarge
        "d2.2xlarge" -> pure D2_2XLarge
        "d2.4xlarge" -> pure D2_4XLarge
        "d2.8xlarge" -> pure D2_8XLarge
        "d2.xlarge" -> pure D2_XLarge
        "g2.2xlarge" -> pure G2_2XLarge
        "hi1.4xlarge" -> pure HI1_4XLarge
        "hs1.8xlarge" -> pure HS1_8XLarge
        "i2.2xlarge" -> pure I2_2XLarge
        "i2.4xlarge" -> pure I2_4XLarge
        "i2.8xlarge" -> pure I2_8XLarge
        "i2.xlarge" -> pure I2_XLarge
        "m1.large" -> pure M1_Large
        "m1.medium" -> pure M1_Medium
        "m1.small" -> pure M1_Small
        "m1.xlarge" -> pure M1_XLarge
        "m2.2xlarge" -> pure M2_2XLarge
        "m2.4xlarge" -> pure M2_4XLarge
        "m2.xlarge" -> pure M2_XLarge
        "m3.2xlarge" -> pure M3_2XLarge
        "m3.large" -> pure M3_Large
        "m3.medium" -> pure M3_Medium
        "m3.xlarge" -> pure M3_XLarge
        "m4.10xlarge" -> pure M4_10XLarge
        "m4.2xlarge" -> pure M4_2XLarge
        "m4.4xlarge" -> pure M4_4XLarge
        "m4.large" -> pure M4_Large
        "m4.xlarge" -> pure M4_XLarge
        "r3.2xlarge" -> pure R3_2XLarge
        "r3.4xlarge" -> pure R3_4XLarge
        "r3.8xlarge" -> pure R3_8XLarge
        "r3.large" -> pure R3_Large
        "r3.xlarge" -> pure R3_XLarge
        "t1.micro" -> pure T1_Micro
        "t2.large" -> pure T2_Large
        "t2.medium" -> pure T2_Medium
        "t2.micro" -> pure T2_Micro
        "t2.small" -> pure T2_Small
        e -> fromTextError $ "Failure parsing InstanceType from value: '" <> e
           <> "'. Accepted values: c1.medium, c1.xlarge, c3.2xlarge, c3.4xlarge, c3.8xlarge, c3.large, c3.xlarge, c4.2xlarge, c4.4xlarge, c4.8xlarge, c4.large, c4.xlarge, cc1.4xlarge, cc2.8xlarge, cg1.4xlarge, cr1.8xlarge, d2.2xlarge, d2.4xlarge, d2.8xlarge, d2.xlarge, g2.2xlarge, hi1.4xlarge, hs1.8xlarge, i2.2xlarge, i2.4xlarge, i2.8xlarge, i2.xlarge, m1.large, m1.medium, m1.small, m1.xlarge, m2.2xlarge, m2.4xlarge, m2.xlarge, m3.2xlarge, m3.large, m3.medium, m3.xlarge, m4.10xlarge, m4.2xlarge, m4.4xlarge, m4.large, m4.xlarge, r3.2xlarge, r3.4xlarge, r3.8xlarge, r3.large, r3.xlarge, t1.micro, t2.large, t2.medium, t2.micro, t2.small"

instance ToText InstanceType where
    toText = \case
        C1_Medium -> "c1.medium"
        C1_XLarge -> "c1.xlarge"
        C3_2XLarge -> "c3.2xlarge"
        C3_4XLarge -> "c3.4xlarge"
        C3_8XLarge -> "c3.8xlarge"
        C3_Large -> "c3.large"
        C3_XLarge -> "c3.xlarge"
        C4_2XLarge -> "c4.2xlarge"
        C4_4XLarge -> "c4.4xlarge"
        C4_8XLarge -> "c4.8xlarge"
        C4_Large -> "c4.large"
        C4_XLarge -> "c4.xlarge"
        CC1_4XLarge -> "cc1.4xlarge"
        CC2_8XLarge -> "cc2.8xlarge"
        CG1_4XLarge -> "cg1.4xlarge"
        CR1_8XLarge -> "cr1.8xlarge"
        D2_2XLarge -> "d2.2xlarge"
        D2_4XLarge -> "d2.4xlarge"
        D2_8XLarge -> "d2.8xlarge"
        D2_XLarge -> "d2.xlarge"
        G2_2XLarge -> "g2.2xlarge"
        HI1_4XLarge -> "hi1.4xlarge"
        HS1_8XLarge -> "hs1.8xlarge"
        I2_2XLarge -> "i2.2xlarge"
        I2_4XLarge -> "i2.4xlarge"
        I2_8XLarge -> "i2.8xlarge"
        I2_XLarge -> "i2.xlarge"
        M1_Large -> "m1.large"
        M1_Medium -> "m1.medium"
        M1_Small -> "m1.small"
        M1_XLarge -> "m1.xlarge"
        M2_2XLarge -> "m2.2xlarge"
        M2_4XLarge -> "m2.4xlarge"
        M2_XLarge -> "m2.xlarge"
        M3_2XLarge -> "m3.2xlarge"
        M3_Large -> "m3.large"
        M3_Medium -> "m3.medium"
        M3_XLarge -> "m3.xlarge"
        M4_10XLarge -> "m4.10xlarge"
        M4_2XLarge -> "m4.2xlarge"
        M4_4XLarge -> "m4.4xlarge"
        M4_Large -> "m4.large"
        M4_XLarge -> "m4.xlarge"
        R3_2XLarge -> "r3.2xlarge"
        R3_4XLarge -> "r3.4xlarge"
        R3_8XLarge -> "r3.8xlarge"
        R3_Large -> "r3.large"
        R3_XLarge -> "r3.xlarge"
        T1_Micro -> "t1.micro"
        T2_Large -> "t2.large"
        T2_Medium -> "t2.medium"
        T2_Micro -> "t2.micro"
        T2_Small -> "t2.small"

instance Hashable     InstanceType
instance ToByteString InstanceType
instance ToQuery      InstanceType
instance ToHeader     InstanceType

instance FromXML InstanceType where
    parseXML = parseXMLText "InstanceType"

data ListingState
    = LAvailable
    | LCancelled
    | LPending
    | LSold
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ListingState where
    parser = takeLowerText >>= \case
        "available" -> pure LAvailable
        "cancelled" -> pure LCancelled
        "pending" -> pure LPending
        "sold" -> pure LSold
        e -> fromTextError $ "Failure parsing ListingState from value: '" <> e
           <> "'. Accepted values: available, cancelled, pending, sold"

instance ToText ListingState where
    toText = \case
        LAvailable -> "available"
        LCancelled -> "cancelled"
        LPending -> "pending"
        LSold -> "sold"

instance Hashable     ListingState
instance ToByteString ListingState
instance ToQuery      ListingState
instance ToHeader     ListingState

instance FromXML ListingState where
    parseXML = parseXMLText "ListingState"

data ListingStatus
    = LSActive
    | LSCancelled
    | LSClosed
    | LSPending
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ListingStatus where
    parser = takeLowerText >>= \case
        "active" -> pure LSActive
        "cancelled" -> pure LSCancelled
        "closed" -> pure LSClosed
        "pending" -> pure LSPending
        e -> fromTextError $ "Failure parsing ListingStatus from value: '" <> e
           <> "'. Accepted values: active, cancelled, closed, pending"

instance ToText ListingStatus where
    toText = \case
        LSActive -> "active"
        LSCancelled -> "cancelled"
        LSClosed -> "closed"
        LSPending -> "pending"

instance Hashable     ListingStatus
instance ToByteString ListingStatus
instance ToQuery      ListingStatus
instance ToHeader     ListingStatus

instance FromXML ListingStatus where
    parseXML = parseXMLText "ListingStatus"

data MonitoringState
    = MSDisabled
    | MSDisabling
    | MSEnabled
    | MSPending
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText MonitoringState where
    parser = takeLowerText >>= \case
        "disabled" -> pure MSDisabled
        "disabling" -> pure MSDisabling
        "enabled" -> pure MSEnabled
        "pending" -> pure MSPending
        e -> fromTextError $ "Failure parsing MonitoringState from value: '" <> e
           <> "'. Accepted values: disabled, disabling, enabled, pending"

instance ToText MonitoringState where
    toText = \case
        MSDisabled -> "disabled"
        MSDisabling -> "disabling"
        MSEnabled -> "enabled"
        MSPending -> "pending"

instance Hashable     MonitoringState
instance ToByteString MonitoringState
instance ToQuery      MonitoringState
instance ToHeader     MonitoringState

instance FromXML MonitoringState where
    parseXML = parseXMLText "MonitoringState"

data MoveStatus
    = MovingToVPC
    | RestoringToClassic
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText MoveStatus where
    parser = takeLowerText >>= \case
        "movingtovpc" -> pure MovingToVPC
        "restoringtoclassic" -> pure RestoringToClassic
        e -> fromTextError $ "Failure parsing MoveStatus from value: '" <> e
           <> "'. Accepted values: movingToVpc, restoringToClassic"

instance ToText MoveStatus where
    toText = \case
        MovingToVPC -> "movingToVpc"
        RestoringToClassic -> "restoringToClassic"

instance Hashable     MoveStatus
instance ToByteString MoveStatus
instance ToQuery      MoveStatus
instance ToHeader     MoveStatus

instance FromXML MoveStatus where
    parseXML = parseXMLText "MoveStatus"

data NetworkInterfaceAttribute
    = NIAAttachment
    | NIADescription
    | NIAGroupSet
    | NIASourceDestCheck
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText NetworkInterfaceAttribute where
    parser = takeLowerText >>= \case
        "attachment" -> pure NIAAttachment
        "description" -> pure NIADescription
        "groupset" -> pure NIAGroupSet
        "sourcedestcheck" -> pure NIASourceDestCheck
        e -> fromTextError $ "Failure parsing NetworkInterfaceAttribute from value: '" <> e
           <> "'. Accepted values: attachment, description, groupSet, sourceDestCheck"

instance ToText NetworkInterfaceAttribute where
    toText = \case
        NIAAttachment -> "attachment"
        NIADescription -> "description"
        NIAGroupSet -> "groupSet"
        NIASourceDestCheck -> "sourceDestCheck"

instance Hashable     NetworkInterfaceAttribute
instance ToByteString NetworkInterfaceAttribute
instance ToQuery      NetworkInterfaceAttribute
instance ToHeader     NetworkInterfaceAttribute

data NetworkInterfaceStatus
    = NISAttaching
    | NISAvailable
    | NISDetaching
    | NISInUse
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText NetworkInterfaceStatus where
    parser = takeLowerText >>= \case
        "attaching" -> pure NISAttaching
        "available" -> pure NISAvailable
        "detaching" -> pure NISDetaching
        "in-use" -> pure NISInUse
        e -> fromTextError $ "Failure parsing NetworkInterfaceStatus from value: '" <> e
           <> "'. Accepted values: attaching, available, detaching, in-use"

instance ToText NetworkInterfaceStatus where
    toText = \case
        NISAttaching -> "attaching"
        NISAvailable -> "available"
        NISDetaching -> "detaching"
        NISInUse -> "in-use"

instance Hashable     NetworkInterfaceStatus
instance ToByteString NetworkInterfaceStatus
instance ToQuery      NetworkInterfaceStatus
instance ToHeader     NetworkInterfaceStatus

instance FromXML NetworkInterfaceStatus where
    parseXML = parseXMLText "NetworkInterfaceStatus"

data OfferingTypeValues
    = AllUpfront
    | HeavyUtilization
    | LightUtilization
    | MediumUtilization
    | NoUpfront
    | PartialUpfront
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText OfferingTypeValues where
    parser = takeLowerText >>= \case
        "all upfront" -> pure AllUpfront
        "heavy utilization" -> pure HeavyUtilization
        "light utilization" -> pure LightUtilization
        "medium utilization" -> pure MediumUtilization
        "no upfront" -> pure NoUpfront
        "partial upfront" -> pure PartialUpfront
        e -> fromTextError $ "Failure parsing OfferingTypeValues from value: '" <> e
           <> "'. Accepted values: All Upfront, Heavy Utilization, Light Utilization, Medium Utilization, No Upfront, Partial Upfront"

instance ToText OfferingTypeValues where
    toText = \case
        AllUpfront -> "All Upfront"
        HeavyUtilization -> "Heavy Utilization"
        LightUtilization -> "Light Utilization"
        MediumUtilization -> "Medium Utilization"
        NoUpfront -> "No Upfront"
        PartialUpfront -> "Partial Upfront"

instance Hashable     OfferingTypeValues
instance ToByteString OfferingTypeValues
instance ToQuery      OfferingTypeValues
instance ToHeader     OfferingTypeValues

instance FromXML OfferingTypeValues where
    parseXML = parseXMLText "OfferingTypeValues"

data OperationType
    = Add
    | Remove
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText OperationType where
    parser = takeLowerText >>= \case
        "add" -> pure Add
        "remove" -> pure Remove
        e -> fromTextError $ "Failure parsing OperationType from value: '" <> e
           <> "'. Accepted values: add, remove"

instance ToText OperationType where
    toText = \case
        Add -> "add"
        Remove -> "remove"

instance Hashable     OperationType
instance ToByteString OperationType
instance ToQuery      OperationType
instance ToHeader     OperationType

data PermissionGroup =
    All
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText PermissionGroup where
    parser = takeLowerText >>= \case
        "all" -> pure All
        e -> fromTextError $ "Failure parsing PermissionGroup from value: '" <> e
           <> "'. Accepted values: all"

instance ToText PermissionGroup where
    toText = \case
        All -> "all"

instance Hashable     PermissionGroup
instance ToByteString PermissionGroup
instance ToQuery      PermissionGroup
instance ToHeader     PermissionGroup

instance FromXML PermissionGroup where
    parseXML = parseXMLText "PermissionGroup"

data PlacementGroupState
    = Available
    | Deleted
    | Deleting
    | Pending
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText PlacementGroupState where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "deleted" -> pure Deleted
        "deleting" -> pure Deleting
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing PlacementGroupState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText PlacementGroupState where
    toText = \case
        Available -> "available"
        Deleted -> "deleted"
        Deleting -> "deleting"
        Pending -> "pending"

instance Hashable     PlacementGroupState
instance ToByteString PlacementGroupState
instance ToQuery      PlacementGroupState
instance ToHeader     PlacementGroupState

instance FromXML PlacementGroupState where
    parseXML = parseXMLText "PlacementGroupState"

data PlacementStrategy =
    Cluster
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText PlacementStrategy where
    parser = takeLowerText >>= \case
        "cluster" -> pure Cluster
        e -> fromTextError $ "Failure parsing PlacementStrategy from value: '" <> e
           <> "'. Accepted values: cluster"

instance ToText PlacementStrategy where
    toText = \case
        Cluster -> "cluster"

instance Hashable     PlacementStrategy
instance ToByteString PlacementStrategy
instance ToQuery      PlacementStrategy
instance ToHeader     PlacementStrategy

instance FromXML PlacementStrategy where
    parseXML = parseXMLText "PlacementStrategy"

data PlatformValues =
    Windows
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText PlatformValues where
    parser = takeLowerText >>= \case
        "windows" -> pure Windows
        e -> fromTextError $ "Failure parsing PlatformValues from value: '" <> e
           <> "'. Accepted values: Windows"

instance ToText PlatformValues where
    toText = \case
        Windows -> "Windows"

instance Hashable     PlatformValues
instance ToByteString PlatformValues
instance ToQuery      PlatformValues
instance ToHeader     PlatformValues

instance FromXML PlatformValues where
    parseXML = parseXMLText "PlatformValues"

data ProductCodeValues
    = Devpay
    | Marketplace
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ProductCodeValues where
    parser = takeLowerText >>= \case
        "devpay" -> pure Devpay
        "marketplace" -> pure Marketplace
        e -> fromTextError $ "Failure parsing ProductCodeValues from value: '" <> e
           <> "'. Accepted values: devpay, marketplace"

instance ToText ProductCodeValues where
    toText = \case
        Devpay -> "devpay"
        Marketplace -> "marketplace"

instance Hashable     ProductCodeValues
instance ToByteString ProductCodeValues
instance ToQuery      ProductCodeValues
instance ToHeader     ProductCodeValues

instance FromXML ProductCodeValues where
    parseXML = parseXMLText "ProductCodeValues"

data RIProductDescription
    = RIDLinuxUnix
    | RIDLinuxUnixAmazonVPC
    | RIDWindows
    | RIDWindowsAmazonVPC
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RIProductDescription where
    parser = takeLowerText >>= \case
        "linux/unix" -> pure RIDLinuxUnix
        "linux/unix (amazon vpc)" -> pure RIDLinuxUnixAmazonVPC
        "windows" -> pure RIDWindows
        "windows (amazon vpc)" -> pure RIDWindowsAmazonVPC
        e -> fromTextError $ "Failure parsing RIProductDescription from value: '" <> e
           <> "'. Accepted values: Linux/UNIX, Linux/UNIX (Amazon VPC), Windows, Windows (Amazon VPC)"

instance ToText RIProductDescription where
    toText = \case
        RIDLinuxUnix -> "Linux/UNIX"
        RIDLinuxUnixAmazonVPC -> "Linux/UNIX (Amazon VPC)"
        RIDWindows -> "Windows"
        RIDWindowsAmazonVPC -> "Windows (Amazon VPC)"

instance Hashable     RIProductDescription
instance ToByteString RIProductDescription
instance ToQuery      RIProductDescription
instance ToHeader     RIProductDescription

instance FromXML RIProductDescription where
    parseXML = parseXMLText "RIProductDescription"

data RecurringChargeFrequency =
    Hourly
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RecurringChargeFrequency where
    parser = takeLowerText >>= \case
        "hourly" -> pure Hourly
        e -> fromTextError $ "Failure parsing RecurringChargeFrequency from value: '" <> e
           <> "'. Accepted values: Hourly"

instance ToText RecurringChargeFrequency where
    toText = \case
        Hourly -> "Hourly"

instance Hashable     RecurringChargeFrequency
instance ToByteString RecurringChargeFrequency
instance ToQuery      RecurringChargeFrequency
instance ToHeader     RecurringChargeFrequency

instance FromXML RecurringChargeFrequency where
    parseXML = parseXMLText "RecurringChargeFrequency"

data ReportInstanceReasonCodes
    = InstanceStuckInState
    | NotAcceptingCredentials
    | Other
    | PasswordNotAvailable
    | PerformanceEBSVolume
    | PerformanceInstanceStore
    | PerformanceNetwork
    | PerformanceOther
    | Unresponsive
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ReportInstanceReasonCodes where
    parser = takeLowerText >>= \case
        "instance-stuck-in-state" -> pure InstanceStuckInState
        "not-accepting-credentials" -> pure NotAcceptingCredentials
        "other" -> pure Other
        "password-not-available" -> pure PasswordNotAvailable
        "performance-ebs-volume" -> pure PerformanceEBSVolume
        "performance-instance-store" -> pure PerformanceInstanceStore
        "performance-network" -> pure PerformanceNetwork
        "performance-other" -> pure PerformanceOther
        "unresponsive" -> pure Unresponsive
        e -> fromTextError $ "Failure parsing ReportInstanceReasonCodes from value: '" <> e
           <> "'. Accepted values: instance-stuck-in-state, not-accepting-credentials, other, password-not-available, performance-ebs-volume, performance-instance-store, performance-network, performance-other, unresponsive"

instance ToText ReportInstanceReasonCodes where
    toText = \case
        InstanceStuckInState -> "instance-stuck-in-state"
        NotAcceptingCredentials -> "not-accepting-credentials"
        Other -> "other"
        PasswordNotAvailable -> "password-not-available"
        PerformanceEBSVolume -> "performance-ebs-volume"
        PerformanceInstanceStore -> "performance-instance-store"
        PerformanceNetwork -> "performance-network"
        PerformanceOther -> "performance-other"
        Unresponsive -> "unresponsive"

instance Hashable     ReportInstanceReasonCodes
instance ToByteString ReportInstanceReasonCodes
instance ToQuery      ReportInstanceReasonCodes
instance ToHeader     ReportInstanceReasonCodes

data ReportStatusType
    = RSTImpaired
    | RSTOK
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ReportStatusType where
    parser = takeLowerText >>= \case
        "impaired" -> pure RSTImpaired
        "ok" -> pure RSTOK
        e -> fromTextError $ "Failure parsing ReportStatusType from value: '" <> e
           <> "'. Accepted values: impaired, ok"

instance ToText ReportStatusType where
    toText = \case
        RSTImpaired -> "impaired"
        RSTOK -> "ok"

instance Hashable     ReportStatusType
instance ToByteString ReportStatusType
instance ToQuery      ReportStatusType
instance ToHeader     ReportStatusType

data ReservedInstanceState
    = Active
    | PaymentFailed
    | PaymentPending
    | Retired
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ReservedInstanceState where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "payment-failed" -> pure PaymentFailed
        "payment-pending" -> pure PaymentPending
        "retired" -> pure Retired
        e -> fromTextError $ "Failure parsing ReservedInstanceState from value: '" <> e
           <> "'. Accepted values: active, payment-failed, payment-pending, retired"

instance ToText ReservedInstanceState where
    toText = \case
        Active -> "active"
        PaymentFailed -> "payment-failed"
        PaymentPending -> "payment-pending"
        Retired -> "retired"

instance Hashable     ReservedInstanceState
instance ToByteString ReservedInstanceState
instance ToQuery      ReservedInstanceState
instance ToHeader     ReservedInstanceState

instance FromXML ReservedInstanceState where
    parseXML = parseXMLText "ReservedInstanceState"

data ResetImageAttributeName =
    RIANLaunchPermission
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ResetImageAttributeName where
    parser = takeLowerText >>= \case
        "launchpermission" -> pure RIANLaunchPermission
        e -> fromTextError $ "Failure parsing ResetImageAttributeName from value: '" <> e
           <> "'. Accepted values: launchPermission"

instance ToText ResetImageAttributeName where
    toText = \case
        RIANLaunchPermission -> "launchPermission"

instance Hashable     ResetImageAttributeName
instance ToByteString ResetImageAttributeName
instance ToQuery      ResetImageAttributeName
instance ToHeader     ResetImageAttributeName

data ResourceType
    = CustomerGateway
    | DHCPOptions
    | Image
    | Instance
    | InternetGateway
    | NetworkACL
    | NetworkInterface
    | ReservedInstances
    | RouteTable
    | SecurityGroup
    | Snapshot
    | SpotInstancesRequest
    | Subnet
    | VPC
    | VPNConnection
    | VPNGateway
    | Volume
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ResourceType where
    parser = takeLowerText >>= \case
        "customer-gateway" -> pure CustomerGateway
        "dhcp-options" -> pure DHCPOptions
        "image" -> pure Image
        "instance" -> pure Instance
        "internet-gateway" -> pure InternetGateway
        "network-acl" -> pure NetworkACL
        "network-interface" -> pure NetworkInterface
        "reserved-instances" -> pure ReservedInstances
        "route-table" -> pure RouteTable
        "security-group" -> pure SecurityGroup
        "snapshot" -> pure Snapshot
        "spot-instances-request" -> pure SpotInstancesRequest
        "subnet" -> pure Subnet
        "vpc" -> pure VPC
        "vpn-connection" -> pure VPNConnection
        "vpn-gateway" -> pure VPNGateway
        "volume" -> pure Volume
        e -> fromTextError $ "Failure parsing ResourceType from value: '" <> e
           <> "'. Accepted values: customer-gateway, dhcp-options, image, instance, internet-gateway, network-acl, network-interface, reserved-instances, route-table, security-group, snapshot, spot-instances-request, subnet, vpc, vpn-connection, vpn-gateway, volume"

instance ToText ResourceType where
    toText = \case
        CustomerGateway -> "customer-gateway"
        DHCPOptions -> "dhcp-options"
        Image -> "image"
        Instance -> "instance"
        InternetGateway -> "internet-gateway"
        NetworkACL -> "network-acl"
        NetworkInterface -> "network-interface"
        ReservedInstances -> "reserved-instances"
        RouteTable -> "route-table"
        SecurityGroup -> "security-group"
        Snapshot -> "snapshot"
        SpotInstancesRequest -> "spot-instances-request"
        Subnet -> "subnet"
        VPC -> "vpc"
        VPNConnection -> "vpn-connection"
        VPNGateway -> "vpn-gateway"
        Volume -> "volume"

instance Hashable     ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance FromXML ResourceType where
    parseXML = parseXMLText "ResourceType"

data RouteOrigin
    = CreateRoute
    | CreateRouteTable
    | EnableVGWRoutePropagation
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RouteOrigin where
    parser = takeLowerText >>= \case
        "createroute" -> pure CreateRoute
        "createroutetable" -> pure CreateRouteTable
        "enablevgwroutepropagation" -> pure EnableVGWRoutePropagation
        e -> fromTextError $ "Failure parsing RouteOrigin from value: '" <> e
           <> "'. Accepted values: CreateRoute, CreateRouteTable, EnableVgwRoutePropagation"

instance ToText RouteOrigin where
    toText = \case
        CreateRoute -> "CreateRoute"
        CreateRouteTable -> "CreateRouteTable"
        EnableVGWRoutePropagation -> "EnableVgwRoutePropagation"

instance Hashable     RouteOrigin
instance ToByteString RouteOrigin
instance ToQuery      RouteOrigin
instance ToHeader     RouteOrigin

instance FromXML RouteOrigin where
    parseXML = parseXMLText "RouteOrigin"

data RouteState
    = RSActive
    | RSBlackhole
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RouteState where
    parser = takeLowerText >>= \case
        "active" -> pure RSActive
        "blackhole" -> pure RSBlackhole
        e -> fromTextError $ "Failure parsing RouteState from value: '" <> e
           <> "'. Accepted values: active, blackhole"

instance ToText RouteState where
    toText = \case
        RSActive -> "active"
        RSBlackhole -> "blackhole"

instance Hashable     RouteState
instance ToByteString RouteState
instance ToQuery      RouteState
instance ToHeader     RouteState

instance FromXML RouteState where
    parseXML = parseXMLText "RouteState"

data RuleAction
    = Allow
    | Deny
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RuleAction where
    parser = takeLowerText >>= \case
        "allow" -> pure Allow
        "deny" -> pure Deny
        e -> fromTextError $ "Failure parsing RuleAction from value: '" <> e
           <> "'. Accepted values: allow, deny"

instance ToText RuleAction where
    toText = \case
        Allow -> "allow"
        Deny -> "deny"

instance Hashable     RuleAction
instance ToByteString RuleAction
instance ToQuery      RuleAction
instance ToHeader     RuleAction

instance FromXML RuleAction where
    parseXML = parseXMLText "RuleAction"

data ShutdownBehavior
    = Stop
    | Terminate
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ShutdownBehavior where
    parser = takeLowerText >>= \case
        "stop" -> pure Stop
        "terminate" -> pure Terminate
        e -> fromTextError $ "Failure parsing ShutdownBehavior from value: '" <> e
           <> "'. Accepted values: stop, terminate"

instance ToText ShutdownBehavior where
    toText = \case
        Stop -> "stop"
        Terminate -> "terminate"

instance Hashable     ShutdownBehavior
instance ToByteString ShutdownBehavior
instance ToQuery      ShutdownBehavior
instance ToHeader     ShutdownBehavior

data SnapshotAttributeName
    = SANCreateVolumePermission
    | SANProductCodes
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SnapshotAttributeName where
    parser = takeLowerText >>= \case
        "createvolumepermission" -> pure SANCreateVolumePermission
        "productcodes" -> pure SANProductCodes
        e -> fromTextError $ "Failure parsing SnapshotAttributeName from value: '" <> e
           <> "'. Accepted values: createVolumePermission, productCodes"

instance ToText SnapshotAttributeName where
    toText = \case
        SANCreateVolumePermission -> "createVolumePermission"
        SANProductCodes -> "productCodes"

instance Hashable     SnapshotAttributeName
instance ToByteString SnapshotAttributeName
instance ToQuery      SnapshotAttributeName
instance ToHeader     SnapshotAttributeName

data SnapshotState
    = SSCompleted
    | SSError'
    | SSPending
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SnapshotState where
    parser = takeLowerText >>= \case
        "completed" -> pure SSCompleted
        "error" -> pure SSError'
        "pending" -> pure SSPending
        e -> fromTextError $ "Failure parsing SnapshotState from value: '" <> e
           <> "'. Accepted values: completed, error, pending"

instance ToText SnapshotState where
    toText = \case
        SSCompleted -> "completed"
        SSError' -> "error"
        SSPending -> "pending"

instance Hashable     SnapshotState
instance ToByteString SnapshotState
instance ToQuery      SnapshotState
instance ToHeader     SnapshotState

instance FromXML SnapshotState where
    parseXML = parseXMLText "SnapshotState"

data SpotInstanceState
    = SISActive
    | SISCancelled
    | SISClosed
    | SISFailed
    | SISOpen
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SpotInstanceState where
    parser = takeLowerText >>= \case
        "active" -> pure SISActive
        "cancelled" -> pure SISCancelled
        "closed" -> pure SISClosed
        "failed" -> pure SISFailed
        "open" -> pure SISOpen
        e -> fromTextError $ "Failure parsing SpotInstanceState from value: '" <> e
           <> "'. Accepted values: active, cancelled, closed, failed, open"

instance ToText SpotInstanceState where
    toText = \case
        SISActive -> "active"
        SISCancelled -> "cancelled"
        SISClosed -> "closed"
        SISFailed -> "failed"
        SISOpen -> "open"

instance Hashable     SpotInstanceState
instance ToByteString SpotInstanceState
instance ToQuery      SpotInstanceState
instance ToHeader     SpotInstanceState

instance FromXML SpotInstanceState where
    parseXML = parseXMLText "SpotInstanceState"

data SpotInstanceType
    = OneTime
    | Persistent
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SpotInstanceType where
    parser = takeLowerText >>= \case
        "one-time" -> pure OneTime
        "persistent" -> pure Persistent
        e -> fromTextError $ "Failure parsing SpotInstanceType from value: '" <> e
           <> "'. Accepted values: one-time, persistent"

instance ToText SpotInstanceType where
    toText = \case
        OneTime -> "one-time"
        Persistent -> "persistent"

instance Hashable     SpotInstanceType
instance ToByteString SpotInstanceType
instance ToQuery      SpotInstanceType
instance ToHeader     SpotInstanceType

instance FromXML SpotInstanceType where
    parseXML = parseXMLText "SpotInstanceType"

data State
    = SAvailable
    | SDeleted
    | SDeleting
    | SPending
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText State where
    parser = takeLowerText >>= \case
        "available" -> pure SAvailable
        "deleted" -> pure SDeleted
        "deleting" -> pure SDeleting
        "pending" -> pure SPending
        e -> fromTextError $ "Failure parsing State from value: '" <> e
           <> "'. Accepted values: Available, Deleted, Deleting, Pending"

instance ToText State where
    toText = \case
        SAvailable -> "Available"
        SDeleted -> "Deleted"
        SDeleting -> "Deleting"
        SPending -> "Pending"

instance Hashable     State
instance ToByteString State
instance ToQuery      State
instance ToHeader     State

instance FromXML State where
    parseXML = parseXMLText "State"

data StatusName =
    Reachability
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StatusName where
    parser = takeLowerText >>= \case
        "reachability" -> pure Reachability
        e -> fromTextError $ "Failure parsing StatusName from value: '" <> e
           <> "'. Accepted values: reachability"

instance ToText StatusName where
    toText = \case
        Reachability -> "reachability"

instance Hashable     StatusName
instance ToByteString StatusName
instance ToQuery      StatusName
instance ToHeader     StatusName

instance FromXML StatusName where
    parseXML = parseXMLText "StatusName"

data StatusType
    = STFailed
    | STInitializing
    | STInsufficientData
    | STPassed
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StatusType where
    parser = takeLowerText >>= \case
        "failed" -> pure STFailed
        "initializing" -> pure STInitializing
        "insufficient-data" -> pure STInsufficientData
        "passed" -> pure STPassed
        e -> fromTextError $ "Failure parsing StatusType from value: '" <> e
           <> "'. Accepted values: failed, initializing, insufficient-data, passed"

instance ToText StatusType where
    toText = \case
        STFailed -> "failed"
        STInitializing -> "initializing"
        STInsufficientData -> "insufficient-data"
        STPassed -> "passed"

instance Hashable     StatusType
instance ToByteString StatusType
instance ToQuery      StatusType
instance ToHeader     StatusType

instance FromXML StatusType where
    parseXML = parseXMLText "StatusType"

data SubnetState
    = SubAvailable
    | SubPending
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SubnetState where
    parser = takeLowerText >>= \case
        "available" -> pure SubAvailable
        "pending" -> pure SubPending
        e -> fromTextError $ "Failure parsing SubnetState from value: '" <> e
           <> "'. Accepted values: available, pending"

instance ToText SubnetState where
    toText = \case
        SubAvailable -> "available"
        SubPending -> "pending"

instance Hashable     SubnetState
instance ToByteString SubnetState
instance ToQuery      SubnetState
instance ToHeader     SubnetState

instance FromXML SubnetState where
    parseXML = parseXMLText "SubnetState"

data SummaryStatus
    = SSImpaired
    | SSInitializing
    | SSInsufficientData
    | SSNotApplicable
    | SSOK
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SummaryStatus where
    parser = takeLowerText >>= \case
        "impaired" -> pure SSImpaired
        "initializing" -> pure SSInitializing
        "insufficient-data" -> pure SSInsufficientData
        "not-applicable" -> pure SSNotApplicable
        "ok" -> pure SSOK
        e -> fromTextError $ "Failure parsing SummaryStatus from value: '" <> e
           <> "'. Accepted values: impaired, initializing, insufficient-data, not-applicable, ok"

instance ToText SummaryStatus where
    toText = \case
        SSImpaired -> "impaired"
        SSInitializing -> "initializing"
        SSInsufficientData -> "insufficient-data"
        SSNotApplicable -> "not-applicable"
        SSOK -> "ok"

instance Hashable     SummaryStatus
instance ToByteString SummaryStatus
instance ToQuery      SummaryStatus
instance ToHeader     SummaryStatus

instance FromXML SummaryStatus where
    parseXML = parseXMLText "SummaryStatus"

data TelemetryStatus
    = Down
    | UP
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText TelemetryStatus where
    parser = takeLowerText >>= \case
        "down" -> pure Down
        "up" -> pure UP
        e -> fromTextError $ "Failure parsing TelemetryStatus from value: '" <> e
           <> "'. Accepted values: DOWN, UP"

instance ToText TelemetryStatus where
    toText = \case
        Down -> "DOWN"
        UP -> "UP"

instance Hashable     TelemetryStatus
instance ToByteString TelemetryStatus
instance ToQuery      TelemetryStatus
instance ToHeader     TelemetryStatus

instance FromXML TelemetryStatus where
    parseXML = parseXMLText "TelemetryStatus"

data Tenancy
    = Dedicated
    | Default
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Tenancy where
    parser = takeLowerText >>= \case
        "dedicated" -> pure Dedicated
        "default" -> pure Default
        e -> fromTextError $ "Failure parsing Tenancy from value: '" <> e
           <> "'. Accepted values: dedicated, default"

instance ToText Tenancy where
    toText = \case
        Dedicated -> "dedicated"
        Default -> "default"

instance Hashable     Tenancy
instance ToByteString Tenancy
instance ToQuery      Tenancy
instance ToHeader     Tenancy

instance FromXML Tenancy where
    parseXML = parseXMLText "Tenancy"

data TrafficType
    = TTAccept
    | TTAll
    | TTReject
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText TrafficType where
    parser = takeLowerText >>= \case
        "accept" -> pure TTAccept
        "all" -> pure TTAll
        "reject" -> pure TTReject
        e -> fromTextError $ "Failure parsing TrafficType from value: '" <> e
           <> "'. Accepted values: ACCEPT, ALL, REJECT"

instance ToText TrafficType where
    toText = \case
        TTAccept -> "ACCEPT"
        TTAll -> "ALL"
        TTReject -> "REJECT"

instance Hashable     TrafficType
instance ToByteString TrafficType
instance ToQuery      TrafficType
instance ToHeader     TrafficType

instance FromXML TrafficType where
    parseXML = parseXMLText "TrafficType"

data VPCAttributeName
    = EnableDNSHostnames
    | EnableDNSSupport
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VPCAttributeName where
    parser = takeLowerText >>= \case
        "enablednshostnames" -> pure EnableDNSHostnames
        "enablednssupport" -> pure EnableDNSSupport
        e -> fromTextError $ "Failure parsing VPCAttributeName from value: '" <> e
           <> "'. Accepted values: enableDnsHostnames, enableDnsSupport"

instance ToText VPCAttributeName where
    toText = \case
        EnableDNSHostnames -> "enableDnsHostnames"
        EnableDNSSupport -> "enableDnsSupport"

instance Hashable     VPCAttributeName
instance ToByteString VPCAttributeName
instance ToQuery      VPCAttributeName
instance ToHeader     VPCAttributeName

data VPCPeeringConnectionStateReasonCode
    = VPCSRCActive
    | VPCSRCDeleted
    | VPCSRCDeleting
    | VPCSRCExpired
    | VPCSRCFailed
    | VPCSRCInitiatingRequest
    | VPCSRCPendingAcceptance
    | VPCSRCProvisioning
    | VPCSRCRejected
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VPCPeeringConnectionStateReasonCode where
    parser = takeLowerText >>= \case
        "active" -> pure VPCSRCActive
        "deleted" -> pure VPCSRCDeleted
        "deleting" -> pure VPCSRCDeleting
        "expired" -> pure VPCSRCExpired
        "failed" -> pure VPCSRCFailed
        "initiating-request" -> pure VPCSRCInitiatingRequest
        "pending-acceptance" -> pure VPCSRCPendingAcceptance
        "provisioning" -> pure VPCSRCProvisioning
        "rejected" -> pure VPCSRCRejected
        e -> fromTextError $ "Failure parsing VPCPeeringConnectionStateReasonCode from value: '" <> e
           <> "'. Accepted values: active, deleted, deleting, expired, failed, initiating-request, pending-acceptance, provisioning, rejected"

instance ToText VPCPeeringConnectionStateReasonCode where
    toText = \case
        VPCSRCActive -> "active"
        VPCSRCDeleted -> "deleted"
        VPCSRCDeleting -> "deleting"
        VPCSRCExpired -> "expired"
        VPCSRCFailed -> "failed"
        VPCSRCInitiatingRequest -> "initiating-request"
        VPCSRCPendingAcceptance -> "pending-acceptance"
        VPCSRCProvisioning -> "provisioning"
        VPCSRCRejected -> "rejected"

instance Hashable     VPCPeeringConnectionStateReasonCode
instance ToByteString VPCPeeringConnectionStateReasonCode
instance ToQuery      VPCPeeringConnectionStateReasonCode
instance ToHeader     VPCPeeringConnectionStateReasonCode

instance FromXML VPCPeeringConnectionStateReasonCode where
    parseXML = parseXMLText "VPCPeeringConnectionStateReasonCode"

data VPCState
    = VPCSAvailable
    | VPCSPending
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VPCState where
    parser = takeLowerText >>= \case
        "available" -> pure VPCSAvailable
        "pending" -> pure VPCSPending
        e -> fromTextError $ "Failure parsing VPCState from value: '" <> e
           <> "'. Accepted values: available, pending"

instance ToText VPCState where
    toText = \case
        VPCSAvailable -> "available"
        VPCSPending -> "pending"

instance Hashable     VPCState
instance ToByteString VPCState
instance ToQuery      VPCState
instance ToHeader     VPCState

instance FromXML VPCState where
    parseXML = parseXMLText "VPCState"

data VPNState
    = VSAvailable
    | VSDeleted
    | VSDeleting
    | VSPending
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VPNState where
    parser = takeLowerText >>= \case
        "available" -> pure VSAvailable
        "deleted" -> pure VSDeleted
        "deleting" -> pure VSDeleting
        "pending" -> pure VSPending
        e -> fromTextError $ "Failure parsing VPNState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText VPNState where
    toText = \case
        VSAvailable -> "available"
        VSDeleted -> "deleted"
        VSDeleting -> "deleting"
        VSPending -> "pending"

instance Hashable     VPNState
instance ToByteString VPNState
instance ToQuery      VPNState
instance ToHeader     VPNState

instance FromXML VPNState where
    parseXML = parseXMLText "VPNState"

data VPNStaticRouteSource =
    Static
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VPNStaticRouteSource where
    parser = takeLowerText >>= \case
        "static" -> pure Static
        e -> fromTextError $ "Failure parsing VPNStaticRouteSource from value: '" <> e
           <> "'. Accepted values: Static"

instance ToText VPNStaticRouteSource where
    toText = \case
        Static -> "Static"

instance Hashable     VPNStaticRouteSource
instance ToByteString VPNStaticRouteSource
instance ToQuery      VPNStaticRouteSource
instance ToHeader     VPNStaticRouteSource

instance FromXML VPNStaticRouteSource where
    parseXML = parseXMLText "VPNStaticRouteSource"

data VirtualizationType
    = HVM
    | Paravirtual
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VirtualizationType where
    parser = takeLowerText >>= \case
        "hvm" -> pure HVM
        "paravirtual" -> pure Paravirtual
        e -> fromTextError $ "Failure parsing VirtualizationType from value: '" <> e
           <> "'. Accepted values: hvm, paravirtual"

instance ToText VirtualizationType where
    toText = \case
        HVM -> "hvm"
        Paravirtual -> "paravirtual"

instance Hashable     VirtualizationType
instance ToByteString VirtualizationType
instance ToQuery      VirtualizationType
instance ToHeader     VirtualizationType

instance FromXML VirtualizationType where
    parseXML = parseXMLText "VirtualizationType"

data VolumeAttachmentState
    = VASAttached
    | VASAttaching
    | VASDetached
    | VASDetaching
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VolumeAttachmentState where
    parser = takeLowerText >>= \case
        "attached" -> pure VASAttached
        "attaching" -> pure VASAttaching
        "detached" -> pure VASDetached
        "detaching" -> pure VASDetaching
        e -> fromTextError $ "Failure parsing VolumeAttachmentState from value: '" <> e
           <> "'. Accepted values: attached, attaching, detached, detaching"

instance ToText VolumeAttachmentState where
    toText = \case
        VASAttached -> "attached"
        VASAttaching -> "attaching"
        VASDetached -> "detached"
        VASDetaching -> "detaching"

instance Hashable     VolumeAttachmentState
instance ToByteString VolumeAttachmentState
instance ToQuery      VolumeAttachmentState
instance ToHeader     VolumeAttachmentState

instance FromXML VolumeAttachmentState where
    parseXML = parseXMLText "VolumeAttachmentState"

data VolumeAttributeName
    = VANAutoEnableIO
    | VANProductCodes
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VolumeAttributeName where
    parser = takeLowerText >>= \case
        "autoenableio" -> pure VANAutoEnableIO
        "productcodes" -> pure VANProductCodes
        e -> fromTextError $ "Failure parsing VolumeAttributeName from value: '" <> e
           <> "'. Accepted values: autoEnableIO, productCodes"

instance ToText VolumeAttributeName where
    toText = \case
        VANAutoEnableIO -> "autoEnableIO"
        VANProductCodes -> "productCodes"

instance Hashable     VolumeAttributeName
instance ToByteString VolumeAttributeName
instance ToQuery      VolumeAttributeName
instance ToHeader     VolumeAttributeName

data VolumeState
    = VAvailable
    | VCreating
    | VDeleted
    | VDeleting
    | VError'
    | VInUse
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VolumeState where
    parser = takeLowerText >>= \case
        "available" -> pure VAvailable
        "creating" -> pure VCreating
        "deleted" -> pure VDeleted
        "deleting" -> pure VDeleting
        "error" -> pure VError'
        "in-use" -> pure VInUse
        e -> fromTextError $ "Failure parsing VolumeState from value: '" <> e
           <> "'. Accepted values: available, creating, deleted, deleting, error, in-use"

instance ToText VolumeState where
    toText = \case
        VAvailable -> "available"
        VCreating -> "creating"
        VDeleted -> "deleted"
        VDeleting -> "deleting"
        VError' -> "error"
        VInUse -> "in-use"

instance Hashable     VolumeState
instance ToByteString VolumeState
instance ToQuery      VolumeState
instance ToHeader     VolumeState

instance FromXML VolumeState where
    parseXML = parseXMLText "VolumeState"

data VolumeStatusInfoStatus
    = Impaired
    | InsufficientData
    | OK
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VolumeStatusInfoStatus where
    parser = takeLowerText >>= \case
        "impaired" -> pure Impaired
        "insufficient-data" -> pure InsufficientData
        "ok" -> pure OK
        e -> fromTextError $ "Failure parsing VolumeStatusInfoStatus from value: '" <> e
           <> "'. Accepted values: impaired, insufficient-data, ok"

instance ToText VolumeStatusInfoStatus where
    toText = \case
        Impaired -> "impaired"
        InsufficientData -> "insufficient-data"
        OK -> "ok"

instance Hashable     VolumeStatusInfoStatus
instance ToByteString VolumeStatusInfoStatus
instance ToQuery      VolumeStatusInfoStatus
instance ToHeader     VolumeStatusInfoStatus

instance FromXML VolumeStatusInfoStatus where
    parseXML = parseXMLText "VolumeStatusInfoStatus"

data VolumeStatusName
    = IOEnabled
    | IOPerformance
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VolumeStatusName where
    parser = takeLowerText >>= \case
        "io-enabled" -> pure IOEnabled
        "io-performance" -> pure IOPerformance
        e -> fromTextError $ "Failure parsing VolumeStatusName from value: '" <> e
           <> "'. Accepted values: io-enabled, io-performance"

instance ToText VolumeStatusName where
    toText = \case
        IOEnabled -> "io-enabled"
        IOPerformance -> "io-performance"

instance Hashable     VolumeStatusName
instance ToByteString VolumeStatusName
instance ToQuery      VolumeStatusName
instance ToHeader     VolumeStatusName

instance FromXML VolumeStatusName where
    parseXML = parseXMLText "VolumeStatusName"

data VolumeType
    = GP2
    | IO1
    | Standard
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VolumeType where
    parser = takeLowerText >>= \case
        "gp2" -> pure GP2
        "io1" -> pure IO1
        "standard" -> pure Standard
        e -> fromTextError $ "Failure parsing VolumeType from value: '" <> e
           <> "'. Accepted values: gp2, io1, standard"

instance ToText VolumeType where
    toText = \case
        GP2 -> "gp2"
        IO1 -> "io1"
        Standard -> "standard"

instance Hashable     VolumeType
instance ToByteString VolumeType
instance ToQuery      VolumeType
instance ToHeader     VolumeType

instance FromXML VolumeType where
    parseXML = parseXMLText "VolumeType"
