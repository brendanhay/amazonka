{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Elastic Compute Cloud (Amazon EC2) is a web service that provides
-- resizable compute capacity in the cloud. It is designed to make web-scale
-- computing easier for developers. Amazon EC2’s simple web service interface
-- allows you to obtain and configure capacity with minimal friction. It
-- provides you with complete control of your computing resources and lets you
-- run on Amazon’s proven computing environment. Amazon EC2 reduces the time
-- required to obtain and boot new server instances to minutes, allowing you
-- to quickly scale capacity, both up and down, as your computing requirements
-- change. Amazon EC2 changes the economics of computing by allowing you to
-- pay only for capacity that you actually use. Amazon EC2 provides developers
-- the tools to build failure resilient applications and isolate themselves
-- from common failure scenarios.
module Network.AWS.EC2.V2014_06_15.Types where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2014-06-15@) of the
-- @Amazon Elastic Compute Cloud@ service.
data EC2 deriving (Typeable)

instance AWSService EC2 where
    type Sg EC2 = V4
    data Er EC2
        = EC2Client HttpException
        | EC2Serializer String
        | EC2Service String

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "ec2"
        , _svcVersion  = "2014-06-15"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er EC2)
deriving instance Generic (Er EC2)

instance AWSError (Er EC2) where
    awsError = const "EC2Error"

instance AWSServiceError (Er EC2) where
    serviceError    = EC2Service
    clientError     = EC2Client
    serializerError = EC2Serializer

instance Exception (Er EC2)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://ec2.amazonaws.com/doc/2014-06-15/"
    }

data AccountAttributeName
    = AccountAttributeNameDefaultVpc -- ^ default-vpc
    | AccountAttributeNameSupportedPlatforms -- ^ supported-platforms
      deriving (Eq, Show, Generic)

instance Hashable AccountAttributeName

instance FromText AccountAttributeName where
    parser = match "default-vpc" AccountAttributeNameDefaultVpc
         <|> match "supported-platforms" AccountAttributeNameSupportedPlatforms

instance ToText AccountAttributeName where
    toText AccountAttributeNameDefaultVpc = "default-vpc"
    toText AccountAttributeNameSupportedPlatforms = "supported-platforms"

instance ToByteString AccountAttributeName

instance ToQuery AccountAttributeName where
    toQuery = genericQuery def

-- | The architecture of the instance.
data ArchitectureValues
    = ArchitectureValuesI386 -- ^ i386
    | ArchitectureValuesX8664 -- ^ x86_64
      deriving (Eq, Show, Generic)

instance Hashable ArchitectureValues

instance FromText ArchitectureValues where
    parser = match "i386" ArchitectureValuesI386
         <|> match "x86_64" ArchitectureValuesX8664

instance ToText ArchitectureValues where
    toText ArchitectureValuesI386 = "i386"
    toText ArchitectureValuesX8664 = "x86_64"

instance ToByteString ArchitectureValues

instance FromXML ArchitectureValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ArchitectureValues"

instance ToQuery ArchitectureValues where
    toQuery = genericQuery def

-- | The current state of the attachment.
data AttachmentStatus
    = AttachmentStatusAttached -- ^ attached
    | AttachmentStatusAttaching -- ^ attaching
    | AttachmentStatusDetached -- ^ detached
    | AttachmentStatusDetaching -- ^ detaching
      deriving (Eq, Show, Generic)

instance Hashable AttachmentStatus

instance FromText AttachmentStatus where
    parser = match "attached" AttachmentStatusAttached
         <|> match "attaching" AttachmentStatusAttaching
         <|> match "detached" AttachmentStatusDetached
         <|> match "detaching" AttachmentStatusDetaching

instance ToText AttachmentStatus where
    toText AttachmentStatusAttached = "attached"
    toText AttachmentStatusAttaching = "attaching"
    toText AttachmentStatusDetached = "detached"
    toText AttachmentStatusDetaching = "detaching"

instance ToByteString AttachmentStatus

instance FromXML AttachmentStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery AttachmentStatus where
    toQuery = genericQuery def

-- | The state of the Availability Zone.
data AvailabilityZoneState
    = AvailabilityZoneStateAvailable -- ^ available
      deriving (Eq, Show, Generic)

instance Hashable AvailabilityZoneState

instance FromText AvailabilityZoneState where
    parser = match "available" AvailabilityZoneStateAvailable

instance ToText AvailabilityZoneState where
    toText AvailabilityZoneStateAvailable = "available"

instance ToByteString AvailabilityZoneState

instance FromXML AvailabilityZoneState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "zoneState"

instance ToQuery AvailabilityZoneState where
    toQuery = genericQuery def

-- | The state of the task.
data BundleTaskState
    = BundleTaskStateBundling -- ^ bundling
    | BundleTaskStateCancelling -- ^ cancelling
    | BundleTaskStateComplete -- ^ complete
    | BundleTaskStateFailed -- ^ failed
    | BundleTaskStatePending -- ^ pending
    | BundleTaskStateStoring -- ^ storing
    | BundleTaskStateWaitingForShutdown -- ^ waiting-for-shutdown
      deriving (Eq, Show, Generic)

instance Hashable BundleTaskState

instance FromText BundleTaskState where
    parser = match "bundling" BundleTaskStateBundling
         <|> match "cancelling" BundleTaskStateCancelling
         <|> match "complete" BundleTaskStateComplete
         <|> match "failed" BundleTaskStateFailed
         <|> match "pending" BundleTaskStatePending
         <|> match "storing" BundleTaskStateStoring
         <|> match "waiting-for-shutdown" BundleTaskStateWaitingForShutdown

instance ToText BundleTaskState where
    toText BundleTaskStateBundling = "bundling"
    toText BundleTaskStateCancelling = "cancelling"
    toText BundleTaskStateComplete = "complete"
    toText BundleTaskStateFailed = "failed"
    toText BundleTaskStatePending = "pending"
    toText BundleTaskStateStoring = "storing"
    toText BundleTaskStateWaitingForShutdown = "waiting-for-shutdown"

instance ToByteString BundleTaskState

instance FromXML BundleTaskState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery BundleTaskState where
    toQuery = genericQuery def

-- | The state of the Spot Instance request.
data CancelSpotInstanceRequestState
    = CancelSpotInstanceRequestStateActive -- ^ active
    | CancelSpotInstanceRequestStateCancelled -- ^ cancelled
    | CancelSpotInstanceRequestStateClosed -- ^ closed
    | CancelSpotInstanceRequestStateCompleted -- ^ completed
    | CancelSpotInstanceRequestStateOpen -- ^ open
      deriving (Eq, Show, Generic)

instance Hashable CancelSpotInstanceRequestState

instance FromText CancelSpotInstanceRequestState where
    parser = match "active" CancelSpotInstanceRequestStateActive
         <|> match "cancelled" CancelSpotInstanceRequestStateCancelled
         <|> match "closed" CancelSpotInstanceRequestStateClosed
         <|> match "completed" CancelSpotInstanceRequestStateCompleted
         <|> match "open" CancelSpotInstanceRequestStateOpen

instance ToText CancelSpotInstanceRequestState where
    toText CancelSpotInstanceRequestStateActive = "active"
    toText CancelSpotInstanceRequestStateCancelled = "cancelled"
    toText CancelSpotInstanceRequestStateClosed = "closed"
    toText CancelSpotInstanceRequestStateCompleted = "completed"
    toText CancelSpotInstanceRequestStateOpen = "open"

instance ToByteString CancelSpotInstanceRequestState

instance FromXML CancelSpotInstanceRequestState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery CancelSpotInstanceRequestState where
    toQuery = genericQuery def

-- | 
data ContainerFormat
    = ContainerFormatOva -- ^ ova
      deriving (Eq, Show, Generic)

instance Hashable ContainerFormat

instance FromText ContainerFormat where
    parser = match "ova" ContainerFormatOva

instance ToText ContainerFormat where
    toText ContainerFormatOva = "ova"

instance ToByteString ContainerFormat

instance FromXML ContainerFormat where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ContainerFormat"

instance ToQuery ContainerFormat where
    toQuery = genericQuery def

-- | The state of the conversion task.
data ConversionTaskState
    = ConversionTaskStateActive -- ^ active
    | ConversionTaskStateCancelled -- ^ cancelled
    | ConversionTaskStateCancelling -- ^ cancelling
    | ConversionTaskStateCompleted -- ^ completed
      deriving (Eq, Show, Generic)

instance Hashable ConversionTaskState

instance FromText ConversionTaskState where
    parser = match "active" ConversionTaskStateActive
         <|> match "cancelled" ConversionTaskStateCancelled
         <|> match "cancelling" ConversionTaskStateCancelling
         <|> match "completed" ConversionTaskStateCompleted

instance ToText ConversionTaskState where
    toText ConversionTaskStateActive = "active"
    toText ConversionTaskStateCancelled = "cancelled"
    toText ConversionTaskStateCancelling = "cancelling"
    toText ConversionTaskStateCompleted = "completed"

instance ToByteString ConversionTaskState

instance FromXML ConversionTaskState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery ConversionTaskState where
    toQuery = genericQuery def

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is USD.
data CurrencyCodeValues
    = CurrencyCodeValuesUsd -- ^ USD
      deriving (Eq, Show, Generic)

instance Hashable CurrencyCodeValues

instance FromText CurrencyCodeValues where
    parser = match "USD" CurrencyCodeValuesUsd

instance ToText CurrencyCodeValues where
    toText CurrencyCodeValuesUsd = "USD"

instance ToByteString CurrencyCodeValues

instance FromXML CurrencyCodeValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "currencyCode"

instance ToQuery CurrencyCodeValues where
    toQuery = genericQuery def

-- | The state of the Spot Instance datafeed subscription.
data DatafeedSubscriptionState
    = DatafeedSubscriptionStateActive -- ^ Active
    | DatafeedSubscriptionStateInactive -- ^ Inactive
      deriving (Eq, Show, Generic)

instance Hashable DatafeedSubscriptionState

instance FromText DatafeedSubscriptionState where
    parser = match "Active" DatafeedSubscriptionStateActive
         <|> match "Inactive" DatafeedSubscriptionStateInactive

instance ToText DatafeedSubscriptionState where
    toText DatafeedSubscriptionStateActive = "Active"
    toText DatafeedSubscriptionStateInactive = "Inactive"

instance ToByteString DatafeedSubscriptionState

instance FromXML DatafeedSubscriptionState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

-- | The root device type used by the AMI. The AMI can use an Amazon EBS volume
-- or an instance store volume.
data DeviceType
    = DeviceTypeEbs -- ^ ebs
    | DeviceTypeInstanceStore -- ^ instance-store
      deriving (Eq, Show, Generic)

instance Hashable DeviceType

instance FromText DeviceType where
    parser = match "ebs" DeviceTypeEbs
         <|> match "instance-store" DeviceTypeInstanceStore

instance ToText DeviceType where
    toText DeviceTypeEbs = "ebs"
    toText DeviceTypeInstanceStore = "instance-store"

instance ToByteString DeviceType

instance FromXML DeviceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "rootDeviceType"

instance ToQuery DeviceType where
    toQuery = genericQuery def

-- | The disk image format.
data DiskImageFormat
    = DiskImageFormatRaw -- ^ RAW
    | DiskImageFormatVhd -- ^ VHD
    | DiskImageFormatVmdk -- ^ VMDK
      deriving (Eq, Show, Generic)

instance Hashable DiskImageFormat

instance FromText DiskImageFormat where
    parser = match "RAW" DiskImageFormatRaw
         <|> match "VHD" DiskImageFormatVhd
         <|> match "VMDK" DiskImageFormatVmdk

instance ToText DiskImageFormat where
    toText DiskImageFormatRaw = "RAW"
    toText DiskImageFormatVhd = "VHD"
    toText DiskImageFormatVmdk = "VMDK"

instance ToByteString DiskImageFormat

instance FromXML DiskImageFormat where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DiskImageFormat"

instance ToQuery DiskImageFormat where
    toQuery = genericQuery def

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (standard) or instances in a VPC (vpc).
data DomainType
    = DomainTypeStandard -- ^ standard
    | DomainTypeVpc -- ^ vpc
      deriving (Eq, Show, Generic)

instance Hashable DomainType

instance FromText DomainType where
    parser = match "standard" DomainTypeStandard
         <|> match "vpc" DomainTypeVpc

instance ToText DomainType where
    toText DomainTypeStandard = "standard"
    toText DomainTypeVpc = "vpc"

instance ToByteString DomainType

instance FromXML DomainType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "domain"

instance ToQuery DomainType where
    toQuery = genericQuery def

-- | The associated code of the event.
data EventCode
    = EventCodeInstanceReboot -- ^ instance-reboot
    | EventCodeInstanceRetirement -- ^ instance-retirement
    | EventCodeInstanceStop -- ^ instance-stop
    | EventCodeSystemMaintenance -- ^ system-maintenance
    | EventCodeSystemReboot -- ^ system-reboot
      deriving (Eq, Show, Generic)

instance Hashable EventCode

instance FromText EventCode where
    parser = match "instance-reboot" EventCodeInstanceReboot
         <|> match "instance-retirement" EventCodeInstanceRetirement
         <|> match "instance-stop" EventCodeInstanceStop
         <|> match "system-maintenance" EventCodeSystemMaintenance
         <|> match "system-reboot" EventCodeSystemReboot

instance ToText EventCode where
    toText EventCodeInstanceReboot = "instance-reboot"
    toText EventCodeInstanceRetirement = "instance-retirement"
    toText EventCodeInstanceStop = "instance-stop"
    toText EventCodeSystemMaintenance = "system-maintenance"
    toText EventCodeSystemReboot = "system-reboot"

instance ToByteString EventCode

instance FromXML EventCode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "code"

instance ToQuery EventCode where
    toQuery = genericQuery def

-- | The target virtualization environment.
data ExportEnvironment
    = ExportEnvironmentCitrix -- ^ citrix
    | ExportEnvironmentMicrosoft -- ^ microsoft
    | ExportEnvironmentVmware -- ^ vmware
      deriving (Eq, Show, Generic)

instance Hashable ExportEnvironment

instance FromText ExportEnvironment where
    parser = match "citrix" ExportEnvironmentCitrix
         <|> match "microsoft" ExportEnvironmentMicrosoft
         <|> match "vmware" ExportEnvironmentVmware

instance ToText ExportEnvironment where
    toText ExportEnvironmentCitrix = "citrix"
    toText ExportEnvironmentMicrosoft = "microsoft"
    toText ExportEnvironmentVmware = "vmware"

instance ToByteString ExportEnvironment

instance FromXML ExportEnvironment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ExportEnvironment"

instance ToQuery ExportEnvironment where
    toQuery = genericQuery def

-- | The state of the conversion task.
data ExportTaskState
    = ExportTaskStateActive -- ^ active
    | ExportTaskStateCancelled -- ^ cancelled
    | ExportTaskStateCancelling -- ^ cancelling
    | ExportTaskStateCompleted -- ^ completed
      deriving (Eq, Show, Generic)

instance Hashable ExportTaskState

instance FromText ExportTaskState where
    parser = match "active" ExportTaskStateActive
         <|> match "cancelled" ExportTaskStateCancelled
         <|> match "cancelling" ExportTaskStateCancelling
         <|> match "completed" ExportTaskStateCompleted

instance ToText ExportTaskState where
    toText ExportTaskStateActive = "active"
    toText ExportTaskStateCancelled = "cancelled"
    toText ExportTaskStateCancelling = "cancelling"
    toText ExportTaskStateCompleted = "completed"

instance ToByteString ExportTaskState

instance FromXML ExportTaskState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery ExportTaskState where
    toQuery = genericQuery def

-- | The type of VPN connection this virtual private gateway supports.
data GatewayType
    = GatewayTypeIpsec1 -- ^ ipsec.1
      deriving (Eq, Show, Generic)

instance Hashable GatewayType

instance FromText GatewayType where
    parser = match "ipsec.1" GatewayTypeIpsec1

instance ToText GatewayType where
    toText GatewayTypeIpsec1 = "ipsec.1"

instance ToByteString GatewayType

instance FromXML GatewayType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GatewayType"

instance ToQuery GatewayType where
    toQuery = genericQuery def

-- | The hypervisor type of the instance.
data HypervisorType
    = HypervisorTypeOvm -- ^ ovm
    | HypervisorTypeXen -- ^ xen
      deriving (Eq, Show, Generic)

instance Hashable HypervisorType

instance FromText HypervisorType where
    parser = match "ovm" HypervisorTypeOvm
         <|> match "xen" HypervisorTypeXen

instance ToText HypervisorType where
    toText HypervisorTypeOvm = "ovm"
    toText HypervisorTypeXen = "xen"

instance ToByteString HypervisorType

instance FromXML HypervisorType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "hypervisor"

instance ToQuery HypervisorType where
    toQuery = genericQuery def

-- | The AMI attribute.
data ImageAttributeName
    = ImageAttributeNameBlockDeviceMapping -- ^ blockDeviceMapping
    | ImageAttributeNameDescription -- ^ description
    | ImageAttributeNameKernel -- ^ kernel
    | ImageAttributeNameLaunchPermission -- ^ launchPermission
    | ImageAttributeNameProductCodes -- ^ productCodes
    | ImageAttributeNameRamdisk -- ^ ramdisk
      deriving (Eq, Show, Generic)

instance Hashable ImageAttributeName

instance FromText ImageAttributeName where
    parser = match "blockDeviceMapping" ImageAttributeNameBlockDeviceMapping
         <|> match "description" ImageAttributeNameDescription
         <|> match "kernel" ImageAttributeNameKernel
         <|> match "launchPermission" ImageAttributeNameLaunchPermission
         <|> match "productCodes" ImageAttributeNameProductCodes
         <|> match "ramdisk" ImageAttributeNameRamdisk

instance ToText ImageAttributeName where
    toText ImageAttributeNameBlockDeviceMapping = "blockDeviceMapping"
    toText ImageAttributeNameDescription = "description"
    toText ImageAttributeNameKernel = "kernel"
    toText ImageAttributeNameLaunchPermission = "launchPermission"
    toText ImageAttributeNameProductCodes = "productCodes"
    toText ImageAttributeNameRamdisk = "ramdisk"

instance ToByteString ImageAttributeName

instance ToQuery ImageAttributeName where
    toQuery = genericQuery def

-- | The current state of the AMI. If the state is available, the image is
-- successfully registered and can be used to launch an instance.
data ImageState
    = ImageStateAvailable -- ^ available
    | ImageStateDeregistered -- ^ deregistered
      deriving (Eq, Show, Generic)

instance Hashable ImageState

instance FromText ImageState where
    parser = match "available" ImageStateAvailable
         <|> match "deregistered" ImageStateDeregistered

instance ToText ImageState where
    toText ImageStateAvailable = "available"
    toText ImageStateDeregistered = "deregistered"

instance ToByteString ImageState

instance FromXML ImageState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "imageState"

instance ToQuery ImageState where
    toQuery = genericQuery def

-- | The type of image.
data ImageTypeValues
    = ImageTypeValuesKernel -- ^ kernel
    | ImageTypeValuesMachine -- ^ machine
    | ImageTypeValuesRamdisk -- ^ ramdisk
      deriving (Eq, Show, Generic)

instance Hashable ImageTypeValues

instance FromText ImageTypeValues where
    parser = match "kernel" ImageTypeValuesKernel
         <|> match "machine" ImageTypeValuesMachine
         <|> match "ramdisk" ImageTypeValuesRamdisk

instance ToText ImageTypeValues where
    toText ImageTypeValuesKernel = "kernel"
    toText ImageTypeValuesMachine = "machine"
    toText ImageTypeValuesRamdisk = "ramdisk"

instance ToByteString ImageTypeValues

instance FromXML ImageTypeValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "imageType"

instance ToQuery ImageTypeValues where
    toQuery = genericQuery def

-- | The attribute to reset.
data InstanceAttributeName
    = InstanceAttributeNameBlockDeviceMapping -- ^ blockDeviceMapping
    | InstanceAttributeNameDisableApiTermination -- ^ disableApiTermination
    | InstanceAttributeNameEbsOptimized -- ^ ebsOptimized
    | InstanceAttributeNameGroupSet -- ^ groupSet
    | InstanceAttributeNameInstanceInitiatedShutdownBehavior -- ^ instanceInitiatedShutdownBehavior
    | InstanceAttributeNameInstanceType -- ^ instanceType
    | InstanceAttributeNameKernel -- ^ kernel
    | InstanceAttributeNameProductCodes -- ^ productCodes
    | InstanceAttributeNameRamdisk -- ^ ramdisk
    | InstanceAttributeNameRootDeviceName -- ^ rootDeviceName
    | InstanceAttributeNameSourceDestCheck -- ^ sourceDestCheck
    | InstanceAttributeNameSriovNetSupport -- ^ sriovNetSupport
    | InstanceAttributeNameUserData -- ^ userData
      deriving (Eq, Show, Generic)

instance Hashable InstanceAttributeName

instance FromText InstanceAttributeName where
    parser = match "blockDeviceMapping" InstanceAttributeNameBlockDeviceMapping
         <|> match "disableApiTermination" InstanceAttributeNameDisableApiTermination
         <|> match "ebsOptimized" InstanceAttributeNameEbsOptimized
         <|> match "groupSet" InstanceAttributeNameGroupSet
         <|> match "instanceInitiatedShutdownBehavior" InstanceAttributeNameInstanceInitiatedShutdownBehavior
         <|> match "instanceType" InstanceAttributeNameInstanceType
         <|> match "kernel" InstanceAttributeNameKernel
         <|> match "productCodes" InstanceAttributeNameProductCodes
         <|> match "ramdisk" InstanceAttributeNameRamdisk
         <|> match "rootDeviceName" InstanceAttributeNameRootDeviceName
         <|> match "sourceDestCheck" InstanceAttributeNameSourceDestCheck
         <|> match "sriovNetSupport" InstanceAttributeNameSriovNetSupport
         <|> match "userData" InstanceAttributeNameUserData

instance ToText InstanceAttributeName where
    toText InstanceAttributeNameBlockDeviceMapping = "blockDeviceMapping"
    toText InstanceAttributeNameDisableApiTermination = "disableApiTermination"
    toText InstanceAttributeNameEbsOptimized = "ebsOptimized"
    toText InstanceAttributeNameGroupSet = "groupSet"
    toText InstanceAttributeNameInstanceInitiatedShutdownBehavior = "instanceInitiatedShutdownBehavior"
    toText InstanceAttributeNameInstanceType = "instanceType"
    toText InstanceAttributeNameKernel = "kernel"
    toText InstanceAttributeNameProductCodes = "productCodes"
    toText InstanceAttributeNameRamdisk = "ramdisk"
    toText InstanceAttributeNameRootDeviceName = "rootDeviceName"
    toText InstanceAttributeNameSourceDestCheck = "sourceDestCheck"
    toText InstanceAttributeNameSriovNetSupport = "sriovNetSupport"
    toText InstanceAttributeNameUserData = "userData"

instance ToByteString InstanceAttributeName

instance ToQuery InstanceAttributeName where
    toQuery = genericQuery def

-- | Indicates whether this is a Spot Instance.
data InstanceLifecycleType
    = InstanceLifecycleTypeSpot -- ^ spot
      deriving (Eq, Show, Generic)

instance Hashable InstanceLifecycleType

instance FromText InstanceLifecycleType where
    parser = match "spot" InstanceLifecycleTypeSpot

instance ToText InstanceLifecycleType where
    toText InstanceLifecycleTypeSpot = "spot"

instance ToByteString InstanceLifecycleType

instance FromXML InstanceLifecycleType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "instanceLifecycle"

instance ToQuery InstanceLifecycleType where
    toQuery = genericQuery def

-- | The current state of the instance.
data InstanceStateName
    = InstanceStateNamePending -- ^ pending
    | InstanceStateNameRunning -- ^ running
    | InstanceStateNameShuttingDown -- ^ shutting-down
    | InstanceStateNameStopped -- ^ stopped
    | InstanceStateNameStopping -- ^ stopping
    | InstanceStateNameTerminated -- ^ terminated
      deriving (Eq, Show, Generic)

instance Hashable InstanceStateName

instance FromText InstanceStateName where
    parser = match "pending" InstanceStateNamePending
         <|> match "running" InstanceStateNameRunning
         <|> match "shutting-down" InstanceStateNameShuttingDown
         <|> match "stopped" InstanceStateNameStopped
         <|> match "stopping" InstanceStateNameStopping
         <|> match "terminated" InstanceStateNameTerminated

instance ToText InstanceStateName where
    toText InstanceStateNamePending = "pending"
    toText InstanceStateNameRunning = "running"
    toText InstanceStateNameShuttingDown = "shutting-down"
    toText InstanceStateNameStopped = "stopped"
    toText InstanceStateNameStopping = "stopping"
    toText InstanceStateNameTerminated = "terminated"

instance ToByteString InstanceStateName

instance FromXML InstanceStateName where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "name"

instance ToQuery InstanceStateName where
    toQuery = genericQuery def

-- | The instance type. For more information, see Instance Types in the Amazon
-- Elastic Compute Cloud User Guide.
data InstanceType
    = C1Medium -- ^ c1.medium
    | C1Xlarge -- ^ c1.xlarge
    | C32Xlarge -- ^ c3.2xlarge
    | C34Xlarge -- ^ c3.4xlarge
    | C38Xlarge -- ^ c3.8xlarge
    | C3Large -- ^ c3.large
    | C3Xlarge -- ^ c3.xlarge
    | Cc14Xlarge -- ^ cc1.4xlarge
    | Cc28Xlarge -- ^ cc2.8xlarge
    | Cg14Xlarge -- ^ cg1.4xlarge
    | Cr18Xlarge -- ^ cr1.8xlarge
    | G22Xlarge -- ^ g2.2xlarge
    | Hi14Xlarge -- ^ hi1.4xlarge
    | Hs18Xlarge -- ^ hs1.8xlarge
    | I22Xlarge -- ^ i2.2xlarge
    | I24Xlarge -- ^ i2.4xlarge
    | I28Xlarge -- ^ i2.8xlarge
    | I2Xlarge -- ^ i2.xlarge
    | M1Large -- ^ m1.large
    | M1Medium -- ^ m1.medium
    | M1Small -- ^ m1.small
    | M1Xlarge -- ^ m1.xlarge
    | M22Xlarge -- ^ m2.2xlarge
    | M24Xlarge -- ^ m2.4xlarge
    | M2Xlarge -- ^ m2.xlarge
    | M32Xlarge -- ^ m3.2xlarge
    | M3Large -- ^ m3.large
    | M3Medium -- ^ m3.medium
    | M3Xlarge -- ^ m3.xlarge
    | R32Xlarge -- ^ r3.2xlarge
    | R34Xlarge -- ^ r3.4xlarge
    | R38Xlarge -- ^ r3.8xlarge
    | R3Large -- ^ r3.large
    | R3Xlarge -- ^ r3.xlarge
    | T1Micro -- ^ t1.micro
    | T2Medium -- ^ t2.medium
    | T2Micro -- ^ t2.micro
    | T2Small -- ^ t2.small
      deriving (Eq, Show, Generic)

instance Hashable InstanceType

instance FromText InstanceType where
    parser = match "c1.medium" C1Medium
         <|> match "c1.xlarge" C1Xlarge
         <|> match "c3.2xlarge" C32Xlarge
         <|> match "c3.4xlarge" C34Xlarge
         <|> match "c3.8xlarge" C38Xlarge
         <|> match "c3.large" C3Large
         <|> match "c3.xlarge" C3Xlarge
         <|> match "cc1.4xlarge" Cc14Xlarge
         <|> match "cc2.8xlarge" Cc28Xlarge
         <|> match "cg1.4xlarge" Cg14Xlarge
         <|> match "cr1.8xlarge" Cr18Xlarge
         <|> match "g2.2xlarge" G22Xlarge
         <|> match "hi1.4xlarge" Hi14Xlarge
         <|> match "hs1.8xlarge" Hs18Xlarge
         <|> match "i2.2xlarge" I22Xlarge
         <|> match "i2.4xlarge" I24Xlarge
         <|> match "i2.8xlarge" I28Xlarge
         <|> match "i2.xlarge" I2Xlarge
         <|> match "m1.large" M1Large
         <|> match "m1.medium" M1Medium
         <|> match "m1.small" M1Small
         <|> match "m1.xlarge" M1Xlarge
         <|> match "m2.2xlarge" M22Xlarge
         <|> match "m2.4xlarge" M24Xlarge
         <|> match "m2.xlarge" M2Xlarge
         <|> match "m3.2xlarge" M32Xlarge
         <|> match "m3.large" M3Large
         <|> match "m3.medium" M3Medium
         <|> match "m3.xlarge" M3Xlarge
         <|> match "r3.2xlarge" R32Xlarge
         <|> match "r3.4xlarge" R34Xlarge
         <|> match "r3.8xlarge" R38Xlarge
         <|> match "r3.large" R3Large
         <|> match "r3.xlarge" R3Xlarge
         <|> match "t1.micro" T1Micro
         <|> match "t2.medium" T2Medium
         <|> match "t2.micro" T2Micro
         <|> match "t2.small" T2Small

instance ToText InstanceType where
    toText C1Medium = "c1.medium"
    toText C1Xlarge = "c1.xlarge"
    toText C32Xlarge = "c3.2xlarge"
    toText C34Xlarge = "c3.4xlarge"
    toText C38Xlarge = "c3.8xlarge"
    toText C3Large = "c3.large"
    toText C3Xlarge = "c3.xlarge"
    toText Cc14Xlarge = "cc1.4xlarge"
    toText Cc28Xlarge = "cc2.8xlarge"
    toText Cg14Xlarge = "cg1.4xlarge"
    toText Cr18Xlarge = "cr1.8xlarge"
    toText G22Xlarge = "g2.2xlarge"
    toText Hi14Xlarge = "hi1.4xlarge"
    toText Hs18Xlarge = "hs1.8xlarge"
    toText I22Xlarge = "i2.2xlarge"
    toText I24Xlarge = "i2.4xlarge"
    toText I28Xlarge = "i2.8xlarge"
    toText I2Xlarge = "i2.xlarge"
    toText M1Large = "m1.large"
    toText M1Medium = "m1.medium"
    toText M1Small = "m1.small"
    toText M1Xlarge = "m1.xlarge"
    toText M22Xlarge = "m2.2xlarge"
    toText M24Xlarge = "m2.4xlarge"
    toText M2Xlarge = "m2.xlarge"
    toText M32Xlarge = "m3.2xlarge"
    toText M3Large = "m3.large"
    toText M3Medium = "m3.medium"
    toText M3Xlarge = "m3.xlarge"
    toText R32Xlarge = "r3.2xlarge"
    toText R34Xlarge = "r3.4xlarge"
    toText R38Xlarge = "r3.8xlarge"
    toText R3Large = "r3.large"
    toText R3Xlarge = "r3.xlarge"
    toText T1Micro = "t1.micro"
    toText T2Medium = "t2.medium"
    toText T2Micro = "t2.micro"
    toText T2Small = "t2.small"

instance ToByteString InstanceType

instance FromXML InstanceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "InstanceType"

instance ToQuery InstanceType where
    toQuery = genericQuery def

-- | The states of the listed Reserved Instances.
data ListingState
    = ListingStateAvailable -- ^ available
    | ListingStateCancelled -- ^ cancelled
    | ListingStatePending -- ^ pending
    | ListingStateSold -- ^ sold
      deriving (Eq, Show, Generic)

instance Hashable ListingState

instance FromText ListingState where
    parser = match "available" ListingStateAvailable
         <|> match "cancelled" ListingStateCancelled
         <|> match "pending" ListingStatePending
         <|> match "sold" ListingStateSold

instance ToText ListingState where
    toText ListingStateAvailable = "available"
    toText ListingStateCancelled = "cancelled"
    toText ListingStatePending = "pending"
    toText ListingStateSold = "sold"

instance ToByteString ListingState

instance FromXML ListingState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery ListingState where
    toQuery = genericQuery def

-- | The status of the Reserved Instance listing.
data ListingStatus
    = ListingStatusActive -- ^ active
    | ListingStatusCancelled -- ^ cancelled
    | ListingStatusClosed -- ^ closed
    | ListingStatusPending -- ^ pending
      deriving (Eq, Show, Generic)

instance Hashable ListingStatus

instance FromText ListingStatus where
    parser = match "active" ListingStatusActive
         <|> match "cancelled" ListingStatusCancelled
         <|> match "closed" ListingStatusClosed
         <|> match "pending" ListingStatusPending

instance ToText ListingStatus where
    toText ListingStatusActive = "active"
    toText ListingStatusCancelled = "cancelled"
    toText ListingStatusClosed = "closed"
    toText ListingStatusPending = "pending"

instance ToByteString ListingStatus

instance FromXML ListingStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery ListingStatus where
    toQuery = genericQuery def

-- | Indicates whether monitoring is enabled for the instance.
data MonitoringState
    = MonitoringStateDisabled -- ^ disabled
    | MonitoringStateEnabled -- ^ enabled
    | MonitoringStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance Hashable MonitoringState

instance FromText MonitoringState where
    parser = match "disabled" MonitoringStateDisabled
         <|> match "enabled" MonitoringStateEnabled
         <|> match "pending" MonitoringStatePending

instance ToText MonitoringState where
    toText MonitoringStateDisabled = "disabled"
    toText MonitoringStateEnabled = "enabled"
    toText MonitoringStatePending = "pending"

instance ToByteString MonitoringState

instance FromXML MonitoringState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery MonitoringState where
    toQuery = genericQuery def

-- | The attribute of the network interface.
data NetworkInterfaceAttribute
    = NetworkInterfaceAttributeAttachment -- ^ attachment
    | NetworkInterfaceAttributeDescription -- ^ description
    | NetworkInterfaceAttributeGroupSet -- ^ groupSet
    | NetworkInterfaceAttributeSourceDestCheck -- ^ sourceDestCheck
      deriving (Eq, Show, Generic)

instance Hashable NetworkInterfaceAttribute

instance FromText NetworkInterfaceAttribute where
    parser = match "attachment" NetworkInterfaceAttributeAttachment
         <|> match "description" NetworkInterfaceAttributeDescription
         <|> match "groupSet" NetworkInterfaceAttributeGroupSet
         <|> match "sourceDestCheck" NetworkInterfaceAttributeSourceDestCheck

instance ToText NetworkInterfaceAttribute where
    toText NetworkInterfaceAttributeAttachment = "attachment"
    toText NetworkInterfaceAttributeDescription = "description"
    toText NetworkInterfaceAttributeGroupSet = "groupSet"
    toText NetworkInterfaceAttributeSourceDestCheck = "sourceDestCheck"

instance ToByteString NetworkInterfaceAttribute

instance ToQuery NetworkInterfaceAttribute where
    toQuery = genericQuery def

-- | The status of the network interface.
data NetworkInterfaceStatus
    = NetworkInterfaceStatusAttaching -- ^ attaching
    | NetworkInterfaceStatusAvailable -- ^ available
    | NetworkInterfaceStatusDetaching -- ^ detaching
    | NetworkInterfaceStatusInUse -- ^ in-use
      deriving (Eq, Show, Generic)

instance Hashable NetworkInterfaceStatus

instance FromText NetworkInterfaceStatus where
    parser = match "attaching" NetworkInterfaceStatusAttaching
         <|> match "available" NetworkInterfaceStatusAvailable
         <|> match "detaching" NetworkInterfaceStatusDetaching
         <|> match "in-use" NetworkInterfaceStatusInUse

instance ToText NetworkInterfaceStatus where
    toText NetworkInterfaceStatusAttaching = "attaching"
    toText NetworkInterfaceStatusAvailable = "available"
    toText NetworkInterfaceStatusDetaching = "detaching"
    toText NetworkInterfaceStatusInUse = "in-use"

instance ToByteString NetworkInterfaceStatus

instance FromXML NetworkInterfaceStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery NetworkInterfaceStatus where
    toQuery = genericQuery def

-- | The Reserved Instance offering type.
data OfferingTypeValues
    = OfferingTypeValuesHeavyUtilization -- ^ Heavy Utilization
    | OfferingTypeValuesLightUtilization -- ^ Light Utilization
    | OfferingTypeValuesMediumUtilization -- ^ Medium Utilization
      deriving (Eq, Show, Generic)

instance Hashable OfferingTypeValues

instance FromText OfferingTypeValues where
    parser = match "Heavy Utilization" OfferingTypeValuesHeavyUtilization
         <|> match "Light Utilization" OfferingTypeValuesLightUtilization
         <|> match "Medium Utilization" OfferingTypeValuesMediumUtilization

instance ToText OfferingTypeValues where
    toText OfferingTypeValuesHeavyUtilization = "Heavy Utilization"
    toText OfferingTypeValuesLightUtilization = "Light Utilization"
    toText OfferingTypeValuesMediumUtilization = "Medium Utilization"

instance ToByteString OfferingTypeValues

instance FromXML OfferingTypeValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OfferingTypeValues"

instance ToQuery OfferingTypeValues where
    toQuery = genericQuery def

-- | The specific group that is to be added or removed from a volume's list of
-- create volume permissions.
data PermissionGroup
    = PermissionGroupAll -- ^ all
      deriving (Eq, Show, Generic)

instance Hashable PermissionGroup

instance FromText PermissionGroup where
    parser = match "all" PermissionGroupAll

instance ToText PermissionGroup where
    toText PermissionGroupAll = "all"

instance ToByteString PermissionGroup

instance FromXML PermissionGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "group"

instance ToQuery PermissionGroup where
    toQuery = genericQuery def

-- | The state of the placement group.
data PlacementGroupState
    = PlacementGroupStateAvailable -- ^ available
    | PlacementGroupStateDeleted -- ^ deleted
    | PlacementGroupStateDeleting -- ^ deleting
    | PlacementGroupStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance Hashable PlacementGroupState

instance FromText PlacementGroupState where
    parser = match "available" PlacementGroupStateAvailable
         <|> match "deleted" PlacementGroupStateDeleted
         <|> match "deleting" PlacementGroupStateDeleting
         <|> match "pending" PlacementGroupStatePending

instance ToText PlacementGroupState where
    toText PlacementGroupStateAvailable = "available"
    toText PlacementGroupStateDeleted = "deleted"
    toText PlacementGroupStateDeleting = "deleting"
    toText PlacementGroupStatePending = "pending"

instance ToByteString PlacementGroupState

instance FromXML PlacementGroupState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery PlacementGroupState where
    toQuery = genericQuery def

-- | The placement strategy.
data PlacementStrategy
    = PlacementStrategyCluster -- ^ cluster
      deriving (Eq, Show, Generic)

instance Hashable PlacementStrategy

instance FromText PlacementStrategy where
    parser = match "cluster" PlacementStrategyCluster

instance ToText PlacementStrategy where
    toText PlacementStrategyCluster = "cluster"

instance ToByteString PlacementStrategy

instance FromXML PlacementStrategy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PlacementStrategy"

instance ToQuery PlacementStrategy where
    toQuery = genericQuery def

-- | The instance operating system.
data PlatformValues
    = PlatformValuesWindows -- ^ Windows
      deriving (Eq, Show, Generic)

instance Hashable PlatformValues

instance FromText PlatformValues where
    parser = match "Windows" PlatformValuesWindows

instance ToText PlatformValues where
    toText PlatformValuesWindows = "Windows"

instance ToByteString PlatformValues

instance FromXML PlatformValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PlatformValues"

instance ToQuery PlatformValues where
    toQuery = genericQuery def

-- | The type of product code.
data ProductCodeValues
    = ProductCodeValuesDevpay -- ^ devpay
    | ProductCodeValuesMarketplace -- ^ marketplace
      deriving (Eq, Show, Generic)

instance Hashable ProductCodeValues

instance FromText ProductCodeValues where
    parser = match "devpay" ProductCodeValuesDevpay
         <|> match "marketplace" ProductCodeValuesMarketplace

instance ToText ProductCodeValues where
    toText ProductCodeValuesDevpay = "devpay"
    toText ProductCodeValuesMarketplace = "marketplace"

instance ToByteString ProductCodeValues

instance FromXML ProductCodeValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "type"

instance ToQuery ProductCodeValues where
    toQuery = genericQuery def

-- | A general description of the AMI.
data RIProductDescription
    = RIProductDescriptionLinuxUnix -- ^ Linux/UNIX
    | RIProductDescriptionLinuxUnixAmazonVpc -- ^ Linux/UNIX (Amazon VPC)
    | RIProductDescriptionWindows -- ^ Windows
    | RIProductDescriptionWindowsAmazonVpc -- ^ Windows (Amazon VPC)
      deriving (Eq, Show, Generic)

instance Hashable RIProductDescription

instance FromText RIProductDescription where
    parser = match "Linux/UNIX" RIProductDescriptionLinuxUnix
         <|> match "Linux/UNIX (Amazon VPC)" RIProductDescriptionLinuxUnixAmazonVpc
         <|> match "Windows" RIProductDescriptionWindows
         <|> match "Windows (Amazon VPC)" RIProductDescriptionWindowsAmazonVpc

instance ToText RIProductDescription where
    toText RIProductDescriptionLinuxUnix = "Linux/UNIX"
    toText RIProductDescriptionLinuxUnixAmazonVpc = "Linux/UNIX (Amazon VPC)"
    toText RIProductDescriptionWindows = "Windows"
    toText RIProductDescriptionWindowsAmazonVpc = "Windows (Amazon VPC)"

instance ToByteString RIProductDescription

instance FromXML RIProductDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "productDescription"

instance ToQuery RIProductDescription where
    toQuery = genericQuery def

-- | The frequency of the recurring charge.
data RecurringChargeFrequency
    = RecurringChargeFrequencyHourly -- ^ Hourly
      deriving (Eq, Show, Generic)

instance Hashable RecurringChargeFrequency

instance FromText RecurringChargeFrequency where
    parser = match "Hourly" RecurringChargeFrequencyHourly

instance ToText RecurringChargeFrequency where
    toText RecurringChargeFrequencyHourly = "Hourly"

instance ToByteString RecurringChargeFrequency

instance FromXML RecurringChargeFrequency where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "frequency"

instance ToQuery RecurringChargeFrequency where
    toQuery = genericQuery def

data ReportInstanceReasonCodes
    = ReportInstanceReasonCodesInstanceStuckInState -- ^ instance-stuck-in-state
    | ReportInstanceReasonCodesNotAcceptingCredentials -- ^ not-accepting-credentials
    | ReportInstanceReasonCodesOther -- ^ other
    | ReportInstanceReasonCodesPasswordNotAvailable -- ^ password-not-available
    | ReportInstanceReasonCodesPerformanceEbsVolume -- ^ performance-ebs-volume
    | ReportInstanceReasonCodesPerformanceInstanceStore -- ^ performance-instance-store
    | ReportInstanceReasonCodesPerformanceNetwork -- ^ performance-network
    | ReportInstanceReasonCodesPerformanceOther -- ^ performance-other
    | ReportInstanceReasonCodesUnresponsive -- ^ unresponsive
      deriving (Eq, Show, Generic)

instance Hashable ReportInstanceReasonCodes

instance FromText ReportInstanceReasonCodes where
    parser = match "instance-stuck-in-state" ReportInstanceReasonCodesInstanceStuckInState
         <|> match "not-accepting-credentials" ReportInstanceReasonCodesNotAcceptingCredentials
         <|> match "other" ReportInstanceReasonCodesOther
         <|> match "password-not-available" ReportInstanceReasonCodesPasswordNotAvailable
         <|> match "performance-ebs-volume" ReportInstanceReasonCodesPerformanceEbsVolume
         <|> match "performance-instance-store" ReportInstanceReasonCodesPerformanceInstanceStore
         <|> match "performance-network" ReportInstanceReasonCodesPerformanceNetwork
         <|> match "performance-other" ReportInstanceReasonCodesPerformanceOther
         <|> match "unresponsive" ReportInstanceReasonCodesUnresponsive

instance ToText ReportInstanceReasonCodes where
    toText ReportInstanceReasonCodesInstanceStuckInState = "instance-stuck-in-state"
    toText ReportInstanceReasonCodesNotAcceptingCredentials = "not-accepting-credentials"
    toText ReportInstanceReasonCodesOther = "other"
    toText ReportInstanceReasonCodesPasswordNotAvailable = "password-not-available"
    toText ReportInstanceReasonCodesPerformanceEbsVolume = "performance-ebs-volume"
    toText ReportInstanceReasonCodesPerformanceInstanceStore = "performance-instance-store"
    toText ReportInstanceReasonCodesPerformanceNetwork = "performance-network"
    toText ReportInstanceReasonCodesPerformanceOther = "performance-other"
    toText ReportInstanceReasonCodesUnresponsive = "unresponsive"

instance ToByteString ReportInstanceReasonCodes

instance ToQuery ReportInstanceReasonCodes where
    toQuery = genericQuery def

-- | The status of all instances listed.
data ReportStatusType
    = ReportStatusTypeImpaired -- ^ impaired
    | ReportStatusTypeOk -- ^ ok
      deriving (Eq, Show, Generic)

instance Hashable ReportStatusType

instance FromText ReportStatusType where
    parser = match "impaired" ReportStatusTypeImpaired
         <|> match "ok" ReportStatusTypeOk

instance ToText ReportStatusType where
    toText ReportStatusTypeImpaired = "impaired"
    toText ReportStatusTypeOk = "ok"

instance ToByteString ReportStatusType

instance ToQuery ReportStatusType where
    toQuery = genericQuery def

-- | The state of the Reserved Instance purchase.
data ReservedInstanceState
    = ReservedInstanceStateActive -- ^ active
    | ReservedInstanceStatePaymentFailed -- ^ payment-failed
    | ReservedInstanceStatePaymentPending -- ^ payment-pending
    | ReservedInstanceStateRetired -- ^ retired
      deriving (Eq, Show, Generic)

instance Hashable ReservedInstanceState

instance FromText ReservedInstanceState where
    parser = match "active" ReservedInstanceStateActive
         <|> match "payment-failed" ReservedInstanceStatePaymentFailed
         <|> match "payment-pending" ReservedInstanceStatePaymentPending
         <|> match "retired" ReservedInstanceStateRetired

instance ToText ReservedInstanceState where
    toText ReservedInstanceStateActive = "active"
    toText ReservedInstanceStatePaymentFailed = "payment-failed"
    toText ReservedInstanceStatePaymentPending = "payment-pending"
    toText ReservedInstanceStateRetired = "retired"

instance ToByteString ReservedInstanceState

instance FromXML ReservedInstanceState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery ReservedInstanceState where
    toQuery = genericQuery def

-- | The attribute to reset (currently you can only reset the launch permission
-- attribute).
data ResetImageAttributeName
    = ResetImageAttributeNameLaunchPermission -- ^ launchPermission
      deriving (Eq, Show, Generic)

instance Hashable ResetImageAttributeName

instance FromText ResetImageAttributeName where
    parser = match "launchPermission" ResetImageAttributeNameLaunchPermission

instance ToText ResetImageAttributeName where
    toText ResetImageAttributeNameLaunchPermission = "launchPermission"

instance ToByteString ResetImageAttributeName

instance ToQuery ResetImageAttributeName where
    toQuery = genericQuery def

-- | The type of resource.
data ResourceType
    = ResourceTypeCustomerGateway -- ^ customer-gateway
    | ResourceTypeDhcpOptions -- ^ dhcp-options
    | ResourceTypeImage -- ^ image
    | ResourceTypeInstance -- ^ instance
    | ResourceTypeInternetGateway -- ^ internet-gateway
    | ResourceTypeNetworkAcl -- ^ network-acl
    | ResourceTypeNetworkInterface -- ^ network-interface
    | ResourceTypeReservedInstances -- ^ reserved-instances
    | ResourceTypeRouteTable -- ^ route-table
    | ResourceTypeSecurityGroup -- ^ security-group
    | ResourceTypeSnapshot -- ^ snapshot
    | ResourceTypeSpotInstancesRequest -- ^ spot-instances-request
    | ResourceTypeSubnet -- ^ subnet
    | ResourceTypeVolume -- ^ volume
    | ResourceTypeVpc -- ^ vpc
    | ResourceTypeVpnConnection -- ^ vpn-connection
    | ResourceTypeVpnGateway -- ^ vpn-gateway
      deriving (Eq, Show, Generic)

instance Hashable ResourceType

instance FromText ResourceType where
    parser = match "customer-gateway" ResourceTypeCustomerGateway
         <|> match "dhcp-options" ResourceTypeDhcpOptions
         <|> match "image" ResourceTypeImage
         <|> match "instance" ResourceTypeInstance
         <|> match "internet-gateway" ResourceTypeInternetGateway
         <|> match "network-acl" ResourceTypeNetworkAcl
         <|> match "network-interface" ResourceTypeNetworkInterface
         <|> match "reserved-instances" ResourceTypeReservedInstances
         <|> match "route-table" ResourceTypeRouteTable
         <|> match "security-group" ResourceTypeSecurityGroup
         <|> match "snapshot" ResourceTypeSnapshot
         <|> match "spot-instances-request" ResourceTypeSpotInstancesRequest
         <|> match "subnet" ResourceTypeSubnet
         <|> match "volume" ResourceTypeVolume
         <|> match "vpc" ResourceTypeVpc
         <|> match "vpn-connection" ResourceTypeVpnConnection
         <|> match "vpn-gateway" ResourceTypeVpnGateway

instance ToText ResourceType where
    toText ResourceTypeCustomerGateway = "customer-gateway"
    toText ResourceTypeDhcpOptions = "dhcp-options"
    toText ResourceTypeImage = "image"
    toText ResourceTypeInstance = "instance"
    toText ResourceTypeInternetGateway = "internet-gateway"
    toText ResourceTypeNetworkAcl = "network-acl"
    toText ResourceTypeNetworkInterface = "network-interface"
    toText ResourceTypeReservedInstances = "reserved-instances"
    toText ResourceTypeRouteTable = "route-table"
    toText ResourceTypeSecurityGroup = "security-group"
    toText ResourceTypeSnapshot = "snapshot"
    toText ResourceTypeSpotInstancesRequest = "spot-instances-request"
    toText ResourceTypeSubnet = "subnet"
    toText ResourceTypeVolume = "volume"
    toText ResourceTypeVpc = "vpc"
    toText ResourceTypeVpnConnection = "vpn-connection"
    toText ResourceTypeVpnGateway = "vpn-gateway"

instance ToByteString ResourceType

instance FromXML ResourceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "resourceType"

instance ToQuery ResourceType where
    toQuery = genericQuery def

-- | Describes how the route was created. CreateRouteTable indicates that route
-- was automatically created when the route table was created. CreateRoute
-- indicates that the route was manually added to the route table.
-- EnableVgwRoutePropagation indicates that the route was propagated by route
-- propagation.
data RouteOrigin
    = RouteOriginCreateRoute -- ^ CreateRoute
    | RouteOriginCreateRouteTable -- ^ CreateRouteTable
    | RouteOriginEnableVgwRoutePropagation -- ^ EnableVgwRoutePropagation
      deriving (Eq, Show, Generic)

instance Hashable RouteOrigin

instance FromText RouteOrigin where
    parser = match "CreateRoute" RouteOriginCreateRoute
         <|> match "CreateRouteTable" RouteOriginCreateRouteTable
         <|> match "EnableVgwRoutePropagation" RouteOriginEnableVgwRoutePropagation

instance ToText RouteOrigin where
    toText RouteOriginCreateRoute = "CreateRoute"
    toText RouteOriginCreateRouteTable = "CreateRouteTable"
    toText RouteOriginEnableVgwRoutePropagation = "EnableVgwRoutePropagation"

instance ToByteString RouteOrigin

instance FromXML RouteOrigin where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "origin"

instance ToQuery RouteOrigin where
    toQuery = genericQuery def

-- | The state of the route. The blackhole state indicates that the route's
-- target isn't available (for example, the specified gateway isn't attached
-- to the VPC, or the specified NAT instance has been terminated).
data RouteState
    = RouteStateActive -- ^ active
    | RouteStateBlackhole -- ^ blackhole
      deriving (Eq, Show, Generic)

instance Hashable RouteState

instance FromText RouteState where
    parser = match "active" RouteStateActive
         <|> match "blackhole" RouteStateBlackhole

instance ToText RouteState where
    toText RouteStateActive = "active"
    toText RouteStateBlackhole = "blackhole"

instance ToByteString RouteState

instance FromXML RouteState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery RouteState where
    toQuery = genericQuery def

-- | Indicates whether to allow or deny the traffic that matches the rule.
data RuleAction
    = RuleActionAllow -- ^ allow
    | RuleActionDeny -- ^ deny
      deriving (Eq, Show, Generic)

instance Hashable RuleAction

instance FromText RuleAction where
    parser = match "allow" RuleActionAllow
         <|> match "deny" RuleActionDeny

instance ToText RuleAction where
    toText RuleActionAllow = "allow"
    toText RuleActionDeny = "deny"

instance ToByteString RuleAction

instance FromXML RuleAction where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ruleAction"

instance ToQuery RuleAction where
    toQuery = genericQuery def

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown).
data ShutdownBehavior
    = ShutdownBehaviorStop -- ^ stop
    | ShutdownBehaviorTerminate -- ^ terminate
      deriving (Eq, Show, Generic)

instance Hashable ShutdownBehavior

instance FromText ShutdownBehavior where
    parser = match "stop" ShutdownBehaviorStop
         <|> match "terminate" ShutdownBehaviorTerminate

instance ToText ShutdownBehavior where
    toText ShutdownBehaviorStop = "stop"
    toText ShutdownBehaviorTerminate = "terminate"

instance ToByteString ShutdownBehavior

instance ToQuery ShutdownBehavior where
    toQuery = genericQuery def

-- | The snapshot attribute you would like to view.
data SnapshotAttributeName
    = SnapshotAttributeNameCreateVolumePermission -- ^ createVolumePermission
    | SnapshotAttributeNameProductCodes -- ^ productCodes
      deriving (Eq, Show, Generic)

instance Hashable SnapshotAttributeName

instance FromText SnapshotAttributeName where
    parser = match "createVolumePermission" SnapshotAttributeNameCreateVolumePermission
         <|> match "productCodes" SnapshotAttributeNameProductCodes

instance ToText SnapshotAttributeName where
    toText SnapshotAttributeNameCreateVolumePermission = "createVolumePermission"
    toText SnapshotAttributeNameProductCodes = "productCodes"

instance ToByteString SnapshotAttributeName

instance ToQuery SnapshotAttributeName where
    toQuery = genericQuery def

-- | The snapshot state.
data SnapshotState
    = SnapshotStateCompleted -- ^ completed
    | SnapshotStateError -- ^ error
    | SnapshotStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance Hashable SnapshotState

instance FromText SnapshotState where
    parser = match "completed" SnapshotStateCompleted
         <|> match "error" SnapshotStateError
         <|> match "pending" SnapshotStatePending

instance ToText SnapshotState where
    toText SnapshotStateCompleted = "completed"
    toText SnapshotStateError = "error"
    toText SnapshotStatePending = "pending"

instance ToByteString SnapshotState

instance FromXML SnapshotState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery SnapshotState where
    toQuery = genericQuery def

-- | The state of the Spot Instance request. Spot bid status information can
-- help you track your Spot Instance requests. For information, see Tracking
-- Spot Requests with Bid Status Codes in the Amazon Elastic Compute Cloud
-- User Guide.
data SpotInstanceState
    = SpotInstanceStateActive -- ^ active
    | SpotInstanceStateCancelled -- ^ cancelled
    | SpotInstanceStateClosed -- ^ closed
    | SpotInstanceStateFailed -- ^ failed
    | SpotInstanceStateOpen -- ^ open
      deriving (Eq, Show, Generic)

instance Hashable SpotInstanceState

instance FromText SpotInstanceState where
    parser = match "active" SpotInstanceStateActive
         <|> match "cancelled" SpotInstanceStateCancelled
         <|> match "closed" SpotInstanceStateClosed
         <|> match "failed" SpotInstanceStateFailed
         <|> match "open" SpotInstanceStateOpen

instance ToText SpotInstanceState where
    toText SpotInstanceStateActive = "active"
    toText SpotInstanceStateCancelled = "cancelled"
    toText SpotInstanceStateClosed = "closed"
    toText SpotInstanceStateFailed = "failed"
    toText SpotInstanceStateOpen = "open"

instance ToByteString SpotInstanceState

instance FromXML SpotInstanceState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery SpotInstanceState where
    toQuery = genericQuery def

-- | The Spot Instance request type. Default: one-time.
data SpotInstanceType
    = SpotInstanceTypeOneTime -- ^ one-time
    | SpotInstanceTypePersistent -- ^ persistent
      deriving (Eq, Show, Generic)

instance Hashable SpotInstanceType

instance FromText SpotInstanceType where
    parser = match "one-time" SpotInstanceTypeOneTime
         <|> match "persistent" SpotInstanceTypePersistent

instance ToText SpotInstanceType where
    toText SpotInstanceTypeOneTime = "one-time"
    toText SpotInstanceTypePersistent = "persistent"

instance ToByteString SpotInstanceType

instance FromXML SpotInstanceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SpotInstanceType"

instance ToQuery SpotInstanceType where
    toQuery = genericQuery def

-- | The type of instance status.
data StatusName
    = StatusNameReachability -- ^ reachability
      deriving (Eq, Show, Generic)

instance Hashable StatusName

instance FromText StatusName where
    parser = match "reachability" StatusNameReachability

instance ToText StatusName where
    toText StatusNameReachability = "reachability"

instance ToByteString StatusName

instance FromXML StatusName where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "name"

instance ToQuery StatusName where
    toQuery = genericQuery def

-- | The status.
data StatusType
    = StatusTypeFailed -- ^ failed
    | StatusTypeInsufficientData -- ^ insufficient-data
    | StatusTypePassed -- ^ passed
      deriving (Eq, Show, Generic)

instance Hashable StatusType

instance FromText StatusType where
    parser = match "failed" StatusTypeFailed
         <|> match "insufficient-data" StatusTypeInsufficientData
         <|> match "passed" StatusTypePassed

instance ToText StatusType where
    toText StatusTypeFailed = "failed"
    toText StatusTypeInsufficientData = "insufficient-data"
    toText StatusTypePassed = "passed"

instance ToByteString StatusType

instance FromXML StatusType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery StatusType where
    toQuery = genericQuery def

-- | The current state of the subnet.
data SubnetState
    = SubnetStateAvailable -- ^ available
    | SubnetStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance Hashable SubnetState

instance FromText SubnetState where
    parser = match "available" SubnetStateAvailable
         <|> match "pending" SubnetStatePending

instance ToText SubnetState where
    toText SubnetStateAvailable = "available"
    toText SubnetStatePending = "pending"

instance ToByteString SubnetState

instance FromXML SubnetState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery SubnetState where
    toQuery = genericQuery def

-- | The status.
data SummaryStatus
    = SummaryStatusImpaired -- ^ impaired
    | SummaryStatusInsufficientData -- ^ insufficient-data
    | SummaryStatusNotApplicable -- ^ not-applicable
    | SummaryStatusOk -- ^ ok
      deriving (Eq, Show, Generic)

instance Hashable SummaryStatus

instance FromText SummaryStatus where
    parser = match "impaired" SummaryStatusImpaired
         <|> match "insufficient-data" SummaryStatusInsufficientData
         <|> match "not-applicable" SummaryStatusNotApplicable
         <|> match "ok" SummaryStatusOk

instance ToText SummaryStatus where
    toText SummaryStatusImpaired = "impaired"
    toText SummaryStatusInsufficientData = "insufficient-data"
    toText SummaryStatusNotApplicable = "not-applicable"
    toText SummaryStatusOk = "ok"

instance ToByteString SummaryStatus

instance FromXML SummaryStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery SummaryStatus where
    toQuery = genericQuery def

-- | The status of the VPN tunnel.
data TelemetryStatus
    = TelemetryStatusDown -- ^ DOWN
    | TelemetryStatusUp -- ^ UP
      deriving (Eq, Show, Generic)

instance Hashable TelemetryStatus

instance FromText TelemetryStatus where
    parser = match "DOWN" TelemetryStatusDown
         <|> match "UP" TelemetryStatusUp

instance ToText TelemetryStatus where
    toText TelemetryStatusDown = "DOWN"
    toText TelemetryStatusUp = "UP"

instance ToByteString TelemetryStatus

instance FromXML TelemetryStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery TelemetryStatus where
    toQuery = genericQuery def

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of dedicated runs on single-tenant hardware.
data Tenancy
    = TenancyDedicated -- ^ dedicated
    | TenancyDefault -- ^ default
      deriving (Eq, Show, Generic)

instance Hashable Tenancy

instance FromText Tenancy where
    parser = match "dedicated" TenancyDedicated
         <|> match "default" TenancyDefault

instance ToText Tenancy where
    toText TenancyDedicated = "dedicated"
    toText TenancyDefault = "default"

instance ToByteString Tenancy

instance FromXML Tenancy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tenancy"

instance ToQuery Tenancy where
    toQuery = genericQuery def

-- | The virtualization type of the instance.
data VirtualizationType
    = VirtualizationTypeHvm -- ^ hvm
    | VirtualizationTypeParavirtual -- ^ paravirtual
      deriving (Eq, Show, Generic)

instance Hashable VirtualizationType

instance FromText VirtualizationType where
    parser = match "hvm" VirtualizationTypeHvm
         <|> match "paravirtual" VirtualizationTypeParavirtual

instance ToText VirtualizationType where
    toText VirtualizationTypeHvm = "hvm"
    toText VirtualizationTypeParavirtual = "paravirtual"

instance ToByteString VirtualizationType

instance FromXML VirtualizationType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "virtualizationType"

instance ToQuery VirtualizationType where
    toQuery = genericQuery def

-- | The attachment state of the volume.
data VolumeAttachmentState
    = VolumeAttachmentStateAttached -- ^ attached
    | VolumeAttachmentStateAttaching -- ^ attaching
    | VolumeAttachmentStateDetached -- ^ detached
    | VolumeAttachmentStateDetaching -- ^ detaching
      deriving (Eq, Show, Generic)

instance Hashable VolumeAttachmentState

instance FromText VolumeAttachmentState where
    parser = match "attached" VolumeAttachmentStateAttached
         <|> match "attaching" VolumeAttachmentStateAttaching
         <|> match "detached" VolumeAttachmentStateDetached
         <|> match "detaching" VolumeAttachmentStateDetaching

instance ToText VolumeAttachmentState where
    toText VolumeAttachmentStateAttached = "attached"
    toText VolumeAttachmentStateAttaching = "attaching"
    toText VolumeAttachmentStateDetached = "detached"
    toText VolumeAttachmentStateDetaching = "detaching"

instance ToByteString VolumeAttachmentState

instance FromXML VolumeAttachmentState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery VolumeAttachmentState where
    toQuery = genericQuery def

-- | The instance attribute.
data VolumeAttributeName
    = VolumeAttributeNameAutoEnableIO -- ^ autoEnableIO
    | VolumeAttributeNameProductCodes -- ^ productCodes
      deriving (Eq, Show, Generic)

instance Hashable VolumeAttributeName

instance FromText VolumeAttributeName where
    parser = match "autoEnableIO" VolumeAttributeNameAutoEnableIO
         <|> match "productCodes" VolumeAttributeNameProductCodes

instance ToText VolumeAttributeName where
    toText VolumeAttributeNameAutoEnableIO = "autoEnableIO"
    toText VolumeAttributeNameProductCodes = "productCodes"

instance ToByteString VolumeAttributeName

instance ToQuery VolumeAttributeName where
    toQuery = genericQuery def

-- | The volume state.
data VolumeState
    = VolumeStateAvailable -- ^ available
    | VolumeStateCreating -- ^ creating
    | VolumeStateDeleted -- ^ deleted
    | VolumeStateDeleting -- ^ deleting
    | VolumeStateError -- ^ error
    | VolumeStateInUse -- ^ in-use
      deriving (Eq, Show, Generic)

instance Hashable VolumeState

instance FromText VolumeState where
    parser = match "available" VolumeStateAvailable
         <|> match "creating" VolumeStateCreating
         <|> match "deleted" VolumeStateDeleted
         <|> match "deleting" VolumeStateDeleting
         <|> match "error" VolumeStateError
         <|> match "in-use" VolumeStateInUse

instance ToText VolumeState where
    toText VolumeStateAvailable = "available"
    toText VolumeStateCreating = "creating"
    toText VolumeStateDeleted = "deleted"
    toText VolumeStateDeleting = "deleting"
    toText VolumeStateError = "error"
    toText VolumeStateInUse = "in-use"

instance ToByteString VolumeState

instance FromXML VolumeState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery VolumeState where
    toQuery = genericQuery def

-- | The status of the volume.
data VolumeStatusInfoStatus
    = VolumeStatusInfoStatusImpaired -- ^ impaired
    | VolumeStatusInfoStatusInsufficientData -- ^ insufficient-data
    | VolumeStatusInfoStatusOk -- ^ ok
      deriving (Eq, Show, Generic)

instance Hashable VolumeStatusInfoStatus

instance FromText VolumeStatusInfoStatus where
    parser = match "impaired" VolumeStatusInfoStatusImpaired
         <|> match "insufficient-data" VolumeStatusInfoStatusInsufficientData
         <|> match "ok" VolumeStatusInfoStatusOk

instance ToText VolumeStatusInfoStatus where
    toText VolumeStatusInfoStatusImpaired = "impaired"
    toText VolumeStatusInfoStatusInsufficientData = "insufficient-data"
    toText VolumeStatusInfoStatusOk = "ok"

instance ToByteString VolumeStatusInfoStatus

instance FromXML VolumeStatusInfoStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery VolumeStatusInfoStatus where
    toQuery = genericQuery def

-- | The name of the volume status.
data VolumeStatusName
    = VolumeStatusNameIoEnabled -- ^ io-enabled
    | VolumeStatusNameIoPerformance -- ^ io-performance
      deriving (Eq, Show, Generic)

instance Hashable VolumeStatusName

instance FromText VolumeStatusName where
    parser = match "io-enabled" VolumeStatusNameIoEnabled
         <|> match "io-performance" VolumeStatusNameIoPerformance

instance ToText VolumeStatusName where
    toText VolumeStatusNameIoEnabled = "io-enabled"
    toText VolumeStatusNameIoPerformance = "io-performance"

instance ToByteString VolumeStatusName

instance FromXML VolumeStatusName where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "name"

instance ToQuery VolumeStatusName where
    toQuery = genericQuery def

-- | The volume type. gp2 for General Purpose (SSD) volumes, io1 for Provisioned
-- IOPS (SSD) volumes, and standard for Magnetic volumes. Default: standard.
data VolumeType
    = VolumeTypeGp2 -- ^ gp2
    | VolumeTypeIo1 -- ^ io1
    | VolumeTypeStandard -- ^ standard
      deriving (Eq, Show, Generic)

instance Hashable VolumeType

instance FromText VolumeType where
    parser = match "gp2" VolumeTypeGp2
         <|> match "io1" VolumeTypeIo1
         <|> match "standard" VolumeTypeStandard

instance ToText VolumeType where
    toText VolumeTypeGp2 = "gp2"
    toText VolumeTypeIo1 = "io1"
    toText VolumeTypeStandard = "standard"

instance ToByteString VolumeType

instance FromXML VolumeType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VolumeType"

instance ToQuery VolumeType where
    toQuery = genericQuery def

-- | The VPC attribute.
data VpcAttributeName
    = VpcAttributeNameEnableDnsHostnames -- ^ enableDnsHostnames
    | VpcAttributeNameEnableDnsSupport -- ^ enableDnsSupport
      deriving (Eq, Show, Generic)

instance Hashable VpcAttributeName

instance FromText VpcAttributeName where
    parser = match "enableDnsHostnames" VpcAttributeNameEnableDnsHostnames
         <|> match "enableDnsSupport" VpcAttributeNameEnableDnsSupport

instance ToText VpcAttributeName where
    toText VpcAttributeNameEnableDnsHostnames = "enableDnsHostnames"
    toText VpcAttributeNameEnableDnsSupport = "enableDnsSupport"

instance ToByteString VpcAttributeName

instance ToQuery VpcAttributeName where
    toQuery = genericQuery def

-- | The current state of the VPC.
data VpcState
    = VpcStateAvailable -- ^ available
    | VpcStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance Hashable VpcState

instance FromText VpcState where
    parser = match "available" VpcStateAvailable
         <|> match "pending" VpcStatePending

instance ToText VpcState where
    toText VpcStateAvailable = "available"
    toText VpcStatePending = "pending"

instance ToByteString VpcState

instance FromXML VpcState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery VpcState where
    toQuery = genericQuery def

-- | The current state of the virtual private gateway.
data VpnState
    = VpnStateAvailable -- ^ available
    | VpnStateDeleted -- ^ deleted
    | VpnStateDeleting -- ^ deleting
    | VpnStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance Hashable VpnState

instance FromText VpnState where
    parser = match "available" VpnStateAvailable
         <|> match "deleted" VpnStateDeleted
         <|> match "deleting" VpnStateDeleting
         <|> match "pending" VpnStatePending

instance ToText VpnState where
    toText VpnStateAvailable = "available"
    toText VpnStateDeleted = "deleted"
    toText VpnStateDeleting = "deleting"
    toText VpnStatePending = "pending"

instance ToByteString VpnState

instance FromXML VpnState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery VpnState where
    toQuery = genericQuery def

-- | Indicates how the routes were provided.
data VpnStaticRouteSource
    = VpnStaticRouteSourceStatic -- ^ Static
      deriving (Eq, Show, Generic)

instance Hashable VpnStaticRouteSource

instance FromText VpnStaticRouteSource where
    parser = match "Static" VpnStaticRouteSourceStatic

instance ToText VpnStaticRouteSource where
    toText VpnStaticRouteSourceStatic = "Static"

instance ToByteString VpnStaticRouteSource

instance FromXML VpnStaticRouteSource where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "source"

instance ToQuery VpnStaticRouteSource where
    toQuery = genericQuery def

-- | Describes a value of an account attribute.
newtype AccountAttributeValue = AccountAttributeValue
    { _aaagAttributeValue :: Maybe Text
      -- ^ The value.
    } deriving (Show, Generic)

instance FromXML AccountAttributeValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery AccountAttributeValue where
    toQuery = genericQuery def

-- | 
newtype AttributeBooleanValue = AttributeBooleanValue
    { _abvValue :: Maybe Bool
      -- ^ 
    } deriving (Show, Generic)

instance FromXML AttributeBooleanValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AttributeBooleanValue"

instance ToQuery AttributeBooleanValue where
    toQuery = genericQuery def

-- | A description for the network interface.
newtype AttributeValue = AttributeValue
    { _awValue :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

instance FromXML AttributeValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AttributeValue"

instance ToQuery AttributeValue where
    toQuery = genericQuery def

-- | Describes a message about an Availability Zone.
newtype AvailabilityZoneMessage = AvailabilityZoneMessage
    { _azmMessage :: Maybe Text
      -- ^ The message about the Availability Zone.
    } deriving (Show, Generic)

instance FromXML AvailabilityZoneMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery AvailabilityZoneMessage where
    toQuery = genericQuery def

-- | Describes an IP range.
newtype IpRange = IpRange
    { _irCidrIp :: Maybe Text
      -- ^ The CIDR range. You can either specify a CIDR range or a source
      -- security group, not both.
    } deriving (Show, Generic)

instance FromXML IpRange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IpRange"

instance ToQuery IpRange where
    toQuery = genericQuery def

-- | The monitoring information for the instance.
newtype Monitoring = Monitoring
    { _mgState :: Maybe MonitoringState
      -- ^ Indicates whether monitoring is enabled for the instance.
    } deriving (Show, Generic)

instance FromXML Monitoring where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "monitoring"

instance ToQuery Monitoring where
    toQuery = genericQuery def

-- | Describes a virtual private gateway propagating route.
newtype PropagatingVgw = PropagatingVgw
    { _ppwGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway (VGW).
    } deriving (Show, Generic)

instance FromXML PropagatingVgw where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery PropagatingVgw where
    toQuery = genericQuery def

-- | Describes the ID of a Reserved Instance.
newtype ReservedInstancesId = ReservedInstancesId
    { _rijReservedInstancesId :: Maybe Text
      -- ^ The ID of the Reserved Instance.
    } deriving (Show, Generic)

instance FromXML ReservedInstancesId where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery ReservedInstancesId where
    toQuery = genericQuery def

-- | The monitoring for the instance.
newtype RunInstancesMonitoringEnabled = RunInstancesMonitoringEnabled
    { _rimeEnabled :: Bool
      -- ^ Indicates whether monitoring is enabled for the instance.
    } deriving (Show, Generic)

instance ToQuery RunInstancesMonitoringEnabled where
    toQuery = genericQuery def

-- | The Amazon S3 storage locations.
newtype Storage = Storage
    { _seS3 :: Maybe S3Storage
      -- ^ An Amazon S3 storage location.
    } deriving (Show, Generic)

instance FromXML Storage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "storage"

instance ToQuery Storage where
    toQuery = genericQuery def

-- | 
newtype VolumeDetail = VolumeDetail
    { _vdSize :: Integer
      -- ^ The size of the volume, in GiB.
    } deriving (Show, Generic)

instance FromXML VolumeDetail where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VolumeDetail"

instance ToQuery VolumeDetail where
    toQuery = genericQuery def

-- | The VPN connection options.
newtype VpnConnectionOptions = VpnConnectionOptions
    { _vcoStaticRoutesOnly :: Maybe Bool
      -- ^ Indicates whether the VPN connection uses static routes only.
      -- Static routes must be used for devices that don't support BGP.
    } deriving (Show, Generic)

instance FromXML VpnConnectionOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "options"

instance ToQuery VpnConnectionOptions where
    toQuery = genericQuery def

-- | Indicates whether the VPN connection requires static routes. If you are
-- creating a VPN connection for a device that does not support BGP, you must
-- specify true. Default: false.
newtype VpnConnectionOptionsSpecification = VpnConnectionOptionsSpecification
    { _vcosStaticRoutesOnly :: Maybe Bool
      -- ^ Indicates whether the VPN connection uses static routes only.
      -- Static routes must be used for devices that don't support BGP.
    } deriving (Show, Generic)

instance ToQuery VpnConnectionOptionsSpecification where
    toQuery = genericQuery def

-- | Describes an account attribute.
data AccountAttribute = AccountAttribute
    { _aaAttributeValues :: [AccountAttributeValue]
      -- ^ One or more values for the account attribute.
    , _aaAttributeName :: Maybe Text
      -- ^ The name of the account attribute.
    } deriving (Show, Generic)

instance FromXML AccountAttribute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes an Elastic IP address.
data Address = Address
    { _ayAssociationId :: Maybe Text
      -- ^ The ID representing the association of the address with an
      -- instance in a VPC.
    , _ayInstanceId :: Maybe Text
      -- ^ The ID of the instance the address is associated with (if any).
    , _ayNetworkInterfaceOwnerId :: Maybe Text
      -- ^ The ID of the AWS account that owns the network interface.
    , _ayAllocationId :: Maybe Text
      -- ^ The ID representing the allocation of the address for use with
      -- EC2-VPC.
    , _ayDomain :: Maybe DomainType
      -- ^ Indicates whether this Elastic IP address is for use with
      -- instances in EC2-Classic (standard) or instances in a VPC (vpc).
    , _ayNetworkInterfaceId :: Maybe Text
      -- ^ The ID of the network interface.
    , _ayPrivateIpAddress :: Maybe Text
      -- ^ The private IP address associated with the Elastic IP address.
    , _ayPublicIp :: Maybe Text
      -- ^ The Elastic IP address.
    } deriving (Show, Generic)

instance FromXML Address where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes an Availability Zone.
data AvailabilityZone = AvailabilityZone
    { _aaalState :: Maybe AvailabilityZoneState
      -- ^ The state of the Availability Zone.
    , _aaalRegionName :: Maybe Text
      -- ^ The name of the region.
    , _aaalZoneName :: Maybe Text
      -- ^ The name of the Availability Zone.
    , _aaalMessages :: [AvailabilityZoneMessage]
      -- ^ Any messages about the Availability Zone.
    } deriving (Show, Generic)

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a block device mapping.
data BlockDeviceMapping = BlockDeviceMapping
    { _bdmVirtualName :: Maybe Text
      -- ^ The virtual device name.
    , _bdmNoDevice :: Maybe Text
      -- ^ Suppresses the specified device included in the block device
      -- mapping of the AMI.
    , _bdmEbs :: Maybe EbsBlockDevice
      -- ^ Parameters used to automatically set up Amazon EBS volumes when
      -- the instance is launched.
    , _bdmDeviceName :: Maybe Text
      -- ^ The device name exposed to the instance (for example, /dev/sdh).
    } deriving (Show, Generic)

instance FromXML BlockDeviceMapping where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BlockDeviceMapping"

instance ToQuery BlockDeviceMapping where
    toQuery = genericQuery def

-- | The bundle task.
data BundleTask = BundleTask
    { _bxInstanceId :: Maybe Text
      -- ^ The ID of the instance associated with this bundle task.
    , _bxState :: Maybe BundleTaskState
      -- ^ The state of the task.
    , _bxProgress :: Maybe Text
      -- ^ The level of task completion, as a percent (for example, 20%).
    , _bxStartTime :: Maybe ISO8601
      -- ^ The time this task started.
    , _bxBundleId :: Maybe Text
      -- ^ The ID for this bundle task.
    , _bxStorage :: Maybe Storage
      -- ^ The Amazon S3 storage locations.
    , _bxUpdateTime :: Maybe ISO8601
      -- ^ The time of the most recent update for the task.
    , _bxBundleTaskError :: Maybe BundleTaskError
      -- ^ If the task fails, a description of the error.
    } deriving (Show, Generic)

instance FromXML BundleTask where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "bundleInstanceTask"

-- | If the task fails, a description of the error.
data BundleTaskError = BundleTaskError
    { _bteCode :: Maybe Text
      -- ^ The error code.
    , _bteMessage :: Maybe Text
      -- ^ The error message.
    } deriving (Show, Generic)

instance FromXML BundleTaskError where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "error"

instance ToQuery BundleTaskError where
    toQuery = genericQuery def

-- | Describes a request to cancel a Spot Instance.
data CancelledSpotInstanceRequest = CancelledSpotInstanceRequest
    { _csirState :: Maybe CancelSpotInstanceRequestState
      -- ^ The state of the Spot Instance request.
    , _csirSpotInstanceRequestId :: Maybe Text
      -- ^ The ID of the Spot Instance request.
    } deriving (Show, Generic)

instance FromXML CancelledSpotInstanceRequest where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | 
data ConversionTask = ConversionTask
    { _ctImportInstance :: Maybe ImportInstanceTaskDetails
      -- ^ If the task is for importing an instance, this contains
      -- information about the import instance task.
    , _ctState :: ConversionTaskState
      -- ^ The state of the conversion task.
    , _ctStatusMessage :: Maybe Text
      -- ^ The status message related to the conversion task.
    , _ctImportVolume :: Maybe ImportVolumeTaskDetails
      -- ^ If the task is for importing a volume, this contains information
      -- about the import volume task.
    , _ctConversionTaskId :: Text
      -- ^ The ID of the conversion task.
    , _ctExpirationTime :: Maybe Text
      -- ^ The time when the task expires. If the upload isn't complete
      -- before the expiration time, we automatically cancel the task.
    , _ctTags :: [Tag]
      -- ^ 
    } deriving (Show, Generic)

instance FromXML ConversionTask where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "conversionTask"

-- | 
data CreateVolumePermission = CreateVolumePermission
    { _cvpGroup :: Maybe PermissionGroup
      -- ^ The specific group that is to be added or removed from a volume's
      -- list of create volume permissions.
    , _cvpUserId :: Maybe Text
      -- ^ The specific AWS account ID that is to be added or removed from a
      -- volume's list of create volume permissions.
    } deriving (Show, Generic)

instance FromXML CreateVolumePermission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery CreateVolumePermission where
    toQuery = genericQuery def

-- | A JSON representation of the snapshot attribute modification.
data CreateVolumePermissionModifications = CreateVolumePermissionModifications
    { _cvpmRemove :: [CreateVolumePermission]
      -- ^ Removes a specific AWS account ID or group from a volume's list
      -- of create volume permissions.
    , _cvpmAdd :: [CreateVolumePermission]
      -- ^ Adds a specific AWS account ID or group to a volume's list of
      -- create volume permissions.
    } deriving (Show, Generic)

instance ToQuery CreateVolumePermissionModifications where
    toQuery = genericQuery def

-- | Information about the customer gateway.
data CustomerGateway = CustomerGateway
    { _cgState :: Maybe Text
      -- ^ The current state of the customer gateway.
    , _cgIpAddress :: Maybe Text
      -- ^ The Internet-routable IP address of the customer gateway's
      -- outside interface.
    , _cgBgpAsn :: Maybe Text
      -- ^ The customer gateway's Border Gateway Protocol (BGP) Autonomous
      -- System Number (ASN).
    , _cgCustomerGatewayId :: Maybe Text
      -- ^ The ID of the customer gateway.
    , _cgType :: Maybe Text
      -- ^ The type of VPN connection the customer gateway supports.
    , _cgTags :: [Tag]
      -- ^ Any tags assigned to the customer gateway.
    } deriving (Show, Generic)

instance FromXML CustomerGateway where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "customerGateway"

-- | Describes a DHCP configuration option.
data DhcpConfiguration = DhcpConfiguration
    { _dcValues :: [Text]
      -- ^ One or more values for the DHCP option.
    , _dcKey :: Maybe Text
      -- ^ The name of a DHCP option.
    } deriving (Show, Generic)

instance FromXML DhcpConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery DhcpConfiguration where
    toQuery = genericQuery def

-- | Describes a set of DHCP options.
data DhcpOptions = DhcpOptions
    { _doDhcpConfigurations :: [DhcpConfiguration]
      -- ^ One or more DHCP options in the set.
    , _doDhcpOptionsId :: Maybe Text
      -- ^ The ID of the set of DHCP options.
    , _doTags :: [Tag]
      -- ^ Any tags assigned to the DHCP options set.
    } deriving (Show, Generic)

instance FromXML DhcpOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a disk image.
data DiskImage = DiskImage
    { _diImage :: Maybe DiskImageDetail
      -- ^ 
    , _diVolume :: Maybe VolumeDetail
      -- ^ 
    , _diDescription :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

instance ToQuery DiskImage where
    toQuery = genericQuery def

-- | The image.
data DiskImageDescription = DiskImageDescription
    { _dieSize :: Integer
      -- ^ The size of the disk image.
    , _dieChecksum :: Maybe Text
      -- ^ The checksum computed for the disk image.
    , _dieFormat :: DiskImageFormat
      -- ^ The disk image format.
    , _dieImportManifestUrl :: Text
      -- ^ A presigned URL for the import manifest stored in Amazon S3. For
      -- information about creating a presigned URL for an Amazon S3
      -- object, read the "Query String Request Authentication
      -- Alternative" section of the Authenticating REST Requests topic in
      -- the Amazon Simple Storage Service Developer Guide.
    } deriving (Show, Generic)

instance FromXML DiskImageDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "image"

instance ToQuery DiskImageDescription where
    toQuery = genericQuery def

-- | 
data DiskImageDetail = DiskImageDetail
    { _didFormat :: DiskImageFormat
      -- ^ The disk image format.
    , _didImportManifestUrl :: Text
      -- ^ A presigned URL for the import manifest stored in Amazon S3. For
      -- information about creating a presigned URL for an Amazon S3
      -- object, read the "Query String Request Authentication
      -- Alternative" section of the Authenticating REST Requests topic in
      -- the Amazon Simple Storage Service Developer Guide.
    , _didBytes :: Integer
      -- ^ 
    } deriving (Show, Generic)

instance FromXML DiskImageDetail where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DiskImageDetail"

instance ToQuery DiskImageDetail where
    toQuery = genericQuery def

-- | The volume.
data DiskImageVolumeDescription = DiskImageVolumeDescription
    { _divdSize :: Maybe Integer
      -- ^ The size of the volume.
    , _divdId :: Text
      -- ^ The volume identifier.
    } deriving (Show, Generic)

instance FromXML DiskImageVolumeDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "volume"

instance ToQuery DiskImageVolumeDescription where
    toQuery = genericQuery def

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
data EbsBlockDevice = EbsBlockDevice
    { _ebdDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the Amazon EBS volume is deleted on instance
      -- termination.
    , _ebdVolumeSize :: Maybe Integer
      -- ^ The size of the volume, in GiB. Constraints: If the volume type
      -- is io1, the minimum size of the volume is 10 GiB; otherwise, the
      -- minimum size is 1 GiB. The maximum volume size is 1024 GiB.
      -- Default: If you're creating the volume from a snapshot and don't
      -- specify a volume size, the default is the snapshot size.
    , _ebdIops :: Maybe Integer
      -- ^ The number of I/O operations per second (IOPS) that the volume
      -- supports. For Provisioned IOPS (SSD) volumes, this represents the
      -- number of IOPS that are provisioned for the volume. For General
      -- Purpose (SSD) volumes, this represents the baseline performance
      -- of the volume and the rate at which the volume accumulates I/O
      -- credits for bursting. For more information on General Purpose
      -- (SSD) baseline performance, I/O credits, and bursting, see Amazon
      -- EBS Volume Types in the Amazon Elastic Compute Cloud User Guide.
      -- Constraint: Range is 100 to 4000 for Provisioned IOPS (SSD)
      -- volumes and 3 to 3072 for General Purpose (SSD) volumes.
      -- Condition: This parameter is required for requests to create io1
      -- volumes; it is not used in requests to create standard or gp2
      -- volumes.
    , _ebdEncrypted :: Maybe Bool
      -- ^ Indicates whether the Amazon EBS volume is encrypted.
    , _ebdVolumeType :: Maybe VolumeType
      -- ^ The volume type. gp2 for General Purpose (SSD) volumes, io1 for
      -- Provisioned IOPS (SSD) volumes, and standard for Magnetic
      -- volumes. Default: standard.
    , _ebdSnapshotId :: Maybe Text
      -- ^ The ID of the snapshot.
    } deriving (Show, Generic)

instance FromXML EbsBlockDevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EbsBlockDevice"

instance ToQuery EbsBlockDevice where
    toQuery = genericQuery def

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
data EbsInstanceBlockDevice = EbsInstanceBlockDevice
    { _eibdStatus :: Maybe AttachmentStatus
      -- ^ The attachment state.
    , _eibdDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the volume is deleted on instance termination.
    , _eibdVolumeId :: Maybe Text
      -- ^ The ID of the Amazon EBS volume.
    , _eibdAttachTime :: Maybe ISO8601
      -- ^ The time stamp when the attachment initiated.
    } deriving (Show, Generic)

instance FromXML EbsInstanceBlockDevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ebs"

instance ToQuery EbsInstanceBlockDevice where
    toQuery = genericQuery def

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
data EbsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification
    { _eibdsDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the volume is deleted on instance termination.
    , _eibdsVolumeId :: Maybe Text
      -- ^ The ID of the Amazon EBS volume.
    } deriving (Show, Generic)

instance FromXML EbsInstanceBlockDeviceSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EbsInstanceBlockDeviceSpecification"

instance ToQuery EbsInstanceBlockDeviceSpecification where
    toQuery = genericQuery def

-- | 
data ExportTask = ExportTask
    { _etExportTaskId :: Maybe Text
      -- ^ The ID of the export task.
    , _etState :: Maybe ExportTaskState
      -- ^ The state of the conversion task.
    , _etExportToS3Task :: Maybe ExportToS3Task
      -- ^ 
    , _etInstanceExportDetails :: Maybe InstanceExportDetails
      -- ^ The instance being exported.
    , _etStatusMessage :: Maybe Text
      -- ^ The status message related to the export task.
    , _etDescription :: Maybe Text
      -- ^ A description of the resource being exported.
    } deriving (Show, Generic)

instance FromXML ExportTask where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "exportTask"

-- | 
data ExportToS3Task = ExportToS3Task
    { _etstS3Key :: Maybe Text
      -- ^ 
    , _etstContainerFormat :: Maybe ContainerFormat
      -- ^ The container format used to combine disk images with metadata
      -- (such as OVF). If absent, only the disk image is exported.
    , _etstS3Bucket :: Maybe Text
      -- ^ The Amazon S3 bucket for the destination image. The destination
      -- bucket must exist and grant WRITE and READ_ACL permissions to the
      -- AWS account vm-import-export@amazon.com.
    , _etstDiskImageFormat :: Maybe DiskImageFormat
      -- ^ The format for the exported image.
    } deriving (Show, Generic)

instance FromXML ExportToS3Task where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "exportToS3"

instance ToQuery ExportToS3Task where
    toQuery = genericQuery def

-- | 
data ExportToS3TaskSpecification = ExportToS3TaskSpecification
    { _etstsContainerFormat :: Maybe ContainerFormat
      -- ^ 
    , _etstsS3Prefix :: Maybe Text
      -- ^ The image is written to a single object in the Amazon S3 bucket
      -- at the S3 key s3prefix + exportTaskId + '.' + diskImageFormat.
    , _etstsS3Bucket :: Maybe Text
      -- ^ 
    , _etstsDiskImageFormat :: Maybe DiskImageFormat
      -- ^ 
    } deriving (Show, Generic)

instance ToQuery ExportToS3TaskSpecification where
    toQuery = genericQuery def

-- | 
data Filter = Filter
    { _frValues :: [Text]
      -- ^ One or more filter values.
    , _frName :: Maybe Text
      -- ^ The name of the filter.
    } deriving (Show, Generic)

instance ToQuery Filter where
    toQuery = genericQuery def

-- | Describes a security group.
data GroupIdentifier = GroupIdentifier
    { _giGroupId :: Maybe Text
      -- ^ The ID of the security group.
    , _giGroupName :: Maybe Text
      -- ^ The name of the security group.
    } deriving (Show, Generic)

instance FromXML GroupIdentifier where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery GroupIdentifier where
    toQuery = genericQuery def

-- | The IAM instance profile associated with the instance.
data IamInstanceProfile = IamInstanceProfile
    { _iiieArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the instance profile.
    , _iiieId :: Maybe Text
      -- ^ The ID of the instance profile.
    } deriving (Show, Generic)

instance FromXML IamInstanceProfile where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "iamInstanceProfile"

instance ToQuery IamInstanceProfile where
    toQuery = genericQuery def

-- | The IAM instance profile.
data IamInstanceProfileSpecification = IamInstanceProfileSpecification
    { _iipsArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the instance profile.
    , _iipsName :: Maybe Text
      -- ^ The name of the instance profile.
    } deriving (Show, Generic)

instance FromXML IamInstanceProfileSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IamInstanceProfileSpecification"

instance ToQuery IamInstanceProfileSpecification where
    toQuery = genericQuery def

-- | ICMP protocol: The ICMP type and code.
data IcmpTypeCode = IcmpTypeCode
    { _itcCode :: Maybe Integer
      -- ^ The ICMP type. A value of -1 means all types.
    , _itcType :: Maybe Integer
      -- ^ The ICMP code. A value of -1 means all codes for the specified
      -- ICMP type.
    } deriving (Show, Generic)

instance FromXML IcmpTypeCode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "icmpTypeCode"

instance ToQuery IcmpTypeCode where
    toQuery = genericQuery def

-- | Describes an image.
data Image = Image
    { _ijState :: Maybe ImageState
      -- ^ The current state of the AMI. If the state is available, the
      -- image is successfully registered and can be used to launch an
      -- instance.
    , _ijVirtualizationType :: Maybe VirtualizationType
      -- ^ The type of virtualization of the AMI.
    , _ijHypervisor :: Maybe HypervisorType
      -- ^ The hypervisor type of the image.
    , _ijPlatform :: Maybe PlatformValues
      -- ^ The value is Windows for Windows AMIs; otherwise blank.
    , _ijImageLocation :: Maybe Text
      -- ^ The location of the AMI.
    , _ijImageOwnerAlias :: Maybe Text
      -- ^ The AWS account alias (for example, amazon, self) or the AWS
      -- account ID of the AMI owner.
    , _ijRamdiskId :: Maybe Text
      -- ^ The RAM disk associated with the image, if any. Only applicable
      -- for machine images.
    , _ijKernelId :: Maybe Text
      -- ^ The kernel associated with the image, if any. Only applicable for
      -- machine images.
    , _ijRootDeviceName :: Maybe Text
      -- ^ The device name of the root device (for example, /dev/sda1 or
      -- xvda).
    , _ijSriovNetSupport :: Maybe Text
      -- ^ Specifies whether enhanced networking is enabled.
    , _ijOwnerId :: Maybe Text
      -- ^ The AWS account ID of the image owner.
    , _ijImageType :: Maybe ImageTypeValues
      -- ^ The type of image.
    , _ijName :: Maybe Text
      -- ^ The name of the AMI that was provided during image creation.
    , _ijImageId :: Maybe Text
      -- ^ The ID of the AMI.
    , _ijArchitecture :: Maybe ArchitectureValues
      -- ^ The architecture of the image.
    , _ijProductCodes :: [ProductCode]
      -- ^ Any product codes associated with the AMI.
    , _ijStateReason :: Maybe StateReason
      -- ^ The reason for the state change.
    , _ijRootDeviceType :: Maybe DeviceType
      -- ^ The type of root device used by the AMI. The AMI can use an
      -- Amazon EBS volume or an instance store volume.
    , _ijDescription :: Maybe Text
      -- ^ The description of the AMI that was provided during image
      -- creation.
    , _ijBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ Any block device mapping entries.
    , _ijTags :: [Tag]
      -- ^ Any tags assigned to the image.
    , _ijPublic :: Maybe Bool
      -- ^ Indicates whether the image has public launch permissions. The
      -- value is true if this image has public launch permissions or
      -- false if it has only implicit and explicit launch permissions.
    } deriving (Show, Generic)

instance FromXML Image where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | 
data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification
    { _iilsAdditionalInfo :: Maybe Text
      -- ^ 
    , _iilsGroupNames :: [Text]
      -- ^ One or more security group names.
    , _iilsSubnetId :: Maybe Text
      -- ^ [EC2-VPC] The ID of the subnet to launch the instance into.
    , _iilsInstanceType :: Maybe InstanceType
      -- ^ The instance type. For more information, see Instance Types in
      -- the Amazon Elastic Compute Cloud User Guide.
    , _iilsUserData :: Maybe Text
      -- ^ User data to be made available to the instance.
    , _iilsMonitoring :: Maybe Bool
      -- ^ 
    , _iilsPrivateIpAddress :: Maybe Text
      -- ^ [EC2-VPC] Optionally, you can use this parameter to assign the
      -- instance a specific available IP address from the IP address
      -- range of the subnet.
    , _iilsInstanceInitiatedShutdownBehavior :: Maybe ShutdownBehavior
      -- ^ Indicates whether an instance stops or terminates when you
      -- initiate shutdown from the instance (using the operating system
      -- command for system shutdown).
    , _iilsArchitecture :: Maybe ArchitectureValues
      -- ^ The architecture of the instance.
    , _iilsPlacement :: Maybe Placement
      -- ^ 
    } deriving (Show, Generic)

instance ToQuery ImportInstanceLaunchSpecification where
    toQuery = genericQuery def

-- | If the task is for importing an instance, this contains information about
-- the import instance task.
data ImportInstanceTaskDetails = ImportInstanceTaskDetails
    { _iitdInstanceId :: Maybe Text
      -- ^ 
    , _iitdPlatform :: Maybe PlatformValues
      -- ^ The instance operating system.
    , _iitdVolumes :: [ImportInstanceVolumeDetailItem]
      -- ^ 
    , _iitdDescription :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

instance FromXML ImportInstanceTaskDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "importInstance"

instance ToQuery ImportInstanceTaskDetails where
    toQuery = genericQuery def

-- | Describes an import volume task.
data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem
    { _iivdiStatus :: Text
      -- ^ The status of the import of this particular disk image.
    , _iivdiBytesConverted :: Integer
      -- ^ The number of bytes converted so far.
    , _iivdiImage :: DiskImageDescription
      -- ^ The image.
    , _iivdiVolume :: DiskImageVolumeDescription
      -- ^ The volume.
    , _iivdiAvailabilityZone :: Text
      -- ^ The Availability Zone where the resulting instance will reside.
    , _iivdiStatusMessage :: Maybe Text
      -- ^ The status information or errors related to the disk image.
    , _iivdiDescription :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

instance FromXML ImportInstanceVolumeDetailItem where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery ImportInstanceVolumeDetailItem where
    toQuery = genericQuery def

-- | If the task is for importing a volume, this contains information about the
-- import volume task.
data ImportVolumeTaskDetails = ImportVolumeTaskDetails
    { _ivtdBytesConverted :: Integer
      -- ^ The number of bytes converted so far.
    , _ivtdImage :: DiskImageDescription
      -- ^ The image.
    , _ivtdVolume :: DiskImageVolumeDescription
      -- ^ The volume.
    , _ivtdAvailabilityZone :: Text
      -- ^ The Availability Zone where the resulting volume will reside.
    , _ivtdDescription :: Maybe Text
      -- ^ The description you provided when starting the import volume
      -- task.
    } deriving (Show, Generic)

instance FromXML ImportVolumeTaskDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "importVolume"

instance ToQuery ImportVolumeTaskDetails where
    toQuery = genericQuery def

-- | Describes an instance.
data Instance = Instance
    { _ieInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _ieState :: Maybe InstanceState
      -- ^ The current state of the instance.
    , _ieVirtualizationType :: Maybe VirtualizationType
      -- ^ The virtualization type of the instance.
    , _iePublicDnsName :: Maybe Text
      -- ^ The public DNS name assigned to the instance. This name is not
      -- available until the instance enters the running state.
    , _ieHypervisor :: Maybe HypervisorType
      -- ^ The hypervisor type of the instance.
    , _iePlatform :: Maybe PlatformValues
      -- ^ The value is Windows for Windows instances; otherwise blank.
    , _ieSecurityGroups :: [GroupIdentifier]
      -- ^ One or more security groups for the instance.
    , _ieClientToken :: Maybe Text
      -- ^ The idempotency token you provided when you launched the
      -- instance.
    , _ieSourceDestCheck :: Maybe Bool
      -- ^ Specifies whether to enable an instance launched in a VPC to
      -- perform NAT. This controls whether source/destination checking is
      -- enabled on the instance. A value of true means checking is
      -- enabled, and false means checking is disabled. The value must be
      -- false for the instance to perform NAT. For more information, see
      -- NAT Instances in the Amazon Virtual Private Cloud User Guide.
    , _ieVpcId :: Maybe Text
      -- ^ The ID of the VPC in which the instance is running.
    , _ieKeyName :: Maybe Text
      -- ^ The name of the key pair, if this instance was launched with an
      -- associated key pair.
    , _ieLaunchTime :: Maybe ISO8601
      -- ^ The time the instance was launched.
    , _ieNetworkInterfaces :: [InstanceNetworkInterface]
      -- ^ [EC2-VPC] One or more network interfaces for the instance.
    , _ieRamdiskId :: Maybe Text
      -- ^ The RAM disk associated with this instance.
    , _ieSubnetId :: Maybe Text
      -- ^ The ID of the subnet in which the instance is running.
    , _ieKernelId :: Maybe Text
      -- ^ The kernel associated with this instance.
    , _ieRootDeviceName :: Maybe Text
      -- ^ The root device name (for example, /dev/sda1).
    , _ieInstanceType :: Maybe InstanceType
      -- ^ The instance type.
    , _ieSriovNetSupport :: Maybe Text
      -- ^ Specifies whether enhanced networking is enabled.
    , _ieEbsOptimized :: Maybe Bool
      -- ^ Indicates whether the instance is optimized for EBS I/O. This
      -- optimization provides dedicated throughput to Amazon EBS and an
      -- optimized configuration stack to provide optimal I/O performance.
      -- This optimization isn't available with all instance types.
      -- Additional usage charges apply when using an EBS Optimized
      -- instance.
    , _ieMonitoring :: Maybe Monitoring
      -- ^ The monitoring information for the instance.
    , _ieStateTransitionReason :: Maybe Text
      -- ^ The reason for the most recent state transition. This might be an
      -- empty string.
    , _ieInstanceLifecycle :: Maybe InstanceLifecycleType
      -- ^ Indicates whether this is a Spot Instance.
    , _ieIamInstanceProfile :: Maybe IamInstanceProfile
      -- ^ The IAM instance profile associated with the instance.
    , _ieImageId :: Maybe Text
      -- ^ The ID of the AMI used to launch the instance.
    , _iePrivateIpAddress :: Maybe Text
      -- ^ The private IP address assigned to the instance.
    , _ieArchitecture :: Maybe ArchitectureValues
      -- ^ The architecture of the image.
    , _ieProductCodes :: [ProductCode]
      -- ^ The product codes attached to this instance.
    , _ieSpotInstanceRequestId :: Maybe Text
      -- ^ The ID of the Spot Instance request.
    , _iePrivateDnsName :: Maybe Text
      -- ^ The private DNS name assigned to the instance. This DNS name can
      -- only be used inside the Amazon EC2 network. This name is not
      -- available until the instance enters the running state.
    , _ieStateReason :: Maybe StateReason
      -- ^ The reason for the most recent state transition.
    , _ieRootDeviceType :: Maybe DeviceType
      -- ^ The root device type used by the AMI. The AMI can use an Amazon
      -- EBS volume or an instance store volume.
    , _ieBlockDeviceMappings :: [InstanceBlockDeviceMapping]
      -- ^ Any block device mapping entries for the instance.
    , _ieAmiLaunchIndex :: Maybe Integer
      -- ^ The AMI launch index, which can be used to find this instance in
      -- the launch group.
    , _iePublicIpAddress :: Maybe Text
      -- ^ The public IP address assigned to the instance.
    , _iePlacement :: Maybe Placement
      -- ^ The location where the instance launched.
    , _ieTags :: [Tag]
      -- ^ Any tags assigned to the instance.
    } deriving (Show, Generic)

instance FromXML Instance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery Instance where
    toQuery = genericQuery def

-- | Describes a block device mapping.
data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { _ibdmEbs :: Maybe EbsInstanceBlockDevice
      -- ^ Parameters used to automatically set up Amazon EBS volumes when
      -- the instance is launched.
    , _ibdmDeviceName :: Maybe Text
      -- ^ The device name exposed to the instance (for example, /dev/sdh).
    } deriving (Show, Generic)

instance FromXML InstanceBlockDeviceMapping where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceBlockDeviceMapping where
    toQuery = genericQuery def

-- | Describes a block device mapping entry.
data InstanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification
    { _ibdmsVirtualName :: Maybe Text
      -- ^ The virtual device name.
    , _ibdmsNoDevice :: Maybe Text
      -- ^ suppress the specified device included in the block device
      -- mapping.
    , _ibdmsEbs :: Maybe EbsInstanceBlockDeviceSpecification
      -- ^ Parameters used to automatically set up Amazon EBS volumes when
      -- the instance is launched.
    , _ibdmsDeviceName :: Maybe Text
      -- ^ The device name exposed to the instance (for example, /dev/sdh).
    } deriving (Show, Generic)

instance ToQuery InstanceBlockDeviceMappingSpecification where
    toQuery = genericQuery def

-- | Describes a Reserved Instance listing state.
data InstanceCount = InstanceCount
    { _icState :: Maybe ListingState
      -- ^ The states of the listed Reserved Instances.
    , _icInstanceCount :: Maybe Integer
      -- ^ he number of listed Reserved Instances in the state specified by
      -- the state.
    } deriving (Show, Generic)

instance FromXML InstanceCount where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceCount where
    toQuery = genericQuery def

-- | The instance being exported.
data InstanceExportDetails = InstanceExportDetails
    { _iedTargetEnvironment :: Maybe ExportEnvironment
      -- ^ The target virtualization environment.
    , _iedInstanceId :: Maybe Text
      -- ^ The ID of the resource being exported.
    } deriving (Show, Generic)

instance FromXML InstanceExportDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "instanceExport"

instance ToQuery InstanceExportDetails where
    toQuery = genericQuery def

-- | Describes the monitoring information of the instance.
data InstanceMonitoring = InstanceMonitoring
    { _inInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _inMonitoring :: Maybe Monitoring
      -- ^ The monitoring information.
    } deriving (Show, Generic)

instance FromXML InstanceMonitoring where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a network interface.
data InstanceNetworkInterface = InstanceNetworkInterface
    { _iniGroups :: [GroupIdentifier]
      -- ^ One or more security groups.
    , _iniStatus :: Maybe NetworkInterfaceStatus
      -- ^ The status of the network interface.
    , _iniPrivateIpAddresses :: [InstancePrivateIpAddress]
      -- ^ The private IP addresses associated with the network interface.
    , _iniSourceDestCheck :: Maybe Bool
      -- ^ Indicates whether to validate network traffic to or from this
      -- network interface.
    , _iniVpcId :: Maybe Text
      -- ^ The ID of the VPC.
    , _iniNetworkInterfaceId :: Maybe Text
      -- ^ The ID of the network interface.
    , _iniSubnetId :: Maybe Text
      -- ^ The ID of the subnet.
    , _iniAttachment :: Maybe InstanceNetworkInterfaceAttachment
      -- ^ The network interface attachment.
    , _iniOwnerId :: Maybe Text
      -- ^ The ID of the AWS account that created the network interface.
    , _iniPrivateIpAddress :: Maybe Text
      -- ^ The IP address of the network interface within the subnet.
    , _iniPrivateDnsName :: Maybe Text
      -- ^ The private DNS name.
    , _iniDescription :: Maybe Text
      -- ^ The description.
    , _iniAssociation :: Maybe InstanceNetworkInterfaceAssociation
      -- ^ The association information for an Elastic IP associated with the
      -- network interface.
    } deriving (Show, Generic)

instance FromXML InstanceNetworkInterface where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceNetworkInterface where
    toQuery = genericQuery def

-- | The association information for an Elastic IP address for the network
-- interface.
data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
    { _iniaPublicDnsName :: Maybe Text
      -- ^ The public DNS name.
    , _iniaIpOwnerId :: Maybe Text
      -- ^ The ID of the owner of the Elastic IP address.
    , _iniaPublicIp :: Maybe Text
      -- ^ The address of the Elastic IP address bound to the network
      -- interface.
    } deriving (Show, Generic)

instance FromXML InstanceNetworkInterfaceAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "association"

instance ToQuery InstanceNetworkInterfaceAssociation where
    toQuery = genericQuery def

-- | The network interface attachment.
data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { _inibStatus :: Maybe AttachmentStatus
      -- ^ The attachment state.
    , _inibDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the network interface is deleted when the
      -- instance is terminated.
    , _inibAttachmentId :: Maybe Text
      -- ^ The ID of the network interface attachment.
    , _inibAttachTime :: Maybe ISO8601
      -- ^ The time stamp when the attachment initiated.
    , _inibDeviceIndex :: Maybe Integer
      -- ^ The index of the device on the instance for the network interface
      -- attachment.
    } deriving (Show, Generic)

instance FromXML InstanceNetworkInterfaceAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "attachment"

instance ToQuery InstanceNetworkInterfaceAttachment where
    toQuery = genericQuery def

-- | Describes a network interface.
data InstanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification
    { _inisGroups :: [Text]
      -- ^ The IDs of the security groups for the network interface.
    , _inisPrivateIpAddresses :: [PrivateIpAddressSpecification]
      -- ^ One or more private IP addresses to assign to the network
      -- interface.
    , _inisDeleteOnTermination :: Maybe Bool
      -- ^ If set to true, the interface is deleted when the instance is
      -- terminated.
    , _inisAssociatePublicIpAddress :: Maybe Bool
      -- ^ Indicates whether to auto-assign a public IP address to an
      -- instance in a VPC. This public IP address can be assigned to the
      -- network interface for eth0 only when you launch the instance. You
      -- must create the network interface instead of using an existing
      -- network interface for eth0, and you must not specify more than
      -- one network interface.
    , _inisNetworkInterfaceId :: Maybe Text
      -- ^ The ID of the network interface.
    , _inisSubnetId :: Maybe Text
      -- ^ The ID of the subnet associated with the network string.
    , _inisPrivateIpAddress :: Maybe Text
      -- ^ The private IP address of the network interface.
    , _inisSecondaryPrivateIpAddressCount :: Maybe Integer
      -- ^ The number of secondary private IP addresses.
    , _inisDescription :: Maybe Text
      -- ^ The description of the network interface.
    , _inisDeviceIndex :: Maybe Integer
      -- ^ The index of the device on the instance for the network interface
      -- attachment.
    } deriving (Show, Generic)

instance FromXML InstanceNetworkInterfaceSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NetworkInterface"

instance ToQuery InstanceNetworkInterfaceSpecification where
    toQuery = genericQuery def

-- | Describes a private IP address.
data InstancePrivateIpAddress = InstancePrivateIpAddress
    { _ipiaPrimary :: Maybe Bool
      -- ^ Indicates whether this IP address is the primary private IP
      -- address of the network interface.
    , _ipiaPrivateIpAddress :: Maybe Text
      -- ^ The private IP address of the network interface.
    , _ipiaPrivateDnsName :: Maybe Text
      -- ^ The private DNS name.
    , _ipiaAssociation :: Maybe InstanceNetworkInterfaceAssociation
      -- ^ The association information for an Elastic IP address for the
      -- network interface.
    } deriving (Show, Generic)

instance FromXML InstancePrivateIpAddress where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstancePrivateIpAddress where
    toQuery = genericQuery def

-- | The current state of the instance.
data InstanceState = InstanceState
    { _iihName :: Maybe InstanceStateName
      -- ^ The current state of the instance.
    , _iihCode :: Maybe Integer
      -- ^ The low byte represents the state. The high byte is an opaque
      -- internal value and should be ignored. 0 : pending 16 : running 32
      -- : shutting-down 48 : terminated 64 : stopping 80 : stopped.
    } deriving (Show, Generic)

instance FromXML InstanceState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "currentState"

instance ToQuery InstanceState where
    toQuery = genericQuery def

-- | Describes an instance state change.
data InstanceStateChange = InstanceStateChange
    { _iscInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _iscCurrentState :: Maybe InstanceState
      -- ^ The current state of the instance.
    , _iscPreviousState :: Maybe InstanceState
      -- ^ The previous state of the instance.
    } deriving (Show, Generic)

instance FromXML InstanceStateChange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes the status of an instance.
data InstanceStatus = InstanceStatus
    { _iiiizInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _iiiizSystemStatus :: Maybe InstanceStatusSummary
      -- ^ Reports impaired functionality that stems from issues related to
      -- the systems that support an instance, such as hardware failures
      -- and network connectivity problems.
    , _iiiizEvents :: [InstanceStatusEvent]
      -- ^ Extra information regarding events associated with the instance.
    , _iiiizAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone of the instance.
    , _iiiizInstanceStatus :: Maybe InstanceStatusSummary
      -- ^ Reports impaired functionality that stems from issues internal to
      -- the instance, such as impaired reachability.
    , _iiiizInstanceState :: Maybe InstanceState
      -- ^ The intended state of the instance. DescribeInstanceStatus
      -- requires that an instance be in the running state.
    } deriving (Show, Generic)

instance FromXML InstanceStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes the instance status.
data InstanceStatusDetails = InstanceStatusDetails
    { _isdStatus :: Maybe StatusType
      -- ^ The status.
    , _isdImpairedSince :: Maybe ISO8601
      -- ^ The time when a status check failed. For an instance that was
      -- launched and impaired, this is the time when the instance was
      -- launched.
    , _isdName :: Maybe StatusName
      -- ^ The type of instance status.
    } deriving (Show, Generic)

instance FromXML InstanceStatusDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceStatusDetails where
    toQuery = genericQuery def

-- | Describes an instance event.
data InstanceStatusEvent = InstanceStatusEvent
    { _iseNotBefore :: Maybe ISO8601
      -- ^ The earliest scheduled start time for the event.
    , _iseCode :: Maybe EventCode
      -- ^ The associated code of the event.
    , _iseDescription :: Maybe Text
      -- ^ A description of the event.
    , _iseNotAfter :: Maybe ISO8601
      -- ^ The latest scheduled end time for the event.
    } deriving (Show, Generic)

instance FromXML InstanceStatusEvent where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceStatusEvent where
    toQuery = genericQuery def

-- | Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
data InstanceStatusSummary = InstanceStatusSummary
    { _issStatus :: Maybe SummaryStatus
      -- ^ The status.
    , _issDetails :: [InstanceStatusDetails]
      -- ^ The system instance health or application instance health.
    } deriving (Show, Generic)

instance FromXML InstanceStatusSummary where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "systemStatus"

instance ToQuery InstanceStatusSummary where
    toQuery = genericQuery def

-- | Information about the Internet gateway.
data InternetGateway = InternetGateway
    { _igAttachments :: [InternetGatewayAttachment]
      -- ^ Any VPCs attached to the Internet gateway.
    , _igInternetGatewayId :: Maybe Text
      -- ^ The ID of the Internet gateway.
    , _igTags :: [Tag]
      -- ^ Any tags assigned to the Internet gateway.
    } deriving (Show, Generic)

instance FromXML InternetGateway where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "internetGateway"

-- | Describes the attachment of a VPC to an Internet gateway.
data InternetGatewayAttachment = InternetGatewayAttachment
    { _igaState :: Maybe AttachmentStatus
      -- ^ The current state of the attachment.
    , _igaVpcId :: Maybe Text
      -- ^ The ID of the VPC.
    } deriving (Show, Generic)

instance FromXML InternetGatewayAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InternetGatewayAttachment where
    toQuery = genericQuery def

-- | Describes a security group rule.
data IpPermission = IpPermission
    { _ipFromPort :: Maybe Integer
      -- ^ The start of port range for the TCP and UDP protocols, or an ICMP
      -- type number. A value of -1 indicates all ICMP types.
    , _ipUserIdGroupPairs :: [UserIdGroupPair]
      -- ^ One or more security group and AWS account ID pairs.
    , _ipIpProtocol :: Maybe Text
      -- ^ The protocol. When you call DescribeSecurityGroups, the protocol
      -- value returned is the number. Exception: For TCP, UDP, and ICMP,
      -- the value returned is the name (for example, tcp, udp, or icmp).
      -- For a list of protocol numbers, see Protocol Numbers.
    , _ipToPort :: Maybe Integer
      -- ^ The end of port range for the TCP and UDP protocols, or an ICMP
      -- code. A value of -1 indicates all ICMP codes for the specified
      -- ICMP type.
    , _ipIpRanges :: [IpRange]
      -- ^ One or more IP ranges.
    } deriving (Show, Generic)

instance FromXML IpPermission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IpPermission"

instance ToQuery IpPermission where
    toQuery = genericQuery def

-- | Describes a key pair.
data KeyPairInfo = KeyPairInfo
    { _kpiKeyFingerprint :: Maybe Text
      -- ^ If you used CreateKeyPair to create the key pair, this is the
      -- SHA-1 digest of the DER encoded private key. If you used
      -- ImportKeyPair to provide AWS the public key, this is the MD5
      -- public key fingerprint as specified in section 4 of RFC4716.
    , _kpiKeyName :: Maybe Text
      -- ^ The name of the key pair.
    } deriving (Show, Generic)

instance FromXML KeyPairInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a launch permission.
data LaunchPermission = LaunchPermission
    { _lvGroup :: Maybe PermissionGroup
      -- ^ The name of the group.
    , _lvUserId :: Maybe Text
      -- ^ The AWS account ID.
    } deriving (Show, Generic)

instance FromXML LaunchPermission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LaunchPermission"

instance ToQuery LaunchPermission where
    toQuery = genericQuery def

-- | 
data LaunchPermissionModifications = LaunchPermissionModifications
    { _lpmRemove :: [LaunchPermission]
      -- ^ The AWS account ID to remove from the list of launch permissions
      -- for the AMI.
    , _lpmAdd :: [LaunchPermission]
      -- ^ The AWS account ID to add to the list of launch permissions for
      -- the AMI.
    } deriving (Show, Generic)

instance ToQuery LaunchPermissionModifications where
    toQuery = genericQuery def

-- | The launch specification.
data LaunchSpecification = LaunchSpecification
    { _lxSecurityGroupIds :: [Text]
    , _lxSecurityGroups :: [Text]
    , _lxKeyName :: Maybe Text
      -- ^ The name of the key pair.
    , _lxNetworkInterfaces :: [InstanceNetworkInterfaceSpecification]
      -- ^ One or more network interfaces.
    , _lxRamdiskId :: Maybe Text
      -- ^ The ID of the RAM disk.
    , _lxSubnetId :: Maybe Text
      -- ^ The ID of the subnet in which to launch the Spot Instance.
    , _lxKernelId :: Maybe Text
      -- ^ The ID of the kernel.
    , _lxInstanceType :: Maybe InstanceType
      -- ^ The instance type.
    , _lxEbsOptimized :: Maybe Bool
      -- ^ Indicates whether the instance is optimized for EBS I/O. This
      -- optimization provides dedicated throughput to Amazon EBS and an
      -- optimized configuration stack to provide optimal EBS I/O
      -- performance. This optimization isn't available with all instance
      -- types. Additional usage charges apply when using an EBS Optimized
      -- instance. Default: false.
    , _lxUserData :: Maybe Text
      -- ^ The Base64-encoded MIME user data to make available to the
      -- instances.
    , _lxMonitoring :: Maybe Monitoring
    , _lxIamInstanceProfile :: Maybe IamInstanceProfileSpecification
      -- ^ The IAM instance profile.
    , _lxImageId :: Maybe Text
      -- ^ The ID of the AMI.
    , _lxAddressingType :: Maybe Text
      -- ^ 
    , _lxBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ One or more block device mapping entries.
    , _lxPlacement :: Maybe SpotPlacement
      -- ^ The placement information for the instance.
    } deriving (Show, Generic)

instance FromXML LaunchSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LaunchSpecification"

instance ToQuery LaunchSpecification where
    toQuery = genericQuery def

-- | Information about the network ACL.
data NetworkAcl = NetworkAcl
    { _naEntries :: [NetworkAclEntry]
      -- ^ One or more entries (rules) in the network ACL.
    , _naNetworkAclId :: Maybe Text
      -- ^ The ID of the network ACL.
    , _naVpcId :: Maybe Text
      -- ^ The ID of the VPC for the network ACL.
    , _naAssociations :: [NetworkAclAssociation]
      -- ^ Any associations between the network ACL and one or more subnets.
    , _naTags :: [Tag]
      -- ^ Any tags assigned to the network ACL.
    , _naIsDefault :: Maybe Bool
      -- ^ Indicates whether this is the default network ACL for the VPC.
    } deriving (Show, Generic)

instance FromXML NetworkAcl where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "networkAcl"

-- | Describes an association between a network ACL and a subnet.
data NetworkAclAssociation = NetworkAclAssociation
    { _naaNetworkAclId :: Maybe Text
      -- ^ The ID of the network ACL.
    , _naaSubnetId :: Maybe Text
      -- ^ The ID of the subnet.
    , _naaNetworkAclAssociationId :: Maybe Text
      -- ^ The ID of the association between a network ACL and a subnet.
    } deriving (Show, Generic)

instance FromXML NetworkAclAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery NetworkAclAssociation where
    toQuery = genericQuery def

-- | Describes an entry in a network ACL.
data NetworkAclEntry = NetworkAclEntry
    { _naeIcmpTypeCode :: Maybe IcmpTypeCode
      -- ^ ICMP protocol: The ICMP type and code.
    , _naeRuleNumber :: Maybe Integer
      -- ^ The rule number for the entry. ACL entries are processed in
      -- ascending order by rule number.
    , _naeRuleAction :: Maybe RuleAction
      -- ^ Indicates whether to allow or deny the traffic that matches the
      -- rule.
    , _naeProtocol :: Maybe Text
      -- ^ The protocol. A value of -1 means all protocols.
    , _naePortRange :: Maybe PortRange
      -- ^ TCP or UDP protocols: The range of ports the rule applies to.
    , _naeCidrBlock :: Maybe Text
      -- ^ The network range to allow or deny, in CIDR notation.
    , _naeEgress :: Maybe Bool
      -- ^ Indicates whether the rule is an egress rule (applied to traffic
      -- leaving the subnet).
    } deriving (Show, Generic)

instance FromXML NetworkAclEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery NetworkAclEntry where
    toQuery = genericQuery def

-- | Describes a network interface.
data NetworkInterface = NetworkInterface
    { _niGroups :: [GroupIdentifier]
      -- ^ Any security groups for the network interface.
    , _niStatus :: Maybe NetworkInterfaceStatus
      -- ^ The status of the network interface.
    , _niPrivateIpAddresses :: [NetworkInterfacePrivateIpAddress]
      -- ^ The private IP addresses associated with the network interface.
    , _niSourceDestCheck :: Maybe Bool
      -- ^ Indicates whether traffic to or from the instance is validated.
    , _niVpcId :: Maybe Text
      -- ^ The ID of the VPC.
    , _niTagSet :: [Tag]
      -- ^ Any tags assigned to the network interface.
    , _niRequesterManaged :: Maybe Bool
      -- ^ Indicates whether the network interface is being managed by AWS.
    , _niNetworkInterfaceId :: Maybe Text
      -- ^ The ID of the network interface.
    , _niSubnetId :: Maybe Text
      -- ^ The ID of the subnet.
    , _niMacAddress :: Maybe Text
      -- ^ The MAC address.
    , _niAttachment :: Maybe NetworkInterfaceAttachment
      -- ^ The network interface attachment.
    , _niOwnerId :: Maybe Text
      -- ^ The AWS account ID of the owner of the network interface.
    , _niAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone.
    , _niPrivateIpAddress :: Maybe Text
      -- ^ The IP address of the network interface within the subnet.
    , _niPrivateDnsName :: Maybe Text
      -- ^ The private DNS name.
    , _niRequesterId :: Maybe Text
      -- ^ The ID of the entity that launched the instance on your behalf
      -- (for example, AWS Management Console or Auto Scaling).
    , _niDescription :: Maybe Text
      -- ^ A description.
    , _niAssociation :: Maybe NetworkInterfaceAssociation
      -- ^ The association information for an Elastic IP associated with the
      -- network interface.
    } deriving (Show, Generic)

instance FromXML NetworkInterface where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The association information for an Elastic IP address associated with the
-- network interface.
data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { _niaAssociationId :: Maybe Text
      -- ^ The association ID.
    , _niaPublicDnsName :: Maybe Text
      -- ^ The public DNS name.
    , _niaAllocationId :: Maybe Text
      -- ^ The allocation ID.
    , _niaIpOwnerId :: Maybe Text
      -- ^ The ID of the Elastic IP address owner.
    , _niaPublicIp :: Maybe Text
      -- ^ The address of the Elastic IP address bound to the network
      -- interface.
    } deriving (Show, Generic)

instance FromXML NetworkInterfaceAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "association"

instance ToQuery NetworkInterfaceAssociation where
    toQuery = genericQuery def

-- | The network interface attachment.
data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { _nibInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _nibStatus :: Maybe AttachmentStatus
      -- ^ The attachment state.
    , _nibDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the network interface is deleted when the
      -- instance is terminated.
    , _nibAttachmentId :: Maybe Text
      -- ^ The ID of the network interface attachment.
    , _nibInstanceOwnerId :: Maybe Text
      -- ^ The AWS account ID of the owner of the instance.
    , _nibAttachTime :: Maybe ISO8601
      -- ^ The timestamp indicating when the attachment initiated.
    , _nibDeviceIndex :: Maybe Integer
      -- ^ The device index of the network interface attachment on the
      -- instance.
    } deriving (Show, Generic)

instance FromXML NetworkInterfaceAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "attachment"

instance ToQuery NetworkInterfaceAttachment where
    toQuery = genericQuery def

-- | The ID of the interface attachment.
data NetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges
    { _niacDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the network interface is deleted when the
      -- instance is terminated.
    , _niacAttachmentId :: Maybe Text
      -- ^ The ID of the network interface attachment.
    } deriving (Show, Generic)

instance ToQuery NetworkInterfaceAttachmentChanges where
    toQuery = genericQuery def

-- | Describes the private IP address of a network interface.
data NetworkInterfacePrivateIpAddress = NetworkInterfacePrivateIpAddress
    { _nipiaPrimary :: Maybe Bool
      -- ^ Indicates whether this IP address is the primary private IP
      -- address of the network interface.
    , _nipiaPrivateIpAddress :: Maybe Text
      -- ^ The private IP address.
    , _nipiaPrivateDnsName :: Maybe Text
      -- ^ The private DNS name.
    , _nipiaAssociation :: Maybe NetworkInterfaceAssociation
      -- ^ The association information for an Elastic IP address associated
      -- with the network interface.
    } deriving (Show, Generic)

instance FromXML NetworkInterfacePrivateIpAddress where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery NetworkInterfacePrivateIpAddress where
    toQuery = genericQuery def

-- | 
data Placement = Placement
    { _pAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone of the instance.
    , _pTenancy :: Maybe Tenancy
      -- ^ The tenancy of the instance (if the instance is running in a
      -- VPC). An instance with a tenancy of dedicated runs on
      -- single-tenant hardware.
    , _pGroupName :: Maybe Text
      -- ^ The name of the placement group the instance is in (for cluster
      -- compute instances).
    } deriving (Show, Generic)

instance FromXML Placement where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Placement"

instance ToQuery Placement where
    toQuery = genericQuery def

-- | Describes a placement group.
data PlacementGroup = PlacementGroup
    { _plState :: Maybe PlacementGroupState
      -- ^ The state of the placement group.
    , _plStrategy :: Maybe PlacementStrategy
      -- ^ The placement strategy.
    , _plGroupName :: Maybe Text
      -- ^ The name of the placement group.
    } deriving (Show, Generic)

instance FromXML PlacementGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | TCP or UDP protocols: The range of ports the rule applies to.
data PortRange = PortRange
    { _prTo :: Maybe Integer
      -- ^ The last port in the range.
    , _prFrom :: Maybe Integer
      -- ^ The first port in the range.
    } deriving (Show, Generic)

instance FromXML PortRange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "portRange"

instance ToQuery PortRange where
    toQuery = genericQuery def

-- | Describes the price for a Reserved Instance.
data PriceSchedule = PriceSchedule
    { _psCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ The currency for transacting the Reserved Instance resale. At
      -- this time, the only supported currency is USD.
    , _psTerm :: Maybe Integer
      -- ^ The number of months remaining in the reservation. For example, 2
      -- is the second to the last month before the capacity reservation
      -- expires.
    , _psActive :: Maybe Bool
      -- ^ The current price schedule, as determined by the term remaining
      -- for the Reserved Instance in the listing. A specific price
      -- schedule is always in effect, but only one price schedule can be
      -- active at any time. Take, for example, a Reserved Instance
      -- listing that has five months remaining in its term. When you
      -- specify price schedules for five months and two months, this
      -- means that schedule 1, covering the first three months of the
      -- remaining term, will be active during months 5, 4, and 3. Then
      -- schedule 2, covering the last two months of the term, will be
      -- active for months 2 and 1.
    , _psPrice :: Maybe Double
      -- ^ The fixed price for the term.
    } deriving (Show, Generic)

instance FromXML PriceSchedule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery PriceSchedule where
    toQuery = genericQuery def

-- | Describes the price for a Reserved Instance.
data PriceScheduleSpecification = PriceScheduleSpecification
    { _pssCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ The currency for transacting the Reserved Instance resale. At
      -- this time, the only supported currency is USD.
    , _pssTerm :: Maybe Integer
      -- ^ The number of months remaining in the reservation. For example, 2
      -- is the second to the last month before the capacity reservation
      -- expires.
    , _pssPrice :: Maybe Double
      -- ^ The fixed price for the term.
    } deriving (Show, Generic)

instance ToQuery PriceScheduleSpecification where
    toQuery = genericQuery def

-- | Describes a Reserved Instance offering.
data PricingDetail = PricingDetail
    { _peCount :: Maybe Integer
      -- ^ The number of instances available for the price.
    , _pePrice :: Maybe Double
      -- ^ The price per instance.
    } deriving (Show, Generic)

instance FromXML PricingDetail where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery PricingDetail where
    toQuery = genericQuery def

-- | Describes a secondary private IP address for a network interface.
data PrivateIpAddressSpecification = PrivateIpAddressSpecification
    { _piasPrimary :: Maybe Bool
      -- ^ Indicates whether the private IP address is the primary private
      -- IP address.
    , _piasPrivateIpAddress :: Text
      -- ^ The private IP addresses.
    } deriving (Show, Generic)

instance FromXML PrivateIpAddressSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PrivateIpAddresses"

instance ToQuery PrivateIpAddressSpecification where
    toQuery = genericQuery def

-- | Describes a product code.
data ProductCode = ProductCode
    { _pcProductCodeType :: Maybe ProductCodeValues
      -- ^ The type of product code.
    , _pcProductCodeId :: Maybe Text
      -- ^ The product code.
    } deriving (Show, Generic)

instance FromXML ProductCode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery ProductCode where
    toQuery = genericQuery def

-- | Describes a recurring charge.
data RecurringCharge = RecurringCharge
    { _rcAmount :: Maybe Double
      -- ^ The amount of the recurring charge.
    , _rcFrequency :: Maybe RecurringChargeFrequency
      -- ^ The frequency of the recurring charge.
    } deriving (Show, Generic)

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery RecurringCharge where
    toQuery = genericQuery def

-- | Describes a region.
data Region = Region
    { _rrRegionName :: Maybe Text
      -- ^ The name of the region.
    , _rrEndpoint :: Maybe Text
      -- ^ The region service endpoint.
    } deriving (Show, Generic)

instance FromXML Region where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a reservation.
data Reservation = Reservation
    { _rpGroups :: [GroupIdentifier]
      -- ^ One or more security groups.
    , _rpOwnerId :: Maybe Text
      -- ^ The ID of the AWS account that owns the reservation.
    , _rpInstances :: [Instance]
      -- ^ One or more instances.
    , _rpReservationId :: Maybe Text
      -- ^ The ID of the reservation.
    , _rpRequesterId :: Maybe Text
      -- ^ The ID of the requester that launched the instances on your
      -- behalf (for example, AWS Management Console or Auto Scaling).
    } deriving (Show, Generic)

instance FromXML Reservation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Specified for Reserved Instance Marketplace offerings to limit the total
-- order and ensure that the Reserved Instances are not purchased at
-- unexpected prices.
data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice
    { _rilpAmount :: Maybe Double
      -- ^ Used for Reserved Instance Marketplace offerings. Specifies the
      -- limit price on the total order (instanceCount * price).
    , _rilpCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ The currency in which the limitPrice amount is specified. At this
      -- time, the only supported currency is USD.
    } deriving (Show, Generic)

instance ToQuery ReservedInstanceLimitPrice where
    toQuery = genericQuery def

-- | Describes a Reserved Instance.
data ReservedInstances = ReservedInstances
    { _riState :: Maybe ReservedInstanceState
      -- ^ The state of the Reserved Instance purchase.
    , _riCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ The currency of the Reserved Instance. It's specified using ISO
      -- 4217 standard currency codes. At this time, the only supported
      -- currency is USD.
    , _riInstanceCount :: Maybe Integer
      -- ^ The number of Reserved Instances purchased.
    , _riProductDescription :: Maybe RIProductDescription
      -- ^ The Reserved Instance description.
    , _riStart :: Maybe ISO8601
      -- ^ The date and time the Reserved Instance started.
    , _riInstanceType :: Maybe InstanceType
      -- ^ The instance type on which the Reserved Instance can be used.
    , _riEnd :: Maybe ISO8601
      -- ^ The time when the Reserved Instance expires.
    , _riAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the Reserved Instance can be used.
    , _riRecurringCharges :: [RecurringCharge]
      -- ^ The recurring charge tag assigned to the resource.
    , _riOfferingType :: Maybe OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , _riUsagePrice :: Maybe Double
      -- ^ The usage price of the Reserved Instance, per hour.
    , _riFixedPrice :: Maybe Double
      -- ^ The purchase price of the Reserved Instance.
    , _riReservedInstancesId :: Maybe Text
      -- ^ The ID of the Reserved Instance.
    , _riInstanceTenancy :: Maybe Tenancy
      -- ^ The tenancy of the reserved instance.
    , _riDuration :: Maybe Integer
      -- ^ The duration of the Reserved Instance, in seconds.
    , _riTags :: [Tag]
      -- ^ Any tags assigned to the resource.
    } deriving (Show, Generic)

instance FromXML ReservedInstances where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes the configuration settings for the modified Reserved Instances.
data ReservedInstancesConfiguration = ReservedInstancesConfiguration
    { _ricPlatform :: Maybe Text
      -- ^ The network platform of the modified Reserved Instances, which is
      -- either EC2-Classic or EC2-VPC.
    , _ricInstanceCount :: Maybe Integer
      -- ^ The number of modified Reserved Instances.
    , _ricInstanceType :: Maybe InstanceType
      -- ^ The instance type for the modified Reserved Instances.
    , _ricAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone for the modified Reserved Instances.
    } deriving (Show, Generic)

instance FromXML ReservedInstancesConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedInstancesConfigurationSetItemType"

instance ToQuery ReservedInstancesConfiguration where
    toQuery = genericQuery def

-- | Describes a Reserved Instance listing.
data ReservedInstancesListing = ReservedInstancesListing
    { _rilStatus :: Maybe ListingStatus
      -- ^ The status of the Reserved Instance listing.
    , _rilClientToken :: Maybe Text
      -- ^ The idempotency token you provided when you created the listing.
    , _rilUpdateDate :: Maybe ISO8601
      -- ^ The last modified timestamp of the listing.
    , _rilCreateDate :: Maybe ISO8601
      -- ^ The time the listing was created.
    , _rilPriceSchedules :: [PriceSchedule]
      -- ^ The price of the Reserved Instance listing.
    , _rilStatusMessage :: Maybe Text
      -- ^ The reason for the current status of the Reserved Instance
      -- listing. The response can be blank.
    , _rilReservedInstancesId :: Maybe Text
      -- ^ The ID of the Reserved Instance.
    , _rilTags :: [Tag]
      -- ^ Any tags assigned to the resource.
    , _rilInstanceCounts :: [InstanceCount]
      -- ^ The number of instances in this state.
    , _rilReservedInstancesListingId :: Maybe Text
      -- ^ The ID of the Reserved Instance listing.
    } deriving (Show, Generic)

instance FromXML ReservedInstancesListing where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a Reserved Instance modification.
data ReservedInstancesModification = ReservedInstancesModification
    { _rixModificationResults :: [ReservedInstancesModificationResult]
      -- ^ Contains target configurations along with their corresponding new
      -- Reserved Instance IDs.
    , _rixStatus :: Maybe Text
      -- ^ The status of the Reserved Instances modification request.
    , _rixClientToken :: Maybe Text
      -- ^ A unique, case-sensitive key supplied by the client to ensure
      -- that the modification request is idempotent.
    , _rixUpdateDate :: Maybe ISO8601
      -- ^ The time when the modification request was last updated.
    , _rixCreateDate :: Maybe ISO8601
      -- ^ The time when the modification request was created.
    , _rixEffectiveDate :: Maybe ISO8601
      -- ^ The time for the modification to become effective.
    , _rixStatusMessage :: Maybe Text
      -- ^ The reason for the status.
    , _rixReservedInstancesModificationId :: Maybe Text
      -- ^ A unique ID for the Reserved Instance modification.
    , _rixReservedInstancesIds :: [ReservedInstancesId]
      -- ^ The IDs of one or more Reserved Instances.
    } deriving (Show, Generic)

instance FromXML ReservedInstancesModification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | 
data ReservedInstancesModificationResult = ReservedInstancesModificationResult
    { _rimrReservedInstancesId :: Maybe Text
      -- ^ The ID for the Reserved Instances that were created as part of
      -- the modification request. This field is only available when the
      -- modification is fulfilled.
    , _rimrTargetConfiguration :: Maybe ReservedInstancesConfiguration
      -- ^ The target Reserved Instances configurations supplied as part of
      -- the modification request.
    } deriving (Show, Generic)

instance FromXML ReservedInstancesModificationResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery ReservedInstancesModificationResult where
    toQuery = genericQuery def

-- | Describes a Reserved Instance offering.
data ReservedInstancesOffering = ReservedInstancesOffering
    { _rioMarketplace :: Maybe Bool
      -- ^ Indicates whether the offering is available through the Reserved
      -- Instance Marketplace (resale) or AWS. If it's a Reserved Instance
      -- Marketplace offering, this is true.
    , _rioCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ The currency of the Reserved Instance offering you are
      -- purchasing. It's specified using ISO 4217 standard currency
      -- codes. At this time, the only supported currency is USD.
    , _rioProductDescription :: Maybe RIProductDescription
      -- ^ The Reserved Instance description.
    , _rioInstanceType :: Maybe InstanceType
      -- ^ The instance type on which the Reserved Instance can be used.
    , _rioAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the Reserved Instance can be used.
    , _rioPricingDetails :: [PricingDetail]
      -- ^ The pricing details of the Reserved Instance offering.
    , _rioRecurringCharges :: [RecurringCharge]
      -- ^ The recurring charge tag assigned to the resource.
    , _rioOfferingType :: Maybe OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , _rioUsagePrice :: Maybe Double
      -- ^ The usage price of the Reserved Instance, per hour.
    , _rioFixedPrice :: Maybe Double
      -- ^ The purchase price of the Reserved Instance.
    , _rioInstanceTenancy :: Maybe Tenancy
      -- ^ The tenancy of the reserved instance.
    , _rioReservedInstancesOfferingId :: Maybe Text
      -- ^ The ID of the Reserved Instance offering.
    , _rioDuration :: Maybe Integer
      -- ^ The duration of the Reserved Instance, in seconds.
    } deriving (Show, Generic)

instance FromXML ReservedInstancesOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a route in a route table.
data Route = Route
    { _reVpcPeeringConnectionId :: Maybe Text
      -- ^ The ID of the VPC peering connection.
    , _reInstanceId :: Maybe Text
      -- ^ The ID of a NAT instance in your VPC.
    , _reOrigin :: Maybe RouteOrigin
      -- ^ Describes how the route was created. CreateRouteTable indicates
      -- that route was automatically created when the route table was
      -- created. CreateRoute indicates that the route was manually added
      -- to the route table. EnableVgwRoutePropagation indicates that the
      -- route was propagated by route propagation.
    , _reState :: Maybe RouteState
      -- ^ The state of the route. The blackhole state indicates that the
      -- route's target isn't available (for example, the specified
      -- gateway isn't attached to the VPC, or the specified NAT instance
      -- has been terminated).
    , _reNetworkInterfaceId :: Maybe Text
      -- ^ The ID of the network interface.
    , _reGatewayId :: Maybe Text
      -- ^ The ID of a gateway attached to your VPC.
    , _reInstanceOwnerId :: Maybe Text
      -- ^ The AWS account ID of the owner of the instance.
    , _reDestinationCidrBlock :: Maybe Text
      -- ^ The CIDR block used for the destination match.
    } deriving (Show, Generic)

instance FromXML Route where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery Route where
    toQuery = genericQuery def

-- | Information about the route table.
data RouteTable = RouteTable
    { _ruRouteTableId :: Maybe Text
      -- ^ The ID of the route table.
    , _ruRoutes :: [Route]
      -- ^ The routes in the route table.
    , _ruVpcId :: Maybe Text
      -- ^ The ID of the VPC.
    , _ruPropagatingVgws :: [PropagatingVgw]
      -- ^ Any virtual private gateway (VGW) propagating routes.
    , _ruAssociations :: [RouteTableAssociation]
      -- ^ The associations between the route table and one or more subnets.
    , _ruTags :: [Tag]
      -- ^ Any tags assigned to the route table.
    } deriving (Show, Generic)

instance FromXML RouteTable where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "routeTable"

-- | Describes an association between a route table and a subnet.
data RouteTableAssociation = RouteTableAssociation
    { _rtaRouteTableId :: Maybe Text
      -- ^ The ID of the route table.
    , _rtaRouteTableAssociationId :: Maybe Text
      -- ^ The ID of the association between a route table and a subnet.
    , _rtaMain :: Maybe Bool
      -- ^ Indicates whether this is the main route table.
    , _rtaSubnetId :: Maybe Text
      -- ^ The ID of the subnet.
    } deriving (Show, Generic)

instance FromXML RouteTableAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery RouteTableAssociation where
    toQuery = genericQuery def

-- | An Amazon S3 storage location.
data S3Storage = S3Storage
    { _ssPrefix :: Maybe Text
      -- ^ The beginning of the file name of the AMI.
    , _ssUploadPolicy :: Maybe Text
      -- ^ A Base64-encoded Amazon S3 upload policy that gives Amazon EC2
      -- permission to upload items into Amazon S3 on your behalf.
    , _ssBucket :: Maybe Text
      -- ^ The bucket in which to store the AMI. You can specify a bucket
      -- that you already own or a new bucket that Amazon EC2 creates on
      -- your behalf. If you specify a bucket that belongs to someone
      -- else, Amazon EC2 returns an error.
    , _ssUploadPolicySignature :: Maybe Text
      -- ^ The signature of the Base64 encoded JSON document.
    , _ssAWSAccessKeyId :: Maybe Text
      -- ^ The access key ID of the owner of the bucket. Before you specify
      -- a value for your access key ID, review and follow the guidance in
      -- Best Practices for Managing AWS Access Keys.
    } deriving (Show, Generic)

instance FromXML S3Storage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "S3Storage"

instance ToQuery S3Storage where
    toQuery = genericQuery def

-- | Describes a security group.
data SecurityGroup = SecurityGroup
    { _siVpcId :: Maybe Text
      -- ^ [EC2-VPC] The ID of the VPC for the security group.
    , _siIpPermissions :: [IpPermission]
      -- ^ One or more inbound rules associated with the security group.
    , _siOwnerId :: Maybe Text
      -- ^ The AWS account ID of the owner of the security group.
    , _siIpPermissionsEgress :: [IpPermission]
      -- ^ [EC2-VPC] One or more outbound rules associated with the security
      -- group.
    , _siGroupId :: Maybe Text
      -- ^ The ID of the security group.
    , _siGroupName :: Maybe Text
      -- ^ The name of the security group.
    , _siDescription :: Maybe Text
      -- ^ A description of the security group.
    , _siTags :: [Tag]
      -- ^ Any tags assigned to the security group.
    } deriving (Show, Generic)

instance FromXML SecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a snapshot.
data Snapshot = Snapshot
    { _ssxState :: Maybe SnapshotState
      -- ^ The snapshot state.
    , _ssxOwnerAlias :: Maybe Text
      -- ^ The AWS account alias (for example, amazon, self) or AWS account
      -- ID that owns the snapshot.
    , _ssxProgress :: Maybe Text
      -- ^ The progress of the snapshot, as a percentage.
    , _ssxStartTime :: Maybe ISO8601
      -- ^ The time stamp when the snapshot was initiated.
    , _ssxVolumeSize :: Maybe Integer
      -- ^ The size of the volume, in GiB.
    , _ssxEncrypted :: Maybe Bool
      -- ^ Indicates whether the snapshot is encrypted.
    , _ssxOwnerId :: Maybe Text
      -- ^ The AWS account ID of the Amazon EBS snapshot owner.
    , _ssxVolumeId :: Maybe Text
      -- ^ The ID of the volume.
    , _ssxDescription :: Maybe Text
      -- ^ The description for the snapshot.
    , _ssxTags :: [Tag]
      -- ^ Any tags assigned to the snapshot.
    , _ssxSnapshotId :: Maybe Text
      -- ^ The ID of the snapshot.
    } deriving (Show, Generic)

instance FromXML Snapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The Spot Instance datafeed subscription.
data SpotDatafeedSubscription = SpotDatafeedSubscription
    { _sdsState :: Maybe DatafeedSubscriptionState
      -- ^ The state of the Spot Instance datafeed subscription.
    , _sdsPrefix :: Maybe Text
      -- ^ The prefix that is prepended to datafeed files.
    , _sdsBucket :: Maybe Text
      -- ^ The Amazon S3 bucket where the Spot Instance datafeed is located.
    , _sdsOwnerId :: Maybe Text
      -- ^ The AWS account ID of the account.
    , _sdsFault :: Maybe SpotInstanceStateFault
      -- ^ The fault codes for the Spot Instance request, if any.
    } deriving (Show, Generic)

instance FromXML SpotDatafeedSubscription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "spotDatafeedSubscription"

-- | Describe a Spot Instance request.
data SpotInstanceRequest = SpotInstanceRequest
    { _siuInstanceId :: Maybe Text
      -- ^ The instance ID, if an instance has been launched to fulfill the
      -- Spot Instance request.
    , _siuStatus :: Maybe SpotInstanceStatus
      -- ^ The status code and status message describing the Spot Instance
      -- request.
    , _siuState :: Maybe SpotInstanceState
      -- ^ The state of the Spot Instance request. Spot bid status
      -- information can help you track your Spot Instance requests. For
      -- information, see Tracking Spot Requests with Bid Status Codes in
      -- the Amazon Elastic Compute Cloud User Guide.
    , _siuProductDescription :: Maybe RIProductDescription
      -- ^ The product description associated with the Spot Instance.
    , _siuSpotPrice :: Maybe Text
      -- ^ The maximum hourly price for any Spot Instance launched to
      -- fulfill the request.
    , _siuLaunchSpecification :: Maybe LaunchSpecification
      -- ^ Additional information for launching instances.
    , _siuAvailabilityZoneGroup :: Maybe Text
      -- ^ The Availability Zone group. If you specify the same Availability
      -- Zone group for all Spot Instance requests, all Spot Instances are
      -- launched in the same Availability Zone.
    , _siuLaunchedAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the bid is launched.
    , _siuValidUntil :: Maybe ISO8601
      -- ^ The end date of the request. If this is a one-time request, the
      -- request remains active until all instances launch, the request is
      -- canceled, or this date is reached. If the request is persistent,
      -- it remains active until it is canceled or this date is reached.
    , _siuLaunchGroup :: Maybe Text
      -- ^ The instance launch group. Launch groups are Spot Instances that
      -- launch together and terminate together.
    , _siuFault :: Maybe SpotInstanceStateFault
      -- ^ The fault codes for the Spot Instance request, if any.
    , _siuSpotInstanceRequestId :: Maybe Text
      -- ^ The ID of the Spot Instance request.
    , _siuType :: Maybe SpotInstanceType
      -- ^ The Spot Instance request type.
    , _siuValidFrom :: Maybe ISO8601
      -- ^ The start date of the request. If this is a one-time request, the
      -- request becomes active at this date and time and remains active
      -- until all instances launch, the request expires, or the request
      -- is canceled. If the request is persistent, the request becomes
      -- active at this date and time and remains active until it expires
      -- or is canceled.
    , _siuCreateTime :: Maybe ISO8601
      -- ^ The time stamp when the Spot Instance request was created.
    , _siuTags :: [Tag]
      -- ^ Any tags assigned to the resource.
    } deriving (Show, Generic)

instance FromXML SpotInstanceRequest where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The fault codes for the Spot Instance request, if any.
data SpotInstanceStateFault = SpotInstanceStateFault
    { _sisfCode :: Maybe Text
      -- ^ The reason code for the Spot Instance state change.
    , _sisfMessage :: Maybe Text
      -- ^ The message for the Spot Instance state change.
    } deriving (Show, Generic)

instance FromXML SpotInstanceStateFault where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "fault"

instance ToQuery SpotInstanceStateFault where
    toQuery = genericQuery def

-- | The status code and status message describing the Spot Instance request.
data SpotInstanceStatus = SpotInstanceStatus
    { _sivUpdateTime :: Maybe ISO8601
      -- ^ The time of the most recent status update.
    , _sivCode :: Maybe Text
      -- ^ The status code of the request.
    , _sivMessage :: Maybe Text
      -- ^ The description for the status code for the Spot request.
    } deriving (Show, Generic)

instance FromXML SpotInstanceStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery SpotInstanceStatus where
    toQuery = genericQuery def

-- | The placement information for the instance.
data SpotPlacement = SpotPlacement
    { _sqAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone.
    , _sqGroupName :: Maybe Text
      -- ^ The Availability Zone group name.
    } deriving (Show, Generic)

instance FromXML SpotPlacement where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SpotPlacement"

instance ToQuery SpotPlacement where
    toQuery = genericQuery def

-- | Describes the Spot Price.
data SpotPrice = SpotPrice
    { _spProductDescription :: Maybe RIProductDescription
      -- ^ A general description of the AMI.
    , _spSpotPrice :: Maybe Text
      -- ^ The maximum price you will pay to launch one or more Spot
      -- Instances.
    , _spInstanceType :: Maybe InstanceType
      -- ^ The instance type.
    , _spAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone.
    , _spTimestamp :: Maybe ISO8601
      -- ^ The date and time the request was created.
    } deriving (Show, Generic)

instance FromXML SpotPrice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The reason for the most recent state transition.
data StateReason = StateReason
    { _srCode :: Maybe Text
      -- ^ The reason code for the state change.
    , _srMessage :: Maybe Text
      -- ^ The message for the state change. Server.SpotInstanceTermination:
      -- A Spot Instance was terminated due to an increase in the market
      -- price. Server.InternalError: An internal error occurred during
      -- instance launch, resulting in termination.
      -- Server.InsufficientInstanceCapacity: There was insufficient
      -- instance capacity to satisfy the launch request.
      -- Client.InternalError: A client error caused the instance to
      -- terminate on launch. Client.InstanceInitiatedShutdown: The
      -- instance was shut down using the shutdown -h command from the
      -- instance. Client.UserInitiatedShutdown: The instance was shut
      -- down using the Amazon EC2 API. Client.VolumeLimitExceeded: The
      -- volume limit was exceeded. Client.InvalidSnapshot.NotFound: The
      -- specified snapshot was not found.
    } deriving (Show, Generic)

instance FromXML StateReason where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "stateReason"

instance ToQuery StateReason where
    toQuery = genericQuery def

-- | Describes a subnet.
data Subnet = Subnet
    { _swState :: Maybe SubnetState
      -- ^ The current state of the subnet.
    , _swAvailableIpAddressCount :: Maybe Integer
      -- ^ The number of unused IP addresses in the subnet. Note that the IP
      -- addresses for any stopped instances are considered unavailable.
    , _swVpcId :: Maybe Text
      -- ^ The ID of the VPC the subnet is in.
    , _swSubnetId :: Maybe Text
      -- ^ The ID of the subnet.
    , _swAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone of the subnet.
    , _swCidrBlock :: Maybe Text
      -- ^ The CIDR block assigned to the subnet.
    , _swMapPublicIpOnLaunch :: Maybe Bool
      -- ^ Indicates whether instances launched in this subnet receive a
      -- public IP address.
    , _swDefaultForAz :: Maybe Bool
      -- ^ Indicates whether this is the default subnet for the Availability
      -- Zone.
    , _swTags :: [Tag]
      -- ^ Any tags assigned to the subnet.
    } deriving (Show, Generic)

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a tag.
data Tag = Tag
    { _tgValue :: Maybe Text
      -- ^ The value of the tag. Constraints: Tag values are case-sensitive
      -- and accept a maximum of 255 Unicode characters.
    , _tgKey :: Maybe Text
      -- ^ The key of the tag. Constraints: Tag keys are case-sensitive and
      -- accept a maximum of 127 Unicode characters. May not begin with
      -- aws:.
    } deriving (Show, Generic)

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery Tag where
    toQuery = genericQuery def

-- | Describes a tag.
data TagDescription = TagDescription
    { _tdResourceId :: Maybe Text
      -- ^ The ID of the resource. For example, ami-1a2b3c4d.
    , _tdResourceType :: Maybe ResourceType
      -- ^ The type of resource.
    , _tdValue :: Maybe Text
      -- ^ The value of the tag.
    , _tdKey :: Maybe Text
      -- ^ The key of the tag.
    } deriving (Show, Generic)

instance FromXML TagDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a security group and AWS account ID pair for EC2-Classic.
data UserIdGroupPair = UserIdGroupPair
    { _uigpUserId :: Maybe Text
      -- ^ The ID of an AWS account.
    , _uigpGroupId :: Maybe Text
      -- ^ The name of the security group in the specified AWS account.
    , _uigpGroupName :: Maybe Text
      -- ^ The ID of the security group owned by the specified AWS account.
    } deriving (Show, Generic)

instance FromXML UserIdGroupPair where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Groups"

instance ToQuery UserIdGroupPair where
    toQuery = genericQuery def

-- | Describes telemetry for a VPN tunnel.
data VgwTelemetry = VgwTelemetry
    { _vvvvvvvvvvvvvyStatus :: Maybe TelemetryStatus
      -- ^ The status of the VPN tunnel.
    , _vvvvvvvvvvvvvyOutsideIpAddress :: Maybe Text
      -- ^ The Internet-routable IP address of the virtual private gateway's
      -- outside interface.
    , _vvvvvvvvvvvvvyLastStatusChange :: Maybe ISO8601
      -- ^ The date and time of the last change in status.
    , _vvvvvvvvvvvvvyAcceptedRouteCount :: Maybe Integer
      -- ^ The number of accepted routes.
    , _vvvvvvvvvvvvvyStatusMessage :: Maybe Text
      -- ^ If an error occurs, a description of the error.
    } deriving (Show, Generic)

instance FromXML VgwTelemetry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VgwTelemetry where
    toQuery = genericQuery def

-- | Describes a volume.
data Volume = Volume
    { _vvvvvvvvvvvvvvvvvvvvvvvvvvkState :: Maybe VolumeState
      -- ^ The volume state.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvkAttachments :: [VolumeAttachment]
      -- ^ 
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvkSize :: Maybe Integer
      -- ^ The size of the volume, in GiBs.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvkIops :: Maybe Integer
      -- ^ The number of I/O operations per second (IOPS) that the volume
      -- supports. For Provisioned IOPS (SSD) volumes, this represents the
      -- number of IOPS that are provisioned for the volume. For General
      -- Purpose (SSD) volumes, this represents the baseline performance
      -- of the volume and the rate at which the volume accumulates I/O
      -- credits for bursting. For more information on General Purpose
      -- (SSD) baseline performance, I/O credits, and bursting, see Amazon
      -- EBS Volume Types in the Amazon Elastic Compute Cloud User Guide.
      -- Constraint: Range is 100 to 4000 for Provisioned IOPS (SSD)
      -- volumes and 3 to 3072 for General Purpose (SSD) volumes.
      -- Condition: This parameter is required for requests to create io1
      -- volumes; it is not used in requests to create standard or gp2
      -- volumes.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvkEncrypted :: Maybe Bool
      -- ^ Indicates whether the volume is encrypted.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvkAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone for the volume.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvkVolumeId :: Maybe Text
      -- ^ The ID of the volume.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvkVolumeType :: Maybe VolumeType
      -- ^ The volume type. This can be gp2 for General Purpose (SSD)
      -- volumes, io1 for Provisioned IOPS (SSD) volumes, or standard for
      -- Magnetic volumes.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvkCreateTime :: Maybe ISO8601
      -- ^ The time stamp when volume creation was initiated.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvkTags :: [Tag]
      -- ^ Any tags assigned to the volume.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvkSnapshotId :: Maybe Text
      -- ^ The snapshot from which the volume was created, if applicable.
    } deriving (Show, Generic)

instance FromXML Volume where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes volume attachment details.
data VolumeAttachment = VolumeAttachment
    { _vvvvvvvvvvvvvvvvvvvvvvvvvvuInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvuDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the Amazon EBS volume is deleted on instance
      -- termination.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvuState :: Maybe VolumeAttachmentState
      -- ^ The attachment state of the volume.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvuDevice :: Maybe Text
      -- ^ The device name.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvuVolumeId :: Maybe Text
      -- ^ The ID of the volume.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvuAttachTime :: Maybe ISO8601
      -- ^ The time stamp when the attachment initiated.
    } deriving (Show, Generic)

instance FromXML VolumeAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VolumeAttachment where
    toQuery = genericQuery def

-- | Describes a volume status operation code.
data VolumeStatusAction = VolumeStatusAction
    { _vsaEventType :: Maybe Text
      -- ^ The event type associated with this operation.
    , _vsaCode :: Maybe Text
      -- ^ The code identifying the operation, for example,
      -- enable-volume-io.
    , _vsaDescription :: Maybe Text
      -- ^ A description of the operation.
    , _vsaEventId :: Maybe Text
      -- ^ The ID of the event associated with this operation.
    } deriving (Show, Generic)

instance FromXML VolumeStatusAction where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VolumeStatusAction where
    toQuery = genericQuery def

-- | Describes a volume status.
data VolumeStatusDetails = VolumeStatusDetails
    { _vsdStatus :: Maybe Text
      -- ^ The intended status of the volume status.
    , _vsdName :: Maybe VolumeStatusName
      -- ^ The name of the volume status.
    } deriving (Show, Generic)

instance FromXML VolumeStatusDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VolumeStatusDetails where
    toQuery = genericQuery def

-- | Describes a volume status event.
data VolumeStatusEvent = VolumeStatusEvent
    { _vseNotBefore :: Maybe ISO8601
      -- ^ The earliest start time of the event.
    , _vseEventType :: Maybe Text
      -- ^ The type of this event.
    , _vseDescription :: Maybe Text
      -- ^ A description of the event.
    , _vseNotAfter :: Maybe ISO8601
      -- ^ The latest end time of the event.
    , _vseEventId :: Maybe Text
      -- ^ The ID of this event.
    } deriving (Show, Generic)

instance FromXML VolumeStatusEvent where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VolumeStatusEvent where
    toQuery = genericQuery def

-- | The volume status.
data VolumeStatusInfo = VolumeStatusInfo
    { _vsjStatus :: Maybe VolumeStatusInfoStatus
      -- ^ The status of the volume.
    , _vsjDetails :: [VolumeStatusDetails]
      -- ^ The details of the volume status.
    } deriving (Show, Generic)

instance FromXML VolumeStatusInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "volumeStatus"

instance ToQuery VolumeStatusInfo where
    toQuery = genericQuery def

-- | Describes the volume status.
data VolumeStatusItem = VolumeStatusItem
    { _vsiVolumeStatus :: Maybe VolumeStatusInfo
      -- ^ The volume status.
    , _vsiActions :: [VolumeStatusAction]
      -- ^ The details of the operation.
    , _vsiEvents :: [VolumeStatusEvent]
      -- ^ A list of events associated with the volume.
    , _vsiAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone of the volume.
    , _vsiVolumeId :: Maybe Text
      -- ^ The volume ID.
    } deriving (Show, Generic)

instance FromXML VolumeStatusItem where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a VPC.
data Vpc = Vpc
    { _vvcState :: Maybe VpcState
      -- ^ The current state of the VPC.
    , _vvcVpcId :: Maybe Text
      -- ^ The ID of the VPC.
    , _vvcDhcpOptionsId :: Maybe Text
      -- ^ The ID of the set of DHCP options you've associated with the VPC
      -- (or default if the default options are associated with the VPC).
    , _vvcCidrBlock :: Maybe Text
      -- ^ The CIDR block for the VPC.
    , _vvcInstanceTenancy :: Maybe Tenancy
      -- ^ The allowed tenancy of instances launched into the VPC.
    , _vvcTags :: [Tag]
      -- ^ Any tags assigned to the VPC.
    , _vvcIsDefault :: Maybe Bool
      -- ^ Indicates whether the VPC is the default VPC.
    } deriving (Show, Generic)

instance FromXML Vpc where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes an attachment between a virtual private gateway and a VPC.
data VpcAttachment = VpcAttachment
    { _vaState :: Maybe AttachmentStatus
      -- ^ The current state of the attachment.
    , _vaVpcId :: Maybe Text
      -- ^ The ID of the VPC.
    } deriving (Show, Generic)

instance FromXML VpcAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VpcAttachment where
    toQuery = genericQuery def

-- | Information about the VPC peering connection.
data VpcPeeringConnection = VpcPeeringConnection
    { _vpcVpcPeeringConnectionId :: Maybe Text
      -- ^ The ID of the VPC peering connection.
    , _vpcStatus :: Maybe VpcPeeringConnectionStateReason
      -- ^ The status of the VPC peering connection.
    , _vpcAccepterVpcInfo :: Maybe VpcPeeringConnectionVpcInfo
      -- ^ The information of the peer VPC.
    , _vpcRequesterVpcInfo :: Maybe VpcPeeringConnectionVpcInfo
      -- ^ The information of the requester VPC.
    , _vpcExpirationTime :: Maybe ISO8601
      -- ^ The time that an unaccepted VPC peering connection will expire.
    , _vpcTags :: [Tag]
      -- ^ Any tags assigned to the resource.
    } deriving (Show, Generic)

instance FromXML VpcPeeringConnection where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "vpcPeeringConnection"

-- | The status of the VPC peering connection.
data VpcPeeringConnectionStateReason = VpcPeeringConnectionStateReason
    { _vpcsrCode :: Maybe Text
      -- ^ The status of the VPC peering connection.
    , _vpcsrMessage :: Maybe Text
      -- ^ A message that provides more information about the status, if
      -- applicable.
    } deriving (Show, Generic)

instance FromXML VpcPeeringConnectionStateReason where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery VpcPeeringConnectionStateReason where
    toQuery = genericQuery def

-- | The information of the peer VPC.
data VpcPeeringConnectionVpcInfo = VpcPeeringConnectionVpcInfo
    { _vpcviVpcId :: Maybe Text
      -- ^ The ID of the VPC.
    , _vpcviOwnerId :: Maybe Text
      -- ^ The AWS account ID of the VPC owner.
    , _vpcviCidrBlock :: Maybe Text
      -- ^ The CIDR block for the VPC.
    } deriving (Show, Generic)

instance FromXML VpcPeeringConnectionVpcInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "accepterVpcInfo"

instance ToQuery VpcPeeringConnectionVpcInfo where
    toQuery = genericQuery def

-- | Describes a VPN connection.
data VpnConnection = VpnConnection
    { _vcCustomerGatewayConfiguration :: Maybe Text
      -- ^ The configuration information for the VPN connection's customer
      -- gateway (in the native XML format). This element is always
      -- present in the CreateVpnConnection response; however, it's
      -- present in the DescribeVpnConnections response only if the VPN
      -- connection is in the pending or available state.
    , _vcState :: Maybe VpnState
      -- ^ The current state of the VPN connection.
    , _vcRoutes :: [VpnStaticRoute]
      -- ^ The static routes associated with the VPN connection.
    , _vcVpnGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway at the AWS side of the VPN
      -- connection.
    , _vcCustomerGatewayId :: Maybe Text
      -- ^ The ID of the customer gateway at your end of the VPN connection.
    , _vcType :: Maybe GatewayType
      -- ^ The type of VPN connection.
    , _vcOptions :: Maybe VpnConnectionOptions
      -- ^ The VPN connection options.
    , _vcVpnConnectionId :: Maybe Text
      -- ^ The ID of the VPN connection.
    , _vcTags :: [Tag]
      -- ^ Any tags assigned to the VPN connection.
    , _vcVgwTelemetry :: [VgwTelemetry]
      -- ^ Information about the VPN tunnel.
    } deriving (Show, Generic)

instance FromXML VpnConnection where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Information about the virtual private gateway.
data VpnGateway = VpnGateway
    { _vvyState :: Maybe VpnState
      -- ^ The current state of the virtual private gateway.
    , _vvyVpcAttachments :: [VpcAttachment]
      -- ^ Any VPCs attached to the virtual private gateway.
    , _vvyVpnGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway.
    , _vvyAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone where the virtual private gateway was
      -- created.
    , _vvyType :: Maybe GatewayType
      -- ^ The type of VPN connection the virtual private gateway supports.
    , _vvyTags :: [Tag]
      -- ^ Any tags assigned to the virtual private gateway.
    } deriving (Show, Generic)

instance FromXML VpnGateway where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "vpnGateway"

-- | Describes a static route for a VPN connection.
data VpnStaticRoute = VpnStaticRoute
    { _vswState :: Maybe VpnState
      -- ^ The current state of the static route.
    , _vswSource :: Maybe VpnStaticRouteSource
      -- ^ Indicates how the routes were provided.
    , _vswDestinationCidrBlock :: Maybe Text
      -- ^ The CIDR block associated with the local subnet of the customer
      -- data center.
    } deriving (Show, Generic)

instance FromXML VpnStaticRoute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VpnStaticRoute where
    toQuery = genericQuery def

makeLenses ''AccountAttributeValue
makeLenses ''AttributeBooleanValue
makeLenses ''AttributeValue
makeLenses ''AvailabilityZoneMessage
makeLenses ''IpRange
makeLenses ''Monitoring
makeLenses ''PropagatingVgw
makeLenses ''ReservedInstancesId
makeLenses ''RunInstancesMonitoringEnabled
makeLenses ''Storage
makeLenses ''VolumeDetail
makeLenses ''VpnConnectionOptions
makeLenses ''VpnConnectionOptionsSpecification
makeLenses ''AccountAttribute
makeLenses ''Address
makeLenses ''AvailabilityZone
makeLenses ''BlockDeviceMapping
makeLenses ''BundleTask
makeLenses ''BundleTaskError
makeLenses ''CancelledSpotInstanceRequest
makeLenses ''ConversionTask
makeLenses ''CreateVolumePermission
makeLenses ''CreateVolumePermissionModifications
makeLenses ''CustomerGateway
makeLenses ''DhcpConfiguration
makeLenses ''DhcpOptions
makeLenses ''DiskImage
makeLenses ''DiskImageDescription
makeLenses ''DiskImageDetail
makeLenses ''DiskImageVolumeDescription
makeLenses ''EbsBlockDevice
makeLenses ''EbsInstanceBlockDevice
makeLenses ''EbsInstanceBlockDeviceSpecification
makeLenses ''ExportTask
makeLenses ''ExportToS3Task
makeLenses ''ExportToS3TaskSpecification
makeLenses ''Filter
makeLenses ''GroupIdentifier
makeLenses ''IamInstanceProfile
makeLenses ''IamInstanceProfileSpecification
makeLenses ''IcmpTypeCode
makeLenses ''Image
makeLenses ''ImportInstanceLaunchSpecification
makeLenses ''ImportInstanceTaskDetails
makeLenses ''ImportInstanceVolumeDetailItem
makeLenses ''ImportVolumeTaskDetails
makeLenses ''Instance
makeLenses ''InstanceBlockDeviceMapping
makeLenses ''InstanceBlockDeviceMappingSpecification
makeLenses ''InstanceCount
makeLenses ''InstanceExportDetails
makeLenses ''InstanceMonitoring
makeLenses ''InstanceNetworkInterface
makeLenses ''InstanceNetworkInterfaceAssociation
makeLenses ''InstanceNetworkInterfaceAttachment
makeLenses ''InstanceNetworkInterfaceSpecification
makeLenses ''InstancePrivateIpAddress
makeLenses ''InstanceState
makeLenses ''InstanceStateChange
makeLenses ''InstanceStatus
makeLenses ''InstanceStatusDetails
makeLenses ''InstanceStatusEvent
makeLenses ''InstanceStatusSummary
makeLenses ''InternetGateway
makeLenses ''InternetGatewayAttachment
makeLenses ''IpPermission
makeLenses ''KeyPairInfo
makeLenses ''LaunchPermission
makeLenses ''LaunchPermissionModifications
makeLenses ''LaunchSpecification
makeLenses ''NetworkAcl
makeLenses ''NetworkAclAssociation
makeLenses ''NetworkAclEntry
makeLenses ''NetworkInterface
makeLenses ''NetworkInterfaceAssociation
makeLenses ''NetworkInterfaceAttachment
makeLenses ''NetworkInterfaceAttachmentChanges
makeLenses ''NetworkInterfacePrivateIpAddress
makeLenses ''Placement
makeLenses ''PlacementGroup
makeLenses ''PortRange
makeLenses ''PriceSchedule
makeLenses ''PriceScheduleSpecification
makeLenses ''PricingDetail
makeLenses ''PrivateIpAddressSpecification
makeLenses ''ProductCode
makeLenses ''RecurringCharge
makeLenses ''Region
makeLenses ''Reservation
makeLenses ''ReservedInstanceLimitPrice
makeLenses ''ReservedInstances
makeLenses ''ReservedInstancesConfiguration
makeLenses ''ReservedInstancesListing
makeLenses ''ReservedInstancesModification
makeLenses ''ReservedInstancesModificationResult
makeLenses ''ReservedInstancesOffering
makeLenses ''Route
makeLenses ''RouteTable
makeLenses ''RouteTableAssociation
makeLenses ''S3Storage
makeLenses ''SecurityGroup
makeLenses ''Snapshot
makeLenses ''SpotDatafeedSubscription
makeLenses ''SpotInstanceRequest
makeLenses ''SpotInstanceStateFault
makeLenses ''SpotInstanceStatus
makeLenses ''SpotPlacement
makeLenses ''SpotPrice
makeLenses ''StateReason
makeLenses ''Subnet
makeLenses ''Tag
makeLenses ''TagDescription
makeLenses ''UserIdGroupPair
makeLenses ''VgwTelemetry
makeLenses ''Volume
makeLenses ''VolumeAttachment
makeLenses ''VolumeStatusAction
makeLenses ''VolumeStatusDetails
makeLenses ''VolumeStatusEvent
makeLenses ''VolumeStatusInfo
makeLenses ''VolumeStatusItem
makeLenses ''Vpc
makeLenses ''VpcAttachment
makeLenses ''VpcPeeringConnection
makeLenses ''VpcPeeringConnectionStateReason
makeLenses ''VpcPeeringConnectionVpcInfo
makeLenses ''VpnConnection
makeLenses ''VpnGateway
makeLenses ''VpnStaticRoute
