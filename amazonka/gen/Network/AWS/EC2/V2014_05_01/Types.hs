{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Elastic Compute Cloud Amazon Elastic Compute Cloud (Amazon EC2)
-- provides resizable computing capacity in the Amazon Web Services (AWS)
-- cloud. Using Amazon EC2 eliminates your need to invest in hardware up
-- front, so you can develop and deploy applications faster.
module Network.AWS.EC2.V2014_05_01.Types where

import Control.Applicative
import Control.Exception      (Exception)
import Data.Default
import Data.Tagged
import Data.Text              (Text)
import Data.Typeable
import GHC.Generics
import Network.AWS.Data
import Network.AWS.Signing.V4
import Network.AWS.Types      hiding (Error)
import Network.HTTP.Client    (HttpException)

-- | Supported version (@2014-05-01@) of the
-- @Amazon Elastic Compute Cloud@ service.
data EC2 deriving (Typeable)

instance AWSService EC2 where
    type Sg EC2 = V4
    data Er EC2
        = EC2Client HttpException
        | EC2Serializer String
        | EC2Service String

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "ec2"
        , _svcVersion  = "2014-05-01"
        , _svcTarget   = Nothing
        }

deriving instance Show     (Er EC2)
deriving instance Generic  (Er EC2)

instance AWSError (Er EC2) where
    awsError = const "EC2Error"

instance ServiceError (Er EC2) where
    serviceError    = EC2Service
    clientError     = EC2Client
    serializerError = EC2Serializer

instance Exception (Er EC2)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://ec2.amazonaws.com/doc/2014-05-01/"
    }

data AccountAttributeName
    = AccountAttributeNameDefaultVpc -- ^ default-vpc
    | AccountAttributeNameSupportedPlatforms -- ^ supported-platforms
      deriving (Eq, Show, Generic)

instance FromText AccountAttributeName where
    parser = match "default-vpc" AccountAttributeNameDefaultVpc
         <|> match "supported-platforms" AccountAttributeNameSupportedPlatforms

instance ToText AccountAttributeName where
    toText AccountAttributeNameDefaultVpc = "default-vpc"
    toText AccountAttributeNameSupportedPlatforms = "supported-platforms"

instance ToByteString AccountAttributeName where
    toBS AccountAttributeNameDefaultVpc = "default-vpc"
    toBS AccountAttributeNameSupportedPlatforms = "supported-platforms"

instance ToXML AccountAttributeName where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "AttributeName"

-- | The architecture of the instance.
data ArchitectureValues
    = ArchitectureValuesI386 -- ^ i386
    | ArchitectureValuesX8664 -- ^ x86_64
      deriving (Eq, Show, Generic)

instance FromText ArchitectureValues where
    parser = match "i386" ArchitectureValuesI386
         <|> match "x86_64" ArchitectureValuesX8664

instance ToText ArchitectureValues where
    toText ArchitectureValuesI386 = "i386"
    toText ArchitectureValuesX8664 = "x86_64"

instance ToByteString ArchitectureValues where
    toBS ArchitectureValuesI386 = "i386"
    toBS ArchitectureValuesX8664 = "x86_64"

instance FromXML ArchitectureValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML ArchitectureValues where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The current state of the attachment.
data AttachmentStatus
    = AttachmentStatusAttached -- ^ attached
    | AttachmentStatusAttaching -- ^ attaching
    | AttachmentStatusDetached -- ^ detached
    | AttachmentStatusDetaching -- ^ detaching
      deriving (Eq, Show, Generic)

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

instance ToByteString AttachmentStatus where
    toBS AttachmentStatusAttached = "attached"
    toBS AttachmentStatusAttaching = "attaching"
    toBS AttachmentStatusDetached = "detached"
    toBS AttachmentStatusDetaching = "detaching"

instance FromXML AttachmentStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToXML AttachmentStatus where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "state"

-- | The state of the Availability Zone.
data AvailabilityZoneState
    = AvailabilityZoneStateAvailable -- ^ available
      deriving (Eq, Show, Generic)

instance FromText AvailabilityZoneState where
    parser = match "available" AvailabilityZoneStateAvailable

instance ToText AvailabilityZoneState where
    toText AvailabilityZoneStateAvailable = "available"

instance ToByteString AvailabilityZoneState where
    toBS AvailabilityZoneStateAvailable = "available"

instance FromXML AvailabilityZoneState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "zoneState"

instance ToXML AvailabilityZoneState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "zoneState"

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

instance ToByteString BundleTaskState where
    toBS BundleTaskStateBundling = "bundling"
    toBS BundleTaskStateCancelling = "cancelling"
    toBS BundleTaskStateComplete = "complete"
    toBS BundleTaskStateFailed = "failed"
    toBS BundleTaskStatePending = "pending"
    toBS BundleTaskStateStoring = "storing"
    toBS BundleTaskStateWaitingForShutdown = "waiting-for-shutdown"

instance FromXML BundleTaskState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToXML BundleTaskState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "state"

-- | The state of the Spot Instance request.
data CancelSpotInstanceRequestState
    = CancelSpotInstanceRequestStateActive -- ^ active
    | CancelSpotInstanceRequestStateCancelled -- ^ cancelled
    | CancelSpotInstanceRequestStateClosed -- ^ closed
    | CancelSpotInstanceRequestStateCompleted -- ^ completed
    | CancelSpotInstanceRequestStateOpen -- ^ open
      deriving (Eq, Show, Generic)

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

instance ToByteString CancelSpotInstanceRequestState where
    toBS CancelSpotInstanceRequestStateActive = "active"
    toBS CancelSpotInstanceRequestStateCancelled = "cancelled"
    toBS CancelSpotInstanceRequestStateClosed = "closed"
    toBS CancelSpotInstanceRequestStateCompleted = "completed"
    toBS CancelSpotInstanceRequestStateOpen = "open"

instance FromXML CancelSpotInstanceRequestState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToXML CancelSpotInstanceRequestState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "state"

-- | 
data ContainerFormat
    = ContainerFormatOva -- ^ ova
      deriving (Eq, Show, Generic)

instance FromText ContainerFormat where
    parser = match "ova" ContainerFormatOva

instance ToText ContainerFormat where
    toText ContainerFormatOva = "ova"

instance ToByteString ContainerFormat where
    toBS ContainerFormatOva = "ova"

instance FromXML ContainerFormat where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML ContainerFormat where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The state of the conversion task.
data ConversionTaskState
    = ConversionTaskStateActive -- ^ active
    | ConversionTaskStateCancelled -- ^ cancelled
    | ConversionTaskStateCancelling -- ^ cancelling
    | ConversionTaskStateCompleted -- ^ completed
      deriving (Eq, Show, Generic)

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

instance ToByteString ConversionTaskState where
    toBS ConversionTaskStateActive = "active"
    toBS ConversionTaskStateCancelled = "cancelled"
    toBS ConversionTaskStateCancelling = "cancelling"
    toBS ConversionTaskStateCompleted = "completed"

instance FromXML ConversionTaskState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToXML ConversionTaskState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "state"

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is USD.
data CurrencyCodeValues
    = CurrencyCodeValuesUsd -- ^ USD
      deriving (Eq, Show, Generic)

instance FromText CurrencyCodeValues where
    parser = match "USD" CurrencyCodeValuesUsd

instance ToText CurrencyCodeValues where
    toText CurrencyCodeValuesUsd = "USD"

instance ToByteString CurrencyCodeValues where
    toBS CurrencyCodeValuesUsd = "USD"

instance FromXML CurrencyCodeValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "currencyCode"

instance ToXML CurrencyCodeValues where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "currencyCode"

-- | The state of the Spot Instance datafeed subscription.
data DatafeedSubscriptionState
    = DatafeedSubscriptionStateActive -- ^ Active
    | DatafeedSubscriptionStateInactive -- ^ Inactive
      deriving (Eq, Show, Generic)

instance FromText DatafeedSubscriptionState where
    parser = match "Active" DatafeedSubscriptionStateActive
         <|> match "Inactive" DatafeedSubscriptionStateInactive

instance ToText DatafeedSubscriptionState where
    toText DatafeedSubscriptionStateActive = "Active"
    toText DatafeedSubscriptionStateInactive = "Inactive"

instance ToByteString DatafeedSubscriptionState where
    toBS DatafeedSubscriptionStateActive = "Active"
    toBS DatafeedSubscriptionStateInactive = "Inactive"

instance FromXML DatafeedSubscriptionState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

-- | The root device type used by the AMI. The AMI can use an Amazon EBS volume
-- or an instance store volume.
data DeviceType
    = DeviceTypeEbs -- ^ ebs
    | DeviceTypeInstanceStore -- ^ instance-store
      deriving (Eq, Show, Generic)

instance FromText DeviceType where
    parser = match "ebs" DeviceTypeEbs
         <|> match "instance-store" DeviceTypeInstanceStore

instance ToText DeviceType where
    toText DeviceTypeEbs = "ebs"
    toText DeviceTypeInstanceStore = "instance-store"

instance ToByteString DeviceType where
    toBS DeviceTypeEbs = "ebs"
    toBS DeviceTypeInstanceStore = "instance-store"

instance FromXML DeviceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "rootDeviceType"

instance ToXML DeviceType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "rootDeviceType"

-- | The disk image format.
data DiskImageFormat
    = DiskImageFormatRaw -- ^ RAW
    | DiskImageFormatVhd -- ^ VHD
    | DiskImageFormatVmdk -- ^ VMDK
      deriving (Eq, Show, Generic)

instance FromText DiskImageFormat where
    parser = match "RAW" DiskImageFormatRaw
         <|> match "VHD" DiskImageFormatVhd
         <|> match "VMDK" DiskImageFormatVmdk

instance ToText DiskImageFormat where
    toText DiskImageFormatRaw = "RAW"
    toText DiskImageFormatVhd = "VHD"
    toText DiskImageFormatVmdk = "VMDK"

instance ToByteString DiskImageFormat where
    toBS DiskImageFormatRaw = "RAW"
    toBS DiskImageFormatVhd = "VHD"
    toBS DiskImageFormatVmdk = "VMDK"

instance FromXML DiskImageFormat where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML DiskImageFormat where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (standard) or instances in a VPC (vpc).
data DomainType
    = DomainTypeStandard -- ^ standard
    | DomainTypeVpc -- ^ vpc
      deriving (Eq, Show, Generic)

instance FromText DomainType where
    parser = match "standard" DomainTypeStandard
         <|> match "vpc" DomainTypeVpc

instance ToText DomainType where
    toText DomainTypeStandard = "standard"
    toText DomainTypeVpc = "vpc"

instance ToByteString DomainType where
    toBS DomainTypeStandard = "standard"
    toBS DomainTypeVpc = "vpc"

instance FromXML DomainType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "domain"

instance ToXML DomainType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "domain"

-- | The associated code of the event.
data EventCode
    = EventCodeInstanceReboot -- ^ instance-reboot
    | EventCodeInstanceRetirement -- ^ instance-retirement
    | EventCodeInstanceStop -- ^ instance-stop
    | EventCodeSystemMaintenance -- ^ system-maintenance
    | EventCodeSystemReboot -- ^ system-reboot
      deriving (Eq, Show, Generic)

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

instance ToByteString EventCode where
    toBS EventCodeInstanceReboot = "instance-reboot"
    toBS EventCodeInstanceRetirement = "instance-retirement"
    toBS EventCodeInstanceStop = "instance-stop"
    toBS EventCodeSystemMaintenance = "system-maintenance"
    toBS EventCodeSystemReboot = "system-reboot"

instance FromXML EventCode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "code"

instance ToXML EventCode where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "code"

-- | The target virtualization environment.
data ExportEnvironment
    = ExportEnvironmentCitrix -- ^ citrix
    | ExportEnvironmentMicrosoft -- ^ microsoft
    | ExportEnvironmentVmware -- ^ vmware
      deriving (Eq, Show, Generic)

instance FromText ExportEnvironment where
    parser = match "citrix" ExportEnvironmentCitrix
         <|> match "microsoft" ExportEnvironmentMicrosoft
         <|> match "vmware" ExportEnvironmentVmware

instance ToText ExportEnvironment where
    toText ExportEnvironmentCitrix = "citrix"
    toText ExportEnvironmentMicrosoft = "microsoft"
    toText ExportEnvironmentVmware = "vmware"

instance ToByteString ExportEnvironment where
    toBS ExportEnvironmentCitrix = "citrix"
    toBS ExportEnvironmentMicrosoft = "microsoft"
    toBS ExportEnvironmentVmware = "vmware"

instance FromXML ExportEnvironment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML ExportEnvironment where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The state of the conversion task.
data ExportTaskState
    = ExportTaskStateActive -- ^ active
    | ExportTaskStateCancelled -- ^ cancelled
    | ExportTaskStateCancelling -- ^ cancelling
    | ExportTaskStateCompleted -- ^ completed
      deriving (Eq, Show, Generic)

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

instance ToByteString ExportTaskState where
    toBS ExportTaskStateActive = "active"
    toBS ExportTaskStateCancelled = "cancelled"
    toBS ExportTaskStateCancelling = "cancelling"
    toBS ExportTaskStateCompleted = "completed"

instance FromXML ExportTaskState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToXML ExportTaskState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "state"

-- | The type of VPN connection this virtual private gateway supports.
data GatewayType
    = GatewayTypeIpsec1 -- ^ ipsec.1
      deriving (Eq, Show, Generic)

instance FromText GatewayType where
    parser = match "ipsec.1" GatewayTypeIpsec1

instance ToText GatewayType where
    toText GatewayTypeIpsec1 = "ipsec.1"

instance ToByteString GatewayType where
    toBS GatewayTypeIpsec1 = "ipsec.1"

instance FromXML GatewayType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML GatewayType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The hypervisor type of the instance.
data HypervisorType
    = HypervisorTypeOvm -- ^ ovm
    | HypervisorTypeXen -- ^ xen
      deriving (Eq, Show, Generic)

instance FromText HypervisorType where
    parser = match "ovm" HypervisorTypeOvm
         <|> match "xen" HypervisorTypeXen

instance ToText HypervisorType where
    toText HypervisorTypeOvm = "ovm"
    toText HypervisorTypeXen = "xen"

instance ToByteString HypervisorType where
    toBS HypervisorTypeOvm = "ovm"
    toBS HypervisorTypeXen = "xen"

instance FromXML HypervisorType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "hypervisor"

instance ToXML HypervisorType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "hypervisor"

-- | The AMI attribute.
data ImageAttributeName
    = ImageAttributeNameBlockDeviceMapping -- ^ blockDeviceMapping
    | ImageAttributeNameDescription -- ^ description
    | ImageAttributeNameKernel -- ^ kernel
    | ImageAttributeNameLaunchPermission -- ^ launchPermission
    | ImageAttributeNameProductCodes -- ^ productCodes
    | ImageAttributeNameRamdisk -- ^ ramdisk
      deriving (Eq, Show, Generic)

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

instance ToByteString ImageAttributeName where
    toBS ImageAttributeNameBlockDeviceMapping = "blockDeviceMapping"
    toBS ImageAttributeNameDescription = "description"
    toBS ImageAttributeNameKernel = "kernel"
    toBS ImageAttributeNameLaunchPermission = "launchPermission"
    toBS ImageAttributeNameProductCodes = "productCodes"
    toBS ImageAttributeNameRamdisk = "ramdisk"

instance ToXML ImageAttributeName where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The current state of the AMI. If the state is available, the image is
-- successfully registered and can be used to launch an instance.
data ImageState
    = ImageStateAvailable -- ^ available
    | ImageStateDeregistered -- ^ deregistered
      deriving (Eq, Show, Generic)

instance FromText ImageState where
    parser = match "available" ImageStateAvailable
         <|> match "deregistered" ImageStateDeregistered

instance ToText ImageState where
    toText ImageStateAvailable = "available"
    toText ImageStateDeregistered = "deregistered"

instance ToByteString ImageState where
    toBS ImageStateAvailable = "available"
    toBS ImageStateDeregistered = "deregistered"

instance FromXML ImageState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "imageState"

instance ToXML ImageState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "imageState"

-- | The type of image.
data ImageTypeValues
    = ImageTypeValuesKernel -- ^ kernel
    | ImageTypeValuesMachine -- ^ machine
    | ImageTypeValuesRamdisk -- ^ ramdisk
      deriving (Eq, Show, Generic)

instance FromText ImageTypeValues where
    parser = match "kernel" ImageTypeValuesKernel
         <|> match "machine" ImageTypeValuesMachine
         <|> match "ramdisk" ImageTypeValuesRamdisk

instance ToText ImageTypeValues where
    toText ImageTypeValuesKernel = "kernel"
    toText ImageTypeValuesMachine = "machine"
    toText ImageTypeValuesRamdisk = "ramdisk"

instance ToByteString ImageTypeValues where
    toBS ImageTypeValuesKernel = "kernel"
    toBS ImageTypeValuesMachine = "machine"
    toBS ImageTypeValuesRamdisk = "ramdisk"

instance FromXML ImageTypeValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "imageType"

instance ToXML ImageTypeValues where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "imageType"

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

instance ToByteString InstanceAttributeName where
    toBS InstanceAttributeNameBlockDeviceMapping = "blockDeviceMapping"
    toBS InstanceAttributeNameDisableApiTermination = "disableApiTermination"
    toBS InstanceAttributeNameEbsOptimized = "ebsOptimized"
    toBS InstanceAttributeNameGroupSet = "groupSet"
    toBS InstanceAttributeNameInstanceInitiatedShutdownBehavior = "instanceInitiatedShutdownBehavior"
    toBS InstanceAttributeNameInstanceType = "instanceType"
    toBS InstanceAttributeNameKernel = "kernel"
    toBS InstanceAttributeNameProductCodes = "productCodes"
    toBS InstanceAttributeNameRamdisk = "ramdisk"
    toBS InstanceAttributeNameRootDeviceName = "rootDeviceName"
    toBS InstanceAttributeNameSourceDestCheck = "sourceDestCheck"
    toBS InstanceAttributeNameSriovNetSupport = "sriovNetSupport"
    toBS InstanceAttributeNameUserData = "userData"

instance ToXML InstanceAttributeName where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | Indicates whether this is a Spot Instance.
data InstanceLifecycleType
    = InstanceLifecycleTypeSpot -- ^ spot
      deriving (Eq, Show, Generic)

instance FromText InstanceLifecycleType where
    parser = match "spot" InstanceLifecycleTypeSpot

instance ToText InstanceLifecycleType where
    toText InstanceLifecycleTypeSpot = "spot"

instance ToByteString InstanceLifecycleType where
    toBS InstanceLifecycleTypeSpot = "spot"

instance FromXML InstanceLifecycleType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "instanceLifecycle"

instance ToXML InstanceLifecycleType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "instanceLifecycle"

-- | The current state of the instance.
data InstanceStateName
    = InstanceStateNamePending -- ^ pending
    | InstanceStateNameRunning -- ^ running
    | InstanceStateNameShuttingDown -- ^ shutting-down
    | InstanceStateNameStopped -- ^ stopped
    | InstanceStateNameStopping -- ^ stopping
    | InstanceStateNameTerminated -- ^ terminated
      deriving (Eq, Show, Generic)

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

instance ToByteString InstanceStateName where
    toBS InstanceStateNamePending = "pending"
    toBS InstanceStateNameRunning = "running"
    toBS InstanceStateNameShuttingDown = "shutting-down"
    toBS InstanceStateNameStopped = "stopped"
    toBS InstanceStateNameStopping = "stopping"
    toBS InstanceStateNameTerminated = "terminated"

instance FromXML InstanceStateName where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "name"

instance ToXML InstanceStateName where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "name"

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
      deriving (Eq, Show, Generic)

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

instance ToByteString InstanceType where
    toBS C1Medium = "c1.medium"
    toBS C1Xlarge = "c1.xlarge"
    toBS C32Xlarge = "c3.2xlarge"
    toBS C34Xlarge = "c3.4xlarge"
    toBS C38Xlarge = "c3.8xlarge"
    toBS C3Large = "c3.large"
    toBS C3Xlarge = "c3.xlarge"
    toBS Cc14Xlarge = "cc1.4xlarge"
    toBS Cc28Xlarge = "cc2.8xlarge"
    toBS Cg14Xlarge = "cg1.4xlarge"
    toBS Cr18Xlarge = "cr1.8xlarge"
    toBS G22Xlarge = "g2.2xlarge"
    toBS Hi14Xlarge = "hi1.4xlarge"
    toBS Hs18Xlarge = "hs1.8xlarge"
    toBS I22Xlarge = "i2.2xlarge"
    toBS I24Xlarge = "i2.4xlarge"
    toBS I28Xlarge = "i2.8xlarge"
    toBS I2Xlarge = "i2.xlarge"
    toBS M1Large = "m1.large"
    toBS M1Medium = "m1.medium"
    toBS M1Small = "m1.small"
    toBS M1Xlarge = "m1.xlarge"
    toBS M22Xlarge = "m2.2xlarge"
    toBS M24Xlarge = "m2.4xlarge"
    toBS M2Xlarge = "m2.xlarge"
    toBS M32Xlarge = "m3.2xlarge"
    toBS M3Large = "m3.large"
    toBS M3Medium = "m3.medium"
    toBS M3Xlarge = "m3.xlarge"
    toBS R32Xlarge = "r3.2xlarge"
    toBS R34Xlarge = "r3.4xlarge"
    toBS R38Xlarge = "r3.8xlarge"
    toBS R3Large = "r3.large"
    toBS R3Xlarge = "r3.xlarge"
    toBS T1Micro = "t1.micro"

instance FromXML InstanceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML InstanceType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The states of the listed Reserved Instances.
data ListingState
    = ListingStateAvailable -- ^ available
    | ListingStateCancelled -- ^ cancelled
    | ListingStatePending -- ^ pending
    | ListingStateSold -- ^ sold
      deriving (Eq, Show, Generic)

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

instance ToByteString ListingState where
    toBS ListingStateAvailable = "available"
    toBS ListingStateCancelled = "cancelled"
    toBS ListingStatePending = "pending"
    toBS ListingStateSold = "sold"

instance FromXML ListingState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToXML ListingState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "state"

-- | The status of the Reserved Instance listing.
data ListingStatus
    = ListingStatusActive -- ^ active
    | ListingStatusCancelled -- ^ cancelled
    | ListingStatusClosed -- ^ closed
    | ListingStatusPending -- ^ pending
      deriving (Eq, Show, Generic)

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

instance ToByteString ListingStatus where
    toBS ListingStatusActive = "active"
    toBS ListingStatusCancelled = "cancelled"
    toBS ListingStatusClosed = "closed"
    toBS ListingStatusPending = "pending"

instance FromXML ListingStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToXML ListingStatus where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "status"

-- | Indicates whether monitoring is enabled for the instance.
data MonitoringState
    = MonitoringStateDisabled -- ^ disabled
    | MonitoringStateEnabled -- ^ enabled
    | MonitoringStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance FromText MonitoringState where
    parser = match "disabled" MonitoringStateDisabled
         <|> match "enabled" MonitoringStateEnabled
         <|> match "pending" MonitoringStatePending

instance ToText MonitoringState where
    toText MonitoringStateDisabled = "disabled"
    toText MonitoringStateEnabled = "enabled"
    toText MonitoringStatePending = "pending"

instance ToByteString MonitoringState where
    toBS MonitoringStateDisabled = "disabled"
    toBS MonitoringStateEnabled = "enabled"
    toBS MonitoringStatePending = "pending"

instance FromXML MonitoringState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToXML MonitoringState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "state"

-- | The status of the network interface.
data NetworkInterfaceStatus
    = NetworkInterfaceStatusAttaching -- ^ attaching
    | NetworkInterfaceStatusAvailable -- ^ available
    | NetworkInterfaceStatusDetaching -- ^ detaching
    | NetworkInterfaceStatusInUse -- ^ in-use
      deriving (Eq, Show, Generic)

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

instance ToByteString NetworkInterfaceStatus where
    toBS NetworkInterfaceStatusAttaching = "attaching"
    toBS NetworkInterfaceStatusAvailable = "available"
    toBS NetworkInterfaceStatusDetaching = "detaching"
    toBS NetworkInterfaceStatusInUse = "in-use"

instance FromXML NetworkInterfaceStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToXML NetworkInterfaceStatus where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "status"

-- | The Reserved Instance offering type.
data OfferingTypeValues
    = OfferingTypeValuesHeavyUtilization -- ^ Heavy Utilization
    | OfferingTypeValuesLightUtilization -- ^ Light Utilization
    | OfferingTypeValuesMediumUtilization -- ^ Medium Utilization
      deriving (Eq, Show, Generic)

instance FromText OfferingTypeValues where
    parser = match "Heavy Utilization" OfferingTypeValuesHeavyUtilization
         <|> match "Light Utilization" OfferingTypeValuesLightUtilization
         <|> match "Medium Utilization" OfferingTypeValuesMediumUtilization

instance ToText OfferingTypeValues where
    toText OfferingTypeValuesHeavyUtilization = "Heavy Utilization"
    toText OfferingTypeValuesLightUtilization = "Light Utilization"
    toText OfferingTypeValuesMediumUtilization = "Medium Utilization"

instance ToByteString OfferingTypeValues where
    toBS OfferingTypeValuesHeavyUtilization = "Heavy Utilization"
    toBS OfferingTypeValuesLightUtilization = "Light Utilization"
    toBS OfferingTypeValuesMediumUtilization = "Medium Utilization"

instance FromXML OfferingTypeValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML OfferingTypeValues where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The specific group that is to be added or removed from a volume's list of
-- create volume permissions.
data PermissionGroup
    = PermissionGroupAll -- ^ all
      deriving (Eq, Show, Generic)

instance FromText PermissionGroup where
    parser = match "all" PermissionGroupAll

instance ToText PermissionGroup where
    toText PermissionGroupAll = "all"

instance ToByteString PermissionGroup where
    toBS PermissionGroupAll = "all"

instance FromXML PermissionGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "group"

instance ToXML PermissionGroup where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "group"

-- | The state of the placement group.
data PlacementGroupState
    = PlacementGroupStateAvailable -- ^ available
    | PlacementGroupStateDeleted -- ^ deleted
    | PlacementGroupStateDeleting -- ^ deleting
    | PlacementGroupStatePending -- ^ pending
      deriving (Eq, Show, Generic)

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

instance ToByteString PlacementGroupState where
    toBS PlacementGroupStateAvailable = "available"
    toBS PlacementGroupStateDeleted = "deleted"
    toBS PlacementGroupStateDeleting = "deleting"
    toBS PlacementGroupStatePending = "pending"

instance FromXML PlacementGroupState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToXML PlacementGroupState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "state"

-- | The placement strategy.
data PlacementStrategy
    = PlacementStrategyCluster -- ^ cluster
      deriving (Eq, Show, Generic)

instance FromText PlacementStrategy where
    parser = match "cluster" PlacementStrategyCluster

instance ToText PlacementStrategy where
    toText PlacementStrategyCluster = "cluster"

instance ToByteString PlacementStrategy where
    toBS PlacementStrategyCluster = "cluster"

instance FromXML PlacementStrategy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML PlacementStrategy where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The instance operating system.
data PlatformValues
    = PlatformValuesWindows -- ^ Windows
      deriving (Eq, Show, Generic)

instance FromText PlatformValues where
    parser = match "Windows" PlatformValuesWindows

instance ToText PlatformValues where
    toText PlatformValuesWindows = "Windows"

instance ToByteString PlatformValues where
    toBS PlatformValuesWindows = "Windows"

instance FromXML PlatformValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML PlatformValues where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The type of product code.
data ProductCodeValues
    = ProductCodeValuesDevpay -- ^ devpay
    | ProductCodeValuesMarketplace -- ^ marketplace
      deriving (Eq, Show, Generic)

instance FromText ProductCodeValues where
    parser = match "devpay" ProductCodeValuesDevpay
         <|> match "marketplace" ProductCodeValuesMarketplace

instance ToText ProductCodeValues where
    toText ProductCodeValuesDevpay = "devpay"
    toText ProductCodeValuesMarketplace = "marketplace"

instance ToByteString ProductCodeValues where
    toBS ProductCodeValuesDevpay = "devpay"
    toBS ProductCodeValuesMarketplace = "marketplace"

instance FromXML ProductCodeValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "type"

instance ToXML ProductCodeValues where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "type"

-- | A general description of the AMI.
data RIProductDescription
    = RIProductDescriptionLinuxUnix -- ^ Linux/UNIX
    | RIProductDescriptionLinuxUnixAmazonVpc -- ^ Linux/UNIX (Amazon VPC)
    | RIProductDescriptionWindows -- ^ Windows
    | RIProductDescriptionWindowsAmazonVpc -- ^ Windows (Amazon VPC)
      deriving (Eq, Show, Generic)

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

instance ToByteString RIProductDescription where
    toBS RIProductDescriptionLinuxUnix = "Linux/UNIX"
    toBS RIProductDescriptionLinuxUnixAmazonVpc = "Linux/UNIX (Amazon VPC)"
    toBS RIProductDescriptionWindows = "Windows"
    toBS RIProductDescriptionWindowsAmazonVpc = "Windows (Amazon VPC)"

instance FromXML RIProductDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "productDescription"

instance ToXML RIProductDescription where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "productDescription"

-- | The frequency of the recurring charge.
data RecurringChargeFrequency
    = RecurringChargeFrequencyHourly -- ^ Hourly
      deriving (Eq, Show, Generic)

instance FromText RecurringChargeFrequency where
    parser = match "Hourly" RecurringChargeFrequencyHourly

instance ToText RecurringChargeFrequency where
    toText RecurringChargeFrequencyHourly = "Hourly"

instance ToByteString RecurringChargeFrequency where
    toBS RecurringChargeFrequencyHourly = "Hourly"

instance FromXML RecurringChargeFrequency where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "frequency"

instance ToXML RecurringChargeFrequency where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "frequency"

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

instance ToByteString ReportInstanceReasonCodes where
    toBS ReportInstanceReasonCodesInstanceStuckInState = "instance-stuck-in-state"
    toBS ReportInstanceReasonCodesNotAcceptingCredentials = "not-accepting-credentials"
    toBS ReportInstanceReasonCodesOther = "other"
    toBS ReportInstanceReasonCodesPasswordNotAvailable = "password-not-available"
    toBS ReportInstanceReasonCodesPerformanceEbsVolume = "performance-ebs-volume"
    toBS ReportInstanceReasonCodesPerformanceInstanceStore = "performance-instance-store"
    toBS ReportInstanceReasonCodesPerformanceNetwork = "performance-network"
    toBS ReportInstanceReasonCodesPerformanceOther = "performance-other"
    toBS ReportInstanceReasonCodesUnresponsive = "unresponsive"

instance ToXML ReportInstanceReasonCodes where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ReasonCode"

-- | The status of all instances listed.
data ReportStatusType
    = ReportStatusTypeImpaired -- ^ impaired
    | ReportStatusTypeOk -- ^ ok
      deriving (Eq, Show, Generic)

instance FromText ReportStatusType where
    parser = match "impaired" ReportStatusTypeImpaired
         <|> match "ok" ReportStatusTypeOk

instance ToText ReportStatusType where
    toText ReportStatusTypeImpaired = "impaired"
    toText ReportStatusTypeOk = "ok"

instance ToByteString ReportStatusType where
    toBS ReportStatusTypeImpaired = "impaired"
    toBS ReportStatusTypeOk = "ok"

instance ToXML ReportStatusType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The state of the Reserved Instance purchase.
data ReservedInstanceState
    = ReservedInstanceStateActive -- ^ active
    | ReservedInstanceStatePaymentFailed -- ^ payment-failed
    | ReservedInstanceStatePaymentPending -- ^ payment-pending
    | ReservedInstanceStateRetired -- ^ retired
      deriving (Eq, Show, Generic)

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

instance ToByteString ReservedInstanceState where
    toBS ReservedInstanceStateActive = "active"
    toBS ReservedInstanceStatePaymentFailed = "payment-failed"
    toBS ReservedInstanceStatePaymentPending = "payment-pending"
    toBS ReservedInstanceStateRetired = "retired"

instance FromXML ReservedInstanceState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToXML ReservedInstanceState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "state"

-- | The attribute to reset (currently you can only reset the launch permission
-- attribute).
data ResetImageAttributeName
    = ResetImageAttributeNameLaunchPermission -- ^ launchPermission
      deriving (Eq, Show, Generic)

instance FromText ResetImageAttributeName where
    parser = match "launchPermission" ResetImageAttributeNameLaunchPermission

instance ToText ResetImageAttributeName where
    toText ResetImageAttributeNameLaunchPermission = "launchPermission"

instance ToByteString ResetImageAttributeName where
    toBS ResetImageAttributeNameLaunchPermission = "launchPermission"

instance ToXML ResetImageAttributeName where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

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

instance ToByteString ResourceType where
    toBS ResourceTypeCustomerGateway = "customer-gateway"
    toBS ResourceTypeDhcpOptions = "dhcp-options"
    toBS ResourceTypeImage = "image"
    toBS ResourceTypeInstance = "instance"
    toBS ResourceTypeInternetGateway = "internet-gateway"
    toBS ResourceTypeNetworkAcl = "network-acl"
    toBS ResourceTypeNetworkInterface = "network-interface"
    toBS ResourceTypeReservedInstances = "reserved-instances"
    toBS ResourceTypeRouteTable = "route-table"
    toBS ResourceTypeSecurityGroup = "security-group"
    toBS ResourceTypeSnapshot = "snapshot"
    toBS ResourceTypeSpotInstancesRequest = "spot-instances-request"
    toBS ResourceTypeSubnet = "subnet"
    toBS ResourceTypeVolume = "volume"
    toBS ResourceTypeVpc = "vpc"
    toBS ResourceTypeVpnConnection = "vpn-connection"
    toBS ResourceTypeVpnGateway = "vpn-gateway"

instance FromXML ResourceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "resourceType"

instance ToXML ResourceType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "resourceType"

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

instance FromText RouteOrigin where
    parser = match "CreateRoute" RouteOriginCreateRoute
         <|> match "CreateRouteTable" RouteOriginCreateRouteTable
         <|> match "EnableVgwRoutePropagation" RouteOriginEnableVgwRoutePropagation

instance ToText RouteOrigin where
    toText RouteOriginCreateRoute = "CreateRoute"
    toText RouteOriginCreateRouteTable = "CreateRouteTable"
    toText RouteOriginEnableVgwRoutePropagation = "EnableVgwRoutePropagation"

instance ToByteString RouteOrigin where
    toBS RouteOriginCreateRoute = "CreateRoute"
    toBS RouteOriginCreateRouteTable = "CreateRouteTable"
    toBS RouteOriginEnableVgwRoutePropagation = "EnableVgwRoutePropagation"

instance FromXML RouteOrigin where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "origin"

instance ToXML RouteOrigin where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "origin"

-- | The state of the route. The blackhole state indicates that the route's
-- target isn't available (for example, the specified gateway isn't attached
-- to the VPC, or the specified NAT instance has been terminated).
data RouteState
    = RouteStateActive -- ^ active
    | RouteStateBlackhole -- ^ blackhole
      deriving (Eq, Show, Generic)

instance FromText RouteState where
    parser = match "active" RouteStateActive
         <|> match "blackhole" RouteStateBlackhole

instance ToText RouteState where
    toText RouteStateActive = "active"
    toText RouteStateBlackhole = "blackhole"

instance ToByteString RouteState where
    toBS RouteStateActive = "active"
    toBS RouteStateBlackhole = "blackhole"

instance FromXML RouteState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToXML RouteState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "state"

-- | Indicates whether to allow or deny the traffic that matches the rule.
data RuleAction
    = RuleActionAllow -- ^ allow
    | RuleActionDeny -- ^ deny
      deriving (Eq, Show, Generic)

instance FromText RuleAction where
    parser = match "allow" RuleActionAllow
         <|> match "deny" RuleActionDeny

instance ToText RuleAction where
    toText RuleActionAllow = "allow"
    toText RuleActionDeny = "deny"

instance ToByteString RuleAction where
    toBS RuleActionAllow = "allow"
    toBS RuleActionDeny = "deny"

instance FromXML RuleAction where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ruleAction"

instance ToXML RuleAction where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ruleAction"

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown).
data ShutdownBehavior
    = ShutdownBehaviorStop -- ^ stop
    | ShutdownBehaviorTerminate -- ^ terminate
      deriving (Eq, Show, Generic)

instance FromText ShutdownBehavior where
    parser = match "stop" ShutdownBehaviorStop
         <|> match "terminate" ShutdownBehaviorTerminate

instance ToText ShutdownBehavior where
    toText ShutdownBehaviorStop = "stop"
    toText ShutdownBehaviorTerminate = "terminate"

instance ToByteString ShutdownBehavior where
    toBS ShutdownBehaviorStop = "stop"
    toBS ShutdownBehaviorTerminate = "terminate"

instance ToXML ShutdownBehavior where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The snapshot attribute you would like to view.
data SnapshotAttributeName
    = SnapshotAttributeNameCreateVolumePermission -- ^ createVolumePermission
    | SnapshotAttributeNameProductCodes -- ^ productCodes
      deriving (Eq, Show, Generic)

instance FromText SnapshotAttributeName where
    parser = match "createVolumePermission" SnapshotAttributeNameCreateVolumePermission
         <|> match "productCodes" SnapshotAttributeNameProductCodes

instance ToText SnapshotAttributeName where
    toText SnapshotAttributeNameCreateVolumePermission = "createVolumePermission"
    toText SnapshotAttributeNameProductCodes = "productCodes"

instance ToByteString SnapshotAttributeName where
    toBS SnapshotAttributeNameCreateVolumePermission = "createVolumePermission"
    toBS SnapshotAttributeNameProductCodes = "productCodes"

instance ToXML SnapshotAttributeName where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The snapshot state.
data SnapshotState
    = SnapshotStateCompleted -- ^ completed
    | SnapshotStateError -- ^ error
    | SnapshotStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance FromText SnapshotState where
    parser = match "completed" SnapshotStateCompleted
         <|> match "error" SnapshotStateError
         <|> match "pending" SnapshotStatePending

instance ToText SnapshotState where
    toText SnapshotStateCompleted = "completed"
    toText SnapshotStateError = "error"
    toText SnapshotStatePending = "pending"

instance ToByteString SnapshotState where
    toBS SnapshotStateCompleted = "completed"
    toBS SnapshotStateError = "error"
    toBS SnapshotStatePending = "pending"

instance FromXML SnapshotState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToXML SnapshotState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "status"

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

instance ToByteString SpotInstanceState where
    toBS SpotInstanceStateActive = "active"
    toBS SpotInstanceStateCancelled = "cancelled"
    toBS SpotInstanceStateClosed = "closed"
    toBS SpotInstanceStateFailed = "failed"
    toBS SpotInstanceStateOpen = "open"

instance FromXML SpotInstanceState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToXML SpotInstanceState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "state"

-- | The Spot Instance request type. Default: one-time.
data SpotInstanceType
    = SpotInstanceTypeOneTime -- ^ one-time
    | SpotInstanceTypePersistent -- ^ persistent
      deriving (Eq, Show, Generic)

instance FromText SpotInstanceType where
    parser = match "one-time" SpotInstanceTypeOneTime
         <|> match "persistent" SpotInstanceTypePersistent

instance ToText SpotInstanceType where
    toText SpotInstanceTypeOneTime = "one-time"
    toText SpotInstanceTypePersistent = "persistent"

instance ToByteString SpotInstanceType where
    toBS SpotInstanceTypeOneTime = "one-time"
    toBS SpotInstanceTypePersistent = "persistent"

instance FromXML SpotInstanceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML SpotInstanceType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The type of instance status.
data StatusName
    = StatusNameReachability -- ^ reachability
      deriving (Eq, Show, Generic)

instance FromText StatusName where
    parser = match "reachability" StatusNameReachability

instance ToText StatusName where
    toText StatusNameReachability = "reachability"

instance ToByteString StatusName where
    toBS StatusNameReachability = "reachability"

instance FromXML StatusName where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "name"

instance ToXML StatusName where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "name"

-- | The status.
data StatusType
    = StatusTypeFailed -- ^ failed
    | StatusTypeInsufficientData -- ^ insufficient-data
    | StatusTypePassed -- ^ passed
      deriving (Eq, Show, Generic)

instance FromText StatusType where
    parser = match "failed" StatusTypeFailed
         <|> match "insufficient-data" StatusTypeInsufficientData
         <|> match "passed" StatusTypePassed

instance ToText StatusType where
    toText StatusTypeFailed = "failed"
    toText StatusTypeInsufficientData = "insufficient-data"
    toText StatusTypePassed = "passed"

instance ToByteString StatusType where
    toBS StatusTypeFailed = "failed"
    toBS StatusTypeInsufficientData = "insufficient-data"
    toBS StatusTypePassed = "passed"

instance FromXML StatusType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToXML StatusType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "status"

-- | The type of virtualization.
data String
    = StringHvm -- ^ hvm
    | StringParavirtual -- ^ paravirtual
      deriving (Eq, Show, Generic)

instance FromText String where
    parser = match "hvm" StringHvm
         <|> match "paravirtual" StringParavirtual

instance ToText String where
    toText StringHvm = "hvm"
    toText StringParavirtual = "paravirtual"

instance ToByteString String where
    toBS StringHvm = "hvm"
    toBS StringParavirtual = "paravirtual"

instance ToXML String where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The current state of the subnet.
data SubnetState
    = SubnetStateAvailable -- ^ available
    | SubnetStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance FromText SubnetState where
    parser = match "available" SubnetStateAvailable
         <|> match "pending" SubnetStatePending

instance ToText SubnetState where
    toText SubnetStateAvailable = "available"
    toText SubnetStatePending = "pending"

instance ToByteString SubnetState where
    toBS SubnetStateAvailable = "available"
    toBS SubnetStatePending = "pending"

instance FromXML SubnetState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToXML SubnetState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "state"

-- | The status.
data SummaryStatus
    = SummaryStatusImpaired -- ^ impaired
    | SummaryStatusInsufficientData -- ^ insufficient-data
    | SummaryStatusNotApplicable -- ^ not-applicable
    | SummaryStatusOk -- ^ ok
      deriving (Eq, Show, Generic)

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

instance ToByteString SummaryStatus where
    toBS SummaryStatusImpaired = "impaired"
    toBS SummaryStatusInsufficientData = "insufficient-data"
    toBS SummaryStatusNotApplicable = "not-applicable"
    toBS SummaryStatusOk = "ok"

instance FromXML SummaryStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToXML SummaryStatus where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "status"

-- | The status of the VPN tunnel.
data TelemetryStatus
    = TelemetryStatusDown -- ^ DOWN
    | TelemetryStatusUp -- ^ UP
      deriving (Eq, Show, Generic)

instance FromText TelemetryStatus where
    parser = match "DOWN" TelemetryStatusDown
         <|> match "UP" TelemetryStatusUp

instance ToText TelemetryStatus where
    toText TelemetryStatusDown = "DOWN"
    toText TelemetryStatusUp = "UP"

instance ToByteString TelemetryStatus where
    toBS TelemetryStatusDown = "DOWN"
    toBS TelemetryStatusUp = "UP"

instance FromXML TelemetryStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToXML TelemetryStatus where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "status"

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of dedicated runs on single-tenant hardware.
data Tenancy
    = TenancyDedicated -- ^ dedicated
    | TenancyDefault -- ^ default
      deriving (Eq, Show, Generic)

instance FromText Tenancy where
    parser = match "dedicated" TenancyDedicated
         <|> match "default" TenancyDefault

instance ToText Tenancy where
    toText TenancyDedicated = "dedicated"
    toText TenancyDefault = "default"

instance ToByteString Tenancy where
    toBS TenancyDedicated = "dedicated"
    toBS TenancyDefault = "default"

instance FromXML Tenancy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML Tenancy where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The virtualization type of the instance.
data VirtualizationType
    = VirtualizationTypeHvm -- ^ hvm
    | VirtualizationTypeParavirtual -- ^ paravirtual
      deriving (Eq, Show, Generic)

instance FromText VirtualizationType where
    parser = match "hvm" VirtualizationTypeHvm
         <|> match "paravirtual" VirtualizationTypeParavirtual

instance ToText VirtualizationType where
    toText VirtualizationTypeHvm = "hvm"
    toText VirtualizationTypeParavirtual = "paravirtual"

instance ToByteString VirtualizationType where
    toBS VirtualizationTypeHvm = "hvm"
    toBS VirtualizationTypeParavirtual = "paravirtual"

instance FromXML VirtualizationType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "virtualizationType"

instance ToXML VirtualizationType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "virtualizationType"

-- | The attachment state of the volume.
data VolumeAttachmentState
    = VolumeAttachmentStateAttached -- ^ attached
    | VolumeAttachmentStateAttaching -- ^ attaching
    | VolumeAttachmentStateDetached -- ^ detached
    | VolumeAttachmentStateDetaching -- ^ detaching
      deriving (Eq, Show, Generic)

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

instance ToByteString VolumeAttachmentState where
    toBS VolumeAttachmentStateAttached = "attached"
    toBS VolumeAttachmentStateAttaching = "attaching"
    toBS VolumeAttachmentStateDetached = "detached"
    toBS VolumeAttachmentStateDetaching = "detaching"

instance FromXML VolumeAttachmentState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToXML VolumeAttachmentState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "status"

-- | The instance attribute.
data VolumeAttributeName
    = VolumeAttributeNameAutoEnableIO -- ^ autoEnableIO
    | VolumeAttributeNameProductCodes -- ^ productCodes
      deriving (Eq, Show, Generic)

instance FromText VolumeAttributeName where
    parser = match "autoEnableIO" VolumeAttributeNameAutoEnableIO
         <|> match "productCodes" VolumeAttributeNameProductCodes

instance ToText VolumeAttributeName where
    toText VolumeAttributeNameAutoEnableIO = "autoEnableIO"
    toText VolumeAttributeNameProductCodes = "productCodes"

instance ToByteString VolumeAttributeName where
    toBS VolumeAttributeNameAutoEnableIO = "autoEnableIO"
    toBS VolumeAttributeNameProductCodes = "productCodes"

instance ToXML VolumeAttributeName where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The volume state.
data VolumeState
    = VolumeStateAvailable -- ^ available
    | VolumeStateCreating -- ^ creating
    | VolumeStateDeleted -- ^ deleted
    | VolumeStateDeleting -- ^ deleting
    | VolumeStateError -- ^ error
    | VolumeStateInUse -- ^ in-use
      deriving (Eq, Show, Generic)

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

instance ToByteString VolumeState where
    toBS VolumeStateAvailable = "available"
    toBS VolumeStateCreating = "creating"
    toBS VolumeStateDeleted = "deleted"
    toBS VolumeStateDeleting = "deleting"
    toBS VolumeStateError = "error"
    toBS VolumeStateInUse = "in-use"

instance FromXML VolumeState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToXML VolumeState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "status"

-- | The status of the volume.
data VolumeStatusInfoStatus
    = VolumeStatusInfoStatusImpaired -- ^ impaired
    | VolumeStatusInfoStatusInsufficientData -- ^ insufficient-data
    | VolumeStatusInfoStatusOk -- ^ ok
      deriving (Eq, Show, Generic)

instance FromText VolumeStatusInfoStatus where
    parser = match "impaired" VolumeStatusInfoStatusImpaired
         <|> match "insufficient-data" VolumeStatusInfoStatusInsufficientData
         <|> match "ok" VolumeStatusInfoStatusOk

instance ToText VolumeStatusInfoStatus where
    toText VolumeStatusInfoStatusImpaired = "impaired"
    toText VolumeStatusInfoStatusInsufficientData = "insufficient-data"
    toText VolumeStatusInfoStatusOk = "ok"

instance ToByteString VolumeStatusInfoStatus where
    toBS VolumeStatusInfoStatusImpaired = "impaired"
    toBS VolumeStatusInfoStatusInsufficientData = "insufficient-data"
    toBS VolumeStatusInfoStatusOk = "ok"

instance FromXML VolumeStatusInfoStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToXML VolumeStatusInfoStatus where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "status"

-- | The name of the volume status.
data VolumeStatusName
    = VolumeStatusNameIoEnabled -- ^ io-enabled
    | VolumeStatusNameIoPerformance -- ^ io-performance
      deriving (Eq, Show, Generic)

instance FromText VolumeStatusName where
    parser = match "io-enabled" VolumeStatusNameIoEnabled
         <|> match "io-performance" VolumeStatusNameIoPerformance

instance ToText VolumeStatusName where
    toText VolumeStatusNameIoEnabled = "io-enabled"
    toText VolumeStatusNameIoPerformance = "io-performance"

instance ToByteString VolumeStatusName where
    toBS VolumeStatusNameIoEnabled = "io-enabled"
    toBS VolumeStatusNameIoPerformance = "io-performance"

instance FromXML VolumeStatusName where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "name"

instance ToXML VolumeStatusName where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "name"

-- | The volume type. Default: standard.
data VolumeType
    = VolumeTypeIo1 -- ^ io1
    | VolumeTypeStandard -- ^ standard
      deriving (Eq, Show, Generic)

instance FromText VolumeType where
    parser = match "io1" VolumeTypeIo1
         <|> match "standard" VolumeTypeStandard

instance ToText VolumeType where
    toText VolumeTypeIo1 = "io1"
    toText VolumeTypeStandard = "standard"

instance ToByteString VolumeType where
    toBS VolumeTypeIo1 = "io1"
    toBS VolumeTypeStandard = "standard"

instance FromXML VolumeType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML VolumeType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The VPC attribute.
data VpcAttributeName
    = VpcAttributeNameEnableDnsHostnames -- ^ enableDnsHostnames
    | VpcAttributeNameEnableDnsSupport -- ^ enableDnsSupport
      deriving (Eq, Show, Generic)

instance FromText VpcAttributeName where
    parser = match "enableDnsHostnames" VpcAttributeNameEnableDnsHostnames
         <|> match "enableDnsSupport" VpcAttributeNameEnableDnsSupport

instance ToText VpcAttributeName where
    toText VpcAttributeNameEnableDnsHostnames = "enableDnsHostnames"
    toText VpcAttributeNameEnableDnsSupport = "enableDnsSupport"

instance ToByteString VpcAttributeName where
    toBS VpcAttributeNameEnableDnsHostnames = "enableDnsHostnames"
    toBS VpcAttributeNameEnableDnsSupport = "enableDnsSupport"

instance ToXML VpcAttributeName where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The current state of the VPC.
data VpcState
    = VpcStateAvailable -- ^ available
    | VpcStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance FromText VpcState where
    parser = match "available" VpcStateAvailable
         <|> match "pending" VpcStatePending

instance ToText VpcState where
    toText VpcStateAvailable = "available"
    toText VpcStatePending = "pending"

instance ToByteString VpcState where
    toBS VpcStateAvailable = "available"
    toBS VpcStatePending = "pending"

instance FromXML VpcState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToXML VpcState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "state"

-- | The current state of the virtual private gateway.
data VpnState
    = VpnStateAvailable -- ^ available
    | VpnStateDeleted -- ^ deleted
    | VpnStateDeleting -- ^ deleting
    | VpnStatePending -- ^ pending
      deriving (Eq, Show, Generic)

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

instance ToByteString VpnState where
    toBS VpnStateAvailable = "available"
    toBS VpnStateDeleted = "deleted"
    toBS VpnStateDeleting = "deleting"
    toBS VpnStatePending = "pending"

instance FromXML VpnState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToXML VpnState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "state"

-- | Indicates how the routes were provided.
data VpnStaticRouteSource
    = VpnStaticRouteSourceStatic -- ^ Static
      deriving (Eq, Show, Generic)

instance FromText VpnStaticRouteSource where
    parser = match "Static" VpnStaticRouteSourceStatic

instance ToText VpnStaticRouteSource where
    toText VpnStaticRouteSourceStatic = "Static"

instance ToByteString VpnStaticRouteSource where
    toBS VpnStaticRouteSourceStatic = "Static"

instance FromXML VpnStaticRouteSource where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "source"

instance ToXML VpnStaticRouteSource where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "source"

-- | Describes a value of an account attribute.
newtype AccountAttributeValue = AccountAttributeValue
    { _aavAttributeValue :: Text
      -- ^ The value.
    } deriving (Generic)

instance FromXML AccountAttributeValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML AccountAttributeValue where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Indicates whether source/destination checking is enabled. A value of true
-- means checking is enabled, and false means checking is disabled. This value
-- must be false for a NAT instance to perform NAT. For more information, see
-- NAT Instances in the Amazon Virtual Private Cloud User Guide.
newtype AttributeBooleanValue = AttributeBooleanValue
    { _abvValue :: Bool
      -- ^ 
    } deriving (Generic)

instance FromXML AttributeBooleanValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML AttributeBooleanValue where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | A description for the network interface.
newtype AttributeValue = AttributeValue
    { _avValue :: Text
      -- ^ 
    } deriving (Generic)

instance FromXML AttributeValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML AttributeValue where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | Describes a message about an Availability Zone.
newtype AvailabilityZoneMessage = AvailabilityZoneMessage
    { _azmMessage :: Text
      -- ^ The message about the Availability Zone.
    } deriving (Generic)

instance FromXML AvailabilityZoneMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML AvailabilityZoneMessage where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes an IP range.
newtype IpRange = IpRange
    { _irCidrIp :: Text
      -- ^ The CIDR range. You can either specify a CIDR range or a source
      -- security group, not both.
    } deriving (Generic)

instance FromXML IpRange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML IpRange where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The monitoring information for the instance.
newtype Monitoring = Monitoring
    { _mState :: MonitoringState
      -- ^ Indicates whether monitoring is enabled for the instance.
    } deriving (Generic)

instance FromXML Monitoring where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "monitoring"

instance ToXML Monitoring where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "monitoring"

-- | Describes a virtual private gateway propagating route.
newtype PropagatingVgw = PropagatingVgw
    { _pvGatewayId :: Text
      -- ^ The ID of the virtual private gateway (VGW).
    } deriving (Generic)

instance FromXML PropagatingVgw where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML PropagatingVgw where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes the ID of a Reserved Instance.
newtype ReservedInstancesId = ReservedInstancesId
    { _riiReservedInstancesId :: Text
      -- ^ The ID of the Reserved Instance.
    } deriving (Generic)

instance FromXML ReservedInstancesId where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML ReservedInstancesId where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | The monitoring for the instance.
newtype RunInstancesMonitoringEnabled = RunInstancesMonitoringEnabled
    { _rimeEnabled :: Bool
      -- ^ Indicates whether monitoring is enabled for the instance.
    } deriving (Generic)

instance ToXML RunInstancesMonitoringEnabled where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The Amazon S3 storage locations.
newtype Storage = Storage
    { _sS3 :: S3Storage
      -- ^ An Amazon S3 storage location.
    } deriving (Generic)

instance FromXML Storage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "storage"

instance ToXML Storage where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "storage"

-- | 
newtype VolumeDetail = VolumeDetail
    { _vdSize :: Integer
      -- ^ The size of the volume.
    } deriving (Generic)

instance FromXML VolumeDetail where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML VolumeDetail where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The VPN connection options.
newtype VpnConnectionOptions = VpnConnectionOptions
    { _vcoStaticRoutesOnly :: Bool
      -- ^ Indicates whether the VPN connection uses static routes only.
      -- Static routes must be used for devices that don't support BGP.
    } deriving (Generic)

instance FromXML VpnConnectionOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "options"

instance ToXML VpnConnectionOptions where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "options"

-- | Indicates whether the VPN connection requires static routes. If you are
-- creating a VPN connection for a device that does not support BGP, you must
-- specify true. Default: false.
newtype VpnConnectionOptionsSpecification = VpnConnectionOptionsSpecification
    { _vcosStaticRoutesOnly :: Bool
      -- ^ Indicates whether the VPN connection uses static routes only.
      -- Static routes must be used for devices that don't support BGP.
    } deriving (Generic)

instance ToXML VpnConnectionOptionsSpecification where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | Describes an account attribute.
data AccountAttribute = AccountAttribute
    { _aaAttributeValues :: [AccountAttributeValue]
      -- ^ One or more values for the account attribute.
    , _aaAttributeName :: Text
      -- ^ The name of the account attribute.
    } deriving (Generic)

instance FromXML AccountAttribute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes an Elastic IP address.
data Address = Address
    { _aAssociationId :: Text
      -- ^ The ID representing the association of the address with an
      -- instance in a VPC.
    , _aInstanceId :: Text
      -- ^ The ID of the instance the address is associated with (if any).
    , _aNetworkInterfaceOwnerId :: Text
      -- ^ The ID of the AWS account that owns the network interface.
    , _aAllocationId :: Text
      -- ^ The ID representing the allocation of the address for use with
      -- EC2-VPC.
    , _aDomain :: DomainType
      -- ^ Indicates whether this Elastic IP address is for use with
      -- instances in EC2-Classic (standard) or instances in a VPC (vpc).
    , _aNetworkInterfaceId :: Text
      -- ^ The ID of the network interface.
    , _aPrivateIpAddress :: Text
      -- ^ The private IP address associated with the Elastic IP address.
    , _aPublicIp :: Text
      -- ^ The Elastic IP address.
    } deriving (Generic)

instance FromXML Address where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes an Availability Zone.
data AvailabilityZone = AvailabilityZone
    { _azState :: AvailabilityZoneState
      -- ^ The state of the Availability Zone.
    , _azRegionName :: Text
      -- ^ The name of the region.
    , _azZoneName :: Text
      -- ^ The name of the Availability Zone.
    , _azMessages :: [AvailabilityZoneMessage]
      -- ^ Any messages about the Availability Zone.
    } deriving (Generic)

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a block device mapping.
data BlockDeviceMapping = BlockDeviceMapping
    { _bdmVirtualName :: Text
      -- ^ The virtual device name.
    , _bdmNoDevice :: Text
      -- ^ Suppresses the specified device included in the block device
      -- mapping of the AMI.
    , _bdmEbs :: EbsBlockDevice
      -- ^ Parameters used to automatically set up Amazon EBS volumes when
      -- the instance is launched.
    , _bdmDeviceName :: Text
      -- ^ The device name exposed to the instance (for example, /dev/sdh).
    } deriving (Generic)

instance FromXML BlockDeviceMapping where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BlockDeviceMapping"

instance ToXML BlockDeviceMapping where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "BlockDeviceMapping"

-- | The bundle task.
data BundleTask = BundleTask
    { _btInstanceId :: Text
      -- ^ The ID of the instance associated with this bundle task.
    , _btState :: BundleTaskState
      -- ^ The state of the task.
    , _btProgress :: Text
      -- ^ The level of task completion, as a percent (for example, 20%).
    , _btStartTime :: ISO8601
      -- ^ The time this task started.
    , _btBundleId :: Text
      -- ^ The ID for this bundle task.
    , _btStorage :: Storage
      -- ^ The Amazon S3 storage locations.
    , _btUpdateTime :: ISO8601
      -- ^ The time of the most recent update for the task.
    , _btBundleTaskError :: BundleTaskError
      -- ^ If the task fails, a description of the error.
    } deriving (Generic)

instance FromXML BundleTask where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "bundleInstanceTask"

-- | If the task fails, a description of the error.
data BundleTaskError = BundleTaskError
    { _bteCode :: Text
      -- ^ The error code.
    , _bteMessage :: Text
      -- ^ The error message.
    } deriving (Generic)

instance FromXML BundleTaskError where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "error"

instance ToXML BundleTaskError where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "error"

-- | Describes a request to cancel a Spot Instance.
data CancelledSpotInstanceRequest = CancelledSpotInstanceRequest
    { _csirState :: CancelSpotInstanceRequestState
      -- ^ The state of the Spot Instance request.
    , _csirSpotInstanceRequestId :: Text
      -- ^ The ID of the Spot Instance request.
    } deriving (Generic)

instance FromXML CancelledSpotInstanceRequest where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | 
data ConversionTask = ConversionTask
    { _ctImportInstance :: ImportInstanceTaskDetails
      -- ^ If the task is for importing an instance, this contains
      -- information about the import instance task.
    , _ctState :: ConversionTaskState
      -- ^ The state of the conversion task.
    , _ctStatusMessage :: Text
      -- ^ The status message related to the conversion task.
    , _ctImportVolume :: ImportVolumeTaskDetails
      -- ^ If the task is for importing a volume, this contains information
      -- about the import volume task.
    , _ctConversionTaskId :: Text
      -- ^ The ID of the conversion task.
    , _ctExpirationTime :: Text
      -- ^ The time when the task expires. If the upload isn't complete
      -- before the expiration time, we automatically cancel the task.
    , _ctTags :: [Tag]
      -- ^ 
    } deriving (Generic)

instance FromXML ConversionTask where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "conversionTask"

-- | 
data CreateVolumePermission = CreateVolumePermission
    { _cvpGroup :: PermissionGroup
      -- ^ The specific group that is to be added or removed from a volume's
      -- list of create volume permissions.
    , _cvpUserId :: Text
      -- ^ The specific AWS account ID that is to be added or removed from a
      -- volume's list of create volume permissions.
    } deriving (Generic)

instance FromXML CreateVolumePermission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML CreateVolumePermission where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | A JSON representation of the snapshot attribute modification.
data CreateVolumePermissionModifications = CreateVolumePermissionModifications
    { _cvpmRemove :: [CreateVolumePermission]
      -- ^ Removes a specific AWS account ID or group from a volume's list
      -- of create volume permissions.
    , _cvpmAdd :: [CreateVolumePermission]
      -- ^ Adds a specific AWS account ID or group to a volume's list of
      -- create volume permissions.
    } deriving (Generic)

instance ToXML CreateVolumePermissionModifications where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | Information about the customer gateway.
data CustomerGateway = CustomerGateway
    { _cgState :: Text
      -- ^ The current state of the customer gateway.
    , _cgIpAddress :: Text
      -- ^ The Internet-routable IP address of the customer gateway's
      -- outside interface.
    , _cgBgpAsn :: Text
      -- ^ The customer gateway's Border Gateway Protocol (BGP) Autonomous
      -- System Number (ASN).
    , _cgCustomerGatewayId :: Text
      -- ^ The ID of the customer gateway.
    , _cgType :: Text
      -- ^ The type of VPN connection the customer gateway supports.
    , _cgTags :: [Tag]
      -- ^ Any tags assigned to the customer gateway.
    } deriving (Generic)

instance FromXML CustomerGateway where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "customerGateway"

-- | Describes a DHCP configuration option.
data DhcpConfiguration = DhcpConfiguration
    { _dcValues :: [Text]
      -- ^ One or more values for the DHCP option.
    , _dcKey :: Text
      -- ^ The name of a DHCP option.
    } deriving (Generic)

instance FromXML DhcpConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML DhcpConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes a set of DHCP options.
data DhcpOptions = DhcpOptions
    { _doDhcpConfigurations :: [DhcpConfiguration]
      -- ^ One or more DHCP options in the set.
    , _doDhcpOptionsId :: Text
      -- ^ The ID of the set of DHCP options.
    , _doTags :: [Tag]
      -- ^ Any tags assigned to the DHCP options set.
    } deriving (Generic)

instance FromXML DhcpOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a disk image.
data DiskImage = DiskImage
    { _diImage :: DiskImageDetail
      -- ^ 
    , _diVolume :: VolumeDetail
      -- ^ 
    , _diDescription :: Text
      -- ^ 
    } deriving (Generic)

instance ToXML DiskImage where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DiskImage"

-- | The image.
data DiskImageDescription = DiskImageDescription
    { _didSize :: Integer
      -- ^ The size of the disk image.
    , _didChecksum :: Text
      -- ^ The checksum computed for the disk image.
    , _didFormat :: DiskImageFormat
      -- ^ The disk image format.
    , _didImportManifestUrl :: Text
      -- ^ A presigned URL for the import manifest stored in Amazon S3. For
      -- information about creating a presigned URL for an Amazon S3
      -- object, read the "Query String Request Authentication
      -- Alternative" section of the Authenticating REST Requests topic in
      -- the Amazon Simple Storage Service Developer Guide.
    } deriving (Generic)

instance FromXML DiskImageDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "image"

instance ToXML DiskImageDescription where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "image"

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
    } deriving (Generic)

instance FromXML DiskImageDetail where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML DiskImageDetail where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The volume.
data DiskImageVolumeDescription = DiskImageVolumeDescription
    { _divdSize :: Integer
      -- ^ The size of the volume.
    , _divdId :: Text
      -- ^ The volume identifier.
    } deriving (Generic)

instance FromXML DiskImageVolumeDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "volume"

instance ToXML DiskImageVolumeDescription where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "volume"

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
data EbsBlockDevice = EbsBlockDevice
    { _ebdDeleteOnTermination :: Bool
      -- ^ Indicates whether the Amazon EBS volume is deleted on instance
      -- termination.
    , _ebdVolumeSize :: Integer
      -- ^ The size of the volume, in GiB. Constraints: If the volume type
      -- is io1, the minimum size of the volume is 10 GiB. Default: If
      -- you're creating the volume from a snapshot and don't specify a
      -- volume size, the default is the snapshot size.
    , _ebdIops :: Integer
      -- ^ The number of I/O operations per second (IOPS) that the volume
      -- supports. Constraint: Range is 100 to 4000. Condition: Required
      -- when the volume type is io1; not used with standard volumes.
    , _ebdEncrypted :: Bool
      -- ^ Indicates whether the Amazon EBS volume is encrypted.
    , _ebdVolumeType :: VolumeType
      -- ^ The volume type. Default: standard.
    , _ebdSnapshotId :: Text
      -- ^ The ID of the snapshot.
    } deriving (Generic)

instance FromXML EbsBlockDevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML EbsBlockDevice where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
data EbsInstanceBlockDevice = EbsInstanceBlockDevice
    { _eibdStatus :: AttachmentStatus
      -- ^ The attachment state.
    , _eibdDeleteOnTermination :: Bool
      -- ^ Indicates whether the volume is deleted on instance termination.
    , _eibdVolumeId :: Text
      -- ^ The ID of the Amazon EBS volume.
    , _eibdAttachTime :: ISO8601
      -- ^ The time stamp when the attachment initiated.
    } deriving (Generic)

instance FromXML EbsInstanceBlockDevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ebs"

instance ToXML EbsInstanceBlockDevice where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ebs"

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
data EbsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification
    { _eibdsDeleteOnTermination :: Bool
      -- ^ Indicates whether the volume is deleted on instance termination.
    , _eibdsVolumeId :: Text
      -- ^ The ID of the Amazon EBS volume.
    } deriving (Generic)

instance FromXML EbsInstanceBlockDeviceSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML EbsInstanceBlockDeviceSpecification where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | 
data ExportTask = ExportTask
    { _etExportTaskId :: Text
      -- ^ The ID of the export task.
    , _etState :: ExportTaskState
      -- ^ The state of the conversion task.
    , _etExportToS3Task :: ExportToS3Task
      -- ^ 
    , _etInstanceExportDetails :: InstanceExportDetails
      -- ^ The instance being exported.
    , _etStatusMessage :: Text
      -- ^ The status message related to the export task.
    , _etDescription :: Text
      -- ^ A description of the resource being exported.
    } deriving (Generic)

instance FromXML ExportTask where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "exportTask"

-- | 
data ExportToS3Task = ExportToS3Task
    { _etstS3Key :: Text
      -- ^ 
    , _etstContainerFormat :: ContainerFormat
      -- ^ The container format used to combine disk images with metadata
      -- (such as OVF). If absent, only the disk image is exported.
    , _etstS3Bucket :: Text
      -- ^ The Amazon S3 bucket for the destination image. The destination
      -- bucket must exist and grant WRITE and READ_ACL permissions to the
      -- AWS account vm-import-export@amazon.com.
    , _etstDiskImageFormat :: DiskImageFormat
      -- ^ The format for the exported image.
    } deriving (Generic)

instance FromXML ExportToS3Task where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "exportToS3"

instance ToXML ExportToS3Task where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "exportToS3"

-- | 
data ExportToS3TaskSpecification = ExportToS3TaskSpecification
    { _etstsContainerFormat :: ContainerFormat
      -- ^ 
    , _etstsS3Prefix :: Text
      -- ^ The image is written to a single object in the Amazon S3 bucket
      -- at the S3 key s3prefix + exportTaskId + '.' + diskImageFormat.
    , _etstsS3Bucket :: Text
      -- ^ 
    , _etstsDiskImageFormat :: DiskImageFormat
      -- ^ 
    } deriving (Generic)

instance ToXML ExportToS3TaskSpecification where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ExportToS3"

-- | 
data Filter = Filter
    { _fValues :: [Text]
      -- ^ One or more filter values.
    , _fName :: Text
      -- ^ The name of the filter.
    } deriving (Generic)

instance ToXML Filter where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Filter"

-- | Describes a security group.
data GroupIdentifier = GroupIdentifier
    { _giGroupId :: Text
      -- ^ The ID of the security group.
    , _giGroupName :: Text
      -- ^ The name of the security group.
    } deriving (Generic)

instance FromXML GroupIdentifier where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML GroupIdentifier where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | The IAM instance profile associated with the instance.
data IamInstanceProfile = IamInstanceProfile
    { _iipArn :: Text
      -- ^ The Amazon Resource Name (ARN) of the instance profile.
    , _iipId :: Text
      -- ^ The ID of the instance profile.
    } deriving (Generic)

instance FromXML IamInstanceProfile where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "iamInstanceProfile"

instance ToXML IamInstanceProfile where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "iamInstanceProfile"

-- | The IAM instance profile.
data IamInstanceProfileSpecification = IamInstanceProfileSpecification
    { _iipsArn :: Text
      -- ^ The Amazon Resource Name (ARN) of the instance profile.
    , _iipsName :: Text
      -- ^ The name of the instance profile.
    } deriving (Generic)

instance FromXML IamInstanceProfileSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML IamInstanceProfileSpecification where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | ICMP protocol: The ICMP type and code.
data IcmpTypeCode = IcmpTypeCode
    { _itcCode :: Integer
      -- ^ The ICMP type. A value of -1 means all types.
    , _itcType :: Integer
      -- ^ The ICMP code. A value of -1 means all codes for the specified
      -- ICMP type.
    } deriving (Generic)

instance FromXML IcmpTypeCode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "icmpTypeCode"

instance ToXML IcmpTypeCode where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "icmpTypeCode"

-- | Describes an image.
data Image = Image
    { _iState :: ImageState
      -- ^ The current state of the AMI. If the state is available, the
      -- image is successfully registered and can be used to launch an
      -- instance.
    , _iVirtualizationType :: VirtualizationType
      -- ^ The type of virtualization of the AMI.
    , _iHypervisor :: HypervisorType
      -- ^ The hypervisor type of the image.
    , _iPlatform :: PlatformValues
      -- ^ The value is Windows for Windows AMIs; otherwise blank.
    , _iImageLocation :: Text
      -- ^ The location of the AMI.
    , _iImageOwnerAlias :: Text
      -- ^ The AWS account alias (for example, amazon, self) or the AWS
      -- account ID of the AMI owner.
    , _iRamdiskId :: Text
      -- ^ The RAM disk associated with the image, if any. Only applicable
      -- for machine images.
    , _iKernelId :: Text
      -- ^ The kernel associated with the image, if any. Only applicable for
      -- machine images.
    , _iRootDeviceName :: Text
      -- ^ The device name of the root device (for example, /dev/sda1 or
      -- xvda).
    , _iSriovNetSupport :: Text
      -- ^ Specifies whether enhanced networking is enabled.
    , _iOwnerId :: Text
      -- ^ The AWS account ID of the image owner.
    , _iImageType :: ImageTypeValues
      -- ^ The type of image.
    , _iName :: Text
      -- ^ The name of the AMI that was provided during image creation.
    , _iImageId :: Text
      -- ^ The ID of the AMI.
    , _iArchitecture :: ArchitectureValues
      -- ^ The architecture of the image.
    , _iProductCodes :: [ProductCode]
      -- ^ Any product codes associated with the AMI.
    , _iStateReason :: StateReason
      -- ^ The reason for the state change.
    , _iRootDeviceType :: DeviceType
      -- ^ The type of root device used by the AMI. The AMI can use an
      -- Amazon EBS volume or an instance store volume.
    , _iDescription :: Text
      -- ^ The description of the AMI that was provided during image
      -- creation.
    , _iBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ Any block device mapping entries.
    , _iTags :: [Tag]
      -- ^ Any tags assigned to the image.
    , _iPublic :: Bool
      -- ^ Indicates whether the image has public launch permissions. The
      -- value is true if this image has public launch permissions or
      -- false if it has only implicit and explicit launch permissions.
    } deriving (Generic)

instance FromXML Image where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | 
data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification
    { _iilsAdditionalInfo :: Text
      -- ^ 
    , _iilsGroupNames :: [Text]
      -- ^ One or more security group names.
    , _iilsSubnetId :: Text
      -- ^ [EC2-VPC] The ID of the subnet to launch the instance into.
    , _iilsInstanceType :: InstanceType
      -- ^ The instance type. For more information, see Instance Types in
      -- the Amazon Elastic Compute Cloud User Guide.
    , _iilsUserData :: Text
      -- ^ User data to be made available to the instance.
    , _iilsMonitoring :: Bool
      -- ^ 
    , _iilsPrivateIpAddress :: Text
      -- ^ [EC2-VPC] Optionally, you can use this parameter to assign the
      -- instance a specific available IP address from the IP address
      -- range of the subnet.
    , _iilsInstanceInitiatedShutdownBehavior :: ShutdownBehavior
      -- ^ Indicates whether an instance stops or terminates when you
      -- initiate shutdown from the instance (using the operating system
      -- command for system shutdown).
    , _iilsArchitecture :: ArchitectureValues
      -- ^ The architecture of the instance.
    , _iilsPlacement :: Placement
      -- ^ 
    } deriving (Generic)

instance ToXML ImportInstanceLaunchSpecification where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | If the task is for importing an instance, this contains information about
-- the import instance task.
data ImportInstanceTaskDetails = ImportInstanceTaskDetails
    { _iitdInstanceId :: Text
      -- ^ 
    , _iitdPlatform :: PlatformValues
      -- ^ The instance operating system.
    , _iitdVolumes :: [ImportInstanceVolumeDetailItem]
      -- ^ 
    , _iitdDescription :: Text
      -- ^ 
    } deriving (Generic)

instance FromXML ImportInstanceTaskDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "importInstance"

instance ToXML ImportInstanceTaskDetails where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "importInstance"

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
    , _iivdiStatusMessage :: Text
      -- ^ The status information or errors related to the disk image.
    , _iivdiDescription :: Text
      -- ^ 
    } deriving (Generic)

instance FromXML ImportInstanceVolumeDetailItem where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML ImportInstanceVolumeDetailItem where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

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
    , _ivtdDescription :: Text
      -- ^ The description you provided when starting the import volume
      -- task.
    } deriving (Generic)

instance FromXML ImportVolumeTaskDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "importVolume"

instance ToXML ImportVolumeTaskDetails where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "importVolume"

-- | Describes an instance.
data Instance = Instance
    { _iInstanceId :: Text
      -- ^ The ID of the instance.
    , _iState :: InstanceState
      -- ^ The current state of the instance.
    , _iVirtualizationType :: VirtualizationType
      -- ^ The virtualization type of the instance.
    , _iPublicDnsName :: Text
      -- ^ The public DNS name assigned to the instance. This name is not
      -- available until the instance enters the running state.
    , _iHypervisor :: HypervisorType
      -- ^ The hypervisor type of the instance.
    , _iPlatform :: PlatformValues
      -- ^ The value is Windows for Windows instances; otherwise blank.
    , _iSecurityGroups :: [GroupIdentifier]
      -- ^ One or more security groups for the instance.
    , _iClientToken :: Text
      -- ^ The idempotency token you provided when you launched the
      -- instance.
    , _iSourceDestCheck :: Bool
      -- ^ Specifies whether to enable an instance launched in a VPC to
      -- perform NAT. This controls whether source/destination checking is
      -- enabled on the instance. A value of true means checking is
      -- enabled, and false means checking is disabled. The value must be
      -- false for the instance to perform NAT. For more information, see
      -- NAT Instances in the Amazon Virtual Private Cloud User Guide.
    , _iVpcId :: Text
      -- ^ The ID of the VPC in which the instance is running.
    , _iKeyName :: Text
      -- ^ The name of the key pair, if this instance was launched with an
      -- associated key pair.
    , _iLaunchTime :: ISO8601
      -- ^ The time the instance was launched.
    , _iNetworkInterfaces :: [InstanceNetworkInterface]
      -- ^ [EC2-VPC] One or more network interfaces for the instance.
    , _iRamdiskId :: Text
      -- ^ The RAM disk associated with this instance.
    , _iSubnetId :: Text
      -- ^ The ID of the subnet in which the instance is running.
    , _iKernelId :: Text
      -- ^ The kernel associated with this instance.
    , _iRootDeviceName :: Text
      -- ^ The root device name (for example, /dev/sda1).
    , _iInstanceType :: InstanceType
      -- ^ The instance type.
    , _iSriovNetSupport :: Text
      -- ^ Specifies whether enhanced networking is enabled.
    , _iEbsOptimized :: Bool
      -- ^ Indicates whether the instance is optimized for EBS I/O. This
      -- optimization provides dedicated throughput to Amazon EBS and an
      -- optimized configuration stack to provide optimal I/O performance.
      -- This optimization isn't available with all instance types.
      -- Additional usage charges apply when using an EBS Optimized
      -- instance.
    , _iMonitoring :: Monitoring
      -- ^ The monitoring information for the instance.
    , _iStateTransitionReason :: Text
      -- ^ The reason for the most recent state transition. This might be an
      -- empty string.
    , _iInstanceLifecycle :: InstanceLifecycleType
      -- ^ Indicates whether this is a Spot Instance.
    , _iIamInstanceProfile :: IamInstanceProfile
      -- ^ The IAM instance profile associated with the instance.
    , _iImageId :: Text
      -- ^ The ID of the AMI used to launch the instance.
    , _iPrivateIpAddress :: Text
      -- ^ The private IP address assigned to the instance.
    , _iArchitecture :: ArchitectureValues
      -- ^ The architecture of the image.
    , _iProductCodes :: [ProductCode]
      -- ^ The product codes attached to this instance.
    , _iSpotInstanceRequestId :: Text
      -- ^ The ID of the Spot Instance request.
    , _iPrivateDnsName :: Text
      -- ^ The private DNS name assigned to the instance. This DNS name can
      -- only be used inside the Amazon EC2 network. This name is not
      -- available until the instance enters the running state.
    , _iStateReason :: StateReason
      -- ^ The reason for the most recent state transition.
    , _iRootDeviceType :: DeviceType
      -- ^ The root device type used by the AMI. The AMI can use an Amazon
      -- EBS volume or an instance store volume.
    , _iBlockDeviceMappings :: [InstanceBlockDeviceMapping]
      -- ^ Any block device mapping entries for the instance.
    , _iAmiLaunchIndex :: Integer
      -- ^ The AMI launch index, which can be used to find this instance in
      -- the launch group.
    , _iPublicIpAddress :: Text
      -- ^ The public IP address assigned to the instance.
    , _iPlacement :: Placement
      -- ^ The location where the instance launched.
    , _iTags :: [Tag]
      -- ^ Any tags assigned to the instance.
    } deriving (Generic)

instance FromXML Instance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML Instance where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes a block device mapping.
data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { _ibdmEbs :: EbsInstanceBlockDevice
      -- ^ Parameters used to automatically set up Amazon EBS volumes when
      -- the instance is launched.
    , _ibdmDeviceName :: Text
      -- ^ The device name exposed to the instance (for example, /dev/sdh).
    } deriving (Generic)

instance FromXML InstanceBlockDeviceMapping where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML InstanceBlockDeviceMapping where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes a block device mapping entry.
data InstanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification
    { _ibdmsVirtualName :: Text
      -- ^ The virtual device name.
    , _ibdmsNoDevice :: Text
      -- ^ suppress the specified device included in the block device
      -- mapping.
    , _ibdmsEbs :: EbsInstanceBlockDeviceSpecification
      -- ^ Parameters used to automatically set up Amazon EBS volumes when
      -- the instance is launched.
    , _ibdmsDeviceName :: Text
      -- ^ The device name exposed to the instance (for example, /dev/sdh).
    } deriving (Generic)

instance ToXML InstanceBlockDeviceMappingSpecification where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "BlockDeviceMapping"

-- | Describes a Reserved Instance listing state.
data InstanceCount = InstanceCount
    { _icState :: ListingState
      -- ^ The states of the listed Reserved Instances.
    , _icInstanceCount :: Integer
      -- ^ he number of listed Reserved Instances in the state specified by
      -- the state.
    } deriving (Generic)

instance FromXML InstanceCount where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML InstanceCount where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | The instance being exported.
data InstanceExportDetails = InstanceExportDetails
    { _iedTargetEnvironment :: ExportEnvironment
      -- ^ The target virtualization environment.
    , _iedInstanceId :: Text
      -- ^ The ID of the resource being exported.
    } deriving (Generic)

instance FromXML InstanceExportDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "instanceExport"

instance ToXML InstanceExportDetails where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "instanceExport"

-- | Describes the monitoring information of the instance.
data InstanceMonitoring = InstanceMonitoring
    { _imInstanceId :: Text
      -- ^ The ID of the instance.
    , _imMonitoring :: Monitoring
      -- ^ The monitoring information.
    } deriving (Generic)

instance FromXML InstanceMonitoring where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a network interface.
data InstanceNetworkInterface = InstanceNetworkInterface
    { _iniGroups :: [GroupIdentifier]
      -- ^ One or more security groups.
    , _iniStatus :: NetworkInterfaceStatus
      -- ^ The status of the network interface.
    , _iniPrivateIpAddresses :: [InstancePrivateIpAddress]
      -- ^ The private IP addresses associated with the network interface.
    , _iniSourceDestCheck :: Bool
      -- ^ Indicates whether to validate network traffic to or from this
      -- network interface.
    , _iniVpcId :: Text
      -- ^ The ID of the VPC.
    , _iniNetworkInterfaceId :: Text
      -- ^ The ID of the network interface.
    , _iniSubnetId :: Text
      -- ^ The ID of the subnet.
    , _iniAttachment :: InstanceNetworkInterfaceAttachment
      -- ^ The network interface attachment.
    , _iniOwnerId :: Text
      -- ^ The ID of the AWS account that created the network interface.
    , _iniPrivateIpAddress :: Text
      -- ^ The IP address of the network interface within the subnet.
    , _iniPrivateDnsName :: Text
      -- ^ The private DNS name.
    , _iniDescription :: Text
      -- ^ The description.
    , _iniAssociation :: InstanceNetworkInterfaceAssociation
      -- ^ The association information for an Elastic IP associated with the
      -- network interface.
    } deriving (Generic)

instance FromXML InstanceNetworkInterface where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML InstanceNetworkInterface where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | The association information for an Elastic IP address for the network
-- interface.
data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
    { _iniaPublicDnsName :: Text
      -- ^ The public DNS name.
    , _iniaIpOwnerId :: Text
      -- ^ The ID of the owner of the Elastic IP address.
    , _iniaPublicIp :: Text
      -- ^ The address of the Elastic IP address bound to the network
      -- interface.
    } deriving (Generic)

instance FromXML InstanceNetworkInterfaceAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "association"

instance ToXML InstanceNetworkInterfaceAssociation where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "association"

-- | The network interface attachment.
data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { _iniaStatus :: AttachmentStatus
      -- ^ The attachment state.
    , _iniaDeleteOnTermination :: Bool
      -- ^ Indicates whether the network interface is deleted when the
      -- instance is terminated.
    , _iniaAttachmentId :: Text
      -- ^ The ID of the network interface attachment.
    , _iniaAttachTime :: ISO8601
      -- ^ The time stamp when the attachment initiated.
    , _iniaDeviceIndex :: Integer
      -- ^ The index of the device on the instance for the network interface
      -- attachment.
    } deriving (Generic)

instance FromXML InstanceNetworkInterfaceAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "attachment"

instance ToXML InstanceNetworkInterfaceAttachment where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "attachment"

-- | Describes a network interface.
data InstanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification
    { _inisGroups :: [Text]
      -- ^ The IDs of the security groups for the network interface.
    , _inisPrivateIpAddresses :: [PrivateIpAddressSpecification]
      -- ^ One or more private IP addresses to assign to the network
      -- interface.
    , _inisDeleteOnTermination :: Bool
      -- ^ If set to true, the interface is deleted when the instance is
      -- terminated.
    , _inisAssociatePublicIpAddress :: Bool
      -- ^ Indicates whether to auto-assign a public IP address to an
      -- instance in a VPC. This public IP address can be assigned to the
      -- network interface for eth0 only when you launch the instance. You
      -- must create the network interface instead of using an existing
      -- network interface for eth0, and you must not specify more than
      -- one network interface.
    , _inisNetworkInterfaceId :: Text
      -- ^ The ID of the network interface.
    , _inisSubnetId :: Text
      -- ^ The ID of the subnet associated with the network string.
    , _inisPrivateIpAddress :: Text
      -- ^ The private IP address of the network interface.
    , _inisSecondaryPrivateIpAddressCount :: Integer
      -- ^ The number of secondary private IP addresses.
    , _inisDescription :: Text
      -- ^ The description of the network interface.
    , _inisDeviceIndex :: Integer
      -- ^ The index of the device on the instance for the network interface
      -- attachment.
    } deriving (Generic)

instance FromXML InstanceNetworkInterfaceSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NetworkInterface"

instance ToXML InstanceNetworkInterfaceSpecification where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "NetworkInterface"

-- | Describes a private IP address.
data InstancePrivateIpAddress = InstancePrivateIpAddress
    { _ipiaPrimary :: Bool
      -- ^ Indicates whether this IP address is the primary private IP
      -- address of the network interface.
    , _ipiaPrivateIpAddress :: Text
      -- ^ The private IP address of the network interface.
    , _ipiaPrivateDnsName :: Text
      -- ^ The private DNS name.
    , _ipiaAssociation :: InstanceNetworkInterfaceAssociation
      -- ^ The association information for an Elastic IP address for the
      -- network interface.
    } deriving (Generic)

instance FromXML InstancePrivateIpAddress where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML InstancePrivateIpAddress where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | The current state of the instance.
data InstanceState = InstanceState
    { _isName :: InstanceStateName
      -- ^ The current state of the instance.
    , _isCode :: Integer
      -- ^ The low byte represents the state. The high byte is an opaque
      -- internal value and should be ignored. 0 : pending 16 : running 32
      -- : shutting-down 48 : terminated 64 : stopping 80 : stopped.
    } deriving (Generic)

instance FromXML InstanceState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "currentState"

instance ToXML InstanceState where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "currentState"

-- | Describes an instance state change.
data InstanceStateChange = InstanceStateChange
    { _iscInstanceId :: Text
      -- ^ The ID of the instance.
    , _iscCurrentState :: InstanceState
      -- ^ The current state of the instance.
    , _iscPreviousState :: InstanceState
      -- ^ The previous state of the instance.
    } deriving (Generic)

instance FromXML InstanceStateChange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes the status of an instance.
data InstanceStatus = InstanceStatus
    { _isInstanceId :: Text
      -- ^ The ID of the instance.
    , _isSystemStatus :: InstanceStatusSummary
      -- ^ Reports impaired functionality that stems from issues related to
      -- the systems that support an instance, such as hardware failures
      -- and network connectivity problems.
    , _isEvents :: [InstanceStatusEvent]
      -- ^ Extra information regarding events associated with the instance.
    , _isAvailabilityZone :: Text
      -- ^ The Availability Zone of the instance.
    , _isInstanceStatus :: InstanceStatusSummary
      -- ^ Reports impaired functionality that stems from issues internal to
      -- the instance, such as impaired reachability.
    , _isInstanceState :: InstanceState
      -- ^ The intended state of the instance. DescribeInstanceStatus
      -- requires that an instance be in the running state.
    } deriving (Generic)

instance FromXML InstanceStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes the instance status.
data InstanceStatusDetails = InstanceStatusDetails
    { _isdStatus :: StatusType
      -- ^ The status.
    , _isdImpairedSince :: ISO8601
      -- ^ The time when a status check failed. For an instance that was
      -- launched and impaired, this is the time when the instance was
      -- launched.
    , _isdName :: StatusName
      -- ^ The type of instance status.
    } deriving (Generic)

instance FromXML InstanceStatusDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML InstanceStatusDetails where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes an instance event.
data InstanceStatusEvent = InstanceStatusEvent
    { _iseNotBefore :: ISO8601
      -- ^ The earliest scheduled start time for the event.
    , _iseCode :: EventCode
      -- ^ The associated code of the event.
    , _iseDescription :: Text
      -- ^ A description of the event.
    , _iseNotAfter :: ISO8601
      -- ^ The latest scheduled end time for the event.
    } deriving (Generic)

instance FromXML InstanceStatusEvent where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML InstanceStatusEvent where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
data InstanceStatusSummary = InstanceStatusSummary
    { _issStatus :: SummaryStatus
      -- ^ The status.
    , _issDetails :: [InstanceStatusDetails]
      -- ^ The system instance health or application instance health.
    } deriving (Generic)

instance FromXML InstanceStatusSummary where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "systemStatus"

instance ToXML InstanceStatusSummary where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "systemStatus"

-- | Information about the Internet gateway.
data InternetGateway = InternetGateway
    { _igAttachments :: [InternetGatewayAttachment]
      -- ^ Any VPCs attached to the Internet gateway.
    , _igInternetGatewayId :: Text
      -- ^ The ID of the Internet gateway.
    , _igTags :: [Tag]
      -- ^ Any tags assigned to the Internet gateway.
    } deriving (Generic)

instance FromXML InternetGateway where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "internetGateway"

-- | Describes the attachment of a VPC to an Internet gateway.
data InternetGatewayAttachment = InternetGatewayAttachment
    { _igaState :: AttachmentStatus
      -- ^ The current state of the attachment.
    , _igaVpcId :: Text
      -- ^ The ID of the VPC.
    } deriving (Generic)

instance FromXML InternetGatewayAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML InternetGatewayAttachment where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes a security group rule.
data IpPermission = IpPermission
    { _ipFromPort :: Integer
      -- ^ The start of port range for the TCP and UDP protocols, or an ICMP
      -- type number. A value of -1 indicates all ICMP types.
    , _ipUserIdGroupPairs :: [UserIdGroupPair]
      -- ^ One or more security group and AWS account ID pairs.
    , _ipIpProtocol :: Text
      -- ^ The protocol. When you call DescribeSecurityGroups, the protocol
      -- value returned is the number. Exception: For TCP, UDP, and ICMP,
      -- the value returned is the name (for example, tcp, udp, or icmp).
      -- For a list of protocol numbers, see Protocol Numbers.
    , _ipToPort :: Integer
      -- ^ The end of port range for the TCP and UDP protocols, or an ICMP
      -- code. A value of -1 indicates all ICMP codes for the specified
      -- ICMP type.
    , _ipIpRanges :: [IpRange]
      -- ^ One or more IP ranges.
    } deriving (Generic)

instance FromXML IpPermission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML IpPermission where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | Describes a key pair.
data KeyPairInfo = KeyPairInfo
    { _kpiKeyFingerprint :: Text
      -- ^ If you used CreateKeyPair to create the key pair, this is the
      -- SHA-1 digest of the DER encoded private key. If you used
      -- ImportKeyPair to provide AWS the public key, this is the MD5
      -- public key fingerprint as specified in section 4 of RFC4716.
    , _kpiKeyName :: Text
      -- ^ The name of the key pair.
    } deriving (Generic)

instance FromXML KeyPairInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a launch permission.
data LaunchPermission = LaunchPermission
    { _lpGroup :: PermissionGroup
      -- ^ The name of the group.
    , _lpUserId :: Text
      -- ^ The AWS account ID.
    } deriving (Generic)

instance FromXML LaunchPermission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML LaunchPermission where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | 
data LaunchPermissionModifications = LaunchPermissionModifications
    { _lpmRemove :: [LaunchPermission]
      -- ^ The AWS account ID to remove from the list of launch permissions
      -- for the AMI.
    , _lpmAdd :: [LaunchPermission]
      -- ^ The AWS account ID to add to the list of launch permissions for
      -- the AMI.
    } deriving (Generic)

instance ToXML LaunchPermissionModifications where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | The launch specification.
data LaunchSpecification = LaunchSpecification
    { _lsSecurityGroupIds :: [Text]
    , _lsSecurityGroups :: [Text]
    , _lsKeyName :: Text
      -- ^ The name of the key pair.
    , _lsNetworkInterfaces :: [InstanceNetworkInterfaceSpecification]
      -- ^ One or more network interfaces.
    , _lsRamdiskId :: Text
      -- ^ The ID of the RAM disk.
    , _lsSubnetId :: Text
      -- ^ The ID of the subnet in which to launch the Spot Instance.
    , _lsKernelId :: Text
      -- ^ The ID of the kernel.
    , _lsInstanceType :: InstanceType
      -- ^ The instance type.
    , _lsEbsOptimized :: Bool
      -- ^ Indicates whether the instance is optimized for EBS I/O. This
      -- optimization provides dedicated throughput to Amazon EBS and an
      -- optimized configuration stack to provide optimal EBS I/O
      -- performance. This optimization isn't available with all instance
      -- types. Additional usage charges apply when using an EBS Optimized
      -- instance. Default: false.
    , _lsUserData :: Text
      -- ^ The Base64-encoded MIME user data to make available to the
      -- instances.
    , _lsMonitoring :: Monitoring
    , _lsIamInstanceProfile :: IamInstanceProfileSpecification
      -- ^ The IAM instance profile.
    , _lsImageId :: Text
      -- ^ The ID of the AMI.
    , _lsAddressingType :: Text
      -- ^ 
    , _lsBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ One or more block device mapping entries.
    , _lsPlacement :: SpotPlacement
      -- ^ The placement information for the instance.
    } deriving (Generic)

instance FromXML LaunchSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML LaunchSpecification where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | Information about the network ACL.
data NetworkAcl = NetworkAcl
    { _naEntries :: [NetworkAclEntry]
      -- ^ One or more entries (rules) in the network ACL.
    , _naNetworkAclId :: Text
      -- ^ The ID of the network ACL.
    , _naVpcId :: Text
      -- ^ The ID of the VPC for the network ACL.
    , _naAssociations :: [NetworkAclAssociation]
      -- ^ Any associations between the network ACL and one or more subnets.
    , _naTags :: [Tag]
      -- ^ Any tags assigned to the network ACL.
    , _naIsDefault :: Bool
      -- ^ Indicates whether this is the default network ACL for the VPC.
    } deriving (Generic)

instance FromXML NetworkAcl where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "networkAcl"

-- | Describes an association between a network ACL and a subnet.
data NetworkAclAssociation = NetworkAclAssociation
    { _naaNetworkAclId :: Text
      -- ^ The ID of the network ACL.
    , _naaSubnetId :: Text
      -- ^ The ID of the subnet.
    , _naaNetworkAclAssociationId :: Text
      -- ^ The ID of the association between a network ACL and a subnet.
    } deriving (Generic)

instance FromXML NetworkAclAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML NetworkAclAssociation where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes an entry in a network ACL.
data NetworkAclEntry = NetworkAclEntry
    { _naeIcmpTypeCode :: IcmpTypeCode
      -- ^ ICMP protocol: The ICMP type and code.
    , _naeRuleNumber :: Integer
      -- ^ The rule number for the entry. ACL entries are processed in
      -- ascending order by rule number.
    , _naeRuleAction :: RuleAction
      -- ^ Indicates whether to allow or deny the traffic that matches the
      -- rule.
    , _naeProtocol :: Text
      -- ^ The protocol. A value of -1 means all protocols.
    , _naePortRange :: PortRange
      -- ^ TCP or UDP protocols: The range of ports the rule applies to.
    , _naeCidrBlock :: Text
      -- ^ The network range to allow or deny, in CIDR notation.
    , _naeEgress :: Bool
      -- ^ Indicates whether the rule is an egress rule (applied to traffic
      -- leaving the subnet).
    } deriving (Generic)

instance FromXML NetworkAclEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML NetworkAclEntry where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes a network interface.
data NetworkInterface = NetworkInterface
    { _niGroups :: [GroupIdentifier]
      -- ^ Any security groups for the network interface.
    , _niStatus :: NetworkInterfaceStatus
      -- ^ The status of the network interface.
    , _niPrivateIpAddresses :: [NetworkInterfacePrivateIpAddress]
      -- ^ The private IP addresses associated with the network interface.
    , _niSourceDestCheck :: Bool
      -- ^ Indicates whether traffic to or from the instance is validated.
    , _niVpcId :: Text
      -- ^ The ID of the VPC.
    , _niTagSet :: [Tag]
      -- ^ Any tags assigned to the network interface.
    , _niRequesterManaged :: Bool
      -- ^ Indicates whether the network interface is being managed by AWS.
    , _niNetworkInterfaceId :: Text
      -- ^ The ID of the network interface.
    , _niSubnetId :: Text
      -- ^ The ID of the subnet.
    , _niMacAddress :: Text
      -- ^ The MAC address.
    , _niAttachment :: NetworkInterfaceAttachment
      -- ^ The network interface attachment.
    , _niOwnerId :: Text
      -- ^ The AWS account ID of the owner of the network interface.
    , _niAvailabilityZone :: Text
      -- ^ The Availability Zone.
    , _niPrivateIpAddress :: Text
      -- ^ The IP address of the network interface within the subnet.
    , _niPrivateDnsName :: Text
      -- ^ The private DNS name.
    , _niRequesterId :: Text
      -- ^ The ID of the entity that launched the instance on your behalf
      -- (for example, AWS Management Console or Auto Scaling).
    , _niDescription :: Text
      -- ^ A description.
    , _niAssociation :: NetworkInterfaceAssociation
      -- ^ The association information for an Elastic IP associated with the
      -- network interface.
    } deriving (Generic)

instance FromXML NetworkInterface where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The association information for an Elastic IP address associated with the
-- network interface.
data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { _niaAssociationId :: Text
      -- ^ The association ID.
    , _niaPublicDnsName :: Text
      -- ^ The public DNS name.
    , _niaAllocationId :: Text
      -- ^ The allocation ID.
    , _niaIpOwnerId :: Text
      -- ^ The ID of the Elastic IP address owner.
    , _niaPublicIp :: Text
      -- ^ The address of the Elastic IP address bound to the network
      -- interface.
    } deriving (Generic)

instance FromXML NetworkInterfaceAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "association"

instance ToXML NetworkInterfaceAssociation where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "association"

-- | The network interface attachment.
data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { _niaInstanceId :: Text
      -- ^ The ID of the instance.
    , _niaStatus :: AttachmentStatus
      -- ^ The attachment state.
    , _niaDeleteOnTermination :: Bool
      -- ^ Indicates whether the network interface is deleted when the
      -- instance is terminated.
    , _niaAttachmentId :: Text
      -- ^ The ID of the network interface attachment.
    , _niaInstanceOwnerId :: Text
      -- ^ The AWS account ID of the owner of the instance.
    , _niaAttachTime :: ISO8601
      -- ^ The timestamp indicating when the attachment initiated.
    , _niaDeviceIndex :: Integer
      -- ^ The device index of the network interface attachment on the
      -- instance.
    } deriving (Generic)

instance FromXML NetworkInterfaceAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "attachment"

instance ToXML NetworkInterfaceAttachment where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "attachment"

-- | The ID of the interface attachment.
data NetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges
    { _niacDeleteOnTermination :: Bool
      -- ^ Indicates whether the network interface is deleted when the
      -- instance is terminated.
    , _niacAttachmentId :: Text
      -- ^ The ID of the network interface attachment.
    } deriving (Generic)

instance ToXML NetworkInterfaceAttachmentChanges where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | Describes the private IP address of a network interface.
data NetworkInterfacePrivateIpAddress = NetworkInterfacePrivateIpAddress
    { _nipiaPrimary :: Bool
      -- ^ Indicates whether this IP address is the primary private IP
      -- address of the network interface.
    , _nipiaPrivateIpAddress :: Text
      -- ^ The private IP address.
    , _nipiaPrivateDnsName :: Text
      -- ^ The private DNS name.
    , _nipiaAssociation :: NetworkInterfaceAssociation
      -- ^ The association information for an Elastic IP address associated
      -- with the network interface.
    } deriving (Generic)

instance FromXML NetworkInterfacePrivateIpAddress where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML NetworkInterfacePrivateIpAddress where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | 
data Placement = Placement
    { _pAvailabilityZone :: Text
      -- ^ The Availability Zone of the instance.
    , _pTenancy :: Tenancy
      -- ^ The tenancy of the instance (if the instance is running in a
      -- VPC). An instance with a tenancy of dedicated runs on
      -- single-tenant hardware.
    , _pGroupName :: Text
      -- ^ The name of the placement group the instance is in (for cluster
      -- compute instances).
    } deriving (Generic)

instance FromXML Placement where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML Placement where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | Describes a placement group.
data PlacementGroup = PlacementGroup
    { _pgState :: PlacementGroupState
      -- ^ The state of the placement group.
    , _pgStrategy :: PlacementStrategy
      -- ^ The placement strategy.
    , _pgGroupName :: Text
      -- ^ The name of the placement group.
    } deriving (Generic)

instance FromXML PlacementGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | TCP or UDP protocols: The range of ports the rule applies to.
data PortRange = PortRange
    { _prTo :: Integer
      -- ^ The last port in the range.
    , _prFrom :: Integer
      -- ^ The first port in the range.
    } deriving (Generic)

instance FromXML PortRange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "portRange"

instance ToXML PortRange where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "portRange"

-- | Describes the price for a Reserved Instance.
data PriceSchedule = PriceSchedule
    { _psCurrencyCode :: CurrencyCodeValues
      -- ^ The currency for transacting the Reserved Instance resale. At
      -- this time, the only supported currency is USD.
    , _psTerm :: Integer
      -- ^ The number of months remaining in the reservation. For example, 2
      -- is the second to the last month before the capacity reservation
      -- expires.
    , _psActive :: Bool
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
    , _psPrice :: Double
      -- ^ The fixed price for the term.
    } deriving (Generic)

instance FromXML PriceSchedule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML PriceSchedule where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes the price for a Reserved Instance.
data PriceScheduleSpecification = PriceScheduleSpecification
    { _pssCurrencyCode :: CurrencyCodeValues
      -- ^ The currency for transacting the Reserved Instance resale. At
      -- this time, the only supported currency is USD.
    , _pssTerm :: Integer
      -- ^ The number of months remaining in the reservation. For example, 2
      -- is the second to the last month before the capacity reservation
      -- expires.
    , _pssPrice :: Double
      -- ^ The fixed price for the term.
    } deriving (Generic)

instance ToXML PriceScheduleSpecification where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | Describes a Reserved Instance offering.
data PricingDetail = PricingDetail
    { _pdCount :: Integer
      -- ^ The number of instances available for the price.
    , _pdPrice :: Double
      -- ^ The price per instance.
    } deriving (Generic)

instance FromXML PricingDetail where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML PricingDetail where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes a secondary private IP address for a network interface.
data PrivateIpAddressSpecification = PrivateIpAddressSpecification
    { _piasPrimary :: Bool
      -- ^ Indicates whether the private IP address is the primary private
      -- IP address.
    , _piasPrivateIpAddress :: Text
      -- ^ The private IP addresses.
    } deriving (Generic)

instance FromXML PrivateIpAddressSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PrivateIpAddresses"

instance ToXML PrivateIpAddressSpecification where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "PrivateIpAddresses"

-- | Describes a product code.
data ProductCode = ProductCode
    { _pcProductCodeType :: ProductCodeValues
      -- ^ The type of product code.
    , _pcProductCodeId :: Text
      -- ^ The product code.
    } deriving (Generic)

instance FromXML ProductCode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML ProductCode where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes a recurring charge.
data RecurringCharge = RecurringCharge
    { _rcAmount :: Double
      -- ^ The amount of the recurring charge.
    , _rcFrequency :: RecurringChargeFrequency
      -- ^ The frequency of the recurring charge.
    } deriving (Generic)

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML RecurringCharge where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes a region.
data Region = Region
    { _rRegionName :: Text
      -- ^ The name of the region.
    , _rEndpoint :: Text
      -- ^ The region service endpoint.
    } deriving (Generic)

instance FromXML Region where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a reservation.
data Reservation = Reservation
    { _rGroups :: [GroupIdentifier]
      -- ^ One or more security groups.
    , _rOwnerId :: Text
      -- ^ The ID of the AWS account that owns the reservation.
    , _rInstances :: [Instance]
      -- ^ One or more instances.
    , _rReservationId :: Text
      -- ^ The ID of the reservation.
    , _rRequesterId :: Text
      -- ^ The ID of the requester that launched the instances on your
      -- behalf (for example, AWS Management Console or Auto Scaling).
    } deriving (Generic)

instance FromXML Reservation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Specified for Reserved Instance Marketplace offerings to limit the total
-- order and ensure that the Reserved Instances are not purchased at
-- unexpected prices.
data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice
    { _rilpAmount :: Double
      -- ^ Used for Reserved Instance Marketplace offerings. Specifies the
      -- limit price on the total order (instanceCount * price).
    , _rilpCurrencyCode :: CurrencyCodeValues
      -- ^ The currency in which the limitPrice amount is specified. At this
      -- time, the only supported currency is USD.
    } deriving (Generic)

instance ToXML ReservedInstanceLimitPrice where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | Describes a Reserved Instance.
data ReservedInstances = ReservedInstances
    { _riState :: ReservedInstanceState
      -- ^ The state of the Reserved Instance purchase.
    , _riCurrencyCode :: CurrencyCodeValues
      -- ^ The currency of the Reserved Instance. It's specified using ISO
      -- 4217 standard currency codes. At this time, the only supported
      -- currency is USD.
    , _riInstanceCount :: Integer
      -- ^ The number of Reserved Instances purchased.
    , _riProductDescription :: RIProductDescription
      -- ^ The Reserved Instance description.
    , _riStart :: ISO8601
      -- ^ The date and time the Reserved Instance started.
    , _riInstanceType :: InstanceType
      -- ^ The instance type on which the Reserved Instance can be used.
    , _riEnd :: ISO8601
      -- ^ The time when the Reserved Instance expires.
    , _riAvailabilityZone :: Text
      -- ^ The Availability Zone in which the Reserved Instance can be used.
    , _riRecurringCharges :: [RecurringCharge]
      -- ^ The recurring charge tag assigned to the resource.
    , _riOfferingType :: OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , _riUsagePrice :: Double
      -- ^ The usage price of the Reserved Instance, per hour.
    , _riFixedPrice :: Double
      -- ^ The purchase price of the Reserved Instance.
    , _riReservedInstancesId :: Text
      -- ^ The ID of the Reserved Instance.
    , _riInstanceTenancy :: Tenancy
      -- ^ The tenancy of the reserved instance.
    , _riDuration :: Integer
      -- ^ The duration of the Reserved Instance, in seconds.
    , _riTags :: [Tag]
      -- ^ Any tags assigned to the resource.
    } deriving (Generic)

instance FromXML ReservedInstances where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes the configuration settings for the modified Reserved Instances.
data ReservedInstancesConfiguration = ReservedInstancesConfiguration
    { _ricPlatform :: Text
      -- ^ The network platform of the modified Reserved Instances, which is
      -- either EC2-Classic or EC2-VPC.
    , _ricInstanceCount :: Integer
      -- ^ The number of modified Reserved Instances.
    , _ricInstanceType :: InstanceType
      -- ^ The instance type for the modified Reserved Instances.
    , _ricAvailabilityZone :: Text
      -- ^ The Availability Zone for the modified Reserved Instances.
    } deriving (Generic)

instance FromXML ReservedInstancesConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedInstancesConfigurationSetItemType"

instance ToXML ReservedInstancesConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ReservedInstancesConfigurationSetItemType"

-- | Describes a Reserved Instance listing.
data ReservedInstancesListing = ReservedInstancesListing
    { _rilStatus :: ListingStatus
      -- ^ The status of the Reserved Instance listing.
    , _rilClientToken :: Text
      -- ^ The idempotency token you provided when you created the listing.
    , _rilUpdateDate :: ISO8601
      -- ^ The last modified timestamp of the listing.
    , _rilCreateDate :: ISO8601
      -- ^ The time the listing was created.
    , _rilPriceSchedules :: [PriceSchedule]
      -- ^ The price of the Reserved Instance listing.
    , _rilStatusMessage :: Text
      -- ^ The reason for the current status of the Reserved Instance
      -- listing. The response can be blank.
    , _rilReservedInstancesId :: Text
      -- ^ The ID of the Reserved Instance.
    , _rilTags :: [Tag]
      -- ^ Any tags assigned to the resource.
    , _rilInstanceCounts :: [InstanceCount]
      -- ^ The number of instances in this state.
    , _rilReservedInstancesListingId :: Text
      -- ^ The ID of the Reserved Instance listing.
    } deriving (Generic)

instance FromXML ReservedInstancesListing where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a Reserved Instance modification.
data ReservedInstancesModification = ReservedInstancesModification
    { _rimModificationResults :: [ReservedInstancesModificationResult]
      -- ^ Contains target configurations along with their corresponding new
      -- Reserved Instance IDs.
    , _rimStatus :: Text
      -- ^ The status of the Reserved Instances modification request.
    , _rimClientToken :: Text
      -- ^ A unique, case-sensitive key supplied by the client to ensure
      -- that the modification request is idempotent.
    , _rimUpdateDate :: ISO8601
      -- ^ The time when the modification request was last updated.
    , _rimCreateDate :: ISO8601
      -- ^ The time when the modification request was created.
    , _rimEffectiveDate :: ISO8601
      -- ^ The time for the modification to become effective.
    , _rimStatusMessage :: Text
      -- ^ The reason for the status.
    , _rimReservedInstancesModificationId :: Text
      -- ^ A unique ID for the Reserved Instance modification.
    , _rimReservedInstancesIds :: [ReservedInstancesId]
      -- ^ The IDs of one or more Reserved Instances.
    } deriving (Generic)

instance FromXML ReservedInstancesModification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | 
data ReservedInstancesModificationResult = ReservedInstancesModificationResult
    { _rimrReservedInstancesId :: Text
      -- ^ The ID for the Reserved Instances that were created as part of
      -- the modification request. This field is only available when the
      -- modification is fulfilled.
    , _rimrTargetConfiguration :: ReservedInstancesConfiguration
      -- ^ The target Reserved Instances configurations supplied as part of
      -- the modification request.
    } deriving (Generic)

instance FromXML ReservedInstancesModificationResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML ReservedInstancesModificationResult where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes a Reserved Instance offering.
data ReservedInstancesOffering = ReservedInstancesOffering
    { _rioMarketplace :: Bool
      -- ^ Indicates whether the offering is available through the Reserved
      -- Instance Marketplace (resale) or AWS. If it's a Reserved Instance
      -- Marketplace offering, this is true.
    , _rioCurrencyCode :: CurrencyCodeValues
      -- ^ The currency of the Reserved Instance offering you are
      -- purchasing. It's specified using ISO 4217 standard currency
      -- codes. At this time, the only supported currency is USD.
    , _rioProductDescription :: RIProductDescription
      -- ^ The Reserved Instance description.
    , _rioInstanceType :: InstanceType
      -- ^ The instance type on which the Reserved Instance can be used.
    , _rioAvailabilityZone :: Text
      -- ^ The Availability Zone in which the Reserved Instance can be used.
    , _rioPricingDetails :: [PricingDetail]
      -- ^ The pricing details of the Reserved Instance offering.
    , _rioRecurringCharges :: [RecurringCharge]
      -- ^ The recurring charge tag assigned to the resource.
    , _rioOfferingType :: OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , _rioUsagePrice :: Double
      -- ^ The usage price of the Reserved Instance, per hour.
    , _rioFixedPrice :: Double
      -- ^ The purchase price of the Reserved Instance.
    , _rioInstanceTenancy :: Tenancy
      -- ^ The tenancy of the reserved instance.
    , _rioReservedInstancesOfferingId :: Text
      -- ^ The ID of the Reserved Instance offering.
    , _rioDuration :: Integer
      -- ^ The duration of the Reserved Instance, in seconds.
    } deriving (Generic)

instance FromXML ReservedInstancesOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a route in a route table.
data Route = Route
    { _rVpcPeeringConnectionId :: Text
      -- ^ The ID of the VPC peering connection.
    , _rInstanceId :: Text
      -- ^ The ID of a NAT instance in your VPC.
    , _rOrigin :: RouteOrigin
      -- ^ Describes how the route was created. CreateRouteTable indicates
      -- that route was automatically created when the route table was
      -- created. CreateRoute indicates that the route was manually added
      -- to the route table. EnableVgwRoutePropagation indicates that the
      -- route was propagated by route propagation.
    , _rState :: RouteState
      -- ^ The state of the route. The blackhole state indicates that the
      -- route's target isn't available (for example, the specified
      -- gateway isn't attached to the VPC, or the specified NAT instance
      -- has been terminated).
    , _rNetworkInterfaceId :: Text
      -- ^ The ID of the network interface.
    , _rGatewayId :: Text
      -- ^ The ID of a gateway attached to your VPC.
    , _rInstanceOwnerId :: Text
      -- ^ The AWS account ID of the owner of the instance.
    , _rDestinationCidrBlock :: Text
      -- ^ The CIDR block used for the destination match.
    } deriving (Generic)

instance FromXML Route where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML Route where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Information about the route table.
data RouteTable = RouteTable
    { _rtRouteTableId :: Text
      -- ^ The ID of the route table.
    , _rtRoutes :: [Route]
      -- ^ The routes in the route table.
    , _rtVpcId :: Text
      -- ^ The ID of the VPC.
    , _rtPropagatingVgws :: [PropagatingVgw]
      -- ^ Any virtual private gateway (VGW) propagating routes.
    , _rtAssociations :: [RouteTableAssociation]
      -- ^ The associations between the route table and one or more subnets.
    , _rtTags :: [Tag]
      -- ^ Any tags assigned to the route table.
    } deriving (Generic)

instance FromXML RouteTable where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "routeTable"

-- | Describes an association between a route table and a subnet.
data RouteTableAssociation = RouteTableAssociation
    { _rtaRouteTableId :: Text
      -- ^ The ID of the route table.
    , _rtaRouteTableAssociationId :: Text
      -- ^ The ID of the association between a route table and a subnet.
    , _rtaMain :: Bool
      -- ^ Indicates whether this is the main route table.
    , _rtaSubnetId :: Text
      -- ^ The ID of the subnet.
    } deriving (Generic)

instance FromXML RouteTableAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML RouteTableAssociation where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | An Amazon S3 storage location.
data S3Storage = S3Storage
    { _ssPrefix :: Text
      -- ^ The beginning of the file name of the AMI.
    , _ssUploadPolicy :: Text
      -- ^ A Base64-encoded Amazon S3 upload policy that gives Amazon EC2
      -- permission to upload items into Amazon S3 on your behalf.
    , _ssBucket :: Text
      -- ^ The bucket in which to store the AMI. You can specify a bucket
      -- that you already own or a new bucket that Amazon EC2 creates on
      -- your behalf. If you specify a bucket that belongs to someone
      -- else, Amazon EC2 returns an error.
    , _ssUploadPolicySignature :: Text
      -- ^ The signature of the Base64 encoded JSON document.
    , _ssAWSAccessKeyId :: Text
      -- ^ The access key ID of the owner of the bucket. Before you specify
      -- a value for your access key ID, review and follow the guidance in
      -- Best Practices for Managing AWS Access Keys.
    } deriving (Generic)

instance FromXML S3Storage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML S3Storage where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | Describes a security group.
data SecurityGroup = SecurityGroup
    { _sgVpcId :: Text
      -- ^ [EC2-VPC] The ID of the VPC for the security group.
    , _sgIpPermissions :: [IpPermission]
      -- ^ One or more inbound rules associated with the security group.
    , _sgOwnerId :: Text
      -- ^ The AWS account ID of the owner of the security group.
    , _sgIpPermissionsEgress :: [IpPermission]
      -- ^ [EC2-VPC] One or more outbound rules associated with the security
      -- group.
    , _sgGroupId :: Text
      -- ^ The ID of the security group.
    , _sgGroupName :: Text
      -- ^ The name of the security group.
    , _sgDescription :: Text
      -- ^ A description of the security group.
    , _sgTags :: [Tag]
      -- ^ Any tags assigned to the security group.
    } deriving (Generic)

instance FromXML SecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a snapshot.
data Snapshot = Snapshot
    { _sState :: SnapshotState
      -- ^ The snapshot state.
    , _sOwnerAlias :: Text
      -- ^ The AWS account alias (for example, amazon, self) or AWS account
      -- ID that owns the snapshot.
    , _sProgress :: Text
      -- ^ The progress of the snapshot, as a percentage.
    , _sStartTime :: ISO8601
      -- ^ The time stamp when the snapshot was initiated.
    , _sVolumeSize :: Integer
      -- ^ The size of the volume, in GiB.
    , _sEncrypted :: Bool
      -- ^ Indicates whether the snapshot is encrypted.
    , _sOwnerId :: Text
      -- ^ The AWS account ID of the Amazon EBS snapshot owner.
    , _sVolumeId :: Text
      -- ^ The ID of the volume.
    , _sDescription :: Text
      -- ^ The description for the snapshot.
    , _sTags :: [Tag]
      -- ^ Any tags assigned to the snapshot.
    , _sSnapshotId :: Text
      -- ^ The ID of the snapshot.
    } deriving (Generic)

instance FromXML Snapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The Spot Instance datafeed subscription.
data SpotDatafeedSubscription = SpotDatafeedSubscription
    { _sdsState :: DatafeedSubscriptionState
      -- ^ The state of the Spot Instance datafeed subscription.
    , _sdsPrefix :: Text
      -- ^ The prefix that is prepended to datafeed files.
    , _sdsBucket :: Text
      -- ^ The Amazon S3 bucket where the Spot Instance datafeed is located.
    , _sdsOwnerId :: Text
      -- ^ The AWS account ID of the account.
    , _sdsFault :: SpotInstanceStateFault
      -- ^ The fault codes for the Spot Instance request, if any.
    } deriving (Generic)

instance FromXML SpotDatafeedSubscription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "spotDatafeedSubscription"

-- | Describe a Spot Instance request.
data SpotInstanceRequest = SpotInstanceRequest
    { _sirInstanceId :: Text
      -- ^ The instance ID, if an instance has been launched to fulfill the
      -- Spot Instance request.
    , _sirStatus :: SpotInstanceStatus
      -- ^ The status code and status message describing the Spot Instance
      -- request.
    , _sirState :: SpotInstanceState
      -- ^ The state of the Spot Instance request. Spot bid status
      -- information can help you track your Spot Instance requests. For
      -- information, see Tracking Spot Requests with Bid Status Codes in
      -- the Amazon Elastic Compute Cloud User Guide.
    , _sirProductDescription :: RIProductDescription
      -- ^ The product description associated with the Spot Instance.
    , _sirSpotPrice :: Text
      -- ^ The maximum hourly price for any Spot Instance launched to
      -- fulfill the request.
    , _sirLaunchSpecification :: LaunchSpecification
      -- ^ Additional information for launching instances.
    , _sirAvailabilityZoneGroup :: Text
      -- ^ The Availability Zone group. If you specify the same Availability
      -- Zone group for all Spot Instance requests, all Spot Instances are
      -- launched in the same Availability Zone.
    , _sirLaunchedAvailabilityZone :: Text
      -- ^ The Availability Zone in which the bid is launched.
    , _sirValidUntil :: ISO8601
      -- ^ The end date of the request. If this is a one-time request, the
      -- request remains active until all instances launch, the request is
      -- canceled, or this date is reached. If the request is persistent,
      -- it remains active until it is canceled or this date is reached.
    , _sirLaunchGroup :: Text
      -- ^ The instance launch group. Launch groups are Spot Instances that
      -- launch together and terminate together.
    , _sirFault :: SpotInstanceStateFault
      -- ^ The fault codes for the Spot Instance request, if any.
    , _sirSpotInstanceRequestId :: Text
      -- ^ The ID of the Spot Instance request.
    , _sirType :: SpotInstanceType
      -- ^ The Spot Instance request type.
    , _sirValidFrom :: ISO8601
      -- ^ The start date of the request. If this is a one-time request, the
      -- request becomes active at this date and time and remains active
      -- until all instances launch, the request expires, or the request
      -- is canceled. If the request is persistent, the request becomes
      -- active at this date and time and remains active until it expires
      -- or is canceled.
    , _sirCreateTime :: ISO8601
      -- ^ The time stamp when the Spot Instance request was created.
    , _sirTags :: [Tag]
      -- ^ Any tags assigned to the resource.
    } deriving (Generic)

instance FromXML SpotInstanceRequest where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The fault codes for the Spot Instance request, if any.
data SpotInstanceStateFault = SpotInstanceStateFault
    { _sisfCode :: Text
      -- ^ The reason code for the Spot Instance state change.
    , _sisfMessage :: Text
      -- ^ The message for the Spot Instance state change.
    } deriving (Generic)

instance FromXML SpotInstanceStateFault where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "fault"

instance ToXML SpotInstanceStateFault where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "fault"

-- | The status code and status message describing the Spot Instance request.
data SpotInstanceStatus = SpotInstanceStatus
    { _sisUpdateTime :: ISO8601
      -- ^ The time of the most recent status update.
    , _sisCode :: Text
      -- ^ The status code of the request.
    , _sisMessage :: Text
      -- ^ The description for the status code for the Spot request.
    } deriving (Generic)

instance FromXML SpotInstanceStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToXML SpotInstanceStatus where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "status"

-- | The placement information for the instance.
data SpotPlacement = SpotPlacement
    { _spAvailabilityZone :: Text
      -- ^ The Availability Zone.
    , _spGroupName :: Text
      -- ^ The Availability Zone group name.
    } deriving (Generic)

instance FromXML SpotPlacement where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ""

instance ToXML SpotPlacement where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot ""

-- | Describes the Spot Price.
data SpotPrice = SpotPrice
    { _spProductDescription :: RIProductDescription
      -- ^ A general description of the AMI.
    , _spSpotPrice :: Text
      -- ^ The maximum price you will pay to launch one or more Spot
      -- Instances.
    , _spInstanceType :: InstanceType
      -- ^ The instance type.
    , _spAvailabilityZone :: Text
      -- ^ The Availability Zone.
    , _spTimestamp :: ISO8601
      -- ^ The date and time the request was created.
    } deriving (Generic)

instance FromXML SpotPrice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The reason for the most recent state transition.
data StateReason = StateReason
    { _srCode :: Text
      -- ^ The reason code for the state change.
    , _srMessage :: Text
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
    } deriving (Generic)

instance FromXML StateReason where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "stateReason"

instance ToXML StateReason where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "stateReason"

-- | Describes a subnet.
data Subnet = Subnet
    { _sState :: SubnetState
      -- ^ The current state of the subnet.
    , _sAvailableIpAddressCount :: Integer
      -- ^ The number of unused IP addresses in the subnet. Note that the IP
      -- addresses for any stopped instances are considered unavailable.
    , _sVpcId :: Text
      -- ^ The ID of the VPC the subnet is in.
    , _sSubnetId :: Text
      -- ^ The ID of the subnet.
    , _sAvailabilityZone :: Text
      -- ^ The Availability Zone of the subnet.
    , _sCidrBlock :: Text
      -- ^ The CIDR block assigned to the subnet.
    , _sMapPublicIpOnLaunch :: Bool
      -- ^ Indicates whether instances launched in this subnet receive a
      -- public IP address.
    , _sDefaultForAz :: Bool
      -- ^ Indicates whether this is the default subnet for the Availability
      -- Zone.
    , _sTags :: [Tag]
      -- ^ Any tags assigned to the subnet.
    } deriving (Generic)

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a tag.
data Tag = Tag
    { _tValue :: Text
      -- ^ The value of the tag. Constraints: Tag values are case-sensitive
      -- and accept a maximum of 255 Unicode characters.
    , _tKey :: Text
      -- ^ The key of the tag. Constraints: Tag keys are case-sensitive and
      -- accept a maximum of 127 Unicode characters. May not begin with
      -- aws:.
    } deriving (Generic)

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML Tag where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes a tag.
data TagDescription = TagDescription
    { _tdResourceId :: Text
      -- ^ The ID of the resource. For example, ami-1a2b3c4d.
    , _tdResourceType :: ResourceType
      -- ^ The type of resource.
    , _tdValue :: Text
      -- ^ The value of the tag.
    , _tdKey :: Text
      -- ^ The key of the tag.
    } deriving (Generic)

instance FromXML TagDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a security group and AWS account ID pair for EC2-Classic.
data UserIdGroupPair = UserIdGroupPair
    { _uigpUserId :: Text
      -- ^ The ID of an AWS account.
    , _uigpGroupId :: Text
      -- ^ The name of the security group in the specified AWS account.
    , _uigpGroupName :: Text
      -- ^ The ID of the security group owned by the specified AWS account.
    } deriving (Generic)

instance FromXML UserIdGroupPair where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Groups"

instance ToXML UserIdGroupPair where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Groups"

-- | Describes telemetry for a VPN tunnel.
data VgwTelemetry = VgwTelemetry
    { _vtStatus :: TelemetryStatus
      -- ^ The status of the VPN tunnel.
    , _vtOutsideIpAddress :: Text
      -- ^ The Internet-routable IP address of the virtual private gateway's
      -- outside interface.
    , _vtLastStatusChange :: ISO8601
      -- ^ The date and time of the last change in status.
    , _vtAcceptedRouteCount :: Integer
      -- ^ The number of accepted routes.
    , _vtStatusMessage :: Text
      -- ^ If an error occurs, a description of the error.
    } deriving (Generic)

instance FromXML VgwTelemetry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML VgwTelemetry where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes a volume.
data Volume = Volume
    { _vState :: VolumeState
      -- ^ The volume state.
    , _vAttachments :: [VolumeAttachment]
      -- ^ 
    , _vSize :: Integer
      -- ^ The size of the volume, in GiBs.
    , _vIops :: Integer
      -- ^ The number of I/O operations per second (IOPS) that the volume
      -- supports.
    , _vEncrypted :: Bool
      -- ^ Indicates whether the volume is encrypted.
    , _vAvailabilityZone :: Text
      -- ^ The Availability Zone for the volume.
    , _vVolumeId :: Text
      -- ^ The ID of the volume.
    , _vVolumeType :: VolumeType
      -- ^ The volume type. This can be standard for standard EBS volumes or
      -- io1 for Provisioned IOPS volumes.
    , _vCreateTime :: ISO8601
      -- ^ The time stamp when volume creation was initiated.
    , _vTags :: [Tag]
      -- ^ Any tags assigned to the volume.
    , _vSnapshotId :: Text
      -- ^ The snapshot from which the volume was created, if applicable.
    } deriving (Generic)

instance FromXML Volume where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes volume attachment details.
data VolumeAttachment = VolumeAttachment
    { _vaInstanceId :: Text
      -- ^ The ID of the instance.
    , _vaDeleteOnTermination :: Bool
      -- ^ Indicates whether the Amazon EBS volume is deleted on instance
      -- termination.
    , _vaState :: VolumeAttachmentState
      -- ^ The attachment state of the volume.
    , _vaDevice :: Text
      -- ^ The device name.
    , _vaVolumeId :: Text
      -- ^ The ID of the volume.
    , _vaAttachTime :: ISO8601
      -- ^ The time stamp when the attachment initiated.
    } deriving (Generic)

instance FromXML VolumeAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML VolumeAttachment where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes a volume status operation code.
data VolumeStatusAction = VolumeStatusAction
    { _vsaEventType :: Text
      -- ^ The event type associated with this operation.
    , _vsaCode :: Text
      -- ^ The code identifying the operation, for example,
      -- enable-volume-io.
    , _vsaDescription :: Text
      -- ^ A description of the operation.
    , _vsaEventId :: Text
      -- ^ The ID of the event associated with this operation.
    } deriving (Generic)

instance FromXML VolumeStatusAction where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML VolumeStatusAction where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes a volume status.
data VolumeStatusDetails = VolumeStatusDetails
    { _vsdStatus :: Text
      -- ^ The intended status of the volume status.
    , _vsdName :: VolumeStatusName
      -- ^ The name of the volume status.
    } deriving (Generic)

instance FromXML VolumeStatusDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML VolumeStatusDetails where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Describes a volume status event.
data VolumeStatusEvent = VolumeStatusEvent
    { _vseNotBefore :: ISO8601
      -- ^ The earliest start time of the event.
    , _vseEventType :: Text
      -- ^ The type of this event.
    , _vseDescription :: Text
      -- ^ A description of the event.
    , _vseNotAfter :: ISO8601
      -- ^ The latest end time of the event.
    , _vseEventId :: Text
      -- ^ The ID of this event.
    } deriving (Generic)

instance FromXML VolumeStatusEvent where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML VolumeStatusEvent where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | The volume status.
data VolumeStatusInfo = VolumeStatusInfo
    { _vsiStatus :: VolumeStatusInfoStatus
      -- ^ The status of the volume.
    , _vsiDetails :: [VolumeStatusDetails]
      -- ^ The details of the volume status.
    } deriving (Generic)

instance FromXML VolumeStatusInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "volumeStatus"

instance ToXML VolumeStatusInfo where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "volumeStatus"

-- | Describes the volume status.
data VolumeStatusItem = VolumeStatusItem
    { _vsiVolumeStatus :: VolumeStatusInfo
      -- ^ The volume status.
    , _vsiActions :: [VolumeStatusAction]
      -- ^ The details of the operation.
    , _vsiEvents :: [VolumeStatusEvent]
      -- ^ A list of events associated with the volume.
    , _vsiAvailabilityZone :: Text
      -- ^ The Availability Zone of the volume.
    , _vsiVolumeId :: Text
      -- ^ The volume ID.
    } deriving (Generic)

instance FromXML VolumeStatusItem where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a VPC.
data Vpc = Vpc
    { _vState :: VpcState
      -- ^ The current state of the VPC.
    , _vVpcId :: Text
      -- ^ The ID of the VPC.
    , _vDhcpOptionsId :: Text
      -- ^ The ID of the set of DHCP options you've associated with the VPC
      -- (or default if the default options are associated with the VPC).
    , _vCidrBlock :: Text
      -- ^ The CIDR block for the VPC.
    , _vInstanceTenancy :: Tenancy
      -- ^ The allowed tenancy of instances launched into the VPC.
    , _vTags :: [Tag]
      -- ^ Any tags assigned to the VPC.
    , _vIsDefault :: Bool
      -- ^ Indicates whether the VPC is the default VPC.
    } deriving (Generic)

instance FromXML Vpc where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes an attachment between a virtual private gateway and a VPC.
data VpcAttachment = VpcAttachment
    { _vaState :: AttachmentStatus
      -- ^ The current state of the attachment.
    , _vaVpcId :: Text
      -- ^ The ID of the VPC.
    } deriving (Generic)

instance FromXML VpcAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML VpcAttachment where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"

-- | Information about the VPC peering connection.
data VpcPeeringConnection = VpcPeeringConnection
    { _vpcVpcPeeringConnectionId :: Text
      -- ^ The ID of the VPC peering connection.
    , _vpcStatus :: VpcPeeringConnectionStateReason
      -- ^ The status of the VPC peering connection.
    , _vpcAccepterVpcInfo :: VpcPeeringConnectionVpcInfo
      -- ^ The information of the peer VPC.
    , _vpcRequesterVpcInfo :: VpcPeeringConnectionVpcInfo
      -- ^ The information of the requester VPC.
    , _vpcExpirationTime :: ISO8601
      -- ^ The time that an unaccepted VPC peering connection will expire.
    , _vpcTags :: [Tag]
      -- ^ Any tags assigned to the resource.
    } deriving (Generic)

instance FromXML VpcPeeringConnection where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "vpcPeeringConnection"

-- | The status of the VPC peering connection.
data VpcPeeringConnectionStateReason = VpcPeeringConnectionStateReason
    { _vpcsrCode :: Text
      -- ^ The status of the VPC peering connection.
    , _vpcsrMessage :: Text
      -- ^ A message that provides more information about the status, if
      -- applicable.
    } deriving (Generic)

instance FromXML VpcPeeringConnectionStateReason where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToXML VpcPeeringConnectionStateReason where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "status"

-- | The information of the peer VPC.
data VpcPeeringConnectionVpcInfo = VpcPeeringConnectionVpcInfo
    { _vpcviVpcId :: Text
      -- ^ The ID of the VPC.
    , _vpcviOwnerId :: Text
      -- ^ The AWS account ID of the VPC owner.
    , _vpcviCidrBlock :: Text
      -- ^ The CIDR block for the VPC.
    } deriving (Generic)

instance FromXML VpcPeeringConnectionVpcInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "accepterVpcInfo"

instance ToXML VpcPeeringConnectionVpcInfo where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "accepterVpcInfo"

-- | Describes a VPN connection.
data VpnConnection = VpnConnection
    { _vcCustomerGatewayConfiguration :: Text
      -- ^ The configuration information for the VPN connection's customer
      -- gateway (in the native XML format). This element is always
      -- present in the CreateVpnConnection response; however, it's
      -- present in the DescribeVpnConnections response only if the VPN
      -- connection is in the pending or available state.
    , _vcState :: VpnState
      -- ^ The current state of the VPN connection.
    , _vcRoutes :: [VpnStaticRoute]
      -- ^ The static routes associated with the VPN connection.
    , _vcVpnGatewayId :: Text
      -- ^ The ID of the virtual private gateway at the AWS side of the VPN
      -- connection.
    , _vcCustomerGatewayId :: Text
      -- ^ The ID of the customer gateway at your end of the VPN connection.
    , _vcType :: GatewayType
      -- ^ The type of VPN connection.
    , _vcOptions :: VpnConnectionOptions
      -- ^ The VPN connection options.
    , _vcVpnConnectionId :: Text
      -- ^ The ID of the VPN connection.
    , _vcTags :: [Tag]
      -- ^ Any tags assigned to the VPN connection.
    , _vcVgwTelemetry :: [VgwTelemetry]
      -- ^ Information about the VPN tunnel.
    } deriving (Generic)

instance FromXML VpnConnection where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Information about the virtual private gateway.
data VpnGateway = VpnGateway
    { _vgState :: VpnState
      -- ^ The current state of the virtual private gateway.
    , _vgVpcAttachments :: [VpcAttachment]
      -- ^ Any VPCs attached to the virtual private gateway.
    , _vgVpnGatewayId :: Text
      -- ^ The ID of the virtual private gateway.
    , _vgAvailabilityZone :: Text
      -- ^ The Availability Zone where the virtual private gateway was
      -- created.
    , _vgType :: GatewayType
      -- ^ The type of VPN connection the virtual private gateway supports.
    , _vgTags :: [Tag]
      -- ^ Any tags assigned to the virtual private gateway.
    } deriving (Generic)

instance FromXML VpnGateway where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "vpnGateway"

-- | Describes a static route for a VPN connection.
data VpnStaticRoute = VpnStaticRoute
    { _vsrState :: VpnState
      -- ^ The current state of the static route.
    , _vsrSource :: VpnStaticRouteSource
      -- ^ Indicates how the routes were provided.
    , _vsrDestinationCidrBlock :: Text
      -- ^ The CIDR block associated with the local subnet of the customer
      -- data center.
    } deriving (Generic)

instance FromXML VpnStaticRoute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToXML VpnStaticRoute where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "item"
