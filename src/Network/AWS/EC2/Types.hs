{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.EC2.Types where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import           Data.Text             (Text)
import           Data.Text.Encoding
import           Data.Time
import           Network.AWS.Internal
import           Text.Read

-- | Currently supported version (2013-07-15) of the EC2 service.
ec2 :: Service
ec2 = Regional "ec2" "2013-08-15"

data EC2Error = EC2Error
    { ecCode    :: !Text
    , ecMessage :: !Text
    } deriving (Eq, Ord, Show, Generic)

instance IsXML EC2Error where
    xmlPickler = genericXMLPickler $ defaultXMLOptions
       { xmlCtorModifier = mkAnNName . stripPrefix "EC2"
       }

instance IsXML [EC2Error] where
    xmlPickler = xpElemList (mkAnNName "Error") xmlPickler

data EC2ErrorResponse = EC2ErrorResponse
    { eerErrors    :: [EC2Error]
    , eerRequestID :: !Text
    } deriving (Eq, Ord, Show, Generic)

instance ToError EC2ErrorResponse where
    toError = Err . show

instance IsXML EC2ErrorResponse

data Protocol = TCP | UDP | ICMP
    deriving (Eq, Ord, Generic)

instance Show Protocol where
    show TCP  = "tcp"
    show UDP  = "udp"
    show ICMP = "icmp"

instance Read Protocol where
    readPrec = readAssocList
        [ ("tcp",  TCP)
        , ("udp",  UDP)
        , ("icmp", ICMP)
        ]

instance IsQuery Protocol where
    queryPickler = qpPrim

instance IsXML Protocol where
    xmlPickler = xpContent xpPrim

data AddressDomain = AddressStandard | AddressVPC
    deriving (Eq)

instance Show AddressDomain where
    show AddressStandard = "standard"
    show AddressVPC      = "vpc"

instance Read AddressDomain where
    readPrec = readAssocList
        [ ("standard", AddressStandard)
        , ("vpc",      AddressVPC)
        ]

instance IsQuery AddressDomain where
    queryPickler = qpPrim

instance IsXML AddressDomain where
    xmlPickler = xpContent xpPrim

data VolumeStatus = Attaching | Attached | Detaching | Detached
    deriving (Eq)

instance Show VolumeStatus where
    show Attaching = "attaching"
    show Attached  = "attached"
    show Detaching = "detaching"
    show Detached  = "detached"

instance Read VolumeStatus where
    readPrec = readAssocList
        [ ("attaching", Attaching)
        , ("attached",  Attached)
        , ("detaching", Detaching)
        , ("detached",  Detached)
        ]

instance IsXML VolumeStatus where
    xmlPickler = xpContent xpPrim

-- data AccountAttributeSetItemType = AccountAttributeSetItemType
--     { aasitAttributeName     :: !Text
--       -- ^ The name of the attribute.
--     , aasitAttributeValueSet :: !AccountAttributeValueSetItemType
--       -- ^ A list of the attribute values, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery AccountAttributeSetItemType

-- instance IsXML AccountAttributeSetItemType where
--     xmlPickler = ec2XML

-- data AccountAttributeValueSetItemType = AccountAttributeValueSetItemType
--     { aavsitAttributeValue :: !Text
--       -- ^ The value of the attribute.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery AccountAttributeValueSetItemType

-- instance IsXML AccountAttributeValueSetItemType where
--     xmlPickler = ec2XML

-- data PrivateIpAddress = PrivateIpAddress
--     { piaPrivateIpAddress :: !Text
--       -- ^ The private IP address.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery AssignPrivateIpAddressesSetItemRequestType

-- instance IsXML AssignPrivateIpAddressesSetItemRequestType where
--     xmlPickler = ec2XML

-- data AttachmentSetItemResponseType = AttachmentSetItemResponseType
--     { asirtVolumeId            :: !Text
--       -- ^ The ID of the volume.
--     , asirtInstanceId          :: !Text
--       -- ^ The ID of the instance.
--     , asirtDevice              :: !Text
--       -- ^ The device name exposed to the instance (for example, /dev/sdh).
--     , asirtStatus              :: !Text
--       -- ^ The attachment state.
--     , asirtAttachTime          :: !UTCTime
--       -- ^ The time stamp when the attachment initiated.
--     , asirtDeleteOnTermination :: !Bool
--       -- ^ Indicates whether the volume is deleted on instance termination.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery AttachmentSetItemResponseType

-- instance IsXML AttachmentSetItemResponseType where
--     xmlPickler = ec2XML

data Attachment = Attachment
    { atVpcId :: !Text
      -- ^ The ID of the VPC.
    , atState :: !Text
      -- ^ The current state of the attachment.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML Attachment where
    xmlPickler = ec2XML

data AvailabilityZoneItemType = AvailabilityZoneItemType
    { azitZoneName   :: !AvailabilityZone
      -- ^ The name of the Availability Zone.
    , azitZoneState  :: !Text
      -- ^ The state of the Availability Zone.
      -- FIXME: Should be - available | impaired | unavailable
    , azitRegionName :: !Region
      -- ^ The name of the region.
    , azitMessageSet :: [AvailabilityZoneMessageType]
      -- ^ Any messages about the Availability Zone.
    } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery AvailabilityZoneItemType

instance IsXML AvailabilityZoneItemType where
    xmlPickler = ec2ItemXML

data AvailabilityZoneMessageType = AvailabilityZoneMessageType
    { azmtMessage :: !Text
      -- ^ The message about the Availability Zone.
    } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery AvailabilityZoneMessageType

instance IsXML AvailabilityZoneMessageType where
    xmlPickler = ec2ItemXML

data BlockDeviceMappingItemType = BlockDeviceMappingItemType
    { bdmitDeviceName  :: !Text
      -- ^ The device name exposed to the instance (for example, /dev/sdh).
    , bdmitVirtualName :: Maybe Text
      -- ^ The virtual device name.
    , bdmitEbs         :: Maybe EbsBlockDeviceType
      -- ^ Parameters used to automatically set up Amazon EBS volumes when
      -- the instance is launched.
    , bdmitNoDevice    :: Maybe Text
      -- ^ Include this empty element to suppress the specified device
      -- included in the block device mapping of the AMI.
    } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery BlockDeviceMappingItemType

instance IsXML BlockDeviceMappingItemType where
    xmlPickler = ec2ItemXML

data BundleInstanceS3Storage = BundleInstanceS3Storage
    { bissAwsAccessKeyId        :: !Text
      -- ^ The access key ID of the owner of the bucket.
    , bissBucket                :: !Text
      -- ^ The bucket in which to store the AMI. You can specify a bucket
      -- that you already own or a new bucket that Amazon EC2 creates on
      -- your behalf. If you specify a bucket that belongs to someone
      -- else, Amazon EC2 returns an error.
    , bissPrefix                :: !Text
      -- ^ The beginning of the file name of the AMI.
    , bissUploadPolicy          :: !Text
      -- ^ A Base64-encoded Amazon S3 upload policy that gives Amazon EC2
      -- permission to upload items into Amazon S3 on the user's behalf.
    , bissUploadPolicySignature :: !Text
      -- ^ The signature of the Base64 encoded JSON document.
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery BundleInstanceS3Storage

instance IsXML BundleInstanceS3Storage where
    xmlPickler = ec2XML

data BundleInstanceTaskStorage = BundleInstanceTaskStorage
    { bitsS3 :: !BundleInstanceS3Storage
      -- ^ An Amazon S3 storage location.
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery BundleInstanceTaskStorage

instance IsXML BundleInstanceTaskStorage where
    xmlPickler = ec2XML

data BundleInstanceTaskError = BundleInstanceTaskError
    { biteCode    :: !Text
      -- ^ The error code.
    , biteMessage :: !Text
      -- ^ The error message.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML BundleInstanceTaskError where
    xmlPickler = ec2XML

data BundleInstanceState
    = Pending
    | WaitingForShutdown --waiting-for-shutdown
    | Bundling
    | Storing
    | Cancelling
    | Complete
    | Failed
      deriving (Eq)

instance Show BundleInstanceState where
    show st = case st of
        Pending            -> "pending"
        WaitingForShutdown -> "waiting-for-shutdown"
        Bundling           -> "bundling"
        Storing            -> "storing"
        Cancelling         -> "cancelling"
        Complete           -> "complete"
        Failed             -> "failed"

instance Read BundleInstanceState where
    readPrec = readAssocList
        [ ("pending",              Pending)
        , ("waiting-for-shutdown", WaitingForShutdown)
        , ("bundling",             Bundling)
        , ("storing",              Storing)
        , ("cancelling",           Cancelling)
        , ("complete",             Complete)
        , ("failed",               Failed)
        ]

instance IsXML BundleInstanceState where
    xmlPickler = xpContent xpPrim

data BundleInstanceTask = BundleInstanceTask
    { bitInstanceId :: !Text
      -- ^ The ID of the instance associated with this bundle task.
    , bitBundleId   :: !Text
      -- ^ The ID for this bundle task.
    , bitState      :: !BundleInstanceState
      -- ^ The state of the task.
    , bitStartTime  :: !UTCTime
      -- ^ The time this task started.
    , bitUpdateTime :: !UTCTime
      -- ^ The time of the most recent update for the task.
    , bitStorage    :: !BundleInstanceTaskStorage
      -- ^ The Amazon S3 storage locations.
    , bitProgress   :: !Text
      -- ^ The level of task completion, as a percent (for example, 20%).
    , bitError      :: Maybe BundleInstanceTaskError
      -- ^ If the task fails, a description of the error.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML BundleInstanceTask where
    xmlPickler = ec2XML

data CancelSpotInstanceRequestsResponseSetItemType = CancelSpotInstanceRequestsResponseSetItemType
    { csirrsitSpotInstanceRequestId :: !Text
      -- ^ The ID of the Spot Instance request.
    , csirrsitState                 :: !Text
      -- ^ The state of the Spot Instance request.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML CancelSpotInstanceRequestsResponseSetItemType where
    xmlPickler = ec2XML

-- data ConversionTaskType = ConversionTaskType
--     { cttConversionTaskId :: !Text
--       -- ^ The ID of the conversion task
--     , cttExpirationTime   :: !Text
--       -- ^ The time when the task expires. If the upload isn't complete
--       -- before the expiration time, we automatically cancel the task.
--     , cttImportVolume     :: !ImportVolumeTaskDetailsType
--       -- ^ If the task is for importing a volume, this contains information
--       -- about the import volume task.
--     , cttImportInstance   :: !ImportInstanceTaskDetailsType
--       -- ^ If the task is for importing an instance, this contains
--       -- information about the import instance task.
--     , cttState            :: !Text
--       -- ^ The state of the conversion task.
--     , cttStatusMessage    :: !Text
--       -- ^ The status message related to the conversion task.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery ConversionTaskType

-- instance IsXML ConversionTaskType where
--     xmlPickler = ec2XML

-- data CreateVolumePermissionItemType = CreateVolumePermissionItemType
--     { cvpitUserId :: !Text
--       -- ^ The ID of an AWS account that can create volumes from the
--       -- snapshot.
--     , cvpitGroup  :: !Text
--       -- ^ The group that is allowed to create volumes from the snapshot.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery CreateVolumePermissionItemType

-- instance IsXML CreateVolumePermissionItemType where
--     xmlPickler = ec2XML

-- data CustomerGatewayType = CustomerGatewayType
--     { cgtCustomerGatewayId :: !Text
--       -- ^ The ID of the customer gateway.
--     , cgtState             :: !Text
--       -- ^ The current state of the customer gateway.
--     , cgtType              :: !Text
--       -- ^ The type of VPN connection the customer gateway supports.
--     , cgtIpAddress         :: !Text
--       -- ^ The Internet-routable IP address of the customer gateway's
--       -- outside interface.
--     , cgtBgpAsn            :: !Integer
--       -- ^ The customer gateway's Border Gateway Protocol (BGP) Autonomous
--       -- System Number (ASN).
--     , cgtTagSet            :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery CustomerGatewayType

-- instance IsXML CustomerGatewayType where
--     xmlPickler = ec2XML

-- data DescribeAddressesResponseItemType = DescribeAddressesResponseItemType
--     { daritPublicIp                :: !Text
--       -- ^ The public IP address.
--     , daritAllocationId            :: !Text
--       -- ^ The ID representing the allocation of the address for use with
--       -- EC2-VPC.
--     , daritDomain                  :: !Text
--       -- ^ Indicates whether this Elastic IP address is for instances in
--       -- EC2-Classic or EC2-VPC.
--     , daritInstanceId              :: !Text
--       -- ^ The ID of the instance the address is associated with (if any).
--     , daritAssociationId           :: !Text
--       -- ^ The ID representing the association of an Elastic IP address with
--       -- an instance in a VPC.
--     , daritNetworkInterfaceId      :: !Text
--       -- ^ The ID of the network interface.
--     , daritNetworkInterfaceOwnerId :: !Text
--       -- ^ The ID of the AWS account that owns the network interface.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery DescribeAddressesResponseItemType

-- instance IsXML DescribeAddressesResponseItemType where
--     xmlPickler = ec2XML

data DescribeImagesResponseItemType = DescribeImagesResponseItemType
    { diritImageId            :: !Text
      -- ^ The ID of the AMI.
    , diritImageLocation      :: !Text
      -- ^ The location of the AMI.
    , diritImageState         :: !Text
      -- ^ Current state of the AMI. If the operation returns available, the
      -- image is successfully registered and available for launching.
    , diritImageOwnerId       :: !Text
      -- ^ AWS account ID of the image owner.
    , diritIsPublic           :: !Bool
      -- ^ Indicates whether the image has public launch permissions. The
      -- value is true if this image has public launch permissions or
      -- false if it has only implicit and explicit launch permissions.
    , diritProductCodes       :: [ProductCodesSetItemType]
      -- ^ Any product codes associated with the AMI.
    , diritArchitecture       :: !Text
      -- ^ The architecture of the image.
    , diritImageType          :: !Text
      -- ^ The type of image.
    , diritKernelId           :: Maybe Text
      -- ^ The kernel associated with the image, if any.
      -- Only applicable for machine images.
    , diritRamdiskId          :: Maybe Text
      -- ^ The RAM disk associated with the image, if any.
      -- Only applicable for machine images.
    , diritPlatform           :: Maybe Text
      -- ^ The value is Windows for Windows AMIs; otherwise blank.
    , diritStateReason        :: Maybe StateReasonType
      -- ^ The reason for the state change.
    , diritImageOwnerAlias    :: Maybe Text
      -- ^ The AWS account alias (for example, amazon, self, etc.) or AWS
      -- account ID that owns the AMI.
    , diritName               :: !Text
      -- ^ The name of the AMI that was provided during image creation.
    , diritDescription        :: Maybe Text
      -- ^ The description of the AMI that was provided during image creation.
    , diritRootDeviceType     :: !Text
      -- ^ The type of root device used by the AMI. The AMI can use an
      -- Amazon EBS volume or an instance store volume.
    , diritRootDeviceName     :: Maybe Text
      -- ^ The device name of the root device (for example, /dev/sda1 or
      -- xvda).
    , diritBlockDeviceMapping :: [BlockDeviceMappingItemType]
      -- ^ Any block device mapping entries.
    , diritVirtualizationType :: !Text
      -- ^ The type of virtualization of the AMI.
    , diritTagSet             :: [ResourceTagSetItemType]
      -- ^ Any tags assigned to the resource.
    , diritHypervisor         :: !Text
      -- ^ The image's hypervisor type.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML DescribeImagesResponseItemType where
    xmlPickler = ec2ItemXML

data DescribeKeyPairsResponseItemType = DescribeKeyPairsResponseItemType
    { dkpritKeyName        :: !Text
      -- ^ The name of the key pair.
    , dkpritKeyFingerprint :: !Text
      -- ^ If you used CreateKeyPair to create the key pair, this is the
      -- SHA-1 digest of the DER encoded private key. If you used
      -- ImportKeyPair to provide AWS the public key, this is the MD5
      -- public key fingerprint as specified in section 4 of RFC4716.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML DescribeKeyPairsResponseItemType where
    xmlPickler = ec2ItemXML

data DescribeReservedInstancesListingsResponseSetItemType = DescribeReservedInstancesListingsResponseSetItemType
    { drilrsitReservedInstancesListingId :: !Text
      -- ^ The ID of the Reserved Instance listing.
    , drilrsitReservedInstancesId        :: !Text
      -- ^ The ID of the Reserved Instance.
    , drilrsitCreateDate                 :: !UTCTime
      -- ^ The time the listing was created.
    , drilrsitUpdateDate                 :: !UTCTime
      -- ^ The last modified timestamp of the listing.
    , drilrsitStatus                     :: !Text
      -- ^ The status of the Reserved Instance listing.
    , drilrsitStatusMessage              :: !Text
      -- ^ The reason for the current status of the Reserved Instance
      -- listing. The response can be blank.
    , drilrsitInstanceCounts             :: [InstanceCountsSetItemType]
      -- ^ The number of instances in this state.
    , drilrsitPriceSchedules             :: [PriceScheduleSetItemType]
      -- ^ The price of the Reserved Instance listing.
    , drilrsitTagSet                     :: [ResourceTagSetItemType]
      -- ^ The tags assigned to the resource. Each tag's information is
      -- wrapped in an item element.
    , drilrsitClientToken                :: !Text
      -- ^ The idempotency token you provided when you created the listing.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML DescribeReservedInstancesListingsResponseSetItemType where
    xmlPickler = ec2ItemXML

-- data DescribeReservedInstancesListingSetItemType = DescribeReservedInstancesListingSetItemType
--     { drilsitReservedInstancesListingId :: !Text
--       -- ^ The ID of the Reserved Instance listing.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery DescribeReservedInstancesListingSetItemType

-- instance IsXML DescribeReservedInstancesListingSetItemType where
--     xmlPickler = ec2XML

-- data DescribeReservedInstancesOfferingsResponseSetItemType = DescribeReservedInstancesOfferingsResponseSetItemType
--     { driorsitReservedInstancesOfferingId :: !Text
--       -- ^ The ID of the Reserved Instance offering.
--     , driorsitInstanceType                :: !Text
--       -- ^ The instance type on which the Reserved Instance can be used.
--     , driorsitAvailabilityZone            :: !Text
--       -- ^ The Availability Zone in which the Reserved Instance can be used.
--     , driorsitDuration                    :: !Integer
--       -- ^ The duration of the Reserved Instance, in seconds.
--     , driorsitFixedPrice                  :: !Double
--       -- ^ The purchase price of the Reserved Instance.
--     , driorsitUsagePrice                  :: !Double
--       -- ^ The usage price of the Reserved Instance, per hour.
--     , driorsitProductDescription          :: !Text
--       -- ^ The Reserved Instance description.
--     , driorsitInstanceTenancy             :: !Text
--       -- ^ The tenancy of the reserved instance.
--     , driorsitCurrencyCode                :: !Text
--       -- ^ The currency of the Reserved Instance offering you are
--       -- purchasing. It's specified using ISO 4217 standard currency
--       -- codes. At this time, the only supported currency is USD.
--     , driorsitOfferingType                :: !Text
--       -- ^ The Reserved Instance offering type.
--     , driorsitRecurringCharges            :: !RecurringChargesSetItemType
--       -- ^ The recurring charge tag assigned to the resource.
--     , driorsitMarketplace                 :: !Bool
--       -- ^ Indicates whether the offering is available through the Reserved
--       -- Instance Marketplace (resale) or AWS. Returns true if it is a
--       -- Marketplace offering.
--     , driorsitPricingDetailsSet           :: !PricingDetailsSetItemType
--       -- ^ The pricing details of the Reserved Instance offering wrapped in
--       -- an item element.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery DescribeReservedInstancesOfferingsResponseSetItemType

-- instance IsXML DescribeReservedInstancesOfferingsResponseSetItemType where
--     xmlPickler = ec2XML

-- data DescribeReservedInstancesOfferingsResponseType = DescribeReservedInstancesOfferingsResponseType
--     { driortRequestId                     :: !Text
--       -- ^ The ID of the Reserved Instance offering request.
--     , driortReservedInstancesOfferingsSet :: !DescribeReservedInstancesOfferingsResponseSetItemType
--       -- ^ The instance type on which the Reserved Instance can be used.
--     , driortNextToken                     :: !Text
--       -- ^ The next paginated set of results to return.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery DescribeReservedInstancesOfferingsResponseType

-- instance IsXML DescribeReservedInstancesOfferingsResponseType where
--     xmlPickler = ec2XML

-- data DescribeReservedInstancesResponseSetItemType = DescribeReservedInstancesResponseSetItemType
--     { drirsitReservedInstancesId :: !Text
--       -- ^ The ID of the Reserved Instance.
--     , drirsitInstanceType        :: !Text
--       -- ^ The instance type on which the Reserved Instance can be used.
--     , drirsitAvailabilityZone    :: !Text
--       -- ^ The Availability Zone in which the Reserved Instance can be used.
--     , drirsitStart               :: !UTCTime
--       -- ^ The date and time the Reserved Instance started.
--     , drirsitDuration            :: !Integer
--       -- ^ The duration of the Reserved Instance, in seconds.
--     , drirsitFixedPrice          :: !Double
--       -- ^ The purchase price of the Reserved Instance.
--     , drirsitUsagePrice          :: !Double
--       -- ^ The usage price of the Reserved Instance, per hour.
--     , drirsitInstanceCount       :: !Integer
--       -- ^ The number of Reserved Instances purchased.
--     , drirsitProductDescription  :: !Text
--       -- ^ The Reserved Instance description.
--     , drirsitState               :: !Text
--       -- ^ The state of the Reserved Instance purchase.
--     , drirsitTagSet              :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     , drirsitInstanceTenancy     :: !Text
--       -- ^ The tenancy of the reserved instance.
--     , drirsitCurrencyCode        :: !Text
--       -- ^ The currency of the Reserved Instance. It's specified using ISO
--       -- 4217 standard currency codes. At this time, the only supported
--       -- currency is USD.
--     , drirsitOfferingType        :: !Text
--       -- ^ The Reserved Instance offering type.
--     , drirsitRecurringCharges    :: !RecurringChargesSetItemType
--       -- ^ The recurring charge tag assigned to the resource.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery DescribeReservedInstancesResponseSetItemType

-- instance IsXML DescribeReservedInstancesResponseSetItemType where
--     xmlPickler = ec2XML

-- data DescribeReservedInstancesSetItemType = DescribeReservedInstancesSetItemType
--     { drisitReservedInstancesId :: !Text
--       -- ^ The ID of the Reserved Instance.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery DescribeReservedInstancesSetItemType

-- instance IsXML DescribeReservedInstancesSetItemType where
--     xmlPickler = ec2XML

-- data DescribeSnapshotsSetItemResponseType = DescribeSnapshotsSetItemResponseType
--     { dssirtSnapshotId  :: !Text
--       -- ^ The ID of the snapshot.
--     , dssirtVolumeId    :: !Text
--       -- ^ The ID of the volume.
--     , dssirtStatus      :: !Text
--       -- ^ The snapshot state.
--     , dssirtStartTime   :: !UTCTime
--       -- ^ The time stamp when the snapshot was initiated.
--     , dssirtProgress    :: !Text
--       -- ^ The progress of the snapshot, as a percentage.
--     , dssirtOwnerId     :: !Text
--       -- ^ The ID of the AWS account that owns the snapshot.
--     , dssirtVolumeSize  :: !Text
--       -- ^ The size of the volume, in GiB.
--     , dssirtDescription :: !Text
--       -- ^ The description of the snapshot.
--     , dssirtOwnerAlias  :: !Text
--       -- ^ The AWS account alias (for example, amazon, self) or AWS account
--       -- ID that owns the AMI.
--     , dssirtTagSet      :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery DescribeSnapshotsSetItemResponseType

-- instance IsXML DescribeSnapshotsSetItemResponseType where
--     xmlPickler = ec2XML

-- data DescribeVolumesSetItemResponseType = DescribeVolumesSetItemResponseType
--     { dvsirtVolumeId         :: !Text
--       -- ^ The ID of the volume.
--     , dvsirtSize             :: !Text
--       -- ^ The size of the volume, in GiBs.
--     , dvsirtSnapshotId       :: !Text
--       -- ^ The snapshot from which the volume was created (optional).
--     , dvsirtAvailabilityZone :: !Text
--       -- ^ The Availability Zone in which the volume was created.
--     , dvsirtStatus           :: !Text
--       -- ^ The state of the volume.
--     , dvsirtCreateTime       :: !UTCTime
--       -- ^ The time stamp when volume creation was initiated.
--     , dvsirtAttachmentSet    :: !AttachmentSetItemResponseType
--       -- ^ Any volumes attached, each one wrapped in an item element.
--     , dvsirtTagSet           :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     , dvsirtVolumeType       :: !Text
--       -- ^ The volume type.
--     , dvsirtIops             :: !Integer
--       -- ^ The number of I/O operations per second (IOPS) that the volume
--       -- supports.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery DescribeVolumesSetItemResponseType

-- instance IsXML DescribeVolumesSetItemResponseType where
--     xmlPickler = ec2XML

-- data DhcpConfigurationItemType = DhcpConfigurationItemType
--     { dcitKey      :: !Text
--       -- ^ The name of a DHCP option.
--     , dcitValueSet :: !DhcpValueType
--       -- ^ Any values for a DHCP option, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery DhcpConfigurationItemType

-- instance IsXML DhcpConfigurationItemType where
--     xmlPickler = ec2XML

-- data DhcpOptionsType = DhcpOptionsType
--     { dotDhcpOptionsId        :: !Text
--       -- ^ The ID of the set of DHCP options.
--     , dotDhcpConfigurationSet :: !DhcpConfigurationItemType
--       -- ^ The DHCP options in the set. Each option's key and set of values
--       -- are wrapped in an item element.
--     , dotTagSet               :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery DhcpOptionsType

-- instance IsXML DhcpOptionsType where
--     xmlPickler = ec2XML

-- data DhcpValueType = DhcpValueType
--     { dvtValue :: !Text
--       -- ^ A value for the DHCP option.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery DhcpValueType

-- instance IsXML DhcpValueType where
--     xmlPickler = ec2XML

-- data DiskImageDescriptionType = DiskImageDescriptionType
--     { didtFormat            :: !Text
--       -- ^ The disk image format.
--     , didtSize              :: !Integer
--       -- ^ The size of the disk image.
--     , didtImportManifestUrl :: !Text
--       -- ^ A presigned URL for the import manifest stored in Amazon S3. For
--       -- information about creating a presigned URL for an Amazon S3
--       -- object, read the "Query String Request Authentication
--       -- Alternative" section of the Authenticating REST Requests topic in
--       -- the Amazon Simple Storage Service Developer Guide.
--     , didtChecksum          :: !Text
--       -- ^ The checksum computed for the disk image.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery DiskImageDescriptionType

-- instance IsXML DiskImageDescriptionType where
--     xmlPickler = ec2XML

-- data DiskImageVolumeDescriptionType = DiskImageVolumeDescriptionType
--     { divdtSize :: !Integer
--       -- ^ The size of the volume.
--     , divdtId   :: !Text
--       -- ^ The volume identifier.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery DiskImageVolumeDescriptionType

-- instance IsXML DiskImageVolumeDescriptionType where
--     xmlPickler = ec2XML

data EbsBlockDeviceType = EbsBlockDeviceType
    { ebdtSnapshotId          :: !Text
      -- ^ The ID of the snapshot.
    , ebdtVolumeSize          :: !Integer
      -- ^ The size of the volume, in GiB.
    , ebdtDeleteOnTermination :: !Bool
      -- ^ Indicates whether the Amazon EBS volume is deleted on instance termination.
    , ebdtVolumeType          :: !Text
      -- ^ The volume type.
    , ebdtIops                :: Maybe Integer
      -- ^ The number of I/O operations per second (IOPS) that the volume
      -- supports.
    } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery EbsBlockDeviceType

instance IsXML EbsBlockDeviceType where
    xmlPickler = ec2XML

data EbsInstanceBlockDeviceMappingResponseType = EbsInstanceBlockDeviceMappingResponseType
    { eibdmrtVolumeId            :: !Text
      -- ^ The ID of the Amazon EBS volume.
    , eibdmrtStatus              :: !Text
      -- ^ The attachment state.
    , eibdmrtAttachTime          :: !UTCTime
      -- ^ The time stamp when the attachment initiated.
    , eibdmrtDeleteOnTermination :: !Bool
      -- ^ Indicates whether the volume is deleted on instance termination.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML EbsInstanceBlockDeviceMappingResponseType where
    xmlPickler = ec2ItemXML

-- data ExportToS3Task = ExportToS3Task
--     { etstDiskImageFormat :: Maybe Text
--     , etstContainerFormat :: Maybe Text
--     , etstS3Bucket        :: !Text
--     , etstS3Prefix        :: !Text
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery ExportToS3Task

-- instance IsXML ExportToS3Task where
--     xmlPickler = ec2XML

-- data ExportTaskResponseType = ExportTaskResponseType
--     { etrtExportTaskId   :: !Text
--       -- ^ The ID of the export task.
--     , etrtDescription    :: !Text
--       -- ^ A description of the resource being exported.
--     , etrtState          :: !Text
--       -- ^ The state of the conversion task.
--     , etrtStatusMessage  :: !Text
--       -- ^ The status message related to the export task.
--     , etrtInstanceExport :: !InstanceExportTaskResponseType
--       -- ^ The instance being exported.
--     , etrtExportToS3     :: !ExportToS3TaskResponseType
--       -- ^ The destination Amazon S3 bucket.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery ExportTaskResponseType

-- instance IsXML ExportTaskResponseType where
--     xmlPickler = ec2XML

-- data ExportToS3TaskResponseType = ExportToS3TaskResponseType
--     { etstrtDiskImageFormat :: !Text
--       -- ^ The format for the exported image.
--     , etstrtContainerFormat :: !Text
--       -- ^ The container format used to combine disk images with metadata
--       -- (such as OVF).
--     , etstrtS3Bucket        :: !Text
--       -- ^ The Amazon S3 bucket for the destination image.
--     , etstrtS3Key           :: !Text
--       -- ^ The image written to a single object in s3bucket at the S3 key
--       -- s3prefix + exportTaskId + '.' +diskImageFormat.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery ExportToS3TaskResponseType

-- instance IsXML ExportToS3TaskResponseType where
--     xmlPickler = ec2XML

data GroupItemType = GroupItemType
    { gitGroupId   :: !Text
      -- ^ The ID of the security group.
    , gitGroupName :: !Text
      -- ^ The name of the security group.
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery GroupItemType

instance IsXML GroupItemType where
    xmlPickler = ec2ItemXML

data IamInstanceProfileRequestType = IamInstanceProfileRequestType
    { iiprtArn  :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the instance profile.
    , iiprtName :: Maybe Text
      -- ^ The name of the instance profile.
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery IamInstanceProfileRequestType

-- instance IsXML IamInstanceProfileRequestType where
--     xmlPickler = ec2XML

data IamInstanceProfileResponseType = IamInstanceProfileResponseType
    { iipruArn :: !Text
      -- ^ The Amazon Resource Name (ARN) of the instance profile.
    , iipruId  :: !Text
      -- ^ The ID of the instance profile.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML IamInstanceProfileResponseType where
    xmlPickler = ec2XML

-- data IcmpType = IcmpType
--     { itctCode :: !Integer
--       -- ^ The ICMP code. A value of -1 means all codes for the specified
--       -- ICMP type.
--     , itctType :: !Integer
--       -- ^ The ICMP type. A value of -1 means all types.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery IcmpTypeCodeType

-- instance IsXML IcmpTypeCodeType where
--     xmlPickler = ec2XML

-- data InstancePlacement = InstancePlacement
--     { ipAvailabilityZone :: !Maybe Text
--     , ipGroupName        :: !Maybe Text
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery InstancePlacement

-- instance IsXML InstancePlacement where
--     xmlPickler = ec2XML

-- data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification
--     { iilsArchitecture                      :: !Text
--     , iilsGroupSet                          :: Members GroupItemType
--     , iilsUserData                          :: Maybe UserDataType
--     , iilsInstance                          :: !Text
--     , iilsPlacement                         :: Maybe InstancePlacementType
--     , iilsMonitoring                        :: Maybe MonitoringInstanceType
--     , iilsSubnetId                          :: Maybe Text
--     , iilsInstanceInitiatedShutdownBehavior :: Maybe Text
--     , iilsPrivateIpAddress                  :: Maybe Text
--     } deriving (Show)

-- data ImportInstanceTaskDetailsType = ImportInstanceTaskDetailsType
--     { iitdtVolumes     :: !ImportInstanceVolumeDetailItemType
--       -- ^ Any instance volumes for import, each one wrapped in an item
--       -- element.
--     , iitdtInstanceId  :: !Text
--       -- ^ The ID of the instance.
--     , iitdtPlatform    :: !Text
--       -- ^ The value is Windows for Windows AMIs; otherwise blank.
--     , iitdtDescription :: !Text
--       -- ^ An optional description of the instance.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery ImportInstanceTaskDetailsType

-- instance IsXML ImportInstanceTaskDetailsType where
--     xmlPickler = ec2XML

-- data ImportInstanceVolumeDetailItemType = ImportInstanceVolumeDetailItemType
--     { iivditBytesConverted   :: !Integer
--       -- ^ The number of bytes converted so far.
--     , iivditAvailabilityZone :: !Text
--       -- ^ The Availability Zone where the resulting instance will reside.
--     , iivditImage            :: !DiskImageDescriptionType
--       -- ^ The image.
--     , iivditDescription      :: !Text
--       -- ^ The description you provided when starting the import instance
--       -- task.
--     , iivditVolume           :: !DiskImageVolumeDescriptionType
--       -- ^ The volume.
--     , iivditStatus           :: !Text
--       -- ^ The status of the import of this particular disk image.
--     , iivditStatusMessage    :: !Text
--       -- ^ The status information or errors related to the disk image.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery ImportInstanceVolumeDetailItemType

-- instance IsXML ImportInstanceVolumeDetailItemType where
--     xmlPickler = ec2XML

-- data ImportVolumeTaskDetailsType = ImportVolumeTaskDetailsType
--     { ivtdtBytesConverted   :: !Integer
--       -- ^ The number of bytes converted so far.
--     , ivtdtAvailabilityZone :: !Text
--       -- ^ The Availability Zone where the resulting volume will reside.
--     , ivtdtDescription      :: !Text
--       -- ^ The description you provided when starting the import volume
--       -- task.
--     , ivtdtImage            :: !DiskImageDescriptionType
--       -- ^ The image.
--     , ivtdtVolume           :: !DiskImageVolumeDescriptionType
--       -- ^ The volume.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery ImportVolumeTaskDetailsType

-- instance IsXML ImportVolumeTaskDetailsType where
--     xmlPickler = ec2XML

data InstanceBlockDeviceMappingItemType = InstanceBlockDeviceMappingItemType
    { ibdmitDeviceName  :: !Text
      -- ^ The device name exposed to the instance (for example, /dev/sdh or
      -- xvdh).
    , ibdmitVirtualName :: !Text
      -- ^ The virtual device name.
    , ibdmitEbs         :: !InstanceEbsBlockDeviceType
      -- ^ Parameters used to automatically set up Amazon EBS volumes when
      -- the instance is launched.
    , ibdmitNoDevice    :: !Text
      -- ^ Include this empty element to suppress the specified device
      -- included in the block device mapping of the AMI.
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery InstanceBlockDeviceMappingItemType

-- instance IsXML InstanceBlockDeviceMappingItemType where
--     xmlPickler = ec2XML

data InstanceBlockDeviceMappingResponseItemType = InstanceBlockDeviceMappingResponseItemType
    { ibdmritDeviceName :: !Text
      -- ^ The device name exposed to the instance (for example, /dev/sdh, or xvdh).
    , ibdmritEbs        :: !EbsInstanceBlockDeviceMappingResponseType
      -- ^ Parameters used to automatically set up Amazon EBS volumes when
      -- the instance is launched.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML InstanceBlockDeviceMappingResponseItemType where
    xmlPickler = ec2ItemXML

data InstanceCountsSetItemType = InstanceCountsSetItemType
    { icsitState         :: !Text
      -- ^ The states of the listed Reserved Instances.
    , icsitInstanceCount :: !Integer
      -- ^ The number of listed Reserved Instances in the state specified by
      -- the state.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML InstanceCountsSetItemType where
    xmlPickler = ec2XML

data InstanceEbsBlockDeviceType = InstanceEbsBlockDeviceType
    { iebdtDeleteOnTermination :: !Bool
      -- ^ Indicates whether the volume is deleted on instance termination.
    , iebdtVolumeId            :: !Text
      -- ^ The ID of the volume.
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery InstanceEbsBlockDeviceType

-- instance IsXML InstanceEbsBlockDeviceType where
--     xmlPickler = ec2XML

-- data InstanceExportTaskResponseType = InstanceExportTaskResponseType
--     { ietrtInstanceId        :: !Text
--       -- ^ The ID of the resource being exported.
--     , ietrtTargetEnvironment :: !Text
--       -- ^ The target virtualization environment.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery InstanceExportTaskResponseType

-- instance IsXML InstanceExportTaskResponseType where
--     xmlPickler = ec2XML

data InstanceMonitoringStateType = InstanceMonitoringStateType
    { imstState :: !Text
      -- ^ The state of monitoring for the instance.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML InstanceMonitoringStateType where
    xmlPickler = ec2XML

data InstanceNetworkInterfaceAssociationType = InstanceNetworkInterfaceAssociationType
    { iniatPublicIp      :: !Text
      -- ^ The address of the Elastic IP address bound to the network interface.
    , iniatPublicDnsName :: !Text
      -- ^ The public DNS name.
    , iniatIpOwnerId     :: !Text
      -- ^ The ID of the owner of the Elastic IP address.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML InstanceNetworkInterfaceAssociationType where
    xmlPickler = ec2XML

data InstanceNetworkInterfaceAttachmentType = InstanceNetworkInterfaceAttachmentType
    { iniatAttachmentID        :: !Text
    -- ^ The ID of the network interface attachment.
    , iniatDeviceIndex         :: !Integer
      -- ^ The index of the device on the instance for the network interface attachment.
    , iniatStatus              :: !Text
      -- ^ The attachment state.
    , iniatAttachTime          :: !UTCTime
      -- ^ The time stamp when the attachment initiated.
    , iniatDeleteOnTermination :: !Bool
      -- ^ Indicates whether the network interface is deleted when the
      -- instance is terminated.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML InstanceNetworkInterfaceAttachmentType where
    xmlPickler = ec2XML

-- data InstanceNetworkInterfaceSetItemRequestType = InstanceNetworkInterfaceSetItemRequestType
--     { inisirtNetworkInterfaceId             :: !Text
--       -- ^ The ID of the network interface.
--     , inisirtDeviceIndex                    :: !Integer
--       -- ^ Required. The index of the device on the instance for the network
--       -- interface attachment.
--     , inisirtSubnetId                       :: !Text
--       -- ^ The ID of the subnet associated with the network string.
--     , inisirtDescription                    :: !Text
--       -- ^ The description of the network interface.
--     , inisirtPrivateIpAddress               :: !Text
--       -- ^ The private IP address of the network interface.
--     , inisirtGroupSet                       :: !SecurityGroupIdSetItemType
--       -- ^ The IDs of the security groups for use by the network interface.
--     , inisirtDeleteOnTermination            :: !Bool
--       -- ^ If set to true, the interface is deleted when the instance is
--       -- terminated.
--     , inisirtPrivateIpAddressesSet          :: !PrivateIpAddressesSetItemRequestType
--       -- ^ The list of IP addresses to assign to the network interface.
--     , inisirtSecondaryPrivateIpAddressCount :: !Integer
--       -- ^ The number of secondary private IP addresses. You cannot specify
--       -- this option with privateIpAddressSet.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery InstanceNetworkInterfaceSetItemRequestType

-- instance IsXML InstanceNetworkInterfaceSetItemRequestType where
--     xmlPickler = ec2XML

data InstanceNetworkInterfaceSetItemType = InstanceNetworkInterfaceSetItemType
    { inisitNetworkInterfaceId    :: !Text
      -- ^ The ID of the network interface.
    , inisitSubnetId              :: !Text
      -- ^ The ID of the subnet.
    , inisitVpcId                 :: !Text
      -- ^ The ID of the VPC.
    , inisitDescription           :: !Text
      -- ^ The description.
    , inisitOwnerId               :: !Text
      -- ^ The ID of the customer who created the network interface.
    , inisitStatus                :: !Text
      -- ^ The status of the network interface.
    , inisitMacAddress            :: !Text
      -- ^ The MAC address.
    , inisitPrivateIpAddress      :: !Text
      -- ^ The IP address of the network interface within the subnet.
    , inisitPrivateDnsName        :: !Text
      -- ^ The private DNS name.
    , inisitSourceDestCheck       :: !Bool
      -- ^ Indicates whether to validate network traffic to or from this
      -- network interface.
    , inisitGroupSet              :: [GroupItemType]
      -- ^ A security group.
    , inisitAttachment            :: !InstanceNetworkInterfaceAttachmentType
      -- ^ The network interface attachment.
    , inisitAssociation           :: !InstanceNetworkInterfaceAssociationType
      -- ^ The association information for an Elastic IP associated with the
      -- network interface.
    , inisitPrivateIpAddressesSet :: [InstancePrivateIpAddressesSetItemType]
      -- ^ The private IP addresses associated with the network interface.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML InstanceNetworkInterfaceSetItemType where
    xmlPickler = ec2XML

data InstancePrivateIpAddressesSetItemType = InstancePrivateIpAddressesSetItemType
    { ipiasitPrivateIpAddress :: !Text
      -- ^ The private IP address of the network interface
    , ipiasitPrivateDnsName   :: !Text
      -- ^ The private DNS name.
    , ipiasitPrimary          :: !Bool
      -- ^ Indicates whether this IP address is the primary private IP
      -- address of the network interface.
    , ipiasitAssociation      :: !InstanceNetworkInterfaceAssociationType
      -- ^ The association information for an Elastic IP address associated
      -- with the network interface.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML InstancePrivateIpAddressesSetItemType where
    xmlPickler = ec2XML

-- data InstanceStateChangeType = InstanceStateChangeType
--     { isctInstanceId    :: !Text
--       -- ^ The instance ID.
--     , isctCurrentState  :: !InstanceStateType
--       -- ^ The current state of the instance.
--     , isctPreviousState :: !InstanceStateType
--       -- ^ The previous state of the instance.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery InstanceStateChangeType

-- instance IsXML InstanceStateChangeType where
--     xmlPickler = ec2XML

data InstanceStateType = InstanceStateType
    { istCode :: !Integer
      -- ^ The low byte represents the state. The high byte is an opaque
      -- internal value and should be ignored.
    , istName :: !Text
      -- ^ The current state of the instance.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML InstanceStateType where
    xmlPickler = ec2XML

-- data InstanceStatusDetailsSetType = InstanceStatusDetailsSetType
--     { isdstName          :: !Text
--       -- ^ The type of instance status detail.
--     , isdstStatus        :: !Text
--       -- ^ The status.
--     , isdstImpairedSince :: !UTCTime
--       -- ^ The time when a status check failed. For an instance that was
--       -- launched and impaired, this is the time when the instance was
--       -- launched.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery InstanceStatusDetailsSetType

-- instance IsXML InstanceStatusDetailsSetType where
--     xmlPickler = ec2XML

-- data InstanceStatusEventsSetType = InstanceStatusEventsSetType
--     { isest[:: !InstanceStatusEventType]
--       -- ^ The scheduled events for the instance.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery InstanceStatusEventsSetType

-- instance IsXML InstanceStatusEventsSetType where
--     xmlPickler = ec2XML

-- data InstanceStatusEventType = InstanceStatusEventType
--     { isetCode        :: !Text
--       -- ^ The associated code of the event.
--     , isetDescription :: !Text
--       -- ^ A description of the event.
--     , isetNotBefore   :: !UTCTime
--       -- ^ The earliest scheduled start time for the event.
--     , isetNotAfter    :: !UTCTime
--       -- ^ The latest scheduled end time for the event.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery InstanceStatusEventType

-- instance IsXML InstanceStatusEventType where
--     xmlPickler = ec2XML

-- data InstanceStatusItemType = InstanceStatusItemType
--     { isitInstanceId       :: !Text
--       -- ^ The ID of the instance.
--     , isitAvailabilityZone :: !Text
--       -- ^ The Availability Zone of the instance.
--     , isitEventsSet        :: !InstanceStatusEventsSetType
--       -- ^ Extra information regarding events associated with the instance.
--     , isitInstanceState    :: !InstanceStateType
--       -- ^ The intended state of the instance. Calls to
--       -- DescribeInstanceStatus require that an instance be in the running
--       -- state.
--     , isitSystemStatus     :: !InstanceStatusType
--       -- ^ Reports impaired functionality that stems from issues related to
--       -- the systems that support an instance, such as hardware failures
--       -- and network connectivity problems.
--     , isitInstanceStatus   :: !InstanceStatusType
--       -- ^ Reports impaired functionality that arises from problems internal
--       -- to the instance. The DescribeInstanceStatus response elements
--       -- report such problems as impaired reachability.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery InstanceStatusItemType

-- instance IsXML InstanceStatusItemType where
--     xmlPickler = ec2XML

-- data InstanceStatusSetType = InstanceStatusSetType
--     { isst[:: !InstanceStatusItemType]
--       -- ^ The status of the instance.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery InstanceStatusSetType

-- instance IsXML InstanceStatusSetType where
--     xmlPickler = ec2XML

-- data InstanceStatusType = InstanceStatusType
--     { istStatus  :: !Text
--       -- ^ The status.
--     , istDetails :: !InstanceStatusDetailsSetType
--       -- ^ The system instance health or application instance health.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery InstanceStatusType

-- instance IsXML InstanceStatusType where
--     xmlPickler = ec2XML

-- data InternetGatewayAttachmentType = InternetGatewayAttachmentType
--     { igatVpcId :: !Text
--       -- ^ The ID of the VPC.
--     , igatState :: !Text
--       -- ^ The current state of the attachment.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery InternetGatewayAttachmentType

-- instance IsXML InternetGatewayAttachmentType where
--     xmlPickler = ec2XML

-- data InternetGatewayType = InternetGatewayType
--     { igtInternetGatewayId :: !Text
--       -- ^ The ID of the Internet gateway.
--     , igtAttachmentSet     :: !InternetGatewayAttachmentType
--       -- ^ Any VPCs attached to the Internet gateway, each one wrapped in an
--       -- item element.
--     , igtTagSet            :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery InternetGatewayType

-- instance IsXML InternetGatewayType where
--     xmlPickler = ec2XML

data UserIdGroupPair = UserIdGroupPair
    { uigUserId    :: Maybe Text
      -- ^ The ID of an AWS account. Cannot be used when specifying a CIDR
      -- IP address range.
    , uigGroupId   :: Maybe Text
      -- ^ The ID of the security group in the specified AWS account.
      -- Cannot be used when specifying a CIDR IP address range.
    , uigGroupName :: Maybe Text
      -- ^ The name of the security group in the specified AWS account.
      -- Cannot be used when specifying a CIDR IP address range.
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery UserIdGroupPair

instance IsXML UserIdGroupPair where
    xmlPickler = ec2ItemXML

data IpPermissionType = IpPermissionType
    { iptIpProtocol :: !Protocol
      -- ^ The protocol.
    , iptFromPort   :: !Integer
      -- ^ The start of port range for the ICMP and UDP protocols, or an ICMP
      -- type number. A value of -1 indicates all ICMP types.
    , iptToPort     :: !Integer
      -- ^ The end of port range for the ICMP and UDP protocols, or an ICMP
      -- code. A value of -1 indicates all ICMP codes for the given ICMP type.
    , iptGroups     :: [UserIdGroupPair]
      -- ^ A list of security group and AWS account ID pairs.
    , iptIpRanges   :: [IpRange]
      -- ^ A list of IP ranges.
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery IpPermissionType

instance IsXML IpPermissionType where
    xmlPickler = ec2ItemXML

data IpRange = IpRange
    { irCidrIp :: !Text
      -- ^ The CIDR range. You can either specify a CIDR range or a source
      -- security group, not both.
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery IpRange

instance IsXML IpRange where
    xmlPickler = ec2ItemXML

-- data LaunchPermissionItemType = LaunchPermissionItemType
--     { lpitGroup  :: !Text
--       -- ^ The name of the group.
--     , lpitUserId :: !Text
--       -- ^ The AWS account ID.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery LaunchPermissionItemType

-- instance IsXML LaunchPermissionItemType where
--     xmlPickler = ec2XML

-- data LaunchSpecificationRequestType = LaunchSpecificationRequestType
--     { lsrtImageId             :: !Text
--       -- ^ The AMI ID.
--     , lsrtKeyName             :: !Text
--       -- ^ The name of the key pair.
--     , lsrtGroupSet            :: !GroupItemType
--       -- ^ A list of security groups. Each group is wrapped in an item
--       -- element.
--     , lsrtUserData            :: !UserDataType
--       -- ^ Base64-encoded MIME user data made available to the instance(s)
--       -- in the reservation.
--     , lsrtInstanceType        :: !Text
--       -- ^ The instance type.
--     , lsrtPlacement           :: !PlacementRequestType
--       -- ^ The placement information for the instance.
--     , lsrtKernelId            :: !Text
--       -- ^ The ID of the kernel to select.
--     , lsrtRamdiskId           :: !Text
--       -- ^ The ID of the RAM disk to select. Some kernels require additional
--       -- drivers at launch. Check the kernel requirements for information
--       -- on whether you need to specify a RAM disk and search for the
--       -- kernel ID.
--     , lsrtBlockDeviceMapping  :: !BlockDeviceMappingItemType
--       -- ^ Any block device mapping entries for the instance. Each entry is
--       -- wrapped in an item element.
--     , lsrtMonitoring          :: !MonitoringInstanceType
--       -- ^ The monitoring information for the instance.
--     , lsrtSubnetId            :: !Text
--       -- ^ The ID of the subnet.
--     , lsrtNetworkInterfaceSet :: !InstanceNetworkInterfaceSetItemRequestType
--       -- ^ The network interfaces associated with the instance.
--     , lsrtIamInstanceProfile  :: !IamInstanceProfileRequestType
--       -- ^ The IAM Instance Profile (IIP) associated with the instance.
--     , lsrtEbsOptimized        :: !Bool
--       -- ^ Indicates whether the instance is optimized for EBS I/O. This
--       -- optimization provides dedicated throughput to Amazon EBS and an
--       -- optimized configuration stack to provide optimal EBS I/O
--       -- performance. This optimization isn't available with all instance
--       -- types. Additional usage charges apply when using an EBS Optimized
--       -- instance.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery LaunchSpecificationRequestType

-- instance IsXML LaunchSpecificationRequestType where
--     xmlPickler = ec2XML

-- data LaunchSpecificationResponseType = LaunchSpecificationResponseType
--     { lsruImageId             :: !Text
--       -- ^ The AMI ID.
--     , lsruKeyName             :: !Text
--       -- ^ The name of the key pair.
--     , lsruGroupSet            :: !GroupItemType
--       -- ^ A list of security groups. Each group is wrapped in an item
--       -- element.
--     , lsruInstanceType        :: !Text
--       -- ^ The instance type.
--     , lsruPlacement           :: !PlacementRequestType
--       -- ^ The placement information for the instance.
--     , lsruKernelId            :: !Text
--       -- ^ The ID of the kernel to select.
--     , lsruRamdiskId           :: !Text
--       -- ^ The ID of the RAM disk to select. Some kernels require additional
--       -- drivers at launch. Check the kernel requirements for information
--       -- on whether you need to specify a RAM disk and search for the
--       -- kernel ID.
--     , lsruBlockDeviceMapping  :: !BlockDeviceMappingItemType
--       -- ^ Any block device mapping entries for the instance. Each entry is
--       -- wrapped in an item element.
--     , lsruMonitoring          :: !MonitoringInstanceType
--       -- ^ The monitoring information for the instance.
--     , lsruSubnetId            :: !Text
--       -- ^ The ID of the subnet.
--     , lsruNetworkInterfaceSet :: !InstanceNetworkInterfaceSetItemRequestType
--       -- ^ The network interfaces for the instance.
--     , lsruIamInstanceProfile  :: !IamInstanceProfileRequestType
--       -- ^ The IAM Instance Profile (IIP) associated with the instance.
--     , lsruEbsOptimized        :: !Bool
--       -- ^ Indicates whether the instance is optimized for EBS I/O. This
--       -- optimization provides dedicated throughput to Amazon EBS and an
--       -- optimized configuration stack to provide optimal EBS I/O
--       -- performance. This optimization isn't available with all instance
--       -- types. Additional usage charges apply when using an EBS Optimized
--       -- instance.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery LaunchSpecificationResponseType

-- instance IsXML LaunchSpecificationResponseType where
--     xmlPickler = ec2XML

data MonitoringInstanceType = MonitoringInstanceType
    { mitEnabled :: !Bool
      -- ^ Indicates whether monitoring is enabled for the instance.
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery MonitoringInstanceType

-- instance IsXML MonitoringInstanceType where
--     xmlPickler = ec2XML

-- data MonitorInstancesResponseSetItemType = MonitorInstancesResponseSetItemType
--     { mirsitInstanceId :: !Text
--       -- ^ The instance ID.
--     , mirsitMonitoring :: !InstanceMonitoringStateType
--       -- ^ The monitoring information.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery MonitorInstancesResponseSetItemType

-- instance IsXML MonitorInstancesResponseSetItemType where
--     xmlPickler = ec2XML

-- data NetworkAclAssociationType = NetworkAclAssociationType
--     { naatNetworkAclAssociationId :: !Text
--       -- ^ An identifier representing the association between a network ACL
--       -- and a subnet.
--     , naatNetworkAclId            :: !Text
--       -- ^ The ID of the network ACL.
--     , naatSubnetId                :: !Text
--       -- ^ The ID of the subnet.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery NetworkAclAssociationType

-- instance IsXML NetworkAclAssociationType where
--     xmlPickler = ec2XML

-- data NetworkAclEntryType = NetworkAclEntryType
--     { naetRuleNumber   :: !Integer
--       -- ^ The rule number for the entry. ACL entries are processed in
--       -- ascending order by rule number.
--     , naetProtocol     :: !Integer
--       -- ^ The protocol. A value of -1 means all protocols.
--     , naetRuleAction   :: !Text
--       -- ^ Indicates whether to allow or deny the traffic that matches the
--       -- rule.
--     , naetEgress       :: !Bool
--       -- ^ Indicates an egress rule (rule is applied to traffic leaving the
--       -- subnet). Value of true indicates egress.
--     , naetCidrBlock    :: !Text
--       -- ^ The network range to allow or deny, in CIDR notation.
--     , naetIcmpTypeCode :: !IcmpTypeCodeType
--       -- ^ ICMP protocol: The ICMP type and code.
--     , naetPortRange    :: !PortRangeType
--       -- ^ ICMP or UDP protocols: The range of ports the rule applies to.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery NetworkAclEntryType

-- instance IsXML NetworkAclEntryType where
--     xmlPickler = ec2XML

-- data NetworkAclType = NetworkAclType
--     { natNetworkAclId   :: !Text
--       -- ^ The ID of the network ACL.
--     , natVpcId          :: !Text
--       -- ^ The ID of the VPC for the network ACL.
--     , natDefault        :: !Bool
--       -- ^ Indicates whether this is the default network ACL for the VPC.
--     , natEntrySet       :: !NetworkAclEntryType
--       -- ^ A list of entries (rules) in the network ACL. Each entry is
--       -- wrapped in an item element.
--     , natAssociationSet :: !NetworkAclAssociationType
--       -- ^ A list of associations between the network ACL and one or more
--       -- subnets. Each association is wrapped in an item element.
--     , natTagSet         :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery NetworkAclType

-- instance IsXML NetworkAclType where
--     xmlPickler = ec2XML

data NetworkInterfaceAssociationType = NetworkInterfaceAssociationType
    { niatPublicIp      :: !Text
      -- ^ The address of the Elastic IP address bound to the network
      -- interface.
    , niatPublicDnsName :: !Text
      -- ^ The public DNS name.
    , niatIpOwnerId     :: !Text
      -- ^ The ID of the Elastic IP address owner.
    , niatAllocationID  :: !Text
      -- ^ The allocation ID.
    , niatAssociationID :: !Text
      -- ^ The association ID.
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery NetworkInterfaceAssociationType

-- instance IsXML NetworkInterfaceAssociationType where
--     xmlPickler = ec2XML

data NetworkInterfaceAttachmentType = NetworkInterfaceAttachmentType
    { niatAttachmentID :: !Text
      -- ^ The ID of the network interface attachment.
    , niatInstanceID   :: !Text
      -- ^ The ID of the instance.
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery NetworkInterfaceAttachmentType

-- instance IsXML NetworkInterfaceAttachmentType where
--     xmlPickler = ec2XML

data NetworkInterfacePrivateIpAddressesSetItemType = NetworkInterfacePrivateIpAddressesSetItemType
    { nipiasitPrivateIpAddress :: !Text
      -- ^ The private IP address of the network interface.
    , nipiasitPrivateDnsName   :: !Text
      -- ^ The private DNS name.
    , nipiasitPrimary          :: !Bool
      -- ^ Indicates whether this IP address is the primary private IP
      -- address of the network interface.
    , nipiasitAssociation      :: !NetworkInterfaceAssociationType
      -- ^ The association information for an Elastic IP address associated
      -- with the network interface.
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery NetworkInterfacePrivateIpAddressesSetItemType

-- instance IsXML NetworkInterfacePrivateIpAddressesSetItemType where
--     xmlPickler = ec2XML

data NetworkInterfaceType = NetworkInterfaceType
    { nitNetworkInterfaceId    :: !Text
      -- ^ The ID of the network interface.
    , nitSubnetId              :: !Text
      -- ^ The ID of the subnet.
    , niuNetworkInterfaceId    :: !Text
      -- ^ The ID of the network interface.
    , niuSubnetId              :: !Text
      -- ^ The ID of the subnet.
    , niuVpcId                 :: !Text
      -- ^ The ID of the VPC.
    , niuAvailabilityZone      :: !Text
      -- ^ The Availability Zone.
    , niuDescription           :: !Text
      -- ^ A description.
    , niuOwnerId               :: !Text
      -- ^ The ID of the customer who created the interface.
    , niuRequesterId           :: !Text
      -- ^ The ID of the entity that launched the instance on your behalf
      -- (for example, AWS Management Console or Auto Scaling)
    , niuRequesterManaged      :: !Text
      -- ^ Indicates whether the network interface is being managed by AWS.
    , niuStatus                :: !Text
      -- ^ The status of the network interface.
    , niuMacAddress            :: !Text
      -- ^ The MAC address.
    , niuPrivateIpAddress      :: !Text
      -- ^ The IP address of the network interface within the subnet.
    , niuPrivateDnsName        :: !Text
      -- ^ The private DNS name.
    , niuSourceDestCheck       :: !Bool
      -- ^ Indicates whether traffic to or from the instance is validated.
    , niuGroupSet              :: !GroupItemType
      -- ^ The security group.
    , niuAttachment            :: !NetworkInterfaceAttachmentType
      -- ^ The network interface attachment.
    , niuAssociation           :: !NetworkInterfaceAssociationType
      -- ^ The association information for an Elastic IP associated with the
      -- network interface.
    , niuTagSet                :: !ResourceTagSetItemType
      -- ^ The tags assigned to the resource.
    , niuPrivateIpAddressesSet :: !NetworkInterfacePrivateIpAddressesSetItemType
      -- ^ The private IP addresses associated with the network interface.
      -- [are returned in a set.]
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery NetworkInterfaceType

-- instance IsXML NetworkInterfaceType where
--     xmlPickler = ec2XML

-- data PlacementGroupInfoType = PlacementGroupInfoType
--     { pgitGroupName :: !Text
--       -- ^ The name of the placement group.
--     , pgitStrategy  :: !Text
--       -- ^ The placement strategy.
--     , pgitState     :: !Text
--       -- ^ The status of the placement group.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery PlacementGroupInfoType

-- instance IsXML PlacementGroupInfoType where
--     xmlPickler = ec2XML

-- data PlacementRequestType = PlacementRequestType
--     { prtAvailabilityZone :: !Text
--       -- ^ The Availability Zone for the instance.
--     , prtGroupName        :: !Text
--       -- ^ The name of a placement group for the instance.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery PlacementRequestType

-- instance IsXML PlacementRequestType where
--     xmlPickler = ec2XML

-- FIXME: is the corresponding request type now irrelavant?
data PlacementType = PlacementType
    { pruAvailabilityZone :: Maybe AvailabilityZone
      -- ^ The Availability Zone of the instance.
    , pruGroupName        :: Maybe Text
      -- ^ The name of the placement group the instance is in
      -- (for cluster compute instances).
    , pruTenancy          :: Maybe Text
      -- ^ The tenancy of the instance (if the instance is running within a -- VPC).
      -- FIXME: switch to enum default | dedicated
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery PlacementType

instance IsXML PlacementType where
    xmlPickler = ec2XML

-- data PortRangeType = PortRangeType
--     { prtFrom :: !Integer
--       -- ^ The first port in the range.
--     , prtTo   :: !Integer
--       -- ^ The last port in the range.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery PortRangeType

-- instance IsXML PortRangeType where
--     xmlPickler = ec2XML

-- data PriceScheduleRequestSetItemType = PriceScheduleRequestSetItemType
--     { psrsitTerm         :: !Integer
--       -- ^ The number of months remaining in the reservation. For example, 2
--       -- is the second to the last month before the capacity reservation
--       -- expires.
--     , psrsitPrice        :: !Double
--       -- ^ The fixed price for the term.
--     , psrsitCurrencyCode :: !Text
--       -- ^ The currency for transacting the Reserved Instance resale. At
--       -- this time, the only supported currency is USD.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery PriceScheduleRequestSetItemType

-- instance IsXML PriceScheduleRequestSetItemType where
--     xmlPickler = ec2XML

data PriceScheduleSetItemType = PriceScheduleSetItemType
    { pssitTerm         :: !Integer
      -- ^ The number of months remaining in the reservation. For example, 2
      -- is the second to the last month before the capacity reservation
      -- expires.
    , pssitPrice        :: !Double
      -- ^ The fixed price for the term.
    , pssitCurrencyCode :: !Text
      -- ^ The currency for transacting the Reserved Instance resale. At
      -- this time, the only supported currency is USD.
    , pssitActive       :: !Bool
      -- ^ The current price schedule, as determined by the term remaining
      -- for the Reserved Instance in the listing.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML PriceScheduleSetItemType where
    xmlPickler = ec2XML

-- data PriceScheduleSetType = PriceScheduleSetType
--     { psst[:: !PriceScheduleSetItemType]
--       -- ^ The Reserved Instance listing price schedule item.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery PriceScheduleSetType

-- instance IsXML PriceScheduleSetType where
--     xmlPickler = ec2XML

-- data PricingDetailsSetItemType = PricingDetailsSetItemType
--     { pdsitPrice :: !Integer
--       -- ^ The price per instance.
--     , pdsitCount :: !Integer
--       -- ^ The number of instances available for the price.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery PricingDetailsSetItemType

-- instance IsXML PricingDetailsSetItemType where
--     xmlPickler = ec2XML

-- data PrivateIpAddressesSetItemRequestType = PrivateIpAddressesSetItemRequestType
--     { piasirtPrivateIpAddressesSet :: !AssignPrivateIpAddressesSetItemRequestType
--       -- ^ The list of private IP addresses.
--     , piasirtPrimary               :: !Bool
--       -- ^ Indicates whether the private IP address is the primary private
--       -- IP address.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery PrivateIpAddressesSetItemRequestType

-- instance IsXML PrivateIpAddressesSetItemRequestType where
--     xmlPickler = ec2XML

-- data ProductCodeItemType = ProductCodeItemType
--     { pcitProductCode :: !Text
--       -- ^ The product code.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery ProductCodeItemType

-- instance IsXML ProductCodeItemType where
--     xmlPickler = ec2XML

data ProductCodesSetItemType = ProductCodesSetItemType
    { pcsitProductCode :: !Text
      -- ^ The product code.
    , pcsitType        :: !Text
      -- ^ The type of product code.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML ProductCodesSetItemType where
    xmlPickler = ec2ItemXML

-- data ProductDescriptionSetItemType = ProductDescriptionSetItemType
--     { pdsitProductDescription :: !Text
--       -- ^ The description of the AMI.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery ProductDescriptionSetItemType

-- instance IsXML ProductDescriptionSetItemType where
--     xmlPickler = ec2XML

-- data PropagatingVgwType = PropagatingVgwType
--     { pvtGatewayID :: !Text
--       -- ^ The ID of the virtual private gateway (VGW).
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery PropagatingVgwType

-- instance IsXML PropagatingVgwType where
--     xmlPickler = ec2XML

-- data RecurringChargesSetItemType = RecurringChargesSetItemType
--     { rcsitFrequency :: !Text
--       -- ^ The frequency of the recurring charge.
--     , rcsitAmount    :: !Double
--       -- ^ The amount of the recurring charge.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery RecurringChargesSetItemType

-- instance IsXML RecurringChargesSetItemType where
--     xmlPickler = ec2XML

data RegionItemType = RegionItemType
    { ritRegionName     :: !Text
      -- ^ The name of the region.
    , ritRegionEndpoint :: !Text
      -- ^ The region service endpoint.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML RegionItemType where
    xmlPickler = ec2ItemXML

data ReservationInfoType = ReservationInfoType
    { ritReservationId :: !Text
      -- ^ The ID of the reservation.
    , ritOwnerId       :: !Text
      -- ^ The ID of the AWS account that owns the reservation.
    , ritGroupSet      :: [GroupItemType]
      -- ^ A list of security groups.
    , ritInstancesSet  :: [RunningInstancesItemType]
      -- ^ A list of instances.
    , ritRequesterId   :: Maybe Text
      -- ^ The ID of the requester that launched the instances on your
      -- behalf (for example, AWS Management Console or Auto Scaling).
    } deriving (Eq, Ord, Show, Generic)

instance IsXML ReservationInfoType where
    xmlPickler = ec2ItemXML

-- data ReservedInstanceLimitPriceType = ReservedInstanceLimitPriceType
--     { rilptAmount       :: !Double
--       -- ^ Used for Reserved Instance Marketplace offerings. Specifies the
--       -- limit price on the total order (instanceCount * price).
--     , rilptCurrencyCode :: !Double
--       -- ^ Currency in which the limitPrice amount is specified. At this
--       -- time, the only supported currency is USD.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery ReservedInstanceLimitPriceType

-- instance IsXML ReservedInstanceLimitPriceType where
--     xmlPickler = ec2XML

data ResourceTagSetItemType = ResourceTagSetItemType
    { rtsitKey   :: !Text
      -- ^ The tag key.
    , rtsitValue :: !Text
      -- ^ The tag value.
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery ResourceTagSetItemType

instance IsXML ResourceTagSetItemType where
    xmlPickler = ec2ItemXML

-- data RouteTableAssociationType = RouteTableAssociationType
--     { rtatRouteTableAssociationId :: !Text
--       -- ^ An identifier representing the association between a route table
--       -- and a subnet.
--     , rtatRouteTableId            :: !Text
--       -- ^ The ID of the route table.
--     , rtatSubnetId                :: !Text
--       -- ^ The ID of the subnet.
--     , rtatMain                    :: !Bool
--       -- ^ Indicates whether this is the main route table.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery RouteTableAssociationType

-- instance IsXML RouteTableAssociationType where
--     xmlPickler = ec2XML

-- data RouteTableType = RouteTableType
--     { rttRouteTableId      :: !Text
--       -- ^ The route table's ID.
--     , rttVpcId             :: !Text
--       -- ^ The ID of the VPC for the route table.
--     , rttRouteSet          :: !RouteType
--       -- ^ A list of routes in the route table. Each route is wrapped in an
--       -- item element.
--     , rttAssociationSet    :: !RouteTableAssociationType
--       -- ^ A list of associations between the route table and one or more
--       -- subnets. Each association is wrapped in an item element.
--     , rttPropagatingVgwSet :: !PropagatingVgwType
--       -- ^ The IDs of any virtual private gateways (VGW) propagating routes,
--       -- each route wrapped in an item element.
--     , rttTagSet            :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery RouteTableType

-- instance IsXML RouteTableType where
--     xmlPickler = ec2XML

-- data RouteType = RouteType
--     { rtDestinationCidrBlock :: !Text
--       -- ^ The CIDR address block used for the destination match.
--     , rtGatewayId            :: !Text
--       -- ^ The ID of a gateway attached to your VPC.
--     , rtInstanceId           :: !Text
--       -- ^ The ID of a NAT instance in your VPC.
--     , rtInstanceOwnerId      :: !Text
--       -- ^ The owner of the instance.
--     , rtNetworkInterfaceId   :: !Text
--       -- ^ The network interface ID.
--     , rtState                :: !Text
--       -- ^ The state of the route. The blackhole state indicates that the
--       -- route's target isn't available (for example, the specified
--       -- gateway isn't attached to the VPC, or the specified NAT instance
--       -- has been terminated).
--     , rtOrigin               :: !Text
--       -- ^ Describes how the route was created.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery RouteType

-- instance IsXML RouteType where
--     xmlPickler = ec2XML

data RunningInstancesItemType = RunningInstancesItemType
    { riitInstanceId            :: !Text
      -- ^ The ID of the instance launched.
    , riitImageId               :: !Text
      -- ^ The ID of the AMI used to launch the instance.
    , riitInstanceState         :: !InstanceStateType
      -- ^ The current state of the instance.
    , riitPrivateDnsName        :: Maybe Text
      -- ^ The private DNS name assigned to the instance. This DNS name can
      -- only be used inside the Amazon EC2 network. This element remains
      -- empty until the instance enters the running state.
    , riitDnsName               :: Maybe Text
      -- ^ The public DNS name assigned to the instance. This element
      -- remains empty until the instance enters the running state.
    , riitReason                :: Maybe Text
      -- ^ The reason for the most recent state transition.
      -- This might be an empty string.
    , riitKeyName               :: !Text
      -- ^ The key pair name, if this instance was launched with an
      -- associated key pair.
    , riitAmiLaunchIndex        :: !Text
      -- ^ The AMI launch index, which can be used to find this instance in
      -- the launch group.
    , riitProductCodes          :: [ProductCodesSetItemType]
      -- ^ The product codes attached to this instance.
    , riitInstanceType          :: InstanceType
      -- ^ The instance type.
    , riitLaunchTime            :: !UTCTime
      -- ^ The time the instance was launched.
    , riitPlacement             :: !PlacementType
      -- ^ The location where the instance launched.
    , riitKernelId              :: Maybe Text
      -- ^ The kernel associated with this instance.
    , riitRamdiskId             :: Maybe Text
      -- ^ The RAM disk associated with this instance.
    , riitPlatform              :: Maybe Text
      -- ^ The value is Windows for Windows AMIs; otherwise blank.
    , riitMonitoring            :: !InstanceMonitoringStateType
      -- ^ The monitoring information for the instance.
    , riitSubnetId              :: Maybe Text
      -- ^ The ID of the subnet in which the instance is running.
    , riitVpcId                 :: Maybe Text
      -- ^ The ID of the VPC in which the instance is running.
    , riitPrivateIpAddress      :: Maybe Text
      -- ^ The private IP address assigned to the instance.
    , riitIpAddress             :: Maybe Text
      -- ^ The IP address of the instance.
    , riitSourceDestCheck       :: Maybe Bool
      -- ^ Specifies whether to enable an instance launched in a VPC to
      -- perform NAT. This controls whether source/destination checking is
      -- enabled on the instance. A value of true means checking is
      -- enabled, and false means checking is disabled. The value must be
      -- false for the instance to perform NAT. For more information, go
      -- to NAT Instances in the Amazon Virtual Private Cloud User Guide.
    , riitGroupSet              :: [GroupItemType]
      -- ^ A list of the security groups for the instance.
    , riitStateReason           :: Maybe StateReasonType
      -- ^ The reason for the most recent state transition. See
      -- StateReasonType for a listing of supported state change codes.
    , riitArchitecture          :: !Text
      -- ^ The architecture of the image.
    , riitRootDeviceType        :: !Text
      -- ^ The root device type used by the AMI. The AMI can use an Amazon
      -- EBS or instance store root device.
    , riitRootDeviceName        :: Maybe Text
      -- ^ The root device name (for example, /dev/sda1).
    , riitBlockDeviceMapping    :: [InstanceBlockDeviceMappingResponseItemType]
      -- ^ Any block device mapping entries for the instance.
    , riitInstanceLifecycle     :: Maybe Text
      -- ^ Indicates whether this is a Spot Instance.
    , riitSpotInstanceRequestId :: Maybe Text
      -- ^ The ID of the Spot Instance request.
    , riitVirtualizationType    :: !Text
      -- ^ The instance's virtualization type.
    , riitClientToken           :: Maybe Text
      -- ^ The idempotency token you provided when you launched the instance.
    , riitTagSet                :: [ResourceTagSetItemType]
      -- ^ Any tags assigned to the resource.
    , riitHypervisor            :: !Text
      -- ^ The instance's hypervisor type.
    , riitNetworkInterfaceSet   :: [InstanceNetworkInterfaceSetItemType]
      -- ^ The network interfaces for the instance.
    , riitIamInstanceProfile    :: Maybe IamInstanceProfileResponseType
      -- ^ The IAM Instance Profile (IIP) associated with the instance.
    , riitEbsOptimized          :: !Bool
      -- ^ Indicates whether the instance is optimized for EBS I/O.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML RunningInstancesItemType where
    xmlPickler = ec2ItemXML

-- data SecurityGroupIdSetItemType = SecurityGroupIdSetItemType
--     { sgisitGroupId :: !Text
--       -- ^ The ID of the security group associated with the network
--       -- interface.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery SecurityGroupIdSetItemType

-- instance IsXML SecurityGroupIdSetItemType where
--     xmlPickler = ec2XML

data SecurityGroupItemType = SecurityGroupItemType
    { sgitOwnerId             :: !Text
      -- ^ The AWS account ID of the owner of the security group.
    , sgitGroupId             :: !Text
      -- ^ The ID of the security group.
    , sgitGroupName           :: !Text
      -- ^ The name of the security group.
    , sgitGroupDescription    :: !Text
      -- ^ A description of the security group.
    , sgitVpcId               :: Maybe Text
      -- ^ [EC2-VPC] The ID of the VPC for the security group.
    , sgitIpPermissions       :: [IpPermissionType]
      -- ^ A list of inbound rules associated with the security group.
    , sgitIpPermissionsEgress :: [IpPermissionType]
      -- ^ [EC2-VPC] A list of outbound rules associated with the security group.
    -- , sgitTagSet              :: [ResourceTagSetItemType]
    --   -- ^ Any tags assigned to the resource, each one wrapped in an item element.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML SecurityGroupItemType where
    xmlPickler = ec2ItemXML

-- data SpotDatafeedSubscriptionType = SpotDatafeedSubscriptionType
--     { sdstOwnerId :: !Text
--       -- ^ The AWS account ID of the account.
--     , sdstBucket  :: !Text
--       -- ^ The Amazon S3 bucket where the Spot Instance datafeed is located.
--     , sdstPrefix  :: !Text
--       -- ^ The prefix that is prepended to datafeed files.
--     , sdstState   :: !Text
--       -- ^ The state of the Spot Instance datafeed subscription.
--     , sdstFault   :: !SpotInstanceStateFaultType
--       -- ^ The fault codes for the Spot Instance request, if any.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery SpotDatafeedSubscriptionType

-- instance IsXML SpotDatafeedSubscriptionType where
--     xmlPickler = ec2XML

-- data SpotInstanceRequestSetItemType = SpotInstanceRequestSetItemType
--     { sirsitSpotInstanceRequestId    :: !Text
--       -- ^ The ID of the Spot Instance request.
--     , sirsitSpotPrice                :: !Text
--       -- ^ The maximum hourly price for any Spot Instance launched to
--       -- fulfill the request.
--     , sirsitType                     :: !Text
--       -- ^ The Spot Instance request type.
--     , sirsitState                    :: !Text
--       -- ^ The state of the Spot Instance request. Spot bid status
--       -- information can help you track your Spot Instance requests. For
--       -- information, see Tracking Spot Requests with Bid Status Codes in
--       -- the Amazon Elastic Compute Cloud User Guide.
--     , sirsitFault                    :: !SpotInstanceStateFaultType
--       -- ^ The fault codes for the Spot Instance request, if any.
--     , sirsitStatus                   :: !SpotInstanceStatusMessageType
--       -- ^ The status code and status message describing the Spot Instance
--       -- request.
--     , sirsitValidFrom                :: !UTCTime
--       -- ^ The start date of the request. If this is a one-time request, the
--       -- request becomes active at this date and time and remains active
--       -- until all instances launch, the request expires, or the request
--       -- is canceled. If the request is persistent, the request becomes
--       -- active at this date and time and remains active until it expires
--       -- or is canceled.
--     , sirsitValidUntil               :: !UTCTime
--       -- ^ The end date of the request. If this is a one-time request, the
--       -- request remains active until all instances launch, the request is
--       -- canceled, or this date is reached. If the request is persistent,
--       -- it remains active until it is canceled or this date is reached.
--     , sirsitLaunchGroup              :: !Text
--       -- ^ The instance launch group. Launch groups are Spot Instances that
--       -- launch together and terminate together.
--     , sirsitAvailabilityZoneGroup    :: !Text
--       -- ^ The Availability Zone group. If you specify the same Availability
--       -- Zone group for all Spot Instance requests, all Spot Instances are
--       -- launched in the same Availability Zone.
--     , sirsitLaunchedAvailabilityZone :: !Text
--       -- ^ The Availability Zone in which the bid is launched.
--     , sirsitLaunchSpecification      :: !LaunchSpecificationResponseType
--       -- ^ Additional information for launching instances.
--     , sirsitInstanceId               :: !Text
--       -- ^ The instance ID, if an instance has been launched to fulfill the
--       -- Spot Instance request.
--     , sirsitCreateTime               :: !UTCTime
--       -- ^ The time stamp when the Spot Instance request was created.
--     , sirsitProductDescription       :: !Text
--       -- ^ The product description associated with the Spot Instance.
--     , sirsitTagSet                   :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery SpotInstanceRequestSetItemType

-- instance IsXML SpotInstanceRequestSetItemType where
--     xmlPickler = ec2XML

-- data SpotInstanceStateFaultType = SpotInstanceStateFaultType
--     { sisftCode    :: !Text
--       -- ^ The reason code for the Spot Instance state change.
--     , sisftMessage :: !Text
--       -- ^ The message for the Spot Instance state change.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery SpotInstanceStateFaultType

-- instance IsXML SpotInstanceStateFaultType where
--     xmlPickler = ec2XML

-- data SpotInstanceStatusMessageType = SpotInstanceStatusMessageType
--     { sismtCode       :: !Text
--       -- ^ The status code of the request.
--     , sismtUpdateTime :: !UTCTime
--       -- ^ The time of the most recent status update.
--     , sismtMessage    :: !Text
--       -- ^ The description for the status code for the Spot request.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery SpotInstanceStatusMessageType

-- instance IsXML SpotInstanceStatusMessageType where
--     xmlPickler = ec2XML

-- data SpotPriceHistorySetItemType = SpotPriceHistorySetItemType
--     { sphsitInstanceType       :: !Text
--       -- ^ The instance type.
--     , sphsitProductDescription :: !Text
--       -- ^ A general description of the AMI.
--     , sphsitSpotPrice          :: !Text
--       -- ^ The maximum price you will pay to launch one or more Spot
--       -- Instances.
--     , sphsitTimestamp          :: !UTCTime
--       -- ^ The date and time the request was created.
--     , sphsitAvailabilityZone   :: !Text
--       -- ^ The Availability Zone.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery SpotPriceHistorySetItemType

-- instance IsXML SpotPriceHistorySetItemType where
--     xmlPickler = ec2XML

data StateReasonType = StateReasonType
    { srtCode    :: !Text
      -- ^ The reason code for the state change.
    , srtMessage :: !Text
      -- ^ The message for the state change.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML StateReasonType where
    xmlPickler = ec2XML

-- data SubnetType = SubnetType
--     { stSubnetId                :: !Text
--       -- ^ The ID of the subnet.
--     , stState                   :: !Text
--       -- ^ The current state of the subnet.
--     , stVpcId                   :: !Text
--       -- ^ The ID of the VPC the subnet is in.
--     , stCidrBlock               :: !Text
--       -- ^ The CIDR block assigned to the subnet.
--     , stAvailableIpAddressCount :: !Integer
--       -- ^ The number of unused IP addresses in the subnet (the IP addresses
--       -- for any stopped instances are considered unavailable).
--     , stAvailabilityZone        :: !Text
--       -- ^ The Availability Zone of the subnet.
--     , stDefaultForAz            :: !Bool
--       -- ^ Indicates whether this is the default subnet for the Availability
--       -- Zone.
--     , stMapPublicIpOnLaunch     :: !Bool
--       -- ^ Indicates whether instances launched in this subnet receive a
--       -- public IP address.
--     , stTagSet                  :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery SubnetType

-- instance IsXML SubnetType where
--     xmlPickler = ec2XML

data TagSetItemType = TagSetItemType
    { tsitResourceId   :: !Text
      -- ^ The ID of the resource. For example, ami-1a2b3c4d.
    , tsitResourceType :: !Text
      -- ^ The type of resource.
    , tsitKey          :: !Text
      -- ^ The key of the tag.
    , tsitValue        :: !Text
      -- ^ The value of the tag.
    } deriving (Eq, Ord, Show, Generic)

instance IsXML TagSetItemType where
    xmlPickler = ec2ItemXML

-- data UserDataType = UserDataType
--     { udtData :: !Text
--       -- ^ The Base64-encoded MIME user data made available to the
--       -- instance(s) in the reservation.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery UserDataType

-- instance IsXML UserDataType where
--     xmlPickler = ec2XML

-- data VolumeStatusItemType = VolumeStatusItemType
--     { vsitVolumeId         :: !Text
--       -- ^ The volume ID.
--     , vsitAvailabilityZone :: !Text
--       -- ^ The Availability Zone of the volume.
--     , vsitVolumeStatus     :: !VolumeStatusInfoType
--       -- ^ The volume status. The status of each volume is wrapped in an
--       -- item element.
--     , vsitEventSet         :: !VolumeStatusEventItemType
--       -- ^ A list of events associated with the volume. Each event is
--       -- wrapped in an item element.
--     , vsitActionSet        :: !VolumeStatusActionItemType
--       -- ^ The details of the action. Each action detail is wrapped in an
--       -- item element.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery VolumeStatusItemType

-- instance IsXML VolumeStatusItemType where
--     xmlPickler = ec2XML

-- data VolumeStatusInfoType = VolumeStatusInfoType
--     { vsitStatus  :: !Text
--       -- ^ The status of the volume.
--     , vsitDetails :: !VolumeStatusDetailsItemType
--       -- ^ The details of the volume status. Each volume status detail is
--       -- wrapped in an item type.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery VolumeStatusInfoType

-- instance IsXML VolumeStatusInfoType where
--     xmlPickler = ec2XML

-- data VolumeStatusDetailsItemType = VolumeStatusDetailsItemType
--     { vsditName   :: !Text
--       -- ^ The name of the volume status.
--     , vsditStatus :: !Text
--       -- ^ The intended status of the volume status.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery VolumeStatusDetailsItemType

-- instance IsXML VolumeStatusDetailsItemType where
--     xmlPickler = ec2XML

-- data VolumeStatusEventItemType = VolumeStatusEventItemType
--     { vseitEventType   :: !Text
--       -- ^ The type of this event.
--     , vseitEventId     :: !Text
--       -- ^ The ID of this event.
--     , vseitDescription :: !Text
--       -- ^ A description of the event.
--     , vseitNotBefore   :: !UTCTime
--       -- ^ The earliest start time of the event.
--     , vseitNotAfter    :: !UTCTime
--       -- ^ The latest end time of the event.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery VolumeStatusEventItemType

-- instance IsXML VolumeStatusEventItemType where
--     xmlPickler = ec2XML

-- data VolumeStatusActionItemType = VolumeStatusActionItemType
--     { vsaitCode        :: !Text
--       -- ^ The code identifying the action, for example, enable-volume-io.
--     , vsaitEventType   :: !Text
--       -- ^ The event type associated with this action.
--     , vsaitEventId     :: !Text
--       -- ^ The ID of the event associated with this action.
--     , vsaitDescription :: !Text
--       -- ^ A description of the action.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery VolumeStatusActionItemType

-- instance IsXML VolumeStatusActionItemType where
--     xmlPickler = ec2XML

-- data VpcType = VpcType
--     { vtVpcId           :: !Text
--       -- ^ The ID of the VPC.
--     , vtState           :: !Text
--       -- ^ The current state of the VPC.
--     , vtCidrBlock       :: !Text
--       -- ^ The CIDR block for the VPC.
--     , vtDhcpOptionsId   :: !Text
--       -- ^ The ID of the set of DHCP options you've associated with the VPC
--       -- (or default if the default options are associated with the VPC).
--     , vtTagSet          :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     , vtInstanceTenancy :: !Text
--       -- ^ The allowed tenancy of instances launched into the VPC.
--     , vtIsDefault       :: !Bool
--       -- ^ Indicates whether the VPC is the default VPC.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery VpcType

-- instance IsXML VpcType where
--     xmlPickler = ec2XML

-- data VpnConnectionOptions = VpnConnectionOptions
--     { vcortStaticRoutesOnly :: !Bool
--       -- ^ Indicates whether the VPN connection uses static routes only.
--       -- Static routes must be used for devices that don't support BGP.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery VpnConnectionOptionsResponseType

-- instance IsXML VpnConnectionOptionsResponseType where
--     xmlPickler = ec2XML

-- data VpnConnectionType = VpnConnectionType
--     { vctVpnConnectionId              :: !Text
--       -- ^ The ID of the VPN connection.
--     , vctState                        :: !Text
--       -- ^ The current state of the VPN connection.
--     , vctCustomerGatewayConfiguration :: !Text
--       -- ^ The configuration information for the VPN connection's customer
--       -- gateway (in the native XML format). This element is always
--       -- present in the CreateVpnConnection response; however, it's
--       -- present in the DescribeVpnConnections response only if the VPN
--       -- connection is in the pending or available state.
--     , vctType                         :: !Text
--       -- ^ The type of VPN connection.
--     , vctCustomerGatewayId            :: !Text
--       -- ^ The ID of the customer gateway at your end of the VPN connection.
--     , vctVpnGatewayId                 :: !Text
--       -- ^ The ID of the virtual private gateway at the AWS side of the VPN
--       -- connection.
--     , vctTagSet                       :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     , vctVgwTelemetry                 :: !VpnTunnelTelemetryType
--       -- ^ The virtual private gateway. Each gateway is wrapped in an item
--       -- element.
--     , vctOptions                      :: !VpnConnectionOptionsResponseType
--       -- ^ The option set describing the VPN connection.
--     , vctRoutes                       :: !VpnStaticRouteType
--       -- ^ The set of static routes associated with a VPN connection.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery VpnConnectionType

-- instance IsXML VpnConnectionType where
--     xmlPickler = ec2XML

-- data VpnGatewayType = VpnGatewayType
--     { vgtVpnGatewayId     :: !Text
--       -- ^ The ID of the virtual private gateway.
--     , vgtState            :: !Text
--       -- ^ The current state of the virtual private gateway.
--     , vgtType             :: !Text
--       -- ^ The type of VPN connection the virtual private gateway supports.
--     , vgtAvailabilityZone :: !Text
--       -- ^ The Availability Zone where the virtual private gateway was
--       -- created.
--     , vgtAttachments      :: !AttachmentType
--       -- ^ Any VPCs attached to the virtual private gateway, each one
--       -- wrapped in an item element.
--     , vgtTagSet           :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery VpnGatewayType

-- instance IsXML VpnGatewayType where
--     xmlPickler = ec2XML

-- data VpnStaticRouteType = VpnStaticRouteType
--     { vsrtDestinationCidrBlock :: !Text
--       -- ^ The CIDR block associated with the local subnet of the customer
--       -- data center.
--     , vsrtSource               :: !Text
--       -- ^ Indicates how the routes were provided.
--     , vsrtState                :: !Text
--       -- ^ The current state of the static route.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery VpnStaticRouteType

-- instance IsXML VpnStaticRouteType where
--     xmlPickler = ec2XML

-- data VpnTunnelTelemetryType = VpnTunnelTelemetryType
--     { vtttOutsideIpAddress   :: !Text
--       -- ^ The Internet-routable IP address of the virtual private gateway's
--       -- outside interface.
--     , vtttStatus             :: !Text
--       -- ^ The status of the VPN tunnel.
--     , vtttLastStatusChange   :: !UTCTime
--       -- ^ The date and time of the last change in status.
--     , vtttStatusMessage      :: !Text
--       -- ^ If an error occurs, a description of the error.
--     , vtttAcceptedRouteCount :: !Integer
--       -- ^ The number of accepted routes.
--     } deriving (Eq, Ord, Show, Generic)

-- instance IsQuery VpnTunnelTelemetryType

-- instance IsXML VpnTunnelTelemetryType where
--     xmlPickler = ec2XML

data Filter = Filter
    { filterName  :: !Text
    , filterValue :: [Text]
    } deriving (Eq, Ord, Show, Generic)

instance IsQuery Filter

newtype Filters a = Filters { unFilters :: [a] }
    deriving (Eq, Ord, Show, Generic)

instance IsQuery a => IsQuery (Filters a)

data TagResourceType
    = CustomerGateway
    | DhcpOptions
    | Image
    | Instance
    | InternetGateway
    | NetworkAcl
    | NetworkInterface
    | ReservedInstances
    | RouteTable
    | SecurityGroup
    | Snapshot
    | SpotInstancesRequest
    | Subnet
    | Volume
    | Vpc
    | VpnConnection
    | VpnGateway
      deriving (Eq, Ord, Read, Generic)

instance Show TagResourceType where
    show t = case t of
        CustomerGateway      -> "customer-gateway"
        DhcpOptions          -> "dhcp-options"
        Image                -> "image"
        Instance             -> "instance"
        InternetGateway      -> "internet-gateway"
        NetworkAcl           -> "network-acl"
        NetworkInterface     -> "network-interface"
        ReservedInstances    -> "reserved-instances"
        RouteTable           -> "route-table"
        SecurityGroup        -> "security-group"
        Snapshot             -> "snapshot"
        SpotInstancesRequest -> "spot-instances-request"
        Subnet               -> "subnet"
        Volume               -> "volume"
        Vpc                  -> "vpc"
        VpnConnection        -> "vpn-connection"
        VpnGateway           -> "vpn-gateway"

instance IsQuery TagResourceType where
    queryPickler = qpPrim

    -- = customer-gateway
    -- | dhcp-options
    -- | image
    -- | instance
    -- | internet-gateway
    -- | network-acl
    -- | network-interface
    -- | reserved-instances
    -- | route-table
    -- | security-group
    -- | snapshot
    -- | spot-instances-request
    -- | subnet
    -- | volume
    -- | vpc
    -- | vpn-connection
    -- | vpn-gateway

data TagFilter
    = TagKey [Text]
      -- ^ The tag key.
    | TagResourceId [Text]
      -- ^ The resource ID.
    | TagResourceType [TagResourceType]
      -- ^ The resource type.
    | TagValue [Text]
      -- ^ The tag value.
      deriving (Eq, Ord, Show, Generic)

instance IsQuery TagFilter where
    queryPickler = QueryPU p u
      where
        p (TagKey ks)          = List $ Pair "Name" (Value "key") : map enc ks
        p (TagResourceId is)   = List $ Pair "Name" (Value "resource-id") : map enc is
        p (TagResourceType ts) = List $ Pair "Name" (Value "resource-type") : map enc' ts
        p (TagValue vs)        = List $ Pair "Name" (Value "value") : map enc vs

        u = undefined

        enc  = Pair "Value" . Value . encodeUtf8
        enc' = Pair "Value" . Value . BS.pack . show

-- | XML namespace to annotate EC2 elements with.
ec2NS :: ByteString
ec2NS = "http://ec2.amazonaws.com/doc/" <> svcVersion ec2 <> "/"

-- | Helper to define EC2 namespaced XML elements.
ec2Elem :: ByteString -> NName ByteString
ec2Elem = mkNName ec2NS

ec2XML :: XMLGeneric a
ec2XML = withNS' ec2NS $ (namespacedXMLOptions ec2NS)
    { xmlFieldModifier = mkNName ec2NS . lowerHead . stripLower
    , xmlListElement   = mkNName ec2NS "item"
    }

ec2ItemXML :: XMLGeneric a
ec2ItemXML = withRootNS' ec2NS "item" $ (namespacedXMLOptions ec2NS)
    { xmlFieldModifier = mkNName ec2NS . lowerHead . stripLower
    , xmlListElement   = mkNName ec2NS "item"
    }
