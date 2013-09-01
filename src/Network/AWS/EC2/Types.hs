{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
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

import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import           Data.Time
import           Network.AWS.Internal
import           Text.Read

-- | Currently supported version (2013-07-15) of the EC2 service.
ec2Version :: ByteString
ec2Version = "2013-07-15"

-- | XML namespace to annotate EC2 elements with.
ec2NS :: ByteString
ec2NS = "http://ec2.amazonaws.com/doc/" <> ec2Version <> "/"

-- | Helper to define EC2 namespaced XML elements.
ec2Elem :: ByteString -> NName ByteString
ec2Elem = mkNName ec2NS

ec2XML :: XMLGeneric a
ec2XML = withNS' ec2NS $ (xmlOptions ec2NS)
    { xmlFieldModifier = mkNName ec2NS . BS.pack . lowerFirst . dropLower
    , xmlListElement   = mkNName ec2NS "item"
    }

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
--     { aasitAttributeName     :: !ByteString
--       -- ^ The name of the attribute.
--     , aasitAttributeValueSet :: !AccountAttributeValueSetItemType
--       -- ^ A list of the attribute values, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery AccountAttributeSetItemType

-- instance IsXML AccountAttributeSetItemType where
--     xmlPickler = ec2XML

-- data AccountAttributeValueSetItemType = AccountAttributeValueSetItemType
--     { aavsitAttributeValue :: !ByteString
--       -- ^ The value of the attribute.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery AccountAttributeValueSetItemType

-- instance IsXML AccountAttributeValueSetItemType where
--     xmlPickler = ec2XML

-- data PrivateIpAddress = PrivateIpAddress
--     { piaPrivateIpAddress :: !ByteString
--       -- ^ The private IP address.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery AssignPrivateIpAddressesSetItemRequestType

-- instance IsXML AssignPrivateIpAddressesSetItemRequestType where
--     xmlPickler = ec2XML

-- data AttachmentSetItemResponseType = AttachmentSetItemResponseType
--     { asirtVolumeId            :: !ByteString
--       -- ^ The ID of the volume.
--     , asirtInstanceId          :: !ByteString
--       -- ^ The ID of the instance.
--     , asirtDevice              :: !ByteString
--       -- ^ The device name exposed to the instance (for example, /dev/sdh).
--     , asirtStatus              :: !ByteString
--       -- ^ The attachment state.
--     , asirtAttachTime          :: !UTCTime
--       -- ^ The time stamp when the attachment initiated.
--     , asirtDeleteOnTermination :: !Bool
--       -- ^ Indicates whether the volume is deleted on instance termination.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery AttachmentSetItemResponseType

-- instance IsXML AttachmentSetItemResponseType where
--     xmlPickler = ec2XML

data Attachment = Attachment
    { atVpcId :: !ByteString
      -- ^ The ID of the VPC.
    , atState :: !ByteString
      -- ^ The current state of the attachment.
    } deriving (Eq, Show, Generic)

instance IsXML Attachment where
    xmlPickler = ec2XML

-- data AvailabilityZoneItemType = AvailabilityZoneItemType
--     { azitZoneName   :: !ByteString
--       -- ^ The name of the Availability Zone.
--     , azitZoneState  :: !ByteString
--       -- ^ The state of the Availability Zone.
--     , azitRegionName :: !ByteString
--       -- ^ The name of the region.
--     , azitMessageSet :: !AvailabilityZoneMessageType
--       -- ^ Any messages about the Availability Zone, each one wrapped in an
--       -- item element.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery AvailabilityZoneItemType

-- instance IsXML AvailabilityZoneItemType where
--     xmlPickler = ec2XML

-- data AvailabilityZoneMessageType = AvailabilityZoneMessageType
--     { azmtMessage :: !ByteString
--       -- ^ The message about the Availability Zone.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery AvailabilityZoneMessageType

-- instance IsXML AvailabilityZoneMessageType where
--     xmlPickler = ec2XML

-- data BlockDeviceMappingItemType = BlockDeviceMappingItemType
--     { bdmitDeviceName  :: !ByteString
--       -- ^ The device name exposed to the instance (for example, /dev/sdh).
--     , bdmitVirtualName :: !ByteString
--       -- ^ The virtual device name.
--     , bdmitEbs         :: !EbsBlockDeviceType
--       -- ^ Parameters used to automatically set up Amazon EBS volumes when
--       -- the instance is launched.
--     , bdmitNoDevice    :: !ByteString
--       -- ^ Include this empty element to suppress the specified device
--       -- included in the block device mapping of the AMI.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery BlockDeviceMappingItemType

-- instance IsXML BlockDeviceMappingItemType where
--     xmlPickler = ec2XML

data BundleInstanceS3Storage = BundleInstanceS3Storage
    { bissAwsAccessKeyId        :: !ByteString
      -- ^ The access key ID of the owner of the bucket.
    , bissBucket                :: !ByteString
      -- ^ The bucket in which to store the AMI. You can specify a bucket
      -- that you already own or a new bucket that Amazon EC2 creates on
      -- your behalf. If you specify a bucket that belongs to someone
      -- else, Amazon EC2 returns an error.
    , bissPrefix                :: !ByteString
      -- ^ The beginning of the file name of the AMI.
    , bissUploadPolicy          :: !ByteString
      -- ^ A Base64-encoded Amazon S3 upload policy that gives Amazon EC2
      -- permission to upload items into Amazon S3 on the user's behalf.
    , bissUploadPolicySignature :: !ByteString
      -- ^ The signature of the Base64 encoded JSON document.
    } deriving (Eq, Show, Generic)

instance IsQuery BundleInstanceS3Storage

instance IsXML BundleInstanceS3Storage where
    xmlPickler = ec2XML

data BundleInstanceTaskStorage = BundleInstanceTaskStorage
    { bitsS3 :: !BundleInstanceS3Storage
      -- ^ An Amazon S3 storage location.
    } deriving (Eq, Show, Generic)

instance IsQuery BundleInstanceTaskStorage

instance IsXML BundleInstanceTaskStorage where
    xmlPickler = ec2XML

data BundleInstanceTaskError = BundleInstanceTaskError
    { biteCode    :: !ByteString
      -- ^ The error code.
    , biteMessage :: !ByteString
      -- ^ The error message.
    } deriving (Eq, Show, Generic)

instance IsXML BundleInstanceTaskError where
    xmlPickler = ec2XML

data BundleInstanceTask = BundleInstanceTask
    { bitInstanceId :: !ByteString
      -- ^ The ID of the instance associated with this bundle task.
    , bitBundleId   :: !ByteString
      -- ^ The ID for this bundle task.
    , bitState      :: !ByteString
      -- ^ The state of the task.
    , bitStartTime  :: !UTCTime
      -- ^ The time this task started.
    , bitUpdateTime :: !UTCTime
      -- ^ The time of the most recent update for the task.
    , bitStorage    :: !BundleInstanceTaskStorage
      -- ^ The Amazon S3 storage locations.
    , bitProgress   :: !ByteString
      -- ^ The level of task completion, as a percent (for example, 20%).
    , bitError      :: !BundleInstanceTaskError
      -- ^ If the task fails, a description of the error.
    } deriving (Eq, Show, Generic)

instance IsXML BundleInstanceTask where
    xmlPickler = ec2XML

-- data CancelSpotInstanceRequestsResponseSetItemType = CancelSpotInstanceRequestsResponseSetItemType
--     { csirrsitSpotInstanceRequestId :: !ByteString
--       -- ^ The ID of the Spot Instance request.
--     , csirrsitState                 :: !ByteString
--       -- ^ The state of the Spot Instance request.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CancelSpotInstanceRequestsResponseSetItemType

-- instance IsXML CancelSpotInstanceRequestsResponseSetItemType where
--     xmlPickler = ec2XML

-- data ConversionTaskType = ConversionTaskType
--     { cttConversionTaskId :: !ByteString
--       -- ^ The ID of the conversion task
--     , cttExpirationTime   :: !ByteString
--       -- ^ The time when the task expires. If the upload isn't complete
--       -- before the expiration time, we automatically cancel the task.
--     , cttImportVolume     :: !ImportVolumeTaskDetailsType
--       -- ^ If the task is for importing a volume, this contains information
--       -- about the import volume task.
--     , cttImportInstance   :: !ImportInstanceTaskDetailsType
--       -- ^ If the task is for importing an instance, this contains
--       -- information about the import instance task.
--     , cttState            :: !ByteString
--       -- ^ The state of the conversion task.
--     , cttStatusMessage    :: !ByteString
--       -- ^ The status message related to the conversion task.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ConversionTaskType

-- instance IsXML ConversionTaskType where
--     xmlPickler = ec2XML

-- data CreateVolumePermissionItemType = CreateVolumePermissionItemType
--     { cvpitUserId :: !ByteString
--       -- ^ The ID of an AWS account that can create volumes from the
--       -- snapshot.
--     , cvpitGroup  :: !ByteString
--       -- ^ The group that is allowed to create volumes from the snapshot.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateVolumePermissionItemType

-- instance IsXML CreateVolumePermissionItemType where
--     xmlPickler = ec2XML

-- data CustomerGatewayType = CustomerGatewayType
--     { cgtCustomerGatewayId :: !ByteString
--       -- ^ The ID of the customer gateway.
--     , cgtState             :: !ByteString
--       -- ^ The current state of the customer gateway.
--     , cgtType              :: !ByteString
--       -- ^ The type of VPN connection the customer gateway supports.
--     , cgtIpAddress         :: !ByteString
--       -- ^ The Internet-routable IP address of the customer gateway's
--       -- outside interface.
--     , cgtBgpAsn            :: !Integer
--       -- ^ The customer gateway's Border Gateway Protocol (BGP) Autonomous
--       -- System Number (ASN).
--     , cgtTagSet            :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CustomerGatewayType

-- instance IsXML CustomerGatewayType where
--     xmlPickler = ec2XML

-- data DescribeAddressesResponseItemType = DescribeAddressesResponseItemType
--     { daritPublicIp                :: !ByteString
--       -- ^ The public IP address.
--     , daritAllocationId            :: !ByteString
--       -- ^ The ID representing the allocation of the address for use with
--       -- EC2-VPC.
--     , daritDomain                  :: !ByteString
--       -- ^ Indicates whether this Elastic IP address is for instances in
--       -- EC2-Classic or EC2-VPC.
--     , daritInstanceId              :: !ByteString
--       -- ^ The ID of the instance the address is associated with (if any).
--     , daritAssociationId           :: !ByteString
--       -- ^ The ID representing the association of an Elastic IP address with
--       -- an instance in a VPC.
--     , daritNetworkInterfaceId      :: !ByteString
--       -- ^ The ID of the network interface.
--     , daritNetworkInterfaceOwnerId :: !ByteString
--       -- ^ The ID of the AWS account that owns the network interface.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeAddressesResponseItemType

-- instance IsXML DescribeAddressesResponseItemType where
--     xmlPickler = ec2XML

-- data DescribeImagesResponseItemType = DescribeImagesResponseItemType
--     { diritImageId            :: !ByteString
--       -- ^ The ID of the AMI.
--     , diritImageLocation      :: !ByteString
--       -- ^ The location of the AMI.
--     , diritImageState         :: !ByteString
--       -- ^ Current state of the AMI. If the operation returns available, the
--       -- image is successfully registered and available for launching.
--     , diritImageOwnerId       :: !ByteString
--       -- ^ AWS account ID of the image owner.
--     , diritIsPublic           :: !Bool
--       -- ^ Indicates whether the image has public launch permissions. The
--       -- value is true if this image has public launch permissions or
--       -- false if it has only implicit and explicit launch permissions.
--     , diritProductCodes       :: !ProductCodesSetItemType
--       -- ^ Any product codes associated with the AMI, each one wrapped in an
--       -- item element.
--     , diritArchitecture       :: !ByteString
--       -- ^ The architecture of the image.
--     , diritImageType          :: !ByteString
--       -- ^ The type of image.
--     , diritKernelId           :: !ByteString
--       -- ^ The kernel associated with the image, if any. Only applicable for
--       -- machine images.
--     , diritRamdiskId          :: !ByteString
--       -- ^ The RAM disk associated with the image, if any. Only applicable
--       -- for machine images.
--     , diritPlatform           :: !ByteString
--       -- ^ The value is Windows for Windows AMIs; otherwise blank.
--     , diritStateReason        :: !StateReasonType
--       -- ^ The reason for the state change.
--     , diritImageOwnerAlias    :: !ByteString
--       -- ^ The AWS account alias (for example, amazon, self, etc.) or AWS
--       -- account ID that owns the AMI.
--     , diritName               :: !ByteString
--       -- ^ The name of the AMI that was provided during image creation.
--     , diritDescription        :: !ByteString
--       -- ^ The description of the AMI that was provided during image
--       -- creation.
--     , diritRootDeviceType     :: !ByteString
--       -- ^ The type of root device used by the AMI. The AMI can use an
--       -- Amazon EBS volume or an instance store volume.
--     , diritRootDeviceName     :: !ByteString
--       -- ^ The device name of the root device (for example, /dev/sda1 or
--       -- xvda).
--     , diritBlockDeviceMapping :: !BlockDeviceMappingItemType
--       -- ^ Any block device mapping entries, each one wrapped in an item
--       -- element.
--     , diritVirtualizationType :: !ByteString
--       -- ^ The type of virtualization of the AMI.
--     , diritTagSet             :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     , diritHypervisor         :: !ByteString
--       -- ^ The image's hypervisor type.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeImagesResponseItemType

-- instance IsXML DescribeImagesResponseItemType where
--     xmlPickler = ec2XML

-- data DescribeKeyPairsResponseItemType = DescribeKeyPairsResponseItemType
--     { dkpritKeyName        :: !ByteString
--       -- ^ The name of the key pair.
--     , dkpritKeyFingerprint :: !ByteString
--       -- ^ If you used CreateKeyPair to create the key pair, this is the
--       -- SHA-1 digest of the DER encoded private key. If you used
--       -- ImportKeyPair to provide AWS the public key, this is the MD5
--       -- public key fingerprint as specified in section 4 of RFC4716.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeKeyPairsResponseItemType

-- instance IsXML DescribeKeyPairsResponseItemType where
--     xmlPickler = ec2XML

-- data DescribeReservedInstancesListingsResponseSetItemType = DescribeReservedInstancesListingsResponseSetItemType
--     { drilrsitReservedInstancesListingId :: !ByteString
--       -- ^ The ID of the Reserved Instance listing.
--     , drilrsitReservedInstancesId        :: !ByteString
--       -- ^ The ID of the Reserved Instance.
--     , drilrsitCreateDate                 :: !UTCTime
--       -- ^ The time the listing was created.
--     , drilrsitUpdateDate                 :: !UTCTime
--       -- ^ The last modified timestamp of the listing.
--     , drilrsitStatus                     :: !ByteString
--       -- ^ The status of the Reserved Instance listing.
--     , drilrsitStatusMessage              :: !ByteString
--       -- ^ The reason for the current status of the Reserved Instance
--       -- listing. The response can be blank.
--     , drilrsitInstanceCounts             :: !InstanceCountsSetType
--       -- ^ The number of instances in this state.
--     , drilrsitPriceSchedules             :: !PriceScheduleSetType
--       -- ^ The price of the Reserved Instance listing.
--     , drilrsitTagSet                     :: !ResourceTagSetItemType
--       -- ^ The tags assigned to the resource. Each tag's information is
--       -- wrapped in an item element.
--     , drilrsitClientToken                :: !ByteString
--       -- ^ The idempotency token you provided when you created the listing.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeReservedInstancesListingsResponseSetItemType

-- instance IsXML DescribeReservedInstancesListingsResponseSetItemType where
--     xmlPickler = ec2XML

-- data DescribeReservedInstancesListingSetItemType = DescribeReservedInstancesListingSetItemType
--     { drilsitReservedInstancesListingId :: !ByteString
--       -- ^ The ID of the Reserved Instance listing.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeReservedInstancesListingSetItemType

-- instance IsXML DescribeReservedInstancesListingSetItemType where
--     xmlPickler = ec2XML

-- data DescribeReservedInstancesOfferingsResponseSetItemType = DescribeReservedInstancesOfferingsResponseSetItemType
--     { driorsitReservedInstancesOfferingId :: !ByteString
--       -- ^ The ID of the Reserved Instance offering.
--     , driorsitInstanceType                :: !ByteString
--       -- ^ The instance type on which the Reserved Instance can be used.
--     , driorsitAvailabilityZone            :: !ByteString
--       -- ^ The Availability Zone in which the Reserved Instance can be used.
--     , driorsitDuration                    :: !Integer
--       -- ^ The duration of the Reserved Instance, in seconds.
--     , driorsitFixedPrice                  :: !Double
--       -- ^ The purchase price of the Reserved Instance.
--     , driorsitUsagePrice                  :: !Double
--       -- ^ The usage price of the Reserved Instance, per hour.
--     , driorsitProductDescription          :: !ByteString
--       -- ^ The Reserved Instance description.
--     , driorsitInstanceTenancy             :: !ByteString
--       -- ^ The tenancy of the reserved instance.
--     , driorsitCurrencyCode                :: !ByteString
--       -- ^ The currency of the Reserved Instance offering you are
--       -- purchasing. It's specified using ISO 4217 standard currency
--       -- codes. At this time, the only supported currency is USD.
--     , driorsitOfferingType                :: !ByteString
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
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeReservedInstancesOfferingsResponseSetItemType

-- instance IsXML DescribeReservedInstancesOfferingsResponseSetItemType where
--     xmlPickler = ec2XML

-- data DescribeReservedInstancesOfferingsResponseType = DescribeReservedInstancesOfferingsResponseType
--     { driortRequestId                     :: !ByteString
--       -- ^ The ID of the Reserved Instance offering request.
--     , driortReservedInstancesOfferingsSet :: !DescribeReservedInstancesOfferingsResponseSetItemType
--       -- ^ The instance type on which the Reserved Instance can be used.
--     , driortNextToken                     :: !ByteString
--       -- ^ The next paginated set of results to return.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeReservedInstancesOfferingsResponseType

-- instance IsXML DescribeReservedInstancesOfferingsResponseType where
--     xmlPickler = ec2XML

-- data DescribeReservedInstancesResponseSetItemType = DescribeReservedInstancesResponseSetItemType
--     { drirsitReservedInstancesId :: !ByteString
--       -- ^ The ID of the Reserved Instance.
--     , drirsitInstanceType        :: !ByteString
--       -- ^ The instance type on which the Reserved Instance can be used.
--     , drirsitAvailabilityZone    :: !ByteString
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
--     , drirsitProductDescription  :: !ByteString
--       -- ^ The Reserved Instance description.
--     , drirsitState               :: !ByteString
--       -- ^ The state of the Reserved Instance purchase.
--     , drirsitTagSet              :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     , drirsitInstanceTenancy     :: !ByteString
--       -- ^ The tenancy of the reserved instance.
--     , drirsitCurrencyCode        :: !ByteString
--       -- ^ The currency of the Reserved Instance. It's specified using ISO
--       -- 4217 standard currency codes. At this time, the only supported
--       -- currency is USD.
--     , drirsitOfferingType        :: !ByteString
--       -- ^ The Reserved Instance offering type.
--     , drirsitRecurringCharges    :: !RecurringChargesSetItemType
--       -- ^ The recurring charge tag assigned to the resource.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeReservedInstancesResponseSetItemType

-- instance IsXML DescribeReservedInstancesResponseSetItemType where
--     xmlPickler = ec2XML

-- data DescribeReservedInstancesSetItemType = DescribeReservedInstancesSetItemType
--     { drisitReservedInstancesId :: !ByteString
--       -- ^ The ID of the Reserved Instance.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeReservedInstancesSetItemType

-- instance IsXML DescribeReservedInstancesSetItemType where
--     xmlPickler = ec2XML

-- data DescribeSnapshotsSetItemResponseType = DescribeSnapshotsSetItemResponseType
--     { dssirtSnapshotId  :: !ByteString
--       -- ^ The ID of the snapshot.
--     , dssirtVolumeId    :: !ByteString
--       -- ^ The ID of the volume.
--     , dssirtStatus      :: !ByteString
--       -- ^ The snapshot state.
--     , dssirtStartTime   :: !UTCTime
--       -- ^ The time stamp when the snapshot was initiated.
--     , dssirtProgress    :: !ByteString
--       -- ^ The progress of the snapshot, as a percentage.
--     , dssirtOwnerId     :: !ByteString
--       -- ^ The ID of the AWS account that owns the snapshot.
--     , dssirtVolumeSize  :: !ByteString
--       -- ^ The size of the volume, in GiB.
--     , dssirtDescription :: !ByteString
--       -- ^ The description of the snapshot.
--     , dssirtOwnerAlias  :: !ByteString
--       -- ^ The AWS account alias (for example, amazon, self) or AWS account
--       -- ID that owns the AMI.
--     , dssirtTagSet      :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeSnapshotsSetItemResponseType

-- instance IsXML DescribeSnapshotsSetItemResponseType where
--     xmlPickler = ec2XML

-- data DescribeVolumesSetItemResponseType = DescribeVolumesSetItemResponseType
--     { dvsirtVolumeId         :: !ByteString
--       -- ^ The ID of the volume.
--     , dvsirtSize             :: !ByteString
--       -- ^ The size of the volume, in GiBs.
--     , dvsirtSnapshotId       :: !ByteString
--       -- ^ The snapshot from which the volume was created (optional).
--     , dvsirtAvailabilityZone :: !ByteString
--       -- ^ The Availability Zone in which the volume was created.
--     , dvsirtStatus           :: !ByteString
--       -- ^ The state of the volume.
--     , dvsirtCreateTime       :: !UTCTime
--       -- ^ The time stamp when volume creation was initiated.
--     , dvsirtAttachmentSet    :: !AttachmentSetItemResponseType
--       -- ^ Any volumes attached, each one wrapped in an item element.
--     , dvsirtTagSet           :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     , dvsirtVolumeType       :: !ByteString
--       -- ^ The volume type.
--     , dvsirtIops             :: !Integer
--       -- ^ The number of I/O operations per second (IOPS) that the volume
--       -- supports.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeVolumesSetItemResponseType

-- instance IsXML DescribeVolumesSetItemResponseType where
--     xmlPickler = ec2XML

-- data DhcpConfigurationItemType = DhcpConfigurationItemType
--     { dcitKey      :: !ByteString
--       -- ^ The name of a DHCP option.
--     , dcitValueSet :: !DhcpValueType
--       -- ^ Any values for a DHCP option, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DhcpConfigurationItemType

-- instance IsXML DhcpConfigurationItemType where
--     xmlPickler = ec2XML

-- data DhcpOptionsType = DhcpOptionsType
--     { dotDhcpOptionsId        :: !ByteString
--       -- ^ The ID of the set of DHCP options.
--     , dotDhcpConfigurationSet :: !DhcpConfigurationItemType
--       -- ^ The DHCP options in the set. Each option's key and set of values
--       -- are wrapped in an item element.
--     , dotTagSet               :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DhcpOptionsType

-- instance IsXML DhcpOptionsType where
--     xmlPickler = ec2XML

-- data DhcpValueType = DhcpValueType
--     { dvtValue :: !ByteString
--       -- ^ A value for the DHCP option.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DhcpValueType

-- instance IsXML DhcpValueType where
--     xmlPickler = ec2XML

-- data DiskImageDescriptionType = DiskImageDescriptionType
--     { didtFormat            :: !ByteString
--       -- ^ The disk image format.
--     , didtSize              :: !Integer
--       -- ^ The size of the disk image.
--     , didtImportManifestUrl :: !ByteString
--       -- ^ A presigned URL for the import manifest stored in Amazon S3. For
--       -- information about creating a presigned URL for an Amazon S3
--       -- object, read the "Query String Request Authentication
--       -- Alternative" section of the Authenticating REST Requests topic in
--       -- the Amazon Simple Storage Service Developer Guide.
--     , didtChecksum          :: !ByteString
--       -- ^ The checksum computed for the disk image.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DiskImageDescriptionType

-- instance IsXML DiskImageDescriptionType where
--     xmlPickler = ec2XML

-- data DiskImageVolumeDescriptionType = DiskImageVolumeDescriptionType
--     { divdtSize :: !Integer
--       -- ^ The size of the volume.
--     , divdtId   :: !ByteString
--       -- ^ The volume identifier.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DiskImageVolumeDescriptionType

-- instance IsXML DiskImageVolumeDescriptionType where
--     xmlPickler = ec2XML

-- data EbsBlockDeviceType = EbsBlockDeviceType
--     { ebdtSnapshotId          :: !ByteString
--       -- ^ The ID of the snapshot.
--     , ebdtVolumeSize          :: !Integer
--       -- ^ The size of the volume, in GiB.
--     , ebdtDeleteOnTermination :: !Bool
--       -- ^ Indicates whether the Amazon EBS volume is deleted on instance
--       -- termination.
--     , ebdtVolumeType          :: !ByteString
--       -- ^ The volume type.
--     , ebdtIops                :: !Integer
--       -- ^ The number of I/O operations per second (IOPS) that the volume
--       -- supports.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery EbsBlockDeviceType

-- instance IsXML EbsBlockDeviceType where
--     xmlPickler = ec2XML

-- data EbsInstanceBlockDeviceMappingResponseType = EbsInstanceBlockDeviceMappingResponseType
--     { eibdmrtVolumeId            :: !ByteString
--       -- ^ The ID of the Amazon EBS volume.
--     , eibdmrtStatus              :: !ByteString
--       -- ^ The attachment state.
--     , eibdmrtAttachTime          :: !UTCTime
--       -- ^ The time stamp when the attachment initiated.
--     , eibdmrtDeleteOnTermination :: !Bool
--       -- ^ Indicates whether the volume is deleted on instance termination.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery EbsInstanceBlockDeviceMappingResponseType

-- instance IsXML EbsInstanceBlockDeviceMappingResponseType where
--     xmlPickler = ec2XML

-- data ExportToS3Task = ExportToS3Task
--     { etstDiskImageFormat :: Maybe ByteString
--     , etstContainerFormat :: Maybe ByteString
--     , etstS3Bucket        :: !ByteString
--     , etstS3Prefix        :: !ByteString
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ExportToS3Task

-- instance IsXML ExportToS3Task where
--     xmlPickler = ec2XML

-- data ExportTaskResponseType = ExportTaskResponseType
--     { etrtExportTaskId   :: !ByteString
--       -- ^ The ID of the export task.
--     , etrtDescription    :: !ByteString
--       -- ^ A description of the resource being exported.
--     , etrtState          :: !ByteString
--       -- ^ The state of the conversion task.
--     , etrtStatusMessage  :: !ByteString
--       -- ^ The status message related to the export task.
--     , etrtInstanceExport :: !InstanceExportTaskResponseType
--       -- ^ The instance being exported.
--     , etrtExportToS3     :: !ExportToS3TaskResponseType
--       -- ^ The destination Amazon S3 bucket.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ExportTaskResponseType

-- instance IsXML ExportTaskResponseType where
--     xmlPickler = ec2XML

-- data ExportToS3TaskResponseType = ExportToS3TaskResponseType
--     { etstrtDiskImageFormat :: !ByteString
--       -- ^ The format for the exported image.
--     , etstrtContainerFormat :: !ByteString
--       -- ^ The container format used to combine disk images with metadata
--       -- (such as OVF).
--     , etstrtS3Bucket        :: !ByteString
--       -- ^ The Amazon S3 bucket for the destination image.
--     , etstrtS3Key           :: !ByteString
--       -- ^ The image written to a single object in s3bucket at the S3 key
--       -- s3prefix + exportTaskId + '.' +diskImageFormat.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ExportToS3TaskResponseType

-- instance IsXML ExportToS3TaskResponseType where
--     xmlPickler = ec2XML

-- data GroupItemType = GroupItemType
--     { gitGroupId   :: !ByteString
--       -- ^ The ID of the security group.
--     , gitGroupName :: !ByteString
--       -- ^ The name of the security group.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery GroupItemType

-- instance IsXML GroupItemType where
--     xmlPickler = ec2XML

-- data IamInstanceProfileRequestType = IamInstanceProfileRequestType
--     { iiprtArn  :: !ByteString
--       -- ^ The Amazon Resource Name (ARN) of the instance profile.
--     , iiprtName :: !ByteString
--       -- ^ The name of the instance profile.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery IamInstanceProfileRequestType

-- instance IsXML IamInstanceProfileRequestType where
--     xmlPickler = ec2XML

-- data IamInstanceProfileResponseType = IamInstanceProfileResponseType
--     { iipruArn :: !ByteString
--       -- ^ The Amazon Resource Name (ARN) of the instance profile.
--     , iipruId  :: !ByteString
--       -- ^ The ID of the instance profile.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery IamInstanceProfileResponseType

-- instance IsXML IamInstanceProfileResponseType where
--     xmlPickler = ec2XML

-- data IcmpType = IcmpType
--     { itctCode :: !Integer
--       -- ^ The ICMP code. A value of -1 means all codes for the specified
--       -- ICMP type.
--     , itctType :: !Integer
--       -- ^ The ICMP type. A value of -1 means all types.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery IcmpTypeCodeType

-- instance IsXML IcmpTypeCodeType where
--     xmlPickler = ec2XML

-- data InstancePlacement = InstancePlacement
--     { ipAvailabilityZone :: !Maybe ByteString
--     , ipGroupName        :: !Maybe ByteString
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstancePlacement

-- instance IsXML InstancePlacement where
--     xmlPickler = ec2XML

-- data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification
--     { iilsArchitecture                      :: !ByteString
--     , iilsGroupSet                          :: Members GroupItemType
--     , iilsUserData                          :: Maybe UserDataType
--     , iilsInstance                          :: !ByteString
--     , iilsPlacement                         :: Maybe InstancePlacementType
--     , iilsMonitoring                        :: Maybe MonitoringInstanceType
--     , iilsSubnetId                          :: Maybe ByteString
--     , iilsInstanceInitiatedShutdownBehavior :: Maybe ByteString
--     , iilsPrivateIpAddress                  :: Maybe ByteString
--     } deriving (Show)

-- data ImportInstanceTaskDetailsType = ImportInstanceTaskDetailsType
--     { iitdtVolumes     :: !ImportInstanceVolumeDetailItemType
--       -- ^ Any instance volumes for import, each one wrapped in an item
--       -- element.
--     , iitdtInstanceId  :: !ByteString
--       -- ^ The ID of the instance.
--     , iitdtPlatform    :: !ByteString
--       -- ^ The value is Windows for Windows AMIs; otherwise blank.
--     , iitdtDescription :: !ByteString
--       -- ^ An optional description of the instance.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ImportInstanceTaskDetailsType

-- instance IsXML ImportInstanceTaskDetailsType where
--     xmlPickler = ec2XML

-- data ImportInstanceVolumeDetailItemType = ImportInstanceVolumeDetailItemType
--     { iivditBytesConverted   :: !Integer
--       -- ^ The number of bytes converted so far.
--     , iivditAvailabilityZone :: !ByteString
--       -- ^ The Availability Zone where the resulting instance will reside.
--     , iivditImage            :: !DiskImageDescriptionType
--       -- ^ The image.
--     , iivditDescription      :: !ByteString
--       -- ^ The description you provided when starting the import instance
--       -- task.
--     , iivditVolume           :: !DiskImageVolumeDescriptionType
--       -- ^ The volume.
--     , iivditStatus           :: !ByteString
--       -- ^ The status of the import of this particular disk image.
--     , iivditStatusMessage    :: !ByteString
--       -- ^ The status information or errors related to the disk image.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ImportInstanceVolumeDetailItemType

-- instance IsXML ImportInstanceVolumeDetailItemType where
--     xmlPickler = ec2XML

-- data ImportVolumeTaskDetailsType = ImportVolumeTaskDetailsType
--     { ivtdtBytesConverted   :: !Integer
--       -- ^ The number of bytes converted so far.
--     , ivtdtAvailabilityZone :: !ByteString
--       -- ^ The Availability Zone where the resulting volume will reside.
--     , ivtdtDescription      :: !ByteString
--       -- ^ The description you provided when starting the import volume
--       -- task.
--     , ivtdtImage            :: !DiskImageDescriptionType
--       -- ^ The image.
--     , ivtdtVolume           :: !DiskImageVolumeDescriptionType
--       -- ^ The volume.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ImportVolumeTaskDetailsType

-- instance IsXML ImportVolumeTaskDetailsType where
--     xmlPickler = ec2XML

-- data InstanceBlockDeviceMappingItemType = InstanceBlockDeviceMappingItemType
--     { ibdmitDeviceName  :: !ByteString
--       -- ^ The device name exposed to the instance (for example, /dev/sdh or
--       -- xvdh).
--     , ibdmitVirtualName :: !ByteString
--       -- ^ The virtual device name.
--     , ibdmitEbs         :: !InstanceEbsBlockDeviceType
--       -- ^ Parameters used to automatically set up Amazon EBS volumes when
--       -- the instance is launched.
--     , ibdmitNoDevice    :: !ByteString
--       -- ^ Include this empty element to suppress the specified device
--       -- included in the block device mapping of the AMI.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceBlockDeviceMappingItemType

-- instance IsXML InstanceBlockDeviceMappingItemType where
--     xmlPickler = ec2XML

-- data InstanceBlockDeviceMappingResponseItemType = InstanceBlockDeviceMappingResponseItemType
--     { ibdmritDeviceName :: !ByteString
--       -- ^ The device name exposed to the instance (for example, /dev/sdh,
--       -- or xvdh).
--     , ibdmritEbs        :: !EbsInstanceBlockDeviceMappingResponseType
--       -- ^ Parameters used to automatically set up Amazon EBS volumes when
--       -- the instance is launched.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceBlockDeviceMappingResponseItemType

-- instance IsXML InstanceBlockDeviceMappingResponseItemType where
--     xmlPickler = ec2XML

-- data InstanceCountsSetItemType = InstanceCountsSetItemType
--     { icsitState         :: !ByteString
--       -- ^ The states of the listed Reserved Instances.
--     , icsitInstanceCount :: !Integer
--       -- ^ The number of listed Reserved Instances in the state specified by
--       -- the state.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceCountsSetItemType

-- instance IsXML InstanceCountsSetItemType where
--     xmlPickler = ec2XML

-- data InstanceCountsSetType = InstanceCountsSetType
--     { icstItem :: !InstanceCountsSetItemType
--       -- ^ The Reserved Instance listing item.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceCountsSetType

-- instance IsXML InstanceCountsSetType where
--     xmlPickler = ec2XML

-- data InstanceEbsBlockDeviceType = InstanceEbsBlockDeviceType
--     { iebdtDeleteOnTermination :: !Bool
--       -- ^ Indicates whether the volume is deleted on instance termination.
--     , iebdtVolumeId            :: !ByteString
--       -- ^ The ID of the volume.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceEbsBlockDeviceType

-- instance IsXML InstanceEbsBlockDeviceType where
--     xmlPickler = ec2XML

-- data InstanceExportTaskResponseType = InstanceExportTaskResponseType
--     { ietrtInstanceId        :: !ByteString
--       -- ^ The ID of the resource being exported.
--     , ietrtTargetEnvironment :: !ByteString
--       -- ^ The target virtualization environment.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceExportTaskResponseType

-- instance IsXML InstanceExportTaskResponseType where
--     xmlPickler = ec2XML

-- data InstanceMonitoringStateType = InstanceMonitoringStateType
--     { imstState :: !ByteString
--       -- ^ The state of monitoring for the instance. The disabled state
--       -- means that Detailed Monitoring is disabled for the instance. The
--       -- enabled state means that Detailed Monitoring is enabled for the
--       -- instance. The pending state means that the instance is launching
--       -- or that you recently enabled Detailed Monitoring for the
--       -- instance.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceMonitoringStateType

-- instance IsXML InstanceMonitoringStateType where
--     xmlPickler = ec2XML

-- data InstanceNetworkInterfaceAssociationType = InstanceNetworkInterfaceAssociationType
--     { iniatPublicIp      :: !ByteString
--       -- ^ The address of the Elastic IP address bound to the network
--       -- interface.
--     , iniatPublicDnsName :: !ByteString
--       -- ^ The public DNS name.
--     , iniatIpOwnerId     :: !ByteString
--       -- ^ The ID of the owner of the Elastic IP address.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceNetworkInterfaceAssociationType

-- instance IsXML InstanceNetworkInterfaceAssociationType where
--     xmlPickler = ec2XML

-- data InstanceNetworkInterfaceAttachmentType = InstanceNetworkInterfaceAttachmentType
--     { iniatAttachmentID        :: !ByteString
--       -- ^ The ID of the network interface attachment.
--     , iniatDeviceIndex         :: !Integer
--       -- ^ The index of the device on the instance for the network interface
--       -- attachment.
--     , iniatStatus              :: !ByteString
--       -- ^ The attachment state.
--     , iniatAttachTime          :: !UTCTime
--       -- ^ The time stamp when the attachment initiated.
--     , iniatDeleteOnTermination :: !Bool
--       -- ^ Indicates whether the network interface is deleted when the
--       -- instance is terminated.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceNetworkInterfaceAttachmentType

-- instance IsXML InstanceNetworkInterfaceAttachmentType where
--     xmlPickler = ec2XML

-- data InstanceNetworkInterfaceSetItemRequestType = InstanceNetworkInterfaceSetItemRequestType
--     { inisirtNetworkInterfaceId             :: !ByteString
--       -- ^ The ID of the network interface.
--     , inisirtDeviceIndex                    :: !Integer
--       -- ^ Required. The index of the device on the instance for the network
--       -- interface attachment.
--     , inisirtSubnetId                       :: !ByteString
--       -- ^ The ID of the subnet associated with the network string.
--     , inisirtDescription                    :: !ByteString
--       -- ^ The description of the network interface.
--     , inisirtPrivateIpAddress               :: !ByteString
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
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceNetworkInterfaceSetItemRequestType

-- instance IsXML InstanceNetworkInterfaceSetItemRequestType where
--     xmlPickler = ec2XML

-- data InstanceNetworkInterfaceSetItemType = InstanceNetworkInterfaceSetItemType
--     { inisitNetworkInterfaceId    :: !ByteString
--       -- ^ The ID of the network interface.
--     , inisitSubnetId              :: !ByteString
--       -- ^ The ID of the subnet.
--     , inisitVpcId                 :: !ByteString
--       -- ^ The ID of the VPC.
--     , inisitDescription           :: !ByteString
--       -- ^ The description.
--     , inisitOwnerId               :: !ByteString
--       -- ^ The ID of the customer who created the network interface.
--     , inisitStatus                :: !ByteString
--       -- ^ The status of the network interface.
--     , inisitMacAddress            :: !ByteString
--       -- ^ The MAC address.
--     , inisitPrivateIpAddress      :: !ByteString
--       -- ^ The IP address of the network interface within the subnet.
--     , inisitPrivateDnsName        :: !ByteString
--       -- ^ The private DNS name.
--     , inisitSourceDestCheck       :: !Bool
--       -- ^ Indicates whether to validate network traffic to or from this
--       -- network interface.
--     , inisitGroupSet              :: Members GroupItemType
--       -- ^ A security group.
--     , inisitAttachment            :: !InstanceNetworkInterfaceAttachmentType
--       -- ^ The network interface attachment.
--     , inisitAssociation           :: !InstanceNetworkInterfaceAssociationType
--       -- ^ The association information for an Elastic IP associated with the
--       -- network interface.
--     , inisitPrivateIpAddressesSet :: !InstancePrivateIpAddressesSetItemType
--       -- ^ The private IP addresses associated with the network interface.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceNetworkInterfaceSetItemType

-- instance IsXML InstanceNetworkInterfaceSetItemType where
--     xmlPickler = ec2XML

-- data InstancePrivateIpAddressesSetItemType = InstancePrivateIpAddressesSetItemType
--     { ipiasitPrivateIpAddress :: !ByteString
--       -- ^ The private IP address of the network interface
--     , ipiasitPrivateDnsName   :: !ByteString
--       -- ^ The private DNS name.
--     , ipiasitPrimary          :: !Bool
--       -- ^ Indicates whether this IP address is the primary private IP
--       -- address of the network interface.
--     , ipiasitAssociation      :: !InstanceNetworkInterfaceAssociationType
--       -- ^ The association information for an Elastic IP address associated
--       -- with the network interface.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstancePrivateIpAddressesSetItemType

-- instance IsXML InstancePrivateIpAddressesSetItemType where
--     xmlPickler = ec2XML

-- data InstanceStateChangeType = InstanceStateChangeType
--     { isctInstanceId    :: !ByteString
--       -- ^ The instance ID.
--     , isctCurrentState  :: !InstanceStateType
--       -- ^ The current state of the instance.
--     , isctPreviousState :: !InstanceStateType
--       -- ^ The previous state of the instance.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceStateChangeType

-- instance IsXML InstanceStateChangeType where
--     xmlPickler = ec2XML

-- data InstanceStateType = InstanceStateType
--     { istCode :: !Integer
--       -- ^ The low byte represents the state. The high byte is an opaque
--       -- internal value and should be ignored.
--     , istName :: !ByteString
--       -- ^ The current state of the instance.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceStateType

-- instance IsXML InstanceStateType where
--     xmlPickler = ec2XML

-- data InstanceStatusDetailsSetType = InstanceStatusDetailsSetType
--     { isdstName          :: !ByteString
--       -- ^ The type of instance status detail.
--     , isdstStatus        :: !ByteString
--       -- ^ The status.
--     , isdstImpairedSince :: !UTCTime
--       -- ^ The time when a status check failed. For an instance that was
--       -- launched and impaired, this is the time when the instance was
--       -- launched.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceStatusDetailsSetType

-- instance IsXML InstanceStatusDetailsSetType where
--     xmlPickler = ec2XML

-- data InstanceStatusEventsSetType = InstanceStatusEventsSetType
--     { isestItem :: !InstanceStatusEventType
--       -- ^ The scheduled events for the instance.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceStatusEventsSetType

-- instance IsXML InstanceStatusEventsSetType where
--     xmlPickler = ec2XML

-- data InstanceStatusEventType = InstanceStatusEventType
--     { isetCode        :: !ByteString
--       -- ^ The associated code of the event.
--     , isetDescription :: !ByteString
--       -- ^ A description of the event.
--     , isetNotBefore   :: !UTCTime
--       -- ^ The earliest scheduled start time for the event.
--     , isetNotAfter    :: !UTCTime
--       -- ^ The latest scheduled end time for the event.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceStatusEventType

-- instance IsXML InstanceStatusEventType where
--     xmlPickler = ec2XML

-- data InstanceStatusItemType = InstanceStatusItemType
--     { isitInstanceId       :: !ByteString
--       -- ^ The ID of the instance.
--     , isitAvailabilityZone :: !ByteString
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
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceStatusItemType

-- instance IsXML InstanceStatusItemType where
--     xmlPickler = ec2XML

-- data InstanceStatusSetType = InstanceStatusSetType
--     { isstItem :: !InstanceStatusItemType
--       -- ^ The status of the instance.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceStatusSetType

-- instance IsXML InstanceStatusSetType where
--     xmlPickler = ec2XML

-- data InstanceStatusType = InstanceStatusType
--     { istStatus  :: !ByteString
--       -- ^ The status.
--     , istDetails :: !InstanceStatusDetailsSetType
--       -- ^ The system instance health or application instance health.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InstanceStatusType

-- instance IsXML InstanceStatusType where
--     xmlPickler = ec2XML

-- data InternetGatewayAttachmentType = InternetGatewayAttachmentType
--     { igatVpcId :: !ByteString
--       -- ^ The ID of the VPC.
--     , igatState :: !ByteString
--       -- ^ The current state of the attachment.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InternetGatewayAttachmentType

-- instance IsXML InternetGatewayAttachmentType where
--     xmlPickler = ec2XML

-- data InternetGatewayType = InternetGatewayType
--     { igtInternetGatewayId :: !ByteString
--       -- ^ The ID of the Internet gateway.
--     , igtAttachmentSet     :: !InternetGatewayAttachmentType
--       -- ^ Any VPCs attached to the Internet gateway, each one wrapped in an
--       -- item element.
--     , igtTagSet            :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery InternetGatewayType

-- instance IsXML InternetGatewayType where
--     xmlPickler = ec2XML

data UserIdGroupPair = UserIdGroupPair
    { uigUserId    :: !ByteString
      -- ^ The ID of an AWS account. Cannot be used when specifying a CIDR
      -- IP address range.
    , uigGroupId   :: !ByteString
      -- ^ The ID of the security group in the specified AWS account.
      -- Cannot be used when specifying a CIDR IP address range.
    , uigGroupName :: !ByteString
      -- ^ The name of the security group in the specified AWS account.
      -- Cannot be used when specifying a CIDR IP address range.
    } deriving (Eq, Show, Generic)

instance IsQuery UserIdGroupPair

data IpPermission = IpPermission
    { iptIpProtocol :: !ByteString
      -- ^ The protocol.
    , iptFromPort   :: !Integer
      -- ^ The start of port range for the TCP and UDP protocols, or an ICMP
      -- type number. A value of -1 indicates all ICMP types.
    , iptToPort     :: !Integer
      -- ^ The end of port range for the TCP and UDP protocols, or an ICMP
      -- code. A value of -1 indicates all ICMP codes for the given ICMP
      -- type.
    , iptGroups     :: [UserIdGroupPair]
      -- ^ A list of security group and AWS account ID pairs.
    , iptIpRanges   :: [IpRange]
      -- ^ A list of IP ranges.
    } deriving (Eq, Show, Generic)

instance IsQuery IpPermission

data IpRange = IpRange
    { irCidrIp :: !ByteString
      -- ^ The CIDR range. You can either specify a CIDR range or a source
      -- security group, not both.
    } deriving (Eq, Show, Generic)

instance IsQuery IpRange

-- data LaunchPermissionItemType = LaunchPermissionItemType
--     { lpitGroup  :: !ByteString
--       -- ^ The name of the group.
--     , lpitUserId :: !ByteString
--       -- ^ The AWS account ID.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery LaunchPermissionItemType

-- instance IsXML LaunchPermissionItemType where
--     xmlPickler = ec2XML

-- data LaunchSpecificationRequestType = LaunchSpecificationRequestType
--     { lsrtImageId             :: !ByteString
--       -- ^ The AMI ID.
--     , lsrtKeyName             :: !ByteString
--       -- ^ The name of the key pair.
--     , lsrtGroupSet            :: !GroupItemType
--       -- ^ A list of security groups. Each group is wrapped in an item
--       -- element.
--     , lsrtUserData            :: !UserDataType
--       -- ^ Base64-encoded MIME user data made available to the instance(s)
--       -- in the reservation.
--     , lsrtInstanceType        :: !ByteString
--       -- ^ The instance type.
--     , lsrtPlacement           :: !PlacementRequestType
--       -- ^ The placement information for the instance.
--     , lsrtKernelId            :: !ByteString
--       -- ^ The ID of the kernel to select.
--     , lsrtRamdiskId           :: !ByteString
--       -- ^ The ID of the RAM disk to select. Some kernels require additional
--       -- drivers at launch. Check the kernel requirements for information
--       -- on whether you need to specify a RAM disk and search for the
--       -- kernel ID.
--     , lsrtBlockDeviceMapping  :: !BlockDeviceMappingItemType
--       -- ^ Any block device mapping entries for the instance. Each entry is
--       -- wrapped in an item element.
--     , lsrtMonitoring          :: !MonitoringInstanceType
--       -- ^ The monitoring information for the instance.
--     , lsrtSubnetId            :: !ByteString
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
--     } deriving (Eq, Show, Generic)

-- instance IsQuery LaunchSpecificationRequestType

-- instance IsXML LaunchSpecificationRequestType where
--     xmlPickler = ec2XML

-- data LaunchSpecificationResponseType = LaunchSpecificationResponseType
--     { lsruImageId             :: !ByteString
--       -- ^ The AMI ID.
--     , lsruKeyName             :: !ByteString
--       -- ^ The name of the key pair.
--     , lsruGroupSet            :: !GroupItemType
--       -- ^ A list of security groups. Each group is wrapped in an item
--       -- element.
--     , lsruInstanceType        :: !ByteString
--       -- ^ The instance type.
--     , lsruPlacement           :: !PlacementRequestType
--       -- ^ The placement information for the instance.
--     , lsruKernelId            :: !ByteString
--       -- ^ The ID of the kernel to select.
--     , lsruRamdiskId           :: !ByteString
--       -- ^ The ID of the RAM disk to select. Some kernels require additional
--       -- drivers at launch. Check the kernel requirements for information
--       -- on whether you need to specify a RAM disk and search for the
--       -- kernel ID.
--     , lsruBlockDeviceMapping  :: !BlockDeviceMappingItemType
--       -- ^ Any block device mapping entries for the instance. Each entry is
--       -- wrapped in an item element.
--     , lsruMonitoring          :: !MonitoringInstanceType
--       -- ^ The monitoring information for the instance.
--     , lsruSubnetId            :: !ByteString
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
--     } deriving (Eq, Show, Generic)

-- instance IsQuery LaunchSpecificationResponseType

-- instance IsXML LaunchSpecificationResponseType where
--     xmlPickler = ec2XML

-- data MonitoringInstanceType = MonitoringInstanceType
--     { mitEnabled :: !Bool
--       -- ^ Indicates whether monitoring is enabled for the instance.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery MonitoringInstanceType

-- instance IsXML MonitoringInstanceType where
--     xmlPickler = ec2XML

-- data MonitorInstancesResponseSetItemType = MonitorInstancesResponseSetItemType
--     { mirsitInstanceId :: !ByteString
--       -- ^ The instance ID.
--     , mirsitMonitoring :: !InstanceMonitoringStateType
--       -- ^ The monitoring information.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery MonitorInstancesResponseSetItemType

-- instance IsXML MonitorInstancesResponseSetItemType where
--     xmlPickler = ec2XML

-- data NetworkAclAssociationType = NetworkAclAssociationType
--     { naatNetworkAclAssociationId :: !ByteString
--       -- ^ An identifier representing the association between a network ACL
--       -- and a subnet.
--     , naatNetworkAclId            :: !ByteString
--       -- ^ The ID of the network ACL.
--     , naatSubnetId                :: !ByteString
--       -- ^ The ID of the subnet.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery NetworkAclAssociationType

-- instance IsXML NetworkAclAssociationType where
--     xmlPickler = ec2XML

-- data NetworkAclEntryType = NetworkAclEntryType
--     { naetRuleNumber   :: !Integer
--       -- ^ The rule number for the entry. ACL entries are processed in
--       -- ascending order by rule number.
--     , naetProtocol     :: !Integer
--       -- ^ The protocol. A value of -1 means all protocols.
--     , naetRuleAction   :: !ByteString
--       -- ^ Indicates whether to allow or deny the traffic that matches the
--       -- rule.
--     , naetEgress       :: !Bool
--       -- ^ Indicates an egress rule (rule is applied to traffic leaving the
--       -- subnet). Value of true indicates egress.
--     , naetCidrBlock    :: !ByteString
--       -- ^ The network range to allow or deny, in CIDR notation.
--     , naetIcmpTypeCode :: !IcmpTypeCodeType
--       -- ^ ICMP protocol: The ICMP type and code.
--     , naetPortRange    :: !PortRangeType
--       -- ^ TCP or UDP protocols: The range of ports the rule applies to.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery NetworkAclEntryType

-- instance IsXML NetworkAclEntryType where
--     xmlPickler = ec2XML

-- data NetworkAclType = NetworkAclType
--     { natNetworkAclId   :: !ByteString
--       -- ^ The ID of the network ACL.
--     , natVpcId          :: !ByteString
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
--     } deriving (Eq, Show, Generic)

-- instance IsQuery NetworkAclType

-- instance IsXML NetworkAclType where
--     xmlPickler = ec2XML

-- data NetworkInterfaceAssociationType = NetworkInterfaceAssociationType
--     { niatPublicIp      :: !ByteString
--       -- ^ The address of the Elastic IP address bound to the network
--       -- interface.
--     , niatPublicDnsName :: !ByteString
--       -- ^ The public DNS name.
--     , niatIpOwnerId     :: !ByteString
--       -- ^ The ID of the Elastic IP address owner.
--     , niatAllocationID  :: !ByteString
--       -- ^ The allocation ID.
--     , niatAssociationID :: !ByteString
--       -- ^ The association ID.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery NetworkInterfaceAssociationType

-- instance IsXML NetworkInterfaceAssociationType where
--     xmlPickler = ec2XML

-- data NetworkInterfaceAttachmentType = NetworkInterfaceAttachmentType
--     { niatAttachmentID :: !ByteString
--       -- ^ The ID of the network interface attachment.
--     , niatInstanceID   :: !ByteString
--       -- ^ The ID of the instance.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery NetworkInterfaceAttachmentType

-- instance IsXML NetworkInterfaceAttachmentType where
--     xmlPickler = ec2XML

-- data NetworkInterfacePrivateIpAddressesSetItemType = NetworkInterfacePrivateIpAddressesSetItemType
--     { nipiasitPrivateIpAddress :: !ByteString
--       -- ^ The private IP address of the network interface.
--     , nipiasitPrivateDnsName   :: !ByteString
--       -- ^ The private DNS name.
--     , nipiasitPrimary          :: !Bool
--       -- ^ Indicates whether this IP address is the primary private IP
--       -- address of the network interface.
--     , nipiasitAssociation      :: !NetworkInterfaceAssociationType
--       -- ^ The association information for an Elastic IP address associated
--       -- with the network interface.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery NetworkInterfacePrivateIpAddressesSetItemType

-- instance IsXML NetworkInterfacePrivateIpAddressesSetItemType where
--     xmlPickler = ec2XML

-- data NetworkInterfaceType = NetworkInterfaceType
--     { nitNetworkInterfaceId    :: !ByteString
--       -- ^ The ID of the network interface.
--     , nitSubnetId              :: !ByteString
--       -- ^ The ID of the subnet.
--     , niuNetworkInterfaceId    :: !ByteString
--       -- ^ The ID of the network interface.
--     , niuSubnetId              :: !ByteString
--       -- ^ The ID of the subnet.
--     , niuVpcId                 :: !ByteString
--       -- ^ The ID of the VPC.
--     , niuAvailabilityZone      :: !ByteString
--       -- ^ The Availability Zone.
--     , niuDescription           :: !ByteString
--       -- ^ A description.
--     , niuOwnerId               :: !ByteString
--       -- ^ The ID of the customer who created the interface.
--     , niuRequesterId           :: !ByteString
--       -- ^ The ID of the entity that launched the instance on your behalf
--       -- (for example, AWS Management Console or Auto Scaling)
--     , niuRequesterManaged      :: !ByteString
--       -- ^ Indicates whether the network interface is being managed by AWS.
--     , niuStatus                :: !ByteString
--       -- ^ The status of the network interface.
--     , niuMacAddress            :: !ByteString
--       -- ^ The MAC address.
--     , niuPrivateIpAddress      :: !ByteString
--       -- ^ The IP address of the network interface within the subnet.
--     , niuPrivateDnsName        :: !ByteString
--       -- ^ The private DNS name.
--     , niuSourceDestCheck       :: !Bool
--       -- ^ Indicates whether traffic to or from the instance is validated.
--     , niuGroupSet              :: !GroupItemType
--       -- ^ The security group.
--     , niuAttachment            :: !NetworkInterfaceAttachmentType
--       -- ^ The network interface attachment.
--     , niuAssociation           :: !NetworkInterfaceAssociationType
--       -- ^ The association information for an Elastic IP associated with the
--       -- network interface.
--     , niuTagSet                :: !ResourceTagSetItemType
--       -- ^ The tags assigned to the resource.
--     , niuPrivateIpAddressesSet :: !NetworkInterfacePrivateIpAddressesSetItemType
--       -- ^ The private IP addresses associated with the network interface.
--       -- Items are returned in a set.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery NetworkInterfaceType

-- instance IsXML NetworkInterfaceType where
--     xmlPickler = ec2XML

-- data PlacementGroupInfoType = PlacementGroupInfoType
--     { pgitGroupName :: !ByteString
--       -- ^ The name of the placement group.
--     , pgitStrategy  :: !ByteString
--       -- ^ The placement strategy.
--     , pgitState     :: !ByteString
--       -- ^ The status of the placement group.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery PlacementGroupInfoType

-- instance IsXML PlacementGroupInfoType where
--     xmlPickler = ec2XML

-- data PlacementRequestType = PlacementRequestType
--     { prtAvailabilityZone :: !ByteString
--       -- ^ The Availability Zone for the instance.
--     , prtGroupName        :: !ByteString
--       -- ^ The name of a placement group for the instance.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery PlacementRequestType

-- instance IsXML PlacementRequestType where
--     xmlPickler = ec2XML

-- data PlacementResponseType = PlacementResponseType
--     { pruAvailabilityZone :: !ByteString
--       -- ^ The Availability Zone of the instance.
--     , pruGroupName        :: !ByteString
--       -- ^ The name of the placement group the instance is in (for cluster
--       -- compute instances).
--     , pruTenancy          :: !ByteString
--       -- ^ The tenancy of the instance (if the instance is running within a
--       -- VPC). An instance with a tenancy of dedicated runs on
--       -- single-tenant hardware.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery PlacementResponseType

-- instance IsXML PlacementResponseType where
--     xmlPickler = ec2XML

-- data PortRangeType = PortRangeType
--     { prtFrom :: !Integer
--       -- ^ The first port in the range.
--     , prtTo   :: !Integer
--       -- ^ The last port in the range.
--     } deriving (Eq, Show, Generic)

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
--     , psrsitCurrencyCode :: !ByteString
--       -- ^ The currency for transacting the Reserved Instance resale. At
--       -- this time, the only supported currency is USD.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery PriceScheduleRequestSetItemType

-- instance IsXML PriceScheduleRequestSetItemType where
--     xmlPickler = ec2XML

-- data PriceScheduleSetItemType = PriceScheduleSetItemType
--     { pssitTerm         :: !Integer
--       -- ^ The number of months remaining in the reservation. For example, 2
--       -- is the second to the last month before the capacity reservation
--       -- expires.
--     , pssitPrice        :: !Double
--       -- ^ The fixed price for the term.
--     , pssitCurrencyCode :: !ByteString
--       -- ^ The currency for transacting the Reserved Instance resale. At
--       -- this time, the only supported currency is USD.
--     , pssitActive       :: !Bool
--       -- ^ The current price schedule, as determined by the term remaining
--       -- for the Reserved Instance in the listing.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery PriceScheduleSetItemType

-- instance IsXML PriceScheduleSetItemType where
--     xmlPickler = ec2XML

-- data PriceScheduleSetType = PriceScheduleSetType
--     { psstItem :: !PriceScheduleSetItemType
--       -- ^ The Reserved Instance listing price schedule item.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery PriceScheduleSetType

-- instance IsXML PriceScheduleSetType where
--     xmlPickler = ec2XML

-- data PricingDetailsSetItemType = PricingDetailsSetItemType
--     { pdsitPrice :: !Integer
--       -- ^ The price per instance.
--     , pdsitCount :: !Integer
--       -- ^ The number of instances available for the price.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery PricingDetailsSetItemType

-- instance IsXML PricingDetailsSetItemType where
--     xmlPickler = ec2XML

-- data PrivateIpAddressesSetItemRequestType = PrivateIpAddressesSetItemRequestType
--     { piasirtPrivateIpAddressesSet :: !AssignPrivateIpAddressesSetItemRequestType
--       -- ^ The list of private IP addresses.
--     , piasirtPrimary               :: !Bool
--       -- ^ Indicates whether the private IP address is the primary private
--       -- IP address.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery PrivateIpAddressesSetItemRequestType

-- instance IsXML PrivateIpAddressesSetItemRequestType where
--     xmlPickler = ec2XML

-- data ProductCodeItemType = ProductCodeItemType
--     { pcitProductCode :: !ByteString
--       -- ^ The product code.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ProductCodeItemType

-- instance IsXML ProductCodeItemType where
--     xmlPickler = ec2XML

-- data ProductCodesSetItemType = ProductCodesSetItemType
--     { pcsitProductCode :: !ByteString
--       -- ^ The product code.
--     , pcsitType        :: !ByteString
--       -- ^ The type of product code.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ProductCodesSetItemType

-- instance IsXML ProductCodesSetItemType where
--     xmlPickler = ec2XML

-- data ProductDescriptionSetItemType = ProductDescriptionSetItemType
--     { pdsitProductDescription :: !ByteString
--       -- ^ The description of the AMI.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ProductDescriptionSetItemType

-- instance IsXML ProductDescriptionSetItemType where
--     xmlPickler = ec2XML

-- data PropagatingVgwType = PropagatingVgwType
--     { pvtGatewayID :: !ByteString
--       -- ^ The ID of the virtual private gateway (VGW).
--     } deriving (Eq, Show, Generic)

-- instance IsQuery PropagatingVgwType

-- instance IsXML PropagatingVgwType where
--     xmlPickler = ec2XML

-- data RecurringChargesSetItemType = RecurringChargesSetItemType
--     { rcsitFrequency :: !ByteString
--       -- ^ The frequency of the recurring charge.
--     , rcsitAmount    :: !Double
--       -- ^ The amount of the recurring charge.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery RecurringChargesSetItemType

-- instance IsXML RecurringChargesSetItemType where
--     xmlPickler = ec2XML

-- data RegionItemType = RegionItemType
--     { ritRegionName     :: !ByteString
--       -- ^ The name of the region.
--     , ritRegionEndpoint :: !ByteString
--       -- ^ The region service endpoint.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery RegionItemType

-- instance IsXML RegionItemType where
--     xmlPickler = ec2XML

-- data ReservationInfoType = ReservationInfoType
--     { ritReservationId :: !ByteString
--       -- ^ The ID of the reservation.
--     , ritOwnerId       :: !ByteString
--       -- ^ The ID of the AWS account that owns the reservation.
--     , ritGroupSet      :: !GroupItemType
--       -- ^ A list of security groups. Each group is wrapped in an item
--       -- element.
--     , ritInstancesSet  :: !RunningInstancesItemType
--       -- ^ A list of instances. Each instance is wrapped in an item element.
--     , ritRequesterId   :: !ByteString
--       -- ^ The ID of the requester that launched the instances on your
--       -- behalf (for example, AWS Management Console or Auto Scaling).
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ReservationInfoType

-- instance IsXML ReservationInfoType where
--     xmlPickler = ec2XML

-- data ReservedInstanceLimitPriceType = ReservedInstanceLimitPriceType
--     { rilptAmount       :: !Double
--       -- ^ Used for Reserved Instance Marketplace offerings. Specifies the
--       -- limit price on the total order (instanceCount * price).
--     , rilptCurrencyCode :: !Double
--       -- ^ Currency in which the limitPrice amount is specified. At this
--       -- time, the only supported currency is USD.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ReservedInstanceLimitPriceType

-- instance IsXML ReservedInstanceLimitPriceType where
--     xmlPickler = ec2XML

-- data ResourceTagSetItemType = ResourceTagSetItemType
--     { rtsitKey   :: !ByteString
--       -- ^ The tag key.
--     , rtsitValue :: !ByteString
--       -- ^ The tag value.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ResourceTagSetItemType

-- instance IsXML ResourceTagSetItemType where
--     xmlPickler = ec2XML

-- data RouteTableAssociationType = RouteTableAssociationType
--     { rtatRouteTableAssociationId :: !ByteString
--       -- ^ An identifier representing the association between a route table
--       -- and a subnet.
--     , rtatRouteTableId            :: !ByteString
--       -- ^ The ID of the route table.
--     , rtatSubnetId                :: !ByteString
--       -- ^ The ID of the subnet.
--     , rtatMain                    :: !Bool
--       -- ^ Indicates whether this is the main route table.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery RouteTableAssociationType

-- instance IsXML RouteTableAssociationType where
--     xmlPickler = ec2XML

-- data RouteTableType = RouteTableType
--     { rttRouteTableId      :: !ByteString
--       -- ^ The route table's ID.
--     , rttVpcId             :: !ByteString
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
--     } deriving (Eq, Show, Generic)

-- instance IsQuery RouteTableType

-- instance IsXML RouteTableType where
--     xmlPickler = ec2XML

-- data RouteType = RouteType
--     { rtDestinationCidrBlock :: !ByteString
--       -- ^ The CIDR address block used for the destination match.
--     , rtGatewayId            :: !ByteString
--       -- ^ The ID of a gateway attached to your VPC.
--     , rtInstanceId           :: !ByteString
--       -- ^ The ID of a NAT instance in your VPC.
--     , rtInstanceOwnerId      :: !ByteString
--       -- ^ The owner of the instance.
--     , rtNetworkInterfaceId   :: !ByteString
--       -- ^ The network interface ID.
--     , rtState                :: !ByteString
--       -- ^ The state of the route. The blackhole state indicates that the
--       -- route's target isn't available (for example, the specified
--       -- gateway isn't attached to the VPC, or the specified NAT instance
--       -- has been terminated).
--     , rtOrigin               :: !ByteString
--       -- ^ Describes how the route was created.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery RouteType

-- instance IsXML RouteType where
--     xmlPickler = ec2XML

-- data RunningInstancesItemType = RunningInstancesItemType
--     { riitInstanceId            :: !ByteString
--       -- ^ The ID of the instance launched.
--     , riitImageId               :: !ByteString
--       -- ^ The ID of the AMI used to launch the instance.
--     , riitInstanceState         :: !InstanceStateType
--       -- ^ The current state of the instance.
--     , riitPrivateDnsName        :: !ByteString
--       -- ^ The private DNS name assigned to the instance. This DNS name can
--       -- only be used inside the Amazon EC2 network. This element remains
--       -- empty until the instance enters the running state.
--     , riitDnsName               :: !ByteString
--       -- ^ The public DNS name assigned to the instance. This element
--       -- remains empty until the instance enters the running state.
--     , riitReason                :: !ByteString
--       -- ^ The reason for the most recent state transition. This might be an
--       -- empty string.
--     , riitKeyName               :: !ByteString
--       -- ^ The key pair name, if this instance was launched with an
--       -- associated key pair.
--     , riitAmiLaunchIndex        :: !ByteString
--       -- ^ The AMI launch index, which can be used to find this instance in
--       -- the launch group.
--     , riitProductCodes          :: !ProductCodesSetItemType
--       -- ^ The product codes attached to this instance. Each product code is
--       -- wrapped in an item element.
--     , riitInstanceType          :: !ByteString
--       -- ^ The instance type.
--     , riitLaunchTime            :: !UTCTime
--       -- ^ The time the instance was launched.
--     , riitPlacement             :: !PlacementResponseType
--       -- ^ The location where the instance launched.
--     , riitKernelId              :: !ByteString
--       -- ^ The kernel associated with this instance.
--     , riitRamdiskId             :: !ByteString
--       -- ^ The RAM disk associated with this instance.
--     , riitPlatform              :: !ByteString
--       -- ^ The value is Windows for Windows AMIs; otherwise blank.
--     , riitMonitoring            :: !InstanceMonitoringStateType
--       -- ^ The monitoring information for the instance.
--     , riitSubnetId              :: !ByteString
--       -- ^ The ID of the subnet in which the instance is running.
--     , riitVpcId                 :: !ByteString
--       -- ^ The ID of the VPC in which the instance is running.
--     , riitPrivateIpAddress      :: !ByteString
--       -- ^ The private IP address assigned to the instance.
--     , riitIpAddress             :: !ByteString
--       -- ^ The IP address of the instance.
--     , riitSourceDestCheck       :: !Bool
--       -- ^ Specifies whether to enable an instance launched in a VPC to
--       -- perform NAT. This controls whether source/destination checking is
--       -- enabled on the instance. A value of true means checking is
--       -- enabled, and false means checking is disabled. The value must be
--       -- false for the instance to perform NAT. For more information, go
--       -- to NAT Instances in the Amazon Virtual Private Cloud User Guide.
--     , riitGroupSet              :: !GroupItemType
--       -- ^ A list of the security groups for the instance. Each group is
--       -- wrapped in an item element.
--     , riitStateReason           :: !StateReasonType
--       -- ^ The reason for the most recent state transition. See
--       -- StateReasonType for a listing of supported state change codes.
--     , riitArchitecture          :: !ByteString
--       -- ^ The architecture of the image.
--     , riitRootDeviceType        :: !ByteString
--       -- ^ The root device type used by the AMI. The AMI can use an Amazon
--       -- EBS or instance store root device.
--     , riitRootDeviceName        :: !ByteString
--       -- ^ The root device name (for example, /dev/sda1).
--     , riitBlockDeviceMapping    :: !InstanceBlockDeviceMappingResponseItemType
--       -- ^ Any block device mapping entries for the instance, each one
--       -- wrapped in an item element.
--     , riitInstanceLifecycle     :: !ByteString
--       -- ^ Indicates whether this is a Spot Instance.
--     , riitSpotInstanceRequestId :: !ByteString
--       -- ^ The ID of the Spot Instance request.
--     , riitVirtualizationType    :: !ByteString
--       -- ^ The instance's virtualization type.
--     , riitClientToken           :: !ByteString
--       -- ^ The idempotency token you provided when you launched the
--       -- instance.
--     , riitTagSet                :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     , riitHypervisor            :: !ByteString
--       -- ^ The instance's hypervisor type.
--     , riitNetworkInterfaceSet   :: !InstanceNetworkInterfaceSetItemType
--       -- ^ The network interfaces for the instance.
--     , riitIamInstanceProfile    :: !IamInstanceProfileResponseType
--       -- ^ The IAM Instance Profile (IIP) associated with the instance.
--     , riitEbsOptimized          :: !Bool
--       -- ^ Indicates whether the instance is optimized for EBS I/O. This
--       -- optimization provides dedicated throughput to Amazon EBS and an
--       -- optimized configuration stack to provide optimal I/O performance.
--       -- This optimization isn't available with all instance types.
--       -- Additional usage charges apply when using an EBS Optimized
--       -- instance.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery RunningInstancesItemType

-- instance IsXML RunningInstancesItemType where
--     xmlPickler = ec2XML

-- data SecurityGroupIdSetItemType = SecurityGroupIdSetItemType
--     { sgisitGroupId :: !ByteString
--       -- ^ The ID of the security group associated with the network
--       -- interface.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery SecurityGroupIdSetItemType

-- instance IsXML SecurityGroupIdSetItemType where
--     xmlPickler = ec2XML

-- data SecurityGroupItemType = SecurityGroupItemType
--     { sgitOwnerId             :: !ByteString
--       -- ^ The AWS account ID of the owner of the security group.
--     , sgitGroupId             :: !ByteString
--       -- ^ The ID of the security group.
--     , sgitGroupName           :: !ByteString
--       -- ^ The name of the security group.
--     , sgitGroupDescription    :: !ByteString
--       -- ^ A description of the security group.
--     , sgitVpcId               :: !ByteString
--       -- ^ [EC2-VPC] The ID of the VPC for the security group.
--     , sgitIpPermissions       :: !IpPermissionType
--       -- ^ A list of inbound rules associated with the security group. Each
--       -- permission is wrapped in an item element.
--     , sgitIpPermissionsEgress :: !IpPermissionType
--       -- ^ [EC2-VPC] A list of outbound rules associated with the security
--       -- group. Each permission is wrapped in an item element.
--     , sgitTagSet              :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery SecurityGroupItemType

-- instance IsXML SecurityGroupItemType where
--     xmlPickler = ec2XML

-- data SpotDatafeedSubscriptionType = SpotDatafeedSubscriptionType
--     { sdstOwnerId :: !ByteString
--       -- ^ The AWS account ID of the account.
--     , sdstBucket  :: !ByteString
--       -- ^ The Amazon S3 bucket where the Spot Instance datafeed is located.
--     , sdstPrefix  :: !ByteString
--       -- ^ The prefix that is prepended to datafeed files.
--     , sdstState   :: !ByteString
--       -- ^ The state of the Spot Instance datafeed subscription.
--     , sdstFault   :: !SpotInstanceStateFaultType
--       -- ^ The fault codes for the Spot Instance request, if any.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery SpotDatafeedSubscriptionType

-- instance IsXML SpotDatafeedSubscriptionType where
--     xmlPickler = ec2XML

-- data SpotInstanceRequestSetItemType = SpotInstanceRequestSetItemType
--     { sirsitSpotInstanceRequestId    :: !ByteString
--       -- ^ The ID of the Spot Instance request.
--     , sirsitSpotPrice                :: !ByteString
--       -- ^ The maximum hourly price for any Spot Instance launched to
--       -- fulfill the request.
--     , sirsitType                     :: !ByteString
--       -- ^ The Spot Instance request type.
--     , sirsitState                    :: !ByteString
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
--     , sirsitLaunchGroup              :: !ByteString
--       -- ^ The instance launch group. Launch groups are Spot Instances that
--       -- launch together and terminate together.
--     , sirsitAvailabilityZoneGroup    :: !ByteString
--       -- ^ The Availability Zone group. If you specify the same Availability
--       -- Zone group for all Spot Instance requests, all Spot Instances are
--       -- launched in the same Availability Zone.
--     , sirsitLaunchedAvailabilityZone :: !ByteString
--       -- ^ The Availability Zone in which the bid is launched.
--     , sirsitLaunchSpecification      :: !LaunchSpecificationResponseType
--       -- ^ Additional information for launching instances.
--     , sirsitInstanceId               :: !ByteString
--       -- ^ The instance ID, if an instance has been launched to fulfill the
--       -- Spot Instance request.
--     , sirsitCreateTime               :: !UTCTime
--       -- ^ The time stamp when the Spot Instance request was created.
--     , sirsitProductDescription       :: !ByteString
--       -- ^ The product description associated with the Spot Instance.
--     , sirsitTagSet                   :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery SpotInstanceRequestSetItemType

-- instance IsXML SpotInstanceRequestSetItemType where
--     xmlPickler = ec2XML

-- data SpotInstanceStateFaultType = SpotInstanceStateFaultType
--     { sisftCode    :: !ByteString
--       -- ^ The reason code for the Spot Instance state change.
--     , sisftMessage :: !ByteString
--       -- ^ The message for the Spot Instance state change.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery SpotInstanceStateFaultType

-- instance IsXML SpotInstanceStateFaultType where
--     xmlPickler = ec2XML

-- data SpotInstanceStatusMessageType = SpotInstanceStatusMessageType
--     { sismtCode       :: !ByteString
--       -- ^ The status code of the request.
--     , sismtUpdateTime :: !UTCTime
--       -- ^ The time of the most recent status update.
--     , sismtMessage    :: !ByteString
--       -- ^ The description for the status code for the Spot request.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery SpotInstanceStatusMessageType

-- instance IsXML SpotInstanceStatusMessageType where
--     xmlPickler = ec2XML

-- data SpotPriceHistorySetItemType = SpotPriceHistorySetItemType
--     { sphsitInstanceType       :: !ByteString
--       -- ^ The instance type.
--     , sphsitProductDescription :: !ByteString
--       -- ^ A general description of the AMI.
--     , sphsitSpotPrice          :: !ByteString
--       -- ^ The maximum price you will pay to launch one or more Spot
--       -- Instances.
--     , sphsitTimestamp          :: !UTCTime
--       -- ^ The date and time the request was created.
--     , sphsitAvailabilityZone   :: !ByteString
--       -- ^ The Availability Zone.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery SpotPriceHistorySetItemType

-- instance IsXML SpotPriceHistorySetItemType where
--     xmlPickler = ec2XML

-- data StateReasonType = StateReasonType
--     { srtCode    :: !ByteString
--       -- ^ The reason code for the state change.
--     , srtMessage :: !ByteString
--       -- ^ The message for the state change.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery StateReasonType

-- instance IsXML StateReasonType where
--     xmlPickler = ec2XML

-- data SubnetType = SubnetType
--     { stSubnetId                :: !ByteString
--       -- ^ The ID of the subnet.
--     , stState                   :: !ByteString
--       -- ^ The current state of the subnet.
--     , stVpcId                   :: !ByteString
--       -- ^ The ID of the VPC the subnet is in.
--     , stCidrBlock               :: !ByteString
--       -- ^ The CIDR block assigned to the subnet.
--     , stAvailableIpAddressCount :: !Integer
--       -- ^ The number of unused IP addresses in the subnet (the IP addresses
--       -- for any stopped instances are considered unavailable).
--     , stAvailabilityZone        :: !ByteString
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
--     } deriving (Eq, Show, Generic)

-- instance IsQuery SubnetType

-- instance IsXML SubnetType where
--     xmlPickler = ec2XML

-- data TagSetItemType = TagSetItemType
--     { tsitResourceId   :: !ByteString
--       -- ^ The ID of the resource. For example, ami-1a2b3c4d.
--     , tsitResourceType :: !ByteString
--       -- ^ The type of resource.
--     , tsitKey          :: !ByteString
--       -- ^ The key of the tag.
--     , tsitValue        :: !ByteString
--       -- ^ The value of the tag.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery TagSetItemType

-- instance IsXML TagSetItemType where
--     xmlPickler = ec2XML

-- data UserDataType = UserDataType
--     { udtData :: !ByteString
--       -- ^ The Base64-encoded MIME user data made available to the
--       -- instance(s) in the reservation.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery UserDataType

-- instance IsXML UserDataType where
--     xmlPickler = ec2XML

-- data VolumeStatusItemType = VolumeStatusItemType
--     { vsitVolumeId         :: !ByteString
--       -- ^ The volume ID.
--     , vsitAvailabilityZone :: !ByteString
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
--     } deriving (Eq, Show, Generic)

-- instance IsQuery VolumeStatusItemType

-- instance IsXML VolumeStatusItemType where
--     xmlPickler = ec2XML

-- data VolumeStatusInfoType = VolumeStatusInfoType
--     { vsitStatus  :: !ByteString
--       -- ^ The status of the volume.
--     , vsitDetails :: !VolumeStatusDetailsItemType
--       -- ^ The details of the volume status. Each volume status detail is
--       -- wrapped in an item type.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery VolumeStatusInfoType

-- instance IsXML VolumeStatusInfoType where
--     xmlPickler = ec2XML

-- data VolumeStatusDetailsItemType = VolumeStatusDetailsItemType
--     { vsditName   :: !ByteString
--       -- ^ The name of the volume status.
--     , vsditStatus :: !ByteString
--       -- ^ The intended status of the volume status.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery VolumeStatusDetailsItemType

-- instance IsXML VolumeStatusDetailsItemType where
--     xmlPickler = ec2XML

-- data VolumeStatusEventItemType = VolumeStatusEventItemType
--     { vseitEventType   :: !ByteString
--       -- ^ The type of this event.
--     , vseitEventId     :: !ByteString
--       -- ^ The ID of this event.
--     , vseitDescription :: !ByteString
--       -- ^ A description of the event.
--     , vseitNotBefore   :: !UTCTime
--       -- ^ The earliest start time of the event.
--     , vseitNotAfter    :: !UTCTime
--       -- ^ The latest end time of the event.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery VolumeStatusEventItemType

-- instance IsXML VolumeStatusEventItemType where
--     xmlPickler = ec2XML

-- data VolumeStatusActionItemType = VolumeStatusActionItemType
--     { vsaitCode        :: !ByteString
--       -- ^ The code identifying the action, for example, enable-volume-io.
--     , vsaitEventType   :: !ByteString
--       -- ^ The event type associated with this action.
--     , vsaitEventId     :: !ByteString
--       -- ^ The ID of the event associated with this action.
--     , vsaitDescription :: !ByteString
--       -- ^ A description of the action.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery VolumeStatusActionItemType

-- instance IsXML VolumeStatusActionItemType where
--     xmlPickler = ec2XML

-- data VpcType = VpcType
--     { vtVpcId           :: !ByteString
--       -- ^ The ID of the VPC.
--     , vtState           :: !ByteString
--       -- ^ The current state of the VPC.
--     , vtCidrBlock       :: !ByteString
--       -- ^ The CIDR block for the VPC.
--     , vtDhcpOptionsId   :: !ByteString
--       -- ^ The ID of the set of DHCP options you've associated with the VPC
--       -- (or default if the default options are associated with the VPC).
--     , vtTagSet          :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     , vtInstanceTenancy :: !ByteString
--       -- ^ The allowed tenancy of instances launched into the VPC.
--     , vtIsDefault       :: !Bool
--       -- ^ Indicates whether the VPC is the default VPC.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery VpcType

-- instance IsXML VpcType where
--     xmlPickler = ec2XML

-- data VpnConnectionOptions = VpnConnectionOptions
--     { vcortStaticRoutesOnly :: !Bool
--       -- ^ Indicates whether the VPN connection uses static routes only.
--       -- Static routes must be used for devices that don't support BGP.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery VpnConnectionOptionsResponseType

-- instance IsXML VpnConnectionOptionsResponseType where
--     xmlPickler = ec2XML

-- data VpnConnectionType = VpnConnectionType
--     { vctVpnConnectionId              :: !ByteString
--       -- ^ The ID of the VPN connection.
--     , vctState                        :: !ByteString
--       -- ^ The current state of the VPN connection.
--     , vctCustomerGatewayConfiguration :: !ByteString
--       -- ^ The configuration information for the VPN connection's customer
--       -- gateway (in the native XML format). This element is always
--       -- present in the CreateVpnConnection response; however, it's
--       -- present in the DescribeVpnConnections response only if the VPN
--       -- connection is in the pending or available state.
--     , vctType                         :: !ByteString
--       -- ^ The type of VPN connection.
--     , vctCustomerGatewayId            :: !ByteString
--       -- ^ The ID of the customer gateway at your end of the VPN connection.
--     , vctVpnGatewayId                 :: !ByteString
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
--     } deriving (Eq, Show, Generic)

-- instance IsQuery VpnConnectionType

-- instance IsXML VpnConnectionType where
--     xmlPickler = ec2XML

-- data VpnGatewayType = VpnGatewayType
--     { vgtVpnGatewayId     :: !ByteString
--       -- ^ The ID of the virtual private gateway.
--     , vgtState            :: !ByteString
--       -- ^ The current state of the virtual private gateway.
--     , vgtType             :: !ByteString
--       -- ^ The type of VPN connection the virtual private gateway supports.
--     , vgtAvailabilityZone :: !ByteString
--       -- ^ The Availability Zone where the virtual private gateway was
--       -- created.
--     , vgtAttachments      :: !AttachmentType
--       -- ^ Any VPCs attached to the virtual private gateway, each one
--       -- wrapped in an item element.
--     , vgtTagSet           :: !ResourceTagSetItemType
--       -- ^ Any tags assigned to the resource, each one wrapped in an item
--       -- element.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery VpnGatewayType

-- instance IsXML VpnGatewayType where
--     xmlPickler = ec2XML

-- data VpnStaticRouteType = VpnStaticRouteType
--     { vsrtDestinationCidrBlock :: !ByteString
--       -- ^ The CIDR block associated with the local subnet of the customer
--       -- data center.
--     , vsrtSource               :: !ByteString
--       -- ^ Indicates how the routes were provided.
--     , vsrtState                :: !ByteString
--       -- ^ The current state of the static route.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery VpnStaticRouteType

-- instance IsXML VpnStaticRouteType where
--     xmlPickler = ec2XML

-- data VpnTunnelTelemetryType = VpnTunnelTelemetryType
--     { vtttOutsideIpAddress   :: !ByteString
--       -- ^ The Internet-routable IP address of the virtual private gateway's
--       -- outside interface.
--     , vtttStatus             :: !ByteString
--       -- ^ The status of the VPN tunnel.
--     , vtttLastStatusChange   :: !UTCTime
--       -- ^ The date and time of the last change in status.
--     , vtttStatusMessage      :: !ByteString
--       -- ^ If an error occurs, a description of the error.
--     , vtttAcceptedRouteCount :: !Integer
--       -- ^ The number of accepted routes.
--     } deriving (Eq, Show, Generic)

-- instance IsQuery VpnTunnelTelemetryType

-- instance IsXML VpnTunnelTelemetryType where
--     xmlPickler = ec2XML
