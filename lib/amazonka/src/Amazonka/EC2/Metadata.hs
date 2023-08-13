{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Amazonka.EC2.Metadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- This module contains functions for retrieving various EC2 metadata from an
-- instance's local metadata endpoint. It assumes that you're running the code
-- on an EC2 instance or have a compatible @instance-data@ endpoint available.
--
-- It is intended to be usable when you need to make metadata calls prior to
-- initialisation of the 'Amazonka.Env.Env'.
module Amazonka.EC2.Metadata
  ( -- * EC2 Instance Check
    isEC2,

    -- * Retrieving Instance Data
    dynamic,
    metadata,
    userdata,
    identity,

    -- ** Path Constructors
    Dynamic (..),
    Metadata (..),
    Autoscaling (..),
    Mapping (..),
    ElasticGpus (..),
    ElasticInference (..),
    Events (..),
    Maintenance (..),
    Recommendations (..),
    IAM (..),
    IdentityCredentialsEC2 (..),
    Interface (..),
    Placement (..),
    Services (..),
    Spot (..),
    Tags (..),

    -- ** Identity Document
    IdentityDocument (..),

    -- *** Lenses
    identityDocument_devpayProductCodes,
    identityDocument_billingProducts,
    identityDocument_version,
    identityDocument_privateIp,
    identityDocument_availabilityZone,
    identityDocument_region,
    identityDocument_instanceId,
    identityDocument_instanceType,
    identityDocument_accountId,
    identityDocument_imageId,
    identityDocument_kernelId,
    identityDocument_ramdiskId,
    identityDocument_architecture,
    identityDocument_pendingTime,
  )
where

import Amazonka.Data
import Amazonka.Prelude
import Amazonka.Types (Region)
import qualified Control.Exception as Exception
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import Network.HTTP.Simple (setRequestHeader, setRequestMethod)

data Dynamic
  = -- | Value showing whether the customer has enabled detailed one-minute
    -- monitoring in CloudWatch.
    --
    -- Valid values: @enabled@ | @disabled@.
    FWS
  | -- | JSON containing instance attributes, such as instance-id,
    -- private IP address, etc.
    -- /See:/ 'identity', 'InstanceDocument'.
    Document
  | -- | Used to verify the document's authenticity and content against the
    -- signature.
    PKCS7
  | -- | Data that can be used by other parties to verify its origin
    -- and authenticity.
    Signature
  deriving stock (Eq, Ord, Show, Generic)

instance ToText Dynamic where
  toText =
    ("dynamic/" <>) . \case
      FWS -> "fws/instance-monitoring"
      Document -> "instance-identity/document"
      PKCS7 -> "instance-identity/pkcs7"
      Signature -> "instance-identity/signature"

-- | Instance metadata categories. The list of supported categories
-- are listed in the [EC2 Documentation](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instancedata-data-categories.html).
data Metadata
  = -- | The AMI ID used to launch the instance.
    AMIId
  | -- | If you started more than one instance at the same time, this value
    -- indicates the order in which the instance was launched.
    -- The value of the first instance launched is 0.
    AMILaunchIndex
  | -- | The path to the AMI's manifest file in Amazon S3.
    -- If you used an Amazon EBS-backed AMI to launch the instance,
    -- the returned result is @unknown@.
    AMIManifestPath
  | -- | The AMI IDs of any instances that were rebundled to create this AMI.
    -- This value will only exist if the AMI manifest file contained an
    -- @ancestor-amis@ key.
    AncestorAMIIds
  | -- | See: 'Autoscaling'
    Autoscaling !Autoscaling
  | -- | See: 'Mapping'
    BlockDevice !Mapping
  | -- | See: 'ElasticGpus'
    ElasticGpus !ElasticGpus
  | -- | See 'ElasticInference'
    ElasticInference !ElasticInference
  | -- | See 'Events'
    Events !Events
  | -- | If the EC2 instance is using IP-based naming (IPBN), this is
    -- the private IPv4 DNS hostname of the instance. If the EC2
    -- instance is using Resource-based naming (RBN), this is the
    -- RBN. In cases where multiple network interfaces are present,
    -- this refers to the eth0 device (the device for which the device
    -- number is 0). For more information about IPBN and RBN, see
    -- [Amazon EC2 instance hostname types](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-naming.html).
    Hostname
  | -- | See: 'IAM'
    IAM !IAM
  | -- | See: 'IdentityCredentialsEC2'
    IdentityCredentialsEC2 !IdentityCredentialsEC2
  | -- | Notifies the instance that it should reboot in preparation for bundling.
    -- Valid values: @none@ | @shutdown@ | @bundle-pending@.
    InstanceAction
  | -- | The ID of this instance.
    InstanceId
  | -- | The purchasing option of this instance. For more
    -- information, see
    -- [Instance purchasing options](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-purchasing-options.html).
    InstanceLifeCycle
  | -- | The type of instance. For more information, see
    -- [Instance types](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html).
    InstanceType
  | -- | The IPv6 address of the instance. In cases where multiple
    -- network interfaces are present, this refers to the eth0 device
    -- (the device for which the device number is 0) network interface
    -- and the first IPv6 address assigned. If no IPv6 address exists
    -- on network interface[0], this item is not set and results in an
    -- HTTP 404 response.
    IPV6
  | -- | The ID of the kernel launched with this instance, if applicable.
    KernelId
  | -- | In cases where multiple network interfaces are present, this
    -- refers to the eth0 device (the device for which the device
    -- number is 0). If the EC2 instance is using IP-based naming
    -- (IPBN), this is the private IPv4 DNS hostname of the
    -- instance. If the EC2 instance is using Resource-based naming
    -- (RBN), this is the RBN. For more information about IPBN, RBN,
    -- and EC2 instance naming, see
    -- [Amazon EC2 instance hostname types](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-naming.html).
    LocalHostname
  | -- | The private IPv4 address of the instance. In cases where
    -- multiple network interfaces are present, this refers to the
    -- eth0 device (the device for which the device number is 0). If
    -- this is an IPv6-only instance, this item is not set and results
    -- in an HTTP 404 response.
    LocalIPV4
  | -- | The instance's media access control (MAC) address. In cases
    -- where multiple network interfaces are present, this refers to
    -- the eth0 device (the device for which the device number is 0).
    MAC
  | -- | See: 'Interface'
    Network !Text !Interface
  | -- | See: 'Placement'
    Placement !Placement
  | -- | AWS Marketplace product codes associated with the instance,
    -- if any.
    ProductCodes
  | -- | The instance's public DNS (IPv4). This category is only
    -- returned if the @enableDnsHostnames@ attribute is set to
    -- @true@. For more information, see
    -- [Using DNS with Your VPC](https://docs.aws.amazon.com/vpc/latest/userguide/vpc-dns.html)
    -- in the /Amazon VPC User Guide/. If the instance only has a
    -- public-IPv6 address and no public-IPv4 address, this item is
    -- not set and results in an HTTP 404 response.
    PublicHostname
  | -- | The public IP address. If an Elastic IP address is associated with the
    -- instance, the value returned is the Elastic IP address.
    PublicIPV4
  | -- | Public key. Only available if supplied at instance launch time.
    OpenSSHKey
  | -- | The ID of the RAM disk specified at launch time, if applicable.
    RAMDiskId
  | -- | ID of the reservation.
    ReservationId
  | -- | The names of the security groups applied to the instance.
    --
    -- After launch, you can change the security groups of the
    -- instances. Such changes are reflected here and in
    -- @network\/interfaces\/macs\/${mac}\/security-groups@.
    SecurityGroups
  | -- | See: 'Services'
    Services !Services
  | -- | See: 'Spot'
    Spot !Spot
  | -- | See: 'Tags'
    Tags !Tags
  | -- | Any other piece of metadata specified by arbitrary key.
    --
    -- This is provided for forward compatibility: should there ever appear any gap
    -- between a future version of AWS API and what (possibly an outdated version of)
    -- this library covers -- the constructor gives the option to work around.
    XtendedMeta !Text
  deriving stock (Eq, Ord, Show, Generic)

instance ToText Metadata where
  toText =
    ("meta-data/" <>) . \case
      AMIId -> "ami-id"
      AMILaunchIndex -> "ami-launch-index"
      AMIManifestPath -> "ami-manifest-path"
      AncestorAMIIds -> "ancestor-ami-ids"
      Autoscaling m -> "autoscaling/" <> toText m
      BlockDevice m -> "block-device-mapping/" <> toText m
      Hostname -> "hostname"
      ElasticGpus m -> "elastic-gpus/" <> toText m
      ElasticInference m -> "elastic-inference/" <> toText m
      Events m -> "events/" <> toText m
      IAM m -> "iam/" <> toText m
      IdentityCredentialsEC2 m -> "identity-credentials/ec2/" <> toText m
      InstanceAction -> "instance-action"
      InstanceId -> "instance-id"
      InstanceLifeCycle -> "instance-life-cycle"
      InstanceType -> "instance-type"
      IPV6 -> "ipv6"
      KernelId -> "kernel-id"
      LocalHostname -> "local-hostname"
      LocalIPV4 -> "local-ipv4"
      MAC -> "mac"
      Network n m -> "network/interfaces/macs/" <> toText n <> "/" <> toText m
      Placement m -> "placement/" <> toText m
      ProductCodes -> "product-codes"
      PublicHostname -> "public-hostname"
      PublicIPV4 -> "public-ipv4"
      OpenSSHKey -> "public-keys/0/openssh-key"
      RAMDiskId -> "ramdisk-id"
      ReservationId -> "reservation-id"
      SecurityGroups -> "security-groups"
      Services m -> "services/" <> toText m
      Spot m -> "spot/" <> toText m
      Tags m -> "tags/" <> toText m
      XtendedMeta m -> m

-- | Metadata keys for @autoscaling/*@.
data Autoscaling
  = -- | Value showing the target Auto Scaling lifecycle state that an
    -- Auto Scaling instance is transitioning to. Present when the
    -- instance transitions to one of the target lifecycle states
    -- after March 10, 2022. Possible values: @Detached@ | @InService@
    -- | @Standby@ | @Terminated@ | @Warmed:Hibernated@ |
    -- @Warmed:Running@ | @Warmed:Stopped@ | @Warmed:Terminated@. See
    -- [Retrieve the target lifecycle state through instance metadata](https://docs.aws.amazon.com/autoscaling/ec2/userguide/retrieving-target-lifecycle-state-through-imds.html)
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    TargetLifecycleState
  deriving stock (Eq, Ord, Show, Generic)

instance ToText Autoscaling where
  toText = \case
    TargetLifecycleState -> "target-lifecycle-state"

-- | Metadata keys for @block-device-mapping/*@.
data Mapping
  = -- | The virtual device that contains the root/boot file system.
    AMI
  | -- | The virtual devices associated with Amazon EBS volumes, if present.
    -- This value is only available in metadata if it is present at launch time.
    -- The N indicates the index of the Amazon EBS volume (such as ebs1 or ebs2).
    EBS !Int
  | -- | The virtual devices associated with ephemeral devices, if present.
    -- The N indicates the index of the ephemeral volume.
    Ephemeral !Int
  | -- | The virtual devices or partitions associated with the root devices,
    -- or partitions on the virtual device, where the root (/ or C:) file system
    -- is associated with the given instance.
    Root
  | -- | The virtual devices associated with swap. Not always present.
    Swap
  deriving stock (Eq, Ord, Show, Generic)

instance ToText Mapping where
  toText = \case
    AMI -> "ami"
    EBS n -> "ebs" <> toText n
    Ephemeral n -> "ephemeral" <> toText n
    Root -> "root"
    Swap -> "root"

-- | Metadata keys for @elastic-gpus/*@.
newtype ElasticGpus
  = -- | If there is an Elastic GPU attached to the instance, contains
    -- a JSON string with information about the Elastic GPU, including
    -- its ID and connection information.
    EGAssociations Text
  deriving stock (Eq, Ord, Show, Generic)

instance ToText ElasticGpus where
  toText = \case
    EGAssociations gpuId -> "associations/" <> gpuId

-- | Metadata keys for @elastic-inference/*@.
newtype ElasticInference
  = -- | If there is an Elastic Inference accelerator attached to the
    -- instance, contains a JSON string with information about the
    -- Elastic Inference accelerator, including its ID and type.
    EIAssociations Text
  deriving stock (Eq, Ord, Show, Generic)

instance ToText ElasticInference where
  toText = \case
    EIAssociations eiId -> "associations/" <> eiId

-- | Metadata keys for @events/*@.
data Events
  = Maintenance !Maintenance
  | Recommendations !Recommendations
  deriving stock (Eq, Ord, Show, Generic)

instance ToText Events where
  toText = \case
    Maintenance m -> "maintenance/" <> toText m
    Recommendations m -> "recommendations/" <> toText m

-- | Metadata keys for @events/maintenance/*@.
data Maintenance
  = -- | If there are completed or canceled maintenance events for the
    -- instance, contains a JSON string with information about the
    -- events. For more information, see
    -- [To view event history about completed or canceled events](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-instances-status-check_sched.html#viewing-event-history).
    History
  | -- | If there are active maintenance events for the instance,
    -- contains a JSON string with information about the events. For
    -- more information, see
    -- [View scheduled events](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-instances-status-check_sched.html#viewing_scheduled_events).
    Scheduled
  deriving stock (Eq, Ord, Show, Generic)

instance ToText Maintenance where
  toText = \case
    History -> "history"
    Scheduled -> "scheduled"

-- | Metadata keys for @events\/recommendations\/*@.
data Recommendations
  = -- | The approximate time, in UTC, when the EC2 instance rebalance
    -- recommendation notification is emitted for the instance. The
    -- following is an example of the metadata for this category:
    -- @{"noticeTime": "2020-11-05T08:22:00Z"}@. This category is
    -- available only after the notification is emitted. For more
    -- information, see
    -- [EC2 instance rebalance recommendations](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/rebalance-recommendations.html).
    Rebalance
  deriving stock (Eq, Ord, Show, Generic)

instance ToText Recommendations where
  toText = \case
    Rebalance -> "rebalance"

-- | Metadata keys for @iam/*@.
data IAM
  = -- | If there is an IAM role associated with the instance,
    -- contains information about the last time the instance profile
    -- was updated, including the instance's LastUpdated date,
    -- InstanceProfileArn, and InstanceProfileId. Otherwise, not
    -- present.
    Info
  | -- | If there is an IAM role associated with the instance,
    -- @role-name@ is the name of the role, and @role-name@ contains the
    -- temporary security credentials associated with the role (for
    -- more information, see
    -- [Retrieve security credentials from instance metadata](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/iam-roles-for-amazon-ec2.html#instance-metadata-security-credentials)).
    -- Otherwise, not present.
    --
    -- See: 'Auth' for JSON deserialisation.
    SecurityCredentials (Maybe Text)
  deriving stock (Eq, Ord, Show, Generic)

instance ToText IAM where
  toText = \case
    Info -> "info"
    SecurityCredentials r -> "security-credentials/" <> maybe mempty toText r

-- | Metadata keys for @identity-credentials\/ec2\/*@.
data IdentityCredentialsEC2
  = -- | Information about the credentials in
    -- @identity-credentials/ec2/security-credentials/ec2-instance@.
    ICEInfo
  | -- | Credentials for the instance identity role that allow
    -- on-instance software to identify itself to AWS to support
    -- features such as EC2 Instance Connect and AWS Systems Manager
    -- Default Host Management Configuration. These credentials have
    -- no policies attached, so they have no additional AWS API
    -- permissions beyond identifying the instance to the AWS
    -- feature. For more information, see [Instance identity
    -- roles](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-identity-roles.html).
    ICESecurityCredentials
  deriving stock (Eq, Ord, Show, Generic)

instance ToText IdentityCredentialsEC2 where
  toText = \case
    ICEInfo -> "info"
    ICESecurityCredentials -> "security-credentials/ec2-instance"

-- | Metadata keys for @network\/interfaces\/macs\/${mac}\/*@.
data Interface
  = -- | The unique device number associated with that interface. The
    -- device number corresponds to the device name; for example, a
    -- @device-number@ of 2 is for the eth2 device. This category
    -- corresponds to the @DeviceIndex@ and @device-index@ fields that
    -- are used by the Amazon EC2 API and the EC2 commands for the AWS
    -- CLI.
    IDeviceNumber
  | -- | The ID of the network interface.
    IInterfaceId
  | -- | The private IPv4 addresses that are associated with each public-ip
    -- address and assigned to that interface.
    IIPV4Associations !Text
  | -- | The IPv6 addresses associated with the interface. Returned
    -- only for instances launched into a VPC.
    IIPV6s
  | -- | The private IPv4 DNS hostname of the instance. In cases where
    -- multiple network interfaces are present, this refers to the
    -- eth0 device (the device for which the device number is 0). If
    -- this is a IPv6-only instance, this is the resource-based
    -- name. For more information about IPBN and RBN, see
    -- [Amazon EC2 instance hostname types](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-naming.html).
    ILocalHostname
  | -- | The private IPv4 addresses associated with the interface. If
    -- this is an IPv6-only network interface, this item is not set
    -- and results in an HTTP 404 response.
    ILocalIPV4s
  | -- | The instance's MAC address.
    IMAC
  | -- | The index of the network card. Some instance types support multiple network cards.
    INetworkCardIndex
  | -- | The ID of the owner of the network interface. In multiple-interface
    -- environments, an interface can be attached by a third party, such as
    -- Elastic Load Balancing. Traffic on an interface is always billed to
    -- the interface owner.
    IOwnerId
  | -- | The interface's public DNS (IPv4). This category is only
    -- returned if the @enableDnsHostnames@ attribute is set to
    -- @true@. For more information, see
    -- [Using DNS with Your VPC](https://docs.aws.amazon.com/vpc/latest/userguide/vpc-dns.html)
    -- in the /Amazon VPC User Guide/. If the instance only has a
    -- public-IPv6 address and no public-IPv4 address, this item is
    -- not set and results in an HTTP 404 response.
    IPublicHostname
  | -- | The Elastic IP addresses associated with the interface. There may be
    -- multiple IP addresses on an instance.
    IPublicIPV4s
  | -- | Security groups to which the network interface belongs.
    ISecurityGroups
  | -- | The IDs of the security groups to which the network interface belongs.
    ISecurityGroupIds
  | -- | The ID of the subnet in which the interface resides.
    ISubnetId
  | -- | The IPv4 CIDR block of the subnet in which the interface resides.
    ISubnetIPV4_CIDRBlock
  | -- | The IPv6 CIDR block of the subnet in which the interface resides.
    ISubnetIPV6_CIDRBlock
  | -- | The ID of the VPC in which the interface resides.
    IVPCId
  | -- | The primary IPv4 CIDR block of the VPC.
    IVPCIPV4_CIDRBlock
  | -- | The IPv4 CIDR blocks for the VPC.
    IVPCIPV4_CIDRBlocks
  | -- | The IPv6 CIDR block of the VPC in which the interface resides.
    IVPCIPV6_CIDRBlocks
  deriving stock (Eq, Ord, Show, Generic)

instance ToText Interface where
  toText = \case
    IDeviceNumber -> "device-number"
    IInterfaceId -> "interface-id"
    IIPV4Associations ip -> "ipv4-associations/" <> toText ip
    IIPV6s -> "ipv6s"
    ILocalHostname -> "local-hostname"
    ILocalIPV4s -> "local-ipv4s"
    IMAC -> "mac"
    INetworkCardIndex -> "network-card-index"
    IOwnerId -> "owner-id"
    IPublicHostname -> "public-hostname"
    IPublicIPV4s -> "public-ipv4s"
    ISecurityGroups -> "security-groups"
    ISecurityGroupIds -> "security-group-ids"
    ISubnetId -> "subnet-id"
    ISubnetIPV4_CIDRBlock -> "subnet-ipv4-cidr-block"
    ISubnetIPV6_CIDRBlock -> "subnet-ipv6-cidr-block"
    IVPCId -> "vpc-id"
    IVPCIPV4_CIDRBlock -> "vpc-ipv4-cidr-block"
    IVPCIPV4_CIDRBlocks -> "vpc-ipv4-cidr-blocks"
    IVPCIPV6_CIDRBlocks -> "vpc-ipv6-cidr-blocks"

-- | Metadata keys for @placement/*@.
data Placement
  = -- | The Availability Zone in which the instance launched.
    AvailabilityZone
  | -- | The static Availability Zone ID in which the instance is
    -- launched. The Availability Zone ID is consistent across
    -- accounts. However, it might be different from the Availability
    -- Zone, which can vary by account.
    AvailabilityZoneId
  | -- | The name of the placement group in which the instance is launched.
    GroupName
  | -- | The ID of the host on which the instance is
    -- launched. Applicable only to Dedicated Hosts.
    HostId
  | -- | The number of the partition in which the instance is launched.
    PartitionNumber
  | -- | The AWS Region in which the instance is launched.
    Region
  deriving stock (Eq, Ord, Show, Generic)

instance ToText Placement where
  toText = \case
    AvailabilityZone -> "availability-zone"
    AvailabilityZoneId -> "availability-zone-id"
    GroupName -> "group-name"
    HostId -> "host-id"
    PartitionNumber -> "partition-number"
    Region -> "region"

-- | Metadata keys for @services/*@.
data Services
  = -- | The domain for AWS resources for the Region.
    Domain
  | -- | The partition that the resource is in. For standard AWS
    -- Regions, the partition is @aws@. If you have resources in other
    -- partitions, the partition is @aws-${partitionname}@. For example,
    -- the partition for resources in the China (Beijing) Region is
    -- @aws-cn@.
    Partition
  deriving stock (Eq, Ord, Show, Generic)

instance ToText Services where
  toText = \case
    Domain -> "domain"
    Partition -> "partition"

-- | Metadata keys for @spot/*@.
data Spot
  = -- | The action (hibernate, stop, or terminate) and the
    -- approximate time, in UTC, when the action will occur. This item
    -- is present only if the Spot Instance has been marked for
    -- hibernate, stop, or terminate. For more information, see
    -- [instance-action](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-instance-termination-notices.html#instance-action-metadata).
    SInstanceAction
  | -- | The approximate time, in UTC, that the operating system for
    -- your Spot Instance will receive the shutdown signal. This item
    -- is present and contains a time value (for example,
    -- 2015-01-05T18:02:00Z) only if the Spot Instance has been marked
    -- for termination by Amazon EC2. The termination-time item is not
    -- set to a time if you terminated the Spot Instance yourself. For
    -- more information, see
    -- [termination-time](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-instance-termination-notices.html#termination-time-metadata).
    STerminationTime
  deriving stock (Eq, Ord, Show, Generic)

instance ToText Spot where
  toText = \case
    SInstanceAction -> "instance-action"
    STerminationTime -> "termination-time"

-- | Metadata keys for @tags/*@.
data Tags
  = -- | The instance tags associated with the instance. Only
    -- available if you explicitly allow access to tags in instance
    -- metadata. For more information, see
    -- [Allow access to tags in instance metadata](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#allow-access-to-tags-in-IMDS).
    Instance
  deriving stock (Eq, Ord, Show, Generic)

instance ToText Tags where
  toText = \case
    Instance -> "instance"

latest :: Text
latest = "http://169.254.169.254/latest/"

-- | Test whether the underlying host is running on EC2 by
-- making an HTTP request to @http://instance-data/latest@.
isEC2 :: MonadIO m => Client.Manager -> m Bool
isEC2 m = liftIO (Exception.catch req err)
  where
    req = do
      !_ <- get m "http://instance-data/latest"

      return True

    err :: Client.HttpException -> IO Bool
    err = const (return False)

-- | Retrieve the specified 'Dynamic' data.
--
-- Throws 'HttpException' if HTTP communication fails.
dynamic :: MonadIO m => Client.Manager -> Dynamic -> m ByteString
dynamic m = get m . mappend latest . toText

-- | Retrieve the specified 'Metadata'.
--
-- Throws 'HttpException' if HTTP communication fails.
metadata :: MonadIO m => Client.Manager -> Metadata -> m ByteString
metadata m = get m . mappend latest . toText

-- | Retrieve the user data. Returns 'Nothing' if no user data is assigned
-- to the instance.
--
-- Throws 'HttpException' if HTTP communication fails.
userdata :: MonadIO m => Client.Manager -> m (Maybe ByteString)
userdata m =
  liftIO $
    Exception.try (get m (latest <> "user-data")) >>= \case
      Left (Client.HttpExceptionRequest _ (Client.StatusCodeException rs _))
        | fromEnum (Client.responseStatus rs) == 404 ->
            return Nothing
      --
      Left e -> Exception.throwIO e
      --
      Right b -> pure (Just b)

-- | Represents an instance's identity document.
--
-- /Note:/ Fields such as '_instanceType' are represented as unparsed 'Text' and
-- will need to be manually parsed using 'fromText' when the relevant types
-- from a library such as "Amazonka.EC2" are brought into scope.
data IdentityDocument = IdentityDocument
  { devpayProductCodes :: Maybe [Text],
    billingProducts :: Maybe [Text],
    version :: Maybe Text,
    privateIp :: Maybe Text,
    availabilityZone :: Text,
    region :: Region,
    instanceId :: Text,
    instanceType :: Text,
    accountId :: Text,
    imageId :: Maybe Text,
    kernelId :: Maybe Text,
    ramdiskId :: Maybe Text,
    architecture :: Maybe Text,
    pendingTime :: Maybe ISO8601
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON IdentityDocument where
  parseJSON = withObject "dynamic/instance-identity/document" $ \o -> do
    devpayProductCodes <- o .:? "devpayProductCodes"
    billingProducts <- o .:? "billingProducts"
    privateIp <- o .:? "privateIp"
    version <- o .:? "version"
    availabilityZone <- o .: "availabilityZone"
    region <- o .: "region"
    instanceId <- o .: "instanceId"
    instanceType <- o .: "instanceType"
    accountId <- o .: "accountId"
    imageId <- o .:? "imageId"
    kernelId <- o .:? "kernelId"
    ramdiskId <- o .:? "ramdiskId"
    architecture <- o .:? "architecture"
    pendingTime <- o .:? "pendingTime"
    pure IdentityDocument {..}

instance ToJSON IdentityDocument where
  toJSON IdentityDocument {..} =
    object
      [ "devpayProductCodes" .= devpayProductCodes,
        "billingProducts" .= billingProducts,
        "privateIp" .= privateIp,
        "version" .= version,
        "availabilityZone" .= availabilityZone,
        "region" .= region,
        "instanceId" .= instanceId,
        "instanceType" .= instanceType,
        "accountId" .= accountId,
        "imageId" .= imageId,
        "kernelId" .= kernelId,
        "ramdiskId" .= ramdiskId,
        "architecture" .= architecture
      ]

{-# INLINE identityDocument_devpayProductCodes #-}
identityDocument_devpayProductCodes :: Lens' IdentityDocument (Maybe [Text])
identityDocument_devpayProductCodes f i@IdentityDocument {devpayProductCodes} = f devpayProductCodes <&> \devpayProductCodes' -> i {devpayProductCodes = devpayProductCodes'}

{-# INLINE identityDocument_billingProducts #-}
identityDocument_billingProducts :: Lens' IdentityDocument (Maybe [Text])
identityDocument_billingProducts f i@IdentityDocument {billingProducts} = f billingProducts <&> \billingProducts' -> i {billingProducts = billingProducts'}

{-# INLINE identityDocument_version #-}
identityDocument_version :: Lens' IdentityDocument (Maybe Text)
identityDocument_version f i@IdentityDocument {version} = f version <&> \version' -> i {version = version'}

{-# INLINE identityDocument_privateIp #-}
identityDocument_privateIp :: Lens' IdentityDocument (Maybe Text)
identityDocument_privateIp f i@IdentityDocument {privateIp} = f privateIp <&> \privateIp' -> i {privateIp = privateIp'}

{-# INLINE identityDocument_availabilityZone #-}
identityDocument_availabilityZone :: Lens' IdentityDocument Text
identityDocument_availabilityZone f i@IdentityDocument {availabilityZone} = f availabilityZone <&> \availabilityZone' -> i {availabilityZone = availabilityZone'}

{-# INLINE identityDocument_region #-}
identityDocument_region :: Lens' IdentityDocument Region
identityDocument_region f i@IdentityDocument {region} = f region <&> \region' -> i {region = region'}

{-# INLINE identityDocument_instanceId #-}
identityDocument_instanceId :: Lens' IdentityDocument Text
identityDocument_instanceId f i@IdentityDocument {instanceId} = f instanceId <&> \instanceId' -> i {instanceId = instanceId'}

{-# INLINE identityDocument_instanceType #-}
identityDocument_instanceType :: Lens' IdentityDocument Text
identityDocument_instanceType f i@IdentityDocument {instanceType} = f instanceType <&> \instanceType' -> i {instanceType = instanceType'}

{-# INLINE identityDocument_accountId #-}
identityDocument_accountId :: Lens' IdentityDocument Text
identityDocument_accountId f i@IdentityDocument {accountId} = f accountId <&> \accountId' -> i {accountId = accountId'}

{-# INLINE identityDocument_imageId #-}
identityDocument_imageId :: Lens' IdentityDocument (Maybe Text)
identityDocument_imageId f i@IdentityDocument {imageId} = f imageId <&> \imageId' -> i {imageId = imageId'}

{-# INLINE identityDocument_kernelId #-}
identityDocument_kernelId :: Lens' IdentityDocument (Maybe Text)
identityDocument_kernelId f i@IdentityDocument {kernelId} = f kernelId <&> \kernelId' -> i {kernelId = kernelId'}

{-# INLINE identityDocument_ramdiskId #-}
identityDocument_ramdiskId :: Lens' IdentityDocument (Maybe Text)
identityDocument_ramdiskId f i@IdentityDocument {ramdiskId} = f ramdiskId <&> \ramdiskId' -> i {ramdiskId = ramdiskId'}

{-# INLINE identityDocument_architecture #-}
identityDocument_architecture :: Lens' IdentityDocument (Maybe Text)
identityDocument_architecture f i@IdentityDocument {architecture} = f architecture <&> \architecture' -> i {architecture = architecture'}

{-# INLINE identityDocument_pendingTime #-}
identityDocument_pendingTime :: Lens' IdentityDocument (Maybe ISO8601)
identityDocument_pendingTime f i@IdentityDocument {pendingTime} = f pendingTime <&> \pendingTime' -> i {pendingTime = pendingTime'}

-- | Retrieve the instance's identity document, detailing various EC2 metadata.
--
-- You can alternatively retrieve the raw unparsed identity document by using
-- 'dynamic' and the 'Document' path.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-identity-documents.html AWS Instance Identity Documents>.
identity ::
  MonadIO m =>
  Client.Manager ->
  m (Either String IdentityDocument)
identity m = eitherDecode . LBS.fromStrict <$> dynamic m Document

get :: MonadIO m => Client.Manager -> Text -> m ByteString
get m url = liftIO $ do
  token <- strip <$> requestToken
  strip <$> requestWith (addToken token) m url
  where
    requestToken =
      requestWith
        ( setRequestMethod "PUT"
            . setRequestHeader "X-aws-ec2-metadata-token-ttl-seconds" ["60"]
        )
        m
        (latest <> "api/token")

    addToken token = setRequestHeader "X-aws-ec2-metadata-token" [token]

    strip bs
      | BS8.isSuffixOf "\n" bs = BS8.init bs
      | otherwise = bs

requestWith ::
  (Client.Request -> Client.Request) ->
  Client.Manager ->
  Text ->
  IO ByteString
requestWith modifyRequest m url = do
  rq <- Client.parseUrlThrow (Text.unpack url)
  rs <- Client.httpLbs (modifyRequest rq) m

  return . LBS.toStrict $ Client.responseBody rs
