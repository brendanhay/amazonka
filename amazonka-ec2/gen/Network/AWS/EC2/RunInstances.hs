{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RunInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches the specified number of instances using an AMI for which you have permissions.
--
--
-- You can specify a number of options, or leave the default options. The following rules apply:
--
--     * [EC2-VPC] If you don't specify a subnet ID, we choose a default subnet from your default VPC for you. If you don't have a default VPC, you must specify a subnet ID in the request.
--
--     * [EC2-Classic] If don't specify an Availability Zone, we choose one for you.
--
--     * Some instance types must be launched into a VPC. If you do not have a default VPC, or if you do not specify a subnet ID, the request fails. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-vpc.html#vpc-only-instance-types Instance Types Available Only in a VPC> .
--
--     * [EC2-VPC] All instances have a network interface with a primary private IPv4 address. If you don't specify this address, we choose one from the IPv4 range of your subnet.
--
--     * Not all instance types support IPv6 addresses. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> .
--
--     * If you don't specify a security group ID, we use the default security group. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Security Groups> .
--
--     * If any of the AMIs have a product code attached for which the user has not subscribed, the request fails.
--
--
--
-- You can create a <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html launch template> , which is a resource that contains the parameters to launch an instance. When you launch an instance using 'RunInstances' , you can specify the launch template instead of specifying the launch parameters.
--
-- To ensure faster instance launches, break up large requests into smaller batches. For example, create five separate launch requests for 100 instances each instead of one launch request for 500 instances.
--
-- An instance is ready for you to use when it's in the @running@ state. You can check the state of your instance using 'DescribeInstances' . You can tag instances and EBS volumes during launch, after launch, or both. For more information, see 'CreateTags' and <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Amazon EC2 Resources> .
--
-- Linux instances have access to the public key of the key pair at boot. You can use this key to provide secure access to the instance. Amazon EC2 public images use this feature to provide secure access without passwords. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- For troubleshooting, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_InstanceStraightToTerminated.html What To Do If An Instance Immediately Terminates> , and <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesConnecting.html Troubleshooting Connecting to Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.RunInstances
    (
    -- * Creating a Request
      runInstances
    , RunInstances
    -- * Request Lenses
    , risAdditionalInfo
    , risSecurityGroupIds
    , risSecurityGroups
    , risClientToken
    , risInstanceMarketOptions
    , risDisableAPITermination
    , risKeyName
    , risNetworkInterfaces
    , risRAMDiskId
    , risCPUOptions
    , risSubnetId
    , risKernelId
    , risInstanceType
    , risEBSOptimized
    , risUserData
    , risMonitoring
    , risTagSpecifications
    , risIPv6AddressCount
    , risIAMInstanceProfile
    , risElasticGpuSpecification
    , risImageId
    , risPrivateIPAddress
    , risInstanceInitiatedShutdownBehavior
    , risLaunchTemplate
    , risCreditSpecification
    , risBlockDeviceMappings
    , risDryRun
    , risPlacement
    , risIPv6Addresses
    , risMaxCount
    , risMinCount

    -- * Destructuring the Response
    , reservation
    , Reservation
    -- * Response Lenses
    , rGroups
    , rInstances
    , rRequesterId
    , rReservationId
    , rOwnerId
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for RunInstances.
--
--
--
-- /See:/ 'runInstances' smart constructor.
data RunInstances = RunInstances'
  { _risAdditionalInfo :: !(Maybe Text)
  , _risSecurityGroupIds :: !(Maybe [Text])
  , _risSecurityGroups :: !(Maybe [Text])
  , _risClientToken :: !(Maybe Text)
  , _risInstanceMarketOptions :: !(Maybe InstanceMarketOptionsRequest)
  , _risDisableAPITermination :: !(Maybe Bool)
  , _risKeyName :: !(Maybe Text)
  , _risNetworkInterfaces :: !(Maybe [InstanceNetworkInterfaceSpecification])
  , _risRAMDiskId :: !(Maybe Text)
  , _risCPUOptions :: !(Maybe CPUOptionsRequest)
  , _risSubnetId :: !(Maybe Text)
  , _risKernelId :: !(Maybe Text)
  , _risInstanceType :: !(Maybe InstanceType)
  , _risEBSOptimized :: !(Maybe Bool)
  , _risUserData :: !(Maybe Text)
  , _risMonitoring :: !(Maybe RunInstancesMonitoringEnabled)
  , _risTagSpecifications :: !(Maybe [TagSpecification])
  , _risIPv6AddressCount :: !(Maybe Int)
  , _risIAMInstanceProfile :: !(Maybe IAMInstanceProfileSpecification)
  , _risElasticGpuSpecification :: !(Maybe [ElasticGpuSpecification])
  , _risImageId :: !(Maybe Text)
  , _risPrivateIPAddress :: !(Maybe Text)
  , _risInstanceInitiatedShutdownBehavior :: !(Maybe ShutdownBehavior)
  , _risLaunchTemplate :: !(Maybe LaunchTemplateSpecification)
  , _risCreditSpecification :: !(Maybe CreditSpecificationRequest)
  , _risBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
  , _risDryRun :: !(Maybe Bool)
  , _risPlacement :: !(Maybe Placement)
  , _risIPv6Addresses :: !(Maybe [InstanceIPv6Address])
  , _risMaxCount :: !Int
  , _risMinCount :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RunInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'risAdditionalInfo' - Reserved.
--
-- * 'risSecurityGroupIds' - One or more security group IDs. You can create a security group using 'CreateSecurityGroup' . Default: Amazon EC2 uses the default security group.
--
-- * 'risSecurityGroups' - [EC2-Classic, default VPC] One or more security group names. For a nondefault VPC, you must use security group IDs instead. Default: Amazon EC2 uses the default security group.
--
-- * 'risClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> . Constraints: Maximum 64 ASCII characters
--
-- * 'risInstanceMarketOptions' - The market (purchasing) option for the instances.
--
-- * 'risDisableAPITermination' - If you set this parameter to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. To change this attribute to @false@ after launch, use 'ModifyInstanceAttribute' . Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to @terminate@ , you can terminate the instance by running the shutdown command from the instance. Default: @false@
--
-- * 'risKeyName' - The name of the key pair. You can create a key pair using 'CreateKeyPair' or 'ImportKeyPair' . /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
--
-- * 'risNetworkInterfaces' - One or more network interfaces.
--
-- * 'risRAMDiskId' - The ID of the RAM disk. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'risCPUOptions' - The CPU options for the instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'risSubnetId' - [EC2-VPC] The ID of the subnet to launch the instance into.
--
-- * 'risKernelId' - The ID of the kernel. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'risInstanceType' - The instance type. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ . Default: @m1.small@
--
-- * 'risEBSOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal Amazon EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance. Default: @false@
--
-- * 'risUserData' - The user data to make available to the instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch> (Linux) and <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows). If you are using a command line tool, base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide base64-encoded text.
--
-- * 'risMonitoring' - The monitoring for the instance.
--
-- * 'risTagSpecifications' - The tags to apply to the resources during launch. You can tag instances and volumes. The specified tags are applied to all instances or volumes that are created during launch.
--
-- * 'risIPv6AddressCount' - [EC2-VPC] A number of IPv6 addresses to associate with the primary network interface. Amazon EC2 chooses the IPv6 addresses from the range of your subnet. You cannot specify this option and the option to assign specific IPv6 addresses in the same request. You can specify this option if you've specified a minimum number of instances to launch.
--
-- * 'risIAMInstanceProfile' - The IAM instance profile.
--
-- * 'risElasticGpuSpecification' - An elastic GPU to associate with the instance.
--
-- * 'risImageId' - The ID of the AMI, which you can get by calling 'DescribeImages' . An AMI is required to launch an instance and must be specified here or in a launch template.
--
-- * 'risPrivateIPAddress' - [EC2-VPC] The primary IPv4 address. You must specify a value from the IPv4 address range of the subnet. Only one private IP address can be designated as primary. You can't specify this option if you've specified the option to designate a private IP address as the primary IP address in a network interface specification. You cannot specify this option if you're launching more than one instance in the request.
--
-- * 'risInstanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown). Default: @stop@
--
-- * 'risLaunchTemplate' - The launch template to use to launch the instances. Any parameters that you specify in 'RunInstances' override the same parameters in the launch template. You can specify either the name or ID of a launch template, but not both.
--
-- * 'risCreditSpecification' - The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ . To change this attribute after launch, use 'ModifyInstanceCreditSpecification' . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/t2-instances.html T2 Instances> in the /Amazon Elastic Compute Cloud User Guide/ . Default: @standard@
--
-- * 'risBlockDeviceMappings' - One or more block device mapping entries. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
--
-- * 'risDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'risPlacement' - The placement for the instance.
--
-- * 'risIPv6Addresses' - [EC2-VPC] Specify one or more IPv6 addresses from the range of the subnet to associate with the primary network interface. You cannot specify this option and the option to assign a number of IPv6 addresses in the same request. You cannot specify this option if you've specified a minimum number of instances to launch.
--
-- * 'risMaxCount' - The maximum number of instances to launch. If you specify more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches the largest possible number of instances above @MinCount@ . Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 FAQ.
--
-- * 'risMinCount' - The minimum number of instances to launch. If you specify a minimum that is more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches no instances. Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 General FAQ.
runInstances
    :: Int -- ^ 'risMaxCount'
    -> Int -- ^ 'risMinCount'
    -> RunInstances
runInstances pMaxCount_ pMinCount_ =
  RunInstances'
    { _risAdditionalInfo = Nothing
    , _risSecurityGroupIds = Nothing
    , _risSecurityGroups = Nothing
    , _risClientToken = Nothing
    , _risInstanceMarketOptions = Nothing
    , _risDisableAPITermination = Nothing
    , _risKeyName = Nothing
    , _risNetworkInterfaces = Nothing
    , _risRAMDiskId = Nothing
    , _risCPUOptions = Nothing
    , _risSubnetId = Nothing
    , _risKernelId = Nothing
    , _risInstanceType = Nothing
    , _risEBSOptimized = Nothing
    , _risUserData = Nothing
    , _risMonitoring = Nothing
    , _risTagSpecifications = Nothing
    , _risIPv6AddressCount = Nothing
    , _risIAMInstanceProfile = Nothing
    , _risElasticGpuSpecification = Nothing
    , _risImageId = Nothing
    , _risPrivateIPAddress = Nothing
    , _risInstanceInitiatedShutdownBehavior = Nothing
    , _risLaunchTemplate = Nothing
    , _risCreditSpecification = Nothing
    , _risBlockDeviceMappings = Nothing
    , _risDryRun = Nothing
    , _risPlacement = Nothing
    , _risIPv6Addresses = Nothing
    , _risMaxCount = pMaxCount_
    , _risMinCount = pMinCount_
    }


-- | Reserved.
risAdditionalInfo :: Lens' RunInstances (Maybe Text)
risAdditionalInfo = lens _risAdditionalInfo (\ s a -> s{_risAdditionalInfo = a})

-- | One or more security group IDs. You can create a security group using 'CreateSecurityGroup' . Default: Amazon EC2 uses the default security group.
risSecurityGroupIds :: Lens' RunInstances [Text]
risSecurityGroupIds = lens _risSecurityGroupIds (\ s a -> s{_risSecurityGroupIds = a}) . _Default . _Coerce

-- | [EC2-Classic, default VPC] One or more security group names. For a nondefault VPC, you must use security group IDs instead. Default: Amazon EC2 uses the default security group.
risSecurityGroups :: Lens' RunInstances [Text]
risSecurityGroups = lens _risSecurityGroups (\ s a -> s{_risSecurityGroups = a}) . _Default . _Coerce

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> . Constraints: Maximum 64 ASCII characters
risClientToken :: Lens' RunInstances (Maybe Text)
risClientToken = lens _risClientToken (\ s a -> s{_risClientToken = a})

-- | The market (purchasing) option for the instances.
risInstanceMarketOptions :: Lens' RunInstances (Maybe InstanceMarketOptionsRequest)
risInstanceMarketOptions = lens _risInstanceMarketOptions (\ s a -> s{_risInstanceMarketOptions = a})

-- | If you set this parameter to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. To change this attribute to @false@ after launch, use 'ModifyInstanceAttribute' . Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to @terminate@ , you can terminate the instance by running the shutdown command from the instance. Default: @false@
risDisableAPITermination :: Lens' RunInstances (Maybe Bool)
risDisableAPITermination = lens _risDisableAPITermination (\ s a -> s{_risDisableAPITermination = a})

-- | The name of the key pair. You can create a key pair using 'CreateKeyPair' or 'ImportKeyPair' . /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
risKeyName :: Lens' RunInstances (Maybe Text)
risKeyName = lens _risKeyName (\ s a -> s{_risKeyName = a})

-- | One or more network interfaces.
risNetworkInterfaces :: Lens' RunInstances [InstanceNetworkInterfaceSpecification]
risNetworkInterfaces = lens _risNetworkInterfaces (\ s a -> s{_risNetworkInterfaces = a}) . _Default . _Coerce

-- | The ID of the RAM disk. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
risRAMDiskId :: Lens' RunInstances (Maybe Text)
risRAMDiskId = lens _risRAMDiskId (\ s a -> s{_risRAMDiskId = a})

-- | The CPU options for the instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
risCPUOptions :: Lens' RunInstances (Maybe CPUOptionsRequest)
risCPUOptions = lens _risCPUOptions (\ s a -> s{_risCPUOptions = a})

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
risSubnetId :: Lens' RunInstances (Maybe Text)
risSubnetId = lens _risSubnetId (\ s a -> s{_risSubnetId = a})

-- | The ID of the kernel. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
risKernelId :: Lens' RunInstances (Maybe Text)
risKernelId = lens _risKernelId (\ s a -> s{_risKernelId = a})

-- | The instance type. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ . Default: @m1.small@
risInstanceType :: Lens' RunInstances (Maybe InstanceType)
risInstanceType = lens _risInstanceType (\ s a -> s{_risInstanceType = a})

-- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal Amazon EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance. Default: @false@
risEBSOptimized :: Lens' RunInstances (Maybe Bool)
risEBSOptimized = lens _risEBSOptimized (\ s a -> s{_risEBSOptimized = a})

-- | The user data to make available to the instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch> (Linux) and <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows). If you are using a command line tool, base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide base64-encoded text.
risUserData :: Lens' RunInstances (Maybe Text)
risUserData = lens _risUserData (\ s a -> s{_risUserData = a})

-- | The monitoring for the instance.
risMonitoring :: Lens' RunInstances (Maybe RunInstancesMonitoringEnabled)
risMonitoring = lens _risMonitoring (\ s a -> s{_risMonitoring = a})

-- | The tags to apply to the resources during launch. You can tag instances and volumes. The specified tags are applied to all instances or volumes that are created during launch.
risTagSpecifications :: Lens' RunInstances [TagSpecification]
risTagSpecifications = lens _risTagSpecifications (\ s a -> s{_risTagSpecifications = a}) . _Default . _Coerce

-- | [EC2-VPC] A number of IPv6 addresses to associate with the primary network interface. Amazon EC2 chooses the IPv6 addresses from the range of your subnet. You cannot specify this option and the option to assign specific IPv6 addresses in the same request. You can specify this option if you've specified a minimum number of instances to launch.
risIPv6AddressCount :: Lens' RunInstances (Maybe Int)
risIPv6AddressCount = lens _risIPv6AddressCount (\ s a -> s{_risIPv6AddressCount = a})

-- | The IAM instance profile.
risIAMInstanceProfile :: Lens' RunInstances (Maybe IAMInstanceProfileSpecification)
risIAMInstanceProfile = lens _risIAMInstanceProfile (\ s a -> s{_risIAMInstanceProfile = a})

-- | An elastic GPU to associate with the instance.
risElasticGpuSpecification :: Lens' RunInstances [ElasticGpuSpecification]
risElasticGpuSpecification = lens _risElasticGpuSpecification (\ s a -> s{_risElasticGpuSpecification = a}) . _Default . _Coerce

-- | The ID of the AMI, which you can get by calling 'DescribeImages' . An AMI is required to launch an instance and must be specified here or in a launch template.
risImageId :: Lens' RunInstances (Maybe Text)
risImageId = lens _risImageId (\ s a -> s{_risImageId = a})

-- | [EC2-VPC] The primary IPv4 address. You must specify a value from the IPv4 address range of the subnet. Only one private IP address can be designated as primary. You can't specify this option if you've specified the option to designate a private IP address as the primary IP address in a network interface specification. You cannot specify this option if you're launching more than one instance in the request.
risPrivateIPAddress :: Lens' RunInstances (Maybe Text)
risPrivateIPAddress = lens _risPrivateIPAddress (\ s a -> s{_risPrivateIPAddress = a})

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown). Default: @stop@
risInstanceInitiatedShutdownBehavior :: Lens' RunInstances (Maybe ShutdownBehavior)
risInstanceInitiatedShutdownBehavior = lens _risInstanceInitiatedShutdownBehavior (\ s a -> s{_risInstanceInitiatedShutdownBehavior = a})

-- | The launch template to use to launch the instances. Any parameters that you specify in 'RunInstances' override the same parameters in the launch template. You can specify either the name or ID of a launch template, but not both.
risLaunchTemplate :: Lens' RunInstances (Maybe LaunchTemplateSpecification)
risLaunchTemplate = lens _risLaunchTemplate (\ s a -> s{_risLaunchTemplate = a})

-- | The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ . To change this attribute after launch, use 'ModifyInstanceCreditSpecification' . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/t2-instances.html T2 Instances> in the /Amazon Elastic Compute Cloud User Guide/ . Default: @standard@
risCreditSpecification :: Lens' RunInstances (Maybe CreditSpecificationRequest)
risCreditSpecification = lens _risCreditSpecification (\ s a -> s{_risCreditSpecification = a})

-- | One or more block device mapping entries. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
risBlockDeviceMappings :: Lens' RunInstances [BlockDeviceMapping]
risBlockDeviceMappings = lens _risBlockDeviceMappings (\ s a -> s{_risBlockDeviceMappings = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
risDryRun :: Lens' RunInstances (Maybe Bool)
risDryRun = lens _risDryRun (\ s a -> s{_risDryRun = a})

-- | The placement for the instance.
risPlacement :: Lens' RunInstances (Maybe Placement)
risPlacement = lens _risPlacement (\ s a -> s{_risPlacement = a})

-- | [EC2-VPC] Specify one or more IPv6 addresses from the range of the subnet to associate with the primary network interface. You cannot specify this option and the option to assign a number of IPv6 addresses in the same request. You cannot specify this option if you've specified a minimum number of instances to launch.
risIPv6Addresses :: Lens' RunInstances [InstanceIPv6Address]
risIPv6Addresses = lens _risIPv6Addresses (\ s a -> s{_risIPv6Addresses = a}) . _Default . _Coerce

-- | The maximum number of instances to launch. If you specify more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches the largest possible number of instances above @MinCount@ . Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 FAQ.
risMaxCount :: Lens' RunInstances Int
risMaxCount = lens _risMaxCount (\ s a -> s{_risMaxCount = a})

-- | The minimum number of instances to launch. If you specify a minimum that is more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches no instances. Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 General FAQ.
risMinCount :: Lens' RunInstances Int
risMinCount = lens _risMinCount (\ s a -> s{_risMinCount = a})

instance AWSRequest RunInstances where
        type Rs RunInstances = Reservation
        request = postQuery ec2
        response = receiveXML (\ s h x -> parseXML x)

instance Hashable RunInstances where

instance NFData RunInstances where

instance ToHeaders RunInstances where
        toHeaders = const mempty

instance ToPath RunInstances where
        toPath = const "/"

instance ToQuery RunInstances where
        toQuery RunInstances'{..}
          = mconcat
              ["Action" =: ("RunInstances" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "AdditionalInfo" =: _risAdditionalInfo,
               toQuery
                 (toQueryList "SecurityGroupId" <$>
                    _risSecurityGroupIds),
               toQuery
                 (toQueryList "SecurityGroup" <$> _risSecurityGroups),
               "ClientToken" =: _risClientToken,
               "InstanceMarketOptions" =: _risInstanceMarketOptions,
               "DisableApiTermination" =: _risDisableAPITermination,
               "KeyName" =: _risKeyName,
               toQuery
                 (toQueryList "NetworkInterface" <$>
                    _risNetworkInterfaces),
               "RamdiskId" =: _risRAMDiskId,
               "CpuOptions" =: _risCPUOptions,
               "SubnetId" =: _risSubnetId,
               "KernelId" =: _risKernelId,
               "InstanceType" =: _risInstanceType,
               "EbsOptimized" =: _risEBSOptimized,
               "UserData" =: _risUserData,
               "Monitoring" =: _risMonitoring,
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _risTagSpecifications),
               "Ipv6AddressCount" =: _risIPv6AddressCount,
               "IamInstanceProfile" =: _risIAMInstanceProfile,
               toQuery
                 (toQueryList "ElasticGpuSpecification" <$>
                    _risElasticGpuSpecification),
               "ImageId" =: _risImageId,
               "PrivateIpAddress" =: _risPrivateIPAddress,
               "InstanceInitiatedShutdownBehavior" =:
                 _risInstanceInitiatedShutdownBehavior,
               "LaunchTemplate" =: _risLaunchTemplate,
               "CreditSpecification" =: _risCreditSpecification,
               toQuery
                 (toQueryList "BlockDeviceMapping" <$>
                    _risBlockDeviceMappings),
               "DryRun" =: _risDryRun, "Placement" =: _risPlacement,
               toQuery
                 (toQueryList "Ipv6Address" <$> _risIPv6Addresses),
               "MaxCount" =: _risMaxCount,
               "MinCount" =: _risMinCount]
