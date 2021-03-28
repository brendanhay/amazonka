{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RunInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches the specified number of instances using an AMI for which you have permissions.
--
-- You can specify a number of options, or leave the default options. The following rules apply:
--
--     * [EC2-VPC] If you don't specify a subnet ID, we choose a default subnet from your default VPC for you. If you don't have a default VPC, you must specify a subnet ID in the request.
--
--
--     * [EC2-Classic] If don't specify an Availability Zone, we choose one for you.
--
--
--     * Some instance types must be launched into a VPC. If you do not have a default VPC, or if you do not specify a subnet ID, the request fails. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-vpc.html#vpc-only-instance-types Instance types available only in a VPC> .
--
--
--     * [EC2-VPC] All instances have a network interface with a primary private IPv4 address. If you don't specify this address, we choose one from the IPv4 range of your subnet.
--
--
--     * Not all instance types support IPv6 addresses. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types> .
--
--
--     * If you don't specify a security group ID, we use the default security group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Security groups> .
--
--
--     * If any of the AMIs have a product code attached for which the user has not subscribed, the request fails.
--
--
-- You can create a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html launch template> , which is a resource that contains the parameters to launch an instance. When you launch an instance using 'RunInstances' , you can specify the launch template instead of specifying the launch parameters.
-- To ensure faster instance launches, break up large requests into smaller batches. For example, create five separate launch requests for 100 instances each instead of one launch request for 500 instances.
-- An instance is ready for you to use when it's in the @running@ state. You can check the state of your instance using 'DescribeInstances' . You can tag instances and EBS volumes during launch, after launch, or both. For more information, see 'CreateTags' and <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources> .
-- Linux instances have access to the public key of the key pair at boot. You can use this key to provide secure access to the instance. Amazon EC2 public images use this feature to provide secure access without passwords. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key pairs> in the /Amazon Elastic Compute Cloud User Guide/ .
-- For troubleshooting, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_InstanceStraightToTerminated.html What to do if an instance immediately terminates> , and <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesConnecting.html Troubleshooting connecting to your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.RunInstances
    (
    -- * Creating a request
      RunInstances (..)
    , mkRunInstances
    -- ** Request lenses
    , risMaxCount
    , risMinCount
    , risAdditionalInfo
    , risBlockDeviceMappings
    , risCapacityReservationSpecification
    , risClientToken
    , risCpuOptions
    , risCreditSpecification
    , risDisableApiTermination
    , risDryRun
    , risEbsOptimized
    , risElasticGpuSpecification
    , risElasticInferenceAccelerators
    , risEnclaveOptions
    , risHibernationOptions
    , risIamInstanceProfile
    , risImageId
    , risInstanceInitiatedShutdownBehavior
    , risInstanceMarketOptions
    , risInstanceType
    , risIpv6AddressCount
    , risIpv6Addresses
    , risKernelId
    , risKeyName
    , risLaunchTemplate
    , risLicenseSpecifications
    , risMetadataOptions
    , risMonitoring
    , risNetworkInterfaces
    , risPlacement
    , risPrivateIpAddress
    , risRamdiskId
    , risSecurityGroupIds
    , risSecurityGroups
    , risSubnetId
    , risTagSpecifications
    , risUserData

     -- * Destructuring the response
    , Types.Reservation (..)
    , Types.mkReservation
    -- ** Response lenses
    , Types.rGroups
    , Types.rInstances
    , Types.rOwnerId
    , Types.rRequesterId
    , Types.rReservationId
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRunInstances' smart constructor.
data RunInstances = RunInstances'
  { maxCount :: Core.Int
    -- ^ The maximum number of instances to launch. If you specify more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches the largest possible number of instances above @MinCount@ .
--
-- Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 FAQ.
  , minCount :: Core.Int
    -- ^ The minimum number of instances to launch. If you specify a minimum that is more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches no instances.
--
-- Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 General FAQ.
  , additionalInfo :: Core.Maybe Core.Text
    -- ^ Reserved.
  , blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping]
    -- ^ The block device mapping entries.
  , capacityReservationSpecification :: Core.Maybe Types.CapacityReservationSpecification
    -- ^ Information about the Capacity Reservation targeting option. If you do not specify this parameter, the instance's Capacity Reservation preference defaults to @open@ , which enables it to run in any open Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier you provide to ensure the idempotency of the request. If you do not specify a client token, a randomly generated token is used for the request to ensure idempotency.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
-- Constraints: Maximum 64 ASCII characters
  , cpuOptions :: Core.Maybe Types.CpuOptionsRequest
    -- ^ The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU options> in the /Amazon Elastic Compute Cloud User Guide/ .
  , creditSpecification :: Core.Maybe Types.CreditSpecificationRequest
    -- ^ The credit option for CPU usage of the burstable performance instance. Valid values are @standard@ and @unlimited@ . To change this attribute after launch, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceCreditSpecification.html ModifyInstanceCreditSpecification> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @standard@ (T2 instances) or @unlimited@ (T3/T3a instances)
  , disableApiTermination :: Core.Maybe Core.Bool
    -- ^ If you set this parameter to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. To change this attribute after launch, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute> . Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to @terminate@ , you can terminate the instance by running the shutdown command from the instance.
--
-- Default: @false@ 
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , ebsOptimized :: Core.Maybe Core.Bool
    -- ^ Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal Amazon EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance.
--
-- Default: @false@ 
  , elasticGpuSpecification :: Core.Maybe [Types.ElasticGpuSpecification]
    -- ^ An elastic GPU to associate with the instance. An Elastic GPU is a GPU resource that you can attach to your Windows instance to accelerate the graphics performance of your applications. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html Amazon EC2 Elastic GPUs> in the /Amazon Elastic Compute Cloud User Guide/ .
  , elasticInferenceAccelerators :: Core.Maybe [Types.ElasticInferenceAccelerator]
    -- ^ An elastic inference accelerator to associate with the instance. Elastic inference accelerators are a resource you can attach to your Amazon EC2 instances to accelerate your Deep Learning (DL) inference workloads.
--
-- You cannot specify accelerators from different generations in the same request.
  , enclaveOptions :: Core.Maybe Types.EnclaveOptionsRequest
    -- ^ Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ .
--
-- You can't enable AWS Nitro Enclaves and hibernation on the same instance.
  , hibernationOptions :: Core.Maybe Types.HibernationOptionsRequest
    -- ^ Indicates whether an instance is enabled for hibernation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You can't enable hibernation and AWS Nitro Enclaves on the same instance.
  , iamInstanceProfile :: Core.Maybe Types.IamInstanceProfileSpecification
    -- ^ The IAM instance profile.
  , imageId :: Core.Maybe Types.ImageId
    -- ^ The ID of the AMI. An AMI ID is required to launch an instance and must be specified here or in a launch template.
  , instanceInitiatedShutdownBehavior :: Core.Maybe Types.ShutdownBehavior
    -- ^ Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- Default: @stop@ 
  , instanceMarketOptions :: Core.Maybe Types.InstanceMarketOptionsRequest
    -- ^ The market (purchasing) option for the instances.
--
-- For 'RunInstances' , persistent Spot Instance requests are only supported when __InstanceInterruptionBehavior__ is set to either @hibernate@ or @stop@ .
  , instanceType :: Core.Maybe Types.InstanceType
    -- ^ The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @m1.small@ 
  , ipv6AddressCount :: Core.Maybe Core.Int
    -- ^ [EC2-VPC] The number of IPv6 addresses to associate with the primary network interface. Amazon EC2 chooses the IPv6 addresses from the range of your subnet. You cannot specify this option and the option to assign specific IPv6 addresses in the same request. You can specify this option if you've specified a minimum number of instances to launch.
--
-- You cannot specify this option and the network interfaces option in the same request.
  , ipv6Addresses :: Core.Maybe [Types.InstanceIpv6Address]
    -- ^ [EC2-VPC] The IPv6 addresses from the range of the subnet to associate with the primary network interface. You cannot specify this option and the option to assign a number of IPv6 addresses in the same request. You cannot specify this option if you've specified a minimum number of instances to launch.
--
-- You cannot specify this option and the network interfaces option in the same request.
  , kernelId :: Core.Maybe Types.KernelId
    -- ^ The ID of the kernel.
--
-- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
  , keyName :: Core.Maybe Types.KeyName
    -- ^ The name of the key pair. You can create a key pair using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair> or <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair> .
--
-- /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
  , launchTemplate :: Core.Maybe Types.LaunchTemplateSpecification
    -- ^ The launch template to use to launch the instances. Any parameters that you specify in 'RunInstances' override the same parameters in the launch template. You can specify either the name or ID of a launch template, but not both.
  , licenseSpecifications :: Core.Maybe [Types.LicenseConfigurationRequest]
    -- ^ The license configurations.
  , metadataOptions :: Core.Maybe Types.InstanceMetadataOptionsRequest
    -- ^ The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> .
  , monitoring :: Core.Maybe Types.RunInstancesMonitoringEnabled
    -- ^ Specifies whether detailed monitoring is enabled for the instance.
  , networkInterfaces :: Core.Maybe [Types.InstanceNetworkInterfaceSpecification]
    -- ^ The network interfaces to associate with the instance. If you specify a network interface, you must specify any security groups and subnets as part of the network interface.
  , placement :: Core.Maybe Types.Placement
    -- ^ The placement for the instance.
  , privateIpAddress :: Core.Maybe Core.Text
    -- ^ [EC2-VPC] The primary IPv4 address. You must specify a value from the IPv4 address range of the subnet.
--
-- Only one private IP address can be designated as primary. You can't specify this option if you've specified the option to designate a private IP address as the primary IP address in a network interface specification. You cannot specify this option if you're launching more than one instance in the request.
-- You cannot specify this option and the network interfaces option in the same request.
  , ramdiskId :: Core.Maybe Types.RamdiskId
    -- ^ The ID of the RAM disk to select. Some kernels require additional drivers at launch. Check the kernel requirements for information about whether you need to specify a RAM disk. To find kernel requirements, go to the AWS Resource Center and search for the kernel ID.
--
-- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
  , securityGroupIds :: Core.Maybe [Types.SecurityGroupId]
    -- ^ The IDs of the security groups. You can create a security group using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup> .
--
-- If you specify a network interface, you must specify any security groups as part of the network interface.
  , securityGroups :: Core.Maybe [Types.SecurityGroupName]
    -- ^ [EC2-Classic, default VPC] The names of the security groups. For a nondefault VPC, you must use security group IDs instead.
--
-- If you specify a network interface, you must specify any security groups as part of the network interface.
-- Default: Amazon EC2 uses the default security group.
  , subnetId :: Core.Maybe Types.SubnetId
    -- ^ [EC2-VPC] The ID of the subnet to launch the instance into.
--
-- If you specify a network interface, you must specify any subnets as part of the network interface.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the resources during launch. You can only tag instances and volumes on launch. The specified tags are applied to all instances or volumes that are created during launch. To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
  , userData :: Core.Maybe Core.Text
    -- ^ The user data to make available to the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running commands on your Linux instance at launch> (Linux) and <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows). If you are using a command line tool, base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide base64-encoded text. User data is limited to 16 KB.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RunInstances' value with any optional fields omitted.
mkRunInstances
    :: Core.Int -- ^ 'maxCount'
    -> Core.Int -- ^ 'minCount'
    -> RunInstances
mkRunInstances maxCount minCount
  = RunInstances'{maxCount, minCount, additionalInfo = Core.Nothing,
                  blockDeviceMappings = Core.Nothing,
                  capacityReservationSpecification = Core.Nothing,
                  clientToken = Core.Nothing, cpuOptions = Core.Nothing,
                  creditSpecification = Core.Nothing,
                  disableApiTermination = Core.Nothing, dryRun = Core.Nothing,
                  ebsOptimized = Core.Nothing,
                  elasticGpuSpecification = Core.Nothing,
                  elasticInferenceAccelerators = Core.Nothing,
                  enclaveOptions = Core.Nothing, hibernationOptions = Core.Nothing,
                  iamInstanceProfile = Core.Nothing, imageId = Core.Nothing,
                  instanceInitiatedShutdownBehavior = Core.Nothing,
                  instanceMarketOptions = Core.Nothing, instanceType = Core.Nothing,
                  ipv6AddressCount = Core.Nothing, ipv6Addresses = Core.Nothing,
                  kernelId = Core.Nothing, keyName = Core.Nothing,
                  launchTemplate = Core.Nothing,
                  licenseSpecifications = Core.Nothing,
                  metadataOptions = Core.Nothing, monitoring = Core.Nothing,
                  networkInterfaces = Core.Nothing, placement = Core.Nothing,
                  privateIpAddress = Core.Nothing, ramdiskId = Core.Nothing,
                  securityGroupIds = Core.Nothing, securityGroups = Core.Nothing,
                  subnetId = Core.Nothing, tagSpecifications = Core.Nothing,
                  userData = Core.Nothing}

-- | The maximum number of instances to launch. If you specify more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches the largest possible number of instances above @MinCount@ .
--
-- Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 FAQ.
--
-- /Note:/ Consider using 'maxCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risMaxCount :: Lens.Lens' RunInstances Core.Int
risMaxCount = Lens.field @"maxCount"
{-# INLINEABLE risMaxCount #-}
{-# DEPRECATED maxCount "Use generic-lens or generic-optics with 'maxCount' instead"  #-}

-- | The minimum number of instances to launch. If you specify a minimum that is more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches no instances.
--
-- Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 General FAQ.
--
-- /Note:/ Consider using 'minCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risMinCount :: Lens.Lens' RunInstances Core.Int
risMinCount = Lens.field @"minCount"
{-# INLINEABLE risMinCount #-}
{-# DEPRECATED minCount "Use generic-lens or generic-optics with 'minCount' instead"  #-}

-- | Reserved.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risAdditionalInfo :: Lens.Lens' RunInstances (Core.Maybe Core.Text)
risAdditionalInfo = Lens.field @"additionalInfo"
{-# INLINEABLE risAdditionalInfo #-}
{-# DEPRECATED additionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead"  #-}

-- | The block device mapping entries.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risBlockDeviceMappings :: Lens.Lens' RunInstances (Core.Maybe [Types.BlockDeviceMapping])
risBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# INLINEABLE risBlockDeviceMappings #-}
{-# DEPRECATED blockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead"  #-}

-- | Information about the Capacity Reservation targeting option. If you do not specify this parameter, the instance's Capacity Reservation preference defaults to @open@ , which enables it to run in any open Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
--
-- /Note:/ Consider using 'capacityReservationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risCapacityReservationSpecification :: Lens.Lens' RunInstances (Core.Maybe Types.CapacityReservationSpecification)
risCapacityReservationSpecification = Lens.field @"capacityReservationSpecification"
{-# INLINEABLE risCapacityReservationSpecification #-}
{-# DEPRECATED capacityReservationSpecification "Use generic-lens or generic-optics with 'capacityReservationSpecification' instead"  #-}

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. If you do not specify a client token, a randomly generated token is used for the request to ensure idempotency.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
-- Constraints: Maximum 64 ASCII characters
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risClientToken :: Lens.Lens' RunInstances (Core.Maybe Core.Text)
risClientToken = Lens.field @"clientToken"
{-# INLINEABLE risClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU options> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'cpuOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risCpuOptions :: Lens.Lens' RunInstances (Core.Maybe Types.CpuOptionsRequest)
risCpuOptions = Lens.field @"cpuOptions"
{-# INLINEABLE risCpuOptions #-}
{-# DEPRECATED cpuOptions "Use generic-lens or generic-optics with 'cpuOptions' instead"  #-}

-- | The credit option for CPU usage of the burstable performance instance. Valid values are @standard@ and @unlimited@ . To change this attribute after launch, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceCreditSpecification.html ModifyInstanceCreditSpecification> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @standard@ (T2 instances) or @unlimited@ (T3/T3a instances)
--
-- /Note:/ Consider using 'creditSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risCreditSpecification :: Lens.Lens' RunInstances (Core.Maybe Types.CreditSpecificationRequest)
risCreditSpecification = Lens.field @"creditSpecification"
{-# INLINEABLE risCreditSpecification #-}
{-# DEPRECATED creditSpecification "Use generic-lens or generic-optics with 'creditSpecification' instead"  #-}

-- | If you set this parameter to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. To change this attribute after launch, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute> . Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to @terminate@ , you can terminate the instance by running the shutdown command from the instance.
--
-- Default: @false@ 
--
-- /Note:/ Consider using 'disableApiTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risDisableApiTermination :: Lens.Lens' RunInstances (Core.Maybe Core.Bool)
risDisableApiTermination = Lens.field @"disableApiTermination"
{-# INLINEABLE risDisableApiTermination #-}
{-# DEPRECATED disableApiTermination "Use generic-lens or generic-optics with 'disableApiTermination' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risDryRun :: Lens.Lens' RunInstances (Core.Maybe Core.Bool)
risDryRun = Lens.field @"dryRun"
{-# INLINEABLE risDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal Amazon EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance.
--
-- Default: @false@ 
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risEbsOptimized :: Lens.Lens' RunInstances (Core.Maybe Core.Bool)
risEbsOptimized = Lens.field @"ebsOptimized"
{-# INLINEABLE risEbsOptimized #-}
{-# DEPRECATED ebsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead"  #-}

-- | An elastic GPU to associate with the instance. An Elastic GPU is a GPU resource that you can attach to your Windows instance to accelerate the graphics performance of your applications. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html Amazon EC2 Elastic GPUs> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'elasticGpuSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risElasticGpuSpecification :: Lens.Lens' RunInstances (Core.Maybe [Types.ElasticGpuSpecification])
risElasticGpuSpecification = Lens.field @"elasticGpuSpecification"
{-# INLINEABLE risElasticGpuSpecification #-}
{-# DEPRECATED elasticGpuSpecification "Use generic-lens or generic-optics with 'elasticGpuSpecification' instead"  #-}

-- | An elastic inference accelerator to associate with the instance. Elastic inference accelerators are a resource you can attach to your Amazon EC2 instances to accelerate your Deep Learning (DL) inference workloads.
--
-- You cannot specify accelerators from different generations in the same request.
--
-- /Note:/ Consider using 'elasticInferenceAccelerators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risElasticInferenceAccelerators :: Lens.Lens' RunInstances (Core.Maybe [Types.ElasticInferenceAccelerator])
risElasticInferenceAccelerators = Lens.field @"elasticInferenceAccelerators"
{-# INLINEABLE risElasticInferenceAccelerators #-}
{-# DEPRECATED elasticInferenceAccelerators "Use generic-lens or generic-optics with 'elasticInferenceAccelerators' instead"  #-}

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ .
--
-- You can't enable AWS Nitro Enclaves and hibernation on the same instance.
--
-- /Note:/ Consider using 'enclaveOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risEnclaveOptions :: Lens.Lens' RunInstances (Core.Maybe Types.EnclaveOptionsRequest)
risEnclaveOptions = Lens.field @"enclaveOptions"
{-# INLINEABLE risEnclaveOptions #-}
{-# DEPRECATED enclaveOptions "Use generic-lens or generic-optics with 'enclaveOptions' instead"  #-}

-- | Indicates whether an instance is enabled for hibernation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You can't enable hibernation and AWS Nitro Enclaves on the same instance.
--
-- /Note:/ Consider using 'hibernationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risHibernationOptions :: Lens.Lens' RunInstances (Core.Maybe Types.HibernationOptionsRequest)
risHibernationOptions = Lens.field @"hibernationOptions"
{-# INLINEABLE risHibernationOptions #-}
{-# DEPRECATED hibernationOptions "Use generic-lens or generic-optics with 'hibernationOptions' instead"  #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risIamInstanceProfile :: Lens.Lens' RunInstances (Core.Maybe Types.IamInstanceProfileSpecification)
risIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# INLINEABLE risIamInstanceProfile #-}
{-# DEPRECATED iamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead"  #-}

-- | The ID of the AMI. An AMI ID is required to launch an instance and must be specified here or in a launch template.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risImageId :: Lens.Lens' RunInstances (Core.Maybe Types.ImageId)
risImageId = Lens.field @"imageId"
{-# INLINEABLE risImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- Default: @stop@ 
--
-- /Note:/ Consider using 'instanceInitiatedShutdownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risInstanceInitiatedShutdownBehavior :: Lens.Lens' RunInstances (Core.Maybe Types.ShutdownBehavior)
risInstanceInitiatedShutdownBehavior = Lens.field @"instanceInitiatedShutdownBehavior"
{-# INLINEABLE risInstanceInitiatedShutdownBehavior #-}
{-# DEPRECATED instanceInitiatedShutdownBehavior "Use generic-lens or generic-optics with 'instanceInitiatedShutdownBehavior' instead"  #-}

-- | The market (purchasing) option for the instances.
--
-- For 'RunInstances' , persistent Spot Instance requests are only supported when __InstanceInterruptionBehavior__ is set to either @hibernate@ or @stop@ .
--
-- /Note:/ Consider using 'instanceMarketOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risInstanceMarketOptions :: Lens.Lens' RunInstances (Core.Maybe Types.InstanceMarketOptionsRequest)
risInstanceMarketOptions = Lens.field @"instanceMarketOptions"
{-# INLINEABLE risInstanceMarketOptions #-}
{-# DEPRECATED instanceMarketOptions "Use generic-lens or generic-optics with 'instanceMarketOptions' instead"  #-}

-- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @m1.small@ 
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risInstanceType :: Lens.Lens' RunInstances (Core.Maybe Types.InstanceType)
risInstanceType = Lens.field @"instanceType"
{-# INLINEABLE risInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | [EC2-VPC] The number of IPv6 addresses to associate with the primary network interface. Amazon EC2 chooses the IPv6 addresses from the range of your subnet. You cannot specify this option and the option to assign specific IPv6 addresses in the same request. You can specify this option if you've specified a minimum number of instances to launch.
--
-- You cannot specify this option and the network interfaces option in the same request.
--
-- /Note:/ Consider using 'ipv6AddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risIpv6AddressCount :: Lens.Lens' RunInstances (Core.Maybe Core.Int)
risIpv6AddressCount = Lens.field @"ipv6AddressCount"
{-# INLINEABLE risIpv6AddressCount #-}
{-# DEPRECATED ipv6AddressCount "Use generic-lens or generic-optics with 'ipv6AddressCount' instead"  #-}

-- | [EC2-VPC] The IPv6 addresses from the range of the subnet to associate with the primary network interface. You cannot specify this option and the option to assign a number of IPv6 addresses in the same request. You cannot specify this option if you've specified a minimum number of instances to launch.
--
-- You cannot specify this option and the network interfaces option in the same request.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risIpv6Addresses :: Lens.Lens' RunInstances (Core.Maybe [Types.InstanceIpv6Address])
risIpv6Addresses = Lens.field @"ipv6Addresses"
{-# INLINEABLE risIpv6Addresses #-}
{-# DEPRECATED ipv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead"  #-}

-- | The ID of the kernel.
--
-- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risKernelId :: Lens.Lens' RunInstances (Core.Maybe Types.KernelId)
risKernelId = Lens.field @"kernelId"
{-# INLINEABLE risKernelId #-}
{-# DEPRECATED kernelId "Use generic-lens or generic-optics with 'kernelId' instead"  #-}

-- | The name of the key pair. You can create a key pair using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair> or <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair> .
--
-- /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risKeyName :: Lens.Lens' RunInstances (Core.Maybe Types.KeyName)
risKeyName = Lens.field @"keyName"
{-# INLINEABLE risKeyName #-}
{-# DEPRECATED keyName "Use generic-lens or generic-optics with 'keyName' instead"  #-}

-- | The launch template to use to launch the instances. Any parameters that you specify in 'RunInstances' override the same parameters in the launch template. You can specify either the name or ID of a launch template, but not both.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risLaunchTemplate :: Lens.Lens' RunInstances (Core.Maybe Types.LaunchTemplateSpecification)
risLaunchTemplate = Lens.field @"launchTemplate"
{-# INLINEABLE risLaunchTemplate #-}
{-# DEPRECATED launchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead"  #-}

-- | The license configurations.
--
-- /Note:/ Consider using 'licenseSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risLicenseSpecifications :: Lens.Lens' RunInstances (Core.Maybe [Types.LicenseConfigurationRequest])
risLicenseSpecifications = Lens.field @"licenseSpecifications"
{-# INLINEABLE risLicenseSpecifications #-}
{-# DEPRECATED licenseSpecifications "Use generic-lens or generic-optics with 'licenseSpecifications' instead"  #-}

-- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> .
--
-- /Note:/ Consider using 'metadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risMetadataOptions :: Lens.Lens' RunInstances (Core.Maybe Types.InstanceMetadataOptionsRequest)
risMetadataOptions = Lens.field @"metadataOptions"
{-# INLINEABLE risMetadataOptions #-}
{-# DEPRECATED metadataOptions "Use generic-lens or generic-optics with 'metadataOptions' instead"  #-}

-- | Specifies whether detailed monitoring is enabled for the instance.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risMonitoring :: Lens.Lens' RunInstances (Core.Maybe Types.RunInstancesMonitoringEnabled)
risMonitoring = Lens.field @"monitoring"
{-# INLINEABLE risMonitoring #-}
{-# DEPRECATED monitoring "Use generic-lens or generic-optics with 'monitoring' instead"  #-}

-- | The network interfaces to associate with the instance. If you specify a network interface, you must specify any security groups and subnets as part of the network interface.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risNetworkInterfaces :: Lens.Lens' RunInstances (Core.Maybe [Types.InstanceNetworkInterfaceSpecification])
risNetworkInterfaces = Lens.field @"networkInterfaces"
{-# INLINEABLE risNetworkInterfaces #-}
{-# DEPRECATED networkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead"  #-}

-- | The placement for the instance.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risPlacement :: Lens.Lens' RunInstances (Core.Maybe Types.Placement)
risPlacement = Lens.field @"placement"
{-# INLINEABLE risPlacement #-}
{-# DEPRECATED placement "Use generic-lens or generic-optics with 'placement' instead"  #-}

-- | [EC2-VPC] The primary IPv4 address. You must specify a value from the IPv4 address range of the subnet.
--
-- Only one private IP address can be designated as primary. You can't specify this option if you've specified the option to designate a private IP address as the primary IP address in a network interface specification. You cannot specify this option if you're launching more than one instance in the request.
-- You cannot specify this option and the network interfaces option in the same request.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risPrivateIpAddress :: Lens.Lens' RunInstances (Core.Maybe Core.Text)
risPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE risPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

-- | The ID of the RAM disk to select. Some kernels require additional drivers at launch. Check the kernel requirements for information about whether you need to specify a RAM disk. To find kernel requirements, go to the AWS Resource Center and search for the kernel ID.
--
-- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risRamdiskId :: Lens.Lens' RunInstances (Core.Maybe Types.RamdiskId)
risRamdiskId = Lens.field @"ramdiskId"
{-# INLINEABLE risRamdiskId #-}
{-# DEPRECATED ramdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead"  #-}

-- | The IDs of the security groups. You can create a security group using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup> .
--
-- If you specify a network interface, you must specify any security groups as part of the network interface.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risSecurityGroupIds :: Lens.Lens' RunInstances (Core.Maybe [Types.SecurityGroupId])
risSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE risSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | [EC2-Classic, default VPC] The names of the security groups. For a nondefault VPC, you must use security group IDs instead.
--
-- If you specify a network interface, you must specify any security groups as part of the network interface.
-- Default: Amazon EC2 uses the default security group.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risSecurityGroups :: Lens.Lens' RunInstances (Core.Maybe [Types.SecurityGroupName])
risSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE risSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
--
-- If you specify a network interface, you must specify any subnets as part of the network interface.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risSubnetId :: Lens.Lens' RunInstances (Core.Maybe Types.SubnetId)
risSubnetId = Lens.field @"subnetId"
{-# INLINEABLE risSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The tags to apply to the resources during launch. You can only tag instances and volumes on launch. The specified tags are applied to all instances or volumes that are created during launch. To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risTagSpecifications :: Lens.Lens' RunInstances (Core.Maybe [Types.TagSpecification])
risTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE risTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

-- | The user data to make available to the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running commands on your Linux instance at launch> (Linux) and <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows). If you are using a command line tool, base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide base64-encoded text. User data is limited to 16 KB.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risUserData :: Lens.Lens' RunInstances (Core.Maybe Core.Text)
risUserData = Lens.field @"userData"
{-# INLINEABLE risUserData #-}
{-# DEPRECATED userData "Use generic-lens or generic-optics with 'userData' instead"  #-}

instance Core.ToQuery RunInstances where
        toQuery RunInstances{..}
          = Core.toQueryPair "Action" ("RunInstances" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "MaxCount" maxCount
              Core.<> Core.toQueryPair "MinCount" minCount
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AdditionalInfo")
                additionalInfo
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "BlockDeviceMapping")
                blockDeviceMappings
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "CapacityReservationSpecification")
                capacityReservationSpecification
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CpuOptions") cpuOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CreditSpecification")
                creditSpecification
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DisableApiTermination")
                disableApiTermination
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EbsOptimized")
                ebsOptimized
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "ElasticGpuSpecification")
                elasticGpuSpecification
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryList "ElasticInferenceAccelerator")
                elasticInferenceAccelerators
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnclaveOptions")
                enclaveOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HibernationOptions")
                hibernationOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IamInstanceProfile")
                iamInstanceProfile
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "ImageId") imageId
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "InstanceInitiatedShutdownBehavior")
                instanceInitiatedShutdownBehavior
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceMarketOptions")
                instanceMarketOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceType")
                instanceType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Ipv6AddressCount")
                ipv6AddressCount
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "Ipv6Address")
                ipv6Addresses
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KernelId") kernelId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "KeyName") keyName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplate")
                launchTemplate
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "LicenseSpecification")
                licenseSpecifications
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MetadataOptions")
                metadataOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Monitoring") monitoring
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "NetworkInterface")
                networkInterfaces
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Placement") placement
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PrivateIpAddress")
                privateIpAddress
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RamdiskId") ramdiskId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "SecurityGroupId")
                securityGroupIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "SecurityGroup")
                securityGroups
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SubnetId") subnetId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserData") userData

instance Core.ToHeaders RunInstances where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RunInstances where
        type Rs RunInstances = Types.Reservation
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveXML (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
