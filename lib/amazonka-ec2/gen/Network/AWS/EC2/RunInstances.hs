{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RunInstances (..),
    mkRunInstances,

    -- ** Request lenses
    risAdditionalInfo,
    risSecurityGroupIds,
    risSecurityGroups,
    risClientToken,
    risElasticInferenceAccelerators,
    risInstanceMarketOptions,
    risLicenseSpecifications,
    risDisableAPITermination,
    risKeyName,
    risNetworkInterfaces,
    risEnclaveOptions,
    risRAMDiskId,
    risCPUOptions,
    risSubnetId,
    risKernelId,
    risInstanceType,
    risCapacityReservationSpecification,
    risEBSOptimized,
    risUserData,
    risMonitoring,
    risTagSpecifications,
    risIPv6AddressCount,
    risHibernationOptions,
    risIAMInstanceProfile,
    risElasticGpuSpecification,
    risImageId,
    risPrivateIPAddress,
    risInstanceInitiatedShutdownBehavior,
    risMetadataOptions,
    risLaunchTemplate,
    risCreditSpecification,
    risBlockDeviceMappings,
    risDryRun,
    risPlacement,
    risIPv6Addresses,
    risMaxCount,
    risMinCount,

    -- * Destructuring the response
    Reservation (..),
    mkReservation,

    -- ** Response lenses
    rGroups,
    rInstances,
    rRequesterId,
    rReservationId,
    rOwnerId,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRunInstances' smart constructor.
data RunInstances = RunInstances'
  { additionalInfo ::
      Lude.Maybe Lude.Text,
    securityGroupIds :: Lude.Maybe [Lude.Text],
    securityGroups :: Lude.Maybe [Lude.Text],
    clientToken :: Lude.Maybe Lude.Text,
    elasticInferenceAccelerators ::
      Lude.Maybe [ElasticInferenceAccelerator],
    instanceMarketOptions :: Lude.Maybe InstanceMarketOptionsRequest,
    licenseSpecifications :: Lude.Maybe [LicenseConfigurationRequest],
    disableAPITermination :: Lude.Maybe Lude.Bool,
    keyName :: Lude.Maybe Lude.Text,
    networkInterfaces ::
      Lude.Maybe [InstanceNetworkInterfaceSpecification],
    enclaveOptions :: Lude.Maybe EnclaveOptionsRequest,
    ramdiskId :: Lude.Maybe Lude.Text,
    cpuOptions :: Lude.Maybe CPUOptionsRequest,
    subnetId :: Lude.Maybe Lude.Text,
    kernelId :: Lude.Maybe Lude.Text,
    instanceType :: Lude.Maybe InstanceType,
    capacityReservationSpecification ::
      Lude.Maybe CapacityReservationSpecification,
    ebsOptimized :: Lude.Maybe Lude.Bool,
    userData :: Lude.Maybe Lude.Text,
    monitoring :: Lude.Maybe RunInstancesMonitoringEnabled,
    tagSpecifications :: Lude.Maybe [TagSpecification],
    ipv6AddressCount :: Lude.Maybe Lude.Int,
    hibernationOptions :: Lude.Maybe HibernationOptionsRequest,
    iamInstanceProfile :: Lude.Maybe IAMInstanceProfileSpecification,
    elasticGpuSpecification :: Lude.Maybe [ElasticGpuSpecification],
    imageId :: Lude.Maybe Lude.Text,
    privateIPAddress :: Lude.Maybe Lude.Text,
    instanceInitiatedShutdownBehavior :: Lude.Maybe ShutdownBehavior,
    metadataOptions :: Lude.Maybe InstanceMetadataOptionsRequest,
    launchTemplate :: Lude.Maybe LaunchTemplateSpecification,
    creditSpecification :: Lude.Maybe CreditSpecificationRequest,
    blockDeviceMappings :: Lude.Maybe [BlockDeviceMapping],
    dryRun :: Lude.Maybe Lude.Bool,
    placement :: Lude.Maybe Placement,
    ipv6Addresses :: Lude.Maybe [InstanceIPv6Address],
    maxCount :: Lude.Int,
    minCount :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RunInstances' with the minimum fields required to make a request.
--
-- * 'additionalInfo' - Reserved.
-- * 'blockDeviceMappings' - The block device mapping entries.
-- * 'capacityReservationSpecification' - Information about the Capacity Reservation targeting option. If you do not specify this parameter, the instance's Capacity Reservation preference defaults to @open@ , which enables it to run in any open Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
-- * 'clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. If you do not specify a client token, a randomly generated token is used for the request to ensure idempotency.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
-- Constraints: Maximum 64 ASCII characters
-- * 'cpuOptions' - The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU options> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'creditSpecification' - The credit option for CPU usage of the burstable performance instance. Valid values are @standard@ and @unlimited@ . To change this attribute after launch, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceCreditSpecification.html ModifyInstanceCreditSpecification> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @standard@ (T2 instances) or @unlimited@ (T3/T3a instances)
-- * 'disableAPITermination' - If you set this parameter to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. To change this attribute after launch, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute> . Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to @terminate@ , you can terminate the instance by running the shutdown command from the instance.
--
-- Default: @false@
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'ebsOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal Amazon EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance.
--
-- Default: @false@
-- * 'elasticGpuSpecification' - An elastic GPU to associate with the instance. An Elastic GPU is a GPU resource that you can attach to your Windows instance to accelerate the graphics performance of your applications. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html Amazon EC2 Elastic GPUs> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'elasticInferenceAccelerators' - An elastic inference accelerator to associate with the instance. Elastic inference accelerators are a resource you can attach to your Amazon EC2 instances to accelerate your Deep Learning (DL) inference workloads.
--
-- You cannot specify accelerators from different generations in the same request.
-- * 'enclaveOptions' - Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ .
--
-- You can't enable AWS Nitro Enclaves and hibernation on the same instance.
-- * 'hibernationOptions' - Indicates whether an instance is enabled for hibernation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You can't enable hibernation and AWS Nitro Enclaves on the same instance.
-- * 'iamInstanceProfile' - The IAM instance profile.
-- * 'imageId' - The ID of the AMI. An AMI ID is required to launch an instance and must be specified here or in a launch template.
-- * 'instanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- Default: @stop@
-- * 'instanceMarketOptions' - The market (purchasing) option for the instances.
--
-- For 'RunInstances' , persistent Spot Instance requests are only supported when __InstanceInterruptionBehavior__ is set to either @hibernate@ or @stop@ .
-- * 'instanceType' - The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @m1.small@
-- * 'ipv6AddressCount' - [EC2-VPC] The number of IPv6 addresses to associate with the primary network interface. Amazon EC2 chooses the IPv6 addresses from the range of your subnet. You cannot specify this option and the option to assign specific IPv6 addresses in the same request. You can specify this option if you've specified a minimum number of instances to launch.
--
-- You cannot specify this option and the network interfaces option in the same request.
-- * 'ipv6Addresses' - [EC2-VPC] The IPv6 addresses from the range of the subnet to associate with the primary network interface. You cannot specify this option and the option to assign a number of IPv6 addresses in the same request. You cannot specify this option if you've specified a minimum number of instances to launch.
--
-- You cannot specify this option and the network interfaces option in the same request.
-- * 'kernelId' - The ID of the kernel.
--
-- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'keyName' - The name of the key pair. You can create a key pair using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair> or <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair> .
--
-- /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
-- * 'launchTemplate' - The launch template to use to launch the instances. Any parameters that you specify in 'RunInstances' override the same parameters in the launch template. You can specify either the name or ID of a launch template, but not both.
-- * 'licenseSpecifications' - The license configurations.
-- * 'maxCount' - The maximum number of instances to launch. If you specify more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches the largest possible number of instances above @MinCount@ .
--
-- Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 FAQ.
-- * 'metadataOptions' - The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> .
-- * 'minCount' - The minimum number of instances to launch. If you specify a minimum that is more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches no instances.
--
-- Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 General FAQ.
-- * 'monitoring' - Specifies whether detailed monitoring is enabled for the instance.
-- * 'networkInterfaces' - The network interfaces to associate with the instance. If you specify a network interface, you must specify any security groups and subnets as part of the network interface.
-- * 'placement' - The placement for the instance.
-- * 'privateIPAddress' - [EC2-VPC] The primary IPv4 address. You must specify a value from the IPv4 address range of the subnet.
--
-- Only one private IP address can be designated as primary. You can't specify this option if you've specified the option to designate a private IP address as the primary IP address in a network interface specification. You cannot specify this option if you're launching more than one instance in the request.
-- You cannot specify this option and the network interfaces option in the same request.
-- * 'ramdiskId' - The ID of the RAM disk to select. Some kernels require additional drivers at launch. Check the kernel requirements for information about whether you need to specify a RAM disk. To find kernel requirements, go to the AWS Resource Center and search for the kernel ID.
--
-- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'securityGroupIds' - The IDs of the security groups. You can create a security group using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup> .
--
-- If you specify a network interface, you must specify any security groups as part of the network interface.
-- * 'securityGroups' - [EC2-Classic, default VPC] The names of the security groups. For a nondefault VPC, you must use security group IDs instead.
--
-- If you specify a network interface, you must specify any security groups as part of the network interface.
-- Default: Amazon EC2 uses the default security group.
-- * 'subnetId' - [EC2-VPC] The ID of the subnet to launch the instance into.
--
-- If you specify a network interface, you must specify any subnets as part of the network interface.
-- * 'tagSpecifications' - The tags to apply to the resources during launch. You can only tag instances and volumes on launch. The specified tags are applied to all instances or volumes that are created during launch. To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
-- * 'userData' - The user data to make available to the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running commands on your Linux instance at launch> (Linux) and <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows). If you are using a command line tool, base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide base64-encoded text. User data is limited to 16 KB.
mkRunInstances ::
  -- | 'maxCount'
  Lude.Int ->
  -- | 'minCount'
  Lude.Int ->
  RunInstances
mkRunInstances pMaxCount_ pMinCount_ =
  RunInstances'
    { additionalInfo = Lude.Nothing,
      securityGroupIds = Lude.Nothing,
      securityGroups = Lude.Nothing,
      clientToken = Lude.Nothing,
      elasticInferenceAccelerators = Lude.Nothing,
      instanceMarketOptions = Lude.Nothing,
      licenseSpecifications = Lude.Nothing,
      disableAPITermination = Lude.Nothing,
      keyName = Lude.Nothing,
      networkInterfaces = Lude.Nothing,
      enclaveOptions = Lude.Nothing,
      ramdiskId = Lude.Nothing,
      cpuOptions = Lude.Nothing,
      subnetId = Lude.Nothing,
      kernelId = Lude.Nothing,
      instanceType = Lude.Nothing,
      capacityReservationSpecification = Lude.Nothing,
      ebsOptimized = Lude.Nothing,
      userData = Lude.Nothing,
      monitoring = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      ipv6AddressCount = Lude.Nothing,
      hibernationOptions = Lude.Nothing,
      iamInstanceProfile = Lude.Nothing,
      elasticGpuSpecification = Lude.Nothing,
      imageId = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      instanceInitiatedShutdownBehavior = Lude.Nothing,
      metadataOptions = Lude.Nothing,
      launchTemplate = Lude.Nothing,
      creditSpecification = Lude.Nothing,
      blockDeviceMappings = Lude.Nothing,
      dryRun = Lude.Nothing,
      placement = Lude.Nothing,
      ipv6Addresses = Lude.Nothing,
      maxCount = pMaxCount_,
      minCount = pMinCount_
    }

-- | Reserved.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risAdditionalInfo :: Lens.Lens' RunInstances (Lude.Maybe Lude.Text)
risAdditionalInfo = Lens.lens (additionalInfo :: RunInstances -> Lude.Maybe Lude.Text) (\s a -> s {additionalInfo = a} :: RunInstances)
{-# DEPRECATED risAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | The IDs of the security groups. You can create a security group using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup> .
--
-- If you specify a network interface, you must specify any security groups as part of the network interface.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risSecurityGroupIds :: Lens.Lens' RunInstances (Lude.Maybe [Lude.Text])
risSecurityGroupIds = Lens.lens (securityGroupIds :: RunInstances -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: RunInstances)
{-# DEPRECATED risSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | [EC2-Classic, default VPC] The names of the security groups. For a nondefault VPC, you must use security group IDs instead.
--
-- If you specify a network interface, you must specify any security groups as part of the network interface.
-- Default: Amazon EC2 uses the default security group.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risSecurityGroups :: Lens.Lens' RunInstances (Lude.Maybe [Lude.Text])
risSecurityGroups = Lens.lens (securityGroups :: RunInstances -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: RunInstances)
{-# DEPRECATED risSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. If you do not specify a client token, a randomly generated token is used for the request to ensure idempotency.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
-- Constraints: Maximum 64 ASCII characters
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risClientToken :: Lens.Lens' RunInstances (Lude.Maybe Lude.Text)
risClientToken = Lens.lens (clientToken :: RunInstances -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: RunInstances)
{-# DEPRECATED risClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | An elastic inference accelerator to associate with the instance. Elastic inference accelerators are a resource you can attach to your Amazon EC2 instances to accelerate your Deep Learning (DL) inference workloads.
--
-- You cannot specify accelerators from different generations in the same request.
--
-- /Note:/ Consider using 'elasticInferenceAccelerators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risElasticInferenceAccelerators :: Lens.Lens' RunInstances (Lude.Maybe [ElasticInferenceAccelerator])
risElasticInferenceAccelerators = Lens.lens (elasticInferenceAccelerators :: RunInstances -> Lude.Maybe [ElasticInferenceAccelerator]) (\s a -> s {elasticInferenceAccelerators = a} :: RunInstances)
{-# DEPRECATED risElasticInferenceAccelerators "Use generic-lens or generic-optics with 'elasticInferenceAccelerators' instead." #-}

-- | The market (purchasing) option for the instances.
--
-- For 'RunInstances' , persistent Spot Instance requests are only supported when __InstanceInterruptionBehavior__ is set to either @hibernate@ or @stop@ .
--
-- /Note:/ Consider using 'instanceMarketOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risInstanceMarketOptions :: Lens.Lens' RunInstances (Lude.Maybe InstanceMarketOptionsRequest)
risInstanceMarketOptions = Lens.lens (instanceMarketOptions :: RunInstances -> Lude.Maybe InstanceMarketOptionsRequest) (\s a -> s {instanceMarketOptions = a} :: RunInstances)
{-# DEPRECATED risInstanceMarketOptions "Use generic-lens or generic-optics with 'instanceMarketOptions' instead." #-}

-- | The license configurations.
--
-- /Note:/ Consider using 'licenseSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risLicenseSpecifications :: Lens.Lens' RunInstances (Lude.Maybe [LicenseConfigurationRequest])
risLicenseSpecifications = Lens.lens (licenseSpecifications :: RunInstances -> Lude.Maybe [LicenseConfigurationRequest]) (\s a -> s {licenseSpecifications = a} :: RunInstances)
{-# DEPRECATED risLicenseSpecifications "Use generic-lens or generic-optics with 'licenseSpecifications' instead." #-}

-- | If you set this parameter to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. To change this attribute after launch, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute> . Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to @terminate@ , you can terminate the instance by running the shutdown command from the instance.
--
-- Default: @false@
--
-- /Note:/ Consider using 'disableAPITermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risDisableAPITermination :: Lens.Lens' RunInstances (Lude.Maybe Lude.Bool)
risDisableAPITermination = Lens.lens (disableAPITermination :: RunInstances -> Lude.Maybe Lude.Bool) (\s a -> s {disableAPITermination = a} :: RunInstances)
{-# DEPRECATED risDisableAPITermination "Use generic-lens or generic-optics with 'disableAPITermination' instead." #-}

-- | The name of the key pair. You can create a key pair using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair> or <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair> .
--
-- /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risKeyName :: Lens.Lens' RunInstances (Lude.Maybe Lude.Text)
risKeyName = Lens.lens (keyName :: RunInstances -> Lude.Maybe Lude.Text) (\s a -> s {keyName = a} :: RunInstances)
{-# DEPRECATED risKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The network interfaces to associate with the instance. If you specify a network interface, you must specify any security groups and subnets as part of the network interface.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risNetworkInterfaces :: Lens.Lens' RunInstances (Lude.Maybe [InstanceNetworkInterfaceSpecification])
risNetworkInterfaces = Lens.lens (networkInterfaces :: RunInstances -> Lude.Maybe [InstanceNetworkInterfaceSpecification]) (\s a -> s {networkInterfaces = a} :: RunInstances)
{-# DEPRECATED risNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ .
--
-- You can't enable AWS Nitro Enclaves and hibernation on the same instance.
--
-- /Note:/ Consider using 'enclaveOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risEnclaveOptions :: Lens.Lens' RunInstances (Lude.Maybe EnclaveOptionsRequest)
risEnclaveOptions = Lens.lens (enclaveOptions :: RunInstances -> Lude.Maybe EnclaveOptionsRequest) (\s a -> s {enclaveOptions = a} :: RunInstances)
{-# DEPRECATED risEnclaveOptions "Use generic-lens or generic-optics with 'enclaveOptions' instead." #-}

-- | The ID of the RAM disk to select. Some kernels require additional drivers at launch. Check the kernel requirements for information about whether you need to specify a RAM disk. To find kernel requirements, go to the AWS Resource Center and search for the kernel ID.
--
-- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risRAMDiskId :: Lens.Lens' RunInstances (Lude.Maybe Lude.Text)
risRAMDiskId = Lens.lens (ramdiskId :: RunInstances -> Lude.Maybe Lude.Text) (\s a -> s {ramdiskId = a} :: RunInstances)
{-# DEPRECATED risRAMDiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU options> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'cpuOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risCPUOptions :: Lens.Lens' RunInstances (Lude.Maybe CPUOptionsRequest)
risCPUOptions = Lens.lens (cpuOptions :: RunInstances -> Lude.Maybe CPUOptionsRequest) (\s a -> s {cpuOptions = a} :: RunInstances)
{-# DEPRECATED risCPUOptions "Use generic-lens or generic-optics with 'cpuOptions' instead." #-}

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
--
-- If you specify a network interface, you must specify any subnets as part of the network interface.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risSubnetId :: Lens.Lens' RunInstances (Lude.Maybe Lude.Text)
risSubnetId = Lens.lens (subnetId :: RunInstances -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: RunInstances)
{-# DEPRECATED risSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The ID of the kernel.
--
-- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risKernelId :: Lens.Lens' RunInstances (Lude.Maybe Lude.Text)
risKernelId = Lens.lens (kernelId :: RunInstances -> Lude.Maybe Lude.Text) (\s a -> s {kernelId = a} :: RunInstances)
{-# DEPRECATED risKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @m1.small@
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risInstanceType :: Lens.Lens' RunInstances (Lude.Maybe InstanceType)
risInstanceType = Lens.lens (instanceType :: RunInstances -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: RunInstances)
{-# DEPRECATED risInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Information about the Capacity Reservation targeting option. If you do not specify this parameter, the instance's Capacity Reservation preference defaults to @open@ , which enables it to run in any open Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
--
-- /Note:/ Consider using 'capacityReservationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risCapacityReservationSpecification :: Lens.Lens' RunInstances (Lude.Maybe CapacityReservationSpecification)
risCapacityReservationSpecification = Lens.lens (capacityReservationSpecification :: RunInstances -> Lude.Maybe CapacityReservationSpecification) (\s a -> s {capacityReservationSpecification = a} :: RunInstances)
{-# DEPRECATED risCapacityReservationSpecification "Use generic-lens or generic-optics with 'capacityReservationSpecification' instead." #-}

-- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal Amazon EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance.
--
-- Default: @false@
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risEBSOptimized :: Lens.Lens' RunInstances (Lude.Maybe Lude.Bool)
risEBSOptimized = Lens.lens (ebsOptimized :: RunInstances -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: RunInstances)
{-# DEPRECATED risEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The user data to make available to the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running commands on your Linux instance at launch> (Linux) and <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows). If you are using a command line tool, base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide base64-encoded text. User data is limited to 16 KB.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risUserData :: Lens.Lens' RunInstances (Lude.Maybe Lude.Text)
risUserData = Lens.lens (userData :: RunInstances -> Lude.Maybe Lude.Text) (\s a -> s {userData = a} :: RunInstances)
{-# DEPRECATED risUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | Specifies whether detailed monitoring is enabled for the instance.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risMonitoring :: Lens.Lens' RunInstances (Lude.Maybe RunInstancesMonitoringEnabled)
risMonitoring = Lens.lens (monitoring :: RunInstances -> Lude.Maybe RunInstancesMonitoringEnabled) (\s a -> s {monitoring = a} :: RunInstances)
{-# DEPRECATED risMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | The tags to apply to the resources during launch. You can only tag instances and volumes on launch. The specified tags are applied to all instances or volumes that are created during launch. To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risTagSpecifications :: Lens.Lens' RunInstances (Lude.Maybe [TagSpecification])
risTagSpecifications = Lens.lens (tagSpecifications :: RunInstances -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: RunInstances)
{-# DEPRECATED risTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | [EC2-VPC] The number of IPv6 addresses to associate with the primary network interface. Amazon EC2 chooses the IPv6 addresses from the range of your subnet. You cannot specify this option and the option to assign specific IPv6 addresses in the same request. You can specify this option if you've specified a minimum number of instances to launch.
--
-- You cannot specify this option and the network interfaces option in the same request.
--
-- /Note:/ Consider using 'ipv6AddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risIPv6AddressCount :: Lens.Lens' RunInstances (Lude.Maybe Lude.Int)
risIPv6AddressCount = Lens.lens (ipv6AddressCount :: RunInstances -> Lude.Maybe Lude.Int) (\s a -> s {ipv6AddressCount = a} :: RunInstances)
{-# DEPRECATED risIPv6AddressCount "Use generic-lens or generic-optics with 'ipv6AddressCount' instead." #-}

-- | Indicates whether an instance is enabled for hibernation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You can't enable hibernation and AWS Nitro Enclaves on the same instance.
--
-- /Note:/ Consider using 'hibernationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risHibernationOptions :: Lens.Lens' RunInstances (Lude.Maybe HibernationOptionsRequest)
risHibernationOptions = Lens.lens (hibernationOptions :: RunInstances -> Lude.Maybe HibernationOptionsRequest) (\s a -> s {hibernationOptions = a} :: RunInstances)
{-# DEPRECATED risHibernationOptions "Use generic-lens or generic-optics with 'hibernationOptions' instead." #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risIAMInstanceProfile :: Lens.Lens' RunInstances (Lude.Maybe IAMInstanceProfileSpecification)
risIAMInstanceProfile = Lens.lens (iamInstanceProfile :: RunInstances -> Lude.Maybe IAMInstanceProfileSpecification) (\s a -> s {iamInstanceProfile = a} :: RunInstances)
{-# DEPRECATED risIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | An elastic GPU to associate with the instance. An Elastic GPU is a GPU resource that you can attach to your Windows instance to accelerate the graphics performance of your applications. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html Amazon EC2 Elastic GPUs> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'elasticGpuSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risElasticGpuSpecification :: Lens.Lens' RunInstances (Lude.Maybe [ElasticGpuSpecification])
risElasticGpuSpecification = Lens.lens (elasticGpuSpecification :: RunInstances -> Lude.Maybe [ElasticGpuSpecification]) (\s a -> s {elasticGpuSpecification = a} :: RunInstances)
{-# DEPRECATED risElasticGpuSpecification "Use generic-lens or generic-optics with 'elasticGpuSpecification' instead." #-}

-- | The ID of the AMI. An AMI ID is required to launch an instance and must be specified here or in a launch template.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risImageId :: Lens.Lens' RunInstances (Lude.Maybe Lude.Text)
risImageId = Lens.lens (imageId :: RunInstances -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: RunInstances)
{-# DEPRECATED risImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | [EC2-VPC] The primary IPv4 address. You must specify a value from the IPv4 address range of the subnet.
--
-- Only one private IP address can be designated as primary. You can't specify this option if you've specified the option to designate a private IP address as the primary IP address in a network interface specification. You cannot specify this option if you're launching more than one instance in the request.
-- You cannot specify this option and the network interfaces option in the same request.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risPrivateIPAddress :: Lens.Lens' RunInstances (Lude.Maybe Lude.Text)
risPrivateIPAddress = Lens.lens (privateIPAddress :: RunInstances -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: RunInstances)
{-# DEPRECATED risPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- Default: @stop@
--
-- /Note:/ Consider using 'instanceInitiatedShutdownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risInstanceInitiatedShutdownBehavior :: Lens.Lens' RunInstances (Lude.Maybe ShutdownBehavior)
risInstanceInitiatedShutdownBehavior = Lens.lens (instanceInitiatedShutdownBehavior :: RunInstances -> Lude.Maybe ShutdownBehavior) (\s a -> s {instanceInitiatedShutdownBehavior = a} :: RunInstances)
{-# DEPRECATED risInstanceInitiatedShutdownBehavior "Use generic-lens or generic-optics with 'instanceInitiatedShutdownBehavior' instead." #-}

-- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> .
--
-- /Note:/ Consider using 'metadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risMetadataOptions :: Lens.Lens' RunInstances (Lude.Maybe InstanceMetadataOptionsRequest)
risMetadataOptions = Lens.lens (metadataOptions :: RunInstances -> Lude.Maybe InstanceMetadataOptionsRequest) (\s a -> s {metadataOptions = a} :: RunInstances)
{-# DEPRECATED risMetadataOptions "Use generic-lens or generic-optics with 'metadataOptions' instead." #-}

-- | The launch template to use to launch the instances. Any parameters that you specify in 'RunInstances' override the same parameters in the launch template. You can specify either the name or ID of a launch template, but not both.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risLaunchTemplate :: Lens.Lens' RunInstances (Lude.Maybe LaunchTemplateSpecification)
risLaunchTemplate = Lens.lens (launchTemplate :: RunInstances -> Lude.Maybe LaunchTemplateSpecification) (\s a -> s {launchTemplate = a} :: RunInstances)
{-# DEPRECATED risLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | The credit option for CPU usage of the burstable performance instance. Valid values are @standard@ and @unlimited@ . To change this attribute after launch, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceCreditSpecification.html ModifyInstanceCreditSpecification> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @standard@ (T2 instances) or @unlimited@ (T3/T3a instances)
--
-- /Note:/ Consider using 'creditSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risCreditSpecification :: Lens.Lens' RunInstances (Lude.Maybe CreditSpecificationRequest)
risCreditSpecification = Lens.lens (creditSpecification :: RunInstances -> Lude.Maybe CreditSpecificationRequest) (\s a -> s {creditSpecification = a} :: RunInstances)
{-# DEPRECATED risCreditSpecification "Use generic-lens or generic-optics with 'creditSpecification' instead." #-}

-- | The block device mapping entries.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risBlockDeviceMappings :: Lens.Lens' RunInstances (Lude.Maybe [BlockDeviceMapping])
risBlockDeviceMappings = Lens.lens (blockDeviceMappings :: RunInstances -> Lude.Maybe [BlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: RunInstances)
{-# DEPRECATED risBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risDryRun :: Lens.Lens' RunInstances (Lude.Maybe Lude.Bool)
risDryRun = Lens.lens (dryRun :: RunInstances -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RunInstances)
{-# DEPRECATED risDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The placement for the instance.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risPlacement :: Lens.Lens' RunInstances (Lude.Maybe Placement)
risPlacement = Lens.lens (placement :: RunInstances -> Lude.Maybe Placement) (\s a -> s {placement = a} :: RunInstances)
{-# DEPRECATED risPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

-- | [EC2-VPC] The IPv6 addresses from the range of the subnet to associate with the primary network interface. You cannot specify this option and the option to assign a number of IPv6 addresses in the same request. You cannot specify this option if you've specified a minimum number of instances to launch.
--
-- You cannot specify this option and the network interfaces option in the same request.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risIPv6Addresses :: Lens.Lens' RunInstances (Lude.Maybe [InstanceIPv6Address])
risIPv6Addresses = Lens.lens (ipv6Addresses :: RunInstances -> Lude.Maybe [InstanceIPv6Address]) (\s a -> s {ipv6Addresses = a} :: RunInstances)
{-# DEPRECATED risIPv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

-- | The maximum number of instances to launch. If you specify more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches the largest possible number of instances above @MinCount@ .
--
-- Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 FAQ.
--
-- /Note:/ Consider using 'maxCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risMaxCount :: Lens.Lens' RunInstances Lude.Int
risMaxCount = Lens.lens (maxCount :: RunInstances -> Lude.Int) (\s a -> s {maxCount = a} :: RunInstances)
{-# DEPRECATED risMaxCount "Use generic-lens or generic-optics with 'maxCount' instead." #-}

-- | The minimum number of instances to launch. If you specify a minimum that is more instances than Amazon EC2 can launch in the target Availability Zone, Amazon EC2 launches no instances.
--
-- Constraints: Between 1 and the maximum number you're allowed for the specified instance type. For more information about the default limits, and how to request an increase, see <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2> in the Amazon EC2 General FAQ.
--
-- /Note:/ Consider using 'minCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risMinCount :: Lens.Lens' RunInstances Lude.Int
risMinCount = Lens.lens (minCount :: RunInstances -> Lude.Int) (\s a -> s {minCount = a} :: RunInstances)
{-# DEPRECATED risMinCount "Use generic-lens or generic-optics with 'minCount' instead." #-}

instance Lude.AWSRequest RunInstances where
  type Rs RunInstances = Reservation
  request = Req.postQuery ec2Service
  response = Res.receiveXML (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders RunInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RunInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery RunInstances where
  toQuery RunInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RunInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "AdditionalInfo" Lude.=: additionalInfo,
        Lude.toQuery
          (Lude.toQueryList "SecurityGroupId" Lude.<$> securityGroupIds),
        Lude.toQuery
          (Lude.toQueryList "SecurityGroup" Lude.<$> securityGroups),
        "ClientToken" Lude.=: clientToken,
        Lude.toQuery
          ( Lude.toQueryList "ElasticInferenceAccelerator"
              Lude.<$> elasticInferenceAccelerators
          ),
        "InstanceMarketOptions" Lude.=: instanceMarketOptions,
        Lude.toQuery
          ( Lude.toQueryList "LicenseSpecification"
              Lude.<$> licenseSpecifications
          ),
        "DisableApiTermination" Lude.=: disableAPITermination,
        "KeyName" Lude.=: keyName,
        Lude.toQuery
          (Lude.toQueryList "NetworkInterface" Lude.<$> networkInterfaces),
        "EnclaveOptions" Lude.=: enclaveOptions,
        "RamdiskId" Lude.=: ramdiskId,
        "CpuOptions" Lude.=: cpuOptions,
        "SubnetId" Lude.=: subnetId,
        "KernelId" Lude.=: kernelId,
        "InstanceType" Lude.=: instanceType,
        "CapacityReservationSpecification"
          Lude.=: capacityReservationSpecification,
        "EbsOptimized" Lude.=: ebsOptimized,
        "UserData" Lude.=: userData,
        "Monitoring" Lude.=: monitoring,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "Ipv6AddressCount" Lude.=: ipv6AddressCount,
        "HibernationOptions" Lude.=: hibernationOptions,
        "IamInstanceProfile" Lude.=: iamInstanceProfile,
        Lude.toQuery
          ( Lude.toQueryList "ElasticGpuSpecification"
              Lude.<$> elasticGpuSpecification
          ),
        "ImageId" Lude.=: imageId,
        "PrivateIpAddress" Lude.=: privateIPAddress,
        "InstanceInitiatedShutdownBehavior"
          Lude.=: instanceInitiatedShutdownBehavior,
        "MetadataOptions" Lude.=: metadataOptions,
        "LaunchTemplate" Lude.=: launchTemplate,
        "CreditSpecification" Lude.=: creditSpecification,
        Lude.toQuery
          ( Lude.toQueryList "BlockDeviceMapping"
              Lude.<$> blockDeviceMappings
          ),
        "DryRun" Lude.=: dryRun,
        "Placement" Lude.=: placement,
        Lude.toQuery
          (Lude.toQueryList "Ipv6Address" Lude.<$> ipv6Addresses),
        "MaxCount" Lude.=: maxCount,
        "MinCount" Lude.=: minCount
      ]
