{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RequestLaunchTemplateData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RequestLaunchTemplateData
  ( RequestLaunchTemplateData (..),

    -- * Smart constructor
    mkRequestLaunchTemplateData,

    -- * Lenses
    rltdSecurityGroupIds,
    rltdSecurityGroups,
    rltdElasticInferenceAccelerators,
    rltdInstanceMarketOptions,
    rltdLicenseSpecifications,
    rltdDisableAPITermination,
    rltdKeyName,
    rltdNetworkInterfaces,
    rltdEnclaveOptions,
    rltdCPUOptions,
    rltdRamDiskId,
    rltdKernelId,
    rltdElasticGpuSpecifications,
    rltdInstanceType,
    rltdCapacityReservationSpecification,
    rltdEBSOptimized,
    rltdUserData,
    rltdMonitoring,
    rltdTagSpecifications,
    rltdHibernationOptions,
    rltdIAMInstanceProfile,
    rltdImageId,
    rltdInstanceInitiatedShutdownBehavior,
    rltdMetadataOptions,
    rltdCreditSpecification,
    rltdBlockDeviceMappings,
    rltdPlacement,
  )
where

import Network.AWS.EC2.Types.CreditSpecificationRequest
import Network.AWS.EC2.Types.ElasticGpuSpecification
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMappingRequest
import Network.AWS.EC2.Types.LaunchTemplateCPUOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator
import Network.AWS.EC2.Types.LaunchTemplateEnclaveOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateHibernationOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateIAMInstanceProfileSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateLicenseConfigurationRequest
import Network.AWS.EC2.Types.LaunchTemplatePlacementRequest
import Network.AWS.EC2.Types.LaunchTemplateTagSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplatesMonitoringRequest
import Network.AWS.EC2.Types.ShutdownBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The information to include in the launch template.
--
-- /See:/ 'mkRequestLaunchTemplateData' smart constructor.
data RequestLaunchTemplateData = RequestLaunchTemplateData'
  { securityGroupIds ::
      Lude.Maybe [Lude.Text],
    securityGroups ::
      Lude.Maybe [Lude.Text],
    elasticInferenceAccelerators ::
      Lude.Maybe
        [LaunchTemplateElasticInferenceAccelerator],
    instanceMarketOptions ::
      Lude.Maybe
        LaunchTemplateInstanceMarketOptionsRequest,
    licenseSpecifications ::
      Lude.Maybe
        [LaunchTemplateLicenseConfigurationRequest],
    disableAPITermination ::
      Lude.Maybe Lude.Bool,
    keyName :: Lude.Maybe Lude.Text,
    networkInterfaces ::
      Lude.Maybe
        [LaunchTemplateInstanceNetworkInterfaceSpecificationRequest],
    enclaveOptions ::
      Lude.Maybe
        LaunchTemplateEnclaveOptionsRequest,
    cpuOptions ::
      Lude.Maybe
        LaunchTemplateCPUOptionsRequest,
    ramDiskId :: Lude.Maybe Lude.Text,
    kernelId :: Lude.Maybe Lude.Text,
    elasticGpuSpecifications ::
      Lude.Maybe [ElasticGpuSpecification],
    instanceType :: Lude.Maybe InstanceType,
    capacityReservationSpecification ::
      Lude.Maybe
        LaunchTemplateCapacityReservationSpecificationRequest,
    ebsOptimized :: Lude.Maybe Lude.Bool,
    userData :: Lude.Maybe Lude.Text,
    monitoring ::
      Lude.Maybe
        LaunchTemplatesMonitoringRequest,
    tagSpecifications ::
      Lude.Maybe
        [LaunchTemplateTagSpecificationRequest],
    hibernationOptions ::
      Lude.Maybe
        LaunchTemplateHibernationOptionsRequest,
    iamInstanceProfile ::
      Lude.Maybe
        LaunchTemplateIAMInstanceProfileSpecificationRequest,
    imageId :: Lude.Maybe Lude.Text,
    instanceInitiatedShutdownBehavior ::
      Lude.Maybe ShutdownBehavior,
    metadataOptions ::
      Lude.Maybe
        LaunchTemplateInstanceMetadataOptionsRequest,
    creditSpecification ::
      Lude.Maybe CreditSpecificationRequest,
    blockDeviceMappings ::
      Lude.Maybe
        [LaunchTemplateBlockDeviceMappingRequest],
    placement ::
      Lude.Maybe
        LaunchTemplatePlacementRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RequestLaunchTemplateData' with the minimum fields required to make a request.
--
-- * 'blockDeviceMappings' - The block device mapping.
-- * 'capacityReservationSpecification' - The Capacity Reservation targeting option. If you do not specify this parameter, the instance's Capacity Reservation preference defaults to @open@ , which enables it to run in any open Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
-- * 'cpuOptions' - The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'creditSpecification' - The credit option for CPU usage of the instance. Valid for T2, T3, or T3a instances only.
-- * 'disableAPITermination' - If you set this parameter to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. To change this attribute after launch, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute> . Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to @terminate@ , you can terminate the instance by running the shutdown command from the instance.
-- * 'ebsOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal Amazon EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance.
-- * 'elasticGpuSpecifications' - An elastic GPU to associate with the instance.
-- * 'elasticInferenceAccelerators' - The elastic inference accelerator for the instance.
-- * 'enclaveOptions' - Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ .
--
-- You can't enable AWS Nitro Enclaves and hibernation on the same instance.
-- * 'hibernationOptions' - Indicates whether an instance is enabled for hibernation. This parameter is valid only if the instance meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'iamInstanceProfile' - The IAM instance profile.
-- * 'imageId' - The ID of the AMI.
-- * 'instanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- Default: @stop@
-- * 'instanceMarketOptions' - The market (purchasing) option for the instances.
-- * 'instanceType' - The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'kernelId' - The ID of the kernel.
--
-- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'keyName' - The name of the key pair. You can create a key pair using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair> or <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair> .
--
-- /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
-- * 'licenseSpecifications' - The license configurations.
-- * 'metadataOptions' - The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'monitoring' - The monitoring for the instance.
-- * 'networkInterfaces' - One or more network interfaces. If you specify a network interface, you must specify any security groups and subnets as part of the network interface.
-- * 'placement' - The placement for the instance.
-- * 'ramDiskId' - The ID of the RAM disk.
--
-- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'securityGroupIds' - One or more security group IDs. You can create a security group using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup> . You cannot specify both a security group ID and security name in the same request.
-- * 'securityGroups' - [EC2-Classic, default VPC] One or more security group names. For a nondefault VPC, you must use security group IDs instead. You cannot specify both a security group ID and security name in the same request.
-- * 'tagSpecifications' - The tags to apply to the resources during launch. You can only tag instances and volumes on launch. The specified tags are applied to all instances or volumes that are created during launch. To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
-- * 'userData' - The Base64-encoded user data to make available to the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch> (Linux) and <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows).
mkRequestLaunchTemplateData ::
  RequestLaunchTemplateData
mkRequestLaunchTemplateData =
  RequestLaunchTemplateData'
    { securityGroupIds = Lude.Nothing,
      securityGroups = Lude.Nothing,
      elasticInferenceAccelerators = Lude.Nothing,
      instanceMarketOptions = Lude.Nothing,
      licenseSpecifications = Lude.Nothing,
      disableAPITermination = Lude.Nothing,
      keyName = Lude.Nothing,
      networkInterfaces = Lude.Nothing,
      enclaveOptions = Lude.Nothing,
      cpuOptions = Lude.Nothing,
      ramDiskId = Lude.Nothing,
      kernelId = Lude.Nothing,
      elasticGpuSpecifications = Lude.Nothing,
      instanceType = Lude.Nothing,
      capacityReservationSpecification = Lude.Nothing,
      ebsOptimized = Lude.Nothing,
      userData = Lude.Nothing,
      monitoring = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      hibernationOptions = Lude.Nothing,
      iamInstanceProfile = Lude.Nothing,
      imageId = Lude.Nothing,
      instanceInitiatedShutdownBehavior = Lude.Nothing,
      metadataOptions = Lude.Nothing,
      creditSpecification = Lude.Nothing,
      blockDeviceMappings = Lude.Nothing,
      placement = Lude.Nothing
    }

-- | One or more security group IDs. You can create a security group using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup> . You cannot specify both a security group ID and security name in the same request.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdSecurityGroupIds :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe [Lude.Text])
rltdSecurityGroupIds = Lens.lens (securityGroupIds :: RequestLaunchTemplateData -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | [EC2-Classic, default VPC] One or more security group names. For a nondefault VPC, you must use security group IDs instead. You cannot specify both a security group ID and security name in the same request.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdSecurityGroups :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe [Lude.Text])
rltdSecurityGroups = Lens.lens (securityGroups :: RequestLaunchTemplateData -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The elastic inference accelerator for the instance.
--
-- /Note:/ Consider using 'elasticInferenceAccelerators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdElasticInferenceAccelerators :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe [LaunchTemplateElasticInferenceAccelerator])
rltdElasticInferenceAccelerators = Lens.lens (elasticInferenceAccelerators :: RequestLaunchTemplateData -> Lude.Maybe [LaunchTemplateElasticInferenceAccelerator]) (\s a -> s {elasticInferenceAccelerators = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdElasticInferenceAccelerators "Use generic-lens or generic-optics with 'elasticInferenceAccelerators' instead." #-}

-- | The market (purchasing) option for the instances.
--
-- /Note:/ Consider using 'instanceMarketOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdInstanceMarketOptions :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe LaunchTemplateInstanceMarketOptionsRequest)
rltdInstanceMarketOptions = Lens.lens (instanceMarketOptions :: RequestLaunchTemplateData -> Lude.Maybe LaunchTemplateInstanceMarketOptionsRequest) (\s a -> s {instanceMarketOptions = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdInstanceMarketOptions "Use generic-lens or generic-optics with 'instanceMarketOptions' instead." #-}

-- | The license configurations.
--
-- /Note:/ Consider using 'licenseSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdLicenseSpecifications :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe [LaunchTemplateLicenseConfigurationRequest])
rltdLicenseSpecifications = Lens.lens (licenseSpecifications :: RequestLaunchTemplateData -> Lude.Maybe [LaunchTemplateLicenseConfigurationRequest]) (\s a -> s {licenseSpecifications = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdLicenseSpecifications "Use generic-lens or generic-optics with 'licenseSpecifications' instead." #-}

-- | If you set this parameter to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. To change this attribute after launch, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute> . Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to @terminate@ , you can terminate the instance by running the shutdown command from the instance.
--
-- /Note:/ Consider using 'disableAPITermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdDisableAPITermination :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe Lude.Bool)
rltdDisableAPITermination = Lens.lens (disableAPITermination :: RequestLaunchTemplateData -> Lude.Maybe Lude.Bool) (\s a -> s {disableAPITermination = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdDisableAPITermination "Use generic-lens or generic-optics with 'disableAPITermination' instead." #-}

-- | The name of the key pair. You can create a key pair using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair> or <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair> .
--
-- /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdKeyName :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe Lude.Text)
rltdKeyName = Lens.lens (keyName :: RequestLaunchTemplateData -> Lude.Maybe Lude.Text) (\s a -> s {keyName = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | One or more network interfaces. If you specify a network interface, you must specify any security groups and subnets as part of the network interface.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdNetworkInterfaces :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe [LaunchTemplateInstanceNetworkInterfaceSpecificationRequest])
rltdNetworkInterfaces = Lens.lens (networkInterfaces :: RequestLaunchTemplateData -> Lude.Maybe [LaunchTemplateInstanceNetworkInterfaceSpecificationRequest]) (\s a -> s {networkInterfaces = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ .
--
-- You can't enable AWS Nitro Enclaves and hibernation on the same instance.
--
-- /Note:/ Consider using 'enclaveOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdEnclaveOptions :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe LaunchTemplateEnclaveOptionsRequest)
rltdEnclaveOptions = Lens.lens (enclaveOptions :: RequestLaunchTemplateData -> Lude.Maybe LaunchTemplateEnclaveOptionsRequest) (\s a -> s {enclaveOptions = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdEnclaveOptions "Use generic-lens or generic-optics with 'enclaveOptions' instead." #-}

-- | The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'cpuOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdCPUOptions :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe LaunchTemplateCPUOptionsRequest)
rltdCPUOptions = Lens.lens (cpuOptions :: RequestLaunchTemplateData -> Lude.Maybe LaunchTemplateCPUOptionsRequest) (\s a -> s {cpuOptions = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdCPUOptions "Use generic-lens or generic-optics with 'cpuOptions' instead." #-}

-- | The ID of the RAM disk.
--
-- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'ramDiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdRamDiskId :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe Lude.Text)
rltdRamDiskId = Lens.lens (ramDiskId :: RequestLaunchTemplateData -> Lude.Maybe Lude.Text) (\s a -> s {ramDiskId = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdRamDiskId "Use generic-lens or generic-optics with 'ramDiskId' instead." #-}

-- | The ID of the kernel.
--
-- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdKernelId :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe Lude.Text)
rltdKernelId = Lens.lens (kernelId :: RequestLaunchTemplateData -> Lude.Maybe Lude.Text) (\s a -> s {kernelId = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | An elastic GPU to associate with the instance.
--
-- /Note:/ Consider using 'elasticGpuSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdElasticGpuSpecifications :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe [ElasticGpuSpecification])
rltdElasticGpuSpecifications = Lens.lens (elasticGpuSpecifications :: RequestLaunchTemplateData -> Lude.Maybe [ElasticGpuSpecification]) (\s a -> s {elasticGpuSpecifications = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdElasticGpuSpecifications "Use generic-lens or generic-optics with 'elasticGpuSpecifications' instead." #-}

-- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdInstanceType :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe InstanceType)
rltdInstanceType = Lens.lens (instanceType :: RequestLaunchTemplateData -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The Capacity Reservation targeting option. If you do not specify this parameter, the instance's Capacity Reservation preference defaults to @open@ , which enables it to run in any open Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
--
-- /Note:/ Consider using 'capacityReservationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdCapacityReservationSpecification :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe LaunchTemplateCapacityReservationSpecificationRequest)
rltdCapacityReservationSpecification = Lens.lens (capacityReservationSpecification :: RequestLaunchTemplateData -> Lude.Maybe LaunchTemplateCapacityReservationSpecificationRequest) (\s a -> s {capacityReservationSpecification = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdCapacityReservationSpecification "Use generic-lens or generic-optics with 'capacityReservationSpecification' instead." #-}

-- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal Amazon EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdEBSOptimized :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe Lude.Bool)
rltdEBSOptimized = Lens.lens (ebsOptimized :: RequestLaunchTemplateData -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The Base64-encoded user data to make available to the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch> (Linux) and <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows).
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdUserData :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe Lude.Text)
rltdUserData = Lens.lens (userData :: RequestLaunchTemplateData -> Lude.Maybe Lude.Text) (\s a -> s {userData = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | The monitoring for the instance.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdMonitoring :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe LaunchTemplatesMonitoringRequest)
rltdMonitoring = Lens.lens (monitoring :: RequestLaunchTemplateData -> Lude.Maybe LaunchTemplatesMonitoringRequest) (\s a -> s {monitoring = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | The tags to apply to the resources during launch. You can only tag instances and volumes on launch. The specified tags are applied to all instances or volumes that are created during launch. To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdTagSpecifications :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe [LaunchTemplateTagSpecificationRequest])
rltdTagSpecifications = Lens.lens (tagSpecifications :: RequestLaunchTemplateData -> Lude.Maybe [LaunchTemplateTagSpecificationRequest]) (\s a -> s {tagSpecifications = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Indicates whether an instance is enabled for hibernation. This parameter is valid only if the instance meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'hibernationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdHibernationOptions :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe LaunchTemplateHibernationOptionsRequest)
rltdHibernationOptions = Lens.lens (hibernationOptions :: RequestLaunchTemplateData -> Lude.Maybe LaunchTemplateHibernationOptionsRequest) (\s a -> s {hibernationOptions = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdHibernationOptions "Use generic-lens or generic-optics with 'hibernationOptions' instead." #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdIAMInstanceProfile :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe LaunchTemplateIAMInstanceProfileSpecificationRequest)
rltdIAMInstanceProfile = Lens.lens (iamInstanceProfile :: RequestLaunchTemplateData -> Lude.Maybe LaunchTemplateIAMInstanceProfileSpecificationRequest) (\s a -> s {iamInstanceProfile = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdImageId :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe Lude.Text)
rltdImageId = Lens.lens (imageId :: RequestLaunchTemplateData -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- Default: @stop@
--
-- /Note:/ Consider using 'instanceInitiatedShutdownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdInstanceInitiatedShutdownBehavior :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe ShutdownBehavior)
rltdInstanceInitiatedShutdownBehavior = Lens.lens (instanceInitiatedShutdownBehavior :: RequestLaunchTemplateData -> Lude.Maybe ShutdownBehavior) (\s a -> s {instanceInitiatedShutdownBehavior = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdInstanceInitiatedShutdownBehavior "Use generic-lens or generic-optics with 'instanceInitiatedShutdownBehavior' instead." #-}

-- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'metadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdMetadataOptions :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe LaunchTemplateInstanceMetadataOptionsRequest)
rltdMetadataOptions = Lens.lens (metadataOptions :: RequestLaunchTemplateData -> Lude.Maybe LaunchTemplateInstanceMetadataOptionsRequest) (\s a -> s {metadataOptions = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdMetadataOptions "Use generic-lens or generic-optics with 'metadataOptions' instead." #-}

-- | The credit option for CPU usage of the instance. Valid for T2, T3, or T3a instances only.
--
-- /Note:/ Consider using 'creditSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdCreditSpecification :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe CreditSpecificationRequest)
rltdCreditSpecification = Lens.lens (creditSpecification :: RequestLaunchTemplateData -> Lude.Maybe CreditSpecificationRequest) (\s a -> s {creditSpecification = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdCreditSpecification "Use generic-lens or generic-optics with 'creditSpecification' instead." #-}

-- | The block device mapping.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdBlockDeviceMappings :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe [LaunchTemplateBlockDeviceMappingRequest])
rltdBlockDeviceMappings = Lens.lens (blockDeviceMappings :: RequestLaunchTemplateData -> Lude.Maybe [LaunchTemplateBlockDeviceMappingRequest]) (\s a -> s {blockDeviceMappings = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The placement for the instance.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdPlacement :: Lens.Lens' RequestLaunchTemplateData (Lude.Maybe LaunchTemplatePlacementRequest)
rltdPlacement = Lens.lens (placement :: RequestLaunchTemplateData -> Lude.Maybe LaunchTemplatePlacementRequest) (\s a -> s {placement = a} :: RequestLaunchTemplateData)
{-# DEPRECATED rltdPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

instance Lude.ToQuery RequestLaunchTemplateData where
  toQuery RequestLaunchTemplateData' {..} =
    Lude.mconcat
      [ Lude.toQuery
          (Lude.toQueryList "SecurityGroupId" Lude.<$> securityGroupIds),
        Lude.toQuery
          (Lude.toQueryList "SecurityGroup" Lude.<$> securityGroups),
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
        "CpuOptions" Lude.=: cpuOptions,
        "RamDiskId" Lude.=: ramDiskId,
        "KernelId" Lude.=: kernelId,
        Lude.toQuery
          ( Lude.toQueryList "ElasticGpuSpecification"
              Lude.<$> elasticGpuSpecifications
          ),
        "InstanceType" Lude.=: instanceType,
        "CapacityReservationSpecification"
          Lude.=: capacityReservationSpecification,
        "EbsOptimized" Lude.=: ebsOptimized,
        "UserData" Lude.=: userData,
        "Monitoring" Lude.=: monitoring,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "HibernationOptions" Lude.=: hibernationOptions,
        "IamInstanceProfile" Lude.=: iamInstanceProfile,
        "ImageId" Lude.=: imageId,
        "InstanceInitiatedShutdownBehavior"
          Lude.=: instanceInitiatedShutdownBehavior,
        "MetadataOptions" Lude.=: metadataOptions,
        "CreditSpecification" Lude.=: creditSpecification,
        Lude.toQuery
          ( Lude.toQueryList "BlockDeviceMapping"
              Lude.<$> blockDeviceMappings
          ),
        "Placement" Lude.=: placement
      ]
