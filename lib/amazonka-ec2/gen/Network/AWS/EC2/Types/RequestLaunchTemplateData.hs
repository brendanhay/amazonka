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
    rltdBlockDeviceMappings,
    rltdCapacityReservationSpecification,
    rltdCpuOptions,
    rltdCreditSpecification,
    rltdDisableApiTermination,
    rltdEbsOptimized,
    rltdElasticGpuSpecifications,
    rltdElasticInferenceAccelerators,
    rltdEnclaveOptions,
    rltdHibernationOptions,
    rltdIamInstanceProfile,
    rltdImageId,
    rltdInstanceInitiatedShutdownBehavior,
    rltdInstanceMarketOptions,
    rltdInstanceType,
    rltdKernelId,
    rltdKeyName,
    rltdLicenseSpecifications,
    rltdMetadataOptions,
    rltdMonitoring,
    rltdNetworkInterfaces,
    rltdPlacement,
    rltdRamDiskId,
    rltdSecurityGroupIds,
    rltdSecurityGroups,
    rltdTagSpecifications,
    rltdUserData,
  )
where

import qualified Network.AWS.EC2.Types.CreditSpecificationRequest as Types
import qualified Network.AWS.EC2.Types.ElasticGpuSpecification as Types
import qualified Network.AWS.EC2.Types.ImageId as Types
import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.KernelId as Types
import qualified Network.AWS.EC2.Types.KeyName as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMappingRequest as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationRequest as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateCpuOptionsRequest as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateEnclaveOptionsRequest as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateHibernationOptionsRequest as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecificationRequest as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateLicenseConfigurationRequest as Types
import qualified Network.AWS.EC2.Types.LaunchTemplatePlacementRequest as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateTagSpecificationRequest as Types
import qualified Network.AWS.EC2.Types.LaunchTemplatesMonitoringRequest as Types
import qualified Network.AWS.EC2.Types.RamDiskId as Types
import qualified Network.AWS.EC2.Types.SecurityGroupId as Types
import qualified Network.AWS.EC2.Types.SecurityGroupName as Types
import qualified Network.AWS.EC2.Types.ShutdownBehavior as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The information to include in the launch template.
--
-- /See:/ 'mkRequestLaunchTemplateData' smart constructor.
data RequestLaunchTemplateData = RequestLaunchTemplateData'
  { -- | The block device mapping.
    blockDeviceMappings :: Core.Maybe [Types.LaunchTemplateBlockDeviceMappingRequest],
    -- | The Capacity Reservation targeting option. If you do not specify this parameter, the instance's Capacity Reservation preference defaults to @open@ , which enables it to run in any open Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
    capacityReservationSpecification :: Core.Maybe Types.LaunchTemplateCapacityReservationSpecificationRequest,
    -- | The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
    cpuOptions :: Core.Maybe Types.LaunchTemplateCpuOptionsRequest,
    -- | The credit option for CPU usage of the instance. Valid for T2, T3, or T3a instances only.
    creditSpecification :: Core.Maybe Types.CreditSpecificationRequest,
    -- | If you set this parameter to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. To change this attribute after launch, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute> . Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to @terminate@ , you can terminate the instance by running the shutdown command from the instance.
    disableApiTermination :: Core.Maybe Core.Bool,
    -- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal Amazon EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance.
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | An elastic GPU to associate with the instance.
    elasticGpuSpecifications :: Core.Maybe [Types.ElasticGpuSpecification],
    -- | The elastic inference accelerator for the instance.
    elasticInferenceAccelerators :: Core.Maybe [Types.LaunchTemplateElasticInferenceAccelerator],
    -- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ .
    --
    -- You can't enable AWS Nitro Enclaves and hibernation on the same instance.
    enclaveOptions :: Core.Maybe Types.LaunchTemplateEnclaveOptionsRequest,
    -- | Indicates whether an instance is enabled for hibernation. This parameter is valid only if the instance meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
    hibernationOptions :: Core.Maybe Types.LaunchTemplateHibernationOptionsRequest,
    -- | The IAM instance profile.
    iamInstanceProfile :: Core.Maybe Types.LaunchTemplateIamInstanceProfileSpecificationRequest,
    -- | The ID of the AMI.
    imageId :: Core.Maybe Types.ImageId,
    -- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
    --
    -- Default: @stop@
    instanceInitiatedShutdownBehavior :: Core.Maybe Types.ShutdownBehavior,
    -- | The market (purchasing) option for the instances.
    instanceMarketOptions :: Core.Maybe Types.LaunchTemplateInstanceMarketOptionsRequest,
    -- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
    instanceType :: Core.Maybe Types.InstanceType,
    -- | The ID of the kernel.
    --
    -- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
    kernelId :: Core.Maybe Types.KernelId,
    -- | The name of the key pair. You can create a key pair using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair> or <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair> .
    --
    -- /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
    keyName :: Core.Maybe Types.KeyName,
    -- | The license configurations.
    licenseSpecifications :: Core.Maybe [Types.LaunchTemplateLicenseConfigurationRequest],
    -- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
    metadataOptions :: Core.Maybe Types.LaunchTemplateInstanceMetadataOptionsRequest,
    -- | The monitoring for the instance.
    monitoring :: Core.Maybe Types.LaunchTemplatesMonitoringRequest,
    -- | One or more network interfaces. If you specify a network interface, you must specify any security groups and subnets as part of the network interface.
    networkInterfaces :: Core.Maybe [Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest],
    -- | The placement for the instance.
    placement :: Core.Maybe Types.LaunchTemplatePlacementRequest,
    -- | The ID of the RAM disk.
    --
    -- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
    ramDiskId :: Core.Maybe Types.RamDiskId,
    -- | One or more security group IDs. You can create a security group using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup> . You cannot specify both a security group ID and security name in the same request.
    securityGroupIds :: Core.Maybe [Types.SecurityGroupId],
    -- | [EC2-Classic, default VPC] One or more security group names. For a nondefault VPC, you must use security group IDs instead. You cannot specify both a security group ID and security name in the same request.
    securityGroups :: Core.Maybe [Types.SecurityGroupName],
    -- | The tags to apply to the resources during launch. You can only tag instances and volumes on launch. The specified tags are applied to all instances or volumes that are created during launch. To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
    tagSpecifications :: Core.Maybe [Types.LaunchTemplateTagSpecificationRequest],
    -- | The Base64-encoded user data to make available to the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch> (Linux) and <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows).
    userData :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RequestLaunchTemplateData' value with any optional fields omitted.
mkRequestLaunchTemplateData ::
  RequestLaunchTemplateData
mkRequestLaunchTemplateData =
  RequestLaunchTemplateData'
    { blockDeviceMappings = Core.Nothing,
      capacityReservationSpecification = Core.Nothing,
      cpuOptions = Core.Nothing,
      creditSpecification = Core.Nothing,
      disableApiTermination = Core.Nothing,
      ebsOptimized = Core.Nothing,
      elasticGpuSpecifications = Core.Nothing,
      elasticInferenceAccelerators = Core.Nothing,
      enclaveOptions = Core.Nothing,
      hibernationOptions = Core.Nothing,
      iamInstanceProfile = Core.Nothing,
      imageId = Core.Nothing,
      instanceInitiatedShutdownBehavior = Core.Nothing,
      instanceMarketOptions = Core.Nothing,
      instanceType = Core.Nothing,
      kernelId = Core.Nothing,
      keyName = Core.Nothing,
      licenseSpecifications = Core.Nothing,
      metadataOptions = Core.Nothing,
      monitoring = Core.Nothing,
      networkInterfaces = Core.Nothing,
      placement = Core.Nothing,
      ramDiskId = Core.Nothing,
      securityGroupIds = Core.Nothing,
      securityGroups = Core.Nothing,
      tagSpecifications = Core.Nothing,
      userData = Core.Nothing
    }

-- | The block device mapping.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdBlockDeviceMappings :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe [Types.LaunchTemplateBlockDeviceMappingRequest])
rltdBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# DEPRECATED rltdBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The Capacity Reservation targeting option. If you do not specify this parameter, the instance's Capacity Reservation preference defaults to @open@ , which enables it to run in any open Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
--
-- /Note:/ Consider using 'capacityReservationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdCapacityReservationSpecification :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.LaunchTemplateCapacityReservationSpecificationRequest)
rltdCapacityReservationSpecification = Lens.field @"capacityReservationSpecification"
{-# DEPRECATED rltdCapacityReservationSpecification "Use generic-lens or generic-optics with 'capacityReservationSpecification' instead." #-}

-- | The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'cpuOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdCpuOptions :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.LaunchTemplateCpuOptionsRequest)
rltdCpuOptions = Lens.field @"cpuOptions"
{-# DEPRECATED rltdCpuOptions "Use generic-lens or generic-optics with 'cpuOptions' instead." #-}

-- | The credit option for CPU usage of the instance. Valid for T2, T3, or T3a instances only.
--
-- /Note:/ Consider using 'creditSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdCreditSpecification :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.CreditSpecificationRequest)
rltdCreditSpecification = Lens.field @"creditSpecification"
{-# DEPRECATED rltdCreditSpecification "Use generic-lens or generic-optics with 'creditSpecification' instead." #-}

-- | If you set this parameter to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. To change this attribute after launch, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute> . Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to @terminate@ , you can terminate the instance by running the shutdown command from the instance.
--
-- /Note:/ Consider using 'disableApiTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdDisableApiTermination :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Core.Bool)
rltdDisableApiTermination = Lens.field @"disableApiTermination"
{-# DEPRECATED rltdDisableApiTermination "Use generic-lens or generic-optics with 'disableApiTermination' instead." #-}

-- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal Amazon EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdEbsOptimized :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Core.Bool)
rltdEbsOptimized = Lens.field @"ebsOptimized"
{-# DEPRECATED rltdEbsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | An elastic GPU to associate with the instance.
--
-- /Note:/ Consider using 'elasticGpuSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdElasticGpuSpecifications :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe [Types.ElasticGpuSpecification])
rltdElasticGpuSpecifications = Lens.field @"elasticGpuSpecifications"
{-# DEPRECATED rltdElasticGpuSpecifications "Use generic-lens or generic-optics with 'elasticGpuSpecifications' instead." #-}

-- | The elastic inference accelerator for the instance.
--
-- /Note:/ Consider using 'elasticInferenceAccelerators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdElasticInferenceAccelerators :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe [Types.LaunchTemplateElasticInferenceAccelerator])
rltdElasticInferenceAccelerators = Lens.field @"elasticInferenceAccelerators"
{-# DEPRECATED rltdElasticInferenceAccelerators "Use generic-lens or generic-optics with 'elasticInferenceAccelerators' instead." #-}

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ .
--
-- You can't enable AWS Nitro Enclaves and hibernation on the same instance.
--
-- /Note:/ Consider using 'enclaveOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdEnclaveOptions :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.LaunchTemplateEnclaveOptionsRequest)
rltdEnclaveOptions = Lens.field @"enclaveOptions"
{-# DEPRECATED rltdEnclaveOptions "Use generic-lens or generic-optics with 'enclaveOptions' instead." #-}

-- | Indicates whether an instance is enabled for hibernation. This parameter is valid only if the instance meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'hibernationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdHibernationOptions :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.LaunchTemplateHibernationOptionsRequest)
rltdHibernationOptions = Lens.field @"hibernationOptions"
{-# DEPRECATED rltdHibernationOptions "Use generic-lens or generic-optics with 'hibernationOptions' instead." #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdIamInstanceProfile :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.LaunchTemplateIamInstanceProfileSpecificationRequest)
rltdIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# DEPRECATED rltdIamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdImageId :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.ImageId)
rltdImageId = Lens.field @"imageId"
{-# DEPRECATED rltdImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- Default: @stop@
--
-- /Note:/ Consider using 'instanceInitiatedShutdownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdInstanceInitiatedShutdownBehavior :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.ShutdownBehavior)
rltdInstanceInitiatedShutdownBehavior = Lens.field @"instanceInitiatedShutdownBehavior"
{-# DEPRECATED rltdInstanceInitiatedShutdownBehavior "Use generic-lens or generic-optics with 'instanceInitiatedShutdownBehavior' instead." #-}

-- | The market (purchasing) option for the instances.
--
-- /Note:/ Consider using 'instanceMarketOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdInstanceMarketOptions :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.LaunchTemplateInstanceMarketOptionsRequest)
rltdInstanceMarketOptions = Lens.field @"instanceMarketOptions"
{-# DEPRECATED rltdInstanceMarketOptions "Use generic-lens or generic-optics with 'instanceMarketOptions' instead." #-}

-- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdInstanceType :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.InstanceType)
rltdInstanceType = Lens.field @"instanceType"
{-# DEPRECATED rltdInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The ID of the kernel.
--
-- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdKernelId :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.KernelId)
rltdKernelId = Lens.field @"kernelId"
{-# DEPRECATED rltdKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The name of the key pair. You can create a key pair using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair> or <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair> .
--
-- /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdKeyName :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.KeyName)
rltdKeyName = Lens.field @"keyName"
{-# DEPRECATED rltdKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The license configurations.
--
-- /Note:/ Consider using 'licenseSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdLicenseSpecifications :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe [Types.LaunchTemplateLicenseConfigurationRequest])
rltdLicenseSpecifications = Lens.field @"licenseSpecifications"
{-# DEPRECATED rltdLicenseSpecifications "Use generic-lens or generic-optics with 'licenseSpecifications' instead." #-}

-- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'metadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdMetadataOptions :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.LaunchTemplateInstanceMetadataOptionsRequest)
rltdMetadataOptions = Lens.field @"metadataOptions"
{-# DEPRECATED rltdMetadataOptions "Use generic-lens or generic-optics with 'metadataOptions' instead." #-}

-- | The monitoring for the instance.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdMonitoring :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.LaunchTemplatesMonitoringRequest)
rltdMonitoring = Lens.field @"monitoring"
{-# DEPRECATED rltdMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | One or more network interfaces. If you specify a network interface, you must specify any security groups and subnets as part of the network interface.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdNetworkInterfaces :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe [Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest])
rltdNetworkInterfaces = Lens.field @"networkInterfaces"
{-# DEPRECATED rltdNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The placement for the instance.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdPlacement :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.LaunchTemplatePlacementRequest)
rltdPlacement = Lens.field @"placement"
{-# DEPRECATED rltdPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

-- | The ID of the RAM disk.
--
-- /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'ramDiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdRamDiskId :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.RamDiskId)
rltdRamDiskId = Lens.field @"ramDiskId"
{-# DEPRECATED rltdRamDiskId "Use generic-lens or generic-optics with 'ramDiskId' instead." #-}

-- | One or more security group IDs. You can create a security group using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup> . You cannot specify both a security group ID and security name in the same request.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdSecurityGroupIds :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe [Types.SecurityGroupId])
rltdSecurityGroupIds = Lens.field @"securityGroupIds"
{-# DEPRECATED rltdSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | [EC2-Classic, default VPC] One or more security group names. For a nondefault VPC, you must use security group IDs instead. You cannot specify both a security group ID and security name in the same request.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdSecurityGroups :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe [Types.SecurityGroupName])
rltdSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED rltdSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The tags to apply to the resources during launch. You can only tag instances and volumes on launch. The specified tags are applied to all instances or volumes that are created during launch. To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdTagSpecifications :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe [Types.LaunchTemplateTagSpecificationRequest])
rltdTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED rltdTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The Base64-encoded user data to make available to the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch> (Linux) and <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows).
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rltdUserData :: Lens.Lens' RequestLaunchTemplateData (Core.Maybe Types.String)
rltdUserData = Lens.field @"userData"
{-# DEPRECATED rltdUserData "Use generic-lens or generic-optics with 'userData' instead." #-}
