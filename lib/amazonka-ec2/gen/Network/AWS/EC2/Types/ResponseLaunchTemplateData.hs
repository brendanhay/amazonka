{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ResponseLaunchTemplateData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ResponseLaunchTemplateData
  ( ResponseLaunchTemplateData (..),

    -- * Smart constructor
    mkResponseLaunchTemplateData,

    -- * Lenses
    rBlockDeviceMappings,
    rCapacityReservationSpecification,
    rCpuOptions,
    rCreditSpecification,
    rDisableApiTermination,
    rEbsOptimized,
    rElasticGpuSpecifications,
    rElasticInferenceAccelerators,
    rEnclaveOptions,
    rHibernationOptions,
    rIamInstanceProfile,
    rImageId,
    rInstanceInitiatedShutdownBehavior,
    rInstanceMarketOptions,
    rInstanceType,
    rKernelId,
    rKeyName,
    rLicenseSpecifications,
    rMetadataOptions,
    rMonitoring,
    rNetworkInterfaces,
    rPlacement,
    rRamDiskId,
    rSecurityGroupIds,
    rSecurityGroups,
    rTagSpecifications,
    rUserData,
  )
where

import qualified Network.AWS.EC2.Types.CreditSpecification as Types
import qualified Network.AWS.EC2.Types.ElasticGpuSpecificationResponse as Types
import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMapping as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationResponse as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateCpuOptions as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateEnclaveOptions as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateHibernationOptions as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecification as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptions as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptions as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateLicenseConfiguration as Types
import qualified Network.AWS.EC2.Types.LaunchTemplatePlacement as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateTagSpecification as Types
import qualified Network.AWS.EC2.Types.LaunchTemplatesMonitoring as Types
import qualified Network.AWS.EC2.Types.ShutdownBehavior as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The information for a launch template.
--
-- /See:/ 'mkResponseLaunchTemplateData' smart constructor.
data ResponseLaunchTemplateData = ResponseLaunchTemplateData'
  { -- | The block device mappings.
    blockDeviceMappings :: Core.Maybe [Types.LaunchTemplateBlockDeviceMapping],
    -- | Information about the Capacity Reservation targeting option.
    capacityReservationSpecification :: Core.Maybe Types.LaunchTemplateCapacityReservationSpecificationResponse,
    -- | The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
    cpuOptions :: Core.Maybe Types.LaunchTemplateCpuOptions,
    -- | The credit option for CPU usage of the instance.
    creditSpecification :: Core.Maybe Types.CreditSpecification,
    -- | If set to @true@ , indicates that the instance cannot be terminated using the Amazon EC2 console, command line tool, or API.
    disableApiTermination :: Core.Maybe Core.Bool,
    -- | Indicates whether the instance is optimized for Amazon EBS I/O.
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | The elastic GPU specification.
    elasticGpuSpecifications :: Core.Maybe [Types.ElasticGpuSpecificationResponse],
    -- | The elastic inference accelerator for the instance.
    elasticInferenceAccelerators :: Core.Maybe [Types.LaunchTemplateElasticInferenceAcceleratorResponse],
    -- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
    enclaveOptions :: Core.Maybe Types.LaunchTemplateEnclaveOptions,
    -- | Indicates whether an instance is configured for hibernation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
    hibernationOptions :: Core.Maybe Types.LaunchTemplateHibernationOptions,
    -- | The IAM instance profile.
    iamInstanceProfile :: Core.Maybe Types.LaunchTemplateIamInstanceProfileSpecification,
    -- | The ID of the AMI that was used to launch the instance.
    imageId :: Core.Maybe Types.String,
    -- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
    instanceInitiatedShutdownBehavior :: Core.Maybe Types.ShutdownBehavior,
    -- | The market (purchasing) option for the instances.
    instanceMarketOptions :: Core.Maybe Types.LaunchTemplateInstanceMarketOptions,
    -- | The instance type.
    instanceType :: Core.Maybe Types.InstanceType,
    -- | The ID of the kernel, if applicable.
    kernelId :: Core.Maybe Types.String,
    -- | The name of the key pair.
    keyName :: Core.Maybe Types.String,
    -- | The license configurations.
    licenseSpecifications :: Core.Maybe [Types.LaunchTemplateLicenseConfiguration],
    -- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
    metadataOptions :: Core.Maybe Types.LaunchTemplateInstanceMetadataOptions,
    -- | The monitoring for the instance.
    monitoring :: Core.Maybe Types.LaunchTemplatesMonitoring,
    -- | The network interfaces.
    networkInterfaces :: Core.Maybe [Types.LaunchTemplateInstanceNetworkInterfaceSpecification],
    -- | The placement of the instance.
    placement :: Core.Maybe Types.LaunchTemplatePlacement,
    -- | The ID of the RAM disk, if applicable.
    ramDiskId :: Core.Maybe Types.String,
    -- | The security group IDs.
    securityGroupIds :: Core.Maybe [Types.String],
    -- | The security group names.
    securityGroups :: Core.Maybe [Types.String],
    -- | The tags.
    tagSpecifications :: Core.Maybe [Types.LaunchTemplateTagSpecification],
    -- | The user data for the instance.
    userData :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ResponseLaunchTemplateData' value with any optional fields omitted.
mkResponseLaunchTemplateData ::
  ResponseLaunchTemplateData
mkResponseLaunchTemplateData =
  ResponseLaunchTemplateData'
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

-- | The block device mappings.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rBlockDeviceMappings :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Types.LaunchTemplateBlockDeviceMapping])
rBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# DEPRECATED rBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | Information about the Capacity Reservation targeting option.
--
-- /Note:/ Consider using 'capacityReservationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCapacityReservationSpecification :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplateCapacityReservationSpecificationResponse)
rCapacityReservationSpecification = Lens.field @"capacityReservationSpecification"
{-# DEPRECATED rCapacityReservationSpecification "Use generic-lens or generic-optics with 'capacityReservationSpecification' instead." #-}

-- | The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'cpuOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCpuOptions :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplateCpuOptions)
rCpuOptions = Lens.field @"cpuOptions"
{-# DEPRECATED rCpuOptions "Use generic-lens or generic-optics with 'cpuOptions' instead." #-}

-- | The credit option for CPU usage of the instance.
--
-- /Note:/ Consider using 'creditSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCreditSpecification :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.CreditSpecification)
rCreditSpecification = Lens.field @"creditSpecification"
{-# DEPRECATED rCreditSpecification "Use generic-lens or generic-optics with 'creditSpecification' instead." #-}

-- | If set to @true@ , indicates that the instance cannot be terminated using the Amazon EC2 console, command line tool, or API.
--
-- /Note:/ Consider using 'disableApiTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDisableApiTermination :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Bool)
rDisableApiTermination = Lens.field @"disableApiTermination"
{-# DEPRECATED rDisableApiTermination "Use generic-lens or generic-optics with 'disableApiTermination' instead." #-}

-- | Indicates whether the instance is optimized for Amazon EBS I/O.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEbsOptimized :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Bool)
rEbsOptimized = Lens.field @"ebsOptimized"
{-# DEPRECATED rEbsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The elastic GPU specification.
--
-- /Note:/ Consider using 'elasticGpuSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rElasticGpuSpecifications :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Types.ElasticGpuSpecificationResponse])
rElasticGpuSpecifications = Lens.field @"elasticGpuSpecifications"
{-# DEPRECATED rElasticGpuSpecifications "Use generic-lens or generic-optics with 'elasticGpuSpecifications' instead." #-}

-- | The elastic inference accelerator for the instance.
--
-- /Note:/ Consider using 'elasticInferenceAccelerators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rElasticInferenceAccelerators :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Types.LaunchTemplateElasticInferenceAcceleratorResponse])
rElasticInferenceAccelerators = Lens.field @"elasticInferenceAccelerators"
{-# DEPRECATED rElasticInferenceAccelerators "Use generic-lens or generic-optics with 'elasticInferenceAccelerators' instead." #-}

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
-- /Note:/ Consider using 'enclaveOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEnclaveOptions :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplateEnclaveOptions)
rEnclaveOptions = Lens.field @"enclaveOptions"
{-# DEPRECATED rEnclaveOptions "Use generic-lens or generic-optics with 'enclaveOptions' instead." #-}

-- | Indicates whether an instance is configured for hibernation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'hibernationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rHibernationOptions :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplateHibernationOptions)
rHibernationOptions = Lens.field @"hibernationOptions"
{-# DEPRECATED rHibernationOptions "Use generic-lens or generic-optics with 'hibernationOptions' instead." #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rIamInstanceProfile :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplateIamInstanceProfileSpecification)
rIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# DEPRECATED rIamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The ID of the AMI that was used to launch the instance.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rImageId :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.String)
rImageId = Lens.field @"imageId"
{-# DEPRECATED rImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- /Note:/ Consider using 'instanceInitiatedShutdownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceInitiatedShutdownBehavior :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.ShutdownBehavior)
rInstanceInitiatedShutdownBehavior = Lens.field @"instanceInitiatedShutdownBehavior"
{-# DEPRECATED rInstanceInitiatedShutdownBehavior "Use generic-lens or generic-optics with 'instanceInitiatedShutdownBehavior' instead." #-}

-- | The market (purchasing) option for the instances.
--
-- /Note:/ Consider using 'instanceMarketOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceMarketOptions :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplateInstanceMarketOptions)
rInstanceMarketOptions = Lens.field @"instanceMarketOptions"
{-# DEPRECATED rInstanceMarketOptions "Use generic-lens or generic-optics with 'instanceMarketOptions' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceType :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.InstanceType)
rInstanceType = Lens.field @"instanceType"
{-# DEPRECATED rInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The ID of the kernel, if applicable.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rKernelId :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.String)
rKernelId = Lens.field @"kernelId"
{-# DEPRECATED rKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rKeyName :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.String)
rKeyName = Lens.field @"keyName"
{-# DEPRECATED rKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The license configurations.
--
-- /Note:/ Consider using 'licenseSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLicenseSpecifications :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Types.LaunchTemplateLicenseConfiguration])
rLicenseSpecifications = Lens.field @"licenseSpecifications"
{-# DEPRECATED rLicenseSpecifications "Use generic-lens or generic-optics with 'licenseSpecifications' instead." #-}

-- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'metadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rMetadataOptions :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplateInstanceMetadataOptions)
rMetadataOptions = Lens.field @"metadataOptions"
{-# DEPRECATED rMetadataOptions "Use generic-lens or generic-optics with 'metadataOptions' instead." #-}

-- | The monitoring for the instance.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rMonitoring :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplatesMonitoring)
rMonitoring = Lens.field @"monitoring"
{-# DEPRECATED rMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | The network interfaces.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rNetworkInterfaces :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Types.LaunchTemplateInstanceNetworkInterfaceSpecification])
rNetworkInterfaces = Lens.field @"networkInterfaces"
{-# DEPRECATED rNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The placement of the instance.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPlacement :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplatePlacement)
rPlacement = Lens.field @"placement"
{-# DEPRECATED rPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

-- | The ID of the RAM disk, if applicable.
--
-- /Note:/ Consider using 'ramDiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRamDiskId :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.String)
rRamDiskId = Lens.field @"ramDiskId"
{-# DEPRECATED rRamDiskId "Use generic-lens or generic-optics with 'ramDiskId' instead." #-}

-- | The security group IDs.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSecurityGroupIds :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Types.String])
rSecurityGroupIds = Lens.field @"securityGroupIds"
{-# DEPRECATED rSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The security group names.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSecurityGroups :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Types.String])
rSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED rSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The tags.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTagSpecifications :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Types.LaunchTemplateTagSpecification])
rTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED rTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The user data for the instance.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rUserData :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.String)
rUserData = Lens.field @"userData"
{-# DEPRECATED rUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

instance Core.FromXML ResponseLaunchTemplateData where
  parseXML x =
    ResponseLaunchTemplateData'
      Core.<$> ( x Core..@? "blockDeviceMappingSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "capacityReservationSpecification")
      Core.<*> (x Core..@? "cpuOptions")
      Core.<*> (x Core..@? "creditSpecification")
      Core.<*> (x Core..@? "disableApiTermination")
      Core.<*> (x Core..@? "ebsOptimized")
      Core.<*> ( x Core..@? "elasticGpuSpecificationSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> ( x Core..@? "elasticInferenceAcceleratorSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "enclaveOptions")
      Core.<*> (x Core..@? "hibernationOptions")
      Core.<*> (x Core..@? "iamInstanceProfile")
      Core.<*> (x Core..@? "imageId")
      Core.<*> (x Core..@? "instanceInitiatedShutdownBehavior")
      Core.<*> (x Core..@? "instanceMarketOptions")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "kernelId")
      Core.<*> (x Core..@? "keyName")
      Core.<*> (x Core..@? "licenseSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "metadataOptions")
      Core.<*> (x Core..@? "monitoring")
      Core.<*> ( x Core..@? "networkInterfaceSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "placement")
      Core.<*> (x Core..@? "ramDiskId")
      Core.<*> ( x Core..@? "securityGroupIdSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "securityGroupSet" Core..<@> Core.parseXMLList "item")
      Core.<*> ( x Core..@? "tagSpecificationSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "userData")
