{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ResponseLaunchTemplateData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ResponseLaunchTemplateData
  ( ResponseLaunchTemplateData (..)
  -- * Smart constructor
  , mkResponseLaunchTemplateData
  -- * Lenses
  , rBlockDeviceMappings
  , rCapacityReservationSpecification
  , rCpuOptions
  , rCreditSpecification
  , rDisableApiTermination
  , rEbsOptimized
  , rElasticGpuSpecifications
  , rElasticInferenceAccelerators
  , rEnclaveOptions
  , rHibernationOptions
  , rIamInstanceProfile
  , rImageId
  , rInstanceInitiatedShutdownBehavior
  , rInstanceMarketOptions
  , rInstanceType
  , rKernelId
  , rKeyName
  , rLicenseSpecifications
  , rMetadataOptions
  , rMonitoring
  , rNetworkInterfaces
  , rPlacement
  , rRamDiskId
  , rSecurityGroupIds
  , rSecurityGroups
  , rTagSpecifications
  , rUserData
  ) where

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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The information for a launch template. 
--
-- /See:/ 'mkResponseLaunchTemplateData' smart constructor.
data ResponseLaunchTemplateData = ResponseLaunchTemplateData'
  { blockDeviceMappings :: Core.Maybe [Types.LaunchTemplateBlockDeviceMapping]
    -- ^ The block device mappings.
  , capacityReservationSpecification :: Core.Maybe Types.LaunchTemplateCapacityReservationSpecificationResponse
    -- ^ Information about the Capacity Reservation targeting option.
  , cpuOptions :: Core.Maybe Types.LaunchTemplateCpuOptions
    -- ^ The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
  , creditSpecification :: Core.Maybe Types.CreditSpecification
    -- ^ The credit option for CPU usage of the instance.
  , disableApiTermination :: Core.Maybe Core.Bool
    -- ^ If set to @true@ , indicates that the instance cannot be terminated using the Amazon EC2 console, command line tool, or API.
  , ebsOptimized :: Core.Maybe Core.Bool
    -- ^ Indicates whether the instance is optimized for Amazon EBS I/O. 
  , elasticGpuSpecifications :: Core.Maybe [Types.ElasticGpuSpecificationResponse]
    -- ^ The elastic GPU specification.
  , elasticInferenceAccelerators :: Core.Maybe [Types.LaunchTemplateElasticInferenceAcceleratorResponse]
    -- ^ The elastic inference accelerator for the instance. 
  , enclaveOptions :: Core.Maybe Types.LaunchTemplateEnclaveOptions
    -- ^ Indicates whether the instance is enabled for AWS Nitro Enclaves.
  , hibernationOptions :: Core.Maybe Types.LaunchTemplateHibernationOptions
    -- ^ Indicates whether an instance is configured for hibernation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
  , iamInstanceProfile :: Core.Maybe Types.LaunchTemplateIamInstanceProfileSpecification
    -- ^ The IAM instance profile.
  , imageId :: Core.Maybe Core.Text
    -- ^ The ID of the AMI that was used to launch the instance.
  , instanceInitiatedShutdownBehavior :: Core.Maybe Types.ShutdownBehavior
    -- ^ Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
  , instanceMarketOptions :: Core.Maybe Types.LaunchTemplateInstanceMarketOptions
    -- ^ The market (purchasing) option for the instances.
  , instanceType :: Core.Maybe Types.InstanceType
    -- ^ The instance type.
  , kernelId :: Core.Maybe Core.Text
    -- ^ The ID of the kernel, if applicable.
  , keyName :: Core.Maybe Core.Text
    -- ^ The name of the key pair.
  , licenseSpecifications :: Core.Maybe [Types.LaunchTemplateLicenseConfiguration]
    -- ^ The license configurations.
  , metadataOptions :: Core.Maybe Types.LaunchTemplateInstanceMetadataOptions
    -- ^ The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
  , monitoring :: Core.Maybe Types.LaunchTemplatesMonitoring
    -- ^ The monitoring for the instance.
  , networkInterfaces :: Core.Maybe [Types.LaunchTemplateInstanceNetworkInterfaceSpecification]
    -- ^ The network interfaces.
  , placement :: Core.Maybe Types.LaunchTemplatePlacement
    -- ^ The placement of the instance.
  , ramDiskId :: Core.Maybe Core.Text
    -- ^ The ID of the RAM disk, if applicable.
  , securityGroupIds :: Core.Maybe [Core.Text]
    -- ^ The security group IDs.
  , securityGroups :: Core.Maybe [Core.Text]
    -- ^ The security group names.
  , tagSpecifications :: Core.Maybe [Types.LaunchTemplateTagSpecification]
    -- ^ The tags.
  , userData :: Core.Maybe Core.Text
    -- ^ The user data for the instance. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ResponseLaunchTemplateData' value with any optional fields omitted.
mkResponseLaunchTemplateData
    :: ResponseLaunchTemplateData
mkResponseLaunchTemplateData
  = ResponseLaunchTemplateData'{blockDeviceMappings = Core.Nothing,
                                capacityReservationSpecification = Core.Nothing,
                                cpuOptions = Core.Nothing, creditSpecification = Core.Nothing,
                                disableApiTermination = Core.Nothing, ebsOptimized = Core.Nothing,
                                elasticGpuSpecifications = Core.Nothing,
                                elasticInferenceAccelerators = Core.Nothing,
                                enclaveOptions = Core.Nothing, hibernationOptions = Core.Nothing,
                                iamInstanceProfile = Core.Nothing, imageId = Core.Nothing,
                                instanceInitiatedShutdownBehavior = Core.Nothing,
                                instanceMarketOptions = Core.Nothing, instanceType = Core.Nothing,
                                kernelId = Core.Nothing, keyName = Core.Nothing,
                                licenseSpecifications = Core.Nothing,
                                metadataOptions = Core.Nothing, monitoring = Core.Nothing,
                                networkInterfaces = Core.Nothing, placement = Core.Nothing,
                                ramDiskId = Core.Nothing, securityGroupIds = Core.Nothing,
                                securityGroups = Core.Nothing, tagSpecifications = Core.Nothing,
                                userData = Core.Nothing}

-- | The block device mappings.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rBlockDeviceMappings :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Types.LaunchTemplateBlockDeviceMapping])
rBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# INLINEABLE rBlockDeviceMappings #-}
{-# DEPRECATED blockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead"  #-}

-- | Information about the Capacity Reservation targeting option.
--
-- /Note:/ Consider using 'capacityReservationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCapacityReservationSpecification :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplateCapacityReservationSpecificationResponse)
rCapacityReservationSpecification = Lens.field @"capacityReservationSpecification"
{-# INLINEABLE rCapacityReservationSpecification #-}
{-# DEPRECATED capacityReservationSpecification "Use generic-lens or generic-optics with 'capacityReservationSpecification' instead"  #-}

-- | The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'cpuOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCpuOptions :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplateCpuOptions)
rCpuOptions = Lens.field @"cpuOptions"
{-# INLINEABLE rCpuOptions #-}
{-# DEPRECATED cpuOptions "Use generic-lens or generic-optics with 'cpuOptions' instead"  #-}

-- | The credit option for CPU usage of the instance.
--
-- /Note:/ Consider using 'creditSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCreditSpecification :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.CreditSpecification)
rCreditSpecification = Lens.field @"creditSpecification"
{-# INLINEABLE rCreditSpecification #-}
{-# DEPRECATED creditSpecification "Use generic-lens or generic-optics with 'creditSpecification' instead"  #-}

-- | If set to @true@ , indicates that the instance cannot be terminated using the Amazon EC2 console, command line tool, or API.
--
-- /Note:/ Consider using 'disableApiTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDisableApiTermination :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Bool)
rDisableApiTermination = Lens.field @"disableApiTermination"
{-# INLINEABLE rDisableApiTermination #-}
{-# DEPRECATED disableApiTermination "Use generic-lens or generic-optics with 'disableApiTermination' instead"  #-}

-- | Indicates whether the instance is optimized for Amazon EBS I/O. 
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEbsOptimized :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Bool)
rEbsOptimized = Lens.field @"ebsOptimized"
{-# INLINEABLE rEbsOptimized #-}
{-# DEPRECATED ebsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead"  #-}

-- | The elastic GPU specification.
--
-- /Note:/ Consider using 'elasticGpuSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rElasticGpuSpecifications :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Types.ElasticGpuSpecificationResponse])
rElasticGpuSpecifications = Lens.field @"elasticGpuSpecifications"
{-# INLINEABLE rElasticGpuSpecifications #-}
{-# DEPRECATED elasticGpuSpecifications "Use generic-lens or generic-optics with 'elasticGpuSpecifications' instead"  #-}

-- | The elastic inference accelerator for the instance. 
--
-- /Note:/ Consider using 'elasticInferenceAccelerators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rElasticInferenceAccelerators :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Types.LaunchTemplateElasticInferenceAcceleratorResponse])
rElasticInferenceAccelerators = Lens.field @"elasticInferenceAccelerators"
{-# INLINEABLE rElasticInferenceAccelerators #-}
{-# DEPRECATED elasticInferenceAccelerators "Use generic-lens or generic-optics with 'elasticInferenceAccelerators' instead"  #-}

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
-- /Note:/ Consider using 'enclaveOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEnclaveOptions :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplateEnclaveOptions)
rEnclaveOptions = Lens.field @"enclaveOptions"
{-# INLINEABLE rEnclaveOptions #-}
{-# DEPRECATED enclaveOptions "Use generic-lens or generic-optics with 'enclaveOptions' instead"  #-}

-- | Indicates whether an instance is configured for hibernation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'hibernationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rHibernationOptions :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplateHibernationOptions)
rHibernationOptions = Lens.field @"hibernationOptions"
{-# INLINEABLE rHibernationOptions #-}
{-# DEPRECATED hibernationOptions "Use generic-lens or generic-optics with 'hibernationOptions' instead"  #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rIamInstanceProfile :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplateIamInstanceProfileSpecification)
rIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# INLINEABLE rIamInstanceProfile #-}
{-# DEPRECATED iamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead"  #-}

-- | The ID of the AMI that was used to launch the instance.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rImageId :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Text)
rImageId = Lens.field @"imageId"
{-# INLINEABLE rImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- /Note:/ Consider using 'instanceInitiatedShutdownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceInitiatedShutdownBehavior :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.ShutdownBehavior)
rInstanceInitiatedShutdownBehavior = Lens.field @"instanceInitiatedShutdownBehavior"
{-# INLINEABLE rInstanceInitiatedShutdownBehavior #-}
{-# DEPRECATED instanceInitiatedShutdownBehavior "Use generic-lens or generic-optics with 'instanceInitiatedShutdownBehavior' instead"  #-}

-- | The market (purchasing) option for the instances.
--
-- /Note:/ Consider using 'instanceMarketOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceMarketOptions :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplateInstanceMarketOptions)
rInstanceMarketOptions = Lens.field @"instanceMarketOptions"
{-# INLINEABLE rInstanceMarketOptions #-}
{-# DEPRECATED instanceMarketOptions "Use generic-lens or generic-optics with 'instanceMarketOptions' instead"  #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceType :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.InstanceType)
rInstanceType = Lens.field @"instanceType"
{-# INLINEABLE rInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The ID of the kernel, if applicable.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rKernelId :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Text)
rKernelId = Lens.field @"kernelId"
{-# INLINEABLE rKernelId #-}
{-# DEPRECATED kernelId "Use generic-lens or generic-optics with 'kernelId' instead"  #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rKeyName :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Text)
rKeyName = Lens.field @"keyName"
{-# INLINEABLE rKeyName #-}
{-# DEPRECATED keyName "Use generic-lens or generic-optics with 'keyName' instead"  #-}

-- | The license configurations.
--
-- /Note:/ Consider using 'licenseSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLicenseSpecifications :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Types.LaunchTemplateLicenseConfiguration])
rLicenseSpecifications = Lens.field @"licenseSpecifications"
{-# INLINEABLE rLicenseSpecifications #-}
{-# DEPRECATED licenseSpecifications "Use generic-lens or generic-optics with 'licenseSpecifications' instead"  #-}

-- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'metadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rMetadataOptions :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplateInstanceMetadataOptions)
rMetadataOptions = Lens.field @"metadataOptions"
{-# INLINEABLE rMetadataOptions #-}
{-# DEPRECATED metadataOptions "Use generic-lens or generic-optics with 'metadataOptions' instead"  #-}

-- | The monitoring for the instance.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rMonitoring :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplatesMonitoring)
rMonitoring = Lens.field @"monitoring"
{-# INLINEABLE rMonitoring #-}
{-# DEPRECATED monitoring "Use generic-lens or generic-optics with 'monitoring' instead"  #-}

-- | The network interfaces.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rNetworkInterfaces :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Types.LaunchTemplateInstanceNetworkInterfaceSpecification])
rNetworkInterfaces = Lens.field @"networkInterfaces"
{-# INLINEABLE rNetworkInterfaces #-}
{-# DEPRECATED networkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead"  #-}

-- | The placement of the instance.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPlacement :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Types.LaunchTemplatePlacement)
rPlacement = Lens.field @"placement"
{-# INLINEABLE rPlacement #-}
{-# DEPRECATED placement "Use generic-lens or generic-optics with 'placement' instead"  #-}

-- | The ID of the RAM disk, if applicable.
--
-- /Note:/ Consider using 'ramDiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRamDiskId :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Text)
rRamDiskId = Lens.field @"ramDiskId"
{-# INLINEABLE rRamDiskId #-}
{-# DEPRECATED ramDiskId "Use generic-lens or generic-optics with 'ramDiskId' instead"  #-}

-- | The security group IDs.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSecurityGroupIds :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Core.Text])
rSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE rSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | The security group names.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSecurityGroups :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Core.Text])
rSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE rSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

-- | The tags.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTagSpecifications :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Types.LaunchTemplateTagSpecification])
rTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE rTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

-- | The user data for the instance. 
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rUserData :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Text)
rUserData = Lens.field @"userData"
{-# INLINEABLE rUserData #-}
{-# DEPRECATED userData "Use generic-lens or generic-optics with 'userData' instead"  #-}

instance Core.FromXML ResponseLaunchTemplateData where
        parseXML x
          = ResponseLaunchTemplateData' Core.<$>
              (x Core..@? "blockDeviceMappingSet" Core..<@>
                 Core.parseXMLList "item")
                Core.<*> x Core..@? "capacityReservationSpecification"
                Core.<*> x Core..@? "cpuOptions"
                Core.<*> x Core..@? "creditSpecification"
                Core.<*> x Core..@? "disableApiTermination"
                Core.<*> x Core..@? "ebsOptimized"
                Core.<*>
                x Core..@? "elasticGpuSpecificationSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*>
                x Core..@? "elasticInferenceAcceleratorSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "enclaveOptions"
                Core.<*> x Core..@? "hibernationOptions"
                Core.<*> x Core..@? "iamInstanceProfile"
                Core.<*> x Core..@? "imageId"
                Core.<*> x Core..@? "instanceInitiatedShutdownBehavior"
                Core.<*> x Core..@? "instanceMarketOptions"
                Core.<*> x Core..@? "instanceType"
                Core.<*> x Core..@? "kernelId"
                Core.<*> x Core..@? "keyName"
                Core.<*> x Core..@? "licenseSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "metadataOptions"
                Core.<*> x Core..@? "monitoring"
                Core.<*>
                x Core..@? "networkInterfaceSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "placement"
                Core.<*> x Core..@? "ramDiskId"
                Core.<*>
                x Core..@? "securityGroupIdSet" Core..<@> Core.parseXMLList "item"
                Core.<*>
                x Core..@? "securityGroupSet" Core..<@> Core.parseXMLList "item"
                Core.<*>
                x Core..@? "tagSpecificationSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "userData"
