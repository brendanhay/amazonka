{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetLaunchSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotFleetLaunchSpecification
  ( SpotFleetLaunchSpecification (..),

    -- * Smart constructor
    mkSpotFleetLaunchSpecification,

    -- * Lenses
    sflsAddressingType,
    sflsBlockDeviceMappings,
    sflsEbsOptimized,
    sflsIamInstanceProfile,
    sflsImageId,
    sflsInstanceType,
    sflsKernelId,
    sflsKeyName,
    sflsMonitoring,
    sflsNetworkInterfaces,
    sflsPlacement,
    sflsRamdiskId,
    sflsSecurityGroups,
    sflsSpotPrice,
    sflsSubnetId,
    sflsTagSpecifications,
    sflsUserData,
    sflsWeightedCapacity,
  )
where

import qualified Network.AWS.EC2.Types.BlockDeviceMapping as Types
import qualified Network.AWS.EC2.Types.GroupIdentifier as Types
import qualified Network.AWS.EC2.Types.IamInstanceProfileSpecification as Types
import qualified Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification as Types
import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.SpotFleetMonitoring as Types
import qualified Network.AWS.EC2.Types.SpotFleetTagSpecification as Types
import qualified Network.AWS.EC2.Types.SpotPlacement as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the launch specification for one or more Spot Instances. If you include On-Demand capacity in your fleet request or want to specify an EFA network device, you can't use @SpotFleetLaunchSpecification@ ; you must use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_LaunchTemplateConfig.html LaunchTemplateConfig> .
--
-- /See:/ 'mkSpotFleetLaunchSpecification' smart constructor.
data SpotFleetLaunchSpecification = SpotFleetLaunchSpecification'
  { -- | Deprecated.
    addressingType :: Core.Maybe Types.String,
    -- | One or more block devices that are mapped to the Spot Instances. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
    blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping],
    -- | Indicates whether the instances are optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
    --
    -- Default: @false@
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | The IAM instance profile.
    iamInstanceProfile :: Core.Maybe Types.IamInstanceProfileSpecification,
    -- | The ID of the AMI.
    imageId :: Core.Maybe Types.String,
    -- | The instance type.
    instanceType :: Core.Maybe Types.InstanceType,
    -- | The ID of the kernel.
    kernelId :: Core.Maybe Types.String,
    -- | The name of the key pair.
    keyName :: Core.Maybe Types.String,
    -- | Enable or disable monitoring for the instances.
    monitoring :: Core.Maybe Types.SpotFleetMonitoring,
    -- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
    networkInterfaces :: Core.Maybe [Types.InstanceNetworkInterfaceSpecification],
    -- | The placement information.
    placement :: Core.Maybe Types.SpotPlacement,
    -- | The ID of the RAM disk. Some kernels require additional drivers at launch. Check the kernel requirements for information about whether you need to specify a RAM disk. To find kernel requirements, refer to the AWS Resource Center and search for the kernel ID.
    ramdiskId :: Core.Maybe Types.String,
    -- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
    securityGroups :: Core.Maybe [Types.GroupIdentifier],
    -- | The maximum price per unit hour that you are willing to pay for a Spot Instance. If this value is not specified, the default is the Spot price specified for the fleet. To determine the Spot price per unit hour, divide the Spot price by the value of @WeightedCapacity@ .
    spotPrice :: Core.Maybe Types.String,
    -- | The IDs of the subnets in which to launch the instances. To specify multiple subnets, separate them using commas; for example, "subnet-1234abcdeexample1, subnet-0987cdef6example2".
    subnetId :: Core.Maybe Types.String,
    -- | The tags to apply during creation.
    tagSpecifications :: Core.Maybe [Types.SpotFleetTagSpecification],
    -- | The Base64-encoded user data that instances use when starting up.
    userData :: Core.Maybe Types.String,
    -- | The number of units provided by the specified instance type. These are the same units that you chose to set the target capacity in terms of instances, or a performance characteristic such as vCPUs, memory, or I/O.
    --
    -- If the target capacity divided by this value is not a whole number, Amazon EC2 rounds the number of instances to the next whole number. If this value is not specified, the default is 1.
    weightedCapacity :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SpotFleetLaunchSpecification' value with any optional fields omitted.
mkSpotFleetLaunchSpecification ::
  SpotFleetLaunchSpecification
mkSpotFleetLaunchSpecification =
  SpotFleetLaunchSpecification'
    { addressingType = Core.Nothing,
      blockDeviceMappings = Core.Nothing,
      ebsOptimized = Core.Nothing,
      iamInstanceProfile = Core.Nothing,
      imageId = Core.Nothing,
      instanceType = Core.Nothing,
      kernelId = Core.Nothing,
      keyName = Core.Nothing,
      monitoring = Core.Nothing,
      networkInterfaces = Core.Nothing,
      placement = Core.Nothing,
      ramdiskId = Core.Nothing,
      securityGroups = Core.Nothing,
      spotPrice = Core.Nothing,
      subnetId = Core.Nothing,
      tagSpecifications = Core.Nothing,
      userData = Core.Nothing,
      weightedCapacity = Core.Nothing
    }

-- | Deprecated.
--
-- /Note:/ Consider using 'addressingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsAddressingType :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.String)
sflsAddressingType = Lens.field @"addressingType"
{-# DEPRECATED sflsAddressingType "Use generic-lens or generic-optics with 'addressingType' instead." #-}

-- | One or more block devices that are mapped to the Spot Instances. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsBlockDeviceMappings :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe [Types.BlockDeviceMapping])
sflsBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# DEPRECATED sflsBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | Indicates whether the instances are optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsEbsOptimized :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Core.Bool)
sflsEbsOptimized = Lens.field @"ebsOptimized"
{-# DEPRECATED sflsEbsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsIamInstanceProfile :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.IamInstanceProfileSpecification)
sflsIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# DEPRECATED sflsIamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsImageId :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.String)
sflsImageId = Lens.field @"imageId"
{-# DEPRECATED sflsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsInstanceType :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.InstanceType)
sflsInstanceType = Lens.field @"instanceType"
{-# DEPRECATED sflsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The ID of the kernel.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsKernelId :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.String)
sflsKernelId = Lens.field @"kernelId"
{-# DEPRECATED sflsKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsKeyName :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.String)
sflsKeyName = Lens.field @"keyName"
{-# DEPRECATED sflsKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | Enable or disable monitoring for the instances.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsMonitoring :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.SpotFleetMonitoring)
sflsMonitoring = Lens.field @"monitoring"
{-# DEPRECATED sflsMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsNetworkInterfaces :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe [Types.InstanceNetworkInterfaceSpecification])
sflsNetworkInterfaces = Lens.field @"networkInterfaces"
{-# DEPRECATED sflsNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The placement information.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsPlacement :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.SpotPlacement)
sflsPlacement = Lens.field @"placement"
{-# DEPRECATED sflsPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

-- | The ID of the RAM disk. Some kernels require additional drivers at launch. Check the kernel requirements for information about whether you need to specify a RAM disk. To find kernel requirements, refer to the AWS Resource Center and search for the kernel ID.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsRamdiskId :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.String)
sflsRamdiskId = Lens.field @"ramdiskId"
{-# DEPRECATED sflsRamdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsSecurityGroups :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe [Types.GroupIdentifier])
sflsSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED sflsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance. If this value is not specified, the default is the Spot price specified for the fleet. To determine the Spot price per unit hour, divide the Spot price by the value of @WeightedCapacity@ .
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsSpotPrice :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.String)
sflsSpotPrice = Lens.field @"spotPrice"
{-# DEPRECATED sflsSpotPrice "Use generic-lens or generic-optics with 'spotPrice' instead." #-}

-- | The IDs of the subnets in which to launch the instances. To specify multiple subnets, separate them using commas; for example, "subnet-1234abcdeexample1, subnet-0987cdef6example2".
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsSubnetId :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.String)
sflsSubnetId = Lens.field @"subnetId"
{-# DEPRECATED sflsSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The tags to apply during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsTagSpecifications :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe [Types.SpotFleetTagSpecification])
sflsTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED sflsTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The Base64-encoded user data that instances use when starting up.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsUserData :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.String)
sflsUserData = Lens.field @"userData"
{-# DEPRECATED sflsUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | The number of units provided by the specified instance type. These are the same units that you chose to set the target capacity in terms of instances, or a performance characteristic such as vCPUs, memory, or I/O.
--
-- If the target capacity divided by this value is not a whole number, Amazon EC2 rounds the number of instances to the next whole number. If this value is not specified, the default is 1.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsWeightedCapacity :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Core.Double)
sflsWeightedCapacity = Lens.field @"weightedCapacity"
{-# DEPRECATED sflsWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}

instance Core.FromXML SpotFleetLaunchSpecification where
  parseXML x =
    SpotFleetLaunchSpecification'
      Core.<$> (x Core..@? "addressingType")
      Core.<*> ( x Core..@? "blockDeviceMapping"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "ebsOptimized")
      Core.<*> (x Core..@? "iamInstanceProfile")
      Core.<*> (x Core..@? "imageId")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "kernelId")
      Core.<*> (x Core..@? "keyName")
      Core.<*> (x Core..@? "monitoring")
      Core.<*> ( x Core..@? "networkInterfaceSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "placement")
      Core.<*> (x Core..@? "ramdiskId")
      Core.<*> (x Core..@? "groupSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "spotPrice")
      Core.<*> (x Core..@? "subnetId")
      Core.<*> ( x Core..@? "tagSpecificationSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "userData")
      Core.<*> (x Core..@? "weightedCapacity")
