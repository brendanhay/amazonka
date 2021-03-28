{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetLaunchSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotFleetLaunchSpecification
  ( SpotFleetLaunchSpecification (..)
  -- * Smart constructor
  , mkSpotFleetLaunchSpecification
  -- * Lenses
  , sflsAddressingType
  , sflsBlockDeviceMappings
  , sflsEbsOptimized
  , sflsIamInstanceProfile
  , sflsImageId
  , sflsInstanceType
  , sflsKernelId
  , sflsKeyName
  , sflsMonitoring
  , sflsNetworkInterfaces
  , sflsPlacement
  , sflsRamdiskId
  , sflsSecurityGroups
  , sflsSpotPrice
  , sflsSubnetId
  , sflsTagSpecifications
  , sflsUserData
  , sflsWeightedCapacity
  ) where

import qualified Network.AWS.EC2.Types.BlockDeviceMapping as Types
import qualified Network.AWS.EC2.Types.GroupIdentifier as Types
import qualified Network.AWS.EC2.Types.IamInstanceProfileSpecification as Types
import qualified Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification as Types
import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.SpotFleetMonitoring as Types
import qualified Network.AWS.EC2.Types.SpotFleetTagSpecification as Types
import qualified Network.AWS.EC2.Types.SpotPlacement as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the launch specification for one or more Spot Instances. If you include On-Demand capacity in your fleet request or want to specify an EFA network device, you can't use @SpotFleetLaunchSpecification@ ; you must use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_LaunchTemplateConfig.html LaunchTemplateConfig> .
--
-- /See:/ 'mkSpotFleetLaunchSpecification' smart constructor.
data SpotFleetLaunchSpecification = SpotFleetLaunchSpecification'
  { addressingType :: Core.Maybe Core.Text
    -- ^ Deprecated.
  , blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping]
    -- ^ One or more block devices that are mapped to the Spot Instances. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
  , ebsOptimized :: Core.Maybe Core.Bool
    -- ^ Indicates whether the instances are optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@ 
  , iamInstanceProfile :: Core.Maybe Types.IamInstanceProfileSpecification
    -- ^ The IAM instance profile.
  , imageId :: Core.Maybe Core.Text
    -- ^ The ID of the AMI.
  , instanceType :: Core.Maybe Types.InstanceType
    -- ^ The instance type.
  , kernelId :: Core.Maybe Core.Text
    -- ^ The ID of the kernel.
  , keyName :: Core.Maybe Core.Text
    -- ^ The name of the key pair.
  , monitoring :: Core.Maybe Types.SpotFleetMonitoring
    -- ^ Enable or disable monitoring for the instances.
  , networkInterfaces :: Core.Maybe [Types.InstanceNetworkInterfaceSpecification]
    -- ^ One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
  , placement :: Core.Maybe Types.SpotPlacement
    -- ^ The placement information.
  , ramdiskId :: Core.Maybe Core.Text
    -- ^ The ID of the RAM disk. Some kernels require additional drivers at launch. Check the kernel requirements for information about whether you need to specify a RAM disk. To find kernel requirements, refer to the AWS Resource Center and search for the kernel ID.
  , securityGroups :: Core.Maybe [Types.GroupIdentifier]
    -- ^ One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
  , spotPrice :: Core.Maybe Core.Text
    -- ^ The maximum price per unit hour that you are willing to pay for a Spot Instance. If this value is not specified, the default is the Spot price specified for the fleet. To determine the Spot price per unit hour, divide the Spot price by the value of @WeightedCapacity@ .
  , subnetId :: Core.Maybe Core.Text
    -- ^ The IDs of the subnets in which to launch the instances. To specify multiple subnets, separate them using commas; for example, "subnet-1234abcdeexample1, subnet-0987cdef6example2".
  , tagSpecifications :: Core.Maybe [Types.SpotFleetTagSpecification]
    -- ^ The tags to apply during creation.
  , userData :: Core.Maybe Core.Text
    -- ^ The Base64-encoded user data that instances use when starting up.
  , weightedCapacity :: Core.Maybe Core.Double
    -- ^ The number of units provided by the specified instance type. These are the same units that you chose to set the target capacity in terms of instances, or a performance characteristic such as vCPUs, memory, or I/O.
--
-- If the target capacity divided by this value is not a whole number, Amazon EC2 rounds the number of instances to the next whole number. If this value is not specified, the default is 1.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SpotFleetLaunchSpecification' value with any optional fields omitted.
mkSpotFleetLaunchSpecification
    :: SpotFleetLaunchSpecification
mkSpotFleetLaunchSpecification
  = SpotFleetLaunchSpecification'{addressingType = Core.Nothing,
                                  blockDeviceMappings = Core.Nothing, ebsOptimized = Core.Nothing,
                                  iamInstanceProfile = Core.Nothing, imageId = Core.Nothing,
                                  instanceType = Core.Nothing, kernelId = Core.Nothing,
                                  keyName = Core.Nothing, monitoring = Core.Nothing,
                                  networkInterfaces = Core.Nothing, placement = Core.Nothing,
                                  ramdiskId = Core.Nothing, securityGroups = Core.Nothing,
                                  spotPrice = Core.Nothing, subnetId = Core.Nothing,
                                  tagSpecifications = Core.Nothing, userData = Core.Nothing,
                                  weightedCapacity = Core.Nothing}

-- | Deprecated.
--
-- /Note:/ Consider using 'addressingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsAddressingType :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Core.Text)
sflsAddressingType = Lens.field @"addressingType"
{-# INLINEABLE sflsAddressingType #-}
{-# DEPRECATED addressingType "Use generic-lens or generic-optics with 'addressingType' instead"  #-}

-- | One or more block devices that are mapped to the Spot Instances. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsBlockDeviceMappings :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe [Types.BlockDeviceMapping])
sflsBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# INLINEABLE sflsBlockDeviceMappings #-}
{-# DEPRECATED blockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead"  #-}

-- | Indicates whether the instances are optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@ 
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsEbsOptimized :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Core.Bool)
sflsEbsOptimized = Lens.field @"ebsOptimized"
{-# INLINEABLE sflsEbsOptimized #-}
{-# DEPRECATED ebsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead"  #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsIamInstanceProfile :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.IamInstanceProfileSpecification)
sflsIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# INLINEABLE sflsIamInstanceProfile #-}
{-# DEPRECATED iamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead"  #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsImageId :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Core.Text)
sflsImageId = Lens.field @"imageId"
{-# INLINEABLE sflsImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsInstanceType :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.InstanceType)
sflsInstanceType = Lens.field @"instanceType"
{-# INLINEABLE sflsInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The ID of the kernel.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsKernelId :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Core.Text)
sflsKernelId = Lens.field @"kernelId"
{-# INLINEABLE sflsKernelId #-}
{-# DEPRECATED kernelId "Use generic-lens or generic-optics with 'kernelId' instead"  #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsKeyName :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Core.Text)
sflsKeyName = Lens.field @"keyName"
{-# INLINEABLE sflsKeyName #-}
{-# DEPRECATED keyName "Use generic-lens or generic-optics with 'keyName' instead"  #-}

-- | Enable or disable monitoring for the instances.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsMonitoring :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.SpotFleetMonitoring)
sflsMonitoring = Lens.field @"monitoring"
{-# INLINEABLE sflsMonitoring #-}
{-# DEPRECATED monitoring "Use generic-lens or generic-optics with 'monitoring' instead"  #-}

-- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsNetworkInterfaces :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe [Types.InstanceNetworkInterfaceSpecification])
sflsNetworkInterfaces = Lens.field @"networkInterfaces"
{-# INLINEABLE sflsNetworkInterfaces #-}
{-# DEPRECATED networkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead"  #-}

-- | The placement information.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsPlacement :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Types.SpotPlacement)
sflsPlacement = Lens.field @"placement"
{-# INLINEABLE sflsPlacement #-}
{-# DEPRECATED placement "Use generic-lens or generic-optics with 'placement' instead"  #-}

-- | The ID of the RAM disk. Some kernels require additional drivers at launch. Check the kernel requirements for information about whether you need to specify a RAM disk. To find kernel requirements, refer to the AWS Resource Center and search for the kernel ID.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsRamdiskId :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Core.Text)
sflsRamdiskId = Lens.field @"ramdiskId"
{-# INLINEABLE sflsRamdiskId #-}
{-# DEPRECATED ramdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead"  #-}

-- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsSecurityGroups :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe [Types.GroupIdentifier])
sflsSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE sflsSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance. If this value is not specified, the default is the Spot price specified for the fleet. To determine the Spot price per unit hour, divide the Spot price by the value of @WeightedCapacity@ .
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsSpotPrice :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Core.Text)
sflsSpotPrice = Lens.field @"spotPrice"
{-# INLINEABLE sflsSpotPrice #-}
{-# DEPRECATED spotPrice "Use generic-lens or generic-optics with 'spotPrice' instead"  #-}

-- | The IDs of the subnets in which to launch the instances. To specify multiple subnets, separate them using commas; for example, "subnet-1234abcdeexample1, subnet-0987cdef6example2".
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsSubnetId :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Core.Text)
sflsSubnetId = Lens.field @"subnetId"
{-# INLINEABLE sflsSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The tags to apply during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsTagSpecifications :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe [Types.SpotFleetTagSpecification])
sflsTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE sflsTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

-- | The Base64-encoded user data that instances use when starting up.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsUserData :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Core.Text)
sflsUserData = Lens.field @"userData"
{-# INLINEABLE sflsUserData #-}
{-# DEPRECATED userData "Use generic-lens or generic-optics with 'userData' instead"  #-}

-- | The number of units provided by the specified instance type. These are the same units that you chose to set the target capacity in terms of instances, or a performance characteristic such as vCPUs, memory, or I/O.
--
-- If the target capacity divided by this value is not a whole number, Amazon EC2 rounds the number of instances to the next whole number. If this value is not specified, the default is 1.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsWeightedCapacity :: Lens.Lens' SpotFleetLaunchSpecification (Core.Maybe Core.Double)
sflsWeightedCapacity = Lens.field @"weightedCapacity"
{-# INLINEABLE sflsWeightedCapacity #-}
{-# DEPRECATED weightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead"  #-}

instance Core.ToQuery SpotFleetLaunchSpecification where
        toQuery SpotFleetLaunchSpecification{..}
          = Core.maybe Core.mempty (Core.toQueryPair "AddressingType")
              addressingType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "BlockDeviceMapping")
                blockDeviceMappings
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EbsOptimized")
                ebsOptimized
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IamInstanceProfile")
                iamInstanceProfile
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "ImageId") imageId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceType")
                instanceType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KernelId") kernelId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "KeyName") keyName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Monitoring") monitoring
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "NetworkInterfaceSet")
                networkInterfaces
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Placement") placement
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RamdiskId") ramdiskId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "GroupSet") securityGroups
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SpotPrice") spotPrice
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SubnetId") subnetId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecificationSet")
                tagSpecifications
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserData") userData
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "WeightedCapacity")
                weightedCapacity

instance Core.FromXML SpotFleetLaunchSpecification where
        parseXML x
          = SpotFleetLaunchSpecification' Core.<$>
              (x Core..@? "addressingType") Core.<*>
                x Core..@? "blockDeviceMapping" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "ebsOptimized"
                Core.<*> x Core..@? "iamInstanceProfile"
                Core.<*> x Core..@? "imageId"
                Core.<*> x Core..@? "instanceType"
                Core.<*> x Core..@? "kernelId"
                Core.<*> x Core..@? "keyName"
                Core.<*> x Core..@? "monitoring"
                Core.<*>
                x Core..@? "networkInterfaceSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "placement"
                Core.<*> x Core..@? "ramdiskId"
                Core.<*> x Core..@? "groupSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "spotPrice"
                Core.<*> x Core..@? "subnetId"
                Core.<*>
                x Core..@? "tagSpecificationSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "userData"
                Core.<*> x Core..@? "weightedCapacity"
