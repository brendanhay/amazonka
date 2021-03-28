{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchSpecification
  ( LaunchSpecification (..)
  -- * Smart constructor
  , mkLaunchSpecification
  -- * Lenses
  , lsAddressingType
  , lsBlockDeviceMappings
  , lsEbsOptimized
  , lsIamInstanceProfile
  , lsImageId
  , lsInstanceType
  , lsKernelId
  , lsKeyName
  , lsMonitoring
  , lsNetworkInterfaces
  , lsPlacement
  , lsRamdiskId
  , lsSecurityGroups
  , lsSubnetId
  , lsUserData
  ) where

import qualified Network.AWS.EC2.Types.BlockDeviceMapping as Types
import qualified Network.AWS.EC2.Types.GroupIdentifier as Types
import qualified Network.AWS.EC2.Types.IamInstanceProfileSpecification as Types
import qualified Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification as Types
import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.RunInstancesMonitoringEnabled as Types
import qualified Network.AWS.EC2.Types.SpotPlacement as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the launch specification for an instance.
--
-- /See:/ 'mkLaunchSpecification' smart constructor.
data LaunchSpecification = LaunchSpecification'
  { addressingType :: Core.Maybe Core.Text
    -- ^ Deprecated.
  , blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping]
    -- ^ One or more block device mapping entries.
  , ebsOptimized :: Core.Maybe Core.Bool
    -- ^ Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
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
  , monitoring :: Core.Maybe Types.RunInstancesMonitoringEnabled
  , networkInterfaces :: Core.Maybe [Types.InstanceNetworkInterfaceSpecification]
    -- ^ One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
  , placement :: Core.Maybe Types.SpotPlacement
    -- ^ The placement information for the instance.
  , ramdiskId :: Core.Maybe Core.Text
    -- ^ The ID of the RAM disk.
  , securityGroups :: Core.Maybe [Types.GroupIdentifier]
    -- ^ One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
  , subnetId :: Core.Maybe Core.Text
    -- ^ The ID of the subnet in which to launch the instance.
  , userData :: Core.Maybe Core.Text
    -- ^ The Base64-encoded user data for the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchSpecification' value with any optional fields omitted.
mkLaunchSpecification
    :: LaunchSpecification
mkLaunchSpecification
  = LaunchSpecification'{addressingType = Core.Nothing,
                         blockDeviceMappings = Core.Nothing, ebsOptimized = Core.Nothing,
                         iamInstanceProfile = Core.Nothing, imageId = Core.Nothing,
                         instanceType = Core.Nothing, kernelId = Core.Nothing,
                         keyName = Core.Nothing, monitoring = Core.Nothing,
                         networkInterfaces = Core.Nothing, placement = Core.Nothing,
                         ramdiskId = Core.Nothing, securityGroups = Core.Nothing,
                         subnetId = Core.Nothing, userData = Core.Nothing}

-- | Deprecated.
--
-- /Note:/ Consider using 'addressingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsAddressingType :: Lens.Lens' LaunchSpecification (Core.Maybe Core.Text)
lsAddressingType = Lens.field @"addressingType"
{-# INLINEABLE lsAddressingType #-}
{-# DEPRECATED addressingType "Use generic-lens or generic-optics with 'addressingType' instead"  #-}

-- | One or more block device mapping entries.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsBlockDeviceMappings :: Lens.Lens' LaunchSpecification (Core.Maybe [Types.BlockDeviceMapping])
lsBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# INLINEABLE lsBlockDeviceMappings #-}
{-# DEPRECATED blockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead"  #-}

-- | Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@ 
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsEbsOptimized :: Lens.Lens' LaunchSpecification (Core.Maybe Core.Bool)
lsEbsOptimized = Lens.field @"ebsOptimized"
{-# INLINEABLE lsEbsOptimized #-}
{-# DEPRECATED ebsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead"  #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsIamInstanceProfile :: Lens.Lens' LaunchSpecification (Core.Maybe Types.IamInstanceProfileSpecification)
lsIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# INLINEABLE lsIamInstanceProfile #-}
{-# DEPRECATED iamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead"  #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsImageId :: Lens.Lens' LaunchSpecification (Core.Maybe Core.Text)
lsImageId = Lens.field @"imageId"
{-# INLINEABLE lsImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsInstanceType :: Lens.Lens' LaunchSpecification (Core.Maybe Types.InstanceType)
lsInstanceType = Lens.field @"instanceType"
{-# INLINEABLE lsInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The ID of the kernel.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsKernelId :: Lens.Lens' LaunchSpecification (Core.Maybe Core.Text)
lsKernelId = Lens.field @"kernelId"
{-# INLINEABLE lsKernelId #-}
{-# DEPRECATED kernelId "Use generic-lens or generic-optics with 'kernelId' instead"  #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsKeyName :: Lens.Lens' LaunchSpecification (Core.Maybe Core.Text)
lsKeyName = Lens.field @"keyName"
{-# INLINEABLE lsKeyName #-}
{-# DEPRECATED keyName "Use generic-lens or generic-optics with 'keyName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMonitoring :: Lens.Lens' LaunchSpecification (Core.Maybe Types.RunInstancesMonitoringEnabled)
lsMonitoring = Lens.field @"monitoring"
{-# INLINEABLE lsMonitoring #-}
{-# DEPRECATED monitoring "Use generic-lens or generic-optics with 'monitoring' instead"  #-}

-- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNetworkInterfaces :: Lens.Lens' LaunchSpecification (Core.Maybe [Types.InstanceNetworkInterfaceSpecification])
lsNetworkInterfaces = Lens.field @"networkInterfaces"
{-# INLINEABLE lsNetworkInterfaces #-}
{-# DEPRECATED networkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead"  #-}

-- | The placement information for the instance.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsPlacement :: Lens.Lens' LaunchSpecification (Core.Maybe Types.SpotPlacement)
lsPlacement = Lens.field @"placement"
{-# INLINEABLE lsPlacement #-}
{-# DEPRECATED placement "Use generic-lens or generic-optics with 'placement' instead"  #-}

-- | The ID of the RAM disk.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsRamdiskId :: Lens.Lens' LaunchSpecification (Core.Maybe Core.Text)
lsRamdiskId = Lens.field @"ramdiskId"
{-# INLINEABLE lsRamdiskId #-}
{-# DEPRECATED ramdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead"  #-}

-- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsSecurityGroups :: Lens.Lens' LaunchSpecification (Core.Maybe [Types.GroupIdentifier])
lsSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE lsSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

-- | The ID of the subnet in which to launch the instance.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsSubnetId :: Lens.Lens' LaunchSpecification (Core.Maybe Core.Text)
lsSubnetId = Lens.field @"subnetId"
{-# INLINEABLE lsSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The Base64-encoded user data for the instance.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsUserData :: Lens.Lens' LaunchSpecification (Core.Maybe Core.Text)
lsUserData = Lens.field @"userData"
{-# INLINEABLE lsUserData #-}
{-# DEPRECATED userData "Use generic-lens or generic-optics with 'userData' instead"  #-}

instance Core.FromXML LaunchSpecification where
        parseXML x
          = LaunchSpecification' Core.<$>
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
                Core.<*> x Core..@? "subnetId"
                Core.<*> x Core..@? "userData"
