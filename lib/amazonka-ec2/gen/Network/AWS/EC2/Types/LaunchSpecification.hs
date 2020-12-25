{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchSpecification
  ( LaunchSpecification (..),

    -- * Smart constructor
    mkLaunchSpecification,

    -- * Lenses
    lsAddressingType,
    lsBlockDeviceMappings,
    lsEbsOptimized,
    lsIamInstanceProfile,
    lsImageId,
    lsInstanceType,
    lsKernelId,
    lsKeyName,
    lsMonitoring,
    lsNetworkInterfaces,
    lsPlacement,
    lsRamdiskId,
    lsSecurityGroups,
    lsSubnetId,
    lsUserData,
  )
where

import qualified Network.AWS.EC2.Types.BlockDeviceMapping as Types
import qualified Network.AWS.EC2.Types.GroupIdentifier as Types
import qualified Network.AWS.EC2.Types.IamInstanceProfileSpecification as Types
import qualified Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification as Types
import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.RunInstancesMonitoringEnabled as Types
import qualified Network.AWS.EC2.Types.SpotPlacement as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the launch specification for an instance.
--
-- /See:/ 'mkLaunchSpecification' smart constructor.
data LaunchSpecification = LaunchSpecification'
  { -- | Deprecated.
    addressingType :: Core.Maybe Types.String,
    -- | One or more block device mapping entries.
    blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping],
    -- | Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
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
    monitoring :: Core.Maybe Types.RunInstancesMonitoringEnabled,
    -- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
    networkInterfaces :: Core.Maybe [Types.InstanceNetworkInterfaceSpecification],
    -- | The placement information for the instance.
    placement :: Core.Maybe Types.SpotPlacement,
    -- | The ID of the RAM disk.
    ramdiskId :: Core.Maybe Types.String,
    -- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
    securityGroups :: Core.Maybe [Types.GroupIdentifier],
    -- | The ID of the subnet in which to launch the instance.
    subnetId :: Core.Maybe Types.String,
    -- | The Base64-encoded user data for the instance.
    userData :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchSpecification' value with any optional fields omitted.
mkLaunchSpecification ::
  LaunchSpecification
mkLaunchSpecification =
  LaunchSpecification'
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
      subnetId = Core.Nothing,
      userData = Core.Nothing
    }

-- | Deprecated.
--
-- /Note:/ Consider using 'addressingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsAddressingType :: Lens.Lens' LaunchSpecification (Core.Maybe Types.String)
lsAddressingType = Lens.field @"addressingType"
{-# DEPRECATED lsAddressingType "Use generic-lens or generic-optics with 'addressingType' instead." #-}

-- | One or more block device mapping entries.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsBlockDeviceMappings :: Lens.Lens' LaunchSpecification (Core.Maybe [Types.BlockDeviceMapping])
lsBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# DEPRECATED lsBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsEbsOptimized :: Lens.Lens' LaunchSpecification (Core.Maybe Core.Bool)
lsEbsOptimized = Lens.field @"ebsOptimized"
{-# DEPRECATED lsEbsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsIamInstanceProfile :: Lens.Lens' LaunchSpecification (Core.Maybe Types.IamInstanceProfileSpecification)
lsIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# DEPRECATED lsIamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsImageId :: Lens.Lens' LaunchSpecification (Core.Maybe Types.String)
lsImageId = Lens.field @"imageId"
{-# DEPRECATED lsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsInstanceType :: Lens.Lens' LaunchSpecification (Core.Maybe Types.InstanceType)
lsInstanceType = Lens.field @"instanceType"
{-# DEPRECATED lsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The ID of the kernel.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsKernelId :: Lens.Lens' LaunchSpecification (Core.Maybe Types.String)
lsKernelId = Lens.field @"kernelId"
{-# DEPRECATED lsKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsKeyName :: Lens.Lens' LaunchSpecification (Core.Maybe Types.String)
lsKeyName = Lens.field @"keyName"
{-# DEPRECATED lsKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMonitoring :: Lens.Lens' LaunchSpecification (Core.Maybe Types.RunInstancesMonitoringEnabled)
lsMonitoring = Lens.field @"monitoring"
{-# DEPRECATED lsMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNetworkInterfaces :: Lens.Lens' LaunchSpecification (Core.Maybe [Types.InstanceNetworkInterfaceSpecification])
lsNetworkInterfaces = Lens.field @"networkInterfaces"
{-# DEPRECATED lsNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The placement information for the instance.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsPlacement :: Lens.Lens' LaunchSpecification (Core.Maybe Types.SpotPlacement)
lsPlacement = Lens.field @"placement"
{-# DEPRECATED lsPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

-- | The ID of the RAM disk.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsRamdiskId :: Lens.Lens' LaunchSpecification (Core.Maybe Types.String)
lsRamdiskId = Lens.field @"ramdiskId"
{-# DEPRECATED lsRamdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsSecurityGroups :: Lens.Lens' LaunchSpecification (Core.Maybe [Types.GroupIdentifier])
lsSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED lsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The ID of the subnet in which to launch the instance.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsSubnetId :: Lens.Lens' LaunchSpecification (Core.Maybe Types.String)
lsSubnetId = Lens.field @"subnetId"
{-# DEPRECATED lsSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The Base64-encoded user data for the instance.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsUserData :: Lens.Lens' LaunchSpecification (Core.Maybe Types.String)
lsUserData = Lens.field @"userData"
{-# DEPRECATED lsUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

instance Core.FromXML LaunchSpecification where
  parseXML x =
    LaunchSpecification'
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
      Core.<*> (x Core..@? "subnetId")
      Core.<*> (x Core..@? "userData")
