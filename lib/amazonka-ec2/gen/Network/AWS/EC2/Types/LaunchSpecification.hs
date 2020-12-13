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
    lsSecurityGroups,
    lsKeyName,
    lsNetworkInterfaces,
    lsRAMDiskId,
    lsSubnetId,
    lsKernelId,
    lsInstanceType,
    lsEBSOptimized,
    lsUserData,
    lsMonitoring,
    lsIAMInstanceProfile,
    lsImageId,
    lsAddressingType,
    lsBlockDeviceMappings,
    lsPlacement,
  )
where

import Network.AWS.EC2.Types.BlockDeviceMapping
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.IAMInstanceProfileSpecification
import Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.RunInstancesMonitoringEnabled
import Network.AWS.EC2.Types.SpotPlacement
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the launch specification for an instance.
--
-- /See:/ 'mkLaunchSpecification' smart constructor.
data LaunchSpecification = LaunchSpecification'
  { -- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
    securityGroups :: Lude.Maybe [GroupIdentifier],
    -- | The name of the key pair.
    keyName :: Lude.Maybe Lude.Text,
    -- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
    networkInterfaces :: Lude.Maybe [InstanceNetworkInterfaceSpecification],
    -- | The ID of the RAM disk.
    ramdiskId :: Lude.Maybe Lude.Text,
    -- | The ID of the subnet in which to launch the instance.
    subnetId :: Lude.Maybe Lude.Text,
    -- | The ID of the kernel.
    kernelId :: Lude.Maybe Lude.Text,
    -- | The instance type.
    instanceType :: Lude.Maybe InstanceType,
    -- | Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
    --
    -- Default: @false@
    ebsOptimized :: Lude.Maybe Lude.Bool,
    -- | The Base64-encoded user data for the instance.
    userData :: Lude.Maybe Lude.Text,
    monitoring :: Lude.Maybe RunInstancesMonitoringEnabled,
    -- | The IAM instance profile.
    iamInstanceProfile :: Lude.Maybe IAMInstanceProfileSpecification,
    -- | The ID of the AMI.
    imageId :: Lude.Maybe Lude.Text,
    -- | Deprecated.
    addressingType :: Lude.Maybe Lude.Text,
    -- | One or more block device mapping entries.
    blockDeviceMappings :: Lude.Maybe [BlockDeviceMapping],
    -- | The placement information for the instance.
    placement :: Lude.Maybe SpotPlacement
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchSpecification' with the minimum fields required to make a request.
--
-- * 'securityGroups' - One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
-- * 'keyName' - The name of the key pair.
-- * 'networkInterfaces' - One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
-- * 'ramdiskId' - The ID of the RAM disk.
-- * 'subnetId' - The ID of the subnet in which to launch the instance.
-- * 'kernelId' - The ID of the kernel.
-- * 'instanceType' - The instance type.
-- * 'ebsOptimized' - Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
-- * 'userData' - The Base64-encoded user data for the instance.
-- * 'monitoring' -
-- * 'iamInstanceProfile' - The IAM instance profile.
-- * 'imageId' - The ID of the AMI.
-- * 'addressingType' - Deprecated.
-- * 'blockDeviceMappings' - One or more block device mapping entries.
-- * 'placement' - The placement information for the instance.
mkLaunchSpecification ::
  LaunchSpecification
mkLaunchSpecification =
  LaunchSpecification'
    { securityGroups = Lude.Nothing,
      keyName = Lude.Nothing,
      networkInterfaces = Lude.Nothing,
      ramdiskId = Lude.Nothing,
      subnetId = Lude.Nothing,
      kernelId = Lude.Nothing,
      instanceType = Lude.Nothing,
      ebsOptimized = Lude.Nothing,
      userData = Lude.Nothing,
      monitoring = Lude.Nothing,
      iamInstanceProfile = Lude.Nothing,
      imageId = Lude.Nothing,
      addressingType = Lude.Nothing,
      blockDeviceMappings = Lude.Nothing,
      placement = Lude.Nothing
    }

-- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsSecurityGroups :: Lens.Lens' LaunchSpecification (Lude.Maybe [GroupIdentifier])
lsSecurityGroups = Lens.lens (securityGroups :: LaunchSpecification -> Lude.Maybe [GroupIdentifier]) (\s a -> s {securityGroups = a} :: LaunchSpecification)
{-# DEPRECATED lsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsKeyName :: Lens.Lens' LaunchSpecification (Lude.Maybe Lude.Text)
lsKeyName = Lens.lens (keyName :: LaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {keyName = a} :: LaunchSpecification)
{-# DEPRECATED lsKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNetworkInterfaces :: Lens.Lens' LaunchSpecification (Lude.Maybe [InstanceNetworkInterfaceSpecification])
lsNetworkInterfaces = Lens.lens (networkInterfaces :: LaunchSpecification -> Lude.Maybe [InstanceNetworkInterfaceSpecification]) (\s a -> s {networkInterfaces = a} :: LaunchSpecification)
{-# DEPRECATED lsNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The ID of the RAM disk.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsRAMDiskId :: Lens.Lens' LaunchSpecification (Lude.Maybe Lude.Text)
lsRAMDiskId = Lens.lens (ramdiskId :: LaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {ramdiskId = a} :: LaunchSpecification)
{-# DEPRECATED lsRAMDiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The ID of the subnet in which to launch the instance.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsSubnetId :: Lens.Lens' LaunchSpecification (Lude.Maybe Lude.Text)
lsSubnetId = Lens.lens (subnetId :: LaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: LaunchSpecification)
{-# DEPRECATED lsSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The ID of the kernel.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsKernelId :: Lens.Lens' LaunchSpecification (Lude.Maybe Lude.Text)
lsKernelId = Lens.lens (kernelId :: LaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {kernelId = a} :: LaunchSpecification)
{-# DEPRECATED lsKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsInstanceType :: Lens.Lens' LaunchSpecification (Lude.Maybe InstanceType)
lsInstanceType = Lens.lens (instanceType :: LaunchSpecification -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: LaunchSpecification)
{-# DEPRECATED lsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsEBSOptimized :: Lens.Lens' LaunchSpecification (Lude.Maybe Lude.Bool)
lsEBSOptimized = Lens.lens (ebsOptimized :: LaunchSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: LaunchSpecification)
{-# DEPRECATED lsEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The Base64-encoded user data for the instance.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsUserData :: Lens.Lens' LaunchSpecification (Lude.Maybe Lude.Text)
lsUserData = Lens.lens (userData :: LaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {userData = a} :: LaunchSpecification)
{-# DEPRECATED lsUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMonitoring :: Lens.Lens' LaunchSpecification (Lude.Maybe RunInstancesMonitoringEnabled)
lsMonitoring = Lens.lens (monitoring :: LaunchSpecification -> Lude.Maybe RunInstancesMonitoringEnabled) (\s a -> s {monitoring = a} :: LaunchSpecification)
{-# DEPRECATED lsMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsIAMInstanceProfile :: Lens.Lens' LaunchSpecification (Lude.Maybe IAMInstanceProfileSpecification)
lsIAMInstanceProfile = Lens.lens (iamInstanceProfile :: LaunchSpecification -> Lude.Maybe IAMInstanceProfileSpecification) (\s a -> s {iamInstanceProfile = a} :: LaunchSpecification)
{-# DEPRECATED lsIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsImageId :: Lens.Lens' LaunchSpecification (Lude.Maybe Lude.Text)
lsImageId = Lens.lens (imageId :: LaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: LaunchSpecification)
{-# DEPRECATED lsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | Deprecated.
--
-- /Note:/ Consider using 'addressingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsAddressingType :: Lens.Lens' LaunchSpecification (Lude.Maybe Lude.Text)
lsAddressingType = Lens.lens (addressingType :: LaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {addressingType = a} :: LaunchSpecification)
{-# DEPRECATED lsAddressingType "Use generic-lens or generic-optics with 'addressingType' instead." #-}

-- | One or more block device mapping entries.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsBlockDeviceMappings :: Lens.Lens' LaunchSpecification (Lude.Maybe [BlockDeviceMapping])
lsBlockDeviceMappings = Lens.lens (blockDeviceMappings :: LaunchSpecification -> Lude.Maybe [BlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: LaunchSpecification)
{-# DEPRECATED lsBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The placement information for the instance.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsPlacement :: Lens.Lens' LaunchSpecification (Lude.Maybe SpotPlacement)
lsPlacement = Lens.lens (placement :: LaunchSpecification -> Lude.Maybe SpotPlacement) (\s a -> s {placement = a} :: LaunchSpecification)
{-# DEPRECATED lsPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

instance Lude.FromXML LaunchSpecification where
  parseXML x =
    LaunchSpecification'
      Lude.<$> ( x Lude..@? "groupSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "keyName")
      Lude.<*> ( x Lude..@? "networkInterfaceSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "ramdiskId")
      Lude.<*> (x Lude..@? "subnetId")
      Lude.<*> (x Lude..@? "kernelId")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "ebsOptimized")
      Lude.<*> (x Lude..@? "userData")
      Lude.<*> (x Lude..@? "monitoring")
      Lude.<*> (x Lude..@? "iamInstanceProfile")
      Lude.<*> (x Lude..@? "imageId")
      Lude.<*> (x Lude..@? "addressingType")
      Lude.<*> ( x Lude..@? "blockDeviceMapping" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "placement")
