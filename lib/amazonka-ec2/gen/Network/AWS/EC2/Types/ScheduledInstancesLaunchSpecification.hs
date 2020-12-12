{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesLaunchSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesLaunchSpecification
  ( ScheduledInstancesLaunchSpecification (..),

    -- * Smart constructor
    mkScheduledInstancesLaunchSpecification,

    -- * Lenses
    silsSecurityGroupIds,
    silsKeyName,
    silsNetworkInterfaces,
    silsRAMDiskId,
    silsSubnetId,
    silsKernelId,
    silsInstanceType,
    silsEBSOptimized,
    silsUserData,
    silsMonitoring,
    silsIAMInstanceProfile,
    silsBlockDeviceMappings,
    silsPlacement,
    silsImageId,
  )
where

import Network.AWS.EC2.Types.ScheduledInstancesBlockDeviceMapping
import Network.AWS.EC2.Types.ScheduledInstancesIAMInstanceProfile
import Network.AWS.EC2.Types.ScheduledInstancesMonitoring
import Network.AWS.EC2.Types.ScheduledInstancesNetworkInterface
import Network.AWS.EC2.Types.ScheduledInstancesPlacement
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the launch specification for a Scheduled Instance.
--
-- If you are launching the Scheduled Instance in EC2-VPC, you must specify the ID of the subnet. You can specify the subnet using either @SubnetId@ or @NetworkInterface@ .
--
-- /See:/ 'mkScheduledInstancesLaunchSpecification' smart constructor.
data ScheduledInstancesLaunchSpecification = ScheduledInstancesLaunchSpecification'
  { securityGroupIds ::
      Lude.Maybe
        [Lude.Text],
    keyName ::
      Lude.Maybe
        Lude.Text,
    networkInterfaces ::
      Lude.Maybe
        [ScheduledInstancesNetworkInterface],
    ramdiskId ::
      Lude.Maybe
        Lude.Text,
    subnetId ::
      Lude.Maybe
        Lude.Text,
    kernelId ::
      Lude.Maybe
        Lude.Text,
    instanceType ::
      Lude.Maybe
        Lude.Text,
    ebsOptimized ::
      Lude.Maybe
        Lude.Bool,
    userData ::
      Lude.Maybe
        Lude.Text,
    monitoring ::
      Lude.Maybe
        ScheduledInstancesMonitoring,
    iamInstanceProfile ::
      Lude.Maybe
        ScheduledInstancesIAMInstanceProfile,
    blockDeviceMappings ::
      Lude.Maybe
        [ScheduledInstancesBlockDeviceMapping],
    placement ::
      Lude.Maybe
        ScheduledInstancesPlacement,
    imageId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledInstancesLaunchSpecification' with the minimum fields required to make a request.
--
-- * 'blockDeviceMappings' - The block device mapping entries.
-- * 'ebsOptimized' - Indicates whether the instances are optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance.
--
-- Default: @false@
-- * 'iamInstanceProfile' - The IAM instance profile.
-- * 'imageId' - The ID of the Amazon Machine Image (AMI).
-- * 'instanceType' - The instance type.
-- * 'kernelId' - The ID of the kernel.
-- * 'keyName' - The name of the key pair.
-- * 'monitoring' - Enable or disable monitoring for the instances.
-- * 'networkInterfaces' - The network interfaces.
-- * 'placement' - The placement information.
-- * 'ramdiskId' - The ID of the RAM disk.
-- * 'securityGroupIds' - The IDs of the security groups.
-- * 'subnetId' - The ID of the subnet in which to launch the instances.
-- * 'userData' - The base64-encoded MIME user data.
mkScheduledInstancesLaunchSpecification ::
  -- | 'imageId'
  Lude.Text ->
  ScheduledInstancesLaunchSpecification
mkScheduledInstancesLaunchSpecification pImageId_ =
  ScheduledInstancesLaunchSpecification'
    { securityGroupIds =
        Lude.Nothing,
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
      blockDeviceMappings = Lude.Nothing,
      placement = Lude.Nothing,
      imageId = pImageId_
    }

-- | The IDs of the security groups.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
silsSecurityGroupIds :: Lens.Lens' ScheduledInstancesLaunchSpecification (Lude.Maybe [Lude.Text])
silsSecurityGroupIds = Lens.lens (securityGroupIds :: ScheduledInstancesLaunchSpecification -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: ScheduledInstancesLaunchSpecification)
{-# DEPRECATED silsSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
silsKeyName :: Lens.Lens' ScheduledInstancesLaunchSpecification (Lude.Maybe Lude.Text)
silsKeyName = Lens.lens (keyName :: ScheduledInstancesLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {keyName = a} :: ScheduledInstancesLaunchSpecification)
{-# DEPRECATED silsKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The network interfaces.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
silsNetworkInterfaces :: Lens.Lens' ScheduledInstancesLaunchSpecification (Lude.Maybe [ScheduledInstancesNetworkInterface])
silsNetworkInterfaces = Lens.lens (networkInterfaces :: ScheduledInstancesLaunchSpecification -> Lude.Maybe [ScheduledInstancesNetworkInterface]) (\s a -> s {networkInterfaces = a} :: ScheduledInstancesLaunchSpecification)
{-# DEPRECATED silsNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The ID of the RAM disk.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
silsRAMDiskId :: Lens.Lens' ScheduledInstancesLaunchSpecification (Lude.Maybe Lude.Text)
silsRAMDiskId = Lens.lens (ramdiskId :: ScheduledInstancesLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {ramdiskId = a} :: ScheduledInstancesLaunchSpecification)
{-# DEPRECATED silsRAMDiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The ID of the subnet in which to launch the instances.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
silsSubnetId :: Lens.Lens' ScheduledInstancesLaunchSpecification (Lude.Maybe Lude.Text)
silsSubnetId = Lens.lens (subnetId :: ScheduledInstancesLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: ScheduledInstancesLaunchSpecification)
{-# DEPRECATED silsSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The ID of the kernel.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
silsKernelId :: Lens.Lens' ScheduledInstancesLaunchSpecification (Lude.Maybe Lude.Text)
silsKernelId = Lens.lens (kernelId :: ScheduledInstancesLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {kernelId = a} :: ScheduledInstancesLaunchSpecification)
{-# DEPRECATED silsKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
silsInstanceType :: Lens.Lens' ScheduledInstancesLaunchSpecification (Lude.Maybe Lude.Text)
silsInstanceType = Lens.lens (instanceType :: ScheduledInstancesLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: ScheduledInstancesLaunchSpecification)
{-# DEPRECATED silsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Indicates whether the instances are optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance.
--
-- Default: @false@
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
silsEBSOptimized :: Lens.Lens' ScheduledInstancesLaunchSpecification (Lude.Maybe Lude.Bool)
silsEBSOptimized = Lens.lens (ebsOptimized :: ScheduledInstancesLaunchSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: ScheduledInstancesLaunchSpecification)
{-# DEPRECATED silsEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The base64-encoded MIME user data.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
silsUserData :: Lens.Lens' ScheduledInstancesLaunchSpecification (Lude.Maybe Lude.Text)
silsUserData = Lens.lens (userData :: ScheduledInstancesLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {userData = a} :: ScheduledInstancesLaunchSpecification)
{-# DEPRECATED silsUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | Enable or disable monitoring for the instances.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
silsMonitoring :: Lens.Lens' ScheduledInstancesLaunchSpecification (Lude.Maybe ScheduledInstancesMonitoring)
silsMonitoring = Lens.lens (monitoring :: ScheduledInstancesLaunchSpecification -> Lude.Maybe ScheduledInstancesMonitoring) (\s a -> s {monitoring = a} :: ScheduledInstancesLaunchSpecification)
{-# DEPRECATED silsMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
silsIAMInstanceProfile :: Lens.Lens' ScheduledInstancesLaunchSpecification (Lude.Maybe ScheduledInstancesIAMInstanceProfile)
silsIAMInstanceProfile = Lens.lens (iamInstanceProfile :: ScheduledInstancesLaunchSpecification -> Lude.Maybe ScheduledInstancesIAMInstanceProfile) (\s a -> s {iamInstanceProfile = a} :: ScheduledInstancesLaunchSpecification)
{-# DEPRECATED silsIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The block device mapping entries.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
silsBlockDeviceMappings :: Lens.Lens' ScheduledInstancesLaunchSpecification (Lude.Maybe [ScheduledInstancesBlockDeviceMapping])
silsBlockDeviceMappings = Lens.lens (blockDeviceMappings :: ScheduledInstancesLaunchSpecification -> Lude.Maybe [ScheduledInstancesBlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: ScheduledInstancesLaunchSpecification)
{-# DEPRECATED silsBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The placement information.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
silsPlacement :: Lens.Lens' ScheduledInstancesLaunchSpecification (Lude.Maybe ScheduledInstancesPlacement)
silsPlacement = Lens.lens (placement :: ScheduledInstancesLaunchSpecification -> Lude.Maybe ScheduledInstancesPlacement) (\s a -> s {placement = a} :: ScheduledInstancesLaunchSpecification)
{-# DEPRECATED silsPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

-- | The ID of the Amazon Machine Image (AMI).
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
silsImageId :: Lens.Lens' ScheduledInstancesLaunchSpecification Lude.Text
silsImageId = Lens.lens (imageId :: ScheduledInstancesLaunchSpecification -> Lude.Text) (\s a -> s {imageId = a} :: ScheduledInstancesLaunchSpecification)
{-# DEPRECATED silsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

instance Lude.ToQuery ScheduledInstancesLaunchSpecification where
  toQuery ScheduledInstancesLaunchSpecification' {..} =
    Lude.mconcat
      [ Lude.toQuery
          (Lude.toQueryList "SecurityGroupId" Lude.<$> securityGroupIds),
        "KeyName" Lude.=: keyName,
        Lude.toQuery
          (Lude.toQueryList "NetworkInterface" Lude.<$> networkInterfaces),
        "RamdiskId" Lude.=: ramdiskId,
        "SubnetId" Lude.=: subnetId,
        "KernelId" Lude.=: kernelId,
        "InstanceType" Lude.=: instanceType,
        "EbsOptimized" Lude.=: ebsOptimized,
        "UserData" Lude.=: userData,
        "Monitoring" Lude.=: monitoring,
        "IamInstanceProfile" Lude.=: iamInstanceProfile,
        Lude.toQuery
          ( Lude.toQueryList "BlockDeviceMapping"
              Lude.<$> blockDeviceMappings
          ),
        "Placement" Lude.=: placement,
        "ImageId" Lude.=: imageId
      ]
