{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RequestSpotLaunchSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RequestSpotLaunchSpecification
  ( RequestSpotLaunchSpecification (..),

    -- * Smart constructor
    mkRequestSpotLaunchSpecification,

    -- * Lenses
    rslsSecurityGroupIds,
    rslsSecurityGroups,
    rslsKeyName,
    rslsNetworkInterfaces,
    rslsRAMDiskId,
    rslsSubnetId,
    rslsKernelId,
    rslsInstanceType,
    rslsEBSOptimized,
    rslsUserData,
    rslsMonitoring,
    rslsIAMInstanceProfile,
    rslsImageId,
    rslsAddressingType,
    rslsBlockDeviceMappings,
    rslsPlacement,
  )
where

import Network.AWS.EC2.Types.BlockDeviceMapping
import Network.AWS.EC2.Types.IAMInstanceProfileSpecification
import Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.RunInstancesMonitoringEnabled
import Network.AWS.EC2.Types.SpotPlacement
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the launch specification for an instance.
--
-- /See:/ 'mkRequestSpotLaunchSpecification' smart constructor.
data RequestSpotLaunchSpecification = RequestSpotLaunchSpecification'
  { securityGroupIds ::
      Lude.Maybe [Lude.Text],
    securityGroups ::
      Lude.Maybe [Lude.Text],
    keyName ::
      Lude.Maybe Lude.Text,
    networkInterfaces ::
      Lude.Maybe
        [InstanceNetworkInterfaceSpecification],
    ramdiskId ::
      Lude.Maybe Lude.Text,
    subnetId ::
      Lude.Maybe Lude.Text,
    kernelId ::
      Lude.Maybe Lude.Text,
    instanceType ::
      Lude.Maybe InstanceType,
    ebsOptimized ::
      Lude.Maybe Lude.Bool,
    userData ::
      Lude.Maybe Lude.Text,
    monitoring ::
      Lude.Maybe
        RunInstancesMonitoringEnabled,
    iamInstanceProfile ::
      Lude.Maybe
        IAMInstanceProfileSpecification,
    imageId ::
      Lude.Maybe Lude.Text,
    addressingType ::
      Lude.Maybe Lude.Text,
    blockDeviceMappings ::
      Lude.Maybe
        [BlockDeviceMapping],
    placement ::
      Lude.Maybe SpotPlacement
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RequestSpotLaunchSpecification' with the minimum fields required to make a request.
--
-- * 'addressingType' - Deprecated.
-- * 'blockDeviceMappings' - One or more block device mapping entries. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
-- * 'ebsOptimized' - Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
-- * 'iamInstanceProfile' - The IAM instance profile.
-- * 'imageId' - The ID of the AMI.
-- * 'instanceType' - The instance type.
-- * 'kernelId' - The ID of the kernel.
-- * 'keyName' - The name of the key pair.
-- * 'monitoring' - Indicates whether basic or detailed monitoring is enabled for the instance.
--
-- Default: Disabled
-- * 'networkInterfaces' - One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
-- * 'placement' - The placement information for the instance.
-- * 'ramdiskId' - The ID of the RAM disk.
-- * 'securityGroupIds' - One or more security group IDs.
-- * 'securityGroups' - One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
-- * 'subnetId' - The IDs of the subnets in which to launch the instance. To specify multiple subnets, separate them using commas; for example, "subnet-1234abcdeexample1, subnet-0987cdef6example2".
-- * 'userData' - The Base64-encoded user data for the instance. User data is limited to 16 KB.
mkRequestSpotLaunchSpecification ::
  RequestSpotLaunchSpecification
mkRequestSpotLaunchSpecification =
  RequestSpotLaunchSpecification'
    { securityGroupIds = Lude.Nothing,
      securityGroups = Lude.Nothing,
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

-- | One or more security group IDs.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsSecurityGroupIds :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe [Lude.Text])
rslsSecurityGroupIds = Lens.lens (securityGroupIds :: RequestSpotLaunchSpecification -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsSecurityGroups :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe [Lude.Text])
rslsSecurityGroups = Lens.lens (securityGroups :: RequestSpotLaunchSpecification -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsKeyName :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe Lude.Text)
rslsKeyName = Lens.lens (keyName :: RequestSpotLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {keyName = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsNetworkInterfaces :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe [InstanceNetworkInterfaceSpecification])
rslsNetworkInterfaces = Lens.lens (networkInterfaces :: RequestSpotLaunchSpecification -> Lude.Maybe [InstanceNetworkInterfaceSpecification]) (\s a -> s {networkInterfaces = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The ID of the RAM disk.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsRAMDiskId :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe Lude.Text)
rslsRAMDiskId = Lens.lens (ramdiskId :: RequestSpotLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {ramdiskId = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsRAMDiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The IDs of the subnets in which to launch the instance. To specify multiple subnets, separate them using commas; for example, "subnet-1234abcdeexample1, subnet-0987cdef6example2".
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsSubnetId :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe Lude.Text)
rslsSubnetId = Lens.lens (subnetId :: RequestSpotLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The ID of the kernel.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsKernelId :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe Lude.Text)
rslsKernelId = Lens.lens (kernelId :: RequestSpotLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {kernelId = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsInstanceType :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe InstanceType)
rslsInstanceType = Lens.lens (instanceType :: RequestSpotLaunchSpecification -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsEBSOptimized :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe Lude.Bool)
rslsEBSOptimized = Lens.lens (ebsOptimized :: RequestSpotLaunchSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The Base64-encoded user data for the instance. User data is limited to 16 KB.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsUserData :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe Lude.Text)
rslsUserData = Lens.lens (userData :: RequestSpotLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {userData = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | Indicates whether basic or detailed monitoring is enabled for the instance.
--
-- Default: Disabled
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsMonitoring :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe RunInstancesMonitoringEnabled)
rslsMonitoring = Lens.lens (monitoring :: RequestSpotLaunchSpecification -> Lude.Maybe RunInstancesMonitoringEnabled) (\s a -> s {monitoring = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsIAMInstanceProfile :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe IAMInstanceProfileSpecification)
rslsIAMInstanceProfile = Lens.lens (iamInstanceProfile :: RequestSpotLaunchSpecification -> Lude.Maybe IAMInstanceProfileSpecification) (\s a -> s {iamInstanceProfile = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsImageId :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe Lude.Text)
rslsImageId = Lens.lens (imageId :: RequestSpotLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | Deprecated.
--
-- /Note:/ Consider using 'addressingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsAddressingType :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe Lude.Text)
rslsAddressingType = Lens.lens (addressingType :: RequestSpotLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {addressingType = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsAddressingType "Use generic-lens or generic-optics with 'addressingType' instead." #-}

-- | One or more block device mapping entries. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsBlockDeviceMappings :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe [BlockDeviceMapping])
rslsBlockDeviceMappings = Lens.lens (blockDeviceMappings :: RequestSpotLaunchSpecification -> Lude.Maybe [BlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The placement information for the instance.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsPlacement :: Lens.Lens' RequestSpotLaunchSpecification (Lude.Maybe SpotPlacement)
rslsPlacement = Lens.lens (placement :: RequestSpotLaunchSpecification -> Lude.Maybe SpotPlacement) (\s a -> s {placement = a} :: RequestSpotLaunchSpecification)
{-# DEPRECATED rslsPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

instance Lude.ToQuery RequestSpotLaunchSpecification where
  toQuery RequestSpotLaunchSpecification' {..} =
    Lude.mconcat
      [ Lude.toQuery
          (Lude.toQueryList "SecurityGroupId" Lude.<$> securityGroupIds),
        Lude.toQuery
          (Lude.toQueryList "SecurityGroup" Lude.<$> securityGroups),
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
        "ImageId" Lude.=: imageId,
        "AddressingType" Lude.=: addressingType,
        Lude.toQuery
          ( Lude.toQueryList "BlockDeviceMapping"
              Lude.<$> blockDeviceMappings
          ),
        "Placement" Lude.=: placement
      ]
