{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RequestSpotLaunchSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RequestSpotLaunchSpecification where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.BlockDeviceMapping
import Network.AWS.EC2.Types.IamInstanceProfileSpecification
import Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.RunInstancesMonitoringEnabled
import Network.AWS.EC2.Types.SpotPlacement
import qualified Network.AWS.Lens as Lens

-- | Describes the launch specification for an instance.
--
-- /See:/ 'newRequestSpotLaunchSpecification' smart constructor.
data RequestSpotLaunchSpecification = RequestSpotLaunchSpecification'
  { -- | One or more security group IDs.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | The instance type.
    instanceType :: Core.Maybe InstanceType,
    -- | Indicates whether the instance is optimized for EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal EBS I\/O performance.
    -- This optimization isn\'t available with all instance types. Additional
    -- usage charges apply when using an EBS Optimized instance.
    --
    -- Default: @false@
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | The Base64-encoded user data for the instance. User data is limited to
    -- 16 KB.
    userData :: Core.Maybe Core.Text,
    -- | The placement information for the instance.
    placement :: Core.Maybe SpotPlacement,
    -- | Deprecated.
    addressingType :: Core.Maybe Core.Text,
    -- | The ID of the RAM disk.
    ramdiskId :: Core.Maybe Core.Text,
    -- | The ID of the AMI.
    imageId :: Core.Maybe Core.Text,
    -- | One or more security groups. When requesting instances in a VPC, you
    -- must specify the IDs of the security groups. When requesting instances
    -- in EC2-Classic, you can specify the names or the IDs of the security
    -- groups.
    securityGroups :: Core.Maybe [Core.Text],
    -- | The IAM instance profile.
    iamInstanceProfile :: Core.Maybe IamInstanceProfileSpecification,
    -- | Indicates whether basic or detailed monitoring is enabled for the
    -- instance.
    --
    -- Default: Disabled
    monitoring :: Core.Maybe RunInstancesMonitoringEnabled,
    -- | One or more block device mapping entries. You can\'t specify both a
    -- snapshot ID and an encryption value. This is because only blank volumes
    -- can be encrypted on creation. If a snapshot is the basis for a volume,
    -- it is not blank and its encryption status is used for the volume
    -- encryption status.
    blockDeviceMappings :: Core.Maybe [BlockDeviceMapping],
    -- | The IDs of the subnets in which to launch the instance. To specify
    -- multiple subnets, separate them using commas; for example,
    -- \"subnet-1234abcdeexample1, subnet-0987cdef6example2\".
    subnetId :: Core.Maybe Core.Text,
    -- | The ID of the kernel.
    kernelId :: Core.Maybe Core.Text,
    -- | The name of the key pair.
    keyName :: Core.Maybe Core.Text,
    -- | One or more network interfaces. If you specify a network interface, you
    -- must specify subnet IDs and security group IDs using the network
    -- interface.
    networkInterfaces :: Core.Maybe [InstanceNetworkInterfaceSpecification]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RequestSpotLaunchSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'requestSpotLaunchSpecification_securityGroupIds' - One or more security group IDs.
--
-- 'instanceType', 'requestSpotLaunchSpecification_instanceType' - The instance type.
--
-- 'ebsOptimized', 'requestSpotLaunchSpecification_ebsOptimized' - Indicates whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
--
-- 'userData', 'requestSpotLaunchSpecification_userData' - The Base64-encoded user data for the instance. User data is limited to
-- 16 KB.
--
-- 'placement', 'requestSpotLaunchSpecification_placement' - The placement information for the instance.
--
-- 'addressingType', 'requestSpotLaunchSpecification_addressingType' - Deprecated.
--
-- 'ramdiskId', 'requestSpotLaunchSpecification_ramdiskId' - The ID of the RAM disk.
--
-- 'imageId', 'requestSpotLaunchSpecification_imageId' - The ID of the AMI.
--
-- 'securityGroups', 'requestSpotLaunchSpecification_securityGroups' - One or more security groups. When requesting instances in a VPC, you
-- must specify the IDs of the security groups. When requesting instances
-- in EC2-Classic, you can specify the names or the IDs of the security
-- groups.
--
-- 'iamInstanceProfile', 'requestSpotLaunchSpecification_iamInstanceProfile' - The IAM instance profile.
--
-- 'monitoring', 'requestSpotLaunchSpecification_monitoring' - Indicates whether basic or detailed monitoring is enabled for the
-- instance.
--
-- Default: Disabled
--
-- 'blockDeviceMappings', 'requestSpotLaunchSpecification_blockDeviceMappings' - One or more block device mapping entries. You can\'t specify both a
-- snapshot ID and an encryption value. This is because only blank volumes
-- can be encrypted on creation. If a snapshot is the basis for a volume,
-- it is not blank and its encryption status is used for the volume
-- encryption status.
--
-- 'subnetId', 'requestSpotLaunchSpecification_subnetId' - The IDs of the subnets in which to launch the instance. To specify
-- multiple subnets, separate them using commas; for example,
-- \"subnet-1234abcdeexample1, subnet-0987cdef6example2\".
--
-- 'kernelId', 'requestSpotLaunchSpecification_kernelId' - The ID of the kernel.
--
-- 'keyName', 'requestSpotLaunchSpecification_keyName' - The name of the key pair.
--
-- 'networkInterfaces', 'requestSpotLaunchSpecification_networkInterfaces' - One or more network interfaces. If you specify a network interface, you
-- must specify subnet IDs and security group IDs using the network
-- interface.
newRequestSpotLaunchSpecification ::
  RequestSpotLaunchSpecification
newRequestSpotLaunchSpecification =
  RequestSpotLaunchSpecification'
    { securityGroupIds =
        Core.Nothing,
      instanceType = Core.Nothing,
      ebsOptimized = Core.Nothing,
      userData = Core.Nothing,
      placement = Core.Nothing,
      addressingType = Core.Nothing,
      ramdiskId = Core.Nothing,
      imageId = Core.Nothing,
      securityGroups = Core.Nothing,
      iamInstanceProfile = Core.Nothing,
      monitoring = Core.Nothing,
      blockDeviceMappings = Core.Nothing,
      subnetId = Core.Nothing,
      kernelId = Core.Nothing,
      keyName = Core.Nothing,
      networkInterfaces = Core.Nothing
    }

-- | One or more security group IDs.
requestSpotLaunchSpecification_securityGroupIds :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe [Core.Text])
requestSpotLaunchSpecification_securityGroupIds = Lens.lens (\RequestSpotLaunchSpecification' {securityGroupIds} -> securityGroupIds) (\s@RequestSpotLaunchSpecification' {} a -> s {securityGroupIds = a} :: RequestSpotLaunchSpecification) Core.. Lens.mapping Lens._Coerce

-- | The instance type.
requestSpotLaunchSpecification_instanceType :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe InstanceType)
requestSpotLaunchSpecification_instanceType = Lens.lens (\RequestSpotLaunchSpecification' {instanceType} -> instanceType) (\s@RequestSpotLaunchSpecification' {} a -> s {instanceType = a} :: RequestSpotLaunchSpecification)

-- | Indicates whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
requestSpotLaunchSpecification_ebsOptimized :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Core.Bool)
requestSpotLaunchSpecification_ebsOptimized = Lens.lens (\RequestSpotLaunchSpecification' {ebsOptimized} -> ebsOptimized) (\s@RequestSpotLaunchSpecification' {} a -> s {ebsOptimized = a} :: RequestSpotLaunchSpecification)

-- | The Base64-encoded user data for the instance. User data is limited to
-- 16 KB.
requestSpotLaunchSpecification_userData :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Core.Text)
requestSpotLaunchSpecification_userData = Lens.lens (\RequestSpotLaunchSpecification' {userData} -> userData) (\s@RequestSpotLaunchSpecification' {} a -> s {userData = a} :: RequestSpotLaunchSpecification)

-- | The placement information for the instance.
requestSpotLaunchSpecification_placement :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe SpotPlacement)
requestSpotLaunchSpecification_placement = Lens.lens (\RequestSpotLaunchSpecification' {placement} -> placement) (\s@RequestSpotLaunchSpecification' {} a -> s {placement = a} :: RequestSpotLaunchSpecification)

-- | Deprecated.
requestSpotLaunchSpecification_addressingType :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Core.Text)
requestSpotLaunchSpecification_addressingType = Lens.lens (\RequestSpotLaunchSpecification' {addressingType} -> addressingType) (\s@RequestSpotLaunchSpecification' {} a -> s {addressingType = a} :: RequestSpotLaunchSpecification)

-- | The ID of the RAM disk.
requestSpotLaunchSpecification_ramdiskId :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Core.Text)
requestSpotLaunchSpecification_ramdiskId = Lens.lens (\RequestSpotLaunchSpecification' {ramdiskId} -> ramdiskId) (\s@RequestSpotLaunchSpecification' {} a -> s {ramdiskId = a} :: RequestSpotLaunchSpecification)

-- | The ID of the AMI.
requestSpotLaunchSpecification_imageId :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Core.Text)
requestSpotLaunchSpecification_imageId = Lens.lens (\RequestSpotLaunchSpecification' {imageId} -> imageId) (\s@RequestSpotLaunchSpecification' {} a -> s {imageId = a} :: RequestSpotLaunchSpecification)

-- | One or more security groups. When requesting instances in a VPC, you
-- must specify the IDs of the security groups. When requesting instances
-- in EC2-Classic, you can specify the names or the IDs of the security
-- groups.
requestSpotLaunchSpecification_securityGroups :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe [Core.Text])
requestSpotLaunchSpecification_securityGroups = Lens.lens (\RequestSpotLaunchSpecification' {securityGroups} -> securityGroups) (\s@RequestSpotLaunchSpecification' {} a -> s {securityGroups = a} :: RequestSpotLaunchSpecification) Core.. Lens.mapping Lens._Coerce

-- | The IAM instance profile.
requestSpotLaunchSpecification_iamInstanceProfile :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe IamInstanceProfileSpecification)
requestSpotLaunchSpecification_iamInstanceProfile = Lens.lens (\RequestSpotLaunchSpecification' {iamInstanceProfile} -> iamInstanceProfile) (\s@RequestSpotLaunchSpecification' {} a -> s {iamInstanceProfile = a} :: RequestSpotLaunchSpecification)

-- | Indicates whether basic or detailed monitoring is enabled for the
-- instance.
--
-- Default: Disabled
requestSpotLaunchSpecification_monitoring :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe RunInstancesMonitoringEnabled)
requestSpotLaunchSpecification_monitoring = Lens.lens (\RequestSpotLaunchSpecification' {monitoring} -> monitoring) (\s@RequestSpotLaunchSpecification' {} a -> s {monitoring = a} :: RequestSpotLaunchSpecification)

-- | One or more block device mapping entries. You can\'t specify both a
-- snapshot ID and an encryption value. This is because only blank volumes
-- can be encrypted on creation. If a snapshot is the basis for a volume,
-- it is not blank and its encryption status is used for the volume
-- encryption status.
requestSpotLaunchSpecification_blockDeviceMappings :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe [BlockDeviceMapping])
requestSpotLaunchSpecification_blockDeviceMappings = Lens.lens (\RequestSpotLaunchSpecification' {blockDeviceMappings} -> blockDeviceMappings) (\s@RequestSpotLaunchSpecification' {} a -> s {blockDeviceMappings = a} :: RequestSpotLaunchSpecification) Core.. Lens.mapping Lens._Coerce

-- | The IDs of the subnets in which to launch the instance. To specify
-- multiple subnets, separate them using commas; for example,
-- \"subnet-1234abcdeexample1, subnet-0987cdef6example2\".
requestSpotLaunchSpecification_subnetId :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Core.Text)
requestSpotLaunchSpecification_subnetId = Lens.lens (\RequestSpotLaunchSpecification' {subnetId} -> subnetId) (\s@RequestSpotLaunchSpecification' {} a -> s {subnetId = a} :: RequestSpotLaunchSpecification)

-- | The ID of the kernel.
requestSpotLaunchSpecification_kernelId :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Core.Text)
requestSpotLaunchSpecification_kernelId = Lens.lens (\RequestSpotLaunchSpecification' {kernelId} -> kernelId) (\s@RequestSpotLaunchSpecification' {} a -> s {kernelId = a} :: RequestSpotLaunchSpecification)

-- | The name of the key pair.
requestSpotLaunchSpecification_keyName :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Core.Text)
requestSpotLaunchSpecification_keyName = Lens.lens (\RequestSpotLaunchSpecification' {keyName} -> keyName) (\s@RequestSpotLaunchSpecification' {} a -> s {keyName = a} :: RequestSpotLaunchSpecification)

-- | One or more network interfaces. If you specify a network interface, you
-- must specify subnet IDs and security group IDs using the network
-- interface.
requestSpotLaunchSpecification_networkInterfaces :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe [InstanceNetworkInterfaceSpecification])
requestSpotLaunchSpecification_networkInterfaces = Lens.lens (\RequestSpotLaunchSpecification' {networkInterfaces} -> networkInterfaces) (\s@RequestSpotLaunchSpecification' {} a -> s {networkInterfaces = a} :: RequestSpotLaunchSpecification) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable RequestSpotLaunchSpecification

instance Core.NFData RequestSpotLaunchSpecification

instance Core.ToQuery RequestSpotLaunchSpecification where
  toQuery RequestSpotLaunchSpecification' {..} =
    Core.mconcat
      [ Core.toQuery
          ( Core.toQueryList "SecurityGroupId"
              Core.<$> securityGroupIds
          ),
        "InstanceType" Core.=: instanceType,
        "EbsOptimized" Core.=: ebsOptimized,
        "UserData" Core.=: userData,
        "Placement" Core.=: placement,
        "AddressingType" Core.=: addressingType,
        "RamdiskId" Core.=: ramdiskId,
        "ImageId" Core.=: imageId,
        Core.toQuery
          ( Core.toQueryList "SecurityGroup"
              Core.<$> securityGroups
          ),
        "IamInstanceProfile" Core.=: iamInstanceProfile,
        "Monitoring" Core.=: monitoring,
        Core.toQuery
          ( Core.toQueryList "BlockDeviceMapping"
              Core.<$> blockDeviceMappings
          ),
        "SubnetId" Core.=: subnetId,
        "KernelId" Core.=: kernelId,
        "KeyName" Core.=: keyName,
        Core.toQuery
          ( Core.toQueryList "NetworkInterface"
              Core.<$> networkInterfaces
          )
      ]
