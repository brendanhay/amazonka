{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.BlockDeviceMapping
import Network.AWS.EC2.Types.IamInstanceProfileSpecification
import Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.RunInstancesMonitoringEnabled
import Network.AWS.EC2.Types.SpotPlacement
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the launch specification for an instance.
--
-- /See:/ 'newRequestSpotLaunchSpecification' smart constructor.
data RequestSpotLaunchSpecification = RequestSpotLaunchSpecification'
  { -- | One or more security group IDs.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The instance type.
    instanceType :: Prelude.Maybe InstanceType,
    -- | Indicates whether the instance is optimized for EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal EBS I\/O performance.
    -- This optimization isn\'t available with all instance types. Additional
    -- usage charges apply when using an EBS Optimized instance.
    --
    -- Default: @false@
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The Base64-encoded user data for the instance. User data is limited to
    -- 16 KB.
    userData :: Prelude.Maybe Prelude.Text,
    -- | The placement information for the instance.
    placement :: Prelude.Maybe SpotPlacement,
    -- | Deprecated.
    addressingType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the RAM disk.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | One or more security groups. When requesting instances in a VPC, you
    -- must specify the IDs of the security groups. When requesting instances
    -- in EC2-Classic, you can specify the names or the IDs of the security
    -- groups.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The IAM instance profile.
    iamInstanceProfile :: Prelude.Maybe IamInstanceProfileSpecification,
    -- | Indicates whether basic or detailed monitoring is enabled for the
    -- instance.
    --
    -- Default: Disabled
    monitoring :: Prelude.Maybe RunInstancesMonitoringEnabled,
    -- | One or more block device mapping entries. You can\'t specify both a
    -- snapshot ID and an encryption value. This is because only blank volumes
    -- can be encrypted on creation. If a snapshot is the basis for a volume,
    -- it is not blank and its encryption status is used for the volume
    -- encryption status.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | The IDs of the subnets in which to launch the instance. To specify
    -- multiple subnets, separate them using commas; for example,
    -- \"subnet-1234abcdeexample1, subnet-0987cdef6example2\".
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the kernel.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The name of the key pair.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | One or more network interfaces. If you specify a network interface, you
    -- must specify subnet IDs and security group IDs using the network
    -- interface.
    networkInterfaces :: Prelude.Maybe [InstanceNetworkInterfaceSpecification]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      instanceType = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      userData = Prelude.Nothing,
      placement = Prelude.Nothing,
      addressingType = Prelude.Nothing,
      ramdiskId = Prelude.Nothing,
      imageId = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      iamInstanceProfile = Prelude.Nothing,
      monitoring = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      keyName = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing
    }

-- | One or more security group IDs.
requestSpotLaunchSpecification_securityGroupIds :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe [Prelude.Text])
requestSpotLaunchSpecification_securityGroupIds = Lens.lens (\RequestSpotLaunchSpecification' {securityGroupIds} -> securityGroupIds) (\s@RequestSpotLaunchSpecification' {} a -> s {securityGroupIds = a} :: RequestSpotLaunchSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The instance type.
requestSpotLaunchSpecification_instanceType :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe InstanceType)
requestSpotLaunchSpecification_instanceType = Lens.lens (\RequestSpotLaunchSpecification' {instanceType} -> instanceType) (\s@RequestSpotLaunchSpecification' {} a -> s {instanceType = a} :: RequestSpotLaunchSpecification)

-- | Indicates whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
requestSpotLaunchSpecification_ebsOptimized :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Bool)
requestSpotLaunchSpecification_ebsOptimized = Lens.lens (\RequestSpotLaunchSpecification' {ebsOptimized} -> ebsOptimized) (\s@RequestSpotLaunchSpecification' {} a -> s {ebsOptimized = a} :: RequestSpotLaunchSpecification)

-- | The Base64-encoded user data for the instance. User data is limited to
-- 16 KB.
requestSpotLaunchSpecification_userData :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Text)
requestSpotLaunchSpecification_userData = Lens.lens (\RequestSpotLaunchSpecification' {userData} -> userData) (\s@RequestSpotLaunchSpecification' {} a -> s {userData = a} :: RequestSpotLaunchSpecification)

-- | The placement information for the instance.
requestSpotLaunchSpecification_placement :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe SpotPlacement)
requestSpotLaunchSpecification_placement = Lens.lens (\RequestSpotLaunchSpecification' {placement} -> placement) (\s@RequestSpotLaunchSpecification' {} a -> s {placement = a} :: RequestSpotLaunchSpecification)

-- | Deprecated.
requestSpotLaunchSpecification_addressingType :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Text)
requestSpotLaunchSpecification_addressingType = Lens.lens (\RequestSpotLaunchSpecification' {addressingType} -> addressingType) (\s@RequestSpotLaunchSpecification' {} a -> s {addressingType = a} :: RequestSpotLaunchSpecification)

-- | The ID of the RAM disk.
requestSpotLaunchSpecification_ramdiskId :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Text)
requestSpotLaunchSpecification_ramdiskId = Lens.lens (\RequestSpotLaunchSpecification' {ramdiskId} -> ramdiskId) (\s@RequestSpotLaunchSpecification' {} a -> s {ramdiskId = a} :: RequestSpotLaunchSpecification)

-- | The ID of the AMI.
requestSpotLaunchSpecification_imageId :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Text)
requestSpotLaunchSpecification_imageId = Lens.lens (\RequestSpotLaunchSpecification' {imageId} -> imageId) (\s@RequestSpotLaunchSpecification' {} a -> s {imageId = a} :: RequestSpotLaunchSpecification)

-- | One or more security groups. When requesting instances in a VPC, you
-- must specify the IDs of the security groups. When requesting instances
-- in EC2-Classic, you can specify the names or the IDs of the security
-- groups.
requestSpotLaunchSpecification_securityGroups :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe [Prelude.Text])
requestSpotLaunchSpecification_securityGroups = Lens.lens (\RequestSpotLaunchSpecification' {securityGroups} -> securityGroups) (\s@RequestSpotLaunchSpecification' {} a -> s {securityGroups = a} :: RequestSpotLaunchSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The IAM instance profile.
requestSpotLaunchSpecification_iamInstanceProfile :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe IamInstanceProfileSpecification)
requestSpotLaunchSpecification_iamInstanceProfile = Lens.lens (\RequestSpotLaunchSpecification' {iamInstanceProfile} -> iamInstanceProfile) (\s@RequestSpotLaunchSpecification' {} a -> s {iamInstanceProfile = a} :: RequestSpotLaunchSpecification)

-- | Indicates whether basic or detailed monitoring is enabled for the
-- instance.
--
-- Default: Disabled
requestSpotLaunchSpecification_monitoring :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe RunInstancesMonitoringEnabled)
requestSpotLaunchSpecification_monitoring = Lens.lens (\RequestSpotLaunchSpecification' {monitoring} -> monitoring) (\s@RequestSpotLaunchSpecification' {} a -> s {monitoring = a} :: RequestSpotLaunchSpecification)

-- | One or more block device mapping entries. You can\'t specify both a
-- snapshot ID and an encryption value. This is because only blank volumes
-- can be encrypted on creation. If a snapshot is the basis for a volume,
-- it is not blank and its encryption status is used for the volume
-- encryption status.
requestSpotLaunchSpecification_blockDeviceMappings :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe [BlockDeviceMapping])
requestSpotLaunchSpecification_blockDeviceMappings = Lens.lens (\RequestSpotLaunchSpecification' {blockDeviceMappings} -> blockDeviceMappings) (\s@RequestSpotLaunchSpecification' {} a -> s {blockDeviceMappings = a} :: RequestSpotLaunchSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The IDs of the subnets in which to launch the instance. To specify
-- multiple subnets, separate them using commas; for example,
-- \"subnet-1234abcdeexample1, subnet-0987cdef6example2\".
requestSpotLaunchSpecification_subnetId :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Text)
requestSpotLaunchSpecification_subnetId = Lens.lens (\RequestSpotLaunchSpecification' {subnetId} -> subnetId) (\s@RequestSpotLaunchSpecification' {} a -> s {subnetId = a} :: RequestSpotLaunchSpecification)

-- | The ID of the kernel.
requestSpotLaunchSpecification_kernelId :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Text)
requestSpotLaunchSpecification_kernelId = Lens.lens (\RequestSpotLaunchSpecification' {kernelId} -> kernelId) (\s@RequestSpotLaunchSpecification' {} a -> s {kernelId = a} :: RequestSpotLaunchSpecification)

-- | The name of the key pair.
requestSpotLaunchSpecification_keyName :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Text)
requestSpotLaunchSpecification_keyName = Lens.lens (\RequestSpotLaunchSpecification' {keyName} -> keyName) (\s@RequestSpotLaunchSpecification' {} a -> s {keyName = a} :: RequestSpotLaunchSpecification)

-- | One or more network interfaces. If you specify a network interface, you
-- must specify subnet IDs and security group IDs using the network
-- interface.
requestSpotLaunchSpecification_networkInterfaces :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe [InstanceNetworkInterfaceSpecification])
requestSpotLaunchSpecification_networkInterfaces = Lens.lens (\RequestSpotLaunchSpecification' {networkInterfaces} -> networkInterfaces) (\s@RequestSpotLaunchSpecification' {} a -> s {networkInterfaces = a} :: RequestSpotLaunchSpecification) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.Hashable
    RequestSpotLaunchSpecification

instance
  Prelude.NFData
    RequestSpotLaunchSpecification

instance
  Prelude.ToQuery
    RequestSpotLaunchSpecification
  where
  toQuery RequestSpotLaunchSpecification' {..} =
    Prelude.mconcat
      [ Prelude.toQuery
          ( Prelude.toQueryList "SecurityGroupId"
              Prelude.<$> securityGroupIds
          ),
        "InstanceType" Prelude.=: instanceType,
        "EbsOptimized" Prelude.=: ebsOptimized,
        "UserData" Prelude.=: userData,
        "Placement" Prelude.=: placement,
        "AddressingType" Prelude.=: addressingType,
        "RamdiskId" Prelude.=: ramdiskId,
        "ImageId" Prelude.=: imageId,
        Prelude.toQuery
          ( Prelude.toQueryList "SecurityGroup"
              Prelude.<$> securityGroups
          ),
        "IamInstanceProfile" Prelude.=: iamInstanceProfile,
        "Monitoring" Prelude.=: monitoring,
        Prelude.toQuery
          ( Prelude.toQueryList "BlockDeviceMapping"
              Prelude.<$> blockDeviceMappings
          ),
        "SubnetId" Prelude.=: subnetId,
        "KernelId" Prelude.=: kernelId,
        "KeyName" Prelude.=: keyName,
        Prelude.toQuery
          ( Prelude.toQueryList "NetworkInterface"
              Prelude.<$> networkInterfaces
          )
      ]
