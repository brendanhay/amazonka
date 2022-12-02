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
-- Module      : Amazonka.EC2.Types.RequestSpotLaunchSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RequestSpotLaunchSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.BlockDeviceMapping
import Amazonka.EC2.Types.IamInstanceProfileSpecification
import Amazonka.EC2.Types.InstanceNetworkInterfaceSpecification
import Amazonka.EC2.Types.InstanceType
import Amazonka.EC2.Types.RunInstancesMonitoringEnabled
import Amazonka.EC2.Types.SpotPlacement
import qualified Amazonka.Prelude as Prelude

-- | Describes the launch specification for an instance.
--
-- /See:/ 'newRequestSpotLaunchSpecification' smart constructor.
data RequestSpotLaunchSpecification = RequestSpotLaunchSpecification'
  { -- | Indicates whether the instance is optimized for EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal EBS I\/O performance.
    -- This optimization isn\'t available with all instance types. Additional
    -- usage charges apply when using an EBS Optimized instance.
    --
    -- Default: @false@
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The IAM instance profile.
    iamInstanceProfile :: Prelude.Maybe IamInstanceProfileSpecification,
    -- | The placement information for the instance.
    placement :: Prelude.Maybe SpotPlacement,
    -- | The Base64-encoded user data for the instance. User data is limited to
    -- 16 KB.
    userData :: Prelude.Maybe Prelude.Text,
    -- | One or more block device mapping entries. You can\'t specify both a
    -- snapshot ID and an encryption value. This is because only blank volumes
    -- can be encrypted on creation. If a snapshot is the basis for a volume,
    -- it is not blank and its encryption status is used for the volume
    -- encryption status.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | One or more security group IDs.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Deprecated.
    addressingType :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether basic or detailed monitoring is enabled for the
    -- instance.
    --
    -- Default: Disabled
    monitoring :: Prelude.Maybe RunInstancesMonitoringEnabled,
    -- | The ID of the subnet in which to launch the instance.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The instance type. Only one instance type can be specified.
    instanceType :: Prelude.Maybe InstanceType,
    -- | One or more security groups. When requesting instances in a VPC, you
    -- must specify the IDs of the security groups. When requesting instances
    -- in EC2-Classic, you can specify the names or the IDs of the security
    -- groups.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the RAM disk.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The name of the key pair.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the kernel.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | One or more network interfaces. If you specify a network interface, you
    -- must specify subnet IDs and security group IDs using the network
    -- interface.
    networkInterfaces :: Prelude.Maybe [InstanceNetworkInterfaceSpecification]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestSpotLaunchSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsOptimized', 'requestSpotLaunchSpecification_ebsOptimized' - Indicates whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
--
-- 'iamInstanceProfile', 'requestSpotLaunchSpecification_iamInstanceProfile' - The IAM instance profile.
--
-- 'placement', 'requestSpotLaunchSpecification_placement' - The placement information for the instance.
--
-- 'userData', 'requestSpotLaunchSpecification_userData' - The Base64-encoded user data for the instance. User data is limited to
-- 16 KB.
--
-- 'blockDeviceMappings', 'requestSpotLaunchSpecification_blockDeviceMappings' - One or more block device mapping entries. You can\'t specify both a
-- snapshot ID and an encryption value. This is because only blank volumes
-- can be encrypted on creation. If a snapshot is the basis for a volume,
-- it is not blank and its encryption status is used for the volume
-- encryption status.
--
-- 'securityGroupIds', 'requestSpotLaunchSpecification_securityGroupIds' - One or more security group IDs.
--
-- 'addressingType', 'requestSpotLaunchSpecification_addressingType' - Deprecated.
--
-- 'monitoring', 'requestSpotLaunchSpecification_monitoring' - Indicates whether basic or detailed monitoring is enabled for the
-- instance.
--
-- Default: Disabled
--
-- 'subnetId', 'requestSpotLaunchSpecification_subnetId' - The ID of the subnet in which to launch the instance.
--
-- 'instanceType', 'requestSpotLaunchSpecification_instanceType' - The instance type. Only one instance type can be specified.
--
-- 'securityGroups', 'requestSpotLaunchSpecification_securityGroups' - One or more security groups. When requesting instances in a VPC, you
-- must specify the IDs of the security groups. When requesting instances
-- in EC2-Classic, you can specify the names or the IDs of the security
-- groups.
--
-- 'ramdiskId', 'requestSpotLaunchSpecification_ramdiskId' - The ID of the RAM disk.
--
-- 'keyName', 'requestSpotLaunchSpecification_keyName' - The name of the key pair.
--
-- 'kernelId', 'requestSpotLaunchSpecification_kernelId' - The ID of the kernel.
--
-- 'imageId', 'requestSpotLaunchSpecification_imageId' - The ID of the AMI.
--
-- 'networkInterfaces', 'requestSpotLaunchSpecification_networkInterfaces' - One or more network interfaces. If you specify a network interface, you
-- must specify subnet IDs and security group IDs using the network
-- interface.
newRequestSpotLaunchSpecification ::
  RequestSpotLaunchSpecification
newRequestSpotLaunchSpecification =
  RequestSpotLaunchSpecification'
    { ebsOptimized =
        Prelude.Nothing,
      iamInstanceProfile = Prelude.Nothing,
      placement = Prelude.Nothing,
      userData = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      addressingType = Prelude.Nothing,
      monitoring = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      ramdiskId = Prelude.Nothing,
      keyName = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      imageId = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing
    }

-- | Indicates whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
requestSpotLaunchSpecification_ebsOptimized :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Bool)
requestSpotLaunchSpecification_ebsOptimized = Lens.lens (\RequestSpotLaunchSpecification' {ebsOptimized} -> ebsOptimized) (\s@RequestSpotLaunchSpecification' {} a -> s {ebsOptimized = a} :: RequestSpotLaunchSpecification)

-- | The IAM instance profile.
requestSpotLaunchSpecification_iamInstanceProfile :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe IamInstanceProfileSpecification)
requestSpotLaunchSpecification_iamInstanceProfile = Lens.lens (\RequestSpotLaunchSpecification' {iamInstanceProfile} -> iamInstanceProfile) (\s@RequestSpotLaunchSpecification' {} a -> s {iamInstanceProfile = a} :: RequestSpotLaunchSpecification)

-- | The placement information for the instance.
requestSpotLaunchSpecification_placement :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe SpotPlacement)
requestSpotLaunchSpecification_placement = Lens.lens (\RequestSpotLaunchSpecification' {placement} -> placement) (\s@RequestSpotLaunchSpecification' {} a -> s {placement = a} :: RequestSpotLaunchSpecification)

-- | The Base64-encoded user data for the instance. User data is limited to
-- 16 KB.
requestSpotLaunchSpecification_userData :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Text)
requestSpotLaunchSpecification_userData = Lens.lens (\RequestSpotLaunchSpecification' {userData} -> userData) (\s@RequestSpotLaunchSpecification' {} a -> s {userData = a} :: RequestSpotLaunchSpecification)

-- | One or more block device mapping entries. You can\'t specify both a
-- snapshot ID and an encryption value. This is because only blank volumes
-- can be encrypted on creation. If a snapshot is the basis for a volume,
-- it is not blank and its encryption status is used for the volume
-- encryption status.
requestSpotLaunchSpecification_blockDeviceMappings :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe [BlockDeviceMapping])
requestSpotLaunchSpecification_blockDeviceMappings = Lens.lens (\RequestSpotLaunchSpecification' {blockDeviceMappings} -> blockDeviceMappings) (\s@RequestSpotLaunchSpecification' {} a -> s {blockDeviceMappings = a} :: RequestSpotLaunchSpecification) Prelude.. Lens.mapping Lens.coerced

-- | One or more security group IDs.
requestSpotLaunchSpecification_securityGroupIds :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe [Prelude.Text])
requestSpotLaunchSpecification_securityGroupIds = Lens.lens (\RequestSpotLaunchSpecification' {securityGroupIds} -> securityGroupIds) (\s@RequestSpotLaunchSpecification' {} a -> s {securityGroupIds = a} :: RequestSpotLaunchSpecification) Prelude.. Lens.mapping Lens.coerced

-- | Deprecated.
requestSpotLaunchSpecification_addressingType :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Text)
requestSpotLaunchSpecification_addressingType = Lens.lens (\RequestSpotLaunchSpecification' {addressingType} -> addressingType) (\s@RequestSpotLaunchSpecification' {} a -> s {addressingType = a} :: RequestSpotLaunchSpecification)

-- | Indicates whether basic or detailed monitoring is enabled for the
-- instance.
--
-- Default: Disabled
requestSpotLaunchSpecification_monitoring :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe RunInstancesMonitoringEnabled)
requestSpotLaunchSpecification_monitoring = Lens.lens (\RequestSpotLaunchSpecification' {monitoring} -> monitoring) (\s@RequestSpotLaunchSpecification' {} a -> s {monitoring = a} :: RequestSpotLaunchSpecification)

-- | The ID of the subnet in which to launch the instance.
requestSpotLaunchSpecification_subnetId :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Text)
requestSpotLaunchSpecification_subnetId = Lens.lens (\RequestSpotLaunchSpecification' {subnetId} -> subnetId) (\s@RequestSpotLaunchSpecification' {} a -> s {subnetId = a} :: RequestSpotLaunchSpecification)

-- | The instance type. Only one instance type can be specified.
requestSpotLaunchSpecification_instanceType :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe InstanceType)
requestSpotLaunchSpecification_instanceType = Lens.lens (\RequestSpotLaunchSpecification' {instanceType} -> instanceType) (\s@RequestSpotLaunchSpecification' {} a -> s {instanceType = a} :: RequestSpotLaunchSpecification)

-- | One or more security groups. When requesting instances in a VPC, you
-- must specify the IDs of the security groups. When requesting instances
-- in EC2-Classic, you can specify the names or the IDs of the security
-- groups.
requestSpotLaunchSpecification_securityGroups :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe [Prelude.Text])
requestSpotLaunchSpecification_securityGroups = Lens.lens (\RequestSpotLaunchSpecification' {securityGroups} -> securityGroups) (\s@RequestSpotLaunchSpecification' {} a -> s {securityGroups = a} :: RequestSpotLaunchSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the RAM disk.
requestSpotLaunchSpecification_ramdiskId :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Text)
requestSpotLaunchSpecification_ramdiskId = Lens.lens (\RequestSpotLaunchSpecification' {ramdiskId} -> ramdiskId) (\s@RequestSpotLaunchSpecification' {} a -> s {ramdiskId = a} :: RequestSpotLaunchSpecification)

-- | The name of the key pair.
requestSpotLaunchSpecification_keyName :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Text)
requestSpotLaunchSpecification_keyName = Lens.lens (\RequestSpotLaunchSpecification' {keyName} -> keyName) (\s@RequestSpotLaunchSpecification' {} a -> s {keyName = a} :: RequestSpotLaunchSpecification)

-- | The ID of the kernel.
requestSpotLaunchSpecification_kernelId :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Text)
requestSpotLaunchSpecification_kernelId = Lens.lens (\RequestSpotLaunchSpecification' {kernelId} -> kernelId) (\s@RequestSpotLaunchSpecification' {} a -> s {kernelId = a} :: RequestSpotLaunchSpecification)

-- | The ID of the AMI.
requestSpotLaunchSpecification_imageId :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe Prelude.Text)
requestSpotLaunchSpecification_imageId = Lens.lens (\RequestSpotLaunchSpecification' {imageId} -> imageId) (\s@RequestSpotLaunchSpecification' {} a -> s {imageId = a} :: RequestSpotLaunchSpecification)

-- | One or more network interfaces. If you specify a network interface, you
-- must specify subnet IDs and security group IDs using the network
-- interface.
requestSpotLaunchSpecification_networkInterfaces :: Lens.Lens' RequestSpotLaunchSpecification (Prelude.Maybe [InstanceNetworkInterfaceSpecification])
requestSpotLaunchSpecification_networkInterfaces = Lens.lens (\RequestSpotLaunchSpecification' {networkInterfaces} -> networkInterfaces) (\s@RequestSpotLaunchSpecification' {} a -> s {networkInterfaces = a} :: RequestSpotLaunchSpecification) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    RequestSpotLaunchSpecification
  where
  hashWithSalt
    _salt
    RequestSpotLaunchSpecification' {..} =
      _salt `Prelude.hashWithSalt` ebsOptimized
        `Prelude.hashWithSalt` iamInstanceProfile
        `Prelude.hashWithSalt` placement
        `Prelude.hashWithSalt` userData
        `Prelude.hashWithSalt` blockDeviceMappings
        `Prelude.hashWithSalt` securityGroupIds
        `Prelude.hashWithSalt` addressingType
        `Prelude.hashWithSalt` monitoring
        `Prelude.hashWithSalt` subnetId
        `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` securityGroups
        `Prelude.hashWithSalt` ramdiskId
        `Prelude.hashWithSalt` keyName
        `Prelude.hashWithSalt` kernelId
        `Prelude.hashWithSalt` imageId
        `Prelude.hashWithSalt` networkInterfaces

instance
  Prelude.NFData
    RequestSpotLaunchSpecification
  where
  rnf RequestSpotLaunchSpecification' {..} =
    Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf iamInstanceProfile
      `Prelude.seq` Prelude.rnf placement
      `Prelude.seq` Prelude.rnf userData
      `Prelude.seq` Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf addressingType
      `Prelude.seq` Prelude.rnf monitoring
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf ramdiskId
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf kernelId
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf networkInterfaces

instance Data.ToQuery RequestSpotLaunchSpecification where
  toQuery RequestSpotLaunchSpecification' {..} =
    Prelude.mconcat
      [ "EbsOptimized" Data.=: ebsOptimized,
        "IamInstanceProfile" Data.=: iamInstanceProfile,
        "Placement" Data.=: placement,
        "UserData" Data.=: userData,
        Data.toQuery
          ( Data.toQueryList "BlockDeviceMapping"
              Prelude.<$> blockDeviceMappings
          ),
        Data.toQuery
          ( Data.toQueryList "SecurityGroupId"
              Prelude.<$> securityGroupIds
          ),
        "AddressingType" Data.=: addressingType,
        "Monitoring" Data.=: monitoring,
        "SubnetId" Data.=: subnetId,
        "InstanceType" Data.=: instanceType,
        Data.toQuery
          ( Data.toQueryList "SecurityGroup"
              Prelude.<$> securityGroups
          ),
        "RamdiskId" Data.=: ramdiskId,
        "KeyName" Data.=: keyName,
        "KernelId" Data.=: kernelId,
        "ImageId" Data.=: imageId,
        Data.toQuery
          ( Data.toQueryList "NetworkInterface"
              Prelude.<$> networkInterfaces
          )
      ]
