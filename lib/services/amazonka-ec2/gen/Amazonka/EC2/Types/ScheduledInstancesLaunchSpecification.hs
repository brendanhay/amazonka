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
-- Module      : Amazonka.EC2.Types.ScheduledInstancesLaunchSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ScheduledInstancesLaunchSpecification where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ScheduledInstancesBlockDeviceMapping
import Amazonka.EC2.Types.ScheduledInstancesIamInstanceProfile
import Amazonka.EC2.Types.ScheduledInstancesMonitoring
import Amazonka.EC2.Types.ScheduledInstancesNetworkInterface
import Amazonka.EC2.Types.ScheduledInstancesPlacement
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the launch specification for a Scheduled Instance.
--
-- If you are launching the Scheduled Instance in EC2-VPC, you must specify
-- the ID of the subnet. You can specify the subnet using either @SubnetId@
-- or @NetworkInterface@.
--
-- /See:/ 'newScheduledInstancesLaunchSpecification' smart constructor.
data ScheduledInstancesLaunchSpecification = ScheduledInstancesLaunchSpecification'
  { -- | The IDs of the security groups.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the key pair.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The network interfaces.
    networkInterfaces :: Prelude.Maybe [ScheduledInstancesNetworkInterface],
    -- | The ID of the RAM disk.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet in which to launch the instances.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the kernel.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The instance type.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the instances are optimized for EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal EBS I\/O performance.
    -- This optimization isn\'t available with all instance types. Additional
    -- usage charges apply when using an EBS-optimized instance.
    --
    -- Default: @false@
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The base64-encoded MIME user data.
    userData :: Prelude.Maybe Prelude.Text,
    -- | Enable or disable monitoring for the instances.
    monitoring :: Prelude.Maybe ScheduledInstancesMonitoring,
    -- | The IAM instance profile.
    iamInstanceProfile :: Prelude.Maybe ScheduledInstancesIamInstanceProfile,
    -- | The block device mapping entries.
    blockDeviceMappings :: Prelude.Maybe [ScheduledInstancesBlockDeviceMapping],
    -- | The placement information.
    placement :: Prelude.Maybe ScheduledInstancesPlacement,
    -- | The ID of the Amazon Machine Image (AMI).
    imageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledInstancesLaunchSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'scheduledInstancesLaunchSpecification_securityGroupIds' - The IDs of the security groups.
--
-- 'keyName', 'scheduledInstancesLaunchSpecification_keyName' - The name of the key pair.
--
-- 'networkInterfaces', 'scheduledInstancesLaunchSpecification_networkInterfaces' - The network interfaces.
--
-- 'ramdiskId', 'scheduledInstancesLaunchSpecification_ramdiskId' - The ID of the RAM disk.
--
-- 'subnetId', 'scheduledInstancesLaunchSpecification_subnetId' - The ID of the subnet in which to launch the instances.
--
-- 'kernelId', 'scheduledInstancesLaunchSpecification_kernelId' - The ID of the kernel.
--
-- 'instanceType', 'scheduledInstancesLaunchSpecification_instanceType' - The instance type.
--
-- 'ebsOptimized', 'scheduledInstancesLaunchSpecification_ebsOptimized' - Indicates whether the instances are optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS-optimized instance.
--
-- Default: @false@
--
-- 'userData', 'scheduledInstancesLaunchSpecification_userData' - The base64-encoded MIME user data.
--
-- 'monitoring', 'scheduledInstancesLaunchSpecification_monitoring' - Enable or disable monitoring for the instances.
--
-- 'iamInstanceProfile', 'scheduledInstancesLaunchSpecification_iamInstanceProfile' - The IAM instance profile.
--
-- 'blockDeviceMappings', 'scheduledInstancesLaunchSpecification_blockDeviceMappings' - The block device mapping entries.
--
-- 'placement', 'scheduledInstancesLaunchSpecification_placement' - The placement information.
--
-- 'imageId', 'scheduledInstancesLaunchSpecification_imageId' - The ID of the Amazon Machine Image (AMI).
newScheduledInstancesLaunchSpecification ::
  -- | 'imageId'
  Prelude.Text ->
  ScheduledInstancesLaunchSpecification
newScheduledInstancesLaunchSpecification pImageId_ =
  ScheduledInstancesLaunchSpecification'
    { securityGroupIds =
        Prelude.Nothing,
      keyName = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      ramdiskId = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      userData = Prelude.Nothing,
      monitoring = Prelude.Nothing,
      iamInstanceProfile = Prelude.Nothing,
      blockDeviceMappings =
        Prelude.Nothing,
      placement = Prelude.Nothing,
      imageId = pImageId_
    }

-- | The IDs of the security groups.
scheduledInstancesLaunchSpecification_securityGroupIds :: Lens.Lens' ScheduledInstancesLaunchSpecification (Prelude.Maybe [Prelude.Text])
scheduledInstancesLaunchSpecification_securityGroupIds = Lens.lens (\ScheduledInstancesLaunchSpecification' {securityGroupIds} -> securityGroupIds) (\s@ScheduledInstancesLaunchSpecification' {} a -> s {securityGroupIds = a} :: ScheduledInstancesLaunchSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The name of the key pair.
scheduledInstancesLaunchSpecification_keyName :: Lens.Lens' ScheduledInstancesLaunchSpecification (Prelude.Maybe Prelude.Text)
scheduledInstancesLaunchSpecification_keyName = Lens.lens (\ScheduledInstancesLaunchSpecification' {keyName} -> keyName) (\s@ScheduledInstancesLaunchSpecification' {} a -> s {keyName = a} :: ScheduledInstancesLaunchSpecification)

-- | The network interfaces.
scheduledInstancesLaunchSpecification_networkInterfaces :: Lens.Lens' ScheduledInstancesLaunchSpecification (Prelude.Maybe [ScheduledInstancesNetworkInterface])
scheduledInstancesLaunchSpecification_networkInterfaces = Lens.lens (\ScheduledInstancesLaunchSpecification' {networkInterfaces} -> networkInterfaces) (\s@ScheduledInstancesLaunchSpecification' {} a -> s {networkInterfaces = a} :: ScheduledInstancesLaunchSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the RAM disk.
scheduledInstancesLaunchSpecification_ramdiskId :: Lens.Lens' ScheduledInstancesLaunchSpecification (Prelude.Maybe Prelude.Text)
scheduledInstancesLaunchSpecification_ramdiskId = Lens.lens (\ScheduledInstancesLaunchSpecification' {ramdiskId} -> ramdiskId) (\s@ScheduledInstancesLaunchSpecification' {} a -> s {ramdiskId = a} :: ScheduledInstancesLaunchSpecification)

-- | The ID of the subnet in which to launch the instances.
scheduledInstancesLaunchSpecification_subnetId :: Lens.Lens' ScheduledInstancesLaunchSpecification (Prelude.Maybe Prelude.Text)
scheduledInstancesLaunchSpecification_subnetId = Lens.lens (\ScheduledInstancesLaunchSpecification' {subnetId} -> subnetId) (\s@ScheduledInstancesLaunchSpecification' {} a -> s {subnetId = a} :: ScheduledInstancesLaunchSpecification)

-- | The ID of the kernel.
scheduledInstancesLaunchSpecification_kernelId :: Lens.Lens' ScheduledInstancesLaunchSpecification (Prelude.Maybe Prelude.Text)
scheduledInstancesLaunchSpecification_kernelId = Lens.lens (\ScheduledInstancesLaunchSpecification' {kernelId} -> kernelId) (\s@ScheduledInstancesLaunchSpecification' {} a -> s {kernelId = a} :: ScheduledInstancesLaunchSpecification)

-- | The instance type.
scheduledInstancesLaunchSpecification_instanceType :: Lens.Lens' ScheduledInstancesLaunchSpecification (Prelude.Maybe Prelude.Text)
scheduledInstancesLaunchSpecification_instanceType = Lens.lens (\ScheduledInstancesLaunchSpecification' {instanceType} -> instanceType) (\s@ScheduledInstancesLaunchSpecification' {} a -> s {instanceType = a} :: ScheduledInstancesLaunchSpecification)

-- | Indicates whether the instances are optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS-optimized instance.
--
-- Default: @false@
scheduledInstancesLaunchSpecification_ebsOptimized :: Lens.Lens' ScheduledInstancesLaunchSpecification (Prelude.Maybe Prelude.Bool)
scheduledInstancesLaunchSpecification_ebsOptimized = Lens.lens (\ScheduledInstancesLaunchSpecification' {ebsOptimized} -> ebsOptimized) (\s@ScheduledInstancesLaunchSpecification' {} a -> s {ebsOptimized = a} :: ScheduledInstancesLaunchSpecification)

-- | The base64-encoded MIME user data.
scheduledInstancesLaunchSpecification_userData :: Lens.Lens' ScheduledInstancesLaunchSpecification (Prelude.Maybe Prelude.Text)
scheduledInstancesLaunchSpecification_userData = Lens.lens (\ScheduledInstancesLaunchSpecification' {userData} -> userData) (\s@ScheduledInstancesLaunchSpecification' {} a -> s {userData = a} :: ScheduledInstancesLaunchSpecification)

-- | Enable or disable monitoring for the instances.
scheduledInstancesLaunchSpecification_monitoring :: Lens.Lens' ScheduledInstancesLaunchSpecification (Prelude.Maybe ScheduledInstancesMonitoring)
scheduledInstancesLaunchSpecification_monitoring = Lens.lens (\ScheduledInstancesLaunchSpecification' {monitoring} -> monitoring) (\s@ScheduledInstancesLaunchSpecification' {} a -> s {monitoring = a} :: ScheduledInstancesLaunchSpecification)

-- | The IAM instance profile.
scheduledInstancesLaunchSpecification_iamInstanceProfile :: Lens.Lens' ScheduledInstancesLaunchSpecification (Prelude.Maybe ScheduledInstancesIamInstanceProfile)
scheduledInstancesLaunchSpecification_iamInstanceProfile = Lens.lens (\ScheduledInstancesLaunchSpecification' {iamInstanceProfile} -> iamInstanceProfile) (\s@ScheduledInstancesLaunchSpecification' {} a -> s {iamInstanceProfile = a} :: ScheduledInstancesLaunchSpecification)

-- | The block device mapping entries.
scheduledInstancesLaunchSpecification_blockDeviceMappings :: Lens.Lens' ScheduledInstancesLaunchSpecification (Prelude.Maybe [ScheduledInstancesBlockDeviceMapping])
scheduledInstancesLaunchSpecification_blockDeviceMappings = Lens.lens (\ScheduledInstancesLaunchSpecification' {blockDeviceMappings} -> blockDeviceMappings) (\s@ScheduledInstancesLaunchSpecification' {} a -> s {blockDeviceMappings = a} :: ScheduledInstancesLaunchSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The placement information.
scheduledInstancesLaunchSpecification_placement :: Lens.Lens' ScheduledInstancesLaunchSpecification (Prelude.Maybe ScheduledInstancesPlacement)
scheduledInstancesLaunchSpecification_placement = Lens.lens (\ScheduledInstancesLaunchSpecification' {placement} -> placement) (\s@ScheduledInstancesLaunchSpecification' {} a -> s {placement = a} :: ScheduledInstancesLaunchSpecification)

-- | The ID of the Amazon Machine Image (AMI).
scheduledInstancesLaunchSpecification_imageId :: Lens.Lens' ScheduledInstancesLaunchSpecification Prelude.Text
scheduledInstancesLaunchSpecification_imageId = Lens.lens (\ScheduledInstancesLaunchSpecification' {imageId} -> imageId) (\s@ScheduledInstancesLaunchSpecification' {} a -> s {imageId = a} :: ScheduledInstancesLaunchSpecification)

instance
  Prelude.Hashable
    ScheduledInstancesLaunchSpecification
  where
  hashWithSalt
    salt'
    ScheduledInstancesLaunchSpecification' {..} =
      salt' `Prelude.hashWithSalt` imageId
        `Prelude.hashWithSalt` placement
        `Prelude.hashWithSalt` blockDeviceMappings
        `Prelude.hashWithSalt` iamInstanceProfile
        `Prelude.hashWithSalt` monitoring
        `Prelude.hashWithSalt` userData
        `Prelude.hashWithSalt` ebsOptimized
        `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` kernelId
        `Prelude.hashWithSalt` subnetId
        `Prelude.hashWithSalt` ramdiskId
        `Prelude.hashWithSalt` networkInterfaces
        `Prelude.hashWithSalt` keyName
        `Prelude.hashWithSalt` securityGroupIds

instance
  Prelude.NFData
    ScheduledInstancesLaunchSpecification
  where
  rnf ScheduledInstancesLaunchSpecification' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf placement
      `Prelude.seq` Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf iamInstanceProfile
      `Prelude.seq` Prelude.rnf monitoring
      `Prelude.seq` Prelude.rnf userData
      `Prelude.seq` Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf kernelId
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf ramdiskId
      `Prelude.seq` Prelude.rnf networkInterfaces
      `Prelude.seq` Prelude.rnf keyName

instance
  Core.ToQuery
    ScheduledInstancesLaunchSpecification
  where
  toQuery ScheduledInstancesLaunchSpecification' {..} =
    Prelude.mconcat
      [ Core.toQuery
          ( Core.toQueryList "SecurityGroupId"
              Prelude.<$> securityGroupIds
          ),
        "KeyName" Core.=: keyName,
        Core.toQuery
          ( Core.toQueryList "NetworkInterface"
              Prelude.<$> networkInterfaces
          ),
        "RamdiskId" Core.=: ramdiskId,
        "SubnetId" Core.=: subnetId,
        "KernelId" Core.=: kernelId,
        "InstanceType" Core.=: instanceType,
        "EbsOptimized" Core.=: ebsOptimized,
        "UserData" Core.=: userData,
        "Monitoring" Core.=: monitoring,
        "IamInstanceProfile" Core.=: iamInstanceProfile,
        Core.toQuery
          ( Core.toQueryList "BlockDeviceMapping"
              Prelude.<$> blockDeviceMappings
          ),
        "Placement" Core.=: placement,
        "ImageId" Core.=: imageId
      ]
