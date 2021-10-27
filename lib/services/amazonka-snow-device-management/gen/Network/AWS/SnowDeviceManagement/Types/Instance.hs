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
-- Module      : Network.AWS.SnowDeviceManagement.Types.Instance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SnowDeviceManagement.Types.Instance where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SnowDeviceManagement.Types.CpuOptions
import Network.AWS.SnowDeviceManagement.Types.InstanceBlockDeviceMapping
import Network.AWS.SnowDeviceManagement.Types.InstanceState
import Network.AWS.SnowDeviceManagement.Types.SecurityGroupIdentifier

-- | The description of an instance. Currently, Amazon EC2 instances are the
-- only supported instance type.
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    state :: Prelude.Maybe InstanceState,
    -- | The security groups for the instance.
    securityGroups :: Prelude.Maybe [SecurityGroupIdentifier],
    -- | When the instance was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The CPU options for the instance.
    cpuOptions :: Prelude.Maybe CpuOptions,
    -- | The device name of the root device volume (for example, @\/dev\/sda1@).
    rootDeviceName :: Prelude.Maybe Prelude.Text,
    -- | The instance type.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AMI used to launch the instance.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The private IPv4 address assigned to the instance.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | When the instance was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | Any block device mapping entries for the instance.
    blockDeviceMappings :: Prelude.Maybe [InstanceBlockDeviceMapping],
    -- | The public IPv4 address assigned to the instance.
    publicIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Machine Image (AMI) launch index, which you can use to find
    -- this instance in the launch group.
    amiLaunchIndex :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'instance_instanceId' - The ID of the instance.
--
-- 'state', 'instance_state' - Undocumented member.
--
-- 'securityGroups', 'instance_securityGroups' - The security groups for the instance.
--
-- 'createdAt', 'instance_createdAt' - When the instance was created.
--
-- 'cpuOptions', 'instance_cpuOptions' - The CPU options for the instance.
--
-- 'rootDeviceName', 'instance_rootDeviceName' - The device name of the root device volume (for example, @\/dev\/sda1@).
--
-- 'instanceType', 'instance_instanceType' - The instance type.
--
-- 'imageId', 'instance_imageId' - The ID of the AMI used to launch the instance.
--
-- 'privateIpAddress', 'instance_privateIpAddress' - The private IPv4 address assigned to the instance.
--
-- 'updatedAt', 'instance_updatedAt' - When the instance was last updated.
--
-- 'blockDeviceMappings', 'instance_blockDeviceMappings' - Any block device mapping entries for the instance.
--
-- 'publicIpAddress', 'instance_publicIpAddress' - The public IPv4 address assigned to the instance.
--
-- 'amiLaunchIndex', 'instance_amiLaunchIndex' - The Amazon Machine Image (AMI) launch index, which you can use to find
-- this instance in the launch group.
newInstance ::
  Instance
newInstance =
  Instance'
    { instanceId = Prelude.Nothing,
      state = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      cpuOptions = Prelude.Nothing,
      rootDeviceName = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      imageId = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      publicIpAddress = Prelude.Nothing,
      amiLaunchIndex = Prelude.Nothing
    }

-- | The ID of the instance.
instance_instanceId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceId = Lens.lens (\Instance' {instanceId} -> instanceId) (\s@Instance' {} a -> s {instanceId = a} :: Instance)

-- | Undocumented member.
instance_state :: Lens.Lens' Instance (Prelude.Maybe InstanceState)
instance_state = Lens.lens (\Instance' {state} -> state) (\s@Instance' {} a -> s {state = a} :: Instance)

-- | The security groups for the instance.
instance_securityGroups :: Lens.Lens' Instance (Prelude.Maybe [SecurityGroupIdentifier])
instance_securityGroups = Lens.lens (\Instance' {securityGroups} -> securityGroups) (\s@Instance' {} a -> s {securityGroups = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | When the instance was created.
instance_createdAt :: Lens.Lens' Instance (Prelude.Maybe Prelude.UTCTime)
instance_createdAt = Lens.lens (\Instance' {createdAt} -> createdAt) (\s@Instance' {} a -> s {createdAt = a} :: Instance) Prelude.. Lens.mapping Core._Time

-- | The CPU options for the instance.
instance_cpuOptions :: Lens.Lens' Instance (Prelude.Maybe CpuOptions)
instance_cpuOptions = Lens.lens (\Instance' {cpuOptions} -> cpuOptions) (\s@Instance' {} a -> s {cpuOptions = a} :: Instance)

-- | The device name of the root device volume (for example, @\/dev\/sda1@).
instance_rootDeviceName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_rootDeviceName = Lens.lens (\Instance' {rootDeviceName} -> rootDeviceName) (\s@Instance' {} a -> s {rootDeviceName = a} :: Instance)

-- | The instance type.
instance_instanceType :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceType = Lens.lens (\Instance' {instanceType} -> instanceType) (\s@Instance' {} a -> s {instanceType = a} :: Instance)

-- | The ID of the AMI used to launch the instance.
instance_imageId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_imageId = Lens.lens (\Instance' {imageId} -> imageId) (\s@Instance' {} a -> s {imageId = a} :: Instance)

-- | The private IPv4 address assigned to the instance.
instance_privateIpAddress :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_privateIpAddress = Lens.lens (\Instance' {privateIpAddress} -> privateIpAddress) (\s@Instance' {} a -> s {privateIpAddress = a} :: Instance)

-- | When the instance was last updated.
instance_updatedAt :: Lens.Lens' Instance (Prelude.Maybe Prelude.UTCTime)
instance_updatedAt = Lens.lens (\Instance' {updatedAt} -> updatedAt) (\s@Instance' {} a -> s {updatedAt = a} :: Instance) Prelude.. Lens.mapping Core._Time

-- | Any block device mapping entries for the instance.
instance_blockDeviceMappings :: Lens.Lens' Instance (Prelude.Maybe [InstanceBlockDeviceMapping])
instance_blockDeviceMappings = Lens.lens (\Instance' {blockDeviceMappings} -> blockDeviceMappings) (\s@Instance' {} a -> s {blockDeviceMappings = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | The public IPv4 address assigned to the instance.
instance_publicIpAddress :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_publicIpAddress = Lens.lens (\Instance' {publicIpAddress} -> publicIpAddress) (\s@Instance' {} a -> s {publicIpAddress = a} :: Instance)

-- | The Amazon Machine Image (AMI) launch index, which you can use to find
-- this instance in the launch group.
instance_amiLaunchIndex :: Lens.Lens' Instance (Prelude.Maybe Prelude.Int)
instance_amiLaunchIndex = Lens.lens (\Instance' {amiLaunchIndex} -> amiLaunchIndex) (\s@Instance' {} a -> s {amiLaunchIndex = a} :: Instance)

instance Core.FromJSON Instance where
  parseJSON =
    Core.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Core..:? "instanceId")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "securityGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "cpuOptions")
            Prelude.<*> (x Core..:? "rootDeviceName")
            Prelude.<*> (x Core..:? "instanceType")
            Prelude.<*> (x Core..:? "imageId")
            Prelude.<*> (x Core..:? "privateIpAddress")
            Prelude.<*> (x Core..:? "updatedAt")
            Prelude.<*> ( x Core..:? "blockDeviceMappings"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "publicIpAddress")
            Prelude.<*> (x Core..:? "amiLaunchIndex")
      )

instance Prelude.Hashable Instance

instance Prelude.NFData Instance
