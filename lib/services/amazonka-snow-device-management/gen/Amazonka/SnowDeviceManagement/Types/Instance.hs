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
-- Module      : Amazonka.SnowDeviceManagement.Types.Instance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.Instance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SnowDeviceManagement.Types.CpuOptions
import Amazonka.SnowDeviceManagement.Types.InstanceBlockDeviceMapping
import Amazonka.SnowDeviceManagement.Types.InstanceState
import Amazonka.SnowDeviceManagement.Types.SecurityGroupIdentifier

-- | The description of an instance. Currently, Amazon EC2 instances are the
-- only supported instance type.
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The Amazon Machine Image (AMI) launch index, which you can use to find
    -- this instance in the launch group.
    amiLaunchIndex :: Prelude.Maybe Prelude.Int,
    -- | Any block device mapping entries for the instance.
    blockDeviceMappings :: Prelude.Maybe [InstanceBlockDeviceMapping],
    -- | The CPU options for the instance.
    cpuOptions :: Prelude.Maybe CpuOptions,
    -- | When the instance was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The ID of the AMI used to launch the instance.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The instance type.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The private IPv4 address assigned to the instance.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The public IPv4 address assigned to the instance.
    publicIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The device name of the root device volume (for example, @\/dev\/sda1@).
    rootDeviceName :: Prelude.Maybe Prelude.Text,
    -- | The security groups for the instance.
    securityGroups :: Prelude.Maybe [SecurityGroupIdentifier],
    state :: Prelude.Maybe InstanceState,
    -- | When the instance was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX
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
-- 'amiLaunchIndex', 'instance_amiLaunchIndex' - The Amazon Machine Image (AMI) launch index, which you can use to find
-- this instance in the launch group.
--
-- 'blockDeviceMappings', 'instance_blockDeviceMappings' - Any block device mapping entries for the instance.
--
-- 'cpuOptions', 'instance_cpuOptions' - The CPU options for the instance.
--
-- 'createdAt', 'instance_createdAt' - When the instance was created.
--
-- 'imageId', 'instance_imageId' - The ID of the AMI used to launch the instance.
--
-- 'instanceId', 'instance_instanceId' - The ID of the instance.
--
-- 'instanceType', 'instance_instanceType' - The instance type.
--
-- 'privateIpAddress', 'instance_privateIpAddress' - The private IPv4 address assigned to the instance.
--
-- 'publicIpAddress', 'instance_publicIpAddress' - The public IPv4 address assigned to the instance.
--
-- 'rootDeviceName', 'instance_rootDeviceName' - The device name of the root device volume (for example, @\/dev\/sda1@).
--
-- 'securityGroups', 'instance_securityGroups' - The security groups for the instance.
--
-- 'state', 'instance_state' - Undocumented member.
--
-- 'updatedAt', 'instance_updatedAt' - When the instance was last updated.
newInstance ::
  Instance
newInstance =
  Instance'
    { amiLaunchIndex = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      cpuOptions = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      imageId = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      publicIpAddress = Prelude.Nothing,
      rootDeviceName = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      state = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The Amazon Machine Image (AMI) launch index, which you can use to find
-- this instance in the launch group.
instance_amiLaunchIndex :: Lens.Lens' Instance (Prelude.Maybe Prelude.Int)
instance_amiLaunchIndex = Lens.lens (\Instance' {amiLaunchIndex} -> amiLaunchIndex) (\s@Instance' {} a -> s {amiLaunchIndex = a} :: Instance)

-- | Any block device mapping entries for the instance.
instance_blockDeviceMappings :: Lens.Lens' Instance (Prelude.Maybe [InstanceBlockDeviceMapping])
instance_blockDeviceMappings = Lens.lens (\Instance' {blockDeviceMappings} -> blockDeviceMappings) (\s@Instance' {} a -> s {blockDeviceMappings = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | The CPU options for the instance.
instance_cpuOptions :: Lens.Lens' Instance (Prelude.Maybe CpuOptions)
instance_cpuOptions = Lens.lens (\Instance' {cpuOptions} -> cpuOptions) (\s@Instance' {} a -> s {cpuOptions = a} :: Instance)

-- | When the instance was created.
instance_createdAt :: Lens.Lens' Instance (Prelude.Maybe Prelude.UTCTime)
instance_createdAt = Lens.lens (\Instance' {createdAt} -> createdAt) (\s@Instance' {} a -> s {createdAt = a} :: Instance) Prelude.. Lens.mapping Data._Time

-- | The ID of the AMI used to launch the instance.
instance_imageId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_imageId = Lens.lens (\Instance' {imageId} -> imageId) (\s@Instance' {} a -> s {imageId = a} :: Instance)

-- | The ID of the instance.
instance_instanceId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceId = Lens.lens (\Instance' {instanceId} -> instanceId) (\s@Instance' {} a -> s {instanceId = a} :: Instance)

-- | The instance type.
instance_instanceType :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceType = Lens.lens (\Instance' {instanceType} -> instanceType) (\s@Instance' {} a -> s {instanceType = a} :: Instance)

-- | The private IPv4 address assigned to the instance.
instance_privateIpAddress :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_privateIpAddress = Lens.lens (\Instance' {privateIpAddress} -> privateIpAddress) (\s@Instance' {} a -> s {privateIpAddress = a} :: Instance)

-- | The public IPv4 address assigned to the instance.
instance_publicIpAddress :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_publicIpAddress = Lens.lens (\Instance' {publicIpAddress} -> publicIpAddress) (\s@Instance' {} a -> s {publicIpAddress = a} :: Instance)

-- | The device name of the root device volume (for example, @\/dev\/sda1@).
instance_rootDeviceName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_rootDeviceName = Lens.lens (\Instance' {rootDeviceName} -> rootDeviceName) (\s@Instance' {} a -> s {rootDeviceName = a} :: Instance)

-- | The security groups for the instance.
instance_securityGroups :: Lens.Lens' Instance (Prelude.Maybe [SecurityGroupIdentifier])
instance_securityGroups = Lens.lens (\Instance' {securityGroups} -> securityGroups) (\s@Instance' {} a -> s {securityGroups = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
instance_state :: Lens.Lens' Instance (Prelude.Maybe InstanceState)
instance_state = Lens.lens (\Instance' {state} -> state) (\s@Instance' {} a -> s {state = a} :: Instance)

-- | When the instance was last updated.
instance_updatedAt :: Lens.Lens' Instance (Prelude.Maybe Prelude.UTCTime)
instance_updatedAt = Lens.lens (\Instance' {updatedAt} -> updatedAt) (\s@Instance' {} a -> s {updatedAt = a} :: Instance) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Instance where
  parseJSON =
    Data.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Data..:? "amiLaunchIndex")
            Prelude.<*> ( x
                            Data..:? "blockDeviceMappings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "cpuOptions")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "imageId")
            Prelude.<*> (x Data..:? "instanceId")
            Prelude.<*> (x Data..:? "instanceType")
            Prelude.<*> (x Data..:? "privateIpAddress")
            Prelude.<*> (x Data..:? "publicIpAddress")
            Prelude.<*> (x Data..:? "rootDeviceName")
            Prelude.<*> (x Data..:? "securityGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable Instance where
  hashWithSalt _salt Instance' {..} =
    _salt
      `Prelude.hashWithSalt` amiLaunchIndex
      `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` cpuOptions
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` publicIpAddress
      `Prelude.hashWithSalt` rootDeviceName
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Instance where
  rnf Instance' {..} =
    Prelude.rnf amiLaunchIndex `Prelude.seq`
      Prelude.rnf blockDeviceMappings `Prelude.seq`
        Prelude.rnf cpuOptions `Prelude.seq`
          Prelude.rnf createdAt `Prelude.seq`
            Prelude.rnf imageId `Prelude.seq`
              Prelude.rnf instanceId `Prelude.seq`
                Prelude.rnf instanceType `Prelude.seq`
                  Prelude.rnf privateIpAddress `Prelude.seq`
                    Prelude.rnf publicIpAddress `Prelude.seq`
                      Prelude.rnf rootDeviceName `Prelude.seq`
                        Prelude.rnf securityGroups `Prelude.seq`
                          Prelude.rnf state `Prelude.seq`
                            Prelude.rnf updatedAt
