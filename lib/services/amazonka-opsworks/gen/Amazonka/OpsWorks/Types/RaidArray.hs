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
-- Module      : Amazonka.OpsWorks.Types.RaidArray
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.RaidArray where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an instance\'s RAID array.
--
-- /See:/ 'newRaidArray' smart constructor.
data RaidArray = RaidArray'
  { -- | The array\'s Availability Zone. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | When the RAID array was created.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The array\'s Linux device. For example \/dev\/mdadm0.
    device :: Prelude.Maybe Prelude.Text,
    -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | For PIOPS volumes, the IOPS per disk.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The array\'s mount point.
    mountPoint :: Prelude.Maybe Prelude.Text,
    -- | The array name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of disks in the array.
    numberOfDisks :: Prelude.Maybe Prelude.Int,
    -- | The array ID.
    raidArrayId :: Prelude.Maybe Prelude.Text,
    -- | The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
    raidLevel :: Prelude.Maybe Prelude.Int,
    -- | The array\'s size.
    size :: Prelude.Maybe Prelude.Int,
    -- | The stack ID.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The volume type, standard or PIOPS.
    volumeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RaidArray' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'raidArray_availabilityZone' - The array\'s Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- 'createdAt', 'raidArray_createdAt' - When the RAID array was created.
--
-- 'device', 'raidArray_device' - The array\'s Linux device. For example \/dev\/mdadm0.
--
-- 'instanceId', 'raidArray_instanceId' - The instance ID.
--
-- 'iops', 'raidArray_iops' - For PIOPS volumes, the IOPS per disk.
--
-- 'mountPoint', 'raidArray_mountPoint' - The array\'s mount point.
--
-- 'name', 'raidArray_name' - The array name.
--
-- 'numberOfDisks', 'raidArray_numberOfDisks' - The number of disks in the array.
--
-- 'raidArrayId', 'raidArray_raidArrayId' - The array ID.
--
-- 'raidLevel', 'raidArray_raidLevel' - The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
--
-- 'size', 'raidArray_size' - The array\'s size.
--
-- 'stackId', 'raidArray_stackId' - The stack ID.
--
-- 'volumeType', 'raidArray_volumeType' - The volume type, standard or PIOPS.
newRaidArray ::
  RaidArray
newRaidArray =
  RaidArray'
    { availabilityZone = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      device = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      iops = Prelude.Nothing,
      mountPoint = Prelude.Nothing,
      name = Prelude.Nothing,
      numberOfDisks = Prelude.Nothing,
      raidArrayId = Prelude.Nothing,
      raidLevel = Prelude.Nothing,
      size = Prelude.Nothing,
      stackId = Prelude.Nothing,
      volumeType = Prelude.Nothing
    }

-- | The array\'s Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
raidArray_availabilityZone :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_availabilityZone = Lens.lens (\RaidArray' {availabilityZone} -> availabilityZone) (\s@RaidArray' {} a -> s {availabilityZone = a} :: RaidArray)

-- | When the RAID array was created.
raidArray_createdAt :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_createdAt = Lens.lens (\RaidArray' {createdAt} -> createdAt) (\s@RaidArray' {} a -> s {createdAt = a} :: RaidArray)

-- | The array\'s Linux device. For example \/dev\/mdadm0.
raidArray_device :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_device = Lens.lens (\RaidArray' {device} -> device) (\s@RaidArray' {} a -> s {device = a} :: RaidArray)

-- | The instance ID.
raidArray_instanceId :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_instanceId = Lens.lens (\RaidArray' {instanceId} -> instanceId) (\s@RaidArray' {} a -> s {instanceId = a} :: RaidArray)

-- | For PIOPS volumes, the IOPS per disk.
raidArray_iops :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Int)
raidArray_iops = Lens.lens (\RaidArray' {iops} -> iops) (\s@RaidArray' {} a -> s {iops = a} :: RaidArray)

-- | The array\'s mount point.
raidArray_mountPoint :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_mountPoint = Lens.lens (\RaidArray' {mountPoint} -> mountPoint) (\s@RaidArray' {} a -> s {mountPoint = a} :: RaidArray)

-- | The array name.
raidArray_name :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_name = Lens.lens (\RaidArray' {name} -> name) (\s@RaidArray' {} a -> s {name = a} :: RaidArray)

-- | The number of disks in the array.
raidArray_numberOfDisks :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Int)
raidArray_numberOfDisks = Lens.lens (\RaidArray' {numberOfDisks} -> numberOfDisks) (\s@RaidArray' {} a -> s {numberOfDisks = a} :: RaidArray)

-- | The array ID.
raidArray_raidArrayId :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_raidArrayId = Lens.lens (\RaidArray' {raidArrayId} -> raidArrayId) (\s@RaidArray' {} a -> s {raidArrayId = a} :: RaidArray)

-- | The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
raidArray_raidLevel :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Int)
raidArray_raidLevel = Lens.lens (\RaidArray' {raidLevel} -> raidLevel) (\s@RaidArray' {} a -> s {raidLevel = a} :: RaidArray)

-- | The array\'s size.
raidArray_size :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Int)
raidArray_size = Lens.lens (\RaidArray' {size} -> size) (\s@RaidArray' {} a -> s {size = a} :: RaidArray)

-- | The stack ID.
raidArray_stackId :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_stackId = Lens.lens (\RaidArray' {stackId} -> stackId) (\s@RaidArray' {} a -> s {stackId = a} :: RaidArray)

-- | The volume type, standard or PIOPS.
raidArray_volumeType :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_volumeType = Lens.lens (\RaidArray' {volumeType} -> volumeType) (\s@RaidArray' {} a -> s {volumeType = a} :: RaidArray)

instance Data.FromJSON RaidArray where
  parseJSON =
    Data.withObject
      "RaidArray"
      ( \x ->
          RaidArray'
            Prelude.<$> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Device")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "Iops")
            Prelude.<*> (x Data..:? "MountPoint")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NumberOfDisks")
            Prelude.<*> (x Data..:? "RaidArrayId")
            Prelude.<*> (x Data..:? "RaidLevel")
            Prelude.<*> (x Data..:? "Size")
            Prelude.<*> (x Data..:? "StackId")
            Prelude.<*> (x Data..:? "VolumeType")
      )

instance Prelude.Hashable RaidArray where
  hashWithSalt _salt RaidArray' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` device
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` mountPoint
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` numberOfDisks
      `Prelude.hashWithSalt` raidArrayId
      `Prelude.hashWithSalt` raidLevel
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` volumeType

instance Prelude.NFData RaidArray where
  rnf RaidArray' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf device
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf mountPoint
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf numberOfDisks
      `Prelude.seq` Prelude.rnf raidArrayId
      `Prelude.seq` Prelude.rnf raidLevel
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf volumeType
