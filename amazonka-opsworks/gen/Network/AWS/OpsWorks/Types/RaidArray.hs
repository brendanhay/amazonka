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
-- Module      : Network.AWS.OpsWorks.Types.RaidArray
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.RaidArray where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an instance\'s RAID array.
--
-- /See:/ 'newRaidArray' smart constructor.
data RaidArray = RaidArray'
  { -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The array\'s size.
    size :: Prelude.Maybe Prelude.Int,
    -- | For PIOPS volumes, the IOPS per disk.
    iops :: Prelude.Maybe Prelude.Int,
    -- | When the RAID array was created.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
    raidLevel :: Prelude.Maybe Prelude.Int,
    -- | The array\'s Linux device. For example \/dev\/mdadm0.
    device :: Prelude.Maybe Prelude.Text,
    -- | The number of disks in the array.
    numberOfDisks :: Prelude.Maybe Prelude.Int,
    -- | The array\'s Availability Zone. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The array name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The array ID.
    raidArrayId :: Prelude.Maybe Prelude.Text,
    -- | The volume type, standard or PIOPS.
    volumeType :: Prelude.Maybe Prelude.Text,
    -- | The stack ID.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The array\'s mount point.
    mountPoint :: Prelude.Maybe Prelude.Text
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
-- 'instanceId', 'raidArray_instanceId' - The instance ID.
--
-- 'size', 'raidArray_size' - The array\'s size.
--
-- 'iops', 'raidArray_iops' - For PIOPS volumes, the IOPS per disk.
--
-- 'createdAt', 'raidArray_createdAt' - When the RAID array was created.
--
-- 'raidLevel', 'raidArray_raidLevel' - The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
--
-- 'device', 'raidArray_device' - The array\'s Linux device. For example \/dev\/mdadm0.
--
-- 'numberOfDisks', 'raidArray_numberOfDisks' - The number of disks in the array.
--
-- 'availabilityZone', 'raidArray_availabilityZone' - The array\'s Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- 'name', 'raidArray_name' - The array name.
--
-- 'raidArrayId', 'raidArray_raidArrayId' - The array ID.
--
-- 'volumeType', 'raidArray_volumeType' - The volume type, standard or PIOPS.
--
-- 'stackId', 'raidArray_stackId' - The stack ID.
--
-- 'mountPoint', 'raidArray_mountPoint' - The array\'s mount point.
newRaidArray ::
  RaidArray
newRaidArray =
  RaidArray'
    { instanceId = Prelude.Nothing,
      size = Prelude.Nothing,
      iops = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      raidLevel = Prelude.Nothing,
      device = Prelude.Nothing,
      numberOfDisks = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      name = Prelude.Nothing,
      raidArrayId = Prelude.Nothing,
      volumeType = Prelude.Nothing,
      stackId = Prelude.Nothing,
      mountPoint = Prelude.Nothing
    }

-- | The instance ID.
raidArray_instanceId :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_instanceId = Lens.lens (\RaidArray' {instanceId} -> instanceId) (\s@RaidArray' {} a -> s {instanceId = a} :: RaidArray)

-- | The array\'s size.
raidArray_size :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Int)
raidArray_size = Lens.lens (\RaidArray' {size} -> size) (\s@RaidArray' {} a -> s {size = a} :: RaidArray)

-- | For PIOPS volumes, the IOPS per disk.
raidArray_iops :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Int)
raidArray_iops = Lens.lens (\RaidArray' {iops} -> iops) (\s@RaidArray' {} a -> s {iops = a} :: RaidArray)

-- | When the RAID array was created.
raidArray_createdAt :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_createdAt = Lens.lens (\RaidArray' {createdAt} -> createdAt) (\s@RaidArray' {} a -> s {createdAt = a} :: RaidArray)

-- | The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
raidArray_raidLevel :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Int)
raidArray_raidLevel = Lens.lens (\RaidArray' {raidLevel} -> raidLevel) (\s@RaidArray' {} a -> s {raidLevel = a} :: RaidArray)

-- | The array\'s Linux device. For example \/dev\/mdadm0.
raidArray_device :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_device = Lens.lens (\RaidArray' {device} -> device) (\s@RaidArray' {} a -> s {device = a} :: RaidArray)

-- | The number of disks in the array.
raidArray_numberOfDisks :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Int)
raidArray_numberOfDisks = Lens.lens (\RaidArray' {numberOfDisks} -> numberOfDisks) (\s@RaidArray' {} a -> s {numberOfDisks = a} :: RaidArray)

-- | The array\'s Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
raidArray_availabilityZone :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_availabilityZone = Lens.lens (\RaidArray' {availabilityZone} -> availabilityZone) (\s@RaidArray' {} a -> s {availabilityZone = a} :: RaidArray)

-- | The array name.
raidArray_name :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_name = Lens.lens (\RaidArray' {name} -> name) (\s@RaidArray' {} a -> s {name = a} :: RaidArray)

-- | The array ID.
raidArray_raidArrayId :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_raidArrayId = Lens.lens (\RaidArray' {raidArrayId} -> raidArrayId) (\s@RaidArray' {} a -> s {raidArrayId = a} :: RaidArray)

-- | The volume type, standard or PIOPS.
raidArray_volumeType :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_volumeType = Lens.lens (\RaidArray' {volumeType} -> volumeType) (\s@RaidArray' {} a -> s {volumeType = a} :: RaidArray)

-- | The stack ID.
raidArray_stackId :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_stackId = Lens.lens (\RaidArray' {stackId} -> stackId) (\s@RaidArray' {} a -> s {stackId = a} :: RaidArray)

-- | The array\'s mount point.
raidArray_mountPoint :: Lens.Lens' RaidArray (Prelude.Maybe Prelude.Text)
raidArray_mountPoint = Lens.lens (\RaidArray' {mountPoint} -> mountPoint) (\s@RaidArray' {} a -> s {mountPoint = a} :: RaidArray)

instance Core.FromJSON RaidArray where
  parseJSON =
    Core.withObject
      "RaidArray"
      ( \x ->
          RaidArray'
            Prelude.<$> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "Size")
            Prelude.<*> (x Core..:? "Iops")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "RaidLevel")
            Prelude.<*> (x Core..:? "Device")
            Prelude.<*> (x Core..:? "NumberOfDisks")
            Prelude.<*> (x Core..:? "AvailabilityZone")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "RaidArrayId")
            Prelude.<*> (x Core..:? "VolumeType")
            Prelude.<*> (x Core..:? "StackId")
            Prelude.<*> (x Core..:? "MountPoint")
      )

instance Prelude.Hashable RaidArray

instance Prelude.NFData RaidArray
