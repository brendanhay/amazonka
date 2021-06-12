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

-- | Describes an instance\'s RAID array.
--
-- /See:/ 'newRaidArray' smart constructor.
data RaidArray = RaidArray'
  { -- | The number of disks in the array.
    numberOfDisks :: Core.Maybe Core.Int,
    -- | The instance ID.
    instanceId :: Core.Maybe Core.Text,
    -- | The stack ID.
    stackId :: Core.Maybe Core.Text,
    -- | The array\'s Linux device. For example \/dev\/mdadm0.
    device :: Core.Maybe Core.Text,
    -- | When the RAID array was created.
    createdAt :: Core.Maybe Core.Text,
    -- | The array ID.
    raidArrayId :: Core.Maybe Core.Text,
    -- | The array\'s Availability Zone. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The array name.
    name :: Core.Maybe Core.Text,
    -- | The array\'s mount point.
    mountPoint :: Core.Maybe Core.Text,
    -- | The volume type, standard or PIOPS.
    volumeType :: Core.Maybe Core.Text,
    -- | The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
    raidLevel :: Core.Maybe Core.Int,
    -- | For PIOPS volumes, the IOPS per disk.
    iops :: Core.Maybe Core.Int,
    -- | The array\'s size.
    size :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RaidArray' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfDisks', 'raidArray_numberOfDisks' - The number of disks in the array.
--
-- 'instanceId', 'raidArray_instanceId' - The instance ID.
--
-- 'stackId', 'raidArray_stackId' - The stack ID.
--
-- 'device', 'raidArray_device' - The array\'s Linux device. For example \/dev\/mdadm0.
--
-- 'createdAt', 'raidArray_createdAt' - When the RAID array was created.
--
-- 'raidArrayId', 'raidArray_raidArrayId' - The array ID.
--
-- 'availabilityZone', 'raidArray_availabilityZone' - The array\'s Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- 'name', 'raidArray_name' - The array name.
--
-- 'mountPoint', 'raidArray_mountPoint' - The array\'s mount point.
--
-- 'volumeType', 'raidArray_volumeType' - The volume type, standard or PIOPS.
--
-- 'raidLevel', 'raidArray_raidLevel' - The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
--
-- 'iops', 'raidArray_iops' - For PIOPS volumes, the IOPS per disk.
--
-- 'size', 'raidArray_size' - The array\'s size.
newRaidArray ::
  RaidArray
newRaidArray =
  RaidArray'
    { numberOfDisks = Core.Nothing,
      instanceId = Core.Nothing,
      stackId = Core.Nothing,
      device = Core.Nothing,
      createdAt = Core.Nothing,
      raidArrayId = Core.Nothing,
      availabilityZone = Core.Nothing,
      name = Core.Nothing,
      mountPoint = Core.Nothing,
      volumeType = Core.Nothing,
      raidLevel = Core.Nothing,
      iops = Core.Nothing,
      size = Core.Nothing
    }

-- | The number of disks in the array.
raidArray_numberOfDisks :: Lens.Lens' RaidArray (Core.Maybe Core.Int)
raidArray_numberOfDisks = Lens.lens (\RaidArray' {numberOfDisks} -> numberOfDisks) (\s@RaidArray' {} a -> s {numberOfDisks = a} :: RaidArray)

-- | The instance ID.
raidArray_instanceId :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raidArray_instanceId = Lens.lens (\RaidArray' {instanceId} -> instanceId) (\s@RaidArray' {} a -> s {instanceId = a} :: RaidArray)

-- | The stack ID.
raidArray_stackId :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raidArray_stackId = Lens.lens (\RaidArray' {stackId} -> stackId) (\s@RaidArray' {} a -> s {stackId = a} :: RaidArray)

-- | The array\'s Linux device. For example \/dev\/mdadm0.
raidArray_device :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raidArray_device = Lens.lens (\RaidArray' {device} -> device) (\s@RaidArray' {} a -> s {device = a} :: RaidArray)

-- | When the RAID array was created.
raidArray_createdAt :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raidArray_createdAt = Lens.lens (\RaidArray' {createdAt} -> createdAt) (\s@RaidArray' {} a -> s {createdAt = a} :: RaidArray)

-- | The array ID.
raidArray_raidArrayId :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raidArray_raidArrayId = Lens.lens (\RaidArray' {raidArrayId} -> raidArrayId) (\s@RaidArray' {} a -> s {raidArrayId = a} :: RaidArray)

-- | The array\'s Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
raidArray_availabilityZone :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raidArray_availabilityZone = Lens.lens (\RaidArray' {availabilityZone} -> availabilityZone) (\s@RaidArray' {} a -> s {availabilityZone = a} :: RaidArray)

-- | The array name.
raidArray_name :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raidArray_name = Lens.lens (\RaidArray' {name} -> name) (\s@RaidArray' {} a -> s {name = a} :: RaidArray)

-- | The array\'s mount point.
raidArray_mountPoint :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raidArray_mountPoint = Lens.lens (\RaidArray' {mountPoint} -> mountPoint) (\s@RaidArray' {} a -> s {mountPoint = a} :: RaidArray)

-- | The volume type, standard or PIOPS.
raidArray_volumeType :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raidArray_volumeType = Lens.lens (\RaidArray' {volumeType} -> volumeType) (\s@RaidArray' {} a -> s {volumeType = a} :: RaidArray)

-- | The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
raidArray_raidLevel :: Lens.Lens' RaidArray (Core.Maybe Core.Int)
raidArray_raidLevel = Lens.lens (\RaidArray' {raidLevel} -> raidLevel) (\s@RaidArray' {} a -> s {raidLevel = a} :: RaidArray)

-- | For PIOPS volumes, the IOPS per disk.
raidArray_iops :: Lens.Lens' RaidArray (Core.Maybe Core.Int)
raidArray_iops = Lens.lens (\RaidArray' {iops} -> iops) (\s@RaidArray' {} a -> s {iops = a} :: RaidArray)

-- | The array\'s size.
raidArray_size :: Lens.Lens' RaidArray (Core.Maybe Core.Int)
raidArray_size = Lens.lens (\RaidArray' {size} -> size) (\s@RaidArray' {} a -> s {size = a} :: RaidArray)

instance Core.FromJSON RaidArray where
  parseJSON =
    Core.withObject
      "RaidArray"
      ( \x ->
          RaidArray'
            Core.<$> (x Core..:? "NumberOfDisks")
            Core.<*> (x Core..:? "InstanceId")
            Core.<*> (x Core..:? "StackId")
            Core.<*> (x Core..:? "Device")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "RaidArrayId")
            Core.<*> (x Core..:? "AvailabilityZone")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "MountPoint")
            Core.<*> (x Core..:? "VolumeType")
            Core.<*> (x Core..:? "RaidLevel")
            Core.<*> (x Core..:? "Iops")
            Core.<*> (x Core..:? "Size")
      )

instance Core.Hashable RaidArray

instance Core.NFData RaidArray
