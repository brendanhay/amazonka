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
-- Module      : Network.AWS.DirectoryService.Types.Snapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.Snapshot where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types.SnapshotStatus
import Network.AWS.DirectoryService.Types.SnapshotType
import qualified Network.AWS.Lens as Lens

-- | Describes a directory snapshot.
--
-- /See:/ 'newSnapshot' smart constructor.
data Snapshot = Snapshot'
  { -- | The snapshot status.
    status :: Core.Maybe SnapshotStatus,
    -- | The date and time that the snapshot was taken.
    startTime :: Core.Maybe Core.POSIX,
    -- | The descriptive name of the snapshot.
    name :: Core.Maybe Core.Text,
    -- | The directory identifier.
    directoryId :: Core.Maybe Core.Text,
    -- | The snapshot identifier.
    snapshotId :: Core.Maybe Core.Text,
    -- | The snapshot type.
    type' :: Core.Maybe SnapshotType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Snapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'snapshot_status' - The snapshot status.
--
-- 'startTime', 'snapshot_startTime' - The date and time that the snapshot was taken.
--
-- 'name', 'snapshot_name' - The descriptive name of the snapshot.
--
-- 'directoryId', 'snapshot_directoryId' - The directory identifier.
--
-- 'snapshotId', 'snapshot_snapshotId' - The snapshot identifier.
--
-- 'type'', 'snapshot_type' - The snapshot type.
newSnapshot ::
  Snapshot
newSnapshot =
  Snapshot'
    { status = Core.Nothing,
      startTime = Core.Nothing,
      name = Core.Nothing,
      directoryId = Core.Nothing,
      snapshotId = Core.Nothing,
      type' = Core.Nothing
    }

-- | The snapshot status.
snapshot_status :: Lens.Lens' Snapshot (Core.Maybe SnapshotStatus)
snapshot_status = Lens.lens (\Snapshot' {status} -> status) (\s@Snapshot' {} a -> s {status = a} :: Snapshot)

-- | The date and time that the snapshot was taken.
snapshot_startTime :: Lens.Lens' Snapshot (Core.Maybe Core.UTCTime)
snapshot_startTime = Lens.lens (\Snapshot' {startTime} -> startTime) (\s@Snapshot' {} a -> s {startTime = a} :: Snapshot) Core.. Lens.mapping Core._Time

-- | The descriptive name of the snapshot.
snapshot_name :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_name = Lens.lens (\Snapshot' {name} -> name) (\s@Snapshot' {} a -> s {name = a} :: Snapshot)

-- | The directory identifier.
snapshot_directoryId :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_directoryId = Lens.lens (\Snapshot' {directoryId} -> directoryId) (\s@Snapshot' {} a -> s {directoryId = a} :: Snapshot)

-- | The snapshot identifier.
snapshot_snapshotId :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_snapshotId = Lens.lens (\Snapshot' {snapshotId} -> snapshotId) (\s@Snapshot' {} a -> s {snapshotId = a} :: Snapshot)

-- | The snapshot type.
snapshot_type :: Lens.Lens' Snapshot (Core.Maybe SnapshotType)
snapshot_type = Lens.lens (\Snapshot' {type'} -> type') (\s@Snapshot' {} a -> s {type' = a} :: Snapshot)

instance Core.FromJSON Snapshot where
  parseJSON =
    Core.withObject
      "Snapshot"
      ( \x ->
          Snapshot'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "DirectoryId")
            Core.<*> (x Core..:? "SnapshotId")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable Snapshot

instance Core.NFData Snapshot
