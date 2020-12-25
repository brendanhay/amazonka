{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Snapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.Snapshot
  ( Snapshot (..),

    -- * Smart constructor
    mkSnapshot,

    -- * Lenses
    sDirectoryId,
    sName,
    sSnapshotId,
    sStartTime,
    sStatus,
    sType,
  )
where

import qualified Network.AWS.DirectoryService.Types.DirectoryId as Types
import qualified Network.AWS.DirectoryService.Types.Name as Types
import qualified Network.AWS.DirectoryService.Types.SnapshotId as Types
import qualified Network.AWS.DirectoryService.Types.SnapshotStatus as Types
import qualified Network.AWS.DirectoryService.Types.SnapshotType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a directory snapshot.
--
-- /See:/ 'mkSnapshot' smart constructor.
data Snapshot = Snapshot'
  { -- | The directory identifier.
    directoryId :: Core.Maybe Types.DirectoryId,
    -- | The descriptive name of the snapshot.
    name :: Core.Maybe Types.Name,
    -- | The snapshot identifier.
    snapshotId :: Core.Maybe Types.SnapshotId,
    -- | The date and time that the snapshot was taken.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The snapshot status.
    status :: Core.Maybe Types.SnapshotStatus,
    -- | The snapshot type.
    type' :: Core.Maybe Types.SnapshotType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Snapshot' value with any optional fields omitted.
mkSnapshot ::
  Snapshot
mkSnapshot =
  Snapshot'
    { directoryId = Core.Nothing,
      name = Core.Nothing,
      snapshotId = Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing,
      type' = Core.Nothing
    }

-- | The directory identifier.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDirectoryId :: Lens.Lens' Snapshot (Core.Maybe Types.DirectoryId)
sDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED sDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The descriptive name of the snapshot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' Snapshot (Core.Maybe Types.Name)
sName = Lens.field @"name"
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The snapshot identifier.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotId :: Lens.Lens' Snapshot (Core.Maybe Types.SnapshotId)
sSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED sSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The date and time that the snapshot was taken.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartTime :: Lens.Lens' Snapshot (Core.Maybe Core.NominalDiffTime)
sStartTime = Lens.field @"startTime"
{-# DEPRECATED sStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The snapshot status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStatus :: Lens.Lens' Snapshot (Core.Maybe Types.SnapshotStatus)
sStatus = Lens.field @"status"
{-# DEPRECATED sStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The snapshot type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sType :: Lens.Lens' Snapshot (Core.Maybe Types.SnapshotType)
sType = Lens.field @"type'"
{-# DEPRECATED sType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Snapshot where
  parseJSON =
    Core.withObject "Snapshot" Core.$
      \x ->
        Snapshot'
          Core.<$> (x Core..:? "DirectoryId")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "SnapshotId")
          Core.<*> (x Core..:? "StartTime")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "Type")
