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
    sStatus,
    sDirectoryId,
    sStartTime,
    sName,
    sType,
    sSnapshotId,
  )
where

import Network.AWS.DirectoryService.Types.SnapshotStatus
import Network.AWS.DirectoryService.Types.SnapshotType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a directory snapshot.
--
-- /See:/ 'mkSnapshot' smart constructor.
data Snapshot = Snapshot'
  { status :: Lude.Maybe SnapshotStatus,
    directoryId :: Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe SnapshotType,
    snapshotId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- * 'directoryId' - The directory identifier.
-- * 'name' - The descriptive name of the snapshot.
-- * 'snapshotId' - The snapshot identifier.
-- * 'startTime' - The date and time that the snapshot was taken.
-- * 'status' - The snapshot status.
-- * 'type'' - The snapshot type.
mkSnapshot ::
  Snapshot
mkSnapshot =
  Snapshot'
    { status = Lude.Nothing,
      directoryId = Lude.Nothing,
      startTime = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing,
      snapshotId = Lude.Nothing
    }

-- | The snapshot status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStatus :: Lens.Lens' Snapshot (Lude.Maybe SnapshotStatus)
sStatus = Lens.lens (status :: Snapshot -> Lude.Maybe SnapshotStatus) (\s a -> s {status = a} :: Snapshot)
{-# DEPRECATED sStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The directory identifier.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDirectoryId :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sDirectoryId = Lens.lens (directoryId :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: Snapshot)
{-# DEPRECATED sDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The date and time that the snapshot was taken.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartTime :: Lens.Lens' Snapshot (Lude.Maybe Lude.Timestamp)
sStartTime = Lens.lens (startTime :: Snapshot -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: Snapshot)
{-# DEPRECATED sStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The descriptive name of the snapshot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sName = Lens.lens (name :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Snapshot)
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The snapshot type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sType :: Lens.Lens' Snapshot (Lude.Maybe SnapshotType)
sType = Lens.lens (type' :: Snapshot -> Lude.Maybe SnapshotType) (\s a -> s {type' = a} :: Snapshot)
{-# DEPRECATED sType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The snapshot identifier.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotId :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sSnapshotId = Lens.lens (snapshotId :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: Snapshot)
{-# DEPRECATED sSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.FromJSON Snapshot where
  parseJSON =
    Lude.withObject
      "Snapshot"
      ( \x ->
          Snapshot'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "DirectoryId")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "SnapshotId")
      )
