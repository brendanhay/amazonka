{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotInfo
  ( SnapshotInfo (..),

    -- * Smart constructor
    mkSnapshotInfo,

    -- * Lenses
    siDescription,
    siEncrypted,
    siOwnerId,
    siProgress,
    siSnapshotId,
    siStartTime,
    siState,
    siTags,
    siVolumeId,
    siVolumeSize,
  )
where

import qualified Network.AWS.EC2.Types.Description as Types
import qualified Network.AWS.EC2.Types.OwnerId as Types
import qualified Network.AWS.EC2.Types.Progress as Types
import qualified Network.AWS.EC2.Types.SnapshotId as Types
import qualified Network.AWS.EC2.Types.SnapshotState as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.VolumeId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a snapshot.
--
-- /See:/ 'mkSnapshotInfo' smart constructor.
data SnapshotInfo = SnapshotInfo'
  { -- | Description specified by the CreateSnapshotRequest that has been applied to all snapshots.
    description :: Core.Maybe Types.Description,
    -- | Indicates whether the snapshot is encrypted.
    encrypted :: Core.Maybe Core.Bool,
    -- | Account id used when creating this snapshot.
    ownerId :: Core.Maybe Types.OwnerId,
    -- | Progress this snapshot has made towards completing.
    progress :: Core.Maybe Types.Progress,
    -- | Snapshot id that can be used to describe this snapshot.
    snapshotId :: Core.Maybe Types.SnapshotId,
    -- | Time this snapshot was started. This is the same for all snapshots initiated by the same request.
    startTime :: Core.Maybe Core.UTCTime,
    -- | Current state of the snapshot.
    state :: Core.Maybe Types.SnapshotState,
    -- | Tags associated with this snapshot.
    tags :: Core.Maybe [Types.Tag],
    -- | Source volume from which this snapshot was created.
    volumeId :: Core.Maybe Types.VolumeId,
    -- | Size of the volume from which this snapshot was created.
    volumeSize :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SnapshotInfo' value with any optional fields omitted.
mkSnapshotInfo ::
  SnapshotInfo
mkSnapshotInfo =
  SnapshotInfo'
    { description = Core.Nothing,
      encrypted = Core.Nothing,
      ownerId = Core.Nothing,
      progress = Core.Nothing,
      snapshotId = Core.Nothing,
      startTime = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      volumeId = Core.Nothing,
      volumeSize = Core.Nothing
    }

-- | Description specified by the CreateSnapshotRequest that has been applied to all snapshots.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDescription :: Lens.Lens' SnapshotInfo (Core.Maybe Types.Description)
siDescription = Lens.field @"description"
{-# DEPRECATED siDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates whether the snapshot is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siEncrypted :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Bool)
siEncrypted = Lens.field @"encrypted"
{-# DEPRECATED siEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | Account id used when creating this snapshot.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siOwnerId :: Lens.Lens' SnapshotInfo (Core.Maybe Types.OwnerId)
siOwnerId = Lens.field @"ownerId"
{-# DEPRECATED siOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | Progress this snapshot has made towards completing.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siProgress :: Lens.Lens' SnapshotInfo (Core.Maybe Types.Progress)
siProgress = Lens.field @"progress"
{-# DEPRECATED siProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | Snapshot id that can be used to describe this snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSnapshotId :: Lens.Lens' SnapshotInfo (Core.Maybe Types.SnapshotId)
siSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED siSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | Time this snapshot was started. This is the same for all snapshots initiated by the same request.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStartTime :: Lens.Lens' SnapshotInfo (Core.Maybe Core.UTCTime)
siStartTime = Lens.field @"startTime"
{-# DEPRECATED siStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Current state of the snapshot.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siState :: Lens.Lens' SnapshotInfo (Core.Maybe Types.SnapshotState)
siState = Lens.field @"state"
{-# DEPRECATED siState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Tags associated with this snapshot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siTags :: Lens.Lens' SnapshotInfo (Core.Maybe [Types.Tag])
siTags = Lens.field @"tags"
{-# DEPRECATED siTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Source volume from which this snapshot was created.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siVolumeId :: Lens.Lens' SnapshotInfo (Core.Maybe Types.VolumeId)
siVolumeId = Lens.field @"volumeId"
{-# DEPRECATED siVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | Size of the volume from which this snapshot was created.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siVolumeSize :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Int)
siVolumeSize = Lens.field @"volumeSize"
{-# DEPRECATED siVolumeSize "Use generic-lens or generic-optics with 'volumeSize' instead." #-}

instance Core.FromXML SnapshotInfo where
  parseXML x =
    SnapshotInfo'
      Core.<$> (x Core..@? "description")
      Core.<*> (x Core..@? "encrypted")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "progress")
      Core.<*> (x Core..@? "snapshotId")
      Core.<*> (x Core..@? "startTime")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "volumeId")
      Core.<*> (x Core..@? "volumeSize")
