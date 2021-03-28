{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SnapshotInfo
  ( SnapshotInfo (..)
  -- * Smart constructor
  , mkSnapshotInfo
  -- * Lenses
  , siDescription
  , siEncrypted
  , siOwnerId
  , siProgress
  , siSnapshotId
  , siStartTime
  , siState
  , siTags
  , siVolumeId
  , siVolumeSize
  ) where

import qualified Network.AWS.EC2.Types.SnapshotState as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a snapshot.
--
-- /See:/ 'mkSnapshotInfo' smart constructor.
data SnapshotInfo = SnapshotInfo'
  { description :: Core.Maybe Core.Text
    -- ^ Description specified by the CreateSnapshotRequest that has been applied to all snapshots.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Indicates whether the snapshot is encrypted.
  , ownerId :: Core.Maybe Core.Text
    -- ^ Account id used when creating this snapshot.
  , progress :: Core.Maybe Core.Text
    -- ^ Progress this snapshot has made towards completing.
  , snapshotId :: Core.Maybe Core.Text
    -- ^ Snapshot id that can be used to describe this snapshot.
  , startTime :: Core.Maybe Core.UTCTime
    -- ^ Time this snapshot was started. This is the same for all snapshots initiated by the same request.
  , state :: Core.Maybe Types.SnapshotState
    -- ^ Current state of the snapshot.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Tags associated with this snapshot.
  , volumeId :: Core.Maybe Core.Text
    -- ^ Source volume from which this snapshot was created.
  , volumeSize :: Core.Maybe Core.Int
    -- ^ Size of the volume from which this snapshot was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SnapshotInfo' value with any optional fields omitted.
mkSnapshotInfo
    :: SnapshotInfo
mkSnapshotInfo
  = SnapshotInfo'{description = Core.Nothing,
                  encrypted = Core.Nothing, ownerId = Core.Nothing,
                  progress = Core.Nothing, snapshotId = Core.Nothing,
                  startTime = Core.Nothing, state = Core.Nothing,
                  tags = Core.Nothing, volumeId = Core.Nothing,
                  volumeSize = Core.Nothing}

-- | Description specified by the CreateSnapshotRequest that has been applied to all snapshots.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDescription :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Text)
siDescription = Lens.field @"description"
{-# INLINEABLE siDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Indicates whether the snapshot is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siEncrypted :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Bool)
siEncrypted = Lens.field @"encrypted"
{-# INLINEABLE siEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | Account id used when creating this snapshot.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siOwnerId :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Text)
siOwnerId = Lens.field @"ownerId"
{-# INLINEABLE siOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | Progress this snapshot has made towards completing.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siProgress :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Text)
siProgress = Lens.field @"progress"
{-# INLINEABLE siProgress #-}
{-# DEPRECATED progress "Use generic-lens or generic-optics with 'progress' instead"  #-}

-- | Snapshot id that can be used to describe this snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSnapshotId :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Text)
siSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE siSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | Time this snapshot was started. This is the same for all snapshots initiated by the same request.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStartTime :: Lens.Lens' SnapshotInfo (Core.Maybe Core.UTCTime)
siStartTime = Lens.field @"startTime"
{-# INLINEABLE siStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | Current state of the snapshot.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siState :: Lens.Lens' SnapshotInfo (Core.Maybe Types.SnapshotState)
siState = Lens.field @"state"
{-# INLINEABLE siState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | Tags associated with this snapshot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siTags :: Lens.Lens' SnapshotInfo (Core.Maybe [Types.Tag])
siTags = Lens.field @"tags"
{-# INLINEABLE siTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Source volume from which this snapshot was created.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siVolumeId :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Text)
siVolumeId = Lens.field @"volumeId"
{-# INLINEABLE siVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

-- | Size of the volume from which this snapshot was created.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siVolumeSize :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Int)
siVolumeSize = Lens.field @"volumeSize"
{-# INLINEABLE siVolumeSize #-}
{-# DEPRECATED volumeSize "Use generic-lens or generic-optics with 'volumeSize' instead"  #-}

instance Core.FromXML SnapshotInfo where
        parseXML x
          = SnapshotInfo' Core.<$>
              (x Core..@? "description") Core.<*> x Core..@? "encrypted" Core.<*>
                x Core..@? "ownerId"
                Core.<*> x Core..@? "progress"
                Core.<*> x Core..@? "snapshotId"
                Core.<*> x Core..@? "startTime"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "volumeId"
                Core.<*> x Core..@? "volumeSize"
