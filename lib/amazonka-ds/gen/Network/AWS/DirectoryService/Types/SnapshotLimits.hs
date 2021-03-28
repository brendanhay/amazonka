{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SnapshotLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.SnapshotLimits
  ( SnapshotLimits (..)
  -- * Smart constructor
  , mkSnapshotLimits
  -- * Lenses
  , slManualSnapshotsCurrentCount
  , slManualSnapshotsLimit
  , slManualSnapshotsLimitReached
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains manual snapshot limit information for a directory.
--
-- /See:/ 'mkSnapshotLimits' smart constructor.
data SnapshotLimits = SnapshotLimits'
  { manualSnapshotsCurrentCount :: Core.Maybe Core.Natural
    -- ^ The current number of manual snapshots of the directory.
  , manualSnapshotsLimit :: Core.Maybe Core.Natural
    -- ^ The maximum number of manual snapshots allowed.
  , manualSnapshotsLimitReached :: Core.Maybe Core.Bool
    -- ^ Indicates if the manual snapshot limit has been reached.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SnapshotLimits' value with any optional fields omitted.
mkSnapshotLimits
    :: SnapshotLimits
mkSnapshotLimits
  = SnapshotLimits'{manualSnapshotsCurrentCount = Core.Nothing,
                    manualSnapshotsLimit = Core.Nothing,
                    manualSnapshotsLimitReached = Core.Nothing}

-- | The current number of manual snapshots of the directory.
--
-- /Note:/ Consider using 'manualSnapshotsCurrentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slManualSnapshotsCurrentCount :: Lens.Lens' SnapshotLimits (Core.Maybe Core.Natural)
slManualSnapshotsCurrentCount = Lens.field @"manualSnapshotsCurrentCount"
{-# INLINEABLE slManualSnapshotsCurrentCount #-}
{-# DEPRECATED manualSnapshotsCurrentCount "Use generic-lens or generic-optics with 'manualSnapshotsCurrentCount' instead"  #-}

-- | The maximum number of manual snapshots allowed.
--
-- /Note:/ Consider using 'manualSnapshotsLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slManualSnapshotsLimit :: Lens.Lens' SnapshotLimits (Core.Maybe Core.Natural)
slManualSnapshotsLimit = Lens.field @"manualSnapshotsLimit"
{-# INLINEABLE slManualSnapshotsLimit #-}
{-# DEPRECATED manualSnapshotsLimit "Use generic-lens or generic-optics with 'manualSnapshotsLimit' instead"  #-}

-- | Indicates if the manual snapshot limit has been reached.
--
-- /Note:/ Consider using 'manualSnapshotsLimitReached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slManualSnapshotsLimitReached :: Lens.Lens' SnapshotLimits (Core.Maybe Core.Bool)
slManualSnapshotsLimitReached = Lens.field @"manualSnapshotsLimitReached"
{-# INLINEABLE slManualSnapshotsLimitReached #-}
{-# DEPRECATED manualSnapshotsLimitReached "Use generic-lens or generic-optics with 'manualSnapshotsLimitReached' instead"  #-}

instance Core.FromJSON SnapshotLimits where
        parseJSON
          = Core.withObject "SnapshotLimits" Core.$
              \ x ->
                SnapshotLimits' Core.<$>
                  (x Core..:? "ManualSnapshotsCurrentCount") Core.<*>
                    x Core..:? "ManualSnapshotsLimit"
                    Core.<*> x Core..:? "ManualSnapshotsLimitReached"
