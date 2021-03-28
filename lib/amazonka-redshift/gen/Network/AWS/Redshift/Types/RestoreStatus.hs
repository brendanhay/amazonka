{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.RestoreStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.RestoreStatus
  ( RestoreStatus (..)
  -- * Smart constructor
  , mkRestoreStatus
  -- * Lenses
  , rsCurrentRestoreRateInMegaBytesPerSecond
  , rsElapsedTimeInSeconds
  , rsEstimatedTimeToCompletionInSeconds
  , rsProgressInMegaBytes
  , rsSnapshotSizeInMegaBytes
  , rsStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

-- | Describes the status of a cluster restore action. Returns null if the cluster was not created by restoring a snapshot.
--
-- /See:/ 'mkRestoreStatus' smart constructor.
data RestoreStatus = RestoreStatus'
  { currentRestoreRateInMegaBytesPerSecond :: Core.Maybe Core.Double
    -- ^ The number of megabytes per second being transferred from the backup storage. Returns the average rate for a completed backup. This field is only updated when you restore to DC2 and DS2 node types. 
  , elapsedTimeInSeconds :: Core.Maybe Core.Integer
    -- ^ The amount of time an in-progress restore has been running, or the amount of time it took a completed restore to finish. This field is only updated when you restore to DC2 and DS2 node types. 
  , estimatedTimeToCompletionInSeconds :: Core.Maybe Core.Integer
    -- ^ The estimate of the time remaining before the restore will complete. Returns 0 for a completed restore. This field is only updated when you restore to DC2 and DS2 node types. 
  , progressInMegaBytes :: Core.Maybe Core.Integer
    -- ^ The number of megabytes that have been transferred from snapshot storage. This field is only updated when you restore to DC2 and DS2 node types. 
  , snapshotSizeInMegaBytes :: Core.Maybe Core.Integer
    -- ^ The size of the set of snapshot data used to restore the cluster. This field is only updated when you restore to DC2 and DS2 node types. 
  , status :: Core.Maybe Core.Text
    -- ^ The status of the restore action. Returns starting, restoring, completed, or failed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreStatus' value with any optional fields omitted.
mkRestoreStatus
    :: RestoreStatus
mkRestoreStatus
  = RestoreStatus'{currentRestoreRateInMegaBytesPerSecond =
                     Core.Nothing,
                   elapsedTimeInSeconds = Core.Nothing,
                   estimatedTimeToCompletionInSeconds = Core.Nothing,
                   progressInMegaBytes = Core.Nothing,
                   snapshotSizeInMegaBytes = Core.Nothing, status = Core.Nothing}

-- | The number of megabytes per second being transferred from the backup storage. Returns the average rate for a completed backup. This field is only updated when you restore to DC2 and DS2 node types. 
--
-- /Note:/ Consider using 'currentRestoreRateInMegaBytesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsCurrentRestoreRateInMegaBytesPerSecond :: Lens.Lens' RestoreStatus (Core.Maybe Core.Double)
rsCurrentRestoreRateInMegaBytesPerSecond = Lens.field @"currentRestoreRateInMegaBytesPerSecond"
{-# INLINEABLE rsCurrentRestoreRateInMegaBytesPerSecond #-}
{-# DEPRECATED currentRestoreRateInMegaBytesPerSecond "Use generic-lens or generic-optics with 'currentRestoreRateInMegaBytesPerSecond' instead"  #-}

-- | The amount of time an in-progress restore has been running, or the amount of time it took a completed restore to finish. This field is only updated when you restore to DC2 and DS2 node types. 
--
-- /Note:/ Consider using 'elapsedTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsElapsedTimeInSeconds :: Lens.Lens' RestoreStatus (Core.Maybe Core.Integer)
rsElapsedTimeInSeconds = Lens.field @"elapsedTimeInSeconds"
{-# INLINEABLE rsElapsedTimeInSeconds #-}
{-# DEPRECATED elapsedTimeInSeconds "Use generic-lens or generic-optics with 'elapsedTimeInSeconds' instead"  #-}

-- | The estimate of the time remaining before the restore will complete. Returns 0 for a completed restore. This field is only updated when you restore to DC2 and DS2 node types. 
--
-- /Note:/ Consider using 'estimatedTimeToCompletionInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsEstimatedTimeToCompletionInSeconds :: Lens.Lens' RestoreStatus (Core.Maybe Core.Integer)
rsEstimatedTimeToCompletionInSeconds = Lens.field @"estimatedTimeToCompletionInSeconds"
{-# INLINEABLE rsEstimatedTimeToCompletionInSeconds #-}
{-# DEPRECATED estimatedTimeToCompletionInSeconds "Use generic-lens or generic-optics with 'estimatedTimeToCompletionInSeconds' instead"  #-}

-- | The number of megabytes that have been transferred from snapshot storage. This field is only updated when you restore to DC2 and DS2 node types. 
--
-- /Note:/ Consider using 'progressInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsProgressInMegaBytes :: Lens.Lens' RestoreStatus (Core.Maybe Core.Integer)
rsProgressInMegaBytes = Lens.field @"progressInMegaBytes"
{-# INLINEABLE rsProgressInMegaBytes #-}
{-# DEPRECATED progressInMegaBytes "Use generic-lens or generic-optics with 'progressInMegaBytes' instead"  #-}

-- | The size of the set of snapshot data used to restore the cluster. This field is only updated when you restore to DC2 and DS2 node types. 
--
-- /Note:/ Consider using 'snapshotSizeInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSnapshotSizeInMegaBytes :: Lens.Lens' RestoreStatus (Core.Maybe Core.Integer)
rsSnapshotSizeInMegaBytes = Lens.field @"snapshotSizeInMegaBytes"
{-# INLINEABLE rsSnapshotSizeInMegaBytes #-}
{-# DEPRECATED snapshotSizeInMegaBytes "Use generic-lens or generic-optics with 'snapshotSizeInMegaBytes' instead"  #-}

-- | The status of the restore action. Returns starting, restoring, completed, or failed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsStatus :: Lens.Lens' RestoreStatus (Core.Maybe Core.Text)
rsStatus = Lens.field @"status"
{-# INLINEABLE rsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML RestoreStatus where
        parseXML x
          = RestoreStatus' Core.<$>
              (x Core..@? "CurrentRestoreRateInMegaBytesPerSecond") Core.<*>
                x Core..@? "ElapsedTimeInSeconds"
                Core.<*> x Core..@? "EstimatedTimeToCompletionInSeconds"
                Core.<*> x Core..@? "ProgressInMegaBytes"
                Core.<*> x Core..@? "SnapshotSizeInMegaBytes"
                Core.<*> x Core..@? "Status"
