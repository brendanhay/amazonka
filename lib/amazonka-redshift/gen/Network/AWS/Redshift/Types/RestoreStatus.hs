{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.RestoreStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.RestoreStatus
  ( RestoreStatus (..),

    -- * Smart constructor
    mkRestoreStatus,

    -- * Lenses
    rsStatus,
    rsEstimatedTimeToCompletionInSeconds,
    rsCurrentRestoreRateInMegaBytesPerSecond,
    rsProgressInMegaBytes,
    rsElapsedTimeInSeconds,
    rsSnapshotSizeInMegaBytes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes the status of a cluster restore action. Returns null if the cluster was not created by restoring a snapshot.
--
-- /See:/ 'mkRestoreStatus' smart constructor.
data RestoreStatus = RestoreStatus'
  { status :: Lude.Maybe Lude.Text,
    estimatedTimeToCompletionInSeconds :: Lude.Maybe Lude.Integer,
    currentRestoreRateInMegaBytesPerSecond ::
      Lude.Maybe Lude.Double,
    progressInMegaBytes :: Lude.Maybe Lude.Integer,
    elapsedTimeInSeconds :: Lude.Maybe Lude.Integer,
    snapshotSizeInMegaBytes :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreStatus' with the minimum fields required to make a request.
--
-- * 'currentRestoreRateInMegaBytesPerSecond' - The number of megabytes per second being transferred from the backup storage. Returns the average rate for a completed backup. This field is only updated when you restore to DC2 and DS2 node types.
-- * 'elapsedTimeInSeconds' - The amount of time an in-progress restore has been running, or the amount of time it took a completed restore to finish. This field is only updated when you restore to DC2 and DS2 node types.
-- * 'estimatedTimeToCompletionInSeconds' - The estimate of the time remaining before the restore will complete. Returns 0 for a completed restore. This field is only updated when you restore to DC2 and DS2 node types.
-- * 'progressInMegaBytes' - The number of megabytes that have been transferred from snapshot storage. This field is only updated when you restore to DC2 and DS2 node types.
-- * 'snapshotSizeInMegaBytes' - The size of the set of snapshot data used to restore the cluster. This field is only updated when you restore to DC2 and DS2 node types.
-- * 'status' - The status of the restore action. Returns starting, restoring, completed, or failed.
mkRestoreStatus ::
  RestoreStatus
mkRestoreStatus =
  RestoreStatus'
    { status = Lude.Nothing,
      estimatedTimeToCompletionInSeconds = Lude.Nothing,
      currentRestoreRateInMegaBytesPerSecond = Lude.Nothing,
      progressInMegaBytes = Lude.Nothing,
      elapsedTimeInSeconds = Lude.Nothing,
      snapshotSizeInMegaBytes = Lude.Nothing
    }

-- | The status of the restore action. Returns starting, restoring, completed, or failed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsStatus :: Lens.Lens' RestoreStatus (Lude.Maybe Lude.Text)
rsStatus = Lens.lens (status :: RestoreStatus -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: RestoreStatus)
{-# DEPRECATED rsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The estimate of the time remaining before the restore will complete. Returns 0 for a completed restore. This field is only updated when you restore to DC2 and DS2 node types.
--
-- /Note:/ Consider using 'estimatedTimeToCompletionInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsEstimatedTimeToCompletionInSeconds :: Lens.Lens' RestoreStatus (Lude.Maybe Lude.Integer)
rsEstimatedTimeToCompletionInSeconds = Lens.lens (estimatedTimeToCompletionInSeconds :: RestoreStatus -> Lude.Maybe Lude.Integer) (\s a -> s {estimatedTimeToCompletionInSeconds = a} :: RestoreStatus)
{-# DEPRECATED rsEstimatedTimeToCompletionInSeconds "Use generic-lens or generic-optics with 'estimatedTimeToCompletionInSeconds' instead." #-}

-- | The number of megabytes per second being transferred from the backup storage. Returns the average rate for a completed backup. This field is only updated when you restore to DC2 and DS2 node types.
--
-- /Note:/ Consider using 'currentRestoreRateInMegaBytesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsCurrentRestoreRateInMegaBytesPerSecond :: Lens.Lens' RestoreStatus (Lude.Maybe Lude.Double)
rsCurrentRestoreRateInMegaBytesPerSecond = Lens.lens (currentRestoreRateInMegaBytesPerSecond :: RestoreStatus -> Lude.Maybe Lude.Double) (\s a -> s {currentRestoreRateInMegaBytesPerSecond = a} :: RestoreStatus)
{-# DEPRECATED rsCurrentRestoreRateInMegaBytesPerSecond "Use generic-lens or generic-optics with 'currentRestoreRateInMegaBytesPerSecond' instead." #-}

-- | The number of megabytes that have been transferred from snapshot storage. This field is only updated when you restore to DC2 and DS2 node types.
--
-- /Note:/ Consider using 'progressInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsProgressInMegaBytes :: Lens.Lens' RestoreStatus (Lude.Maybe Lude.Integer)
rsProgressInMegaBytes = Lens.lens (progressInMegaBytes :: RestoreStatus -> Lude.Maybe Lude.Integer) (\s a -> s {progressInMegaBytes = a} :: RestoreStatus)
{-# DEPRECATED rsProgressInMegaBytes "Use generic-lens or generic-optics with 'progressInMegaBytes' instead." #-}

-- | The amount of time an in-progress restore has been running, or the amount of time it took a completed restore to finish. This field is only updated when you restore to DC2 and DS2 node types.
--
-- /Note:/ Consider using 'elapsedTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsElapsedTimeInSeconds :: Lens.Lens' RestoreStatus (Lude.Maybe Lude.Integer)
rsElapsedTimeInSeconds = Lens.lens (elapsedTimeInSeconds :: RestoreStatus -> Lude.Maybe Lude.Integer) (\s a -> s {elapsedTimeInSeconds = a} :: RestoreStatus)
{-# DEPRECATED rsElapsedTimeInSeconds "Use generic-lens or generic-optics with 'elapsedTimeInSeconds' instead." #-}

-- | The size of the set of snapshot data used to restore the cluster. This field is only updated when you restore to DC2 and DS2 node types.
--
-- /Note:/ Consider using 'snapshotSizeInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSnapshotSizeInMegaBytes :: Lens.Lens' RestoreStatus (Lude.Maybe Lude.Integer)
rsSnapshotSizeInMegaBytes = Lens.lens (snapshotSizeInMegaBytes :: RestoreStatus -> Lude.Maybe Lude.Integer) (\s a -> s {snapshotSizeInMegaBytes = a} :: RestoreStatus)
{-# DEPRECATED rsSnapshotSizeInMegaBytes "Use generic-lens or generic-optics with 'snapshotSizeInMegaBytes' instead." #-}

instance Lude.FromXML RestoreStatus where
  parseXML x =
    RestoreStatus'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "EstimatedTimeToCompletionInSeconds")
      Lude.<*> (x Lude..@? "CurrentRestoreRateInMegaBytesPerSecond")
      Lude.<*> (x Lude..@? "ProgressInMegaBytes")
      Lude.<*> (x Lude..@? "ElapsedTimeInSeconds")
      Lude.<*> (x Lude..@? "SnapshotSizeInMegaBytes")
