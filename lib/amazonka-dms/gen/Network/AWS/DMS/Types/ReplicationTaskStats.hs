{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTaskStats
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationTaskStats
  ( ReplicationTaskStats (..),

    -- * Smart constructor
    mkReplicationTaskStats,

    -- * Lenses
    rtsStopDate,
    rtsFullLoadProgressPercent,
    rtsFullLoadStartDate,
    rtsElapsedTimeMillis,
    rtsStartDate,
    rtsTablesErrored,
    rtsFullLoadFinishDate,
    rtsTablesLoaded,
    rtsTablesQueued,
    rtsTablesLoading,
    rtsFreshStartDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | In response to a request by the @DescribeReplicationTasks@ operation, this object provides a collection of statistics about a replication task.
--
-- /See:/ 'mkReplicationTaskStats' smart constructor.
data ReplicationTaskStats = ReplicationTaskStats'
  { -- | The date the replication task was stopped.
    stopDate :: Lude.Maybe Lude.Timestamp,
    -- | The percent complete for the full load migration task.
    fullLoadProgressPercent :: Lude.Maybe Lude.Int,
    -- | The date the replication task full load was started.
    fullLoadStartDate :: Lude.Maybe Lude.Timestamp,
    -- | The elapsed time of the task, in milliseconds.
    elapsedTimeMillis :: Lude.Maybe Lude.Integer,
    -- | The date the replication task was started either with a fresh start or a resume. For more information, see <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html#DMS-StartReplicationTask-request-StartReplicationTaskType StartReplicationTaskType> .
    startDate :: Lude.Maybe Lude.Timestamp,
    -- | The number of errors that have occurred during this task.
    tablesErrored :: Lude.Maybe Lude.Int,
    -- | The date the replication task full load was completed.
    fullLoadFinishDate :: Lude.Maybe Lude.Timestamp,
    -- | The number of tables loaded for this task.
    tablesLoaded :: Lude.Maybe Lude.Int,
    -- | The number of tables queued for this task.
    tablesQueued :: Lude.Maybe Lude.Int,
    -- | The number of tables currently loading for this task.
    tablesLoading :: Lude.Maybe Lude.Int,
    -- | The date the replication task was started either with a fresh start or a target reload.
    freshStartDate :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationTaskStats' with the minimum fields required to make a request.
--
-- * 'stopDate' - The date the replication task was stopped.
-- * 'fullLoadProgressPercent' - The percent complete for the full load migration task.
-- * 'fullLoadStartDate' - The date the replication task full load was started.
-- * 'elapsedTimeMillis' - The elapsed time of the task, in milliseconds.
-- * 'startDate' - The date the replication task was started either with a fresh start or a resume. For more information, see <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html#DMS-StartReplicationTask-request-StartReplicationTaskType StartReplicationTaskType> .
-- * 'tablesErrored' - The number of errors that have occurred during this task.
-- * 'fullLoadFinishDate' - The date the replication task full load was completed.
-- * 'tablesLoaded' - The number of tables loaded for this task.
-- * 'tablesQueued' - The number of tables queued for this task.
-- * 'tablesLoading' - The number of tables currently loading for this task.
-- * 'freshStartDate' - The date the replication task was started either with a fresh start or a target reload.
mkReplicationTaskStats ::
  ReplicationTaskStats
mkReplicationTaskStats =
  ReplicationTaskStats'
    { stopDate = Lude.Nothing,
      fullLoadProgressPercent = Lude.Nothing,
      fullLoadStartDate = Lude.Nothing,
      elapsedTimeMillis = Lude.Nothing,
      startDate = Lude.Nothing,
      tablesErrored = Lude.Nothing,
      fullLoadFinishDate = Lude.Nothing,
      tablesLoaded = Lude.Nothing,
      tablesQueued = Lude.Nothing,
      tablesLoading = Lude.Nothing,
      freshStartDate = Lude.Nothing
    }

-- | The date the replication task was stopped.
--
-- /Note:/ Consider using 'stopDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsStopDate :: Lens.Lens' ReplicationTaskStats (Lude.Maybe Lude.Timestamp)
rtsStopDate = Lens.lens (stopDate :: ReplicationTaskStats -> Lude.Maybe Lude.Timestamp) (\s a -> s {stopDate = a} :: ReplicationTaskStats)
{-# DEPRECATED rtsStopDate "Use generic-lens or generic-optics with 'stopDate' instead." #-}

-- | The percent complete for the full load migration task.
--
-- /Note:/ Consider using 'fullLoadProgressPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsFullLoadProgressPercent :: Lens.Lens' ReplicationTaskStats (Lude.Maybe Lude.Int)
rtsFullLoadProgressPercent = Lens.lens (fullLoadProgressPercent :: ReplicationTaskStats -> Lude.Maybe Lude.Int) (\s a -> s {fullLoadProgressPercent = a} :: ReplicationTaskStats)
{-# DEPRECATED rtsFullLoadProgressPercent "Use generic-lens or generic-optics with 'fullLoadProgressPercent' instead." #-}

-- | The date the replication task full load was started.
--
-- /Note:/ Consider using 'fullLoadStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsFullLoadStartDate :: Lens.Lens' ReplicationTaskStats (Lude.Maybe Lude.Timestamp)
rtsFullLoadStartDate = Lens.lens (fullLoadStartDate :: ReplicationTaskStats -> Lude.Maybe Lude.Timestamp) (\s a -> s {fullLoadStartDate = a} :: ReplicationTaskStats)
{-# DEPRECATED rtsFullLoadStartDate "Use generic-lens or generic-optics with 'fullLoadStartDate' instead." #-}

-- | The elapsed time of the task, in milliseconds.
--
-- /Note:/ Consider using 'elapsedTimeMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsElapsedTimeMillis :: Lens.Lens' ReplicationTaskStats (Lude.Maybe Lude.Integer)
rtsElapsedTimeMillis = Lens.lens (elapsedTimeMillis :: ReplicationTaskStats -> Lude.Maybe Lude.Integer) (\s a -> s {elapsedTimeMillis = a} :: ReplicationTaskStats)
{-# DEPRECATED rtsElapsedTimeMillis "Use generic-lens or generic-optics with 'elapsedTimeMillis' instead." #-}

-- | The date the replication task was started either with a fresh start or a resume. For more information, see <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html#DMS-StartReplicationTask-request-StartReplicationTaskType StartReplicationTaskType> .
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsStartDate :: Lens.Lens' ReplicationTaskStats (Lude.Maybe Lude.Timestamp)
rtsStartDate = Lens.lens (startDate :: ReplicationTaskStats -> Lude.Maybe Lude.Timestamp) (\s a -> s {startDate = a} :: ReplicationTaskStats)
{-# DEPRECATED rtsStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | The number of errors that have occurred during this task.
--
-- /Note:/ Consider using 'tablesErrored' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsTablesErrored :: Lens.Lens' ReplicationTaskStats (Lude.Maybe Lude.Int)
rtsTablesErrored = Lens.lens (tablesErrored :: ReplicationTaskStats -> Lude.Maybe Lude.Int) (\s a -> s {tablesErrored = a} :: ReplicationTaskStats)
{-# DEPRECATED rtsTablesErrored "Use generic-lens or generic-optics with 'tablesErrored' instead." #-}

-- | The date the replication task full load was completed.
--
-- /Note:/ Consider using 'fullLoadFinishDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsFullLoadFinishDate :: Lens.Lens' ReplicationTaskStats (Lude.Maybe Lude.Timestamp)
rtsFullLoadFinishDate = Lens.lens (fullLoadFinishDate :: ReplicationTaskStats -> Lude.Maybe Lude.Timestamp) (\s a -> s {fullLoadFinishDate = a} :: ReplicationTaskStats)
{-# DEPRECATED rtsFullLoadFinishDate "Use generic-lens or generic-optics with 'fullLoadFinishDate' instead." #-}

-- | The number of tables loaded for this task.
--
-- /Note:/ Consider using 'tablesLoaded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsTablesLoaded :: Lens.Lens' ReplicationTaskStats (Lude.Maybe Lude.Int)
rtsTablesLoaded = Lens.lens (tablesLoaded :: ReplicationTaskStats -> Lude.Maybe Lude.Int) (\s a -> s {tablesLoaded = a} :: ReplicationTaskStats)
{-# DEPRECATED rtsTablesLoaded "Use generic-lens or generic-optics with 'tablesLoaded' instead." #-}

-- | The number of tables queued for this task.
--
-- /Note:/ Consider using 'tablesQueued' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsTablesQueued :: Lens.Lens' ReplicationTaskStats (Lude.Maybe Lude.Int)
rtsTablesQueued = Lens.lens (tablesQueued :: ReplicationTaskStats -> Lude.Maybe Lude.Int) (\s a -> s {tablesQueued = a} :: ReplicationTaskStats)
{-# DEPRECATED rtsTablesQueued "Use generic-lens or generic-optics with 'tablesQueued' instead." #-}

-- | The number of tables currently loading for this task.
--
-- /Note:/ Consider using 'tablesLoading' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsTablesLoading :: Lens.Lens' ReplicationTaskStats (Lude.Maybe Lude.Int)
rtsTablesLoading = Lens.lens (tablesLoading :: ReplicationTaskStats -> Lude.Maybe Lude.Int) (\s a -> s {tablesLoading = a} :: ReplicationTaskStats)
{-# DEPRECATED rtsTablesLoading "Use generic-lens or generic-optics with 'tablesLoading' instead." #-}

-- | The date the replication task was started either with a fresh start or a target reload.
--
-- /Note:/ Consider using 'freshStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsFreshStartDate :: Lens.Lens' ReplicationTaskStats (Lude.Maybe Lude.Timestamp)
rtsFreshStartDate = Lens.lens (freshStartDate :: ReplicationTaskStats -> Lude.Maybe Lude.Timestamp) (\s a -> s {freshStartDate = a} :: ReplicationTaskStats)
{-# DEPRECATED rtsFreshStartDate "Use generic-lens or generic-optics with 'freshStartDate' instead." #-}

instance Lude.FromJSON ReplicationTaskStats where
  parseJSON =
    Lude.withObject
      "ReplicationTaskStats"
      ( \x ->
          ReplicationTaskStats'
            Lude.<$> (x Lude..:? "StopDate")
            Lude.<*> (x Lude..:? "FullLoadProgressPercent")
            Lude.<*> (x Lude..:? "FullLoadStartDate")
            Lude.<*> (x Lude..:? "ElapsedTimeMillis")
            Lude.<*> (x Lude..:? "StartDate")
            Lude.<*> (x Lude..:? "TablesErrored")
            Lude.<*> (x Lude..:? "FullLoadFinishDate")
            Lude.<*> (x Lude..:? "TablesLoaded")
            Lude.<*> (x Lude..:? "TablesQueued")
            Lude.<*> (x Lude..:? "TablesLoading")
            Lude.<*> (x Lude..:? "FreshStartDate")
      )
