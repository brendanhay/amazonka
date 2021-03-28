{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTaskStats
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.ReplicationTaskStats
  ( ReplicationTaskStats (..)
  -- * Smart constructor
  , mkReplicationTaskStats
  -- * Lenses
  , rtsElapsedTimeMillis
  , rtsFreshStartDate
  , rtsFullLoadFinishDate
  , rtsFullLoadProgressPercent
  , rtsFullLoadStartDate
  , rtsStartDate
  , rtsStopDate
  , rtsTablesErrored
  , rtsTablesLoaded
  , rtsTablesLoading
  , rtsTablesQueued
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | In response to a request by the @DescribeReplicationTasks@ operation, this object provides a collection of statistics about a replication task.
--
-- /See:/ 'mkReplicationTaskStats' smart constructor.
data ReplicationTaskStats = ReplicationTaskStats'
  { elapsedTimeMillis :: Core.Maybe Core.Integer
    -- ^ The elapsed time of the task, in milliseconds.
  , freshStartDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the replication task was started either with a fresh start or a target reload.
  , fullLoadFinishDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the replication task full load was completed.
  , fullLoadProgressPercent :: Core.Maybe Core.Int
    -- ^ The percent complete for the full load migration task.
  , fullLoadStartDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the replication task full load was started.
  , startDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the replication task was started either with a fresh start or a resume. For more information, see <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html#DMS-StartReplicationTask-request-StartReplicationTaskType StartReplicationTaskType> .
  , stopDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the replication task was stopped.
  , tablesErrored :: Core.Maybe Core.Int
    -- ^ The number of errors that have occurred during this task.
  , tablesLoaded :: Core.Maybe Core.Int
    -- ^ The number of tables loaded for this task.
  , tablesLoading :: Core.Maybe Core.Int
    -- ^ The number of tables currently loading for this task.
  , tablesQueued :: Core.Maybe Core.Int
    -- ^ The number of tables queued for this task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReplicationTaskStats' value with any optional fields omitted.
mkReplicationTaskStats
    :: ReplicationTaskStats
mkReplicationTaskStats
  = ReplicationTaskStats'{elapsedTimeMillis = Core.Nothing,
                          freshStartDate = Core.Nothing, fullLoadFinishDate = Core.Nothing,
                          fullLoadProgressPercent = Core.Nothing,
                          fullLoadStartDate = Core.Nothing, startDate = Core.Nothing,
                          stopDate = Core.Nothing, tablesErrored = Core.Nothing,
                          tablesLoaded = Core.Nothing, tablesLoading = Core.Nothing,
                          tablesQueued = Core.Nothing}

-- | The elapsed time of the task, in milliseconds.
--
-- /Note:/ Consider using 'elapsedTimeMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsElapsedTimeMillis :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.Integer)
rtsElapsedTimeMillis = Lens.field @"elapsedTimeMillis"
{-# INLINEABLE rtsElapsedTimeMillis #-}
{-# DEPRECATED elapsedTimeMillis "Use generic-lens or generic-optics with 'elapsedTimeMillis' instead"  #-}

-- | The date the replication task was started either with a fresh start or a target reload.
--
-- /Note:/ Consider using 'freshStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsFreshStartDate :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.NominalDiffTime)
rtsFreshStartDate = Lens.field @"freshStartDate"
{-# INLINEABLE rtsFreshStartDate #-}
{-# DEPRECATED freshStartDate "Use generic-lens or generic-optics with 'freshStartDate' instead"  #-}

-- | The date the replication task full load was completed.
--
-- /Note:/ Consider using 'fullLoadFinishDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsFullLoadFinishDate :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.NominalDiffTime)
rtsFullLoadFinishDate = Lens.field @"fullLoadFinishDate"
{-# INLINEABLE rtsFullLoadFinishDate #-}
{-# DEPRECATED fullLoadFinishDate "Use generic-lens or generic-optics with 'fullLoadFinishDate' instead"  #-}

-- | The percent complete for the full load migration task.
--
-- /Note:/ Consider using 'fullLoadProgressPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsFullLoadProgressPercent :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.Int)
rtsFullLoadProgressPercent = Lens.field @"fullLoadProgressPercent"
{-# INLINEABLE rtsFullLoadProgressPercent #-}
{-# DEPRECATED fullLoadProgressPercent "Use generic-lens or generic-optics with 'fullLoadProgressPercent' instead"  #-}

-- | The date the replication task full load was started.
--
-- /Note:/ Consider using 'fullLoadStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsFullLoadStartDate :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.NominalDiffTime)
rtsFullLoadStartDate = Lens.field @"fullLoadStartDate"
{-# INLINEABLE rtsFullLoadStartDate #-}
{-# DEPRECATED fullLoadStartDate "Use generic-lens or generic-optics with 'fullLoadStartDate' instead"  #-}

-- | The date the replication task was started either with a fresh start or a resume. For more information, see <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html#DMS-StartReplicationTask-request-StartReplicationTaskType StartReplicationTaskType> .
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsStartDate :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.NominalDiffTime)
rtsStartDate = Lens.field @"startDate"
{-# INLINEABLE rtsStartDate #-}
{-# DEPRECATED startDate "Use generic-lens or generic-optics with 'startDate' instead"  #-}

-- | The date the replication task was stopped.
--
-- /Note:/ Consider using 'stopDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsStopDate :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.NominalDiffTime)
rtsStopDate = Lens.field @"stopDate"
{-# INLINEABLE rtsStopDate #-}
{-# DEPRECATED stopDate "Use generic-lens or generic-optics with 'stopDate' instead"  #-}

-- | The number of errors that have occurred during this task.
--
-- /Note:/ Consider using 'tablesErrored' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsTablesErrored :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.Int)
rtsTablesErrored = Lens.field @"tablesErrored"
{-# INLINEABLE rtsTablesErrored #-}
{-# DEPRECATED tablesErrored "Use generic-lens or generic-optics with 'tablesErrored' instead"  #-}

-- | The number of tables loaded for this task.
--
-- /Note:/ Consider using 'tablesLoaded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsTablesLoaded :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.Int)
rtsTablesLoaded = Lens.field @"tablesLoaded"
{-# INLINEABLE rtsTablesLoaded #-}
{-# DEPRECATED tablesLoaded "Use generic-lens or generic-optics with 'tablesLoaded' instead"  #-}

-- | The number of tables currently loading for this task.
--
-- /Note:/ Consider using 'tablesLoading' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsTablesLoading :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.Int)
rtsTablesLoading = Lens.field @"tablesLoading"
{-# INLINEABLE rtsTablesLoading #-}
{-# DEPRECATED tablesLoading "Use generic-lens or generic-optics with 'tablesLoading' instead"  #-}

-- | The number of tables queued for this task.
--
-- /Note:/ Consider using 'tablesQueued' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsTablesQueued :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.Int)
rtsTablesQueued = Lens.field @"tablesQueued"
{-# INLINEABLE rtsTablesQueued #-}
{-# DEPRECATED tablesQueued "Use generic-lens or generic-optics with 'tablesQueued' instead"  #-}

instance Core.FromJSON ReplicationTaskStats where
        parseJSON
          = Core.withObject "ReplicationTaskStats" Core.$
              \ x ->
                ReplicationTaskStats' Core.<$>
                  (x Core..:? "ElapsedTimeMillis") Core.<*>
                    x Core..:? "FreshStartDate"
                    Core.<*> x Core..:? "FullLoadFinishDate"
                    Core.<*> x Core..:? "FullLoadProgressPercent"
                    Core.<*> x Core..:? "FullLoadStartDate"
                    Core.<*> x Core..:? "StartDate"
                    Core.<*> x Core..:? "StopDate"
                    Core.<*> x Core..:? "TablesErrored"
                    Core.<*> x Core..:? "TablesLoaded"
                    Core.<*> x Core..:? "TablesLoading"
                    Core.<*> x Core..:? "TablesQueued"
