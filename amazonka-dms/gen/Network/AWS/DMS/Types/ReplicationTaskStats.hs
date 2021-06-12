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
-- Module      : Network.AWS.DMS.Types.ReplicationTaskStats
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationTaskStats where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | In response to a request by the @DescribeReplicationTasks@ operation,
-- this object provides a collection of statistics about a replication
-- task.
--
-- /See:/ 'newReplicationTaskStats' smart constructor.
data ReplicationTaskStats = ReplicationTaskStats'
  { -- | The date the replication task was stopped.
    stopDate :: Core.Maybe Core.POSIX,
    -- | The number of errors that have occurred during this task.
    tablesErrored :: Core.Maybe Core.Int,
    -- | The date the replication task was started either with a fresh start or a
    -- resume. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html#DMS-StartReplicationTask-request-StartReplicationTaskType StartReplicationTaskType>.
    startDate :: Core.Maybe Core.POSIX,
    -- | The date the replication task was started either with a fresh start or a
    -- target reload.
    freshStartDate :: Core.Maybe Core.POSIX,
    -- | The number of tables currently loading for this task.
    tablesLoading :: Core.Maybe Core.Int,
    -- | The date the replication task full load was started.
    fullLoadStartDate :: Core.Maybe Core.POSIX,
    -- | The elapsed time of the task, in milliseconds.
    elapsedTimeMillis :: Core.Maybe Core.Integer,
    -- | The percent complete for the full load migration task.
    fullLoadProgressPercent :: Core.Maybe Core.Int,
    -- | The number of tables queued for this task.
    tablesQueued :: Core.Maybe Core.Int,
    -- | The date the replication task full load was completed.
    fullLoadFinishDate :: Core.Maybe Core.POSIX,
    -- | The number of tables loaded for this task.
    tablesLoaded :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicationTaskStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stopDate', 'replicationTaskStats_stopDate' - The date the replication task was stopped.
--
-- 'tablesErrored', 'replicationTaskStats_tablesErrored' - The number of errors that have occurred during this task.
--
-- 'startDate', 'replicationTaskStats_startDate' - The date the replication task was started either with a fresh start or a
-- resume. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html#DMS-StartReplicationTask-request-StartReplicationTaskType StartReplicationTaskType>.
--
-- 'freshStartDate', 'replicationTaskStats_freshStartDate' - The date the replication task was started either with a fresh start or a
-- target reload.
--
-- 'tablesLoading', 'replicationTaskStats_tablesLoading' - The number of tables currently loading for this task.
--
-- 'fullLoadStartDate', 'replicationTaskStats_fullLoadStartDate' - The date the replication task full load was started.
--
-- 'elapsedTimeMillis', 'replicationTaskStats_elapsedTimeMillis' - The elapsed time of the task, in milliseconds.
--
-- 'fullLoadProgressPercent', 'replicationTaskStats_fullLoadProgressPercent' - The percent complete for the full load migration task.
--
-- 'tablesQueued', 'replicationTaskStats_tablesQueued' - The number of tables queued for this task.
--
-- 'fullLoadFinishDate', 'replicationTaskStats_fullLoadFinishDate' - The date the replication task full load was completed.
--
-- 'tablesLoaded', 'replicationTaskStats_tablesLoaded' - The number of tables loaded for this task.
newReplicationTaskStats ::
  ReplicationTaskStats
newReplicationTaskStats =
  ReplicationTaskStats'
    { stopDate = Core.Nothing,
      tablesErrored = Core.Nothing,
      startDate = Core.Nothing,
      freshStartDate = Core.Nothing,
      tablesLoading = Core.Nothing,
      fullLoadStartDate = Core.Nothing,
      elapsedTimeMillis = Core.Nothing,
      fullLoadProgressPercent = Core.Nothing,
      tablesQueued = Core.Nothing,
      fullLoadFinishDate = Core.Nothing,
      tablesLoaded = Core.Nothing
    }

-- | The date the replication task was stopped.
replicationTaskStats_stopDate :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.UTCTime)
replicationTaskStats_stopDate = Lens.lens (\ReplicationTaskStats' {stopDate} -> stopDate) (\s@ReplicationTaskStats' {} a -> s {stopDate = a} :: ReplicationTaskStats) Core.. Lens.mapping Core._Time

-- | The number of errors that have occurred during this task.
replicationTaskStats_tablesErrored :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.Int)
replicationTaskStats_tablesErrored = Lens.lens (\ReplicationTaskStats' {tablesErrored} -> tablesErrored) (\s@ReplicationTaskStats' {} a -> s {tablesErrored = a} :: ReplicationTaskStats)

-- | The date the replication task was started either with a fresh start or a
-- resume. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html#DMS-StartReplicationTask-request-StartReplicationTaskType StartReplicationTaskType>.
replicationTaskStats_startDate :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.UTCTime)
replicationTaskStats_startDate = Lens.lens (\ReplicationTaskStats' {startDate} -> startDate) (\s@ReplicationTaskStats' {} a -> s {startDate = a} :: ReplicationTaskStats) Core.. Lens.mapping Core._Time

-- | The date the replication task was started either with a fresh start or a
-- target reload.
replicationTaskStats_freshStartDate :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.UTCTime)
replicationTaskStats_freshStartDate = Lens.lens (\ReplicationTaskStats' {freshStartDate} -> freshStartDate) (\s@ReplicationTaskStats' {} a -> s {freshStartDate = a} :: ReplicationTaskStats) Core.. Lens.mapping Core._Time

-- | The number of tables currently loading for this task.
replicationTaskStats_tablesLoading :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.Int)
replicationTaskStats_tablesLoading = Lens.lens (\ReplicationTaskStats' {tablesLoading} -> tablesLoading) (\s@ReplicationTaskStats' {} a -> s {tablesLoading = a} :: ReplicationTaskStats)

-- | The date the replication task full load was started.
replicationTaskStats_fullLoadStartDate :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.UTCTime)
replicationTaskStats_fullLoadStartDate = Lens.lens (\ReplicationTaskStats' {fullLoadStartDate} -> fullLoadStartDate) (\s@ReplicationTaskStats' {} a -> s {fullLoadStartDate = a} :: ReplicationTaskStats) Core.. Lens.mapping Core._Time

-- | The elapsed time of the task, in milliseconds.
replicationTaskStats_elapsedTimeMillis :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.Integer)
replicationTaskStats_elapsedTimeMillis = Lens.lens (\ReplicationTaskStats' {elapsedTimeMillis} -> elapsedTimeMillis) (\s@ReplicationTaskStats' {} a -> s {elapsedTimeMillis = a} :: ReplicationTaskStats)

-- | The percent complete for the full load migration task.
replicationTaskStats_fullLoadProgressPercent :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.Int)
replicationTaskStats_fullLoadProgressPercent = Lens.lens (\ReplicationTaskStats' {fullLoadProgressPercent} -> fullLoadProgressPercent) (\s@ReplicationTaskStats' {} a -> s {fullLoadProgressPercent = a} :: ReplicationTaskStats)

-- | The number of tables queued for this task.
replicationTaskStats_tablesQueued :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.Int)
replicationTaskStats_tablesQueued = Lens.lens (\ReplicationTaskStats' {tablesQueued} -> tablesQueued) (\s@ReplicationTaskStats' {} a -> s {tablesQueued = a} :: ReplicationTaskStats)

-- | The date the replication task full load was completed.
replicationTaskStats_fullLoadFinishDate :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.UTCTime)
replicationTaskStats_fullLoadFinishDate = Lens.lens (\ReplicationTaskStats' {fullLoadFinishDate} -> fullLoadFinishDate) (\s@ReplicationTaskStats' {} a -> s {fullLoadFinishDate = a} :: ReplicationTaskStats) Core.. Lens.mapping Core._Time

-- | The number of tables loaded for this task.
replicationTaskStats_tablesLoaded :: Lens.Lens' ReplicationTaskStats (Core.Maybe Core.Int)
replicationTaskStats_tablesLoaded = Lens.lens (\ReplicationTaskStats' {tablesLoaded} -> tablesLoaded) (\s@ReplicationTaskStats' {} a -> s {tablesLoaded = a} :: ReplicationTaskStats)

instance Core.FromJSON ReplicationTaskStats where
  parseJSON =
    Core.withObject
      "ReplicationTaskStats"
      ( \x ->
          ReplicationTaskStats'
            Core.<$> (x Core..:? "StopDate")
            Core.<*> (x Core..:? "TablesErrored")
            Core.<*> (x Core..:? "StartDate")
            Core.<*> (x Core..:? "FreshStartDate")
            Core.<*> (x Core..:? "TablesLoading")
            Core.<*> (x Core..:? "FullLoadStartDate")
            Core.<*> (x Core..:? "ElapsedTimeMillis")
            Core.<*> (x Core..:? "FullLoadProgressPercent")
            Core.<*> (x Core..:? "TablesQueued")
            Core.<*> (x Core..:? "FullLoadFinishDate")
            Core.<*> (x Core..:? "TablesLoaded")
      )

instance Core.Hashable ReplicationTaskStats

instance Core.NFData ReplicationTaskStats
