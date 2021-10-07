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
import qualified Network.AWS.Prelude as Prelude

-- | In response to a request by the @DescribeReplicationTasks@ operation,
-- this object provides a collection of statistics about a replication
-- task.
--
-- /See:/ 'newReplicationTaskStats' smart constructor.
data ReplicationTaskStats = ReplicationTaskStats'
  { -- | The number of errors that have occurred during this task.
    tablesErrored :: Prelude.Maybe Prelude.Int,
    -- | The date the replication task was stopped.
    stopDate :: Prelude.Maybe Core.POSIX,
    -- | The date the replication task was started either with a fresh start or a
    -- resume. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html#DMS-StartReplicationTask-request-StartReplicationTaskType StartReplicationTaskType>.
    startDate :: Prelude.Maybe Core.POSIX,
    -- | The date the replication task was started either with a fresh start or a
    -- target reload.
    freshStartDate :: Prelude.Maybe Core.POSIX,
    -- | The number of tables currently loading for this task.
    tablesLoading :: Prelude.Maybe Prelude.Int,
    -- | The date the replication task full load was started.
    fullLoadStartDate :: Prelude.Maybe Core.POSIX,
    -- | The elapsed time of the task, in milliseconds.
    elapsedTimeMillis :: Prelude.Maybe Prelude.Integer,
    -- | The percent complete for the full load migration task.
    fullLoadProgressPercent :: Prelude.Maybe Prelude.Int,
    -- | The number of tables queued for this task.
    tablesQueued :: Prelude.Maybe Prelude.Int,
    -- | The number of tables loaded for this task.
    tablesLoaded :: Prelude.Maybe Prelude.Int,
    -- | The date the replication task full load was completed.
    fullLoadFinishDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationTaskStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tablesErrored', 'replicationTaskStats_tablesErrored' - The number of errors that have occurred during this task.
--
-- 'stopDate', 'replicationTaskStats_stopDate' - The date the replication task was stopped.
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
-- 'tablesLoaded', 'replicationTaskStats_tablesLoaded' - The number of tables loaded for this task.
--
-- 'fullLoadFinishDate', 'replicationTaskStats_fullLoadFinishDate' - The date the replication task full load was completed.
newReplicationTaskStats ::
  ReplicationTaskStats
newReplicationTaskStats =
  ReplicationTaskStats'
    { tablesErrored =
        Prelude.Nothing,
      stopDate = Prelude.Nothing,
      startDate = Prelude.Nothing,
      freshStartDate = Prelude.Nothing,
      tablesLoading = Prelude.Nothing,
      fullLoadStartDate = Prelude.Nothing,
      elapsedTimeMillis = Prelude.Nothing,
      fullLoadProgressPercent = Prelude.Nothing,
      tablesQueued = Prelude.Nothing,
      tablesLoaded = Prelude.Nothing,
      fullLoadFinishDate = Prelude.Nothing
    }

-- | The number of errors that have occurred during this task.
replicationTaskStats_tablesErrored :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.Int)
replicationTaskStats_tablesErrored = Lens.lens (\ReplicationTaskStats' {tablesErrored} -> tablesErrored) (\s@ReplicationTaskStats' {} a -> s {tablesErrored = a} :: ReplicationTaskStats)

-- | The date the replication task was stopped.
replicationTaskStats_stopDate :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.UTCTime)
replicationTaskStats_stopDate = Lens.lens (\ReplicationTaskStats' {stopDate} -> stopDate) (\s@ReplicationTaskStats' {} a -> s {stopDate = a} :: ReplicationTaskStats) Prelude.. Lens.mapping Core._Time

-- | The date the replication task was started either with a fresh start or a
-- resume. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html#DMS-StartReplicationTask-request-StartReplicationTaskType StartReplicationTaskType>.
replicationTaskStats_startDate :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.UTCTime)
replicationTaskStats_startDate = Lens.lens (\ReplicationTaskStats' {startDate} -> startDate) (\s@ReplicationTaskStats' {} a -> s {startDate = a} :: ReplicationTaskStats) Prelude.. Lens.mapping Core._Time

-- | The date the replication task was started either with a fresh start or a
-- target reload.
replicationTaskStats_freshStartDate :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.UTCTime)
replicationTaskStats_freshStartDate = Lens.lens (\ReplicationTaskStats' {freshStartDate} -> freshStartDate) (\s@ReplicationTaskStats' {} a -> s {freshStartDate = a} :: ReplicationTaskStats) Prelude.. Lens.mapping Core._Time

-- | The number of tables currently loading for this task.
replicationTaskStats_tablesLoading :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.Int)
replicationTaskStats_tablesLoading = Lens.lens (\ReplicationTaskStats' {tablesLoading} -> tablesLoading) (\s@ReplicationTaskStats' {} a -> s {tablesLoading = a} :: ReplicationTaskStats)

-- | The date the replication task full load was started.
replicationTaskStats_fullLoadStartDate :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.UTCTime)
replicationTaskStats_fullLoadStartDate = Lens.lens (\ReplicationTaskStats' {fullLoadStartDate} -> fullLoadStartDate) (\s@ReplicationTaskStats' {} a -> s {fullLoadStartDate = a} :: ReplicationTaskStats) Prelude.. Lens.mapping Core._Time

-- | The elapsed time of the task, in milliseconds.
replicationTaskStats_elapsedTimeMillis :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.Integer)
replicationTaskStats_elapsedTimeMillis = Lens.lens (\ReplicationTaskStats' {elapsedTimeMillis} -> elapsedTimeMillis) (\s@ReplicationTaskStats' {} a -> s {elapsedTimeMillis = a} :: ReplicationTaskStats)

-- | The percent complete for the full load migration task.
replicationTaskStats_fullLoadProgressPercent :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.Int)
replicationTaskStats_fullLoadProgressPercent = Lens.lens (\ReplicationTaskStats' {fullLoadProgressPercent} -> fullLoadProgressPercent) (\s@ReplicationTaskStats' {} a -> s {fullLoadProgressPercent = a} :: ReplicationTaskStats)

-- | The number of tables queued for this task.
replicationTaskStats_tablesQueued :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.Int)
replicationTaskStats_tablesQueued = Lens.lens (\ReplicationTaskStats' {tablesQueued} -> tablesQueued) (\s@ReplicationTaskStats' {} a -> s {tablesQueued = a} :: ReplicationTaskStats)

-- | The number of tables loaded for this task.
replicationTaskStats_tablesLoaded :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.Int)
replicationTaskStats_tablesLoaded = Lens.lens (\ReplicationTaskStats' {tablesLoaded} -> tablesLoaded) (\s@ReplicationTaskStats' {} a -> s {tablesLoaded = a} :: ReplicationTaskStats)

-- | The date the replication task full load was completed.
replicationTaskStats_fullLoadFinishDate :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.UTCTime)
replicationTaskStats_fullLoadFinishDate = Lens.lens (\ReplicationTaskStats' {fullLoadFinishDate} -> fullLoadFinishDate) (\s@ReplicationTaskStats' {} a -> s {fullLoadFinishDate = a} :: ReplicationTaskStats) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ReplicationTaskStats where
  parseJSON =
    Core.withObject
      "ReplicationTaskStats"
      ( \x ->
          ReplicationTaskStats'
            Prelude.<$> (x Core..:? "TablesErrored")
            Prelude.<*> (x Core..:? "StopDate")
            Prelude.<*> (x Core..:? "StartDate")
            Prelude.<*> (x Core..:? "FreshStartDate")
            Prelude.<*> (x Core..:? "TablesLoading")
            Prelude.<*> (x Core..:? "FullLoadStartDate")
            Prelude.<*> (x Core..:? "ElapsedTimeMillis")
            Prelude.<*> (x Core..:? "FullLoadProgressPercent")
            Prelude.<*> (x Core..:? "TablesQueued")
            Prelude.<*> (x Core..:? "TablesLoaded")
            Prelude.<*> (x Core..:? "FullLoadFinishDate")
      )

instance Prelude.Hashable ReplicationTaskStats

instance Prelude.NFData ReplicationTaskStats
