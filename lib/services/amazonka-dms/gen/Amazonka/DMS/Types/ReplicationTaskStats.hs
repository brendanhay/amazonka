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
-- Module      : Amazonka.DMS.Types.ReplicationTaskStats
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.ReplicationTaskStats where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | In response to a request by the @DescribeReplicationTasks@ operation,
-- this object provides a collection of statistics about a replication
-- task.
--
-- /See:/ 'newReplicationTaskStats' smart constructor.
data ReplicationTaskStats = ReplicationTaskStats'
  { -- | The elapsed time of the task, in milliseconds.
    elapsedTimeMillis :: Prelude.Maybe Prelude.Integer,
    -- | The date the replication task was started either with a fresh start or a
    -- target reload.
    freshStartDate :: Prelude.Maybe Data.POSIX,
    -- | The date the replication task full load was completed.
    fullLoadFinishDate :: Prelude.Maybe Data.POSIX,
    -- | The percent complete for the full load migration task.
    fullLoadProgressPercent :: Prelude.Maybe Prelude.Int,
    -- | The date the replication task full load was started.
    fullLoadStartDate :: Prelude.Maybe Data.POSIX,
    -- | The date the replication task was started either with a fresh start or a
    -- resume. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html#DMS-StartReplicationTask-request-StartReplicationTaskType StartReplicationTaskType>.
    startDate :: Prelude.Maybe Data.POSIX,
    -- | The date the replication task was stopped.
    stopDate :: Prelude.Maybe Data.POSIX,
    -- | The number of errors that have occurred during this task.
    tablesErrored :: Prelude.Maybe Prelude.Int,
    -- | The number of tables loaded for this task.
    tablesLoaded :: Prelude.Maybe Prelude.Int,
    -- | The number of tables currently loading for this task.
    tablesLoading :: Prelude.Maybe Prelude.Int,
    -- | The number of tables queued for this task.
    tablesQueued :: Prelude.Maybe Prelude.Int
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
-- 'elapsedTimeMillis', 'replicationTaskStats_elapsedTimeMillis' - The elapsed time of the task, in milliseconds.
--
-- 'freshStartDate', 'replicationTaskStats_freshStartDate' - The date the replication task was started either with a fresh start or a
-- target reload.
--
-- 'fullLoadFinishDate', 'replicationTaskStats_fullLoadFinishDate' - The date the replication task full load was completed.
--
-- 'fullLoadProgressPercent', 'replicationTaskStats_fullLoadProgressPercent' - The percent complete for the full load migration task.
--
-- 'fullLoadStartDate', 'replicationTaskStats_fullLoadStartDate' - The date the replication task full load was started.
--
-- 'startDate', 'replicationTaskStats_startDate' - The date the replication task was started either with a fresh start or a
-- resume. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html#DMS-StartReplicationTask-request-StartReplicationTaskType StartReplicationTaskType>.
--
-- 'stopDate', 'replicationTaskStats_stopDate' - The date the replication task was stopped.
--
-- 'tablesErrored', 'replicationTaskStats_tablesErrored' - The number of errors that have occurred during this task.
--
-- 'tablesLoaded', 'replicationTaskStats_tablesLoaded' - The number of tables loaded for this task.
--
-- 'tablesLoading', 'replicationTaskStats_tablesLoading' - The number of tables currently loading for this task.
--
-- 'tablesQueued', 'replicationTaskStats_tablesQueued' - The number of tables queued for this task.
newReplicationTaskStats ::
  ReplicationTaskStats
newReplicationTaskStats =
  ReplicationTaskStats'
    { elapsedTimeMillis =
        Prelude.Nothing,
      freshStartDate = Prelude.Nothing,
      fullLoadFinishDate = Prelude.Nothing,
      fullLoadProgressPercent = Prelude.Nothing,
      fullLoadStartDate = Prelude.Nothing,
      startDate = Prelude.Nothing,
      stopDate = Prelude.Nothing,
      tablesErrored = Prelude.Nothing,
      tablesLoaded = Prelude.Nothing,
      tablesLoading = Prelude.Nothing,
      tablesQueued = Prelude.Nothing
    }

-- | The elapsed time of the task, in milliseconds.
replicationTaskStats_elapsedTimeMillis :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.Integer)
replicationTaskStats_elapsedTimeMillis = Lens.lens (\ReplicationTaskStats' {elapsedTimeMillis} -> elapsedTimeMillis) (\s@ReplicationTaskStats' {} a -> s {elapsedTimeMillis = a} :: ReplicationTaskStats)

-- | The date the replication task was started either with a fresh start or a
-- target reload.
replicationTaskStats_freshStartDate :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.UTCTime)
replicationTaskStats_freshStartDate = Lens.lens (\ReplicationTaskStats' {freshStartDate} -> freshStartDate) (\s@ReplicationTaskStats' {} a -> s {freshStartDate = a} :: ReplicationTaskStats) Prelude.. Lens.mapping Data._Time

-- | The date the replication task full load was completed.
replicationTaskStats_fullLoadFinishDate :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.UTCTime)
replicationTaskStats_fullLoadFinishDate = Lens.lens (\ReplicationTaskStats' {fullLoadFinishDate} -> fullLoadFinishDate) (\s@ReplicationTaskStats' {} a -> s {fullLoadFinishDate = a} :: ReplicationTaskStats) Prelude.. Lens.mapping Data._Time

-- | The percent complete for the full load migration task.
replicationTaskStats_fullLoadProgressPercent :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.Int)
replicationTaskStats_fullLoadProgressPercent = Lens.lens (\ReplicationTaskStats' {fullLoadProgressPercent} -> fullLoadProgressPercent) (\s@ReplicationTaskStats' {} a -> s {fullLoadProgressPercent = a} :: ReplicationTaskStats)

-- | The date the replication task full load was started.
replicationTaskStats_fullLoadStartDate :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.UTCTime)
replicationTaskStats_fullLoadStartDate = Lens.lens (\ReplicationTaskStats' {fullLoadStartDate} -> fullLoadStartDate) (\s@ReplicationTaskStats' {} a -> s {fullLoadStartDate = a} :: ReplicationTaskStats) Prelude.. Lens.mapping Data._Time

-- | The date the replication task was started either with a fresh start or a
-- resume. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html#DMS-StartReplicationTask-request-StartReplicationTaskType StartReplicationTaskType>.
replicationTaskStats_startDate :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.UTCTime)
replicationTaskStats_startDate = Lens.lens (\ReplicationTaskStats' {startDate} -> startDate) (\s@ReplicationTaskStats' {} a -> s {startDate = a} :: ReplicationTaskStats) Prelude.. Lens.mapping Data._Time

-- | The date the replication task was stopped.
replicationTaskStats_stopDate :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.UTCTime)
replicationTaskStats_stopDate = Lens.lens (\ReplicationTaskStats' {stopDate} -> stopDate) (\s@ReplicationTaskStats' {} a -> s {stopDate = a} :: ReplicationTaskStats) Prelude.. Lens.mapping Data._Time

-- | The number of errors that have occurred during this task.
replicationTaskStats_tablesErrored :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.Int)
replicationTaskStats_tablesErrored = Lens.lens (\ReplicationTaskStats' {tablesErrored} -> tablesErrored) (\s@ReplicationTaskStats' {} a -> s {tablesErrored = a} :: ReplicationTaskStats)

-- | The number of tables loaded for this task.
replicationTaskStats_tablesLoaded :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.Int)
replicationTaskStats_tablesLoaded = Lens.lens (\ReplicationTaskStats' {tablesLoaded} -> tablesLoaded) (\s@ReplicationTaskStats' {} a -> s {tablesLoaded = a} :: ReplicationTaskStats)

-- | The number of tables currently loading for this task.
replicationTaskStats_tablesLoading :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.Int)
replicationTaskStats_tablesLoading = Lens.lens (\ReplicationTaskStats' {tablesLoading} -> tablesLoading) (\s@ReplicationTaskStats' {} a -> s {tablesLoading = a} :: ReplicationTaskStats)

-- | The number of tables queued for this task.
replicationTaskStats_tablesQueued :: Lens.Lens' ReplicationTaskStats (Prelude.Maybe Prelude.Int)
replicationTaskStats_tablesQueued = Lens.lens (\ReplicationTaskStats' {tablesQueued} -> tablesQueued) (\s@ReplicationTaskStats' {} a -> s {tablesQueued = a} :: ReplicationTaskStats)

instance Data.FromJSON ReplicationTaskStats where
  parseJSON =
    Data.withObject
      "ReplicationTaskStats"
      ( \x ->
          ReplicationTaskStats'
            Prelude.<$> (x Data..:? "ElapsedTimeMillis")
            Prelude.<*> (x Data..:? "FreshStartDate")
            Prelude.<*> (x Data..:? "FullLoadFinishDate")
            Prelude.<*> (x Data..:? "FullLoadProgressPercent")
            Prelude.<*> (x Data..:? "FullLoadStartDate")
            Prelude.<*> (x Data..:? "StartDate")
            Prelude.<*> (x Data..:? "StopDate")
            Prelude.<*> (x Data..:? "TablesErrored")
            Prelude.<*> (x Data..:? "TablesLoaded")
            Prelude.<*> (x Data..:? "TablesLoading")
            Prelude.<*> (x Data..:? "TablesQueued")
      )

instance Prelude.Hashable ReplicationTaskStats where
  hashWithSalt _salt ReplicationTaskStats' {..} =
    _salt `Prelude.hashWithSalt` elapsedTimeMillis
      `Prelude.hashWithSalt` freshStartDate
      `Prelude.hashWithSalt` fullLoadFinishDate
      `Prelude.hashWithSalt` fullLoadProgressPercent
      `Prelude.hashWithSalt` fullLoadStartDate
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` stopDate
      `Prelude.hashWithSalt` tablesErrored
      `Prelude.hashWithSalt` tablesLoaded
      `Prelude.hashWithSalt` tablesLoading
      `Prelude.hashWithSalt` tablesQueued

instance Prelude.NFData ReplicationTaskStats where
  rnf ReplicationTaskStats' {..} =
    Prelude.rnf elapsedTimeMillis
      `Prelude.seq` Prelude.rnf freshStartDate
      `Prelude.seq` Prelude.rnf fullLoadFinishDate
      `Prelude.seq` Prelude.rnf fullLoadProgressPercent
      `Prelude.seq` Prelude.rnf fullLoadStartDate
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf stopDate
      `Prelude.seq` Prelude.rnf tablesErrored
      `Prelude.seq` Prelude.rnf tablesLoaded
      `Prelude.seq` Prelude.rnf tablesLoading
      `Prelude.seq` Prelude.rnf tablesQueued
