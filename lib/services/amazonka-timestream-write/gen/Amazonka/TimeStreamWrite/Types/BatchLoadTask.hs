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
-- Module      : Amazonka.TimeStreamWrite.Types.BatchLoadTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.BatchLoadTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.BatchLoadStatus

-- | Details about a batch load task.
--
-- /See:/ 'newBatchLoadTask' smart constructor.
data BatchLoadTask = BatchLoadTask'
  { -- | The time when the Timestream batch load task was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Database name for the database into which a batch load task loads data.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The time when the Timestream batch load task was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    resumableUntil :: Prelude.Maybe Data.POSIX,
    -- | Table name for the table into which a batch load task loads data.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the batch load task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | Status of the batch load task.
    taskStatus :: Prelude.Maybe BatchLoadStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchLoadTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'batchLoadTask_creationTime' - The time when the Timestream batch load task was created.
--
-- 'databaseName', 'batchLoadTask_databaseName' - Database name for the database into which a batch load task loads data.
--
-- 'lastUpdatedTime', 'batchLoadTask_lastUpdatedTime' - The time when the Timestream batch load task was last updated.
--
-- 'resumableUntil', 'batchLoadTask_resumableUntil' -
--
-- 'tableName', 'batchLoadTask_tableName' - Table name for the table into which a batch load task loads data.
--
-- 'taskId', 'batchLoadTask_taskId' - The ID of the batch load task.
--
-- 'taskStatus', 'batchLoadTask_taskStatus' - Status of the batch load task.
newBatchLoadTask ::
  BatchLoadTask
newBatchLoadTask =
  BatchLoadTask'
    { creationTime = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      resumableUntil = Prelude.Nothing,
      tableName = Prelude.Nothing,
      taskId = Prelude.Nothing,
      taskStatus = Prelude.Nothing
    }

-- | The time when the Timestream batch load task was created.
batchLoadTask_creationTime :: Lens.Lens' BatchLoadTask (Prelude.Maybe Prelude.UTCTime)
batchLoadTask_creationTime = Lens.lens (\BatchLoadTask' {creationTime} -> creationTime) (\s@BatchLoadTask' {} a -> s {creationTime = a} :: BatchLoadTask) Prelude.. Lens.mapping Data._Time

-- | Database name for the database into which a batch load task loads data.
batchLoadTask_databaseName :: Lens.Lens' BatchLoadTask (Prelude.Maybe Prelude.Text)
batchLoadTask_databaseName = Lens.lens (\BatchLoadTask' {databaseName} -> databaseName) (\s@BatchLoadTask' {} a -> s {databaseName = a} :: BatchLoadTask)

-- | The time when the Timestream batch load task was last updated.
batchLoadTask_lastUpdatedTime :: Lens.Lens' BatchLoadTask (Prelude.Maybe Prelude.UTCTime)
batchLoadTask_lastUpdatedTime = Lens.lens (\BatchLoadTask' {lastUpdatedTime} -> lastUpdatedTime) (\s@BatchLoadTask' {} a -> s {lastUpdatedTime = a} :: BatchLoadTask) Prelude.. Lens.mapping Data._Time

batchLoadTask_resumableUntil :: Lens.Lens' BatchLoadTask (Prelude.Maybe Prelude.UTCTime)
batchLoadTask_resumableUntil = Lens.lens (\BatchLoadTask' {resumableUntil} -> resumableUntil) (\s@BatchLoadTask' {} a -> s {resumableUntil = a} :: BatchLoadTask) Prelude.. Lens.mapping Data._Time

-- | Table name for the table into which a batch load task loads data.
batchLoadTask_tableName :: Lens.Lens' BatchLoadTask (Prelude.Maybe Prelude.Text)
batchLoadTask_tableName = Lens.lens (\BatchLoadTask' {tableName} -> tableName) (\s@BatchLoadTask' {} a -> s {tableName = a} :: BatchLoadTask)

-- | The ID of the batch load task.
batchLoadTask_taskId :: Lens.Lens' BatchLoadTask (Prelude.Maybe Prelude.Text)
batchLoadTask_taskId = Lens.lens (\BatchLoadTask' {taskId} -> taskId) (\s@BatchLoadTask' {} a -> s {taskId = a} :: BatchLoadTask)

-- | Status of the batch load task.
batchLoadTask_taskStatus :: Lens.Lens' BatchLoadTask (Prelude.Maybe BatchLoadStatus)
batchLoadTask_taskStatus = Lens.lens (\BatchLoadTask' {taskStatus} -> taskStatus) (\s@BatchLoadTask' {} a -> s {taskStatus = a} :: BatchLoadTask)

instance Data.FromJSON BatchLoadTask where
  parseJSON =
    Data.withObject
      "BatchLoadTask"
      ( \x ->
          BatchLoadTask'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "ResumableUntil")
            Prelude.<*> (x Data..:? "TableName")
            Prelude.<*> (x Data..:? "TaskId")
            Prelude.<*> (x Data..:? "TaskStatus")
      )

instance Prelude.Hashable BatchLoadTask where
  hashWithSalt _salt BatchLoadTask' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` resumableUntil
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` taskId
      `Prelude.hashWithSalt` taskStatus

instance Prelude.NFData BatchLoadTask where
  rnf BatchLoadTask' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf resumableUntil
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf taskStatus
