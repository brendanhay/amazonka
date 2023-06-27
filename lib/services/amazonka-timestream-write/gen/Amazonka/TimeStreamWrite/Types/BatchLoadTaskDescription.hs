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
-- Module      : Amazonka.TimeStreamWrite.Types.BatchLoadTaskDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.BatchLoadTaskDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.BatchLoadProgressReport
import Amazonka.TimeStreamWrite.Types.BatchLoadStatus
import Amazonka.TimeStreamWrite.Types.DataModelConfiguration
import Amazonka.TimeStreamWrite.Types.DataSourceConfiguration
import Amazonka.TimeStreamWrite.Types.ReportConfiguration

-- | Details about a batch load task.
--
-- /See:/ 'newBatchLoadTaskDescription' smart constructor.
data BatchLoadTaskDescription = BatchLoadTaskDescription'
  { -- | The time when the Timestream batch load task was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Data model configuration for a batch load task. This contains details
    -- about where a data model for a batch load task is stored.
    dataModelConfiguration :: Prelude.Maybe DataModelConfiguration,
    -- | Configuration details about the data source for a batch load task.
    dataSourceConfiguration :: Prelude.Maybe DataSourceConfiguration,
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The time when the Timestream batch load task was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    progressReport :: Prelude.Maybe BatchLoadProgressReport,
    recordVersion :: Prelude.Maybe Prelude.Integer,
    -- | Report configuration for a batch load task. This contains details about
    -- where error reports are stored.
    reportConfiguration :: Prelude.Maybe ReportConfiguration,
    resumableUntil :: Prelude.Maybe Data.POSIX,
    targetDatabaseName :: Prelude.Maybe Prelude.Text,
    targetTableName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the batch load task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | Status of the batch load task.
    taskStatus :: Prelude.Maybe BatchLoadStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchLoadTaskDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'batchLoadTaskDescription_creationTime' - The time when the Timestream batch load task was created.
--
-- 'dataModelConfiguration', 'batchLoadTaskDescription_dataModelConfiguration' - Data model configuration for a batch load task. This contains details
-- about where a data model for a batch load task is stored.
--
-- 'dataSourceConfiguration', 'batchLoadTaskDescription_dataSourceConfiguration' - Configuration details about the data source for a batch load task.
--
-- 'errorMessage', 'batchLoadTaskDescription_errorMessage' -
--
-- 'lastUpdatedTime', 'batchLoadTaskDescription_lastUpdatedTime' - The time when the Timestream batch load task was last updated.
--
-- 'progressReport', 'batchLoadTaskDescription_progressReport' -
--
-- 'recordVersion', 'batchLoadTaskDescription_recordVersion' -
--
-- 'reportConfiguration', 'batchLoadTaskDescription_reportConfiguration' - Report configuration for a batch load task. This contains details about
-- where error reports are stored.
--
-- 'resumableUntil', 'batchLoadTaskDescription_resumableUntil' -
--
-- 'targetDatabaseName', 'batchLoadTaskDescription_targetDatabaseName' -
--
-- 'targetTableName', 'batchLoadTaskDescription_targetTableName' -
--
-- 'taskId', 'batchLoadTaskDescription_taskId' - The ID of the batch load task.
--
-- 'taskStatus', 'batchLoadTaskDescription_taskStatus' - Status of the batch load task.
newBatchLoadTaskDescription ::
  BatchLoadTaskDescription
newBatchLoadTaskDescription =
  BatchLoadTaskDescription'
    { creationTime =
        Prelude.Nothing,
      dataModelConfiguration = Prelude.Nothing,
      dataSourceConfiguration = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      progressReport = Prelude.Nothing,
      recordVersion = Prelude.Nothing,
      reportConfiguration = Prelude.Nothing,
      resumableUntil = Prelude.Nothing,
      targetDatabaseName = Prelude.Nothing,
      targetTableName = Prelude.Nothing,
      taskId = Prelude.Nothing,
      taskStatus = Prelude.Nothing
    }

-- | The time when the Timestream batch load task was created.
batchLoadTaskDescription_creationTime :: Lens.Lens' BatchLoadTaskDescription (Prelude.Maybe Prelude.UTCTime)
batchLoadTaskDescription_creationTime = Lens.lens (\BatchLoadTaskDescription' {creationTime} -> creationTime) (\s@BatchLoadTaskDescription' {} a -> s {creationTime = a} :: BatchLoadTaskDescription) Prelude.. Lens.mapping Data._Time

-- | Data model configuration for a batch load task. This contains details
-- about where a data model for a batch load task is stored.
batchLoadTaskDescription_dataModelConfiguration :: Lens.Lens' BatchLoadTaskDescription (Prelude.Maybe DataModelConfiguration)
batchLoadTaskDescription_dataModelConfiguration = Lens.lens (\BatchLoadTaskDescription' {dataModelConfiguration} -> dataModelConfiguration) (\s@BatchLoadTaskDescription' {} a -> s {dataModelConfiguration = a} :: BatchLoadTaskDescription)

-- | Configuration details about the data source for a batch load task.
batchLoadTaskDescription_dataSourceConfiguration :: Lens.Lens' BatchLoadTaskDescription (Prelude.Maybe DataSourceConfiguration)
batchLoadTaskDescription_dataSourceConfiguration = Lens.lens (\BatchLoadTaskDescription' {dataSourceConfiguration} -> dataSourceConfiguration) (\s@BatchLoadTaskDescription' {} a -> s {dataSourceConfiguration = a} :: BatchLoadTaskDescription)

batchLoadTaskDescription_errorMessage :: Lens.Lens' BatchLoadTaskDescription (Prelude.Maybe Prelude.Text)
batchLoadTaskDescription_errorMessage = Lens.lens (\BatchLoadTaskDescription' {errorMessage} -> errorMessage) (\s@BatchLoadTaskDescription' {} a -> s {errorMessage = a} :: BatchLoadTaskDescription)

-- | The time when the Timestream batch load task was last updated.
batchLoadTaskDescription_lastUpdatedTime :: Lens.Lens' BatchLoadTaskDescription (Prelude.Maybe Prelude.UTCTime)
batchLoadTaskDescription_lastUpdatedTime = Lens.lens (\BatchLoadTaskDescription' {lastUpdatedTime} -> lastUpdatedTime) (\s@BatchLoadTaskDescription' {} a -> s {lastUpdatedTime = a} :: BatchLoadTaskDescription) Prelude.. Lens.mapping Data._Time

batchLoadTaskDescription_progressReport :: Lens.Lens' BatchLoadTaskDescription (Prelude.Maybe BatchLoadProgressReport)
batchLoadTaskDescription_progressReport = Lens.lens (\BatchLoadTaskDescription' {progressReport} -> progressReport) (\s@BatchLoadTaskDescription' {} a -> s {progressReport = a} :: BatchLoadTaskDescription)

batchLoadTaskDescription_recordVersion :: Lens.Lens' BatchLoadTaskDescription (Prelude.Maybe Prelude.Integer)
batchLoadTaskDescription_recordVersion = Lens.lens (\BatchLoadTaskDescription' {recordVersion} -> recordVersion) (\s@BatchLoadTaskDescription' {} a -> s {recordVersion = a} :: BatchLoadTaskDescription)

-- | Report configuration for a batch load task. This contains details about
-- where error reports are stored.
batchLoadTaskDescription_reportConfiguration :: Lens.Lens' BatchLoadTaskDescription (Prelude.Maybe ReportConfiguration)
batchLoadTaskDescription_reportConfiguration = Lens.lens (\BatchLoadTaskDescription' {reportConfiguration} -> reportConfiguration) (\s@BatchLoadTaskDescription' {} a -> s {reportConfiguration = a} :: BatchLoadTaskDescription)

batchLoadTaskDescription_resumableUntil :: Lens.Lens' BatchLoadTaskDescription (Prelude.Maybe Prelude.UTCTime)
batchLoadTaskDescription_resumableUntil = Lens.lens (\BatchLoadTaskDescription' {resumableUntil} -> resumableUntil) (\s@BatchLoadTaskDescription' {} a -> s {resumableUntil = a} :: BatchLoadTaskDescription) Prelude.. Lens.mapping Data._Time

batchLoadTaskDescription_targetDatabaseName :: Lens.Lens' BatchLoadTaskDescription (Prelude.Maybe Prelude.Text)
batchLoadTaskDescription_targetDatabaseName = Lens.lens (\BatchLoadTaskDescription' {targetDatabaseName} -> targetDatabaseName) (\s@BatchLoadTaskDescription' {} a -> s {targetDatabaseName = a} :: BatchLoadTaskDescription)

batchLoadTaskDescription_targetTableName :: Lens.Lens' BatchLoadTaskDescription (Prelude.Maybe Prelude.Text)
batchLoadTaskDescription_targetTableName = Lens.lens (\BatchLoadTaskDescription' {targetTableName} -> targetTableName) (\s@BatchLoadTaskDescription' {} a -> s {targetTableName = a} :: BatchLoadTaskDescription)

-- | The ID of the batch load task.
batchLoadTaskDescription_taskId :: Lens.Lens' BatchLoadTaskDescription (Prelude.Maybe Prelude.Text)
batchLoadTaskDescription_taskId = Lens.lens (\BatchLoadTaskDescription' {taskId} -> taskId) (\s@BatchLoadTaskDescription' {} a -> s {taskId = a} :: BatchLoadTaskDescription)

-- | Status of the batch load task.
batchLoadTaskDescription_taskStatus :: Lens.Lens' BatchLoadTaskDescription (Prelude.Maybe BatchLoadStatus)
batchLoadTaskDescription_taskStatus = Lens.lens (\BatchLoadTaskDescription' {taskStatus} -> taskStatus) (\s@BatchLoadTaskDescription' {} a -> s {taskStatus = a} :: BatchLoadTaskDescription)

instance Data.FromJSON BatchLoadTaskDescription where
  parseJSON =
    Data.withObject
      "BatchLoadTaskDescription"
      ( \x ->
          BatchLoadTaskDescription'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DataModelConfiguration")
            Prelude.<*> (x Data..:? "DataSourceConfiguration")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "ProgressReport")
            Prelude.<*> (x Data..:? "RecordVersion")
            Prelude.<*> (x Data..:? "ReportConfiguration")
            Prelude.<*> (x Data..:? "ResumableUntil")
            Prelude.<*> (x Data..:? "TargetDatabaseName")
            Prelude.<*> (x Data..:? "TargetTableName")
            Prelude.<*> (x Data..:? "TaskId")
            Prelude.<*> (x Data..:? "TaskStatus")
      )

instance Prelude.Hashable BatchLoadTaskDescription where
  hashWithSalt _salt BatchLoadTaskDescription' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dataModelConfiguration
      `Prelude.hashWithSalt` dataSourceConfiguration
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` progressReport
      `Prelude.hashWithSalt` recordVersion
      `Prelude.hashWithSalt` reportConfiguration
      `Prelude.hashWithSalt` resumableUntil
      `Prelude.hashWithSalt` targetDatabaseName
      `Prelude.hashWithSalt` targetTableName
      `Prelude.hashWithSalt` taskId
      `Prelude.hashWithSalt` taskStatus

instance Prelude.NFData BatchLoadTaskDescription where
  rnf BatchLoadTaskDescription' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dataModelConfiguration
      `Prelude.seq` Prelude.rnf dataSourceConfiguration
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf progressReport
      `Prelude.seq` Prelude.rnf recordVersion
      `Prelude.seq` Prelude.rnf reportConfiguration
      `Prelude.seq` Prelude.rnf resumableUntil
      `Prelude.seq` Prelude.rnf targetDatabaseName
      `Prelude.seq` Prelude.rnf targetTableName
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf taskStatus
