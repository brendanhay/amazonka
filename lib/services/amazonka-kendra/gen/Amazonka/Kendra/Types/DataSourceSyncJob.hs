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
-- Module      : Amazonka.Kendra.Types.DataSourceSyncJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DataSourceSyncJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DataSourceSyncJobMetrics
import Amazonka.Kendra.Types.DataSourceSyncJobStatus
import Amazonka.Kendra.Types.ErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a data source synchronization job.
--
-- /See:/ 'newDataSourceSyncJob' smart constructor.
data DataSourceSyncJob = DataSourceSyncJob'
  { -- | If the reason that the synchronization failed is due to an error with
    -- the underlying data source, this field contains a code that identifies
    -- the error.
    dataSourceErrorCode :: Prelude.Maybe Prelude.Text,
    -- | The UNIX datetime that the synchronization job completed.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | If the @Status@ field is set to @FAILED@, the @ErrorCode@ field
    -- indicates the reason the synchronization failed.
    errorCode :: Prelude.Maybe ErrorCode,
    -- | If the @Status@ field is set to @ERROR@, the @ErrorMessage@ field
    -- contains a description of the error that caused the synchronization to
    -- fail.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | A identifier for the synchronization job.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | Maps a batch delete document request to a specific data source sync job.
    -- This is optional and should only be supplied when documents are deleted
    -- by a data source connector.
    metrics :: Prelude.Maybe DataSourceSyncJobMetrics,
    -- | The UNIX datetime that the synchronization job started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The execution status of the synchronization job. When the @Status@ field
    -- is set to @SUCCEEDED@, the synchronization job is done. If the status
    -- code is set to @FAILED@, the @ErrorCode@ and @ErrorMessage@ fields give
    -- you the reason for the failure.
    status :: Prelude.Maybe DataSourceSyncJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceSyncJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceErrorCode', 'dataSourceSyncJob_dataSourceErrorCode' - If the reason that the synchronization failed is due to an error with
-- the underlying data source, this field contains a code that identifies
-- the error.
--
-- 'endTime', 'dataSourceSyncJob_endTime' - The UNIX datetime that the synchronization job completed.
--
-- 'errorCode', 'dataSourceSyncJob_errorCode' - If the @Status@ field is set to @FAILED@, the @ErrorCode@ field
-- indicates the reason the synchronization failed.
--
-- 'errorMessage', 'dataSourceSyncJob_errorMessage' - If the @Status@ field is set to @ERROR@, the @ErrorMessage@ field
-- contains a description of the error that caused the synchronization to
-- fail.
--
-- 'executionId', 'dataSourceSyncJob_executionId' - A identifier for the synchronization job.
--
-- 'metrics', 'dataSourceSyncJob_metrics' - Maps a batch delete document request to a specific data source sync job.
-- This is optional and should only be supplied when documents are deleted
-- by a data source connector.
--
-- 'startTime', 'dataSourceSyncJob_startTime' - The UNIX datetime that the synchronization job started.
--
-- 'status', 'dataSourceSyncJob_status' - The execution status of the synchronization job. When the @Status@ field
-- is set to @SUCCEEDED@, the synchronization job is done. If the status
-- code is set to @FAILED@, the @ErrorCode@ and @ErrorMessage@ fields give
-- you the reason for the failure.
newDataSourceSyncJob ::
  DataSourceSyncJob
newDataSourceSyncJob =
  DataSourceSyncJob'
    { dataSourceErrorCode =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      executionId = Prelude.Nothing,
      metrics = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | If the reason that the synchronization failed is due to an error with
-- the underlying data source, this field contains a code that identifies
-- the error.
dataSourceSyncJob_dataSourceErrorCode :: Lens.Lens' DataSourceSyncJob (Prelude.Maybe Prelude.Text)
dataSourceSyncJob_dataSourceErrorCode = Lens.lens (\DataSourceSyncJob' {dataSourceErrorCode} -> dataSourceErrorCode) (\s@DataSourceSyncJob' {} a -> s {dataSourceErrorCode = a} :: DataSourceSyncJob)

-- | The UNIX datetime that the synchronization job completed.
dataSourceSyncJob_endTime :: Lens.Lens' DataSourceSyncJob (Prelude.Maybe Prelude.UTCTime)
dataSourceSyncJob_endTime = Lens.lens (\DataSourceSyncJob' {endTime} -> endTime) (\s@DataSourceSyncJob' {} a -> s {endTime = a} :: DataSourceSyncJob) Prelude.. Lens.mapping Data._Time

-- | If the @Status@ field is set to @FAILED@, the @ErrorCode@ field
-- indicates the reason the synchronization failed.
dataSourceSyncJob_errorCode :: Lens.Lens' DataSourceSyncJob (Prelude.Maybe ErrorCode)
dataSourceSyncJob_errorCode = Lens.lens (\DataSourceSyncJob' {errorCode} -> errorCode) (\s@DataSourceSyncJob' {} a -> s {errorCode = a} :: DataSourceSyncJob)

-- | If the @Status@ field is set to @ERROR@, the @ErrorMessage@ field
-- contains a description of the error that caused the synchronization to
-- fail.
dataSourceSyncJob_errorMessage :: Lens.Lens' DataSourceSyncJob (Prelude.Maybe Prelude.Text)
dataSourceSyncJob_errorMessage = Lens.lens (\DataSourceSyncJob' {errorMessage} -> errorMessage) (\s@DataSourceSyncJob' {} a -> s {errorMessage = a} :: DataSourceSyncJob)

-- | A identifier for the synchronization job.
dataSourceSyncJob_executionId :: Lens.Lens' DataSourceSyncJob (Prelude.Maybe Prelude.Text)
dataSourceSyncJob_executionId = Lens.lens (\DataSourceSyncJob' {executionId} -> executionId) (\s@DataSourceSyncJob' {} a -> s {executionId = a} :: DataSourceSyncJob)

-- | Maps a batch delete document request to a specific data source sync job.
-- This is optional and should only be supplied when documents are deleted
-- by a data source connector.
dataSourceSyncJob_metrics :: Lens.Lens' DataSourceSyncJob (Prelude.Maybe DataSourceSyncJobMetrics)
dataSourceSyncJob_metrics = Lens.lens (\DataSourceSyncJob' {metrics} -> metrics) (\s@DataSourceSyncJob' {} a -> s {metrics = a} :: DataSourceSyncJob)

-- | The UNIX datetime that the synchronization job started.
dataSourceSyncJob_startTime :: Lens.Lens' DataSourceSyncJob (Prelude.Maybe Prelude.UTCTime)
dataSourceSyncJob_startTime = Lens.lens (\DataSourceSyncJob' {startTime} -> startTime) (\s@DataSourceSyncJob' {} a -> s {startTime = a} :: DataSourceSyncJob) Prelude.. Lens.mapping Data._Time

-- | The execution status of the synchronization job. When the @Status@ field
-- is set to @SUCCEEDED@, the synchronization job is done. If the status
-- code is set to @FAILED@, the @ErrorCode@ and @ErrorMessage@ fields give
-- you the reason for the failure.
dataSourceSyncJob_status :: Lens.Lens' DataSourceSyncJob (Prelude.Maybe DataSourceSyncJobStatus)
dataSourceSyncJob_status = Lens.lens (\DataSourceSyncJob' {status} -> status) (\s@DataSourceSyncJob' {} a -> s {status = a} :: DataSourceSyncJob)

instance Data.FromJSON DataSourceSyncJob where
  parseJSON =
    Data.withObject
      "DataSourceSyncJob"
      ( \x ->
          DataSourceSyncJob'
            Prelude.<$> (x Data..:? "DataSourceErrorCode")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "ExecutionId")
            Prelude.<*> (x Data..:? "Metrics")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable DataSourceSyncJob where
  hashWithSalt _salt DataSourceSyncJob' {..} =
    _salt
      `Prelude.hashWithSalt` dataSourceErrorCode
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` executionId
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData DataSourceSyncJob where
  rnf DataSourceSyncJob' {..} =
    Prelude.rnf dataSourceErrorCode
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf executionId
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
