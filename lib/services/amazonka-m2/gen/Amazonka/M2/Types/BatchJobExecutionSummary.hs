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
-- Module      : Amazonka.M2.Types.BatchJobExecutionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.BatchJobExecutionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types.BatchJobExecutionStatus
import Amazonka.M2.Types.BatchJobIdentifier
import Amazonka.M2.Types.BatchJobType
import qualified Amazonka.Prelude as Prelude

-- | A subset of the possible batch job attributes. Used in the batch job
-- list.
--
-- /See:/ 'newBatchJobExecutionSummary' smart constructor.
data BatchJobExecutionSummary = BatchJobExecutionSummary'
  { -- | The unique identifier of this batch job.
    batchJobIdentifier :: Prelude.Maybe BatchJobIdentifier,
    -- | The timestamp when this batch job execution ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier of a particular batch job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The name of a particular batch job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The type of a particular batch job execution.
    jobType :: Prelude.Maybe BatchJobType,
    -- | The batch job return code from either the Blu Age or Micro Focus runtime
    -- engines. For more information, see
    -- <https://www.ibm.com/docs/en/was/8.5.5?topic=model-batch-return-codes Batch return codes>
    -- in the /IBM WebSphere Application Server/ documentation.
    returnCode :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the application that hosts this batch job.
    applicationId :: Prelude.Text,
    -- | The unique identifier of this execution of the batch job.
    executionId :: Prelude.Text,
    -- | The timestamp when a particular batch job execution started.
    startTime :: Data.POSIX,
    -- | The status of a particular batch job execution.
    status :: BatchJobExecutionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchJobExecutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchJobIdentifier', 'batchJobExecutionSummary_batchJobIdentifier' - The unique identifier of this batch job.
--
-- 'endTime', 'batchJobExecutionSummary_endTime' - The timestamp when this batch job execution ended.
--
-- 'jobId', 'batchJobExecutionSummary_jobId' - The unique identifier of a particular batch job.
--
-- 'jobName', 'batchJobExecutionSummary_jobName' - The name of a particular batch job.
--
-- 'jobType', 'batchJobExecutionSummary_jobType' - The type of a particular batch job execution.
--
-- 'returnCode', 'batchJobExecutionSummary_returnCode' - The batch job return code from either the Blu Age or Micro Focus runtime
-- engines. For more information, see
-- <https://www.ibm.com/docs/en/was/8.5.5?topic=model-batch-return-codes Batch return codes>
-- in the /IBM WebSphere Application Server/ documentation.
--
-- 'applicationId', 'batchJobExecutionSummary_applicationId' - The unique identifier of the application that hosts this batch job.
--
-- 'executionId', 'batchJobExecutionSummary_executionId' - The unique identifier of this execution of the batch job.
--
-- 'startTime', 'batchJobExecutionSummary_startTime' - The timestamp when a particular batch job execution started.
--
-- 'status', 'batchJobExecutionSummary_status' - The status of a particular batch job execution.
newBatchJobExecutionSummary ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'executionId'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'status'
  BatchJobExecutionStatus ->
  BatchJobExecutionSummary
newBatchJobExecutionSummary
  pApplicationId_
  pExecutionId_
  pStartTime_
  pStatus_ =
    BatchJobExecutionSummary'
      { batchJobIdentifier =
          Prelude.Nothing,
        endTime = Prelude.Nothing,
        jobId = Prelude.Nothing,
        jobName = Prelude.Nothing,
        jobType = Prelude.Nothing,
        returnCode = Prelude.Nothing,
        applicationId = pApplicationId_,
        executionId = pExecutionId_,
        startTime = Data._Time Lens.# pStartTime_,
        status = pStatus_
      }

-- | The unique identifier of this batch job.
batchJobExecutionSummary_batchJobIdentifier :: Lens.Lens' BatchJobExecutionSummary (Prelude.Maybe BatchJobIdentifier)
batchJobExecutionSummary_batchJobIdentifier = Lens.lens (\BatchJobExecutionSummary' {batchJobIdentifier} -> batchJobIdentifier) (\s@BatchJobExecutionSummary' {} a -> s {batchJobIdentifier = a} :: BatchJobExecutionSummary)

-- | The timestamp when this batch job execution ended.
batchJobExecutionSummary_endTime :: Lens.Lens' BatchJobExecutionSummary (Prelude.Maybe Prelude.UTCTime)
batchJobExecutionSummary_endTime = Lens.lens (\BatchJobExecutionSummary' {endTime} -> endTime) (\s@BatchJobExecutionSummary' {} a -> s {endTime = a} :: BatchJobExecutionSummary) Prelude.. Lens.mapping Data._Time

-- | The unique identifier of a particular batch job.
batchJobExecutionSummary_jobId :: Lens.Lens' BatchJobExecutionSummary (Prelude.Maybe Prelude.Text)
batchJobExecutionSummary_jobId = Lens.lens (\BatchJobExecutionSummary' {jobId} -> jobId) (\s@BatchJobExecutionSummary' {} a -> s {jobId = a} :: BatchJobExecutionSummary)

-- | The name of a particular batch job.
batchJobExecutionSummary_jobName :: Lens.Lens' BatchJobExecutionSummary (Prelude.Maybe Prelude.Text)
batchJobExecutionSummary_jobName = Lens.lens (\BatchJobExecutionSummary' {jobName} -> jobName) (\s@BatchJobExecutionSummary' {} a -> s {jobName = a} :: BatchJobExecutionSummary)

-- | The type of a particular batch job execution.
batchJobExecutionSummary_jobType :: Lens.Lens' BatchJobExecutionSummary (Prelude.Maybe BatchJobType)
batchJobExecutionSummary_jobType = Lens.lens (\BatchJobExecutionSummary' {jobType} -> jobType) (\s@BatchJobExecutionSummary' {} a -> s {jobType = a} :: BatchJobExecutionSummary)

-- | The batch job return code from either the Blu Age or Micro Focus runtime
-- engines. For more information, see
-- <https://www.ibm.com/docs/en/was/8.5.5?topic=model-batch-return-codes Batch return codes>
-- in the /IBM WebSphere Application Server/ documentation.
batchJobExecutionSummary_returnCode :: Lens.Lens' BatchJobExecutionSummary (Prelude.Maybe Prelude.Text)
batchJobExecutionSummary_returnCode = Lens.lens (\BatchJobExecutionSummary' {returnCode} -> returnCode) (\s@BatchJobExecutionSummary' {} a -> s {returnCode = a} :: BatchJobExecutionSummary)

-- | The unique identifier of the application that hosts this batch job.
batchJobExecutionSummary_applicationId :: Lens.Lens' BatchJobExecutionSummary Prelude.Text
batchJobExecutionSummary_applicationId = Lens.lens (\BatchJobExecutionSummary' {applicationId} -> applicationId) (\s@BatchJobExecutionSummary' {} a -> s {applicationId = a} :: BatchJobExecutionSummary)

-- | The unique identifier of this execution of the batch job.
batchJobExecutionSummary_executionId :: Lens.Lens' BatchJobExecutionSummary Prelude.Text
batchJobExecutionSummary_executionId = Lens.lens (\BatchJobExecutionSummary' {executionId} -> executionId) (\s@BatchJobExecutionSummary' {} a -> s {executionId = a} :: BatchJobExecutionSummary)

-- | The timestamp when a particular batch job execution started.
batchJobExecutionSummary_startTime :: Lens.Lens' BatchJobExecutionSummary Prelude.UTCTime
batchJobExecutionSummary_startTime = Lens.lens (\BatchJobExecutionSummary' {startTime} -> startTime) (\s@BatchJobExecutionSummary' {} a -> s {startTime = a} :: BatchJobExecutionSummary) Prelude.. Data._Time

-- | The status of a particular batch job execution.
batchJobExecutionSummary_status :: Lens.Lens' BatchJobExecutionSummary BatchJobExecutionStatus
batchJobExecutionSummary_status = Lens.lens (\BatchJobExecutionSummary' {status} -> status) (\s@BatchJobExecutionSummary' {} a -> s {status = a} :: BatchJobExecutionSummary)

instance Data.FromJSON BatchJobExecutionSummary where
  parseJSON =
    Data.withObject
      "BatchJobExecutionSummary"
      ( \x ->
          BatchJobExecutionSummary'
            Prelude.<$> (x Data..:? "batchJobIdentifier")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "jobId")
            Prelude.<*> (x Data..:? "jobName")
            Prelude.<*> (x Data..:? "jobType")
            Prelude.<*> (x Data..:? "returnCode")
            Prelude.<*> (x Data..: "applicationId")
            Prelude.<*> (x Data..: "executionId")
            Prelude.<*> (x Data..: "startTime")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable BatchJobExecutionSummary where
  hashWithSalt _salt BatchJobExecutionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` batchJobIdentifier
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobType
      `Prelude.hashWithSalt` returnCode
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` executionId
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData BatchJobExecutionSummary where
  rnf BatchJobExecutionSummary' {..} =
    Prelude.rnf batchJobIdentifier
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf returnCode
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf executionId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
