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
-- Module      : Amazonka.FraudDetector.Types.BatchImport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.BatchImport where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.AsyncJobStatus
import qualified Amazonka.Prelude as Prelude

-- | The batch import job details.
--
-- /See:/ 'newBatchImport' smart constructor.
data BatchImport = BatchImport'
  { -- | The ARN of the batch import job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when batch import job completed.
    completionTime :: Prelude.Maybe Prelude.Text,
    -- | The name of the event type.
    eventTypeName :: Prelude.Maybe Prelude.Text,
    -- | The number of records that failed to import.
    failedRecordsCount :: Prelude.Maybe Prelude.Int,
    -- | The reason batch import job failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role to use for this job request.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location of your data file for batch import.
    inputPath :: Prelude.Maybe Prelude.Text,
    -- | The ID of the batch import job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location of your output file.
    outputPath :: Prelude.Maybe Prelude.Text,
    -- | The number of records processed by batch import job.
    processedRecordsCount :: Prelude.Maybe Prelude.Int,
    -- | Timestamp of when the batch import job started.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The status of the batch import job.
    status :: Prelude.Maybe AsyncJobStatus,
    -- | The total number of records in the batch import job.
    totalRecordsCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchImport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'batchImport_arn' - The ARN of the batch import job.
--
-- 'completionTime', 'batchImport_completionTime' - Timestamp of when batch import job completed.
--
-- 'eventTypeName', 'batchImport_eventTypeName' - The name of the event type.
--
-- 'failedRecordsCount', 'batchImport_failedRecordsCount' - The number of records that failed to import.
--
-- 'failureReason', 'batchImport_failureReason' - The reason batch import job failed.
--
-- 'iamRoleArn', 'batchImport_iamRoleArn' - The ARN of the IAM role to use for this job request.
--
-- 'inputPath', 'batchImport_inputPath' - The Amazon S3 location of your data file for batch import.
--
-- 'jobId', 'batchImport_jobId' - The ID of the batch import job.
--
-- 'outputPath', 'batchImport_outputPath' - The Amazon S3 location of your output file.
--
-- 'processedRecordsCount', 'batchImport_processedRecordsCount' - The number of records processed by batch import job.
--
-- 'startTime', 'batchImport_startTime' - Timestamp of when the batch import job started.
--
-- 'status', 'batchImport_status' - The status of the batch import job.
--
-- 'totalRecordsCount', 'batchImport_totalRecordsCount' - The total number of records in the batch import job.
newBatchImport ::
  BatchImport
newBatchImport =
  BatchImport'
    { arn = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      eventTypeName = Prelude.Nothing,
      failedRecordsCount = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      inputPath = Prelude.Nothing,
      jobId = Prelude.Nothing,
      outputPath = Prelude.Nothing,
      processedRecordsCount = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      totalRecordsCount = Prelude.Nothing
    }

-- | The ARN of the batch import job.
batchImport_arn :: Lens.Lens' BatchImport (Prelude.Maybe Prelude.Text)
batchImport_arn = Lens.lens (\BatchImport' {arn} -> arn) (\s@BatchImport' {} a -> s {arn = a} :: BatchImport)

-- | Timestamp of when batch import job completed.
batchImport_completionTime :: Lens.Lens' BatchImport (Prelude.Maybe Prelude.Text)
batchImport_completionTime = Lens.lens (\BatchImport' {completionTime} -> completionTime) (\s@BatchImport' {} a -> s {completionTime = a} :: BatchImport)

-- | The name of the event type.
batchImport_eventTypeName :: Lens.Lens' BatchImport (Prelude.Maybe Prelude.Text)
batchImport_eventTypeName = Lens.lens (\BatchImport' {eventTypeName} -> eventTypeName) (\s@BatchImport' {} a -> s {eventTypeName = a} :: BatchImport)

-- | The number of records that failed to import.
batchImport_failedRecordsCount :: Lens.Lens' BatchImport (Prelude.Maybe Prelude.Int)
batchImport_failedRecordsCount = Lens.lens (\BatchImport' {failedRecordsCount} -> failedRecordsCount) (\s@BatchImport' {} a -> s {failedRecordsCount = a} :: BatchImport)

-- | The reason batch import job failed.
batchImport_failureReason :: Lens.Lens' BatchImport (Prelude.Maybe Prelude.Text)
batchImport_failureReason = Lens.lens (\BatchImport' {failureReason} -> failureReason) (\s@BatchImport' {} a -> s {failureReason = a} :: BatchImport)

-- | The ARN of the IAM role to use for this job request.
batchImport_iamRoleArn :: Lens.Lens' BatchImport (Prelude.Maybe Prelude.Text)
batchImport_iamRoleArn = Lens.lens (\BatchImport' {iamRoleArn} -> iamRoleArn) (\s@BatchImport' {} a -> s {iamRoleArn = a} :: BatchImport)

-- | The Amazon S3 location of your data file for batch import.
batchImport_inputPath :: Lens.Lens' BatchImport (Prelude.Maybe Prelude.Text)
batchImport_inputPath = Lens.lens (\BatchImport' {inputPath} -> inputPath) (\s@BatchImport' {} a -> s {inputPath = a} :: BatchImport)

-- | The ID of the batch import job.
batchImport_jobId :: Lens.Lens' BatchImport (Prelude.Maybe Prelude.Text)
batchImport_jobId = Lens.lens (\BatchImport' {jobId} -> jobId) (\s@BatchImport' {} a -> s {jobId = a} :: BatchImport)

-- | The Amazon S3 location of your output file.
batchImport_outputPath :: Lens.Lens' BatchImport (Prelude.Maybe Prelude.Text)
batchImport_outputPath = Lens.lens (\BatchImport' {outputPath} -> outputPath) (\s@BatchImport' {} a -> s {outputPath = a} :: BatchImport)

-- | The number of records processed by batch import job.
batchImport_processedRecordsCount :: Lens.Lens' BatchImport (Prelude.Maybe Prelude.Int)
batchImport_processedRecordsCount = Lens.lens (\BatchImport' {processedRecordsCount} -> processedRecordsCount) (\s@BatchImport' {} a -> s {processedRecordsCount = a} :: BatchImport)

-- | Timestamp of when the batch import job started.
batchImport_startTime :: Lens.Lens' BatchImport (Prelude.Maybe Prelude.Text)
batchImport_startTime = Lens.lens (\BatchImport' {startTime} -> startTime) (\s@BatchImport' {} a -> s {startTime = a} :: BatchImport)

-- | The status of the batch import job.
batchImport_status :: Lens.Lens' BatchImport (Prelude.Maybe AsyncJobStatus)
batchImport_status = Lens.lens (\BatchImport' {status} -> status) (\s@BatchImport' {} a -> s {status = a} :: BatchImport)

-- | The total number of records in the batch import job.
batchImport_totalRecordsCount :: Lens.Lens' BatchImport (Prelude.Maybe Prelude.Int)
batchImport_totalRecordsCount = Lens.lens (\BatchImport' {totalRecordsCount} -> totalRecordsCount) (\s@BatchImport' {} a -> s {totalRecordsCount = a} :: BatchImport)

instance Data.FromJSON BatchImport where
  parseJSON =
    Data.withObject
      "BatchImport"
      ( \x ->
          BatchImport'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "completionTime")
            Prelude.<*> (x Data..:? "eventTypeName")
            Prelude.<*> (x Data..:? "failedRecordsCount")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "iamRoleArn")
            Prelude.<*> (x Data..:? "inputPath")
            Prelude.<*> (x Data..:? "jobId")
            Prelude.<*> (x Data..:? "outputPath")
            Prelude.<*> (x Data..:? "processedRecordsCount")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "totalRecordsCount")
      )

instance Prelude.Hashable BatchImport where
  hashWithSalt _salt BatchImport' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` eventTypeName
      `Prelude.hashWithSalt` failedRecordsCount
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` inputPath
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` outputPath
      `Prelude.hashWithSalt` processedRecordsCount
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` totalRecordsCount

instance Prelude.NFData BatchImport where
  rnf BatchImport' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf eventTypeName
      `Prelude.seq` Prelude.rnf failedRecordsCount
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf inputPath
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf outputPath
      `Prelude.seq` Prelude.rnf processedRecordsCount
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf totalRecordsCount
