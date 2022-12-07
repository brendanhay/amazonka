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
-- Module      : Amazonka.FraudDetector.Types.BatchPrediction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.BatchPrediction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.AsyncJobStatus
import qualified Amazonka.Prelude as Prelude

-- | The batch prediction details.
--
-- /See:/ 'newBatchPrediction' smart constructor.
data BatchPrediction = BatchPrediction'
  { -- | The Amazon S3 location of your training file.
    inputPath :: Prelude.Maybe Prelude.Text,
    -- | The detector version.
    detectorVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of batch prediction job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The job ID for the batch prediction.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The name of the detector.
    detectorName :: Prelude.Maybe Prelude.Text,
    -- | The batch prediction status.
    status :: Prelude.Maybe AsyncJobStatus,
    -- | Timestamp of when the batch prediction job completed.
    completionTime :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location of your output file.
    outputPath :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role to use for this job request.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of most recent heartbeat indicating the batch prediction job
    -- was making progress.
    lastHeartbeatTime :: Prelude.Maybe Prelude.Text,
    -- | The total number of records in the batch prediction job.
    totalRecordsCount :: Prelude.Maybe Prelude.Int,
    -- | The name of the event type.
    eventTypeName :: Prelude.Maybe Prelude.Text,
    -- | The number of records processed by the batch prediction job.
    processedRecordsCount :: Prelude.Maybe Prelude.Int,
    -- | Timestamp of when the batch prediction job started.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The reason a batch prediction job failed.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPrediction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputPath', 'batchPrediction_inputPath' - The Amazon S3 location of your training file.
--
-- 'detectorVersion', 'batchPrediction_detectorVersion' - The detector version.
--
-- 'arn', 'batchPrediction_arn' - The ARN of batch prediction job.
--
-- 'jobId', 'batchPrediction_jobId' - The job ID for the batch prediction.
--
-- 'detectorName', 'batchPrediction_detectorName' - The name of the detector.
--
-- 'status', 'batchPrediction_status' - The batch prediction status.
--
-- 'completionTime', 'batchPrediction_completionTime' - Timestamp of when the batch prediction job completed.
--
-- 'outputPath', 'batchPrediction_outputPath' - The Amazon S3 location of your output file.
--
-- 'iamRoleArn', 'batchPrediction_iamRoleArn' - The ARN of the IAM role to use for this job request.
--
-- 'lastHeartbeatTime', 'batchPrediction_lastHeartbeatTime' - Timestamp of most recent heartbeat indicating the batch prediction job
-- was making progress.
--
-- 'totalRecordsCount', 'batchPrediction_totalRecordsCount' - The total number of records in the batch prediction job.
--
-- 'eventTypeName', 'batchPrediction_eventTypeName' - The name of the event type.
--
-- 'processedRecordsCount', 'batchPrediction_processedRecordsCount' - The number of records processed by the batch prediction job.
--
-- 'startTime', 'batchPrediction_startTime' - Timestamp of when the batch prediction job started.
--
-- 'failureReason', 'batchPrediction_failureReason' - The reason a batch prediction job failed.
newBatchPrediction ::
  BatchPrediction
newBatchPrediction =
  BatchPrediction'
    { inputPath = Prelude.Nothing,
      detectorVersion = Prelude.Nothing,
      arn = Prelude.Nothing,
      jobId = Prelude.Nothing,
      detectorName = Prelude.Nothing,
      status = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      outputPath = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      lastHeartbeatTime = Prelude.Nothing,
      totalRecordsCount = Prelude.Nothing,
      eventTypeName = Prelude.Nothing,
      processedRecordsCount = Prelude.Nothing,
      startTime = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The Amazon S3 location of your training file.
batchPrediction_inputPath :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_inputPath = Lens.lens (\BatchPrediction' {inputPath} -> inputPath) (\s@BatchPrediction' {} a -> s {inputPath = a} :: BatchPrediction)

-- | The detector version.
batchPrediction_detectorVersion :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_detectorVersion = Lens.lens (\BatchPrediction' {detectorVersion} -> detectorVersion) (\s@BatchPrediction' {} a -> s {detectorVersion = a} :: BatchPrediction)

-- | The ARN of batch prediction job.
batchPrediction_arn :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_arn = Lens.lens (\BatchPrediction' {arn} -> arn) (\s@BatchPrediction' {} a -> s {arn = a} :: BatchPrediction)

-- | The job ID for the batch prediction.
batchPrediction_jobId :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_jobId = Lens.lens (\BatchPrediction' {jobId} -> jobId) (\s@BatchPrediction' {} a -> s {jobId = a} :: BatchPrediction)

-- | The name of the detector.
batchPrediction_detectorName :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_detectorName = Lens.lens (\BatchPrediction' {detectorName} -> detectorName) (\s@BatchPrediction' {} a -> s {detectorName = a} :: BatchPrediction)

-- | The batch prediction status.
batchPrediction_status :: Lens.Lens' BatchPrediction (Prelude.Maybe AsyncJobStatus)
batchPrediction_status = Lens.lens (\BatchPrediction' {status} -> status) (\s@BatchPrediction' {} a -> s {status = a} :: BatchPrediction)

-- | Timestamp of when the batch prediction job completed.
batchPrediction_completionTime :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_completionTime = Lens.lens (\BatchPrediction' {completionTime} -> completionTime) (\s@BatchPrediction' {} a -> s {completionTime = a} :: BatchPrediction)

-- | The Amazon S3 location of your output file.
batchPrediction_outputPath :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_outputPath = Lens.lens (\BatchPrediction' {outputPath} -> outputPath) (\s@BatchPrediction' {} a -> s {outputPath = a} :: BatchPrediction)

-- | The ARN of the IAM role to use for this job request.
batchPrediction_iamRoleArn :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_iamRoleArn = Lens.lens (\BatchPrediction' {iamRoleArn} -> iamRoleArn) (\s@BatchPrediction' {} a -> s {iamRoleArn = a} :: BatchPrediction)

-- | Timestamp of most recent heartbeat indicating the batch prediction job
-- was making progress.
batchPrediction_lastHeartbeatTime :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_lastHeartbeatTime = Lens.lens (\BatchPrediction' {lastHeartbeatTime} -> lastHeartbeatTime) (\s@BatchPrediction' {} a -> s {lastHeartbeatTime = a} :: BatchPrediction)

-- | The total number of records in the batch prediction job.
batchPrediction_totalRecordsCount :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Int)
batchPrediction_totalRecordsCount = Lens.lens (\BatchPrediction' {totalRecordsCount} -> totalRecordsCount) (\s@BatchPrediction' {} a -> s {totalRecordsCount = a} :: BatchPrediction)

-- | The name of the event type.
batchPrediction_eventTypeName :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_eventTypeName = Lens.lens (\BatchPrediction' {eventTypeName} -> eventTypeName) (\s@BatchPrediction' {} a -> s {eventTypeName = a} :: BatchPrediction)

-- | The number of records processed by the batch prediction job.
batchPrediction_processedRecordsCount :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Int)
batchPrediction_processedRecordsCount = Lens.lens (\BatchPrediction' {processedRecordsCount} -> processedRecordsCount) (\s@BatchPrediction' {} a -> s {processedRecordsCount = a} :: BatchPrediction)

-- | Timestamp of when the batch prediction job started.
batchPrediction_startTime :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_startTime = Lens.lens (\BatchPrediction' {startTime} -> startTime) (\s@BatchPrediction' {} a -> s {startTime = a} :: BatchPrediction)

-- | The reason a batch prediction job failed.
batchPrediction_failureReason :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_failureReason = Lens.lens (\BatchPrediction' {failureReason} -> failureReason) (\s@BatchPrediction' {} a -> s {failureReason = a} :: BatchPrediction)

instance Data.FromJSON BatchPrediction where
  parseJSON =
    Data.withObject
      "BatchPrediction"
      ( \x ->
          BatchPrediction'
            Prelude.<$> (x Data..:? "inputPath")
            Prelude.<*> (x Data..:? "detectorVersion")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "jobId")
            Prelude.<*> (x Data..:? "detectorName")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "completionTime")
            Prelude.<*> (x Data..:? "outputPath")
            Prelude.<*> (x Data..:? "iamRoleArn")
            Prelude.<*> (x Data..:? "lastHeartbeatTime")
            Prelude.<*> (x Data..:? "totalRecordsCount")
            Prelude.<*> (x Data..:? "eventTypeName")
            Prelude.<*> (x Data..:? "processedRecordsCount")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "failureReason")
      )

instance Prelude.Hashable BatchPrediction where
  hashWithSalt _salt BatchPrediction' {..} =
    _salt `Prelude.hashWithSalt` inputPath
      `Prelude.hashWithSalt` detectorVersion
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` detectorName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` outputPath
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` lastHeartbeatTime
      `Prelude.hashWithSalt` totalRecordsCount
      `Prelude.hashWithSalt` eventTypeName
      `Prelude.hashWithSalt` processedRecordsCount
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData BatchPrediction where
  rnf BatchPrediction' {..} =
    Prelude.rnf inputPath
      `Prelude.seq` Prelude.rnf detectorVersion
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf detectorName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf outputPath
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf lastHeartbeatTime
      `Prelude.seq` Prelude.rnf totalRecordsCount
      `Prelude.seq` Prelude.rnf eventTypeName
      `Prelude.seq` Prelude.rnf processedRecordsCount
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf failureReason
