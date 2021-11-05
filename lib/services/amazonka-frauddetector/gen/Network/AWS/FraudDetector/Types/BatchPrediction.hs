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
-- Module      : Network.AWS.FraudDetector.Types.BatchPrediction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FraudDetector.Types.BatchPrediction where

import qualified Network.AWS.Core as Core
import Network.AWS.FraudDetector.Types.AsyncJobStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The batch prediction details.
--
-- /See:/ 'newBatchPrediction' smart constructor.
data BatchPrediction = BatchPrediction'
  { -- | The reason a batch prediction job failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role to use for this job request.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The batch prediction status.
    status :: Prelude.Maybe AsyncJobStatus,
    -- | The number of records processed by the batch prediction job.
    processedRecordsCount :: Prelude.Maybe Prelude.Int,
    -- | The total number of records in the batch prediction job.
    totalRecordsCount :: Prelude.Maybe Prelude.Int,
    -- | Timestamp of most recent heartbeat indicating the batch prediction job
    -- was making progress.
    lastHeartbeatTime :: Prelude.Maybe Prelude.Text,
    -- | The job ID for the batch prediction.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of batch prediction job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the batch prediction job started.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The name of the event type.
    eventTypeName :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the batch prediction job completed.
    completionTime :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location of your output file.
    outputPath :: Prelude.Maybe Prelude.Text,
    -- | The name of the detector.
    detectorName :: Prelude.Maybe Prelude.Text,
    -- | The detector version.
    detectorVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location of your training file.
    inputPath :: Prelude.Maybe Prelude.Text
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
-- 'failureReason', 'batchPrediction_failureReason' - The reason a batch prediction job failed.
--
-- 'iamRoleArn', 'batchPrediction_iamRoleArn' - The ARN of the IAM role to use for this job request.
--
-- 'status', 'batchPrediction_status' - The batch prediction status.
--
-- 'processedRecordsCount', 'batchPrediction_processedRecordsCount' - The number of records processed by the batch prediction job.
--
-- 'totalRecordsCount', 'batchPrediction_totalRecordsCount' - The total number of records in the batch prediction job.
--
-- 'lastHeartbeatTime', 'batchPrediction_lastHeartbeatTime' - Timestamp of most recent heartbeat indicating the batch prediction job
-- was making progress.
--
-- 'jobId', 'batchPrediction_jobId' - The job ID for the batch prediction.
--
-- 'arn', 'batchPrediction_arn' - The ARN of batch prediction job.
--
-- 'startTime', 'batchPrediction_startTime' - Timestamp of when the batch prediction job started.
--
-- 'eventTypeName', 'batchPrediction_eventTypeName' - The name of the event type.
--
-- 'completionTime', 'batchPrediction_completionTime' - Timestamp of when the batch prediction job completed.
--
-- 'outputPath', 'batchPrediction_outputPath' - The Amazon S3 location of your output file.
--
-- 'detectorName', 'batchPrediction_detectorName' - The name of the detector.
--
-- 'detectorVersion', 'batchPrediction_detectorVersion' - The detector version.
--
-- 'inputPath', 'batchPrediction_inputPath' - The Amazon S3 location of your training file.
newBatchPrediction ::
  BatchPrediction
newBatchPrediction =
  BatchPrediction'
    { failureReason = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      status = Prelude.Nothing,
      processedRecordsCount = Prelude.Nothing,
      totalRecordsCount = Prelude.Nothing,
      lastHeartbeatTime = Prelude.Nothing,
      jobId = Prelude.Nothing,
      arn = Prelude.Nothing,
      startTime = Prelude.Nothing,
      eventTypeName = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      outputPath = Prelude.Nothing,
      detectorName = Prelude.Nothing,
      detectorVersion = Prelude.Nothing,
      inputPath = Prelude.Nothing
    }

-- | The reason a batch prediction job failed.
batchPrediction_failureReason :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_failureReason = Lens.lens (\BatchPrediction' {failureReason} -> failureReason) (\s@BatchPrediction' {} a -> s {failureReason = a} :: BatchPrediction)

-- | The ARN of the IAM role to use for this job request.
batchPrediction_iamRoleArn :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_iamRoleArn = Lens.lens (\BatchPrediction' {iamRoleArn} -> iamRoleArn) (\s@BatchPrediction' {} a -> s {iamRoleArn = a} :: BatchPrediction)

-- | The batch prediction status.
batchPrediction_status :: Lens.Lens' BatchPrediction (Prelude.Maybe AsyncJobStatus)
batchPrediction_status = Lens.lens (\BatchPrediction' {status} -> status) (\s@BatchPrediction' {} a -> s {status = a} :: BatchPrediction)

-- | The number of records processed by the batch prediction job.
batchPrediction_processedRecordsCount :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Int)
batchPrediction_processedRecordsCount = Lens.lens (\BatchPrediction' {processedRecordsCount} -> processedRecordsCount) (\s@BatchPrediction' {} a -> s {processedRecordsCount = a} :: BatchPrediction)

-- | The total number of records in the batch prediction job.
batchPrediction_totalRecordsCount :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Int)
batchPrediction_totalRecordsCount = Lens.lens (\BatchPrediction' {totalRecordsCount} -> totalRecordsCount) (\s@BatchPrediction' {} a -> s {totalRecordsCount = a} :: BatchPrediction)

-- | Timestamp of most recent heartbeat indicating the batch prediction job
-- was making progress.
batchPrediction_lastHeartbeatTime :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_lastHeartbeatTime = Lens.lens (\BatchPrediction' {lastHeartbeatTime} -> lastHeartbeatTime) (\s@BatchPrediction' {} a -> s {lastHeartbeatTime = a} :: BatchPrediction)

-- | The job ID for the batch prediction.
batchPrediction_jobId :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_jobId = Lens.lens (\BatchPrediction' {jobId} -> jobId) (\s@BatchPrediction' {} a -> s {jobId = a} :: BatchPrediction)

-- | The ARN of batch prediction job.
batchPrediction_arn :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_arn = Lens.lens (\BatchPrediction' {arn} -> arn) (\s@BatchPrediction' {} a -> s {arn = a} :: BatchPrediction)

-- | Timestamp of when the batch prediction job started.
batchPrediction_startTime :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_startTime = Lens.lens (\BatchPrediction' {startTime} -> startTime) (\s@BatchPrediction' {} a -> s {startTime = a} :: BatchPrediction)

-- | The name of the event type.
batchPrediction_eventTypeName :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_eventTypeName = Lens.lens (\BatchPrediction' {eventTypeName} -> eventTypeName) (\s@BatchPrediction' {} a -> s {eventTypeName = a} :: BatchPrediction)

-- | Timestamp of when the batch prediction job completed.
batchPrediction_completionTime :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_completionTime = Lens.lens (\BatchPrediction' {completionTime} -> completionTime) (\s@BatchPrediction' {} a -> s {completionTime = a} :: BatchPrediction)

-- | The Amazon S3 location of your output file.
batchPrediction_outputPath :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_outputPath = Lens.lens (\BatchPrediction' {outputPath} -> outputPath) (\s@BatchPrediction' {} a -> s {outputPath = a} :: BatchPrediction)

-- | The name of the detector.
batchPrediction_detectorName :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_detectorName = Lens.lens (\BatchPrediction' {detectorName} -> detectorName) (\s@BatchPrediction' {} a -> s {detectorName = a} :: BatchPrediction)

-- | The detector version.
batchPrediction_detectorVersion :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_detectorVersion = Lens.lens (\BatchPrediction' {detectorVersion} -> detectorVersion) (\s@BatchPrediction' {} a -> s {detectorVersion = a} :: BatchPrediction)

-- | The Amazon S3 location of your training file.
batchPrediction_inputPath :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_inputPath = Lens.lens (\BatchPrediction' {inputPath} -> inputPath) (\s@BatchPrediction' {} a -> s {inputPath = a} :: BatchPrediction)

instance Core.FromJSON BatchPrediction where
  parseJSON =
    Core.withObject
      "BatchPrediction"
      ( \x ->
          BatchPrediction'
            Prelude.<$> (x Core..:? "failureReason")
            Prelude.<*> (x Core..:? "iamRoleArn")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "processedRecordsCount")
            Prelude.<*> (x Core..:? "totalRecordsCount")
            Prelude.<*> (x Core..:? "lastHeartbeatTime")
            Prelude.<*> (x Core..:? "jobId")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "startTime")
            Prelude.<*> (x Core..:? "eventTypeName")
            Prelude.<*> (x Core..:? "completionTime")
            Prelude.<*> (x Core..:? "outputPath")
            Prelude.<*> (x Core..:? "detectorName")
            Prelude.<*> (x Core..:? "detectorVersion")
            Prelude.<*> (x Core..:? "inputPath")
      )

instance Prelude.Hashable BatchPrediction

instance Prelude.NFData BatchPrediction
