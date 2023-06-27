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
-- Module      : Amazonka.Personalize.Types.BatchSegmentJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.BatchSegmentJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.BatchSegmentJobInput
import Amazonka.Personalize.Types.BatchSegmentJobOutput
import qualified Amazonka.Prelude as Prelude

-- | Contains information on a batch segment job.
--
-- /See:/ 'newBatchSegmentJob' smart constructor.
data BatchSegmentJob = BatchSegmentJob'
  { -- | The Amazon Resource Name (ARN) of the batch segment job.
    batchSegmentJobArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the batch segment job was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | If the batch segment job failed, the reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the filter used on the batch segment job.
    filterArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 path that leads to the input data used to generate the
    -- batch segment job.
    jobInput :: Prelude.Maybe BatchSegmentJobInput,
    -- | The name of the batch segment job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket that contains the output data generated by the
    -- batch segment job.
    jobOutput :: Prelude.Maybe BatchSegmentJobOutput,
    -- | The time at which the batch segment job last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The number of predicted users generated by the batch segment job for
    -- each line of input data. The maximum number of users per segment is 5
    -- million.
    numResults :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the Amazon Identity and Access Management (IAM) role that
    -- requested the batch segment job.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the solution version used by the batch
    -- segment job to generate batch segments.
    solutionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the batch segment job. The status is one of the following
    -- values:
    --
    -- -   PENDING
    --
    -- -   IN PROGRESS
    --
    -- -   ACTIVE
    --
    -- -   CREATE FAILED
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchSegmentJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSegmentJobArn', 'batchSegmentJob_batchSegmentJobArn' - The Amazon Resource Name (ARN) of the batch segment job.
--
-- 'creationDateTime', 'batchSegmentJob_creationDateTime' - The time at which the batch segment job was created.
--
-- 'failureReason', 'batchSegmentJob_failureReason' - If the batch segment job failed, the reason for the failure.
--
-- 'filterArn', 'batchSegmentJob_filterArn' - The ARN of the filter used on the batch segment job.
--
-- 'jobInput', 'batchSegmentJob_jobInput' - The Amazon S3 path that leads to the input data used to generate the
-- batch segment job.
--
-- 'jobName', 'batchSegmentJob_jobName' - The name of the batch segment job.
--
-- 'jobOutput', 'batchSegmentJob_jobOutput' - The Amazon S3 bucket that contains the output data generated by the
-- batch segment job.
--
-- 'lastUpdatedDateTime', 'batchSegmentJob_lastUpdatedDateTime' - The time at which the batch segment job last updated.
--
-- 'numResults', 'batchSegmentJob_numResults' - The number of predicted users generated by the batch segment job for
-- each line of input data. The maximum number of users per segment is 5
-- million.
--
-- 'roleArn', 'batchSegmentJob_roleArn' - The ARN of the Amazon Identity and Access Management (IAM) role that
-- requested the batch segment job.
--
-- 'solutionVersionArn', 'batchSegmentJob_solutionVersionArn' - The Amazon Resource Name (ARN) of the solution version used by the batch
-- segment job to generate batch segments.
--
-- 'status', 'batchSegmentJob_status' - The status of the batch segment job. The status is one of the following
-- values:
--
-- -   PENDING
--
-- -   IN PROGRESS
--
-- -   ACTIVE
--
-- -   CREATE FAILED
newBatchSegmentJob ::
  BatchSegmentJob
newBatchSegmentJob =
  BatchSegmentJob'
    { batchSegmentJobArn =
        Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      filterArn = Prelude.Nothing,
      jobInput = Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobOutput = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      numResults = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      solutionVersionArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the batch segment job.
batchSegmentJob_batchSegmentJobArn :: Lens.Lens' BatchSegmentJob (Prelude.Maybe Prelude.Text)
batchSegmentJob_batchSegmentJobArn = Lens.lens (\BatchSegmentJob' {batchSegmentJobArn} -> batchSegmentJobArn) (\s@BatchSegmentJob' {} a -> s {batchSegmentJobArn = a} :: BatchSegmentJob)

-- | The time at which the batch segment job was created.
batchSegmentJob_creationDateTime :: Lens.Lens' BatchSegmentJob (Prelude.Maybe Prelude.UTCTime)
batchSegmentJob_creationDateTime = Lens.lens (\BatchSegmentJob' {creationDateTime} -> creationDateTime) (\s@BatchSegmentJob' {} a -> s {creationDateTime = a} :: BatchSegmentJob) Prelude.. Lens.mapping Data._Time

-- | If the batch segment job failed, the reason for the failure.
batchSegmentJob_failureReason :: Lens.Lens' BatchSegmentJob (Prelude.Maybe Prelude.Text)
batchSegmentJob_failureReason = Lens.lens (\BatchSegmentJob' {failureReason} -> failureReason) (\s@BatchSegmentJob' {} a -> s {failureReason = a} :: BatchSegmentJob)

-- | The ARN of the filter used on the batch segment job.
batchSegmentJob_filterArn :: Lens.Lens' BatchSegmentJob (Prelude.Maybe Prelude.Text)
batchSegmentJob_filterArn = Lens.lens (\BatchSegmentJob' {filterArn} -> filterArn) (\s@BatchSegmentJob' {} a -> s {filterArn = a} :: BatchSegmentJob)

-- | The Amazon S3 path that leads to the input data used to generate the
-- batch segment job.
batchSegmentJob_jobInput :: Lens.Lens' BatchSegmentJob (Prelude.Maybe BatchSegmentJobInput)
batchSegmentJob_jobInput = Lens.lens (\BatchSegmentJob' {jobInput} -> jobInput) (\s@BatchSegmentJob' {} a -> s {jobInput = a} :: BatchSegmentJob)

-- | The name of the batch segment job.
batchSegmentJob_jobName :: Lens.Lens' BatchSegmentJob (Prelude.Maybe Prelude.Text)
batchSegmentJob_jobName = Lens.lens (\BatchSegmentJob' {jobName} -> jobName) (\s@BatchSegmentJob' {} a -> s {jobName = a} :: BatchSegmentJob)

-- | The Amazon S3 bucket that contains the output data generated by the
-- batch segment job.
batchSegmentJob_jobOutput :: Lens.Lens' BatchSegmentJob (Prelude.Maybe BatchSegmentJobOutput)
batchSegmentJob_jobOutput = Lens.lens (\BatchSegmentJob' {jobOutput} -> jobOutput) (\s@BatchSegmentJob' {} a -> s {jobOutput = a} :: BatchSegmentJob)

-- | The time at which the batch segment job last updated.
batchSegmentJob_lastUpdatedDateTime :: Lens.Lens' BatchSegmentJob (Prelude.Maybe Prelude.UTCTime)
batchSegmentJob_lastUpdatedDateTime = Lens.lens (\BatchSegmentJob' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@BatchSegmentJob' {} a -> s {lastUpdatedDateTime = a} :: BatchSegmentJob) Prelude.. Lens.mapping Data._Time

-- | The number of predicted users generated by the batch segment job for
-- each line of input data. The maximum number of users per segment is 5
-- million.
batchSegmentJob_numResults :: Lens.Lens' BatchSegmentJob (Prelude.Maybe Prelude.Int)
batchSegmentJob_numResults = Lens.lens (\BatchSegmentJob' {numResults} -> numResults) (\s@BatchSegmentJob' {} a -> s {numResults = a} :: BatchSegmentJob)

-- | The ARN of the Amazon Identity and Access Management (IAM) role that
-- requested the batch segment job.
batchSegmentJob_roleArn :: Lens.Lens' BatchSegmentJob (Prelude.Maybe Prelude.Text)
batchSegmentJob_roleArn = Lens.lens (\BatchSegmentJob' {roleArn} -> roleArn) (\s@BatchSegmentJob' {} a -> s {roleArn = a} :: BatchSegmentJob)

-- | The Amazon Resource Name (ARN) of the solution version used by the batch
-- segment job to generate batch segments.
batchSegmentJob_solutionVersionArn :: Lens.Lens' BatchSegmentJob (Prelude.Maybe Prelude.Text)
batchSegmentJob_solutionVersionArn = Lens.lens (\BatchSegmentJob' {solutionVersionArn} -> solutionVersionArn) (\s@BatchSegmentJob' {} a -> s {solutionVersionArn = a} :: BatchSegmentJob)

-- | The status of the batch segment job. The status is one of the following
-- values:
--
-- -   PENDING
--
-- -   IN PROGRESS
--
-- -   ACTIVE
--
-- -   CREATE FAILED
batchSegmentJob_status :: Lens.Lens' BatchSegmentJob (Prelude.Maybe Prelude.Text)
batchSegmentJob_status = Lens.lens (\BatchSegmentJob' {status} -> status) (\s@BatchSegmentJob' {} a -> s {status = a} :: BatchSegmentJob)

instance Data.FromJSON BatchSegmentJob where
  parseJSON =
    Data.withObject
      "BatchSegmentJob"
      ( \x ->
          BatchSegmentJob'
            Prelude.<$> (x Data..:? "batchSegmentJobArn")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "filterArn")
            Prelude.<*> (x Data..:? "jobInput")
            Prelude.<*> (x Data..:? "jobName")
            Prelude.<*> (x Data..:? "jobOutput")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "numResults")
            Prelude.<*> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "solutionVersionArn")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable BatchSegmentJob where
  hashWithSalt _salt BatchSegmentJob' {..} =
    _salt
      `Prelude.hashWithSalt` batchSegmentJobArn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` filterArn
      `Prelude.hashWithSalt` jobInput
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobOutput
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` numResults
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` solutionVersionArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData BatchSegmentJob where
  rnf BatchSegmentJob' {..} =
    Prelude.rnf batchSegmentJobArn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf filterArn
      `Prelude.seq` Prelude.rnf jobInput
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobOutput
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf numResults
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf solutionVersionArn
      `Prelude.seq` Prelude.rnf status
