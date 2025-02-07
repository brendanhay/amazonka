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
-- Module      : Amazonka.Personalize.Types.BatchInferenceJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.BatchInferenceJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.BatchInferenceJobConfig
import Amazonka.Personalize.Types.BatchInferenceJobInput
import Amazonka.Personalize.Types.BatchInferenceJobOutput
import qualified Amazonka.Prelude as Prelude

-- | Contains information on a batch inference job.
--
-- /See:/ 'newBatchInferenceJob' smart constructor.
data BatchInferenceJob = BatchInferenceJob'
  { -- | The Amazon Resource Name (ARN) of the batch inference job.
    batchInferenceJobArn :: Prelude.Maybe Prelude.Text,
    -- | A string to string map of the configuration details of a batch inference
    -- job.
    batchInferenceJobConfig :: Prelude.Maybe BatchInferenceJobConfig,
    -- | The time at which the batch inference job was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | If the batch inference job failed, the reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the filter used on the batch inference job.
    filterArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 path that leads to the input data used to generate the
    -- batch inference job.
    jobInput :: Prelude.Maybe BatchInferenceJobInput,
    -- | The name of the batch inference job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket that contains the output data generated by the
    -- batch inference job.
    jobOutput :: Prelude.Maybe BatchInferenceJobOutput,
    -- | The time at which the batch inference job was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The number of recommendations generated by the batch inference job. This
    -- number includes the error messages generated for failed input records.
    numResults :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the Amazon Identity and Access Management (IAM) role that
    -- requested the batch inference job.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the solution version from which the
    -- batch inference job was created.
    solutionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the batch inference job. The status is one of the
    -- following values:
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
-- Create a value of 'BatchInferenceJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchInferenceJobArn', 'batchInferenceJob_batchInferenceJobArn' - The Amazon Resource Name (ARN) of the batch inference job.
--
-- 'batchInferenceJobConfig', 'batchInferenceJob_batchInferenceJobConfig' - A string to string map of the configuration details of a batch inference
-- job.
--
-- 'creationDateTime', 'batchInferenceJob_creationDateTime' - The time at which the batch inference job was created.
--
-- 'failureReason', 'batchInferenceJob_failureReason' - If the batch inference job failed, the reason for the failure.
--
-- 'filterArn', 'batchInferenceJob_filterArn' - The ARN of the filter used on the batch inference job.
--
-- 'jobInput', 'batchInferenceJob_jobInput' - The Amazon S3 path that leads to the input data used to generate the
-- batch inference job.
--
-- 'jobName', 'batchInferenceJob_jobName' - The name of the batch inference job.
--
-- 'jobOutput', 'batchInferenceJob_jobOutput' - The Amazon S3 bucket that contains the output data generated by the
-- batch inference job.
--
-- 'lastUpdatedDateTime', 'batchInferenceJob_lastUpdatedDateTime' - The time at which the batch inference job was last updated.
--
-- 'numResults', 'batchInferenceJob_numResults' - The number of recommendations generated by the batch inference job. This
-- number includes the error messages generated for failed input records.
--
-- 'roleArn', 'batchInferenceJob_roleArn' - The ARN of the Amazon Identity and Access Management (IAM) role that
-- requested the batch inference job.
--
-- 'solutionVersionArn', 'batchInferenceJob_solutionVersionArn' - The Amazon Resource Name (ARN) of the solution version from which the
-- batch inference job was created.
--
-- 'status', 'batchInferenceJob_status' - The status of the batch inference job. The status is one of the
-- following values:
--
-- -   PENDING
--
-- -   IN PROGRESS
--
-- -   ACTIVE
--
-- -   CREATE FAILED
newBatchInferenceJob ::
  BatchInferenceJob
newBatchInferenceJob =
  BatchInferenceJob'
    { batchInferenceJobArn =
        Prelude.Nothing,
      batchInferenceJobConfig = Prelude.Nothing,
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

-- | The Amazon Resource Name (ARN) of the batch inference job.
batchInferenceJob_batchInferenceJobArn :: Lens.Lens' BatchInferenceJob (Prelude.Maybe Prelude.Text)
batchInferenceJob_batchInferenceJobArn = Lens.lens (\BatchInferenceJob' {batchInferenceJobArn} -> batchInferenceJobArn) (\s@BatchInferenceJob' {} a -> s {batchInferenceJobArn = a} :: BatchInferenceJob)

-- | A string to string map of the configuration details of a batch inference
-- job.
batchInferenceJob_batchInferenceJobConfig :: Lens.Lens' BatchInferenceJob (Prelude.Maybe BatchInferenceJobConfig)
batchInferenceJob_batchInferenceJobConfig = Lens.lens (\BatchInferenceJob' {batchInferenceJobConfig} -> batchInferenceJobConfig) (\s@BatchInferenceJob' {} a -> s {batchInferenceJobConfig = a} :: BatchInferenceJob)

-- | The time at which the batch inference job was created.
batchInferenceJob_creationDateTime :: Lens.Lens' BatchInferenceJob (Prelude.Maybe Prelude.UTCTime)
batchInferenceJob_creationDateTime = Lens.lens (\BatchInferenceJob' {creationDateTime} -> creationDateTime) (\s@BatchInferenceJob' {} a -> s {creationDateTime = a} :: BatchInferenceJob) Prelude.. Lens.mapping Data._Time

-- | If the batch inference job failed, the reason for the failure.
batchInferenceJob_failureReason :: Lens.Lens' BatchInferenceJob (Prelude.Maybe Prelude.Text)
batchInferenceJob_failureReason = Lens.lens (\BatchInferenceJob' {failureReason} -> failureReason) (\s@BatchInferenceJob' {} a -> s {failureReason = a} :: BatchInferenceJob)

-- | The ARN of the filter used on the batch inference job.
batchInferenceJob_filterArn :: Lens.Lens' BatchInferenceJob (Prelude.Maybe Prelude.Text)
batchInferenceJob_filterArn = Lens.lens (\BatchInferenceJob' {filterArn} -> filterArn) (\s@BatchInferenceJob' {} a -> s {filterArn = a} :: BatchInferenceJob)

-- | The Amazon S3 path that leads to the input data used to generate the
-- batch inference job.
batchInferenceJob_jobInput :: Lens.Lens' BatchInferenceJob (Prelude.Maybe BatchInferenceJobInput)
batchInferenceJob_jobInput = Lens.lens (\BatchInferenceJob' {jobInput} -> jobInput) (\s@BatchInferenceJob' {} a -> s {jobInput = a} :: BatchInferenceJob)

-- | The name of the batch inference job.
batchInferenceJob_jobName :: Lens.Lens' BatchInferenceJob (Prelude.Maybe Prelude.Text)
batchInferenceJob_jobName = Lens.lens (\BatchInferenceJob' {jobName} -> jobName) (\s@BatchInferenceJob' {} a -> s {jobName = a} :: BatchInferenceJob)

-- | The Amazon S3 bucket that contains the output data generated by the
-- batch inference job.
batchInferenceJob_jobOutput :: Lens.Lens' BatchInferenceJob (Prelude.Maybe BatchInferenceJobOutput)
batchInferenceJob_jobOutput = Lens.lens (\BatchInferenceJob' {jobOutput} -> jobOutput) (\s@BatchInferenceJob' {} a -> s {jobOutput = a} :: BatchInferenceJob)

-- | The time at which the batch inference job was last updated.
batchInferenceJob_lastUpdatedDateTime :: Lens.Lens' BatchInferenceJob (Prelude.Maybe Prelude.UTCTime)
batchInferenceJob_lastUpdatedDateTime = Lens.lens (\BatchInferenceJob' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@BatchInferenceJob' {} a -> s {lastUpdatedDateTime = a} :: BatchInferenceJob) Prelude.. Lens.mapping Data._Time

-- | The number of recommendations generated by the batch inference job. This
-- number includes the error messages generated for failed input records.
batchInferenceJob_numResults :: Lens.Lens' BatchInferenceJob (Prelude.Maybe Prelude.Int)
batchInferenceJob_numResults = Lens.lens (\BatchInferenceJob' {numResults} -> numResults) (\s@BatchInferenceJob' {} a -> s {numResults = a} :: BatchInferenceJob)

-- | The ARN of the Amazon Identity and Access Management (IAM) role that
-- requested the batch inference job.
batchInferenceJob_roleArn :: Lens.Lens' BatchInferenceJob (Prelude.Maybe Prelude.Text)
batchInferenceJob_roleArn = Lens.lens (\BatchInferenceJob' {roleArn} -> roleArn) (\s@BatchInferenceJob' {} a -> s {roleArn = a} :: BatchInferenceJob)

-- | The Amazon Resource Name (ARN) of the solution version from which the
-- batch inference job was created.
batchInferenceJob_solutionVersionArn :: Lens.Lens' BatchInferenceJob (Prelude.Maybe Prelude.Text)
batchInferenceJob_solutionVersionArn = Lens.lens (\BatchInferenceJob' {solutionVersionArn} -> solutionVersionArn) (\s@BatchInferenceJob' {} a -> s {solutionVersionArn = a} :: BatchInferenceJob)

-- | The status of the batch inference job. The status is one of the
-- following values:
--
-- -   PENDING
--
-- -   IN PROGRESS
--
-- -   ACTIVE
--
-- -   CREATE FAILED
batchInferenceJob_status :: Lens.Lens' BatchInferenceJob (Prelude.Maybe Prelude.Text)
batchInferenceJob_status = Lens.lens (\BatchInferenceJob' {status} -> status) (\s@BatchInferenceJob' {} a -> s {status = a} :: BatchInferenceJob)

instance Data.FromJSON BatchInferenceJob where
  parseJSON =
    Data.withObject
      "BatchInferenceJob"
      ( \x ->
          BatchInferenceJob'
            Prelude.<$> (x Data..:? "batchInferenceJobArn")
            Prelude.<*> (x Data..:? "batchInferenceJobConfig")
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

instance Prelude.Hashable BatchInferenceJob where
  hashWithSalt _salt BatchInferenceJob' {..} =
    _salt
      `Prelude.hashWithSalt` batchInferenceJobArn
      `Prelude.hashWithSalt` batchInferenceJobConfig
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

instance Prelude.NFData BatchInferenceJob where
  rnf BatchInferenceJob' {..} =
    Prelude.rnf batchInferenceJobArn `Prelude.seq`
      Prelude.rnf batchInferenceJobConfig `Prelude.seq`
        Prelude.rnf creationDateTime `Prelude.seq`
          Prelude.rnf failureReason `Prelude.seq`
            Prelude.rnf filterArn `Prelude.seq`
              Prelude.rnf jobInput `Prelude.seq`
                Prelude.rnf jobName `Prelude.seq`
                  Prelude.rnf jobOutput `Prelude.seq`
                    Prelude.rnf lastUpdatedDateTime `Prelude.seq`
                      Prelude.rnf numResults `Prelude.seq`
                        Prelude.rnf roleArn `Prelude.seq`
                          Prelude.rnf solutionVersionArn `Prelude.seq`
                            Prelude.rnf status
