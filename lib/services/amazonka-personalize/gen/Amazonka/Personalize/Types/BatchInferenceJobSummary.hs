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
-- Module      : Amazonka.Personalize.Types.BatchInferenceJobSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.BatchInferenceJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A truncated version of the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_BatchInferenceJob.html BatchInferenceJob>.
-- The
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_ListBatchInferenceJobs.html ListBatchInferenceJobs>
-- operation returns a list of batch inference job summaries.
--
-- /See:/ 'newBatchInferenceJobSummary' smart constructor.
data BatchInferenceJobSummary = BatchInferenceJobSummary'
  { -- | The time at which the batch inference job was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the batch inference job.
    jobName :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the batch inference job.
    batchInferenceJobArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the solution version used by the batch inference job.
    solutionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the batch inference job was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | If the batch inference job failed, the reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchInferenceJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'batchInferenceJobSummary_creationDateTime' - The time at which the batch inference job was created.
--
-- 'jobName', 'batchInferenceJobSummary_jobName' - The name of the batch inference job.
--
-- 'status', 'batchInferenceJobSummary_status' - The status of the batch inference job. The status is one of the
-- following values:
--
-- -   PENDING
--
-- -   IN PROGRESS
--
-- -   ACTIVE
--
-- -   CREATE FAILED
--
-- 'batchInferenceJobArn', 'batchInferenceJobSummary_batchInferenceJobArn' - The Amazon Resource Name (ARN) of the batch inference job.
--
-- 'solutionVersionArn', 'batchInferenceJobSummary_solutionVersionArn' - The ARN of the solution version used by the batch inference job.
--
-- 'lastUpdatedDateTime', 'batchInferenceJobSummary_lastUpdatedDateTime' - The time at which the batch inference job was last updated.
--
-- 'failureReason', 'batchInferenceJobSummary_failureReason' - If the batch inference job failed, the reason for the failure.
newBatchInferenceJobSummary ::
  BatchInferenceJobSummary
newBatchInferenceJobSummary =
  BatchInferenceJobSummary'
    { creationDateTime =
        Prelude.Nothing,
      jobName = Prelude.Nothing,
      status = Prelude.Nothing,
      batchInferenceJobArn = Prelude.Nothing,
      solutionVersionArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The time at which the batch inference job was created.
batchInferenceJobSummary_creationDateTime :: Lens.Lens' BatchInferenceJobSummary (Prelude.Maybe Prelude.UTCTime)
batchInferenceJobSummary_creationDateTime = Lens.lens (\BatchInferenceJobSummary' {creationDateTime} -> creationDateTime) (\s@BatchInferenceJobSummary' {} a -> s {creationDateTime = a} :: BatchInferenceJobSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the batch inference job.
batchInferenceJobSummary_jobName :: Lens.Lens' BatchInferenceJobSummary (Prelude.Maybe Prelude.Text)
batchInferenceJobSummary_jobName = Lens.lens (\BatchInferenceJobSummary' {jobName} -> jobName) (\s@BatchInferenceJobSummary' {} a -> s {jobName = a} :: BatchInferenceJobSummary)

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
batchInferenceJobSummary_status :: Lens.Lens' BatchInferenceJobSummary (Prelude.Maybe Prelude.Text)
batchInferenceJobSummary_status = Lens.lens (\BatchInferenceJobSummary' {status} -> status) (\s@BatchInferenceJobSummary' {} a -> s {status = a} :: BatchInferenceJobSummary)

-- | The Amazon Resource Name (ARN) of the batch inference job.
batchInferenceJobSummary_batchInferenceJobArn :: Lens.Lens' BatchInferenceJobSummary (Prelude.Maybe Prelude.Text)
batchInferenceJobSummary_batchInferenceJobArn = Lens.lens (\BatchInferenceJobSummary' {batchInferenceJobArn} -> batchInferenceJobArn) (\s@BatchInferenceJobSummary' {} a -> s {batchInferenceJobArn = a} :: BatchInferenceJobSummary)

-- | The ARN of the solution version used by the batch inference job.
batchInferenceJobSummary_solutionVersionArn :: Lens.Lens' BatchInferenceJobSummary (Prelude.Maybe Prelude.Text)
batchInferenceJobSummary_solutionVersionArn = Lens.lens (\BatchInferenceJobSummary' {solutionVersionArn} -> solutionVersionArn) (\s@BatchInferenceJobSummary' {} a -> s {solutionVersionArn = a} :: BatchInferenceJobSummary)

-- | The time at which the batch inference job was last updated.
batchInferenceJobSummary_lastUpdatedDateTime :: Lens.Lens' BatchInferenceJobSummary (Prelude.Maybe Prelude.UTCTime)
batchInferenceJobSummary_lastUpdatedDateTime = Lens.lens (\BatchInferenceJobSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@BatchInferenceJobSummary' {} a -> s {lastUpdatedDateTime = a} :: BatchInferenceJobSummary) Prelude.. Lens.mapping Data._Time

-- | If the batch inference job failed, the reason for the failure.
batchInferenceJobSummary_failureReason :: Lens.Lens' BatchInferenceJobSummary (Prelude.Maybe Prelude.Text)
batchInferenceJobSummary_failureReason = Lens.lens (\BatchInferenceJobSummary' {failureReason} -> failureReason) (\s@BatchInferenceJobSummary' {} a -> s {failureReason = a} :: BatchInferenceJobSummary)

instance Data.FromJSON BatchInferenceJobSummary where
  parseJSON =
    Data.withObject
      "BatchInferenceJobSummary"
      ( \x ->
          BatchInferenceJobSummary'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "jobName")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "batchInferenceJobArn")
            Prelude.<*> (x Data..:? "solutionVersionArn")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "failureReason")
      )

instance Prelude.Hashable BatchInferenceJobSummary where
  hashWithSalt _salt BatchInferenceJobSummary' {..} =
    _salt `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` batchInferenceJobArn
      `Prelude.hashWithSalt` solutionVersionArn
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData BatchInferenceJobSummary where
  rnf BatchInferenceJobSummary' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf batchInferenceJobArn
      `Prelude.seq` Prelude.rnf solutionVersionArn
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf failureReason
