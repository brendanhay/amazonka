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
-- Module      : Network.AWS.Personalize.Types.BatchInferenceJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Personalize.Types.BatchInferenceJobSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A truncated version of the BatchInferenceJob datatype. The
-- ListBatchInferenceJobs operation returns a list of batch inference job
-- summaries.
--
-- /See:/ 'newBatchInferenceJobSummary' smart constructor.
data BatchInferenceJobSummary = BatchInferenceJobSummary'
  { -- | If the batch inference job failed, the reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
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
    -- | The name of the batch inference job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The time at which the batch inference job was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the batch inference job.
    batchInferenceJobArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the batch inference job was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the solution version used by the batch inference job.
    solutionVersionArn :: Prelude.Maybe Prelude.Text
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
-- 'failureReason', 'batchInferenceJobSummary_failureReason' - If the batch inference job failed, the reason for the failure.
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
-- 'jobName', 'batchInferenceJobSummary_jobName' - The name of the batch inference job.
--
-- 'lastUpdatedDateTime', 'batchInferenceJobSummary_lastUpdatedDateTime' - The time at which the batch inference job was last updated.
--
-- 'batchInferenceJobArn', 'batchInferenceJobSummary_batchInferenceJobArn' - The Amazon Resource Name (ARN) of the batch inference job.
--
-- 'creationDateTime', 'batchInferenceJobSummary_creationDateTime' - The time at which the batch inference job was created.
--
-- 'solutionVersionArn', 'batchInferenceJobSummary_solutionVersionArn' - The ARN of the solution version used by the batch inference job.
newBatchInferenceJobSummary ::
  BatchInferenceJobSummary
newBatchInferenceJobSummary =
  BatchInferenceJobSummary'
    { failureReason =
        Prelude.Nothing,
      status = Prelude.Nothing,
      jobName = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      batchInferenceJobArn = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      solutionVersionArn = Prelude.Nothing
    }

-- | If the batch inference job failed, the reason for the failure.
batchInferenceJobSummary_failureReason :: Lens.Lens' BatchInferenceJobSummary (Prelude.Maybe Prelude.Text)
batchInferenceJobSummary_failureReason = Lens.lens (\BatchInferenceJobSummary' {failureReason} -> failureReason) (\s@BatchInferenceJobSummary' {} a -> s {failureReason = a} :: BatchInferenceJobSummary)

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

-- | The name of the batch inference job.
batchInferenceJobSummary_jobName :: Lens.Lens' BatchInferenceJobSummary (Prelude.Maybe Prelude.Text)
batchInferenceJobSummary_jobName = Lens.lens (\BatchInferenceJobSummary' {jobName} -> jobName) (\s@BatchInferenceJobSummary' {} a -> s {jobName = a} :: BatchInferenceJobSummary)

-- | The time at which the batch inference job was last updated.
batchInferenceJobSummary_lastUpdatedDateTime :: Lens.Lens' BatchInferenceJobSummary (Prelude.Maybe Prelude.UTCTime)
batchInferenceJobSummary_lastUpdatedDateTime = Lens.lens (\BatchInferenceJobSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@BatchInferenceJobSummary' {} a -> s {lastUpdatedDateTime = a} :: BatchInferenceJobSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the batch inference job.
batchInferenceJobSummary_batchInferenceJobArn :: Lens.Lens' BatchInferenceJobSummary (Prelude.Maybe Prelude.Text)
batchInferenceJobSummary_batchInferenceJobArn = Lens.lens (\BatchInferenceJobSummary' {batchInferenceJobArn} -> batchInferenceJobArn) (\s@BatchInferenceJobSummary' {} a -> s {batchInferenceJobArn = a} :: BatchInferenceJobSummary)

-- | The time at which the batch inference job was created.
batchInferenceJobSummary_creationDateTime :: Lens.Lens' BatchInferenceJobSummary (Prelude.Maybe Prelude.UTCTime)
batchInferenceJobSummary_creationDateTime = Lens.lens (\BatchInferenceJobSummary' {creationDateTime} -> creationDateTime) (\s@BatchInferenceJobSummary' {} a -> s {creationDateTime = a} :: BatchInferenceJobSummary) Prelude.. Lens.mapping Core._Time

-- | The ARN of the solution version used by the batch inference job.
batchInferenceJobSummary_solutionVersionArn :: Lens.Lens' BatchInferenceJobSummary (Prelude.Maybe Prelude.Text)
batchInferenceJobSummary_solutionVersionArn = Lens.lens (\BatchInferenceJobSummary' {solutionVersionArn} -> solutionVersionArn) (\s@BatchInferenceJobSummary' {} a -> s {solutionVersionArn = a} :: BatchInferenceJobSummary)

instance Core.FromJSON BatchInferenceJobSummary where
  parseJSON =
    Core.withObject
      "BatchInferenceJobSummary"
      ( \x ->
          BatchInferenceJobSummary'
            Prelude.<$> (x Core..:? "failureReason")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "jobName")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "batchInferenceJobArn")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "solutionVersionArn")
      )

instance Prelude.Hashable BatchInferenceJobSummary

instance Prelude.NFData BatchInferenceJobSummary
