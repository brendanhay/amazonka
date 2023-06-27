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
-- Module      : Amazonka.Personalize.Types.BatchSegmentJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.BatchSegmentJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A truncated version of the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_BatchSegmentJob.html BatchSegmentJob>
-- datatype.
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_ListBatchSegmentJobs.html ListBatchSegmentJobs>
-- operation returns a list of batch segment job summaries.
--
-- /See:/ 'newBatchSegmentJobSummary' smart constructor.
data BatchSegmentJobSummary = BatchSegmentJobSummary'
  { -- | The Amazon Resource Name (ARN) of the batch segment job.
    batchSegmentJobArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the batch segment job was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | If the batch segment job failed, the reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the batch segment job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The time at which the batch segment job was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
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
-- Create a value of 'BatchSegmentJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSegmentJobArn', 'batchSegmentJobSummary_batchSegmentJobArn' - The Amazon Resource Name (ARN) of the batch segment job.
--
-- 'creationDateTime', 'batchSegmentJobSummary_creationDateTime' - The time at which the batch segment job was created.
--
-- 'failureReason', 'batchSegmentJobSummary_failureReason' - If the batch segment job failed, the reason for the failure.
--
-- 'jobName', 'batchSegmentJobSummary_jobName' - The name of the batch segment job.
--
-- 'lastUpdatedDateTime', 'batchSegmentJobSummary_lastUpdatedDateTime' - The time at which the batch segment job was last updated.
--
-- 'solutionVersionArn', 'batchSegmentJobSummary_solutionVersionArn' - The Amazon Resource Name (ARN) of the solution version used by the batch
-- segment job to generate batch segments.
--
-- 'status', 'batchSegmentJobSummary_status' - The status of the batch segment job. The status is one of the following
-- values:
--
-- -   PENDING
--
-- -   IN PROGRESS
--
-- -   ACTIVE
--
-- -   CREATE FAILED
newBatchSegmentJobSummary ::
  BatchSegmentJobSummary
newBatchSegmentJobSummary =
  BatchSegmentJobSummary'
    { batchSegmentJobArn =
        Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      jobName = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      solutionVersionArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the batch segment job.
batchSegmentJobSummary_batchSegmentJobArn :: Lens.Lens' BatchSegmentJobSummary (Prelude.Maybe Prelude.Text)
batchSegmentJobSummary_batchSegmentJobArn = Lens.lens (\BatchSegmentJobSummary' {batchSegmentJobArn} -> batchSegmentJobArn) (\s@BatchSegmentJobSummary' {} a -> s {batchSegmentJobArn = a} :: BatchSegmentJobSummary)

-- | The time at which the batch segment job was created.
batchSegmentJobSummary_creationDateTime :: Lens.Lens' BatchSegmentJobSummary (Prelude.Maybe Prelude.UTCTime)
batchSegmentJobSummary_creationDateTime = Lens.lens (\BatchSegmentJobSummary' {creationDateTime} -> creationDateTime) (\s@BatchSegmentJobSummary' {} a -> s {creationDateTime = a} :: BatchSegmentJobSummary) Prelude.. Lens.mapping Data._Time

-- | If the batch segment job failed, the reason for the failure.
batchSegmentJobSummary_failureReason :: Lens.Lens' BatchSegmentJobSummary (Prelude.Maybe Prelude.Text)
batchSegmentJobSummary_failureReason = Lens.lens (\BatchSegmentJobSummary' {failureReason} -> failureReason) (\s@BatchSegmentJobSummary' {} a -> s {failureReason = a} :: BatchSegmentJobSummary)

-- | The name of the batch segment job.
batchSegmentJobSummary_jobName :: Lens.Lens' BatchSegmentJobSummary (Prelude.Maybe Prelude.Text)
batchSegmentJobSummary_jobName = Lens.lens (\BatchSegmentJobSummary' {jobName} -> jobName) (\s@BatchSegmentJobSummary' {} a -> s {jobName = a} :: BatchSegmentJobSummary)

-- | The time at which the batch segment job was last updated.
batchSegmentJobSummary_lastUpdatedDateTime :: Lens.Lens' BatchSegmentJobSummary (Prelude.Maybe Prelude.UTCTime)
batchSegmentJobSummary_lastUpdatedDateTime = Lens.lens (\BatchSegmentJobSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@BatchSegmentJobSummary' {} a -> s {lastUpdatedDateTime = a} :: BatchSegmentJobSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the solution version used by the batch
-- segment job to generate batch segments.
batchSegmentJobSummary_solutionVersionArn :: Lens.Lens' BatchSegmentJobSummary (Prelude.Maybe Prelude.Text)
batchSegmentJobSummary_solutionVersionArn = Lens.lens (\BatchSegmentJobSummary' {solutionVersionArn} -> solutionVersionArn) (\s@BatchSegmentJobSummary' {} a -> s {solutionVersionArn = a} :: BatchSegmentJobSummary)

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
batchSegmentJobSummary_status :: Lens.Lens' BatchSegmentJobSummary (Prelude.Maybe Prelude.Text)
batchSegmentJobSummary_status = Lens.lens (\BatchSegmentJobSummary' {status} -> status) (\s@BatchSegmentJobSummary' {} a -> s {status = a} :: BatchSegmentJobSummary)

instance Data.FromJSON BatchSegmentJobSummary where
  parseJSON =
    Data.withObject
      "BatchSegmentJobSummary"
      ( \x ->
          BatchSegmentJobSummary'
            Prelude.<$> (x Data..:? "batchSegmentJobArn")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "jobName")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "solutionVersionArn")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable BatchSegmentJobSummary where
  hashWithSalt _salt BatchSegmentJobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` batchSegmentJobArn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` solutionVersionArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData BatchSegmentJobSummary where
  rnf BatchSegmentJobSummary' {..} =
    Prelude.rnf batchSegmentJobArn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf solutionVersionArn
      `Prelude.seq` Prelude.rnf status
