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
-- Module      : Network.AWS.Glue.Types.BatchStopJobRunSuccessfulSubmission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BatchStopJobRunSuccessfulSubmission where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Records a successful request to stop a specified @JobRun@.
--
-- /See:/ 'newBatchStopJobRunSuccessfulSubmission' smart constructor.
data BatchStopJobRunSuccessfulSubmission = BatchStopJobRunSuccessfulSubmission'
  { -- | The @JobRunId@ of the job run that was stopped.
    jobRunId :: Core.Maybe Core.Text,
    -- | The name of the job definition used in the job run that was stopped.
    jobName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchStopJobRunSuccessfulSubmission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobRunId', 'batchStopJobRunSuccessfulSubmission_jobRunId' - The @JobRunId@ of the job run that was stopped.
--
-- 'jobName', 'batchStopJobRunSuccessfulSubmission_jobName' - The name of the job definition used in the job run that was stopped.
newBatchStopJobRunSuccessfulSubmission ::
  BatchStopJobRunSuccessfulSubmission
newBatchStopJobRunSuccessfulSubmission =
  BatchStopJobRunSuccessfulSubmission'
    { jobRunId =
        Core.Nothing,
      jobName = Core.Nothing
    }

-- | The @JobRunId@ of the job run that was stopped.
batchStopJobRunSuccessfulSubmission_jobRunId :: Lens.Lens' BatchStopJobRunSuccessfulSubmission (Core.Maybe Core.Text)
batchStopJobRunSuccessfulSubmission_jobRunId = Lens.lens (\BatchStopJobRunSuccessfulSubmission' {jobRunId} -> jobRunId) (\s@BatchStopJobRunSuccessfulSubmission' {} a -> s {jobRunId = a} :: BatchStopJobRunSuccessfulSubmission)

-- | The name of the job definition used in the job run that was stopped.
batchStopJobRunSuccessfulSubmission_jobName :: Lens.Lens' BatchStopJobRunSuccessfulSubmission (Core.Maybe Core.Text)
batchStopJobRunSuccessfulSubmission_jobName = Lens.lens (\BatchStopJobRunSuccessfulSubmission' {jobName} -> jobName) (\s@BatchStopJobRunSuccessfulSubmission' {} a -> s {jobName = a} :: BatchStopJobRunSuccessfulSubmission)

instance
  Core.FromJSON
    BatchStopJobRunSuccessfulSubmission
  where
  parseJSON =
    Core.withObject
      "BatchStopJobRunSuccessfulSubmission"
      ( \x ->
          BatchStopJobRunSuccessfulSubmission'
            Core.<$> (x Core..:? "JobRunId")
            Core.<*> (x Core..:? "JobName")
      )

instance
  Core.Hashable
    BatchStopJobRunSuccessfulSubmission

instance
  Core.NFData
    BatchStopJobRunSuccessfulSubmission
