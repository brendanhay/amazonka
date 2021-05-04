{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Records a successful request to stop a specified @JobRun@.
--
-- /See:/ 'newBatchStopJobRunSuccessfulSubmission' smart constructor.
data BatchStopJobRunSuccessfulSubmission = BatchStopJobRunSuccessfulSubmission'
  { -- | The @JobRunId@ of the job run that was stopped.
    jobRunId :: Prelude.Maybe Prelude.Text,
    -- | The name of the job definition used in the job run that was stopped.
    jobName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      jobName = Prelude.Nothing
    }

-- | The @JobRunId@ of the job run that was stopped.
batchStopJobRunSuccessfulSubmission_jobRunId :: Lens.Lens' BatchStopJobRunSuccessfulSubmission (Prelude.Maybe Prelude.Text)
batchStopJobRunSuccessfulSubmission_jobRunId = Lens.lens (\BatchStopJobRunSuccessfulSubmission' {jobRunId} -> jobRunId) (\s@BatchStopJobRunSuccessfulSubmission' {} a -> s {jobRunId = a} :: BatchStopJobRunSuccessfulSubmission)

-- | The name of the job definition used in the job run that was stopped.
batchStopJobRunSuccessfulSubmission_jobName :: Lens.Lens' BatchStopJobRunSuccessfulSubmission (Prelude.Maybe Prelude.Text)
batchStopJobRunSuccessfulSubmission_jobName = Lens.lens (\BatchStopJobRunSuccessfulSubmission' {jobName} -> jobName) (\s@BatchStopJobRunSuccessfulSubmission' {} a -> s {jobName = a} :: BatchStopJobRunSuccessfulSubmission)

instance
  Prelude.FromJSON
    BatchStopJobRunSuccessfulSubmission
  where
  parseJSON =
    Prelude.withObject
      "BatchStopJobRunSuccessfulSubmission"
      ( \x ->
          BatchStopJobRunSuccessfulSubmission'
            Prelude.<$> (x Prelude..:? "JobRunId")
            Prelude.<*> (x Prelude..:? "JobName")
      )

instance
  Prelude.Hashable
    BatchStopJobRunSuccessfulSubmission

instance
  Prelude.NFData
    BatchStopJobRunSuccessfulSubmission
