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
-- Module      : Amazonka.Glue.Types.BatchStopJobRunSuccessfulSubmission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.BatchStopJobRunSuccessfulSubmission where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Records a successful request to stop a specified @JobRun@.
--
-- /See:/ 'newBatchStopJobRunSuccessfulSubmission' smart constructor.
data BatchStopJobRunSuccessfulSubmission = BatchStopJobRunSuccessfulSubmission'
  { -- | The name of the job definition used in the job run that was stopped.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The @JobRunId@ of the job run that was stopped.
    jobRunId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchStopJobRunSuccessfulSubmission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'batchStopJobRunSuccessfulSubmission_jobName' - The name of the job definition used in the job run that was stopped.
--
-- 'jobRunId', 'batchStopJobRunSuccessfulSubmission_jobRunId' - The @JobRunId@ of the job run that was stopped.
newBatchStopJobRunSuccessfulSubmission ::
  BatchStopJobRunSuccessfulSubmission
newBatchStopJobRunSuccessfulSubmission =
  BatchStopJobRunSuccessfulSubmission'
    { jobName =
        Prelude.Nothing,
      jobRunId = Prelude.Nothing
    }

-- | The name of the job definition used in the job run that was stopped.
batchStopJobRunSuccessfulSubmission_jobName :: Lens.Lens' BatchStopJobRunSuccessfulSubmission (Prelude.Maybe Prelude.Text)
batchStopJobRunSuccessfulSubmission_jobName = Lens.lens (\BatchStopJobRunSuccessfulSubmission' {jobName} -> jobName) (\s@BatchStopJobRunSuccessfulSubmission' {} a -> s {jobName = a} :: BatchStopJobRunSuccessfulSubmission)

-- | The @JobRunId@ of the job run that was stopped.
batchStopJobRunSuccessfulSubmission_jobRunId :: Lens.Lens' BatchStopJobRunSuccessfulSubmission (Prelude.Maybe Prelude.Text)
batchStopJobRunSuccessfulSubmission_jobRunId = Lens.lens (\BatchStopJobRunSuccessfulSubmission' {jobRunId} -> jobRunId) (\s@BatchStopJobRunSuccessfulSubmission' {} a -> s {jobRunId = a} :: BatchStopJobRunSuccessfulSubmission)

instance
  Data.FromJSON
    BatchStopJobRunSuccessfulSubmission
  where
  parseJSON =
    Data.withObject
      "BatchStopJobRunSuccessfulSubmission"
      ( \x ->
          BatchStopJobRunSuccessfulSubmission'
            Prelude.<$> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "JobRunId")
      )

instance
  Prelude.Hashable
    BatchStopJobRunSuccessfulSubmission
  where
  hashWithSalt
    _salt
    BatchStopJobRunSuccessfulSubmission' {..} =
      _salt `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` jobRunId

instance
  Prelude.NFData
    BatchStopJobRunSuccessfulSubmission
  where
  rnf BatchStopJobRunSuccessfulSubmission' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobRunId
