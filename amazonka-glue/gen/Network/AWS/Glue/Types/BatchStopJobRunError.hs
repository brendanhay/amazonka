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
-- Module      : Network.AWS.Glue.Types.BatchStopJobRunError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BatchStopJobRunError where

import Network.AWS.Glue.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Records an error that occurred when attempting to stop a specified job
-- run.
--
-- /See:/ 'newBatchStopJobRunError' smart constructor.
data BatchStopJobRunError = BatchStopJobRunError'
  { -- | Specifies details about the error that was encountered.
    errorDetail :: Prelude.Maybe ErrorDetail,
    -- | The @JobRunId@ of the job run in question.
    jobRunId :: Prelude.Maybe Prelude.Text,
    -- | The name of the job definition that is used in the job run in question.
    jobName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchStopJobRunError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorDetail', 'batchStopJobRunError_errorDetail' - Specifies details about the error that was encountered.
--
-- 'jobRunId', 'batchStopJobRunError_jobRunId' - The @JobRunId@ of the job run in question.
--
-- 'jobName', 'batchStopJobRunError_jobName' - The name of the job definition that is used in the job run in question.
newBatchStopJobRunError ::
  BatchStopJobRunError
newBatchStopJobRunError =
  BatchStopJobRunError'
    { errorDetail =
        Prelude.Nothing,
      jobRunId = Prelude.Nothing,
      jobName = Prelude.Nothing
    }

-- | Specifies details about the error that was encountered.
batchStopJobRunError_errorDetail :: Lens.Lens' BatchStopJobRunError (Prelude.Maybe ErrorDetail)
batchStopJobRunError_errorDetail = Lens.lens (\BatchStopJobRunError' {errorDetail} -> errorDetail) (\s@BatchStopJobRunError' {} a -> s {errorDetail = a} :: BatchStopJobRunError)

-- | The @JobRunId@ of the job run in question.
batchStopJobRunError_jobRunId :: Lens.Lens' BatchStopJobRunError (Prelude.Maybe Prelude.Text)
batchStopJobRunError_jobRunId = Lens.lens (\BatchStopJobRunError' {jobRunId} -> jobRunId) (\s@BatchStopJobRunError' {} a -> s {jobRunId = a} :: BatchStopJobRunError)

-- | The name of the job definition that is used in the job run in question.
batchStopJobRunError_jobName :: Lens.Lens' BatchStopJobRunError (Prelude.Maybe Prelude.Text)
batchStopJobRunError_jobName = Lens.lens (\BatchStopJobRunError' {jobName} -> jobName) (\s@BatchStopJobRunError' {} a -> s {jobName = a} :: BatchStopJobRunError)

instance Prelude.FromJSON BatchStopJobRunError where
  parseJSON =
    Prelude.withObject
      "BatchStopJobRunError"
      ( \x ->
          BatchStopJobRunError'
            Prelude.<$> (x Prelude..:? "ErrorDetail")
            Prelude.<*> (x Prelude..:? "JobRunId")
            Prelude.<*> (x Prelude..:? "JobName")
      )

instance Prelude.Hashable BatchStopJobRunError

instance Prelude.NFData BatchStopJobRunError
