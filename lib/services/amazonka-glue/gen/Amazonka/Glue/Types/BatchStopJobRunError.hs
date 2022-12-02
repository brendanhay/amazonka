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
-- Module      : Amazonka.Glue.Types.BatchStopJobRunError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.BatchStopJobRunError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.ErrorDetail
import qualified Amazonka.Prelude as Prelude

-- | Records an error that occurred when attempting to stop a specified job
-- run.
--
-- /See:/ 'newBatchStopJobRunError' smart constructor.
data BatchStopJobRunError = BatchStopJobRunError'
  { -- | The name of the job definition that is used in the job run in question.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The @JobRunId@ of the job run in question.
    jobRunId :: Prelude.Maybe Prelude.Text,
    -- | Specifies details about the error that was encountered.
    errorDetail :: Prelude.Maybe ErrorDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchStopJobRunError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'batchStopJobRunError_jobName' - The name of the job definition that is used in the job run in question.
--
-- 'jobRunId', 'batchStopJobRunError_jobRunId' - The @JobRunId@ of the job run in question.
--
-- 'errorDetail', 'batchStopJobRunError_errorDetail' - Specifies details about the error that was encountered.
newBatchStopJobRunError ::
  BatchStopJobRunError
newBatchStopJobRunError =
  BatchStopJobRunError'
    { jobName = Prelude.Nothing,
      jobRunId = Prelude.Nothing,
      errorDetail = Prelude.Nothing
    }

-- | The name of the job definition that is used in the job run in question.
batchStopJobRunError_jobName :: Lens.Lens' BatchStopJobRunError (Prelude.Maybe Prelude.Text)
batchStopJobRunError_jobName = Lens.lens (\BatchStopJobRunError' {jobName} -> jobName) (\s@BatchStopJobRunError' {} a -> s {jobName = a} :: BatchStopJobRunError)

-- | The @JobRunId@ of the job run in question.
batchStopJobRunError_jobRunId :: Lens.Lens' BatchStopJobRunError (Prelude.Maybe Prelude.Text)
batchStopJobRunError_jobRunId = Lens.lens (\BatchStopJobRunError' {jobRunId} -> jobRunId) (\s@BatchStopJobRunError' {} a -> s {jobRunId = a} :: BatchStopJobRunError)

-- | Specifies details about the error that was encountered.
batchStopJobRunError_errorDetail :: Lens.Lens' BatchStopJobRunError (Prelude.Maybe ErrorDetail)
batchStopJobRunError_errorDetail = Lens.lens (\BatchStopJobRunError' {errorDetail} -> errorDetail) (\s@BatchStopJobRunError' {} a -> s {errorDetail = a} :: BatchStopJobRunError)

instance Data.FromJSON BatchStopJobRunError where
  parseJSON =
    Data.withObject
      "BatchStopJobRunError"
      ( \x ->
          BatchStopJobRunError'
            Prelude.<$> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "JobRunId")
            Prelude.<*> (x Data..:? "ErrorDetail")
      )

instance Prelude.Hashable BatchStopJobRunError where
  hashWithSalt _salt BatchStopJobRunError' {..} =
    _salt `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobRunId
      `Prelude.hashWithSalt` errorDetail

instance Prelude.NFData BatchStopJobRunError where
  rnf BatchStopJobRunError' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobRunId
      `Prelude.seq` Prelude.rnf errorDetail
