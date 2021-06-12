{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchStopJobRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops one or more job runs for a specified job definition.
module Network.AWS.Glue.BatchStopJobRun
  ( -- * Creating a Request
    BatchStopJobRun (..),
    newBatchStopJobRun,

    -- * Request Lenses
    batchStopJobRun_jobName,
    batchStopJobRun_jobRunIds,

    -- * Destructuring the Response
    BatchStopJobRunResponse (..),
    newBatchStopJobRunResponse,

    -- * Response Lenses
    batchStopJobRunResponse_successfulSubmissions,
    batchStopJobRunResponse_errors,
    batchStopJobRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchStopJobRun' smart constructor.
data BatchStopJobRun = BatchStopJobRun'
  { -- | The name of the job definition for which to stop job runs.
    jobName :: Core.Text,
    -- | A list of the @JobRunIds@ that should be stopped for that job
    -- definition.
    jobRunIds :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchStopJobRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'batchStopJobRun_jobName' - The name of the job definition for which to stop job runs.
--
-- 'jobRunIds', 'batchStopJobRun_jobRunIds' - A list of the @JobRunIds@ that should be stopped for that job
-- definition.
newBatchStopJobRun ::
  -- | 'jobName'
  Core.Text ->
  -- | 'jobRunIds'
  Core.NonEmpty Core.Text ->
  BatchStopJobRun
newBatchStopJobRun pJobName_ pJobRunIds_ =
  BatchStopJobRun'
    { jobName = pJobName_,
      jobRunIds = Lens._Coerce Lens.# pJobRunIds_
    }

-- | The name of the job definition for which to stop job runs.
batchStopJobRun_jobName :: Lens.Lens' BatchStopJobRun Core.Text
batchStopJobRun_jobName = Lens.lens (\BatchStopJobRun' {jobName} -> jobName) (\s@BatchStopJobRun' {} a -> s {jobName = a} :: BatchStopJobRun)

-- | A list of the @JobRunIds@ that should be stopped for that job
-- definition.
batchStopJobRun_jobRunIds :: Lens.Lens' BatchStopJobRun (Core.NonEmpty Core.Text)
batchStopJobRun_jobRunIds = Lens.lens (\BatchStopJobRun' {jobRunIds} -> jobRunIds) (\s@BatchStopJobRun' {} a -> s {jobRunIds = a} :: BatchStopJobRun) Core.. Lens._Coerce

instance Core.AWSRequest BatchStopJobRun where
  type
    AWSResponse BatchStopJobRun =
      BatchStopJobRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchStopJobRunResponse'
            Core.<$> ( x Core..?> "SuccessfulSubmissions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "Errors" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchStopJobRun

instance Core.NFData BatchStopJobRun

instance Core.ToHeaders BatchStopJobRun where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.BatchStopJobRun" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchStopJobRun where
  toJSON BatchStopJobRun' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobName" Core..= jobName),
            Core.Just ("JobRunIds" Core..= jobRunIds)
          ]
      )

instance Core.ToPath BatchStopJobRun where
  toPath = Core.const "/"

instance Core.ToQuery BatchStopJobRun where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchStopJobRunResponse' smart constructor.
data BatchStopJobRunResponse = BatchStopJobRunResponse'
  { -- | A list of the JobRuns that were successfully submitted for stopping.
    successfulSubmissions :: Core.Maybe [BatchStopJobRunSuccessfulSubmission],
    -- | A list of the errors that were encountered in trying to stop @JobRuns@,
    -- including the @JobRunId@ for which each error was encountered and
    -- details about the error.
    errors :: Core.Maybe [BatchStopJobRunError],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchStopJobRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successfulSubmissions', 'batchStopJobRunResponse_successfulSubmissions' - A list of the JobRuns that were successfully submitted for stopping.
--
-- 'errors', 'batchStopJobRunResponse_errors' - A list of the errors that were encountered in trying to stop @JobRuns@,
-- including the @JobRunId@ for which each error was encountered and
-- details about the error.
--
-- 'httpStatus', 'batchStopJobRunResponse_httpStatus' - The response's http status code.
newBatchStopJobRunResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchStopJobRunResponse
newBatchStopJobRunResponse pHttpStatus_ =
  BatchStopJobRunResponse'
    { successfulSubmissions =
        Core.Nothing,
      errors = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the JobRuns that were successfully submitted for stopping.
batchStopJobRunResponse_successfulSubmissions :: Lens.Lens' BatchStopJobRunResponse (Core.Maybe [BatchStopJobRunSuccessfulSubmission])
batchStopJobRunResponse_successfulSubmissions = Lens.lens (\BatchStopJobRunResponse' {successfulSubmissions} -> successfulSubmissions) (\s@BatchStopJobRunResponse' {} a -> s {successfulSubmissions = a} :: BatchStopJobRunResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of the errors that were encountered in trying to stop @JobRuns@,
-- including the @JobRunId@ for which each error was encountered and
-- details about the error.
batchStopJobRunResponse_errors :: Lens.Lens' BatchStopJobRunResponse (Core.Maybe [BatchStopJobRunError])
batchStopJobRunResponse_errors = Lens.lens (\BatchStopJobRunResponse' {errors} -> errors) (\s@BatchStopJobRunResponse' {} a -> s {errors = a} :: BatchStopJobRunResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchStopJobRunResponse_httpStatus :: Lens.Lens' BatchStopJobRunResponse Core.Int
batchStopJobRunResponse_httpStatus = Lens.lens (\BatchStopJobRunResponse' {httpStatus} -> httpStatus) (\s@BatchStopJobRunResponse' {} a -> s {httpStatus = a} :: BatchStopJobRunResponse)

instance Core.NFData BatchStopJobRunResponse
