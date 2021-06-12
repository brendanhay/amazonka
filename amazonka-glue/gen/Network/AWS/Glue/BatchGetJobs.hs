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
-- Module      : Network.AWS.Glue.BatchGetJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of job names. After
-- calling the @ListJobs@ operation, you can call this operation to access
-- the data to which you have been granted permissions. This operation
-- supports all IAM permissions, including permission conditions that uses
-- tags.
module Network.AWS.Glue.BatchGetJobs
  ( -- * Creating a Request
    BatchGetJobs (..),
    newBatchGetJobs,

    -- * Request Lenses
    batchGetJobs_jobNames,

    -- * Destructuring the Response
    BatchGetJobsResponse (..),
    newBatchGetJobsResponse,

    -- * Response Lenses
    batchGetJobsResponse_jobsNotFound,
    batchGetJobsResponse_jobs,
    batchGetJobsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetJobs' smart constructor.
data BatchGetJobs = BatchGetJobs'
  { -- | A list of job names, which might be the names returned from the
    -- @ListJobs@ operation.
    jobNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobNames', 'batchGetJobs_jobNames' - A list of job names, which might be the names returned from the
-- @ListJobs@ operation.
newBatchGetJobs ::
  BatchGetJobs
newBatchGetJobs =
  BatchGetJobs' {jobNames = Core.mempty}

-- | A list of job names, which might be the names returned from the
-- @ListJobs@ operation.
batchGetJobs_jobNames :: Lens.Lens' BatchGetJobs [Core.Text]
batchGetJobs_jobNames = Lens.lens (\BatchGetJobs' {jobNames} -> jobNames) (\s@BatchGetJobs' {} a -> s {jobNames = a} :: BatchGetJobs) Core.. Lens._Coerce

instance Core.AWSRequest BatchGetJobs where
  type AWSResponse BatchGetJobs = BatchGetJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetJobsResponse'
            Core.<$> (x Core..?> "JobsNotFound" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Jobs" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchGetJobs

instance Core.NFData BatchGetJobs

instance Core.ToHeaders BatchGetJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.BatchGetJobs" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchGetJobs where
  toJSON BatchGetJobs' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("JobNames" Core..= jobNames)]
      )

instance Core.ToPath BatchGetJobs where
  toPath = Core.const "/"

instance Core.ToQuery BatchGetJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchGetJobsResponse' smart constructor.
data BatchGetJobsResponse = BatchGetJobsResponse'
  { -- | A list of names of jobs not found.
    jobsNotFound :: Core.Maybe [Core.Text],
    -- | A list of job definitions.
    jobs :: Core.Maybe [Job],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobsNotFound', 'batchGetJobsResponse_jobsNotFound' - A list of names of jobs not found.
--
-- 'jobs', 'batchGetJobsResponse_jobs' - A list of job definitions.
--
-- 'httpStatus', 'batchGetJobsResponse_httpStatus' - The response's http status code.
newBatchGetJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchGetJobsResponse
newBatchGetJobsResponse pHttpStatus_ =
  BatchGetJobsResponse'
    { jobsNotFound = Core.Nothing,
      jobs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of names of jobs not found.
batchGetJobsResponse_jobsNotFound :: Lens.Lens' BatchGetJobsResponse (Core.Maybe [Core.Text])
batchGetJobsResponse_jobsNotFound = Lens.lens (\BatchGetJobsResponse' {jobsNotFound} -> jobsNotFound) (\s@BatchGetJobsResponse' {} a -> s {jobsNotFound = a} :: BatchGetJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of job definitions.
batchGetJobsResponse_jobs :: Lens.Lens' BatchGetJobsResponse (Core.Maybe [Job])
batchGetJobsResponse_jobs = Lens.lens (\BatchGetJobsResponse' {jobs} -> jobs) (\s@BatchGetJobsResponse' {} a -> s {jobs = a} :: BatchGetJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetJobsResponse_httpStatus :: Lens.Lens' BatchGetJobsResponse Core.Int
batchGetJobsResponse_httpStatus = Lens.lens (\BatchGetJobsResponse' {httpStatus} -> httpStatus) (\s@BatchGetJobsResponse' {} a -> s {httpStatus = a} :: BatchGetJobsResponse)

instance Core.NFData BatchGetJobsResponse
