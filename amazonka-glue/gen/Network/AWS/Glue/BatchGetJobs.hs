{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetJobs' smart constructor.
data BatchGetJobs = BatchGetJobs'
  { -- | A list of job names, which might be the names returned from the
    -- @ListJobs@ operation.
    jobNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  BatchGetJobs' {jobNames = Prelude.mempty}

-- | A list of job names, which might be the names returned from the
-- @ListJobs@ operation.
batchGetJobs_jobNames :: Lens.Lens' BatchGetJobs [Prelude.Text]
batchGetJobs_jobNames = Lens.lens (\BatchGetJobs' {jobNames} -> jobNames) (\s@BatchGetJobs' {} a -> s {jobNames = a} :: BatchGetJobs) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest BatchGetJobs where
  type Rs BatchGetJobs = BatchGetJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetJobsResponse'
            Prelude.<$> ( x Prelude..?> "JobsNotFound"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "Jobs" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetJobs

instance Prelude.NFData BatchGetJobs

instance Prelude.ToHeaders BatchGetJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.BatchGetJobs" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON BatchGetJobs where
  toJSON BatchGetJobs' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobNames" Prelude..= jobNames)]
      )

instance Prelude.ToPath BatchGetJobs where
  toPath = Prelude.const "/"

instance Prelude.ToQuery BatchGetJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetJobsResponse' smart constructor.
data BatchGetJobsResponse = BatchGetJobsResponse'
  { -- | A list of names of jobs not found.
    jobsNotFound :: Prelude.Maybe [Prelude.Text],
    -- | A list of job definitions.
    jobs :: Prelude.Maybe [Job],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  BatchGetJobsResponse
newBatchGetJobsResponse pHttpStatus_ =
  BatchGetJobsResponse'
    { jobsNotFound =
        Prelude.Nothing,
      jobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of names of jobs not found.
batchGetJobsResponse_jobsNotFound :: Lens.Lens' BatchGetJobsResponse (Prelude.Maybe [Prelude.Text])
batchGetJobsResponse_jobsNotFound = Lens.lens (\BatchGetJobsResponse' {jobsNotFound} -> jobsNotFound) (\s@BatchGetJobsResponse' {} a -> s {jobsNotFound = a} :: BatchGetJobsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of job definitions.
batchGetJobsResponse_jobs :: Lens.Lens' BatchGetJobsResponse (Prelude.Maybe [Job])
batchGetJobsResponse_jobs = Lens.lens (\BatchGetJobsResponse' {jobs} -> jobs) (\s@BatchGetJobsResponse' {} a -> s {jobs = a} :: BatchGetJobsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
batchGetJobsResponse_httpStatus :: Lens.Lens' BatchGetJobsResponse Prelude.Int
batchGetJobsResponse_httpStatus = Lens.lens (\BatchGetJobsResponse' {httpStatus} -> httpStatus) (\s@BatchGetJobsResponse' {} a -> s {httpStatus = a} :: BatchGetJobsResponse)

instance Prelude.NFData BatchGetJobsResponse
