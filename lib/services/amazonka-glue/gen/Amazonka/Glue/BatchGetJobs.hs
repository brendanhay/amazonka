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
-- Module      : Amazonka.Glue.BatchGetJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Glue.BatchGetJobs
  ( -- * Creating a Request
    BatchGetJobs (..),
    newBatchGetJobs,

    -- * Request Lenses
    batchGetJobs_jobNames,

    -- * Destructuring the Response
    BatchGetJobsResponse (..),
    newBatchGetJobsResponse,

    -- * Response Lenses
    batchGetJobsResponse_jobs,
    batchGetJobsResponse_jobsNotFound,
    batchGetJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetJobs' smart constructor.
data BatchGetJobs = BatchGetJobs'
  { -- | A list of job names, which might be the names returned from the
    -- @ListJobs@ operation.
    jobNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
batchGetJobs_jobNames = Lens.lens (\BatchGetJobs' {jobNames} -> jobNames) (\s@BatchGetJobs' {} a -> s {jobNames = a} :: BatchGetJobs) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetJobs where
  type AWSResponse BatchGetJobs = BatchGetJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetJobsResponse'
            Prelude.<$> (x Data..?> "Jobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "JobsNotFound" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetJobs where
  hashWithSalt _salt BatchGetJobs' {..} =
    _salt `Prelude.hashWithSalt` jobNames

instance Prelude.NFData BatchGetJobs where
  rnf BatchGetJobs' {..} = Prelude.rnf jobNames

instance Data.ToHeaders BatchGetJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.BatchGetJobs" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetJobs where
  toJSON BatchGetJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobNames" Data..= jobNames)]
      )

instance Data.ToPath BatchGetJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetJobsResponse' smart constructor.
data BatchGetJobsResponse = BatchGetJobsResponse'
  { -- | A list of job definitions.
    jobs :: Prelude.Maybe [Job],
    -- | A list of names of jobs not found.
    jobsNotFound :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobs', 'batchGetJobsResponse_jobs' - A list of job definitions.
--
-- 'jobsNotFound', 'batchGetJobsResponse_jobsNotFound' - A list of names of jobs not found.
--
-- 'httpStatus', 'batchGetJobsResponse_httpStatus' - The response's http status code.
newBatchGetJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetJobsResponse
newBatchGetJobsResponse pHttpStatus_ =
  BatchGetJobsResponse'
    { jobs = Prelude.Nothing,
      jobsNotFound = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of job definitions.
batchGetJobsResponse_jobs :: Lens.Lens' BatchGetJobsResponse (Prelude.Maybe [Job])
batchGetJobsResponse_jobs = Lens.lens (\BatchGetJobsResponse' {jobs} -> jobs) (\s@BatchGetJobsResponse' {} a -> s {jobs = a} :: BatchGetJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of names of jobs not found.
batchGetJobsResponse_jobsNotFound :: Lens.Lens' BatchGetJobsResponse (Prelude.Maybe [Prelude.Text])
batchGetJobsResponse_jobsNotFound = Lens.lens (\BatchGetJobsResponse' {jobsNotFound} -> jobsNotFound) (\s@BatchGetJobsResponse' {} a -> s {jobsNotFound = a} :: BatchGetJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetJobsResponse_httpStatus :: Lens.Lens' BatchGetJobsResponse Prelude.Int
batchGetJobsResponse_httpStatus = Lens.lens (\BatchGetJobsResponse' {httpStatus} -> httpStatus) (\s@BatchGetJobsResponse' {} a -> s {httpStatus = a} :: BatchGetJobsResponse)

instance Prelude.NFData BatchGetJobsResponse where
  rnf BatchGetJobsResponse' {..} =
    Prelude.rnf jobs
      `Prelude.seq` Prelude.rnf jobsNotFound
      `Prelude.seq` Prelude.rnf httpStatus
