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
-- Module      : Amazonka.Glue.GetJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all current job definitions.
--
-- This operation returns paginated results.
module Amazonka.Glue.GetJobs
  ( -- * Creating a Request
    GetJobs (..),
    newGetJobs,

    -- * Request Lenses
    getJobs_nextToken,
    getJobs_maxResults,

    -- * Destructuring the Response
    GetJobsResponse (..),
    newGetJobsResponse,

    -- * Response Lenses
    getJobsResponse_nextToken,
    getJobsResponse_jobs,
    getJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetJobs' smart constructor.
data GetJobs = GetJobs'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of the response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getJobs_nextToken' - A continuation token, if this is a continuation call.
--
-- 'maxResults', 'getJobs_maxResults' - The maximum size of the response.
newGetJobs ::
  GetJobs
newGetJobs =
  GetJobs'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A continuation token, if this is a continuation call.
getJobs_nextToken :: Lens.Lens' GetJobs (Prelude.Maybe Prelude.Text)
getJobs_nextToken = Lens.lens (\GetJobs' {nextToken} -> nextToken) (\s@GetJobs' {} a -> s {nextToken = a} :: GetJobs)

-- | The maximum size of the response.
getJobs_maxResults :: Lens.Lens' GetJobs (Prelude.Maybe Prelude.Natural)
getJobs_maxResults = Lens.lens (\GetJobs' {maxResults} -> maxResults) (\s@GetJobs' {} a -> s {maxResults = a} :: GetJobs)

instance Core.AWSPager GetJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getJobsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getJobsResponse_jobs Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getJobs_nextToken
          Lens..~ rs
          Lens.^? getJobsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetJobs where
  type AWSResponse GetJobs = GetJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Jobs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetJobs where
  hashWithSalt _salt GetJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData GetJobs where
  rnf GetJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders GetJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetJobs" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetJobs where
  toJSON GetJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath GetJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery GetJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJobsResponse' smart constructor.
data GetJobsResponse = GetJobsResponse'
  { -- | A continuation token, if not all job definitions have yet been returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of job definitions.
    jobs :: Prelude.Maybe [Job],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getJobsResponse_nextToken' - A continuation token, if not all job definitions have yet been returned.
--
-- 'jobs', 'getJobsResponse_jobs' - A list of job definitions.
--
-- 'httpStatus', 'getJobsResponse_httpStatus' - The response's http status code.
newGetJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetJobsResponse
newGetJobsResponse pHttpStatus_ =
  GetJobsResponse'
    { nextToken = Prelude.Nothing,
      jobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if not all job definitions have yet been returned.
getJobsResponse_nextToken :: Lens.Lens' GetJobsResponse (Prelude.Maybe Prelude.Text)
getJobsResponse_nextToken = Lens.lens (\GetJobsResponse' {nextToken} -> nextToken) (\s@GetJobsResponse' {} a -> s {nextToken = a} :: GetJobsResponse)

-- | A list of job definitions.
getJobsResponse_jobs :: Lens.Lens' GetJobsResponse (Prelude.Maybe [Job])
getJobsResponse_jobs = Lens.lens (\GetJobsResponse' {jobs} -> jobs) (\s@GetJobsResponse' {} a -> s {jobs = a} :: GetJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getJobsResponse_httpStatus :: Lens.Lens' GetJobsResponse Prelude.Int
getJobsResponse_httpStatus = Lens.lens (\GetJobsResponse' {httpStatus} -> httpStatus) (\s@GetJobsResponse' {} a -> s {httpStatus = a} :: GetJobsResponse)

instance Prelude.NFData GetJobsResponse where
  rnf GetJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobs
      `Prelude.seq` Prelude.rnf httpStatus
