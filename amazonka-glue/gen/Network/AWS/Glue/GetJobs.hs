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
-- Module      : Network.AWS.Glue.GetJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all current job definitions.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetJobs
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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetJobs' smart constructor.
data GetJobs = GetJobs'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum size of the response.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | A continuation token, if this is a continuation call.
getJobs_nextToken :: Lens.Lens' GetJobs (Core.Maybe Core.Text)
getJobs_nextToken = Lens.lens (\GetJobs' {nextToken} -> nextToken) (\s@GetJobs' {} a -> s {nextToken = a} :: GetJobs)

-- | The maximum size of the response.
getJobs_maxResults :: Lens.Lens' GetJobs (Core.Maybe Core.Natural)
getJobs_maxResults = Lens.lens (\GetJobs' {maxResults} -> maxResults) (\s@GetJobs' {} a -> s {maxResults = a} :: GetJobs)

instance Core.AWSPager GetJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getJobsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^? getJobsResponse_jobs Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getJobs_nextToken
          Lens..~ rs
          Lens.^? getJobsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetJobs where
  type AWSResponse GetJobs = GetJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Jobs" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetJobs

instance Core.NFData GetJobs

instance Core.ToHeaders GetJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetJobs" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetJobs where
  toJSON GetJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath GetJobs where
  toPath = Core.const "/"

instance Core.ToQuery GetJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetJobsResponse' smart constructor.
data GetJobsResponse = GetJobsResponse'
  { -- | A continuation token, if not all job definitions have yet been returned.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of job definitions.
    jobs :: Core.Maybe [Job],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetJobsResponse
newGetJobsResponse pHttpStatus_ =
  GetJobsResponse'
    { nextToken = Core.Nothing,
      jobs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if not all job definitions have yet been returned.
getJobsResponse_nextToken :: Lens.Lens' GetJobsResponse (Core.Maybe Core.Text)
getJobsResponse_nextToken = Lens.lens (\GetJobsResponse' {nextToken} -> nextToken) (\s@GetJobsResponse' {} a -> s {nextToken = a} :: GetJobsResponse)

-- | A list of job definitions.
getJobsResponse_jobs :: Lens.Lens' GetJobsResponse (Core.Maybe [Job])
getJobsResponse_jobs = Lens.lens (\GetJobsResponse' {jobs} -> jobs) (\s@GetJobsResponse' {} a -> s {jobs = a} :: GetJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getJobsResponse_httpStatus :: Lens.Lens' GetJobsResponse Core.Int
getJobsResponse_httpStatus = Lens.lens (\GetJobsResponse' {httpStatus} -> httpStatus) (\s@GetJobsResponse' {} a -> s {httpStatus = a} :: GetJobsResponse)

instance Core.NFData GetJobsResponse
