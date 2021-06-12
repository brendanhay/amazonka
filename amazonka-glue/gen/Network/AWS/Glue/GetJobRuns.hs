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
-- Module      : Network.AWS.Glue.GetJobRuns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for all runs of a given job definition.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetJobRuns
  ( -- * Creating a Request
    GetJobRuns (..),
    newGetJobRuns,

    -- * Request Lenses
    getJobRuns_nextToken,
    getJobRuns_maxResults,
    getJobRuns_jobName,

    -- * Destructuring the Response
    GetJobRunsResponse (..),
    newGetJobRunsResponse,

    -- * Response Lenses
    getJobRunsResponse_nextToken,
    getJobRunsResponse_jobRuns,
    getJobRunsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetJobRuns' smart constructor.
data GetJobRuns = GetJobRuns'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum size of the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the job definition for which to retrieve all job runs.
    jobName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetJobRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getJobRuns_nextToken' - A continuation token, if this is a continuation call.
--
-- 'maxResults', 'getJobRuns_maxResults' - The maximum size of the response.
--
-- 'jobName', 'getJobRuns_jobName' - The name of the job definition for which to retrieve all job runs.
newGetJobRuns ::
  -- | 'jobName'
  Core.Text ->
  GetJobRuns
newGetJobRuns pJobName_ =
  GetJobRuns'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      jobName = pJobName_
    }

-- | A continuation token, if this is a continuation call.
getJobRuns_nextToken :: Lens.Lens' GetJobRuns (Core.Maybe Core.Text)
getJobRuns_nextToken = Lens.lens (\GetJobRuns' {nextToken} -> nextToken) (\s@GetJobRuns' {} a -> s {nextToken = a} :: GetJobRuns)

-- | The maximum size of the response.
getJobRuns_maxResults :: Lens.Lens' GetJobRuns (Core.Maybe Core.Natural)
getJobRuns_maxResults = Lens.lens (\GetJobRuns' {maxResults} -> maxResults) (\s@GetJobRuns' {} a -> s {maxResults = a} :: GetJobRuns)

-- | The name of the job definition for which to retrieve all job runs.
getJobRuns_jobName :: Lens.Lens' GetJobRuns Core.Text
getJobRuns_jobName = Lens.lens (\GetJobRuns' {jobName} -> jobName) (\s@GetJobRuns' {} a -> s {jobName = a} :: GetJobRuns)

instance Core.AWSPager GetJobRuns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getJobRunsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getJobRunsResponse_jobRuns Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getJobRuns_nextToken
          Lens..~ rs
          Lens.^? getJobRunsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetJobRuns where
  type AWSResponse GetJobRuns = GetJobRunsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobRunsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "JobRuns" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetJobRuns

instance Core.NFData GetJobRuns

instance Core.ToHeaders GetJobRuns where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetJobRuns" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetJobRuns where
  toJSON GetJobRuns' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("JobName" Core..= jobName)
          ]
      )

instance Core.ToPath GetJobRuns where
  toPath = Core.const "/"

instance Core.ToQuery GetJobRuns where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetJobRunsResponse' smart constructor.
data GetJobRunsResponse = GetJobRunsResponse'
  { -- | A continuation token, if not all requested job runs have been returned.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of job-run metadata objects.
    jobRuns :: Core.Maybe [JobRun],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetJobRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getJobRunsResponse_nextToken' - A continuation token, if not all requested job runs have been returned.
--
-- 'jobRuns', 'getJobRunsResponse_jobRuns' - A list of job-run metadata objects.
--
-- 'httpStatus', 'getJobRunsResponse_httpStatus' - The response's http status code.
newGetJobRunsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetJobRunsResponse
newGetJobRunsResponse pHttpStatus_ =
  GetJobRunsResponse'
    { nextToken = Core.Nothing,
      jobRuns = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if not all requested job runs have been returned.
getJobRunsResponse_nextToken :: Lens.Lens' GetJobRunsResponse (Core.Maybe Core.Text)
getJobRunsResponse_nextToken = Lens.lens (\GetJobRunsResponse' {nextToken} -> nextToken) (\s@GetJobRunsResponse' {} a -> s {nextToken = a} :: GetJobRunsResponse)

-- | A list of job-run metadata objects.
getJobRunsResponse_jobRuns :: Lens.Lens' GetJobRunsResponse (Core.Maybe [JobRun])
getJobRunsResponse_jobRuns = Lens.lens (\GetJobRunsResponse' {jobRuns} -> jobRuns) (\s@GetJobRunsResponse' {} a -> s {jobRuns = a} :: GetJobRunsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getJobRunsResponse_httpStatus :: Lens.Lens' GetJobRunsResponse Core.Int
getJobRunsResponse_httpStatus = Lens.lens (\GetJobRunsResponse' {httpStatus} -> httpStatus) (\s@GetJobRunsResponse' {} a -> s {httpStatus = a} :: GetJobRunsResponse)

instance Core.NFData GetJobRunsResponse
