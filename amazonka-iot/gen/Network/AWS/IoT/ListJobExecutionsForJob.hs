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
-- Module      : Network.AWS.IoT.ListJobExecutionsForJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the job executions for a job.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListJobExecutionsForJob
  ( -- * Creating a Request
    ListJobExecutionsForJob (..),
    newListJobExecutionsForJob,

    -- * Request Lenses
    listJobExecutionsForJob_nextToken,
    listJobExecutionsForJob_status,
    listJobExecutionsForJob_maxResults,
    listJobExecutionsForJob_jobId,

    -- * Destructuring the Response
    ListJobExecutionsForJobResponse (..),
    newListJobExecutionsForJobResponse,

    -- * Response Lenses
    listJobExecutionsForJobResponse_nextToken,
    listJobExecutionsForJobResponse_executionSummaries,
    listJobExecutionsForJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListJobExecutionsForJob' smart constructor.
data ListJobExecutionsForJob = ListJobExecutionsForJob'
  { -- | The token to retrieve the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The status of the job.
    status :: Core.Maybe JobExecutionStatus,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Natural,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListJobExecutionsForJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobExecutionsForJob_nextToken' - The token to retrieve the next set of results.
--
-- 'status', 'listJobExecutionsForJob_status' - The status of the job.
--
-- 'maxResults', 'listJobExecutionsForJob_maxResults' - The maximum number of results to be returned per request.
--
-- 'jobId', 'listJobExecutionsForJob_jobId' - The unique identifier you assigned to this job when it was created.
newListJobExecutionsForJob ::
  -- | 'jobId'
  Core.Text ->
  ListJobExecutionsForJob
newListJobExecutionsForJob pJobId_ =
  ListJobExecutionsForJob'
    { nextToken = Core.Nothing,
      status = Core.Nothing,
      maxResults = Core.Nothing,
      jobId = pJobId_
    }

-- | The token to retrieve the next set of results.
listJobExecutionsForJob_nextToken :: Lens.Lens' ListJobExecutionsForJob (Core.Maybe Core.Text)
listJobExecutionsForJob_nextToken = Lens.lens (\ListJobExecutionsForJob' {nextToken} -> nextToken) (\s@ListJobExecutionsForJob' {} a -> s {nextToken = a} :: ListJobExecutionsForJob)

-- | The status of the job.
listJobExecutionsForJob_status :: Lens.Lens' ListJobExecutionsForJob (Core.Maybe JobExecutionStatus)
listJobExecutionsForJob_status = Lens.lens (\ListJobExecutionsForJob' {status} -> status) (\s@ListJobExecutionsForJob' {} a -> s {status = a} :: ListJobExecutionsForJob)

-- | The maximum number of results to be returned per request.
listJobExecutionsForJob_maxResults :: Lens.Lens' ListJobExecutionsForJob (Core.Maybe Core.Natural)
listJobExecutionsForJob_maxResults = Lens.lens (\ListJobExecutionsForJob' {maxResults} -> maxResults) (\s@ListJobExecutionsForJob' {} a -> s {maxResults = a} :: ListJobExecutionsForJob)

-- | The unique identifier you assigned to this job when it was created.
listJobExecutionsForJob_jobId :: Lens.Lens' ListJobExecutionsForJob Core.Text
listJobExecutionsForJob_jobId = Lens.lens (\ListJobExecutionsForJob' {jobId} -> jobId) (\s@ListJobExecutionsForJob' {} a -> s {jobId = a} :: ListJobExecutionsForJob)

instance Core.AWSPager ListJobExecutionsForJob where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobExecutionsForJobResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobExecutionsForJobResponse_executionSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listJobExecutionsForJob_nextToken
          Lens..~ rs
          Lens.^? listJobExecutionsForJobResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListJobExecutionsForJob where
  type
    AWSResponse ListJobExecutionsForJob =
      ListJobExecutionsForJobResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobExecutionsForJobResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "executionSummaries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListJobExecutionsForJob

instance Core.NFData ListJobExecutionsForJob

instance Core.ToHeaders ListJobExecutionsForJob where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListJobExecutionsForJob where
  toPath ListJobExecutionsForJob' {..} =
    Core.mconcat ["/jobs/", Core.toBS jobId, "/things"]

instance Core.ToQuery ListJobExecutionsForJob where
  toQuery ListJobExecutionsForJob' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "status" Core.=: status,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListJobExecutionsForJobResponse' smart constructor.
data ListJobExecutionsForJobResponse = ListJobExecutionsForJobResponse'
  { -- | The token for the next set of results, or __null__ if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of job execution summaries.
    executionSummaries :: Core.Maybe [JobExecutionSummaryForJob],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListJobExecutionsForJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobExecutionsForJobResponse_nextToken' - The token for the next set of results, or __null__ if there are no
-- additional results.
--
-- 'executionSummaries', 'listJobExecutionsForJobResponse_executionSummaries' - A list of job execution summaries.
--
-- 'httpStatus', 'listJobExecutionsForJobResponse_httpStatus' - The response's http status code.
newListJobExecutionsForJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListJobExecutionsForJobResponse
newListJobExecutionsForJobResponse pHttpStatus_ =
  ListJobExecutionsForJobResponse'
    { nextToken =
        Core.Nothing,
      executionSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or __null__ if there are no
-- additional results.
listJobExecutionsForJobResponse_nextToken :: Lens.Lens' ListJobExecutionsForJobResponse (Core.Maybe Core.Text)
listJobExecutionsForJobResponse_nextToken = Lens.lens (\ListJobExecutionsForJobResponse' {nextToken} -> nextToken) (\s@ListJobExecutionsForJobResponse' {} a -> s {nextToken = a} :: ListJobExecutionsForJobResponse)

-- | A list of job execution summaries.
listJobExecutionsForJobResponse_executionSummaries :: Lens.Lens' ListJobExecutionsForJobResponse (Core.Maybe [JobExecutionSummaryForJob])
listJobExecutionsForJobResponse_executionSummaries = Lens.lens (\ListJobExecutionsForJobResponse' {executionSummaries} -> executionSummaries) (\s@ListJobExecutionsForJobResponse' {} a -> s {executionSummaries = a} :: ListJobExecutionsForJobResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listJobExecutionsForJobResponse_httpStatus :: Lens.Lens' ListJobExecutionsForJobResponse Core.Int
listJobExecutionsForJobResponse_httpStatus = Lens.lens (\ListJobExecutionsForJobResponse' {httpStatus} -> httpStatus) (\s@ListJobExecutionsForJobResponse' {} a -> s {httpStatus = a} :: ListJobExecutionsForJobResponse)

instance Core.NFData ListJobExecutionsForJobResponse
