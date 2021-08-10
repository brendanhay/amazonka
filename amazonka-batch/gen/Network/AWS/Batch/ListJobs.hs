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
-- Module      : Network.AWS.Batch.ListJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of AWS Batch jobs.
--
-- You must specify only one of the following items:
--
-- -   A job queue ID to return a list of jobs in that job queue
--
-- -   A multi-node parallel job ID to return a list of that job\'s nodes
--
-- -   An array job ID to return a list of that job\'s children
--
-- You can filter the results by job status with the @jobStatus@ parameter.
-- If you don\'t specify a status, only @RUNNING@ jobs are returned.
--
-- This operation returns paginated results.
module Network.AWS.Batch.ListJobs
  ( -- * Creating a Request
    ListJobs (..),
    newListJobs,

    -- * Request Lenses
    listJobs_nextToken,
    listJobs_maxResults,
    listJobs_jobQueue,
    listJobs_jobStatus,
    listJobs_arrayJobId,
    listJobs_multiNodeJobId,

    -- * Destructuring the Response
    ListJobsResponse (..),
    newListJobsResponse,

    -- * Response Lenses
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,
    listJobsResponse_jobSummaryList,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for @ListJobs@.
--
-- /See:/ 'newListJobs' smart constructor.
data ListJobs = ListJobs'
  { -- | The @nextToken@ value returned from a previous paginated @ListJobs@
    -- request where @maxResults@ was used and the results exceeded the value
    -- of that parameter. Pagination continues from the end of the previous
    -- results that returned the @nextToken@ value. This value is @null@ when
    -- there are no more results to return.
    --
    -- This token should be treated as an opaque identifier that\'s only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results returned by @ListJobs@ in paginated
    -- output. When this parameter is used, @ListJobs@ only returns
    -- @maxResults@ results in a single page along with a @nextToken@ response
    -- element. The remaining results of the initial request can be seen by
    -- sending another @ListJobs@ request with the returned @nextToken@ value.
    -- This value can be between 1 and 100. If this parameter isn\'t used, then
    -- @ListJobs@ returns up to 100 results and a @nextToken@ value if
    -- applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The name or full Amazon Resource Name (ARN) of the job queue used to
    -- list jobs.
    jobQueue :: Prelude.Maybe Prelude.Text,
    -- | The job status used to filter jobs in the specified queue. If you don\'t
    -- specify a status, only @RUNNING@ jobs are returned.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The job ID for an array job. Specifying an array job ID with this
    -- parameter lists all child jobs from within the specified array.
    arrayJobId :: Prelude.Maybe Prelude.Text,
    -- | The job ID for a multi-node parallel job. Specifying a multi-node
    -- parallel job ID with this parameter lists all nodes that are associated
    -- with the specified job.
    multiNodeJobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobs_nextToken' - The @nextToken@ value returned from a previous paginated @ListJobs@
-- request where @maxResults@ was used and the results exceeded the value
-- of that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value. This value is @null@ when
-- there are no more results to return.
--
-- This token should be treated as an opaque identifier that\'s only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'maxResults', 'listJobs_maxResults' - The maximum number of results returned by @ListJobs@ in paginated
-- output. When this parameter is used, @ListJobs@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListJobs@ request with the returned @nextToken@ value.
-- This value can be between 1 and 100. If this parameter isn\'t used, then
-- @ListJobs@ returns up to 100 results and a @nextToken@ value if
-- applicable.
--
-- 'jobQueue', 'listJobs_jobQueue' - The name or full Amazon Resource Name (ARN) of the job queue used to
-- list jobs.
--
-- 'jobStatus', 'listJobs_jobStatus' - The job status used to filter jobs in the specified queue. If you don\'t
-- specify a status, only @RUNNING@ jobs are returned.
--
-- 'arrayJobId', 'listJobs_arrayJobId' - The job ID for an array job. Specifying an array job ID with this
-- parameter lists all child jobs from within the specified array.
--
-- 'multiNodeJobId', 'listJobs_multiNodeJobId' - The job ID for a multi-node parallel job. Specifying a multi-node
-- parallel job ID with this parameter lists all nodes that are associated
-- with the specified job.
newListJobs ::
  ListJobs
newListJobs =
  ListJobs'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      jobQueue = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      arrayJobId = Prelude.Nothing,
      multiNodeJobId = Prelude.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated @ListJobs@
-- request where @maxResults@ was used and the results exceeded the value
-- of that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value. This value is @null@ when
-- there are no more results to return.
--
-- This token should be treated as an opaque identifier that\'s only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listJobs_nextToken :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_nextToken = Lens.lens (\ListJobs' {nextToken} -> nextToken) (\s@ListJobs' {} a -> s {nextToken = a} :: ListJobs)

-- | The maximum number of results returned by @ListJobs@ in paginated
-- output. When this parameter is used, @ListJobs@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListJobs@ request with the returned @nextToken@ value.
-- This value can be between 1 and 100. If this parameter isn\'t used, then
-- @ListJobs@ returns up to 100 results and a @nextToken@ value if
-- applicable.
listJobs_maxResults :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Int)
listJobs_maxResults = Lens.lens (\ListJobs' {maxResults} -> maxResults) (\s@ListJobs' {} a -> s {maxResults = a} :: ListJobs)

-- | The name or full Amazon Resource Name (ARN) of the job queue used to
-- list jobs.
listJobs_jobQueue :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_jobQueue = Lens.lens (\ListJobs' {jobQueue} -> jobQueue) (\s@ListJobs' {} a -> s {jobQueue = a} :: ListJobs)

-- | The job status used to filter jobs in the specified queue. If you don\'t
-- specify a status, only @RUNNING@ jobs are returned.
listJobs_jobStatus :: Lens.Lens' ListJobs (Prelude.Maybe JobStatus)
listJobs_jobStatus = Lens.lens (\ListJobs' {jobStatus} -> jobStatus) (\s@ListJobs' {} a -> s {jobStatus = a} :: ListJobs)

-- | The job ID for an array job. Specifying an array job ID with this
-- parameter lists all child jobs from within the specified array.
listJobs_arrayJobId :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_arrayJobId = Lens.lens (\ListJobs' {arrayJobId} -> arrayJobId) (\s@ListJobs' {} a -> s {arrayJobId = a} :: ListJobs)

-- | The job ID for a multi-node parallel job. Specifying a multi-node
-- parallel job ID with this parameter lists all nodes that are associated
-- with the specified job.
listJobs_multiNodeJobId :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_multiNodeJobId = Lens.lens (\ListJobs' {multiNodeJobId} -> multiNodeJobId) (\s@ListJobs' {} a -> s {multiNodeJobId = a} :: ListJobs)

instance Core.AWSPager ListJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listJobsResponse_jobSummaryList) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listJobs_nextToken
          Lens..~ rs
          Lens.^? listJobsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListJobs where
  type AWSResponse ListJobs = ListJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "jobSummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListJobs

instance Prelude.NFData ListJobs

instance Core.ToHeaders ListJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListJobs where
  toJSON ListJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("jobQueue" Core..=) Prelude.<$> jobQueue,
            ("jobStatus" Core..=) Prelude.<$> jobStatus,
            ("arrayJobId" Core..=) Prelude.<$> arrayJobId,
            ("multiNodeJobId" Core..=)
              Prelude.<$> multiNodeJobId
          ]
      )

instance Core.ToPath ListJobs where
  toPath = Prelude.const "/v1/listjobs"

instance Core.ToQuery ListJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { -- | The @nextToken@ value to include in a future @ListJobs@ request. When
    -- the results of a @ListJobs@ request exceed @maxResults@, this value can
    -- be used to retrieve the next page of results. This value is @null@ when
    -- there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of job summaries that match the request.
    jobSummaryList :: [JobSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobsResponse_nextToken' - The @nextToken@ value to include in a future @ListJobs@ request. When
-- the results of a @ListJobs@ request exceed @maxResults@, this value can
-- be used to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
--
-- 'httpStatus', 'listJobsResponse_httpStatus' - The response's http status code.
--
-- 'jobSummaryList', 'listJobsResponse_jobSummaryList' - A list of job summaries that match the request.
newListJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJobsResponse
newListJobsResponse pHttpStatus_ =
  ListJobsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      jobSummaryList = Prelude.mempty
    }

-- | The @nextToken@ value to include in a future @ListJobs@ request. When
-- the results of a @ListJobs@ request exceed @maxResults@, this value can
-- be used to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
listJobsResponse_nextToken :: Lens.Lens' ListJobsResponse (Prelude.Maybe Prelude.Text)
listJobsResponse_nextToken = Lens.lens (\ListJobsResponse' {nextToken} -> nextToken) (\s@ListJobsResponse' {} a -> s {nextToken = a} :: ListJobsResponse)

-- | The response's http status code.
listJobsResponse_httpStatus :: Lens.Lens' ListJobsResponse Prelude.Int
listJobsResponse_httpStatus = Lens.lens (\ListJobsResponse' {httpStatus} -> httpStatus) (\s@ListJobsResponse' {} a -> s {httpStatus = a} :: ListJobsResponse)

-- | A list of job summaries that match the request.
listJobsResponse_jobSummaryList :: Lens.Lens' ListJobsResponse [JobSummary]
listJobsResponse_jobSummaryList = Lens.lens (\ListJobsResponse' {jobSummaryList} -> jobSummaryList) (\s@ListJobsResponse' {} a -> s {jobSummaryList = a} :: ListJobsResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListJobsResponse
