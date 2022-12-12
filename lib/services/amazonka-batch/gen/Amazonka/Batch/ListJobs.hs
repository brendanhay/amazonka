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
-- Module      : Amazonka.Batch.ListJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Batch jobs.
--
-- You must specify only one of the following items:
--
-- -   A job queue ID to return a list of jobs in that job queue
--
-- -   A multi-node parallel job ID to return a list of nodes for that job
--
-- -   An array job ID to return a list of the children for that job
--
-- You can filter the results by job status with the @jobStatus@ parameter.
-- If you don\'t specify a status, only @RUNNING@ jobs are returned.
--
-- This operation returns paginated results.
module Amazonka.Batch.ListJobs
  ( -- * Creating a Request
    ListJobs (..),
    newListJobs,

    -- * Request Lenses
    listJobs_arrayJobId,
    listJobs_filters,
    listJobs_jobQueue,
    listJobs_jobStatus,
    listJobs_maxResults,
    listJobs_multiNodeJobId,
    listJobs_nextToken,

    -- * Destructuring the Response
    ListJobsResponse (..),
    newListJobsResponse,

    -- * Response Lenses
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,
    listJobsResponse_jobSummaryList,
  )
where

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @ListJobs@.
--
-- /See:/ 'newListJobs' smart constructor.
data ListJobs = ListJobs'
  { -- | The job ID for an array job. Specifying an array job ID with this
    -- parameter lists all child jobs from within the specified array.
    arrayJobId :: Prelude.Maybe Prelude.Text,
    -- | The filter to apply to the query. Only one filter can be used at a time.
    -- When the filter is used, @jobStatus@ is ignored. The filter doesn\'t
    -- apply to child jobs in an array or multi-node parallel (MNP) jobs. The
    -- results are sorted by the @createdAt@ field, with the most recent jobs
    -- being first.
    --
    -- [JOB_NAME]
    --     The value of the filter is a case-insensitive match for the job
    --     name. If the value ends with an asterisk (*), the filter matches any
    --     job name that begins with the string before the \'*\'. This
    --     corresponds to the @jobName@ value. For example, @test1@ matches
    --     both @Test1@ and @test1@, and @test1*@ matches both @test1@ and
    --     @Test10@. When the @JOB_NAME@ filter is used, the results are
    --     grouped by the job name and version.
    --
    -- [JOB_DEFINITION]
    --     The value for the filter is the name or Amazon Resource Name (ARN)
    --     of the job definition. This corresponds to the @jobDefinition@
    --     value. The value is case sensitive. When the value for the filter is
    --     the job definition name, the results include all the jobs that used
    --     any revision of that job definition name. If the value ends with an
    --     asterisk (*), the filter matches any job definition name that begins
    --     with the string before the \'*\'. For example, @jd1@ matches only
    --     @jd1@, and @jd1*@ matches both @jd1@ and @jd1A@. The version of the
    --     job definition that\'s used doesn\'t affect the sort order. When the
    --     @JOB_DEFINITION@ filter is used and the ARN is used (which is in the
    --     form
    --     @arn:${Partition}:batch:${Region}:${Account}:job-definition\/${JobDefinitionName}:${Revision}@),
    --     the results include jobs that used the specified revision of the job
    --     definition. Asterisk (*) isn\'t supported when the ARN is used.
    --
    -- [BEFORE_CREATED_AT]
    --     The value for the filter is the time that\'s before the job was
    --     created. This corresponds to the @createdAt@ value. The value is a
    --     string representation of the number of milliseconds since 00:00:00
    --     UTC (midnight) on January 1, 1970.
    --
    -- [AFTER_CREATED_AT]
    --     The value for the filter is the time that\'s after the job was
    --     created. This corresponds to the @createdAt@ value. The value is a
    --     string representation of the number of milliseconds since 00:00:00
    --     UTC (midnight) on January 1, 1970.
    filters :: Prelude.Maybe [KeyValuesPair],
    -- | The name or full Amazon Resource Name (ARN) of the job queue used to
    -- list jobs.
    jobQueue :: Prelude.Maybe Prelude.Text,
    -- | The job status used to filter jobs in the specified queue. If the
    -- @filters@ parameter is specified, the @jobStatus@ parameter is ignored
    -- and jobs with any status are returned. If you don\'t specify a status,
    -- only @RUNNING@ jobs are returned.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The maximum number of results returned by @ListJobs@ in paginated
    -- output. When this parameter is used, @ListJobs@ only returns
    -- @maxResults@ results in a single page and a @nextToken@ response
    -- element. The remaining results of the initial request can be seen by
    -- sending another @ListJobs@ request with the returned @nextToken@ value.
    -- This value can be between 1 and 100. If this parameter isn\'t used, then
    -- @ListJobs@ returns up to 100 results and a @nextToken@ value if
    -- applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The job ID for a multi-node parallel job. Specifying a multi-node
    -- parallel job ID with this parameter lists all nodes that are associated
    -- with the specified job.
    multiNodeJobId :: Prelude.Maybe Prelude.Text,
    -- | The @nextToken@ value returned from a previous paginated @ListJobs@
    -- request where @maxResults@ was used and the results exceeded the value
    -- of that parameter. Pagination continues from the end of the previous
    -- results that returned the @nextToken@ value. This value is @null@ when
    -- there are no more results to return.
    --
    -- Treat this token as an opaque identifier that\'s only used to retrieve
    -- the next items in a list and not for other programmatic purposes.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'arrayJobId', 'listJobs_arrayJobId' - The job ID for an array job. Specifying an array job ID with this
-- parameter lists all child jobs from within the specified array.
--
-- 'filters', 'listJobs_filters' - The filter to apply to the query. Only one filter can be used at a time.
-- When the filter is used, @jobStatus@ is ignored. The filter doesn\'t
-- apply to child jobs in an array or multi-node parallel (MNP) jobs. The
-- results are sorted by the @createdAt@ field, with the most recent jobs
-- being first.
--
-- [JOB_NAME]
--     The value of the filter is a case-insensitive match for the job
--     name. If the value ends with an asterisk (*), the filter matches any
--     job name that begins with the string before the \'*\'. This
--     corresponds to the @jobName@ value. For example, @test1@ matches
--     both @Test1@ and @test1@, and @test1*@ matches both @test1@ and
--     @Test10@. When the @JOB_NAME@ filter is used, the results are
--     grouped by the job name and version.
--
-- [JOB_DEFINITION]
--     The value for the filter is the name or Amazon Resource Name (ARN)
--     of the job definition. This corresponds to the @jobDefinition@
--     value. The value is case sensitive. When the value for the filter is
--     the job definition name, the results include all the jobs that used
--     any revision of that job definition name. If the value ends with an
--     asterisk (*), the filter matches any job definition name that begins
--     with the string before the \'*\'. For example, @jd1@ matches only
--     @jd1@, and @jd1*@ matches both @jd1@ and @jd1A@. The version of the
--     job definition that\'s used doesn\'t affect the sort order. When the
--     @JOB_DEFINITION@ filter is used and the ARN is used (which is in the
--     form
--     @arn:${Partition}:batch:${Region}:${Account}:job-definition\/${JobDefinitionName}:${Revision}@),
--     the results include jobs that used the specified revision of the job
--     definition. Asterisk (*) isn\'t supported when the ARN is used.
--
-- [BEFORE_CREATED_AT]
--     The value for the filter is the time that\'s before the job was
--     created. This corresponds to the @createdAt@ value. The value is a
--     string representation of the number of milliseconds since 00:00:00
--     UTC (midnight) on January 1, 1970.
--
-- [AFTER_CREATED_AT]
--     The value for the filter is the time that\'s after the job was
--     created. This corresponds to the @createdAt@ value. The value is a
--     string representation of the number of milliseconds since 00:00:00
--     UTC (midnight) on January 1, 1970.
--
-- 'jobQueue', 'listJobs_jobQueue' - The name or full Amazon Resource Name (ARN) of the job queue used to
-- list jobs.
--
-- 'jobStatus', 'listJobs_jobStatus' - The job status used to filter jobs in the specified queue. If the
-- @filters@ parameter is specified, the @jobStatus@ parameter is ignored
-- and jobs with any status are returned. If you don\'t specify a status,
-- only @RUNNING@ jobs are returned.
--
-- 'maxResults', 'listJobs_maxResults' - The maximum number of results returned by @ListJobs@ in paginated
-- output. When this parameter is used, @ListJobs@ only returns
-- @maxResults@ results in a single page and a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListJobs@ request with the returned @nextToken@ value.
-- This value can be between 1 and 100. If this parameter isn\'t used, then
-- @ListJobs@ returns up to 100 results and a @nextToken@ value if
-- applicable.
--
-- 'multiNodeJobId', 'listJobs_multiNodeJobId' - The job ID for a multi-node parallel job. Specifying a multi-node
-- parallel job ID with this parameter lists all nodes that are associated
-- with the specified job.
--
-- 'nextToken', 'listJobs_nextToken' - The @nextToken@ value returned from a previous paginated @ListJobs@
-- request where @maxResults@ was used and the results exceeded the value
-- of that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value. This value is @null@ when
-- there are no more results to return.
--
-- Treat this token as an opaque identifier that\'s only used to retrieve
-- the next items in a list and not for other programmatic purposes.
newListJobs ::
  ListJobs
newListJobs =
  ListJobs'
    { arrayJobId = Prelude.Nothing,
      filters = Prelude.Nothing,
      jobQueue = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      multiNodeJobId = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The job ID for an array job. Specifying an array job ID with this
-- parameter lists all child jobs from within the specified array.
listJobs_arrayJobId :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_arrayJobId = Lens.lens (\ListJobs' {arrayJobId} -> arrayJobId) (\s@ListJobs' {} a -> s {arrayJobId = a} :: ListJobs)

-- | The filter to apply to the query. Only one filter can be used at a time.
-- When the filter is used, @jobStatus@ is ignored. The filter doesn\'t
-- apply to child jobs in an array or multi-node parallel (MNP) jobs. The
-- results are sorted by the @createdAt@ field, with the most recent jobs
-- being first.
--
-- [JOB_NAME]
--     The value of the filter is a case-insensitive match for the job
--     name. If the value ends with an asterisk (*), the filter matches any
--     job name that begins with the string before the \'*\'. This
--     corresponds to the @jobName@ value. For example, @test1@ matches
--     both @Test1@ and @test1@, and @test1*@ matches both @test1@ and
--     @Test10@. When the @JOB_NAME@ filter is used, the results are
--     grouped by the job name and version.
--
-- [JOB_DEFINITION]
--     The value for the filter is the name or Amazon Resource Name (ARN)
--     of the job definition. This corresponds to the @jobDefinition@
--     value. The value is case sensitive. When the value for the filter is
--     the job definition name, the results include all the jobs that used
--     any revision of that job definition name. If the value ends with an
--     asterisk (*), the filter matches any job definition name that begins
--     with the string before the \'*\'. For example, @jd1@ matches only
--     @jd1@, and @jd1*@ matches both @jd1@ and @jd1A@. The version of the
--     job definition that\'s used doesn\'t affect the sort order. When the
--     @JOB_DEFINITION@ filter is used and the ARN is used (which is in the
--     form
--     @arn:${Partition}:batch:${Region}:${Account}:job-definition\/${JobDefinitionName}:${Revision}@),
--     the results include jobs that used the specified revision of the job
--     definition. Asterisk (*) isn\'t supported when the ARN is used.
--
-- [BEFORE_CREATED_AT]
--     The value for the filter is the time that\'s before the job was
--     created. This corresponds to the @createdAt@ value. The value is a
--     string representation of the number of milliseconds since 00:00:00
--     UTC (midnight) on January 1, 1970.
--
-- [AFTER_CREATED_AT]
--     The value for the filter is the time that\'s after the job was
--     created. This corresponds to the @createdAt@ value. The value is a
--     string representation of the number of milliseconds since 00:00:00
--     UTC (midnight) on January 1, 1970.
listJobs_filters :: Lens.Lens' ListJobs (Prelude.Maybe [KeyValuesPair])
listJobs_filters = Lens.lens (\ListJobs' {filters} -> filters) (\s@ListJobs' {} a -> s {filters = a} :: ListJobs) Prelude.. Lens.mapping Lens.coerced

-- | The name or full Amazon Resource Name (ARN) of the job queue used to
-- list jobs.
listJobs_jobQueue :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_jobQueue = Lens.lens (\ListJobs' {jobQueue} -> jobQueue) (\s@ListJobs' {} a -> s {jobQueue = a} :: ListJobs)

-- | The job status used to filter jobs in the specified queue. If the
-- @filters@ parameter is specified, the @jobStatus@ parameter is ignored
-- and jobs with any status are returned. If you don\'t specify a status,
-- only @RUNNING@ jobs are returned.
listJobs_jobStatus :: Lens.Lens' ListJobs (Prelude.Maybe JobStatus)
listJobs_jobStatus = Lens.lens (\ListJobs' {jobStatus} -> jobStatus) (\s@ListJobs' {} a -> s {jobStatus = a} :: ListJobs)

-- | The maximum number of results returned by @ListJobs@ in paginated
-- output. When this parameter is used, @ListJobs@ only returns
-- @maxResults@ results in a single page and a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListJobs@ request with the returned @nextToken@ value.
-- This value can be between 1 and 100. If this parameter isn\'t used, then
-- @ListJobs@ returns up to 100 results and a @nextToken@ value if
-- applicable.
listJobs_maxResults :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Int)
listJobs_maxResults = Lens.lens (\ListJobs' {maxResults} -> maxResults) (\s@ListJobs' {} a -> s {maxResults = a} :: ListJobs)

-- | The job ID for a multi-node parallel job. Specifying a multi-node
-- parallel job ID with this parameter lists all nodes that are associated
-- with the specified job.
listJobs_multiNodeJobId :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_multiNodeJobId = Lens.lens (\ListJobs' {multiNodeJobId} -> multiNodeJobId) (\s@ListJobs' {} a -> s {multiNodeJobId = a} :: ListJobs)

-- | The @nextToken@ value returned from a previous paginated @ListJobs@
-- request where @maxResults@ was used and the results exceeded the value
-- of that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value. This value is @null@ when
-- there are no more results to return.
--
-- Treat this token as an opaque identifier that\'s only used to retrieve
-- the next items in a list and not for other programmatic purposes.
listJobs_nextToken :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_nextToken = Lens.lens (\ListJobs' {nextToken} -> nextToken) (\s@ListJobs' {} a -> s {nextToken = a} :: ListJobs)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "jobSummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListJobs where
  hashWithSalt _salt ListJobs' {..} =
    _salt `Prelude.hashWithSalt` arrayJobId
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` jobQueue
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` multiNodeJobId
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListJobs where
  rnf ListJobs' {..} =
    Prelude.rnf arrayJobId
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf jobQueue
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf multiNodeJobId
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListJobs where
  toJSON ListJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("arrayJobId" Data..=) Prelude.<$> arrayJobId,
            ("filters" Data..=) Prelude.<$> filters,
            ("jobQueue" Data..=) Prelude.<$> jobQueue,
            ("jobStatus" Data..=) Prelude.<$> jobStatus,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("multiNodeJobId" Data..=)
              Prelude.<$> multiNodeJobId,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListJobs where
  toPath = Prelude.const "/v1/listjobs"

instance Data.ToQuery ListJobs where
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
listJobsResponse_jobSummaryList = Lens.lens (\ListJobsResponse' {jobSummaryList} -> jobSummaryList) (\s@ListJobsResponse' {} a -> s {jobSummaryList = a} :: ListJobsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListJobsResponse where
  rnf ListJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobSummaryList
