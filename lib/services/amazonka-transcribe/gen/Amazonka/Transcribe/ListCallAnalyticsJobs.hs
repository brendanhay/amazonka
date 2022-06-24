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
-- Module      : Amazonka.Transcribe.ListCallAnalyticsJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List call analytics jobs with a specified status or substring that
-- matches their names.
module Amazonka.Transcribe.ListCallAnalyticsJobs
  ( -- * Creating a Request
    ListCallAnalyticsJobs (..),
    newListCallAnalyticsJobs,

    -- * Request Lenses
    listCallAnalyticsJobs_nextToken,
    listCallAnalyticsJobs_status,
    listCallAnalyticsJobs_maxResults,
    listCallAnalyticsJobs_jobNameContains,

    -- * Destructuring the Response
    ListCallAnalyticsJobsResponse (..),
    newListCallAnalyticsJobsResponse,

    -- * Response Lenses
    listCallAnalyticsJobsResponse_nextToken,
    listCallAnalyticsJobsResponse_status,
    listCallAnalyticsJobsResponse_callAnalyticsJobSummaries,
    listCallAnalyticsJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newListCallAnalyticsJobs' smart constructor.
data ListCallAnalyticsJobs = ListCallAnalyticsJobs'
  { -- | If you receive a truncated result in the previous request of , include
    -- @NextToken@ to fetch the next set of jobs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | When specified, returns only call analytics jobs with the specified
    -- status. Jobs are ordered by creation date, with the most recent jobs
    -- returned first. If you don\'t specify a status, Amazon Transcribe
    -- returns all analytics jobs ordered by creation date.
    status :: Prelude.Maybe CallAnalyticsJobStatus,
    -- | The maximum number of call analytics jobs to return in each page of
    -- results. If there are fewer results than the value you specify, only the
    -- actual results are returned. If you do not specify a value, the default
    -- of 5 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When specified, the jobs returned in the list are limited to jobs whose
    -- name contains the specified string.
    jobNameContains :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCallAnalyticsJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCallAnalyticsJobs_nextToken' - If you receive a truncated result in the previous request of , include
-- @NextToken@ to fetch the next set of jobs.
--
-- 'status', 'listCallAnalyticsJobs_status' - When specified, returns only call analytics jobs with the specified
-- status. Jobs are ordered by creation date, with the most recent jobs
-- returned first. If you don\'t specify a status, Amazon Transcribe
-- returns all analytics jobs ordered by creation date.
--
-- 'maxResults', 'listCallAnalyticsJobs_maxResults' - The maximum number of call analytics jobs to return in each page of
-- results. If there are fewer results than the value you specify, only the
-- actual results are returned. If you do not specify a value, the default
-- of 5 is used.
--
-- 'jobNameContains', 'listCallAnalyticsJobs_jobNameContains' - When specified, the jobs returned in the list are limited to jobs whose
-- name contains the specified string.
newListCallAnalyticsJobs ::
  ListCallAnalyticsJobs
newListCallAnalyticsJobs =
  ListCallAnalyticsJobs'
    { nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      jobNameContains = Prelude.Nothing
    }

-- | If you receive a truncated result in the previous request of , include
-- @NextToken@ to fetch the next set of jobs.
listCallAnalyticsJobs_nextToken :: Lens.Lens' ListCallAnalyticsJobs (Prelude.Maybe Prelude.Text)
listCallAnalyticsJobs_nextToken = Lens.lens (\ListCallAnalyticsJobs' {nextToken} -> nextToken) (\s@ListCallAnalyticsJobs' {} a -> s {nextToken = a} :: ListCallAnalyticsJobs)

-- | When specified, returns only call analytics jobs with the specified
-- status. Jobs are ordered by creation date, with the most recent jobs
-- returned first. If you don\'t specify a status, Amazon Transcribe
-- returns all analytics jobs ordered by creation date.
listCallAnalyticsJobs_status :: Lens.Lens' ListCallAnalyticsJobs (Prelude.Maybe CallAnalyticsJobStatus)
listCallAnalyticsJobs_status = Lens.lens (\ListCallAnalyticsJobs' {status} -> status) (\s@ListCallAnalyticsJobs' {} a -> s {status = a} :: ListCallAnalyticsJobs)

-- | The maximum number of call analytics jobs to return in each page of
-- results. If there are fewer results than the value you specify, only the
-- actual results are returned. If you do not specify a value, the default
-- of 5 is used.
listCallAnalyticsJobs_maxResults :: Lens.Lens' ListCallAnalyticsJobs (Prelude.Maybe Prelude.Natural)
listCallAnalyticsJobs_maxResults = Lens.lens (\ListCallAnalyticsJobs' {maxResults} -> maxResults) (\s@ListCallAnalyticsJobs' {} a -> s {maxResults = a} :: ListCallAnalyticsJobs)

-- | When specified, the jobs returned in the list are limited to jobs whose
-- name contains the specified string.
listCallAnalyticsJobs_jobNameContains :: Lens.Lens' ListCallAnalyticsJobs (Prelude.Maybe Prelude.Text)
listCallAnalyticsJobs_jobNameContains = Lens.lens (\ListCallAnalyticsJobs' {jobNameContains} -> jobNameContains) (\s@ListCallAnalyticsJobs' {} a -> s {jobNameContains = a} :: ListCallAnalyticsJobs)

instance Core.AWSRequest ListCallAnalyticsJobs where
  type
    AWSResponse ListCallAnalyticsJobs =
      ListCallAnalyticsJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCallAnalyticsJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> ( x Core..?> "CallAnalyticsJobSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCallAnalyticsJobs where
  hashWithSalt _salt ListCallAnalyticsJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` jobNameContains

instance Prelude.NFData ListCallAnalyticsJobs where
  rnf ListCallAnalyticsJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf jobNameContains

instance Core.ToHeaders ListCallAnalyticsJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.ListCallAnalyticsJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListCallAnalyticsJobs where
  toJSON ListCallAnalyticsJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Status" Core..=) Prelude.<$> status,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("JobNameContains" Core..=)
              Prelude.<$> jobNameContains
          ]
      )

instance Core.ToPath ListCallAnalyticsJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListCallAnalyticsJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCallAnalyticsJobsResponse' smart constructor.
data ListCallAnalyticsJobsResponse = ListCallAnalyticsJobsResponse'
  { -- | The operation returns a page of jobs at a time. The maximum size of the
    -- page is set by the @MaxResults@ parameter. If there are more jobs in the
    -- list than the page size, Amazon Transcribe returns the @NextPage@ token.
    -- Include the token in your next request to the operation to return next
    -- page of jobs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | When specified, returns only call analytics jobs with that status. Jobs
    -- are ordered by creation date, with the most recent jobs returned first.
    -- If you don\'t specify a status, Amazon Transcribe returns all
    -- transcription jobs ordered by creation date.
    status :: Prelude.Maybe CallAnalyticsJobStatus,
    -- | A list of objects containing summary information for a transcription
    -- job.
    callAnalyticsJobSummaries :: Prelude.Maybe [CallAnalyticsJobSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCallAnalyticsJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCallAnalyticsJobsResponse_nextToken' - The operation returns a page of jobs at a time. The maximum size of the
-- page is set by the @MaxResults@ parameter. If there are more jobs in the
-- list than the page size, Amazon Transcribe returns the @NextPage@ token.
-- Include the token in your next request to the operation to return next
-- page of jobs.
--
-- 'status', 'listCallAnalyticsJobsResponse_status' - When specified, returns only call analytics jobs with that status. Jobs
-- are ordered by creation date, with the most recent jobs returned first.
-- If you don\'t specify a status, Amazon Transcribe returns all
-- transcription jobs ordered by creation date.
--
-- 'callAnalyticsJobSummaries', 'listCallAnalyticsJobsResponse_callAnalyticsJobSummaries' - A list of objects containing summary information for a transcription
-- job.
--
-- 'httpStatus', 'listCallAnalyticsJobsResponse_httpStatus' - The response's http status code.
newListCallAnalyticsJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCallAnalyticsJobsResponse
newListCallAnalyticsJobsResponse pHttpStatus_ =
  ListCallAnalyticsJobsResponse'
    { nextToken =
        Prelude.Nothing,
      status = Prelude.Nothing,
      callAnalyticsJobSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The operation returns a page of jobs at a time. The maximum size of the
-- page is set by the @MaxResults@ parameter. If there are more jobs in the
-- list than the page size, Amazon Transcribe returns the @NextPage@ token.
-- Include the token in your next request to the operation to return next
-- page of jobs.
listCallAnalyticsJobsResponse_nextToken :: Lens.Lens' ListCallAnalyticsJobsResponse (Prelude.Maybe Prelude.Text)
listCallAnalyticsJobsResponse_nextToken = Lens.lens (\ListCallAnalyticsJobsResponse' {nextToken} -> nextToken) (\s@ListCallAnalyticsJobsResponse' {} a -> s {nextToken = a} :: ListCallAnalyticsJobsResponse)

-- | When specified, returns only call analytics jobs with that status. Jobs
-- are ordered by creation date, with the most recent jobs returned first.
-- If you don\'t specify a status, Amazon Transcribe returns all
-- transcription jobs ordered by creation date.
listCallAnalyticsJobsResponse_status :: Lens.Lens' ListCallAnalyticsJobsResponse (Prelude.Maybe CallAnalyticsJobStatus)
listCallAnalyticsJobsResponse_status = Lens.lens (\ListCallAnalyticsJobsResponse' {status} -> status) (\s@ListCallAnalyticsJobsResponse' {} a -> s {status = a} :: ListCallAnalyticsJobsResponse)

-- | A list of objects containing summary information for a transcription
-- job.
listCallAnalyticsJobsResponse_callAnalyticsJobSummaries :: Lens.Lens' ListCallAnalyticsJobsResponse (Prelude.Maybe [CallAnalyticsJobSummary])
listCallAnalyticsJobsResponse_callAnalyticsJobSummaries = Lens.lens (\ListCallAnalyticsJobsResponse' {callAnalyticsJobSummaries} -> callAnalyticsJobSummaries) (\s@ListCallAnalyticsJobsResponse' {} a -> s {callAnalyticsJobSummaries = a} :: ListCallAnalyticsJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCallAnalyticsJobsResponse_httpStatus :: Lens.Lens' ListCallAnalyticsJobsResponse Prelude.Int
listCallAnalyticsJobsResponse_httpStatus = Lens.lens (\ListCallAnalyticsJobsResponse' {httpStatus} -> httpStatus) (\s@ListCallAnalyticsJobsResponse' {} a -> s {httpStatus = a} :: ListCallAnalyticsJobsResponse)

instance Prelude.NFData ListCallAnalyticsJobsResponse where
  rnf ListCallAnalyticsJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf callAnalyticsJobSummaries
      `Prelude.seq` Prelude.rnf httpStatus
