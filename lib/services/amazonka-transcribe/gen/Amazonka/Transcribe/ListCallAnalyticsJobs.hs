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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of Call Analytics jobs that match the specified
-- criteria. If no criteria are specified, all Call Analytics jobs are
-- returned.
--
-- To get detailed information about a specific Call Analytics job, use the
-- operation.
module Amazonka.Transcribe.ListCallAnalyticsJobs
  ( -- * Creating a Request
    ListCallAnalyticsJobs (..),
    newListCallAnalyticsJobs,

    -- * Request Lenses
    listCallAnalyticsJobs_jobNameContains,
    listCallAnalyticsJobs_maxResults,
    listCallAnalyticsJobs_nextToken,
    listCallAnalyticsJobs_status,

    -- * Destructuring the Response
    ListCallAnalyticsJobsResponse (..),
    newListCallAnalyticsJobsResponse,

    -- * Response Lenses
    listCallAnalyticsJobsResponse_callAnalyticsJobSummaries,
    listCallAnalyticsJobsResponse_nextToken,
    listCallAnalyticsJobsResponse_status,
    listCallAnalyticsJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newListCallAnalyticsJobs' smart constructor.
data ListCallAnalyticsJobs = ListCallAnalyticsJobs'
  { -- | Returns only the Call Analytics jobs that contain the specified string.
    -- The search is not case sensitive.
    jobNameContains :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of Call Analytics jobs to return in each page of
    -- results. If there are fewer results than the value that you specify,
    -- only the actual results are returned. If you don\'t specify a value, a
    -- default of 5 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If your @ListCallAnalyticsJobs@ request returns more results than can be
    -- displayed, @NextToken@ is displayed in the response with an associated
    -- string. To get the next page of results, copy this string and repeat
    -- your request, including @NextToken@ with the value of the copied string.
    -- Repeat as needed to view all your results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns only Call Analytics jobs with the specified status. Jobs are
    -- ordered by creation date, with the newest job first. If you don\'t
    -- include @Status@, all Call Analytics jobs are returned.
    status :: Prelude.Maybe CallAnalyticsJobStatus
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
-- 'jobNameContains', 'listCallAnalyticsJobs_jobNameContains' - Returns only the Call Analytics jobs that contain the specified string.
-- The search is not case sensitive.
--
-- 'maxResults', 'listCallAnalyticsJobs_maxResults' - The maximum number of Call Analytics jobs to return in each page of
-- results. If there are fewer results than the value that you specify,
-- only the actual results are returned. If you don\'t specify a value, a
-- default of 5 is used.
--
-- 'nextToken', 'listCallAnalyticsJobs_nextToken' - If your @ListCallAnalyticsJobs@ request returns more results than can be
-- displayed, @NextToken@ is displayed in the response with an associated
-- string. To get the next page of results, copy this string and repeat
-- your request, including @NextToken@ with the value of the copied string.
-- Repeat as needed to view all your results.
--
-- 'status', 'listCallAnalyticsJobs_status' - Returns only Call Analytics jobs with the specified status. Jobs are
-- ordered by creation date, with the newest job first. If you don\'t
-- include @Status@, all Call Analytics jobs are returned.
newListCallAnalyticsJobs ::
  ListCallAnalyticsJobs
newListCallAnalyticsJobs =
  ListCallAnalyticsJobs'
    { jobNameContains =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Returns only the Call Analytics jobs that contain the specified string.
-- The search is not case sensitive.
listCallAnalyticsJobs_jobNameContains :: Lens.Lens' ListCallAnalyticsJobs (Prelude.Maybe Prelude.Text)
listCallAnalyticsJobs_jobNameContains = Lens.lens (\ListCallAnalyticsJobs' {jobNameContains} -> jobNameContains) (\s@ListCallAnalyticsJobs' {} a -> s {jobNameContains = a} :: ListCallAnalyticsJobs)

-- | The maximum number of Call Analytics jobs to return in each page of
-- results. If there are fewer results than the value that you specify,
-- only the actual results are returned. If you don\'t specify a value, a
-- default of 5 is used.
listCallAnalyticsJobs_maxResults :: Lens.Lens' ListCallAnalyticsJobs (Prelude.Maybe Prelude.Natural)
listCallAnalyticsJobs_maxResults = Lens.lens (\ListCallAnalyticsJobs' {maxResults} -> maxResults) (\s@ListCallAnalyticsJobs' {} a -> s {maxResults = a} :: ListCallAnalyticsJobs)

-- | If your @ListCallAnalyticsJobs@ request returns more results than can be
-- displayed, @NextToken@ is displayed in the response with an associated
-- string. To get the next page of results, copy this string and repeat
-- your request, including @NextToken@ with the value of the copied string.
-- Repeat as needed to view all your results.
listCallAnalyticsJobs_nextToken :: Lens.Lens' ListCallAnalyticsJobs (Prelude.Maybe Prelude.Text)
listCallAnalyticsJobs_nextToken = Lens.lens (\ListCallAnalyticsJobs' {nextToken} -> nextToken) (\s@ListCallAnalyticsJobs' {} a -> s {nextToken = a} :: ListCallAnalyticsJobs)

-- | Returns only Call Analytics jobs with the specified status. Jobs are
-- ordered by creation date, with the newest job first. If you don\'t
-- include @Status@, all Call Analytics jobs are returned.
listCallAnalyticsJobs_status :: Lens.Lens' ListCallAnalyticsJobs (Prelude.Maybe CallAnalyticsJobStatus)
listCallAnalyticsJobs_status = Lens.lens (\ListCallAnalyticsJobs' {status} -> status) (\s@ListCallAnalyticsJobs' {} a -> s {status = a} :: ListCallAnalyticsJobs)

instance Core.AWSRequest ListCallAnalyticsJobs where
  type
    AWSResponse ListCallAnalyticsJobs =
      ListCallAnalyticsJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCallAnalyticsJobsResponse'
            Prelude.<$> ( x Data..?> "CallAnalyticsJobSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCallAnalyticsJobs where
  hashWithSalt _salt ListCallAnalyticsJobs' {..} =
    _salt `Prelude.hashWithSalt` jobNameContains
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListCallAnalyticsJobs where
  rnf ListCallAnalyticsJobs' {..} =
    Prelude.rnf jobNameContains
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders ListCallAnalyticsJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.ListCallAnalyticsJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCallAnalyticsJobs where
  toJSON ListCallAnalyticsJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobNameContains" Data..=)
              Prelude.<$> jobNameContains,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Status" Data..=) Prelude.<$> status
          ]
      )

instance Data.ToPath ListCallAnalyticsJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCallAnalyticsJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCallAnalyticsJobsResponse' smart constructor.
data ListCallAnalyticsJobsResponse = ListCallAnalyticsJobsResponse'
  { -- | Provides a summary of information about each result.
    callAnalyticsJobSummaries :: Prelude.Maybe [CallAnalyticsJobSummary],
    -- | If @NextToken@ is present in your response, it indicates that not all
    -- results are displayed. To view the next set of results, copy the string
    -- associated with the @NextToken@ parameter in your results output, then
    -- run your request again including @NextToken@ with the value of the
    -- copied string. Repeat as needed to view all your results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists all Call Analytics jobs that have the status specified in your
    -- request. Jobs are ordered by creation date, with the newest job first.
    status :: Prelude.Maybe CallAnalyticsJobStatus,
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
-- 'callAnalyticsJobSummaries', 'listCallAnalyticsJobsResponse_callAnalyticsJobSummaries' - Provides a summary of information about each result.
--
-- 'nextToken', 'listCallAnalyticsJobsResponse_nextToken' - If @NextToken@ is present in your response, it indicates that not all
-- results are displayed. To view the next set of results, copy the string
-- associated with the @NextToken@ parameter in your results output, then
-- run your request again including @NextToken@ with the value of the
-- copied string. Repeat as needed to view all your results.
--
-- 'status', 'listCallAnalyticsJobsResponse_status' - Lists all Call Analytics jobs that have the status specified in your
-- request. Jobs are ordered by creation date, with the newest job first.
--
-- 'httpStatus', 'listCallAnalyticsJobsResponse_httpStatus' - The response's http status code.
newListCallAnalyticsJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCallAnalyticsJobsResponse
newListCallAnalyticsJobsResponse pHttpStatus_ =
  ListCallAnalyticsJobsResponse'
    { callAnalyticsJobSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides a summary of information about each result.
listCallAnalyticsJobsResponse_callAnalyticsJobSummaries :: Lens.Lens' ListCallAnalyticsJobsResponse (Prelude.Maybe [CallAnalyticsJobSummary])
listCallAnalyticsJobsResponse_callAnalyticsJobSummaries = Lens.lens (\ListCallAnalyticsJobsResponse' {callAnalyticsJobSummaries} -> callAnalyticsJobSummaries) (\s@ListCallAnalyticsJobsResponse' {} a -> s {callAnalyticsJobSummaries = a} :: ListCallAnalyticsJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If @NextToken@ is present in your response, it indicates that not all
-- results are displayed. To view the next set of results, copy the string
-- associated with the @NextToken@ parameter in your results output, then
-- run your request again including @NextToken@ with the value of the
-- copied string. Repeat as needed to view all your results.
listCallAnalyticsJobsResponse_nextToken :: Lens.Lens' ListCallAnalyticsJobsResponse (Prelude.Maybe Prelude.Text)
listCallAnalyticsJobsResponse_nextToken = Lens.lens (\ListCallAnalyticsJobsResponse' {nextToken} -> nextToken) (\s@ListCallAnalyticsJobsResponse' {} a -> s {nextToken = a} :: ListCallAnalyticsJobsResponse)

-- | Lists all Call Analytics jobs that have the status specified in your
-- request. Jobs are ordered by creation date, with the newest job first.
listCallAnalyticsJobsResponse_status :: Lens.Lens' ListCallAnalyticsJobsResponse (Prelude.Maybe CallAnalyticsJobStatus)
listCallAnalyticsJobsResponse_status = Lens.lens (\ListCallAnalyticsJobsResponse' {status} -> status) (\s@ListCallAnalyticsJobsResponse' {} a -> s {status = a} :: ListCallAnalyticsJobsResponse)

-- | The response's http status code.
listCallAnalyticsJobsResponse_httpStatus :: Lens.Lens' ListCallAnalyticsJobsResponse Prelude.Int
listCallAnalyticsJobsResponse_httpStatus = Lens.lens (\ListCallAnalyticsJobsResponse' {httpStatus} -> httpStatus) (\s@ListCallAnalyticsJobsResponse' {} a -> s {httpStatus = a} :: ListCallAnalyticsJobsResponse)

instance Prelude.NFData ListCallAnalyticsJobsResponse where
  rnf ListCallAnalyticsJobsResponse' {..} =
    Prelude.rnf callAnalyticsJobSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
