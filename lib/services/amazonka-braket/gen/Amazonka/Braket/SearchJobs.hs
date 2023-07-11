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
-- Module      : Amazonka.Braket.SearchJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for Amazon Braket jobs that match the specified filter values.
--
-- This operation returns paginated results.
module Amazonka.Braket.SearchJobs
  ( -- * Creating a Request
    SearchJobs (..),
    newSearchJobs,

    -- * Request Lenses
    searchJobs_maxResults,
    searchJobs_nextToken,
    searchJobs_filters,

    -- * Destructuring the Response
    SearchJobsResponse (..),
    newSearchJobsResponse,

    -- * Response Lenses
    searchJobsResponse_nextToken,
    searchJobsResponse_httpStatus,
    searchJobsResponse_jobs,
  )
where

import Amazonka.Braket.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchJobs' smart constructor.
data SearchJobs = SearchJobs'
  { -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token used for pagination of results returned in the response. Use the
    -- token returned from the previous request to continue results where the
    -- previous request ended.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The filter values to use when searching for a job.
    filters :: [SearchJobsFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'searchJobs_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'searchJobs_nextToken' - A token used for pagination of results returned in the response. Use the
-- token returned from the previous request to continue results where the
-- previous request ended.
--
-- 'filters', 'searchJobs_filters' - The filter values to use when searching for a job.
newSearchJobs ::
  SearchJobs
newSearchJobs =
  SearchJobs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filters = Prelude.mempty
    }

-- | The maximum number of results to return in the response.
searchJobs_maxResults :: Lens.Lens' SearchJobs (Prelude.Maybe Prelude.Natural)
searchJobs_maxResults = Lens.lens (\SearchJobs' {maxResults} -> maxResults) (\s@SearchJobs' {} a -> s {maxResults = a} :: SearchJobs)

-- | A token used for pagination of results returned in the response. Use the
-- token returned from the previous request to continue results where the
-- previous request ended.
searchJobs_nextToken :: Lens.Lens' SearchJobs (Prelude.Maybe Prelude.Text)
searchJobs_nextToken = Lens.lens (\SearchJobs' {nextToken} -> nextToken) (\s@SearchJobs' {} a -> s {nextToken = a} :: SearchJobs)

-- | The filter values to use when searching for a job.
searchJobs_filters :: Lens.Lens' SearchJobs [SearchJobsFilter]
searchJobs_filters = Lens.lens (\SearchJobs' {filters} -> filters) (\s@SearchJobs' {} a -> s {filters = a} :: SearchJobs) Prelude.. Lens.coerced

instance Core.AWSPager SearchJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. searchJobsResponse_jobs) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchJobs_nextToken
          Lens..~ rs
          Lens.^? searchJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchJobs where
  type AWSResponse SearchJobs = SearchJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchJobsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "jobs" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable SearchJobs where
  hashWithSalt _salt SearchJobs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters

instance Prelude.NFData SearchJobs where
  rnf SearchJobs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters

instance Data.ToHeaders SearchJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchJobs where
  toJSON SearchJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("filters" Data..= filters)
          ]
      )

instance Data.ToPath SearchJobs where
  toPath = Prelude.const "/jobs"

instance Data.ToQuery SearchJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchJobsResponse' smart constructor.
data SearchJobsResponse = SearchJobsResponse'
  { -- | A token used for pagination of results, or @null@ if there are no
    -- additional results. Use the token value in a subsequent request to
    -- continue results where the previous request ended.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of @JobSummary@ objects for devices that match the specified
    -- filter values.
    jobs :: [JobSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchJobsResponse_nextToken' - A token used for pagination of results, or @null@ if there are no
-- additional results. Use the token value in a subsequent request to
-- continue results where the previous request ended.
--
-- 'httpStatus', 'searchJobsResponse_httpStatus' - The response's http status code.
--
-- 'jobs', 'searchJobsResponse_jobs' - An array of @JobSummary@ objects for devices that match the specified
-- filter values.
newSearchJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchJobsResponse
newSearchJobsResponse pHttpStatus_ =
  SearchJobsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      jobs = Prelude.mempty
    }

-- | A token used for pagination of results, or @null@ if there are no
-- additional results. Use the token value in a subsequent request to
-- continue results where the previous request ended.
searchJobsResponse_nextToken :: Lens.Lens' SearchJobsResponse (Prelude.Maybe Prelude.Text)
searchJobsResponse_nextToken = Lens.lens (\SearchJobsResponse' {nextToken} -> nextToken) (\s@SearchJobsResponse' {} a -> s {nextToken = a} :: SearchJobsResponse)

-- | The response's http status code.
searchJobsResponse_httpStatus :: Lens.Lens' SearchJobsResponse Prelude.Int
searchJobsResponse_httpStatus = Lens.lens (\SearchJobsResponse' {httpStatus} -> httpStatus) (\s@SearchJobsResponse' {} a -> s {httpStatus = a} :: SearchJobsResponse)

-- | An array of @JobSummary@ objects for devices that match the specified
-- filter values.
searchJobsResponse_jobs :: Lens.Lens' SearchJobsResponse [JobSummary]
searchJobsResponse_jobs = Lens.lens (\SearchJobsResponse' {jobs} -> jobs) (\s@SearchJobsResponse' {} a -> s {jobs = a} :: SearchJobsResponse) Prelude.. Lens.coerced

instance Prelude.NFData SearchJobsResponse where
  rnf SearchJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobs
