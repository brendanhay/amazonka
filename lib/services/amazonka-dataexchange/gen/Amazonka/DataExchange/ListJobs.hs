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
-- Module      : Amazonka.DataExchange.ListJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists your jobs sorted by CreatedAt in descending order.
--
-- This operation returns paginated results.
module Amazonka.DataExchange.ListJobs
  ( -- * Creating a Request
    ListJobs (..),
    newListJobs,

    -- * Request Lenses
    listJobs_dataSetId,
    listJobs_maxResults,
    listJobs_nextToken,
    listJobs_revisionId,

    -- * Destructuring the Response
    ListJobsResponse (..),
    newListJobsResponse,

    -- * Response Lenses
    listJobsResponse_jobs,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListJobs' smart constructor.
data ListJobs = ListJobs'
  { -- | The unique identifier for a data set.
    dataSetId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results returned by a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for a revision.
    revisionId :: Prelude.Maybe Prelude.Text
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
-- 'dataSetId', 'listJobs_dataSetId' - The unique identifier for a data set.
--
-- 'maxResults', 'listJobs_maxResults' - The maximum number of results returned by a single call.
--
-- 'nextToken', 'listJobs_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'revisionId', 'listJobs_revisionId' - The unique identifier for a revision.
newListJobs ::
  ListJobs
newListJobs =
  ListJobs'
    { dataSetId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      revisionId = Prelude.Nothing
    }

-- | The unique identifier for a data set.
listJobs_dataSetId :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_dataSetId = Lens.lens (\ListJobs' {dataSetId} -> dataSetId) (\s@ListJobs' {} a -> s {dataSetId = a} :: ListJobs)

-- | The maximum number of results returned by a single call.
listJobs_maxResults :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Natural)
listJobs_maxResults = Lens.lens (\ListJobs' {maxResults} -> maxResults) (\s@ListJobs' {} a -> s {maxResults = a} :: ListJobs)

-- | The token value retrieved from a previous call to access the next page
-- of results.
listJobs_nextToken :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_nextToken = Lens.lens (\ListJobs' {nextToken} -> nextToken) (\s@ListJobs' {} a -> s {nextToken = a} :: ListJobs)

-- | The unique identifier for a revision.
listJobs_revisionId :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_revisionId = Lens.lens (\ListJobs' {revisionId} -> revisionId) (\s@ListJobs' {} a -> s {revisionId = a} :: ListJobs)

instance Core.AWSPager ListJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobsResponse_jobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listJobs_nextToken
              Lens..~ rs
              Lens.^? listJobsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListJobs where
  type AWSResponse ListJobs = ListJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Prelude.<$> (x Data..?> "Jobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJobs where
  hashWithSalt _salt ListJobs' {..} =
    _salt
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` revisionId

instance Prelude.NFData ListJobs where
  rnf ListJobs' {..} =
    Prelude.rnf dataSetId `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf revisionId

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

instance Data.ToPath ListJobs where
  toPath = Prelude.const "/v1/jobs"

instance Data.ToQuery ListJobs where
  toQuery ListJobs' {..} =
    Prelude.mconcat
      [ "dataSetId" Data.=: dataSetId,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "revisionId" Data.=: revisionId
      ]

-- | /See:/ 'newListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { -- | The jobs listed by the request.
    jobs :: Prelude.Maybe [JobEntry],
    -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'jobs', 'listJobsResponse_jobs' - The jobs listed by the request.
--
-- 'nextToken', 'listJobsResponse_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'httpStatus', 'listJobsResponse_httpStatus' - The response's http status code.
newListJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJobsResponse
newListJobsResponse pHttpStatus_ =
  ListJobsResponse'
    { jobs = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The jobs listed by the request.
listJobsResponse_jobs :: Lens.Lens' ListJobsResponse (Prelude.Maybe [JobEntry])
listJobsResponse_jobs = Lens.lens (\ListJobsResponse' {jobs} -> jobs) (\s@ListJobsResponse' {} a -> s {jobs = a} :: ListJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token value retrieved from a previous call to access the next page
-- of results.
listJobsResponse_nextToken :: Lens.Lens' ListJobsResponse (Prelude.Maybe Prelude.Text)
listJobsResponse_nextToken = Lens.lens (\ListJobsResponse' {nextToken} -> nextToken) (\s@ListJobsResponse' {} a -> s {nextToken = a} :: ListJobsResponse)

-- | The response's http status code.
listJobsResponse_httpStatus :: Lens.Lens' ListJobsResponse Prelude.Int
listJobsResponse_httpStatus = Lens.lens (\ListJobsResponse' {httpStatus} -> httpStatus) (\s@ListJobsResponse' {} a -> s {httpStatus = a} :: ListJobsResponse)

instance Prelude.NFData ListJobsResponse where
  rnf ListJobsResponse' {..} =
    Prelude.rnf jobs `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
