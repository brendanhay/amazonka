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
-- Module      : Amazonka.DataBrew.ListJobRuns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the previous runs of a particular DataBrew job.
--
-- This operation returns paginated results.
module Amazonka.DataBrew.ListJobRuns
  ( -- * Creating a Request
    ListJobRuns (..),
    newListJobRuns,

    -- * Request Lenses
    listJobRuns_maxResults,
    listJobRuns_nextToken,
    listJobRuns_name,

    -- * Destructuring the Response
    ListJobRunsResponse (..),
    newListJobRunsResponse,

    -- * Response Lenses
    listJobRunsResponse_nextToken,
    listJobRunsResponse_httpStatus,
    listJobRunsResponse_jobRuns,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListJobRuns' smart constructor.
data ListJobRuns = ListJobRuns'
  { -- | The maximum number of results to return in this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the job.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listJobRuns_maxResults' - The maximum number of results to return in this request.
--
-- 'nextToken', 'listJobRuns_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'name', 'listJobRuns_name' - The name of the job.
newListJobRuns ::
  -- | 'name'
  Prelude.Text ->
  ListJobRuns
newListJobRuns pName_ =
  ListJobRuns'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      name = pName_
    }

-- | The maximum number of results to return in this request.
listJobRuns_maxResults :: Lens.Lens' ListJobRuns (Prelude.Maybe Prelude.Natural)
listJobRuns_maxResults = Lens.lens (\ListJobRuns' {maxResults} -> maxResults) (\s@ListJobRuns' {} a -> s {maxResults = a} :: ListJobRuns)

-- | The token returned by a previous call to retrieve the next set of
-- results.
listJobRuns_nextToken :: Lens.Lens' ListJobRuns (Prelude.Maybe Prelude.Text)
listJobRuns_nextToken = Lens.lens (\ListJobRuns' {nextToken} -> nextToken) (\s@ListJobRuns' {} a -> s {nextToken = a} :: ListJobRuns)

-- | The name of the job.
listJobRuns_name :: Lens.Lens' ListJobRuns Prelude.Text
listJobRuns_name = Lens.lens (\ListJobRuns' {name} -> name) (\s@ListJobRuns' {} a -> s {name = a} :: ListJobRuns)

instance Core.AWSPager ListJobRuns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobRunsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listJobRunsResponse_jobRuns) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listJobRuns_nextToken
              Lens..~ rs
              Lens.^? listJobRunsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListJobRuns where
  type AWSResponse ListJobRuns = ListJobRunsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobRunsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "JobRuns" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListJobRuns where
  hashWithSalt _salt ListJobRuns' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` name

instance Prelude.NFData ListJobRuns where
  rnf ListJobRuns' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf name

instance Data.ToHeaders ListJobRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListJobRuns where
  toPath ListJobRuns' {..} =
    Prelude.mconcat
      ["/jobs/", Data.toBS name, "/jobRuns"]

instance Data.ToQuery ListJobRuns where
  toQuery ListJobRuns' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListJobRunsResponse' smart constructor.
data ListJobRunsResponse = ListJobRunsResponse'
  { -- | A token that you can use in a subsequent call to retrieve the next set
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of job runs that have occurred for the specified job.
    jobRuns :: [JobRun]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobRunsResponse_nextToken' - A token that you can use in a subsequent call to retrieve the next set
-- of results.
--
-- 'httpStatus', 'listJobRunsResponse_httpStatus' - The response's http status code.
--
-- 'jobRuns', 'listJobRunsResponse_jobRuns' - A list of job runs that have occurred for the specified job.
newListJobRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJobRunsResponse
newListJobRunsResponse pHttpStatus_ =
  ListJobRunsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      jobRuns = Prelude.mempty
    }

-- | A token that you can use in a subsequent call to retrieve the next set
-- of results.
listJobRunsResponse_nextToken :: Lens.Lens' ListJobRunsResponse (Prelude.Maybe Prelude.Text)
listJobRunsResponse_nextToken = Lens.lens (\ListJobRunsResponse' {nextToken} -> nextToken) (\s@ListJobRunsResponse' {} a -> s {nextToken = a} :: ListJobRunsResponse)

-- | The response's http status code.
listJobRunsResponse_httpStatus :: Lens.Lens' ListJobRunsResponse Prelude.Int
listJobRunsResponse_httpStatus = Lens.lens (\ListJobRunsResponse' {httpStatus} -> httpStatus) (\s@ListJobRunsResponse' {} a -> s {httpStatus = a} :: ListJobRunsResponse)

-- | A list of job runs that have occurred for the specified job.
listJobRunsResponse_jobRuns :: Lens.Lens' ListJobRunsResponse [JobRun]
listJobRunsResponse_jobRuns = Lens.lens (\ListJobRunsResponse' {jobRuns} -> jobRuns) (\s@ListJobRunsResponse' {} a -> s {jobRuns = a} :: ListJobRunsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListJobRunsResponse where
  rnf ListJobRunsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf jobRuns
