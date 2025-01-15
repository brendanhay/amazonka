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
-- Module      : Amazonka.EMRServerless.ListJobRuns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists job runs based on a set of parameters.
--
-- This operation returns paginated results.
module Amazonka.EMRServerless.ListJobRuns
  ( -- * Creating a Request
    ListJobRuns (..),
    newListJobRuns,

    -- * Request Lenses
    listJobRuns_createdAtAfter,
    listJobRuns_createdAtBefore,
    listJobRuns_maxResults,
    listJobRuns_nextToken,
    listJobRuns_states,
    listJobRuns_applicationId,

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
import Amazonka.EMRServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListJobRuns' smart constructor.
data ListJobRuns = ListJobRuns'
  { -- | The lower bound of the option to filter by creation date and time.
    createdAtAfter :: Prelude.Maybe Data.POSIX,
    -- | The upper bound of the option to filter by creation date and time.
    createdAtBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of job runs that can be listed.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of job run results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional filter for job run states. Note that if this filter contains
    -- multiple states, the resulting list will be grouped by the state.
    states :: Prelude.Maybe [JobRunState],
    -- | The ID of the application for which to list the job run.
    applicationId :: Prelude.Text
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
-- 'createdAtAfter', 'listJobRuns_createdAtAfter' - The lower bound of the option to filter by creation date and time.
--
-- 'createdAtBefore', 'listJobRuns_createdAtBefore' - The upper bound of the option to filter by creation date and time.
--
-- 'maxResults', 'listJobRuns_maxResults' - The maximum number of job runs that can be listed.
--
-- 'nextToken', 'listJobRuns_nextToken' - The token for the next set of job run results.
--
-- 'states', 'listJobRuns_states' - An optional filter for job run states. Note that if this filter contains
-- multiple states, the resulting list will be grouped by the state.
--
-- 'applicationId', 'listJobRuns_applicationId' - The ID of the application for which to list the job run.
newListJobRuns ::
  -- | 'applicationId'
  Prelude.Text ->
  ListJobRuns
newListJobRuns pApplicationId_ =
  ListJobRuns'
    { createdAtAfter = Prelude.Nothing,
      createdAtBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      states = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The lower bound of the option to filter by creation date and time.
listJobRuns_createdAtAfter :: Lens.Lens' ListJobRuns (Prelude.Maybe Prelude.UTCTime)
listJobRuns_createdAtAfter = Lens.lens (\ListJobRuns' {createdAtAfter} -> createdAtAfter) (\s@ListJobRuns' {} a -> s {createdAtAfter = a} :: ListJobRuns) Prelude.. Lens.mapping Data._Time

-- | The upper bound of the option to filter by creation date and time.
listJobRuns_createdAtBefore :: Lens.Lens' ListJobRuns (Prelude.Maybe Prelude.UTCTime)
listJobRuns_createdAtBefore = Lens.lens (\ListJobRuns' {createdAtBefore} -> createdAtBefore) (\s@ListJobRuns' {} a -> s {createdAtBefore = a} :: ListJobRuns) Prelude.. Lens.mapping Data._Time

-- | The maximum number of job runs that can be listed.
listJobRuns_maxResults :: Lens.Lens' ListJobRuns (Prelude.Maybe Prelude.Natural)
listJobRuns_maxResults = Lens.lens (\ListJobRuns' {maxResults} -> maxResults) (\s@ListJobRuns' {} a -> s {maxResults = a} :: ListJobRuns)

-- | The token for the next set of job run results.
listJobRuns_nextToken :: Lens.Lens' ListJobRuns (Prelude.Maybe Prelude.Text)
listJobRuns_nextToken = Lens.lens (\ListJobRuns' {nextToken} -> nextToken) (\s@ListJobRuns' {} a -> s {nextToken = a} :: ListJobRuns)

-- | An optional filter for job run states. Note that if this filter contains
-- multiple states, the resulting list will be grouped by the state.
listJobRuns_states :: Lens.Lens' ListJobRuns (Prelude.Maybe [JobRunState])
listJobRuns_states = Lens.lens (\ListJobRuns' {states} -> states) (\s@ListJobRuns' {} a -> s {states = a} :: ListJobRuns) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the application for which to list the job run.
listJobRuns_applicationId :: Lens.Lens' ListJobRuns Prelude.Text
listJobRuns_applicationId = Lens.lens (\ListJobRuns' {applicationId} -> applicationId) (\s@ListJobRuns' {} a -> s {applicationId = a} :: ListJobRuns)

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
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "jobRuns" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListJobRuns where
  hashWithSalt _salt ListJobRuns' {..} =
    _salt
      `Prelude.hashWithSalt` createdAtAfter
      `Prelude.hashWithSalt` createdAtBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` states
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ListJobRuns where
  rnf ListJobRuns' {..} =
    Prelude.rnf createdAtAfter `Prelude.seq`
      Prelude.rnf createdAtBefore `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf states `Prelude.seq`
              Prelude.rnf applicationId

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
      [ "/applications/",
        Data.toBS applicationId,
        "/jobruns"
      ]

instance Data.ToQuery ListJobRuns where
  toQuery ListJobRuns' {..} =
    Prelude.mconcat
      [ "createdAtAfter" Data.=: createdAtAfter,
        "createdAtBefore" Data.=: createdAtBefore,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "states"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> states)
      ]

-- | /See:/ 'newListJobRunsResponse' smart constructor.
data ListJobRunsResponse = ListJobRunsResponse'
  { -- | The output displays the token for the next set of job run results. This
    -- is required for pagination and is available as a response of the
    -- previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The output lists information about the specified job runs.
    jobRuns :: [JobRunSummary]
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
-- 'nextToken', 'listJobRunsResponse_nextToken' - The output displays the token for the next set of job run results. This
-- is required for pagination and is available as a response of the
-- previous request.
--
-- 'httpStatus', 'listJobRunsResponse_httpStatus' - The response's http status code.
--
-- 'jobRuns', 'listJobRunsResponse_jobRuns' - The output lists information about the specified job runs.
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

-- | The output displays the token for the next set of job run results. This
-- is required for pagination and is available as a response of the
-- previous request.
listJobRunsResponse_nextToken :: Lens.Lens' ListJobRunsResponse (Prelude.Maybe Prelude.Text)
listJobRunsResponse_nextToken = Lens.lens (\ListJobRunsResponse' {nextToken} -> nextToken) (\s@ListJobRunsResponse' {} a -> s {nextToken = a} :: ListJobRunsResponse)

-- | The response's http status code.
listJobRunsResponse_httpStatus :: Lens.Lens' ListJobRunsResponse Prelude.Int
listJobRunsResponse_httpStatus = Lens.lens (\ListJobRunsResponse' {httpStatus} -> httpStatus) (\s@ListJobRunsResponse' {} a -> s {httpStatus = a} :: ListJobRunsResponse)

-- | The output lists information about the specified job runs.
listJobRunsResponse_jobRuns :: Lens.Lens' ListJobRunsResponse [JobRunSummary]
listJobRunsResponse_jobRuns = Lens.lens (\ListJobRunsResponse' {jobRuns} -> jobRuns) (\s@ListJobRunsResponse' {} a -> s {jobRuns = a} :: ListJobRunsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListJobRunsResponse where
  rnf ListJobRunsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf jobRuns
