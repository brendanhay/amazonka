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
-- Module      : Amazonka.EMRContainers.ListJobRuns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists job runs based on a set of parameters. A job run is a unit of
-- work, such as a Spark jar, PySpark script, or SparkSQL query, that you
-- submit to Amazon EMR on EKS.
--
-- This operation returns paginated results.
module Amazonka.EMRContainers.ListJobRuns
  ( -- * Creating a Request
    ListJobRuns (..),
    newListJobRuns,

    -- * Request Lenses
    listJobRuns_states,
    listJobRuns_createdAfter,
    listJobRuns_nextToken,
    listJobRuns_name,
    listJobRuns_maxResults,
    listJobRuns_createdBefore,
    listJobRuns_virtualClusterId,

    -- * Destructuring the Response
    ListJobRunsResponse (..),
    newListJobRunsResponse,

    -- * Response Lenses
    listJobRunsResponse_nextToken,
    listJobRunsResponse_jobRuns,
    listJobRunsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EMRContainers.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListJobRuns' smart constructor.
data ListJobRuns = ListJobRuns'
  { -- | The states of the job run.
    states :: Prelude.Maybe [JobRunState],
    -- | The date and time after which the job runs were submitted.
    createdAfter :: Prelude.Maybe Core.POSIX,
    -- | The token for the next set of job runs to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the job run.
    name :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of job runs that can be listed.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The date and time before which the job runs were submitted.
    createdBefore :: Prelude.Maybe Core.POSIX,
    -- | The ID of the virtual cluster for which to list the job run.
    virtualClusterId :: Prelude.Text
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
-- 'states', 'listJobRuns_states' - The states of the job run.
--
-- 'createdAfter', 'listJobRuns_createdAfter' - The date and time after which the job runs were submitted.
--
-- 'nextToken', 'listJobRuns_nextToken' - The token for the next set of job runs to return.
--
-- 'name', 'listJobRuns_name' - The name of the job run.
--
-- 'maxResults', 'listJobRuns_maxResults' - The maximum number of job runs that can be listed.
--
-- 'createdBefore', 'listJobRuns_createdBefore' - The date and time before which the job runs were submitted.
--
-- 'virtualClusterId', 'listJobRuns_virtualClusterId' - The ID of the virtual cluster for which to list the job run.
newListJobRuns ::
  -- | 'virtualClusterId'
  Prelude.Text ->
  ListJobRuns
newListJobRuns pVirtualClusterId_ =
  ListJobRuns'
    { states = Prelude.Nothing,
      createdAfter = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      name = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      virtualClusterId = pVirtualClusterId_
    }

-- | The states of the job run.
listJobRuns_states :: Lens.Lens' ListJobRuns (Prelude.Maybe [JobRunState])
listJobRuns_states = Lens.lens (\ListJobRuns' {states} -> states) (\s@ListJobRuns' {} a -> s {states = a} :: ListJobRuns) Prelude.. Lens.mapping Lens.coerced

-- | The date and time after which the job runs were submitted.
listJobRuns_createdAfter :: Lens.Lens' ListJobRuns (Prelude.Maybe Prelude.UTCTime)
listJobRuns_createdAfter = Lens.lens (\ListJobRuns' {createdAfter} -> createdAfter) (\s@ListJobRuns' {} a -> s {createdAfter = a} :: ListJobRuns) Prelude.. Lens.mapping Core._Time

-- | The token for the next set of job runs to return.
listJobRuns_nextToken :: Lens.Lens' ListJobRuns (Prelude.Maybe Prelude.Text)
listJobRuns_nextToken = Lens.lens (\ListJobRuns' {nextToken} -> nextToken) (\s@ListJobRuns' {} a -> s {nextToken = a} :: ListJobRuns)

-- | The name of the job run.
listJobRuns_name :: Lens.Lens' ListJobRuns (Prelude.Maybe Prelude.Text)
listJobRuns_name = Lens.lens (\ListJobRuns' {name} -> name) (\s@ListJobRuns' {} a -> s {name = a} :: ListJobRuns)

-- | The maximum number of job runs that can be listed.
listJobRuns_maxResults :: Lens.Lens' ListJobRuns (Prelude.Maybe Prelude.Int)
listJobRuns_maxResults = Lens.lens (\ListJobRuns' {maxResults} -> maxResults) (\s@ListJobRuns' {} a -> s {maxResults = a} :: ListJobRuns)

-- | The date and time before which the job runs were submitted.
listJobRuns_createdBefore :: Lens.Lens' ListJobRuns (Prelude.Maybe Prelude.UTCTime)
listJobRuns_createdBefore = Lens.lens (\ListJobRuns' {createdBefore} -> createdBefore) (\s@ListJobRuns' {} a -> s {createdBefore = a} :: ListJobRuns) Prelude.. Lens.mapping Core._Time

-- | The ID of the virtual cluster for which to list the job run.
listJobRuns_virtualClusterId :: Lens.Lens' ListJobRuns Prelude.Text
listJobRuns_virtualClusterId = Lens.lens (\ListJobRuns' {virtualClusterId} -> virtualClusterId) (\s@ListJobRuns' {} a -> s {virtualClusterId = a} :: ListJobRuns)

instance Core.AWSPager ListJobRuns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobRunsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobRunsResponse_jobRuns Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listJobRuns_nextToken
          Lens..~ rs
          Lens.^? listJobRunsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListJobRuns where
  type AWSResponse ListJobRuns = ListJobRunsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobRunsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "jobRuns" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJobRuns where
  hashWithSalt _salt ListJobRuns' {..} =
    _salt `Prelude.hashWithSalt` states
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` virtualClusterId

instance Prelude.NFData ListJobRuns where
  rnf ListJobRuns' {..} =
    Prelude.rnf states
      `Prelude.seq` Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf virtualClusterId

instance Core.ToHeaders ListJobRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListJobRuns where
  toPath ListJobRuns' {..} =
    Prelude.mconcat
      [ "/virtualclusters/",
        Core.toBS virtualClusterId,
        "/jobruns"
      ]

instance Core.ToQuery ListJobRuns where
  toQuery ListJobRuns' {..} =
    Prelude.mconcat
      [ "states"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> states),
        "createdAfter" Core.=: createdAfter,
        "nextToken" Core.=: nextToken,
        "name" Core.=: name,
        "maxResults" Core.=: maxResults,
        "createdBefore" Core.=: createdBefore
      ]

-- | /See:/ 'newListJobRunsResponse' smart constructor.
data ListJobRunsResponse = ListJobRunsResponse'
  { -- | This output displays the token for the next set of job runs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This output lists information about the specified job runs.
    jobRuns :: Prelude.Maybe [JobRun],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobRunsResponse_nextToken' - This output displays the token for the next set of job runs.
--
-- 'jobRuns', 'listJobRunsResponse_jobRuns' - This output lists information about the specified job runs.
--
-- 'httpStatus', 'listJobRunsResponse_httpStatus' - The response's http status code.
newListJobRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJobRunsResponse
newListJobRunsResponse pHttpStatus_ =
  ListJobRunsResponse'
    { nextToken = Prelude.Nothing,
      jobRuns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This output displays the token for the next set of job runs.
listJobRunsResponse_nextToken :: Lens.Lens' ListJobRunsResponse (Prelude.Maybe Prelude.Text)
listJobRunsResponse_nextToken = Lens.lens (\ListJobRunsResponse' {nextToken} -> nextToken) (\s@ListJobRunsResponse' {} a -> s {nextToken = a} :: ListJobRunsResponse)

-- | This output lists information about the specified job runs.
listJobRunsResponse_jobRuns :: Lens.Lens' ListJobRunsResponse (Prelude.Maybe [JobRun])
listJobRunsResponse_jobRuns = Lens.lens (\ListJobRunsResponse' {jobRuns} -> jobRuns) (\s@ListJobRunsResponse' {} a -> s {jobRuns = a} :: ListJobRunsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listJobRunsResponse_httpStatus :: Lens.Lens' ListJobRunsResponse Prelude.Int
listJobRunsResponse_httpStatus = Lens.lens (\ListJobRunsResponse' {httpStatus} -> httpStatus) (\s@ListJobRunsResponse' {} a -> s {httpStatus = a} :: ListJobRunsResponse)

instance Prelude.NFData ListJobRunsResponse where
  rnf ListJobRunsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobRuns
      `Prelude.seq` Prelude.rnf httpStatus
