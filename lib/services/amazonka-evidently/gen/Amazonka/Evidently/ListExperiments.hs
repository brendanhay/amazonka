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
-- Module      : Amazonka.Evidently.ListExperiments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns configuration details about all the experiments in the specified
-- project.
--
-- This operation returns paginated results.
module Amazonka.Evidently.ListExperiments
  ( -- * Creating a Request
    ListExperiments (..),
    newListExperiments,

    -- * Request Lenses
    listExperiments_maxResults,
    listExperiments_nextToken,
    listExperiments_status,
    listExperiments_project,

    -- * Destructuring the Response
    ListExperimentsResponse (..),
    newListExperimentsResponse,

    -- * Response Lenses
    listExperimentsResponse_experiments,
    listExperimentsResponse_nextToken,
    listExperimentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListExperiments' smart constructor.
data ListExperiments = ListExperiments'
  { -- | The maximum number of results to include in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to use when requesting the next set of results. You received
    -- this token from a previous @ListExperiments@ operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Use this optional parameter to limit the returned results to only the
    -- experiments with the status that you specify here.
    status :: Prelude.Maybe ExperimentStatus,
    -- | The name or ARN of the project to return the experiment list from.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExperiments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listExperiments_maxResults' - The maximum number of results to include in the response.
--
-- 'nextToken', 'listExperiments_nextToken' - The token to use when requesting the next set of results. You received
-- this token from a previous @ListExperiments@ operation.
--
-- 'status', 'listExperiments_status' - Use this optional parameter to limit the returned results to only the
-- experiments with the status that you specify here.
--
-- 'project', 'listExperiments_project' - The name or ARN of the project to return the experiment list from.
newListExperiments ::
  -- | 'project'
  Prelude.Text ->
  ListExperiments
newListExperiments pProject_ =
  ListExperiments'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      project = pProject_
    }

-- | The maximum number of results to include in the response.
listExperiments_maxResults :: Lens.Lens' ListExperiments (Prelude.Maybe Prelude.Natural)
listExperiments_maxResults = Lens.lens (\ListExperiments' {maxResults} -> maxResults) (\s@ListExperiments' {} a -> s {maxResults = a} :: ListExperiments)

-- | The token to use when requesting the next set of results. You received
-- this token from a previous @ListExperiments@ operation.
listExperiments_nextToken :: Lens.Lens' ListExperiments (Prelude.Maybe Prelude.Text)
listExperiments_nextToken = Lens.lens (\ListExperiments' {nextToken} -> nextToken) (\s@ListExperiments' {} a -> s {nextToken = a} :: ListExperiments)

-- | Use this optional parameter to limit the returned results to only the
-- experiments with the status that you specify here.
listExperiments_status :: Lens.Lens' ListExperiments (Prelude.Maybe ExperimentStatus)
listExperiments_status = Lens.lens (\ListExperiments' {status} -> status) (\s@ListExperiments' {} a -> s {status = a} :: ListExperiments)

-- | The name or ARN of the project to return the experiment list from.
listExperiments_project :: Lens.Lens' ListExperiments Prelude.Text
listExperiments_project = Lens.lens (\ListExperiments' {project} -> project) (\s@ListExperiments' {} a -> s {project = a} :: ListExperiments)

instance Core.AWSPager ListExperiments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listExperimentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listExperimentsResponse_experiments
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listExperiments_nextToken
          Lens..~ rs
          Lens.^? listExperimentsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListExperiments where
  type
    AWSResponse ListExperiments =
      ListExperimentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExperimentsResponse'
            Prelude.<$> (x Data..?> "experiments" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExperiments where
  hashWithSalt _salt ListExperiments' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` project

instance Prelude.NFData ListExperiments where
  rnf ListExperiments' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf project

instance Data.ToHeaders ListExperiments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListExperiments where
  toPath ListExperiments' {..} =
    Prelude.mconcat
      ["/projects/", Data.toBS project, "/experiments"]

instance Data.ToQuery ListExperiments where
  toQuery ListExperiments' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "status" Data.=: status
      ]

-- | /See:/ 'newListExperimentsResponse' smart constructor.
data ListExperimentsResponse = ListExperimentsResponse'
  { -- | An array of structures that contain the configuration details of the
    -- experiments in the specified project.
    experiments :: Prelude.Maybe [Experiment],
    -- | The token to use in a subsequent @ListExperiments@ operation to return
    -- the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExperimentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experiments', 'listExperimentsResponse_experiments' - An array of structures that contain the configuration details of the
-- experiments in the specified project.
--
-- 'nextToken', 'listExperimentsResponse_nextToken' - The token to use in a subsequent @ListExperiments@ operation to return
-- the next set of results.
--
-- 'httpStatus', 'listExperimentsResponse_httpStatus' - The response's http status code.
newListExperimentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExperimentsResponse
newListExperimentsResponse pHttpStatus_ =
  ListExperimentsResponse'
    { experiments =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of structures that contain the configuration details of the
-- experiments in the specified project.
listExperimentsResponse_experiments :: Lens.Lens' ListExperimentsResponse (Prelude.Maybe [Experiment])
listExperimentsResponse_experiments = Lens.lens (\ListExperimentsResponse' {experiments} -> experiments) (\s@ListExperimentsResponse' {} a -> s {experiments = a} :: ListExperimentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use in a subsequent @ListExperiments@ operation to return
-- the next set of results.
listExperimentsResponse_nextToken :: Lens.Lens' ListExperimentsResponse (Prelude.Maybe Prelude.Text)
listExperimentsResponse_nextToken = Lens.lens (\ListExperimentsResponse' {nextToken} -> nextToken) (\s@ListExperimentsResponse' {} a -> s {nextToken = a} :: ListExperimentsResponse)

-- | The response's http status code.
listExperimentsResponse_httpStatus :: Lens.Lens' ListExperimentsResponse Prelude.Int
listExperimentsResponse_httpStatus = Lens.lens (\ListExperimentsResponse' {httpStatus} -> httpStatus) (\s@ListExperimentsResponse' {} a -> s {httpStatus = a} :: ListExperimentsResponse)

instance Prelude.NFData ListExperimentsResponse where
  rnf ListExperimentsResponse' {..} =
    Prelude.rnf experiments
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
