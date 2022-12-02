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
-- Module      : Amazonka.Glue.GetWorkflowRuns
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for all runs of a given workflow.
module Amazonka.Glue.GetWorkflowRuns
  ( -- * Creating a Request
    GetWorkflowRuns (..),
    newGetWorkflowRuns,

    -- * Request Lenses
    getWorkflowRuns_nextToken,
    getWorkflowRuns_maxResults,
    getWorkflowRuns_includeGraph,
    getWorkflowRuns_name,

    -- * Destructuring the Response
    GetWorkflowRunsResponse (..),
    newGetWorkflowRunsResponse,

    -- * Response Lenses
    getWorkflowRunsResponse_nextToken,
    getWorkflowRunsResponse_runs,
    getWorkflowRunsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkflowRuns' smart constructor.
data GetWorkflowRuns = GetWorkflowRuns'
  { -- | The maximum size of the response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of workflow runs to be included in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether to include the workflow graph in response or not.
    includeGraph :: Prelude.Maybe Prelude.Bool,
    -- | Name of the workflow whose metadata of runs should be returned.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getWorkflowRuns_nextToken' - The maximum size of the response.
--
-- 'maxResults', 'getWorkflowRuns_maxResults' - The maximum number of workflow runs to be included in the response.
--
-- 'includeGraph', 'getWorkflowRuns_includeGraph' - Specifies whether to include the workflow graph in response or not.
--
-- 'name', 'getWorkflowRuns_name' - Name of the workflow whose metadata of runs should be returned.
newGetWorkflowRuns ::
  -- | 'name'
  Prelude.Text ->
  GetWorkflowRuns
newGetWorkflowRuns pName_ =
  GetWorkflowRuns'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      includeGraph = Prelude.Nothing,
      name = pName_
    }

-- | The maximum size of the response.
getWorkflowRuns_nextToken :: Lens.Lens' GetWorkflowRuns (Prelude.Maybe Prelude.Text)
getWorkflowRuns_nextToken = Lens.lens (\GetWorkflowRuns' {nextToken} -> nextToken) (\s@GetWorkflowRuns' {} a -> s {nextToken = a} :: GetWorkflowRuns)

-- | The maximum number of workflow runs to be included in the response.
getWorkflowRuns_maxResults :: Lens.Lens' GetWorkflowRuns (Prelude.Maybe Prelude.Natural)
getWorkflowRuns_maxResults = Lens.lens (\GetWorkflowRuns' {maxResults} -> maxResults) (\s@GetWorkflowRuns' {} a -> s {maxResults = a} :: GetWorkflowRuns)

-- | Specifies whether to include the workflow graph in response or not.
getWorkflowRuns_includeGraph :: Lens.Lens' GetWorkflowRuns (Prelude.Maybe Prelude.Bool)
getWorkflowRuns_includeGraph = Lens.lens (\GetWorkflowRuns' {includeGraph} -> includeGraph) (\s@GetWorkflowRuns' {} a -> s {includeGraph = a} :: GetWorkflowRuns)

-- | Name of the workflow whose metadata of runs should be returned.
getWorkflowRuns_name :: Lens.Lens' GetWorkflowRuns Prelude.Text
getWorkflowRuns_name = Lens.lens (\GetWorkflowRuns' {name} -> name) (\s@GetWorkflowRuns' {} a -> s {name = a} :: GetWorkflowRuns)

instance Core.AWSRequest GetWorkflowRuns where
  type
    AWSResponse GetWorkflowRuns =
      GetWorkflowRunsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowRunsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Runs")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflowRuns where
  hashWithSalt _salt GetWorkflowRuns' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` includeGraph
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetWorkflowRuns where
  rnf GetWorkflowRuns' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf includeGraph
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders GetWorkflowRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetWorkflowRuns" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetWorkflowRuns where
  toJSON GetWorkflowRuns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("IncludeGraph" Data..=) Prelude.<$> includeGraph,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath GetWorkflowRuns where
  toPath = Prelude.const "/"

instance Data.ToQuery GetWorkflowRuns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWorkflowRunsResponse' smart constructor.
data GetWorkflowRunsResponse = GetWorkflowRunsResponse'
  { -- | A continuation token, if not all requested workflow runs have been
    -- returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of workflow run metadata objects.
    runs :: Prelude.Maybe (Prelude.NonEmpty WorkflowRun),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getWorkflowRunsResponse_nextToken' - A continuation token, if not all requested workflow runs have been
-- returned.
--
-- 'runs', 'getWorkflowRunsResponse_runs' - A list of workflow run metadata objects.
--
-- 'httpStatus', 'getWorkflowRunsResponse_httpStatus' - The response's http status code.
newGetWorkflowRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkflowRunsResponse
newGetWorkflowRunsResponse pHttpStatus_ =
  GetWorkflowRunsResponse'
    { nextToken =
        Prelude.Nothing,
      runs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if not all requested workflow runs have been
-- returned.
getWorkflowRunsResponse_nextToken :: Lens.Lens' GetWorkflowRunsResponse (Prelude.Maybe Prelude.Text)
getWorkflowRunsResponse_nextToken = Lens.lens (\GetWorkflowRunsResponse' {nextToken} -> nextToken) (\s@GetWorkflowRunsResponse' {} a -> s {nextToken = a} :: GetWorkflowRunsResponse)

-- | A list of workflow run metadata objects.
getWorkflowRunsResponse_runs :: Lens.Lens' GetWorkflowRunsResponse (Prelude.Maybe (Prelude.NonEmpty WorkflowRun))
getWorkflowRunsResponse_runs = Lens.lens (\GetWorkflowRunsResponse' {runs} -> runs) (\s@GetWorkflowRunsResponse' {} a -> s {runs = a} :: GetWorkflowRunsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getWorkflowRunsResponse_httpStatus :: Lens.Lens' GetWorkflowRunsResponse Prelude.Int
getWorkflowRunsResponse_httpStatus = Lens.lens (\GetWorkflowRunsResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowRunsResponse' {} a -> s {httpStatus = a} :: GetWorkflowRunsResponse)

instance Prelude.NFData GetWorkflowRunsResponse where
  rnf GetWorkflowRunsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf runs
      `Prelude.seq` Prelude.rnf httpStatus
