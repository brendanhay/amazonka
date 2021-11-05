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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    getWorkflowRuns_includeGraph,
    getWorkflowRuns_nextToken,
    getWorkflowRuns_maxResults,
    getWorkflowRuns_name,

    -- * Destructuring the Response
    GetWorkflowRunsResponse (..),
    newGetWorkflowRunsResponse,

    -- * Response Lenses
    getWorkflowRunsResponse_runs,
    getWorkflowRunsResponse_nextToken,
    getWorkflowRunsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkflowRuns' smart constructor.
data GetWorkflowRuns = GetWorkflowRuns'
  { -- | Specifies whether to include the workflow graph in response or not.
    includeGraph :: Prelude.Maybe Prelude.Bool,
    -- | The maximum size of the response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of workflow runs to be included in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'includeGraph', 'getWorkflowRuns_includeGraph' - Specifies whether to include the workflow graph in response or not.
--
-- 'nextToken', 'getWorkflowRuns_nextToken' - The maximum size of the response.
--
-- 'maxResults', 'getWorkflowRuns_maxResults' - The maximum number of workflow runs to be included in the response.
--
-- 'name', 'getWorkflowRuns_name' - Name of the workflow whose metadata of runs should be returned.
newGetWorkflowRuns ::
  -- | 'name'
  Prelude.Text ->
  GetWorkflowRuns
newGetWorkflowRuns pName_ =
  GetWorkflowRuns'
    { includeGraph = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      name = pName_
    }

-- | Specifies whether to include the workflow graph in response or not.
getWorkflowRuns_includeGraph :: Lens.Lens' GetWorkflowRuns (Prelude.Maybe Prelude.Bool)
getWorkflowRuns_includeGraph = Lens.lens (\GetWorkflowRuns' {includeGraph} -> includeGraph) (\s@GetWorkflowRuns' {} a -> s {includeGraph = a} :: GetWorkflowRuns)

-- | The maximum size of the response.
getWorkflowRuns_nextToken :: Lens.Lens' GetWorkflowRuns (Prelude.Maybe Prelude.Text)
getWorkflowRuns_nextToken = Lens.lens (\GetWorkflowRuns' {nextToken} -> nextToken) (\s@GetWorkflowRuns' {} a -> s {nextToken = a} :: GetWorkflowRuns)

-- | The maximum number of workflow runs to be included in the response.
getWorkflowRuns_maxResults :: Lens.Lens' GetWorkflowRuns (Prelude.Maybe Prelude.Natural)
getWorkflowRuns_maxResults = Lens.lens (\GetWorkflowRuns' {maxResults} -> maxResults) (\s@GetWorkflowRuns' {} a -> s {maxResults = a} :: GetWorkflowRuns)

-- | Name of the workflow whose metadata of runs should be returned.
getWorkflowRuns_name :: Lens.Lens' GetWorkflowRuns Prelude.Text
getWorkflowRuns_name = Lens.lens (\GetWorkflowRuns' {name} -> name) (\s@GetWorkflowRuns' {} a -> s {name = a} :: GetWorkflowRuns)

instance Core.AWSRequest GetWorkflowRuns where
  type
    AWSResponse GetWorkflowRuns =
      GetWorkflowRunsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowRunsResponse'
            Prelude.<$> (x Core..?> "Runs")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflowRuns

instance Prelude.NFData GetWorkflowRuns

instance Core.ToHeaders GetWorkflowRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetWorkflowRuns" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetWorkflowRuns where
  toJSON GetWorkflowRuns' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IncludeGraph" Core..=) Prelude.<$> includeGraph,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath GetWorkflowRuns where
  toPath = Prelude.const "/"

instance Core.ToQuery GetWorkflowRuns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWorkflowRunsResponse' smart constructor.
data GetWorkflowRunsResponse = GetWorkflowRunsResponse'
  { -- | A list of workflow run metadata objects.
    runs :: Prelude.Maybe (Prelude.NonEmpty WorkflowRun),
    -- | A continuation token, if not all requested workflow runs have been
    -- returned.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'runs', 'getWorkflowRunsResponse_runs' - A list of workflow run metadata objects.
--
-- 'nextToken', 'getWorkflowRunsResponse_nextToken' - A continuation token, if not all requested workflow runs have been
-- returned.
--
-- 'httpStatus', 'getWorkflowRunsResponse_httpStatus' - The response's http status code.
newGetWorkflowRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkflowRunsResponse
newGetWorkflowRunsResponse pHttpStatus_ =
  GetWorkflowRunsResponse'
    { runs = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of workflow run metadata objects.
getWorkflowRunsResponse_runs :: Lens.Lens' GetWorkflowRunsResponse (Prelude.Maybe (Prelude.NonEmpty WorkflowRun))
getWorkflowRunsResponse_runs = Lens.lens (\GetWorkflowRunsResponse' {runs} -> runs) (\s@GetWorkflowRunsResponse' {} a -> s {runs = a} :: GetWorkflowRunsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if not all requested workflow runs have been
-- returned.
getWorkflowRunsResponse_nextToken :: Lens.Lens' GetWorkflowRunsResponse (Prelude.Maybe Prelude.Text)
getWorkflowRunsResponse_nextToken = Lens.lens (\GetWorkflowRunsResponse' {nextToken} -> nextToken) (\s@GetWorkflowRunsResponse' {} a -> s {nextToken = a} :: GetWorkflowRunsResponse)

-- | The response's http status code.
getWorkflowRunsResponse_httpStatus :: Lens.Lens' GetWorkflowRunsResponse Prelude.Int
getWorkflowRunsResponse_httpStatus = Lens.lens (\GetWorkflowRunsResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowRunsResponse' {} a -> s {httpStatus = a} :: GetWorkflowRunsResponse)

instance Prelude.NFData GetWorkflowRunsResponse
