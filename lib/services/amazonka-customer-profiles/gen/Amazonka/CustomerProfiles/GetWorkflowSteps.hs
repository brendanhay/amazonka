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
-- Module      : Amazonka.CustomerProfiles.GetWorkflowSteps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get granular list of steps in workflow.
module Amazonka.CustomerProfiles.GetWorkflowSteps
  ( -- * Creating a Request
    GetWorkflowSteps (..),
    newGetWorkflowSteps,

    -- * Request Lenses
    getWorkflowSteps_maxResults,
    getWorkflowSteps_nextToken,
    getWorkflowSteps_domainName,
    getWorkflowSteps_workflowId,

    -- * Destructuring the Response
    GetWorkflowStepsResponse (..),
    newGetWorkflowStepsResponse,

    -- * Response Lenses
    getWorkflowStepsResponse_items,
    getWorkflowStepsResponse_nextToken,
    getWorkflowStepsResponse_workflowId,
    getWorkflowStepsResponse_workflowType,
    getWorkflowStepsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkflowSteps' smart constructor.
data GetWorkflowSteps = GetWorkflowSteps'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | Unique identifier for the workflow.
    workflowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowSteps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getWorkflowSteps_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'getWorkflowSteps_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'domainName', 'getWorkflowSteps_domainName' - The unique name of the domain.
--
-- 'workflowId', 'getWorkflowSteps_workflowId' - Unique identifier for the workflow.
newGetWorkflowSteps ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'workflowId'
  Prelude.Text ->
  GetWorkflowSteps
newGetWorkflowSteps pDomainName_ pWorkflowId_ =
  GetWorkflowSteps'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      domainName = pDomainName_,
      workflowId = pWorkflowId_
    }

-- | The maximum number of results to return per page.
getWorkflowSteps_maxResults :: Lens.Lens' GetWorkflowSteps (Prelude.Maybe Prelude.Natural)
getWorkflowSteps_maxResults = Lens.lens (\GetWorkflowSteps' {maxResults} -> maxResults) (\s@GetWorkflowSteps' {} a -> s {maxResults = a} :: GetWorkflowSteps)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
getWorkflowSteps_nextToken :: Lens.Lens' GetWorkflowSteps (Prelude.Maybe Prelude.Text)
getWorkflowSteps_nextToken = Lens.lens (\GetWorkflowSteps' {nextToken} -> nextToken) (\s@GetWorkflowSteps' {} a -> s {nextToken = a} :: GetWorkflowSteps)

-- | The unique name of the domain.
getWorkflowSteps_domainName :: Lens.Lens' GetWorkflowSteps Prelude.Text
getWorkflowSteps_domainName = Lens.lens (\GetWorkflowSteps' {domainName} -> domainName) (\s@GetWorkflowSteps' {} a -> s {domainName = a} :: GetWorkflowSteps)

-- | Unique identifier for the workflow.
getWorkflowSteps_workflowId :: Lens.Lens' GetWorkflowSteps Prelude.Text
getWorkflowSteps_workflowId = Lens.lens (\GetWorkflowSteps' {workflowId} -> workflowId) (\s@GetWorkflowSteps' {} a -> s {workflowId = a} :: GetWorkflowSteps)

instance Core.AWSRequest GetWorkflowSteps where
  type
    AWSResponse GetWorkflowSteps =
      GetWorkflowStepsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowStepsResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "WorkflowId")
            Prelude.<*> (x Data..?> "WorkflowType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflowSteps where
  hashWithSalt _salt GetWorkflowSteps' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` workflowId

instance Prelude.NFData GetWorkflowSteps where
  rnf GetWorkflowSteps' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf workflowId

instance Data.ToHeaders GetWorkflowSteps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetWorkflowSteps where
  toPath GetWorkflowSteps' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/workflows/",
        Data.toBS workflowId,
        "/steps"
      ]

instance Data.ToQuery GetWorkflowSteps where
  toQuery GetWorkflowSteps' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newGetWorkflowStepsResponse' smart constructor.
data GetWorkflowStepsResponse = GetWorkflowStepsResponse'
  { -- | List containing workflow step details.
    items :: Prelude.Maybe [WorkflowStepItem],
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier for the workflow.
    workflowId :: Prelude.Maybe Prelude.Text,
    -- | The type of workflow. The only supported value is APPFLOW_INTEGRATION.
    workflowType :: Prelude.Maybe WorkflowType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowStepsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getWorkflowStepsResponse_items' - List containing workflow step details.
--
-- 'nextToken', 'getWorkflowStepsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'workflowId', 'getWorkflowStepsResponse_workflowId' - Unique identifier for the workflow.
--
-- 'workflowType', 'getWorkflowStepsResponse_workflowType' - The type of workflow. The only supported value is APPFLOW_INTEGRATION.
--
-- 'httpStatus', 'getWorkflowStepsResponse_httpStatus' - The response's http status code.
newGetWorkflowStepsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkflowStepsResponse
newGetWorkflowStepsResponse pHttpStatus_ =
  GetWorkflowStepsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workflowId = Prelude.Nothing,
      workflowType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List containing workflow step details.
getWorkflowStepsResponse_items :: Lens.Lens' GetWorkflowStepsResponse (Prelude.Maybe [WorkflowStepItem])
getWorkflowStepsResponse_items = Lens.lens (\GetWorkflowStepsResponse' {items} -> items) (\s@GetWorkflowStepsResponse' {} a -> s {items = a} :: GetWorkflowStepsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, this is the token for the next set of
-- results.
getWorkflowStepsResponse_nextToken :: Lens.Lens' GetWorkflowStepsResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepsResponse_nextToken = Lens.lens (\GetWorkflowStepsResponse' {nextToken} -> nextToken) (\s@GetWorkflowStepsResponse' {} a -> s {nextToken = a} :: GetWorkflowStepsResponse)

-- | Unique identifier for the workflow.
getWorkflowStepsResponse_workflowId :: Lens.Lens' GetWorkflowStepsResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepsResponse_workflowId = Lens.lens (\GetWorkflowStepsResponse' {workflowId} -> workflowId) (\s@GetWorkflowStepsResponse' {} a -> s {workflowId = a} :: GetWorkflowStepsResponse)

-- | The type of workflow. The only supported value is APPFLOW_INTEGRATION.
getWorkflowStepsResponse_workflowType :: Lens.Lens' GetWorkflowStepsResponse (Prelude.Maybe WorkflowType)
getWorkflowStepsResponse_workflowType = Lens.lens (\GetWorkflowStepsResponse' {workflowType} -> workflowType) (\s@GetWorkflowStepsResponse' {} a -> s {workflowType = a} :: GetWorkflowStepsResponse)

-- | The response's http status code.
getWorkflowStepsResponse_httpStatus :: Lens.Lens' GetWorkflowStepsResponse Prelude.Int
getWorkflowStepsResponse_httpStatus = Lens.lens (\GetWorkflowStepsResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowStepsResponse' {} a -> s {httpStatus = a} :: GetWorkflowStepsResponse)

instance Prelude.NFData GetWorkflowStepsResponse where
  rnf GetWorkflowStepsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf workflowType
      `Prelude.seq` Prelude.rnf httpStatus
