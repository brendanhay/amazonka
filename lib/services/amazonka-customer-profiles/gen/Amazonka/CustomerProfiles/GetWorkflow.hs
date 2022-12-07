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
-- Module      : Amazonka.CustomerProfiles.GetWorkflow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details of specified workflow.
module Amazonka.CustomerProfiles.GetWorkflow
  ( -- * Creating a Request
    GetWorkflow (..),
    newGetWorkflow,

    -- * Request Lenses
    getWorkflow_domainName,
    getWorkflow_workflowId,

    -- * Destructuring the Response
    GetWorkflowResponse (..),
    newGetWorkflowResponse,

    -- * Response Lenses
    getWorkflowResponse_workflowId,
    getWorkflowResponse_lastUpdatedAt,
    getWorkflowResponse_status,
    getWorkflowResponse_metrics,
    getWorkflowResponse_workflowType,
    getWorkflowResponse_startDate,
    getWorkflowResponse_attributes,
    getWorkflowResponse_errorDescription,
    getWorkflowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkflow' smart constructor.
data GetWorkflow = GetWorkflow'
  { -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | Unique identifier for the workflow.
    workflowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getWorkflow_domainName' - The unique name of the domain.
--
-- 'workflowId', 'getWorkflow_workflowId' - Unique identifier for the workflow.
newGetWorkflow ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'workflowId'
  Prelude.Text ->
  GetWorkflow
newGetWorkflow pDomainName_ pWorkflowId_ =
  GetWorkflow'
    { domainName = pDomainName_,
      workflowId = pWorkflowId_
    }

-- | The unique name of the domain.
getWorkflow_domainName :: Lens.Lens' GetWorkflow Prelude.Text
getWorkflow_domainName = Lens.lens (\GetWorkflow' {domainName} -> domainName) (\s@GetWorkflow' {} a -> s {domainName = a} :: GetWorkflow)

-- | Unique identifier for the workflow.
getWorkflow_workflowId :: Lens.Lens' GetWorkflow Prelude.Text
getWorkflow_workflowId = Lens.lens (\GetWorkflow' {workflowId} -> workflowId) (\s@GetWorkflow' {} a -> s {workflowId = a} :: GetWorkflow)

instance Core.AWSRequest GetWorkflow where
  type AWSResponse GetWorkflow = GetWorkflowResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowResponse'
            Prelude.<$> (x Data..?> "WorkflowId")
            Prelude.<*> (x Data..?> "LastUpdatedAt")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "Metrics")
            Prelude.<*> (x Data..?> "WorkflowType")
            Prelude.<*> (x Data..?> "StartDate")
            Prelude.<*> (x Data..?> "Attributes")
            Prelude.<*> (x Data..?> "ErrorDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflow where
  hashWithSalt _salt GetWorkflow' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` workflowId

instance Prelude.NFData GetWorkflow where
  rnf GetWorkflow' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf workflowId

instance Data.ToHeaders GetWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetWorkflow where
  toPath GetWorkflow' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/workflows/",
        Data.toBS workflowId
      ]

instance Data.ToQuery GetWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWorkflowResponse' smart constructor.
data GetWorkflowResponse = GetWorkflowResponse'
  { -- | Unique identifier for the workflow.
    workflowId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp that represents when workflow execution last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | Status of workflow execution.
    status :: Prelude.Maybe Status,
    -- | Workflow specific execution metrics.
    metrics :: Prelude.Maybe WorkflowMetrics,
    -- | The type of workflow. The only supported value is APPFLOW_INTEGRATION.
    workflowType :: Prelude.Maybe WorkflowType,
    -- | The timestamp that represents when workflow execution started.
    startDate :: Prelude.Maybe Data.POSIX,
    -- | Attributes provided for workflow execution.
    attributes :: Prelude.Maybe WorkflowAttributes,
    -- | Workflow error messages during execution (if any).
    errorDescription :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowId', 'getWorkflowResponse_workflowId' - Unique identifier for the workflow.
--
-- 'lastUpdatedAt', 'getWorkflowResponse_lastUpdatedAt' - The timestamp that represents when workflow execution last updated.
--
-- 'status', 'getWorkflowResponse_status' - Status of workflow execution.
--
-- 'metrics', 'getWorkflowResponse_metrics' - Workflow specific execution metrics.
--
-- 'workflowType', 'getWorkflowResponse_workflowType' - The type of workflow. The only supported value is APPFLOW_INTEGRATION.
--
-- 'startDate', 'getWorkflowResponse_startDate' - The timestamp that represents when workflow execution started.
--
-- 'attributes', 'getWorkflowResponse_attributes' - Attributes provided for workflow execution.
--
-- 'errorDescription', 'getWorkflowResponse_errorDescription' - Workflow error messages during execution (if any).
--
-- 'httpStatus', 'getWorkflowResponse_httpStatus' - The response's http status code.
newGetWorkflowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkflowResponse
newGetWorkflowResponse pHttpStatus_ =
  GetWorkflowResponse'
    { workflowId = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      status = Prelude.Nothing,
      metrics = Prelude.Nothing,
      workflowType = Prelude.Nothing,
      startDate = Prelude.Nothing,
      attributes = Prelude.Nothing,
      errorDescription = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique identifier for the workflow.
getWorkflowResponse_workflowId :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_workflowId = Lens.lens (\GetWorkflowResponse' {workflowId} -> workflowId) (\s@GetWorkflowResponse' {} a -> s {workflowId = a} :: GetWorkflowResponse)

-- | The timestamp that represents when workflow execution last updated.
getWorkflowResponse_lastUpdatedAt :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowResponse_lastUpdatedAt = Lens.lens (\GetWorkflowResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetWorkflowResponse' {} a -> s {lastUpdatedAt = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Data._Time

-- | Status of workflow execution.
getWorkflowResponse_status :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Status)
getWorkflowResponse_status = Lens.lens (\GetWorkflowResponse' {status} -> status) (\s@GetWorkflowResponse' {} a -> s {status = a} :: GetWorkflowResponse)

-- | Workflow specific execution metrics.
getWorkflowResponse_metrics :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe WorkflowMetrics)
getWorkflowResponse_metrics = Lens.lens (\GetWorkflowResponse' {metrics} -> metrics) (\s@GetWorkflowResponse' {} a -> s {metrics = a} :: GetWorkflowResponse)

-- | The type of workflow. The only supported value is APPFLOW_INTEGRATION.
getWorkflowResponse_workflowType :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe WorkflowType)
getWorkflowResponse_workflowType = Lens.lens (\GetWorkflowResponse' {workflowType} -> workflowType) (\s@GetWorkflowResponse' {} a -> s {workflowType = a} :: GetWorkflowResponse)

-- | The timestamp that represents when workflow execution started.
getWorkflowResponse_startDate :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowResponse_startDate = Lens.lens (\GetWorkflowResponse' {startDate} -> startDate) (\s@GetWorkflowResponse' {} a -> s {startDate = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Data._Time

-- | Attributes provided for workflow execution.
getWorkflowResponse_attributes :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe WorkflowAttributes)
getWorkflowResponse_attributes = Lens.lens (\GetWorkflowResponse' {attributes} -> attributes) (\s@GetWorkflowResponse' {} a -> s {attributes = a} :: GetWorkflowResponse)

-- | Workflow error messages during execution (if any).
getWorkflowResponse_errorDescription :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_errorDescription = Lens.lens (\GetWorkflowResponse' {errorDescription} -> errorDescription) (\s@GetWorkflowResponse' {} a -> s {errorDescription = a} :: GetWorkflowResponse)

-- | The response's http status code.
getWorkflowResponse_httpStatus :: Lens.Lens' GetWorkflowResponse Prelude.Int
getWorkflowResponse_httpStatus = Lens.lens (\GetWorkflowResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowResponse' {} a -> s {httpStatus = a} :: GetWorkflowResponse)

instance Prelude.NFData GetWorkflowResponse where
  rnf GetWorkflowResponse' {..} =
    Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf workflowType
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf errorDescription
      `Prelude.seq` Prelude.rnf httpStatus
