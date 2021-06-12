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
-- Module      : Network.AWS.Glue.GetWorkflow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves resource metadata for a workflow.
module Network.AWS.Glue.GetWorkflow
  ( -- * Creating a Request
    GetWorkflow (..),
    newGetWorkflow,

    -- * Request Lenses
    getWorkflow_includeGraph,
    getWorkflow_name,

    -- * Destructuring the Response
    GetWorkflowResponse (..),
    newGetWorkflowResponse,

    -- * Response Lenses
    getWorkflowResponse_workflow,
    getWorkflowResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetWorkflow' smart constructor.
data GetWorkflow = GetWorkflow'
  { -- | Specifies whether to include a graph when returning the workflow
    -- resource metadata.
    includeGraph :: Core.Maybe Core.Bool,
    -- | The name of the workflow to retrieve.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeGraph', 'getWorkflow_includeGraph' - Specifies whether to include a graph when returning the workflow
-- resource metadata.
--
-- 'name', 'getWorkflow_name' - The name of the workflow to retrieve.
newGetWorkflow ::
  -- | 'name'
  Core.Text ->
  GetWorkflow
newGetWorkflow pName_ =
  GetWorkflow'
    { includeGraph = Core.Nothing,
      name = pName_
    }

-- | Specifies whether to include a graph when returning the workflow
-- resource metadata.
getWorkflow_includeGraph :: Lens.Lens' GetWorkflow (Core.Maybe Core.Bool)
getWorkflow_includeGraph = Lens.lens (\GetWorkflow' {includeGraph} -> includeGraph) (\s@GetWorkflow' {} a -> s {includeGraph = a} :: GetWorkflow)

-- | The name of the workflow to retrieve.
getWorkflow_name :: Lens.Lens' GetWorkflow Core.Text
getWorkflow_name = Lens.lens (\GetWorkflow' {name} -> name) (\s@GetWorkflow' {} a -> s {name = a} :: GetWorkflow)

instance Core.AWSRequest GetWorkflow where
  type AWSResponse GetWorkflow = GetWorkflowResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowResponse'
            Core.<$> (x Core..?> "Workflow")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetWorkflow

instance Core.NFData GetWorkflow

instance Core.ToHeaders GetWorkflow where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetWorkflow" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetWorkflow where
  toJSON GetWorkflow' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IncludeGraph" Core..=) Core.<$> includeGraph,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath GetWorkflow where
  toPath = Core.const "/"

instance Core.ToQuery GetWorkflow where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetWorkflowResponse' smart constructor.
data GetWorkflowResponse = GetWorkflowResponse'
  { -- | The resource metadata for the workflow.
    workflow :: Core.Maybe Workflow,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflow', 'getWorkflowResponse_workflow' - The resource metadata for the workflow.
--
-- 'httpStatus', 'getWorkflowResponse_httpStatus' - The response's http status code.
newGetWorkflowResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetWorkflowResponse
newGetWorkflowResponse pHttpStatus_ =
  GetWorkflowResponse'
    { workflow = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource metadata for the workflow.
getWorkflowResponse_workflow :: Lens.Lens' GetWorkflowResponse (Core.Maybe Workflow)
getWorkflowResponse_workflow = Lens.lens (\GetWorkflowResponse' {workflow} -> workflow) (\s@GetWorkflowResponse' {} a -> s {workflow = a} :: GetWorkflowResponse)

-- | The response's http status code.
getWorkflowResponse_httpStatus :: Lens.Lens' GetWorkflowResponse Core.Int
getWorkflowResponse_httpStatus = Lens.lens (\GetWorkflowResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowResponse' {} a -> s {httpStatus = a} :: GetWorkflowResponse)

instance Core.NFData GetWorkflowResponse
