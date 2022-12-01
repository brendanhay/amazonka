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
-- Module      : Amazonka.Glue.GetWorkflow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves resource metadata for a workflow.
module Amazonka.Glue.GetWorkflow
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkflow' smart constructor.
data GetWorkflow = GetWorkflow'
  { -- | Specifies whether to include a graph when returning the workflow
    -- resource metadata.
    includeGraph :: Prelude.Maybe Prelude.Bool,
    -- | The name of the workflow to retrieve.
    name :: Prelude.Text
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
-- 'includeGraph', 'getWorkflow_includeGraph' - Specifies whether to include a graph when returning the workflow
-- resource metadata.
--
-- 'name', 'getWorkflow_name' - The name of the workflow to retrieve.
newGetWorkflow ::
  -- | 'name'
  Prelude.Text ->
  GetWorkflow
newGetWorkflow pName_ =
  GetWorkflow'
    { includeGraph = Prelude.Nothing,
      name = pName_
    }

-- | Specifies whether to include a graph when returning the workflow
-- resource metadata.
getWorkflow_includeGraph :: Lens.Lens' GetWorkflow (Prelude.Maybe Prelude.Bool)
getWorkflow_includeGraph = Lens.lens (\GetWorkflow' {includeGraph} -> includeGraph) (\s@GetWorkflow' {} a -> s {includeGraph = a} :: GetWorkflow)

-- | The name of the workflow to retrieve.
getWorkflow_name :: Lens.Lens' GetWorkflow Prelude.Text
getWorkflow_name = Lens.lens (\GetWorkflow' {name} -> name) (\s@GetWorkflow' {} a -> s {name = a} :: GetWorkflow)

instance Core.AWSRequest GetWorkflow where
  type AWSResponse GetWorkflow = GetWorkflowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowResponse'
            Prelude.<$> (x Core..?> "Workflow")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflow where
  hashWithSalt _salt GetWorkflow' {..} =
    _salt `Prelude.hashWithSalt` includeGraph
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetWorkflow where
  rnf GetWorkflow' {..} =
    Prelude.rnf includeGraph
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders GetWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetWorkflow" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetWorkflow where
  toJSON GetWorkflow' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IncludeGraph" Core..=) Prelude.<$> includeGraph,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath GetWorkflow where
  toPath = Prelude.const "/"

instance Core.ToQuery GetWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWorkflowResponse' smart constructor.
data GetWorkflowResponse = GetWorkflowResponse'
  { -- | The resource metadata for the workflow.
    workflow :: Prelude.Maybe Workflow,
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
-- 'workflow', 'getWorkflowResponse_workflow' - The resource metadata for the workflow.
--
-- 'httpStatus', 'getWorkflowResponse_httpStatus' - The response's http status code.
newGetWorkflowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkflowResponse
newGetWorkflowResponse pHttpStatus_ =
  GetWorkflowResponse'
    { workflow = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource metadata for the workflow.
getWorkflowResponse_workflow :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Workflow)
getWorkflowResponse_workflow = Lens.lens (\GetWorkflowResponse' {workflow} -> workflow) (\s@GetWorkflowResponse' {} a -> s {workflow = a} :: GetWorkflowResponse)

-- | The response's http status code.
getWorkflowResponse_httpStatus :: Lens.Lens' GetWorkflowResponse Prelude.Int
getWorkflowResponse_httpStatus = Lens.lens (\GetWorkflowResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowResponse' {} a -> s {httpStatus = a} :: GetWorkflowResponse)

instance Prelude.NFData GetWorkflowResponse where
  rnf GetWorkflowResponse' {..} =
    Prelude.rnf workflow
      `Prelude.seq` Prelude.rnf httpStatus
