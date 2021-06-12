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
-- Module      : Network.AWS.Glue.GetWorkflowRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata for a given workflow run.
module Network.AWS.Glue.GetWorkflowRun
  ( -- * Creating a Request
    GetWorkflowRun (..),
    newGetWorkflowRun,

    -- * Request Lenses
    getWorkflowRun_includeGraph,
    getWorkflowRun_name,
    getWorkflowRun_runId,

    -- * Destructuring the Response
    GetWorkflowRunResponse (..),
    newGetWorkflowRunResponse,

    -- * Response Lenses
    getWorkflowRunResponse_run,
    getWorkflowRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetWorkflowRun' smart constructor.
data GetWorkflowRun = GetWorkflowRun'
  { -- | Specifies whether to include the workflow graph in response or not.
    includeGraph :: Core.Maybe Core.Bool,
    -- | Name of the workflow being run.
    name :: Core.Text,
    -- | The ID of the workflow run.
    runId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetWorkflowRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeGraph', 'getWorkflowRun_includeGraph' - Specifies whether to include the workflow graph in response or not.
--
-- 'name', 'getWorkflowRun_name' - Name of the workflow being run.
--
-- 'runId', 'getWorkflowRun_runId' - The ID of the workflow run.
newGetWorkflowRun ::
  -- | 'name'
  Core.Text ->
  -- | 'runId'
  Core.Text ->
  GetWorkflowRun
newGetWorkflowRun pName_ pRunId_ =
  GetWorkflowRun'
    { includeGraph = Core.Nothing,
      name = pName_,
      runId = pRunId_
    }

-- | Specifies whether to include the workflow graph in response or not.
getWorkflowRun_includeGraph :: Lens.Lens' GetWorkflowRun (Core.Maybe Core.Bool)
getWorkflowRun_includeGraph = Lens.lens (\GetWorkflowRun' {includeGraph} -> includeGraph) (\s@GetWorkflowRun' {} a -> s {includeGraph = a} :: GetWorkflowRun)

-- | Name of the workflow being run.
getWorkflowRun_name :: Lens.Lens' GetWorkflowRun Core.Text
getWorkflowRun_name = Lens.lens (\GetWorkflowRun' {name} -> name) (\s@GetWorkflowRun' {} a -> s {name = a} :: GetWorkflowRun)

-- | The ID of the workflow run.
getWorkflowRun_runId :: Lens.Lens' GetWorkflowRun Core.Text
getWorkflowRun_runId = Lens.lens (\GetWorkflowRun' {runId} -> runId) (\s@GetWorkflowRun' {} a -> s {runId = a} :: GetWorkflowRun)

instance Core.AWSRequest GetWorkflowRun where
  type
    AWSResponse GetWorkflowRun =
      GetWorkflowRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowRunResponse'
            Core.<$> (x Core..?> "Run")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetWorkflowRun

instance Core.NFData GetWorkflowRun

instance Core.ToHeaders GetWorkflowRun where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetWorkflowRun" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetWorkflowRun where
  toJSON GetWorkflowRun' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IncludeGraph" Core..=) Core.<$> includeGraph,
            Core.Just ("Name" Core..= name),
            Core.Just ("RunId" Core..= runId)
          ]
      )

instance Core.ToPath GetWorkflowRun where
  toPath = Core.const "/"

instance Core.ToQuery GetWorkflowRun where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetWorkflowRunResponse' smart constructor.
data GetWorkflowRunResponse = GetWorkflowRunResponse'
  { -- | The requested workflow run metadata.
    run :: Core.Maybe WorkflowRun,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetWorkflowRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'run', 'getWorkflowRunResponse_run' - The requested workflow run metadata.
--
-- 'httpStatus', 'getWorkflowRunResponse_httpStatus' - The response's http status code.
newGetWorkflowRunResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetWorkflowRunResponse
newGetWorkflowRunResponse pHttpStatus_ =
  GetWorkflowRunResponse'
    { run = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested workflow run metadata.
getWorkflowRunResponse_run :: Lens.Lens' GetWorkflowRunResponse (Core.Maybe WorkflowRun)
getWorkflowRunResponse_run = Lens.lens (\GetWorkflowRunResponse' {run} -> run) (\s@GetWorkflowRunResponse' {} a -> s {run = a} :: GetWorkflowRunResponse)

-- | The response's http status code.
getWorkflowRunResponse_httpStatus :: Lens.Lens' GetWorkflowRunResponse Core.Int
getWorkflowRunResponse_httpStatus = Lens.lens (\GetWorkflowRunResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowRunResponse' {} a -> s {httpStatus = a} :: GetWorkflowRunResponse)

instance Core.NFData GetWorkflowRunResponse
