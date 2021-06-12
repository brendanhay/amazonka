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
-- Module      : Network.AWS.Glue.StartWorkflowRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new run of the specified workflow.
module Network.AWS.Glue.StartWorkflowRun
  ( -- * Creating a Request
    StartWorkflowRun (..),
    newStartWorkflowRun,

    -- * Request Lenses
    startWorkflowRun_name,

    -- * Destructuring the Response
    StartWorkflowRunResponse (..),
    newStartWorkflowRunResponse,

    -- * Response Lenses
    startWorkflowRunResponse_runId,
    startWorkflowRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartWorkflowRun' smart constructor.
data StartWorkflowRun = StartWorkflowRun'
  { -- | The name of the workflow to start.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartWorkflowRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'startWorkflowRun_name' - The name of the workflow to start.
newStartWorkflowRun ::
  -- | 'name'
  Core.Text ->
  StartWorkflowRun
newStartWorkflowRun pName_ =
  StartWorkflowRun' {name = pName_}

-- | The name of the workflow to start.
startWorkflowRun_name :: Lens.Lens' StartWorkflowRun Core.Text
startWorkflowRun_name = Lens.lens (\StartWorkflowRun' {name} -> name) (\s@StartWorkflowRun' {} a -> s {name = a} :: StartWorkflowRun)

instance Core.AWSRequest StartWorkflowRun where
  type
    AWSResponse StartWorkflowRun =
      StartWorkflowRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartWorkflowRunResponse'
            Core.<$> (x Core..?> "RunId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartWorkflowRun

instance Core.NFData StartWorkflowRun

instance Core.ToHeaders StartWorkflowRun where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.StartWorkflowRun" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartWorkflowRun where
  toJSON StartWorkflowRun' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath StartWorkflowRun where
  toPath = Core.const "/"

instance Core.ToQuery StartWorkflowRun where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartWorkflowRunResponse' smart constructor.
data StartWorkflowRunResponse = StartWorkflowRunResponse'
  { -- | An Id for the new run.
    runId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartWorkflowRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'startWorkflowRunResponse_runId' - An Id for the new run.
--
-- 'httpStatus', 'startWorkflowRunResponse_httpStatus' - The response's http status code.
newStartWorkflowRunResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartWorkflowRunResponse
newStartWorkflowRunResponse pHttpStatus_ =
  StartWorkflowRunResponse'
    { runId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An Id for the new run.
startWorkflowRunResponse_runId :: Lens.Lens' StartWorkflowRunResponse (Core.Maybe Core.Text)
startWorkflowRunResponse_runId = Lens.lens (\StartWorkflowRunResponse' {runId} -> runId) (\s@StartWorkflowRunResponse' {} a -> s {runId = a} :: StartWorkflowRunResponse)

-- | The response's http status code.
startWorkflowRunResponse_httpStatus :: Lens.Lens' StartWorkflowRunResponse Core.Int
startWorkflowRunResponse_httpStatus = Lens.lens (\StartWorkflowRunResponse' {httpStatus} -> httpStatus) (\s@StartWorkflowRunResponse' {} a -> s {httpStatus = a} :: StartWorkflowRunResponse)

instance Core.NFData StartWorkflowRunResponse
