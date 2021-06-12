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
-- Module      : Network.AWS.Glue.StartMLEvaluationTaskRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a task to estimate the quality of the transform.
--
-- When you provide label sets as examples of truth, AWS Glue machine
-- learning uses some of those examples to learn from them. The rest of the
-- labels are used as a test to estimate quality.
--
-- Returns a unique identifier for the run. You can call @GetMLTaskRun@ to
-- get more information about the stats of the @EvaluationTaskRun@.
module Network.AWS.Glue.StartMLEvaluationTaskRun
  ( -- * Creating a Request
    StartMLEvaluationTaskRun (..),
    newStartMLEvaluationTaskRun,

    -- * Request Lenses
    startMLEvaluationTaskRun_transformId,

    -- * Destructuring the Response
    StartMLEvaluationTaskRunResponse (..),
    newStartMLEvaluationTaskRunResponse,

    -- * Response Lenses
    startMLEvaluationTaskRunResponse_taskRunId,
    startMLEvaluationTaskRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartMLEvaluationTaskRun' smart constructor.
data StartMLEvaluationTaskRun = StartMLEvaluationTaskRun'
  { -- | The unique identifier of the machine learning transform.
    transformId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartMLEvaluationTaskRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformId', 'startMLEvaluationTaskRun_transformId' - The unique identifier of the machine learning transform.
newStartMLEvaluationTaskRun ::
  -- | 'transformId'
  Core.Text ->
  StartMLEvaluationTaskRun
newStartMLEvaluationTaskRun pTransformId_ =
  StartMLEvaluationTaskRun'
    { transformId =
        pTransformId_
    }

-- | The unique identifier of the machine learning transform.
startMLEvaluationTaskRun_transformId :: Lens.Lens' StartMLEvaluationTaskRun Core.Text
startMLEvaluationTaskRun_transformId = Lens.lens (\StartMLEvaluationTaskRun' {transformId} -> transformId) (\s@StartMLEvaluationTaskRun' {} a -> s {transformId = a} :: StartMLEvaluationTaskRun)

instance Core.AWSRequest StartMLEvaluationTaskRun where
  type
    AWSResponse StartMLEvaluationTaskRun =
      StartMLEvaluationTaskRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMLEvaluationTaskRunResponse'
            Core.<$> (x Core..?> "TaskRunId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartMLEvaluationTaskRun

instance Core.NFData StartMLEvaluationTaskRun

instance Core.ToHeaders StartMLEvaluationTaskRun where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.StartMLEvaluationTaskRun" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartMLEvaluationTaskRun where
  toJSON StartMLEvaluationTaskRun' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TransformId" Core..= transformId)]
      )

instance Core.ToPath StartMLEvaluationTaskRun where
  toPath = Core.const "/"

instance Core.ToQuery StartMLEvaluationTaskRun where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartMLEvaluationTaskRunResponse' smart constructor.
data StartMLEvaluationTaskRunResponse = StartMLEvaluationTaskRunResponse'
  { -- | The unique identifier associated with this run.
    taskRunId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartMLEvaluationTaskRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskRunId', 'startMLEvaluationTaskRunResponse_taskRunId' - The unique identifier associated with this run.
--
-- 'httpStatus', 'startMLEvaluationTaskRunResponse_httpStatus' - The response's http status code.
newStartMLEvaluationTaskRunResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartMLEvaluationTaskRunResponse
newStartMLEvaluationTaskRunResponse pHttpStatus_ =
  StartMLEvaluationTaskRunResponse'
    { taskRunId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier associated with this run.
startMLEvaluationTaskRunResponse_taskRunId :: Lens.Lens' StartMLEvaluationTaskRunResponse (Core.Maybe Core.Text)
startMLEvaluationTaskRunResponse_taskRunId = Lens.lens (\StartMLEvaluationTaskRunResponse' {taskRunId} -> taskRunId) (\s@StartMLEvaluationTaskRunResponse' {} a -> s {taskRunId = a} :: StartMLEvaluationTaskRunResponse)

-- | The response's http status code.
startMLEvaluationTaskRunResponse_httpStatus :: Lens.Lens' StartMLEvaluationTaskRunResponse Core.Int
startMLEvaluationTaskRunResponse_httpStatus = Lens.lens (\StartMLEvaluationTaskRunResponse' {httpStatus} -> httpStatus) (\s@StartMLEvaluationTaskRunResponse' {} a -> s {httpStatus = a} :: StartMLEvaluationTaskRunResponse)

instance Core.NFData StartMLEvaluationTaskRunResponse
