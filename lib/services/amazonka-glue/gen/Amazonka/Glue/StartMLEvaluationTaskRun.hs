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
-- Module      : Amazonka.Glue.StartMLEvaluationTaskRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a task to estimate the quality of the transform.
--
-- When you provide label sets as examples of truth, Glue machine learning
-- uses some of those examples to learn from them. The rest of the labels
-- are used as a test to estimate quality.
--
-- Returns a unique identifier for the run. You can call @GetMLTaskRun@ to
-- get more information about the stats of the @EvaluationTaskRun@.
module Amazonka.Glue.StartMLEvaluationTaskRun
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartMLEvaluationTaskRun' smart constructor.
data StartMLEvaluationTaskRun = StartMLEvaluationTaskRun'
  { -- | The unique identifier of the machine learning transform.
    transformId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StartMLEvaluationTaskRun
newStartMLEvaluationTaskRun pTransformId_ =
  StartMLEvaluationTaskRun'
    { transformId =
        pTransformId_
    }

-- | The unique identifier of the machine learning transform.
startMLEvaluationTaskRun_transformId :: Lens.Lens' StartMLEvaluationTaskRun Prelude.Text
startMLEvaluationTaskRun_transformId = Lens.lens (\StartMLEvaluationTaskRun' {transformId} -> transformId) (\s@StartMLEvaluationTaskRun' {} a -> s {transformId = a} :: StartMLEvaluationTaskRun)

instance Core.AWSRequest StartMLEvaluationTaskRun where
  type
    AWSResponse StartMLEvaluationTaskRun =
      StartMLEvaluationTaskRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMLEvaluationTaskRunResponse'
            Prelude.<$> (x Data..?> "TaskRunId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartMLEvaluationTaskRun where
  hashWithSalt _salt StartMLEvaluationTaskRun' {..} =
    _salt `Prelude.hashWithSalt` transformId

instance Prelude.NFData StartMLEvaluationTaskRun where
  rnf StartMLEvaluationTaskRun' {..} =
    Prelude.rnf transformId

instance Data.ToHeaders StartMLEvaluationTaskRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.StartMLEvaluationTaskRun" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartMLEvaluationTaskRun where
  toJSON StartMLEvaluationTaskRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TransformId" Data..= transformId)]
      )

instance Data.ToPath StartMLEvaluationTaskRun where
  toPath = Prelude.const "/"

instance Data.ToQuery StartMLEvaluationTaskRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartMLEvaluationTaskRunResponse' smart constructor.
data StartMLEvaluationTaskRunResponse = StartMLEvaluationTaskRunResponse'
  { -- | The unique identifier associated with this run.
    taskRunId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartMLEvaluationTaskRunResponse
newStartMLEvaluationTaskRunResponse pHttpStatus_ =
  StartMLEvaluationTaskRunResponse'
    { taskRunId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier associated with this run.
startMLEvaluationTaskRunResponse_taskRunId :: Lens.Lens' StartMLEvaluationTaskRunResponse (Prelude.Maybe Prelude.Text)
startMLEvaluationTaskRunResponse_taskRunId = Lens.lens (\StartMLEvaluationTaskRunResponse' {taskRunId} -> taskRunId) (\s@StartMLEvaluationTaskRunResponse' {} a -> s {taskRunId = a} :: StartMLEvaluationTaskRunResponse)

-- | The response's http status code.
startMLEvaluationTaskRunResponse_httpStatus :: Lens.Lens' StartMLEvaluationTaskRunResponse Prelude.Int
startMLEvaluationTaskRunResponse_httpStatus = Lens.lens (\StartMLEvaluationTaskRunResponse' {httpStatus} -> httpStatus) (\s@StartMLEvaluationTaskRunResponse' {} a -> s {httpStatus = a} :: StartMLEvaluationTaskRunResponse)

instance
  Prelude.NFData
    StartMLEvaluationTaskRunResponse
  where
  rnf StartMLEvaluationTaskRunResponse' {..} =
    Prelude.rnf taskRunId `Prelude.seq`
      Prelude.rnf httpStatus
