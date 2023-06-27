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
-- Module      : Amazonka.ImageBuilder.GetWorkflowExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the runtime information that was logged for a specific runtime
-- instance of the workflow.
module Amazonka.ImageBuilder.GetWorkflowExecution
  ( -- * Creating a Request
    GetWorkflowExecution (..),
    newGetWorkflowExecution,

    -- * Request Lenses
    getWorkflowExecution_workflowExecutionId,

    -- * Destructuring the Response
    GetWorkflowExecutionResponse (..),
    newGetWorkflowExecutionResponse,

    -- * Response Lenses
    getWorkflowExecutionResponse_endTime,
    getWorkflowExecutionResponse_imageBuildVersionArn,
    getWorkflowExecutionResponse_message,
    getWorkflowExecutionResponse_requestId,
    getWorkflowExecutionResponse_startTime,
    getWorkflowExecutionResponse_status,
    getWorkflowExecutionResponse_totalStepCount,
    getWorkflowExecutionResponse_totalStepsFailed,
    getWorkflowExecutionResponse_totalStepsSkipped,
    getWorkflowExecutionResponse_totalStepsSucceeded,
    getWorkflowExecutionResponse_type,
    getWorkflowExecutionResponse_workflowBuildVersionArn,
    getWorkflowExecutionResponse_workflowExecutionId,
    getWorkflowExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkflowExecution' smart constructor.
data GetWorkflowExecution = GetWorkflowExecution'
  { -- | Use the unique identifier for a runtime instance of the workflow to get
    -- runtime details.
    workflowExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowExecutionId', 'getWorkflowExecution_workflowExecutionId' - Use the unique identifier for a runtime instance of the workflow to get
-- runtime details.
newGetWorkflowExecution ::
  -- | 'workflowExecutionId'
  Prelude.Text ->
  GetWorkflowExecution
newGetWorkflowExecution pWorkflowExecutionId_ =
  GetWorkflowExecution'
    { workflowExecutionId =
        pWorkflowExecutionId_
    }

-- | Use the unique identifier for a runtime instance of the workflow to get
-- runtime details.
getWorkflowExecution_workflowExecutionId :: Lens.Lens' GetWorkflowExecution Prelude.Text
getWorkflowExecution_workflowExecutionId = Lens.lens (\GetWorkflowExecution' {workflowExecutionId} -> workflowExecutionId) (\s@GetWorkflowExecution' {} a -> s {workflowExecutionId = a} :: GetWorkflowExecution)

instance Core.AWSRequest GetWorkflowExecution where
  type
    AWSResponse GetWorkflowExecution =
      GetWorkflowExecutionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowExecutionResponse'
            Prelude.<$> (x Data..?> "endTime")
            Prelude.<*> (x Data..?> "imageBuildVersionArn")
            Prelude.<*> (x Data..?> "message")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (x Data..?> "startTime")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "totalStepCount")
            Prelude.<*> (x Data..?> "totalStepsFailed")
            Prelude.<*> (x Data..?> "totalStepsSkipped")
            Prelude.<*> (x Data..?> "totalStepsSucceeded")
            Prelude.<*> (x Data..?> "type")
            Prelude.<*> (x Data..?> "workflowBuildVersionArn")
            Prelude.<*> (x Data..?> "workflowExecutionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflowExecution where
  hashWithSalt _salt GetWorkflowExecution' {..} =
    _salt `Prelude.hashWithSalt` workflowExecutionId

instance Prelude.NFData GetWorkflowExecution where
  rnf GetWorkflowExecution' {..} =
    Prelude.rnf workflowExecutionId

instance Data.ToHeaders GetWorkflowExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetWorkflowExecution where
  toPath = Prelude.const "/GetWorkflowExecution"

instance Data.ToQuery GetWorkflowExecution where
  toQuery GetWorkflowExecution' {..} =
    Prelude.mconcat
      ["workflowExecutionId" Data.=: workflowExecutionId]

-- | /See:/ 'newGetWorkflowExecutionResponse' smart constructor.
data GetWorkflowExecutionResponse = GetWorkflowExecutionResponse'
  { -- | The timestamp when the specified runtime instance of the workflow
    -- finished.
    endTime :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the image resource build version that
    -- the specified runtime instance of the workflow created.
    imageBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The output message from the specified runtime instance of the workflow,
    -- if applicable.
    message :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the specified runtime instance of the workflow
    -- started.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The current runtime status for the specified runtime instance of the
    -- workflow.
    status :: Prelude.Maybe WorkflowExecutionStatus,
    -- | The total number of steps in the specified runtime instance of the
    -- workflow that ran. This number should equal the sum of the step counts
    -- for steps that succeeded, were skipped, and failed.
    totalStepCount :: Prelude.Maybe Prelude.Int,
    -- | A runtime count for the number of steps that failed in the specified
    -- runtime instance of the workflow.
    totalStepsFailed :: Prelude.Maybe Prelude.Int,
    -- | A runtime count for the number of steps that were skipped in the
    -- specified runtime instance of the workflow.
    totalStepsSkipped :: Prelude.Maybe Prelude.Int,
    -- | A runtime count for the number of steps that ran successfully in the
    -- specified runtime instance of the workflow.
    totalStepsSucceeded :: Prelude.Maybe Prelude.Int,
    -- | The type of workflow that Image Builder ran for the specified runtime
    -- instance of the workflow.
    type' :: Prelude.Maybe WorkflowType,
    -- | The Amazon Resource Name (ARN) of the build version for the Image
    -- Builder workflow resource that defines the specified runtime instance of
    -- the workflow.
    workflowBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier that Image Builder assigned to keep track of
    -- runtime details when it ran the workflow.
    workflowExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'getWorkflowExecutionResponse_endTime' - The timestamp when the specified runtime instance of the workflow
-- finished.
--
-- 'imageBuildVersionArn', 'getWorkflowExecutionResponse_imageBuildVersionArn' - The Amazon Resource Name (ARN) of the image resource build version that
-- the specified runtime instance of the workflow created.
--
-- 'message', 'getWorkflowExecutionResponse_message' - The output message from the specified runtime instance of the workflow,
-- if applicable.
--
-- 'requestId', 'getWorkflowExecutionResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'startTime', 'getWorkflowExecutionResponse_startTime' - The timestamp when the specified runtime instance of the workflow
-- started.
--
-- 'status', 'getWorkflowExecutionResponse_status' - The current runtime status for the specified runtime instance of the
-- workflow.
--
-- 'totalStepCount', 'getWorkflowExecutionResponse_totalStepCount' - The total number of steps in the specified runtime instance of the
-- workflow that ran. This number should equal the sum of the step counts
-- for steps that succeeded, were skipped, and failed.
--
-- 'totalStepsFailed', 'getWorkflowExecutionResponse_totalStepsFailed' - A runtime count for the number of steps that failed in the specified
-- runtime instance of the workflow.
--
-- 'totalStepsSkipped', 'getWorkflowExecutionResponse_totalStepsSkipped' - A runtime count for the number of steps that were skipped in the
-- specified runtime instance of the workflow.
--
-- 'totalStepsSucceeded', 'getWorkflowExecutionResponse_totalStepsSucceeded' - A runtime count for the number of steps that ran successfully in the
-- specified runtime instance of the workflow.
--
-- 'type'', 'getWorkflowExecutionResponse_type' - The type of workflow that Image Builder ran for the specified runtime
-- instance of the workflow.
--
-- 'workflowBuildVersionArn', 'getWorkflowExecutionResponse_workflowBuildVersionArn' - The Amazon Resource Name (ARN) of the build version for the Image
-- Builder workflow resource that defines the specified runtime instance of
-- the workflow.
--
-- 'workflowExecutionId', 'getWorkflowExecutionResponse_workflowExecutionId' - The unique identifier that Image Builder assigned to keep track of
-- runtime details when it ran the workflow.
--
-- 'httpStatus', 'getWorkflowExecutionResponse_httpStatus' - The response's http status code.
newGetWorkflowExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkflowExecutionResponse
newGetWorkflowExecutionResponse pHttpStatus_ =
  GetWorkflowExecutionResponse'
    { endTime =
        Prelude.Nothing,
      imageBuildVersionArn = Prelude.Nothing,
      message = Prelude.Nothing,
      requestId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      totalStepCount = Prelude.Nothing,
      totalStepsFailed = Prelude.Nothing,
      totalStepsSkipped = Prelude.Nothing,
      totalStepsSucceeded = Prelude.Nothing,
      type' = Prelude.Nothing,
      workflowBuildVersionArn = Prelude.Nothing,
      workflowExecutionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp when the specified runtime instance of the workflow
-- finished.
getWorkflowExecutionResponse_endTime :: Lens.Lens' GetWorkflowExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowExecutionResponse_endTime = Lens.lens (\GetWorkflowExecutionResponse' {endTime} -> endTime) (\s@GetWorkflowExecutionResponse' {} a -> s {endTime = a} :: GetWorkflowExecutionResponse)

-- | The Amazon Resource Name (ARN) of the image resource build version that
-- the specified runtime instance of the workflow created.
getWorkflowExecutionResponse_imageBuildVersionArn :: Lens.Lens' GetWorkflowExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowExecutionResponse_imageBuildVersionArn = Lens.lens (\GetWorkflowExecutionResponse' {imageBuildVersionArn} -> imageBuildVersionArn) (\s@GetWorkflowExecutionResponse' {} a -> s {imageBuildVersionArn = a} :: GetWorkflowExecutionResponse)

-- | The output message from the specified runtime instance of the workflow,
-- if applicable.
getWorkflowExecutionResponse_message :: Lens.Lens' GetWorkflowExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowExecutionResponse_message = Lens.lens (\GetWorkflowExecutionResponse' {message} -> message) (\s@GetWorkflowExecutionResponse' {} a -> s {message = a} :: GetWorkflowExecutionResponse)

-- | The request ID that uniquely identifies this request.
getWorkflowExecutionResponse_requestId :: Lens.Lens' GetWorkflowExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowExecutionResponse_requestId = Lens.lens (\GetWorkflowExecutionResponse' {requestId} -> requestId) (\s@GetWorkflowExecutionResponse' {} a -> s {requestId = a} :: GetWorkflowExecutionResponse)

-- | The timestamp when the specified runtime instance of the workflow
-- started.
getWorkflowExecutionResponse_startTime :: Lens.Lens' GetWorkflowExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowExecutionResponse_startTime = Lens.lens (\GetWorkflowExecutionResponse' {startTime} -> startTime) (\s@GetWorkflowExecutionResponse' {} a -> s {startTime = a} :: GetWorkflowExecutionResponse)

-- | The current runtime status for the specified runtime instance of the
-- workflow.
getWorkflowExecutionResponse_status :: Lens.Lens' GetWorkflowExecutionResponse (Prelude.Maybe WorkflowExecutionStatus)
getWorkflowExecutionResponse_status = Lens.lens (\GetWorkflowExecutionResponse' {status} -> status) (\s@GetWorkflowExecutionResponse' {} a -> s {status = a} :: GetWorkflowExecutionResponse)

-- | The total number of steps in the specified runtime instance of the
-- workflow that ran. This number should equal the sum of the step counts
-- for steps that succeeded, were skipped, and failed.
getWorkflowExecutionResponse_totalStepCount :: Lens.Lens' GetWorkflowExecutionResponse (Prelude.Maybe Prelude.Int)
getWorkflowExecutionResponse_totalStepCount = Lens.lens (\GetWorkflowExecutionResponse' {totalStepCount} -> totalStepCount) (\s@GetWorkflowExecutionResponse' {} a -> s {totalStepCount = a} :: GetWorkflowExecutionResponse)

-- | A runtime count for the number of steps that failed in the specified
-- runtime instance of the workflow.
getWorkflowExecutionResponse_totalStepsFailed :: Lens.Lens' GetWorkflowExecutionResponse (Prelude.Maybe Prelude.Int)
getWorkflowExecutionResponse_totalStepsFailed = Lens.lens (\GetWorkflowExecutionResponse' {totalStepsFailed} -> totalStepsFailed) (\s@GetWorkflowExecutionResponse' {} a -> s {totalStepsFailed = a} :: GetWorkflowExecutionResponse)

-- | A runtime count for the number of steps that were skipped in the
-- specified runtime instance of the workflow.
getWorkflowExecutionResponse_totalStepsSkipped :: Lens.Lens' GetWorkflowExecutionResponse (Prelude.Maybe Prelude.Int)
getWorkflowExecutionResponse_totalStepsSkipped = Lens.lens (\GetWorkflowExecutionResponse' {totalStepsSkipped} -> totalStepsSkipped) (\s@GetWorkflowExecutionResponse' {} a -> s {totalStepsSkipped = a} :: GetWorkflowExecutionResponse)

-- | A runtime count for the number of steps that ran successfully in the
-- specified runtime instance of the workflow.
getWorkflowExecutionResponse_totalStepsSucceeded :: Lens.Lens' GetWorkflowExecutionResponse (Prelude.Maybe Prelude.Int)
getWorkflowExecutionResponse_totalStepsSucceeded = Lens.lens (\GetWorkflowExecutionResponse' {totalStepsSucceeded} -> totalStepsSucceeded) (\s@GetWorkflowExecutionResponse' {} a -> s {totalStepsSucceeded = a} :: GetWorkflowExecutionResponse)

-- | The type of workflow that Image Builder ran for the specified runtime
-- instance of the workflow.
getWorkflowExecutionResponse_type :: Lens.Lens' GetWorkflowExecutionResponse (Prelude.Maybe WorkflowType)
getWorkflowExecutionResponse_type = Lens.lens (\GetWorkflowExecutionResponse' {type'} -> type') (\s@GetWorkflowExecutionResponse' {} a -> s {type' = a} :: GetWorkflowExecutionResponse)

-- | The Amazon Resource Name (ARN) of the build version for the Image
-- Builder workflow resource that defines the specified runtime instance of
-- the workflow.
getWorkflowExecutionResponse_workflowBuildVersionArn :: Lens.Lens' GetWorkflowExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowExecutionResponse_workflowBuildVersionArn = Lens.lens (\GetWorkflowExecutionResponse' {workflowBuildVersionArn} -> workflowBuildVersionArn) (\s@GetWorkflowExecutionResponse' {} a -> s {workflowBuildVersionArn = a} :: GetWorkflowExecutionResponse)

-- | The unique identifier that Image Builder assigned to keep track of
-- runtime details when it ran the workflow.
getWorkflowExecutionResponse_workflowExecutionId :: Lens.Lens' GetWorkflowExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowExecutionResponse_workflowExecutionId = Lens.lens (\GetWorkflowExecutionResponse' {workflowExecutionId} -> workflowExecutionId) (\s@GetWorkflowExecutionResponse' {} a -> s {workflowExecutionId = a} :: GetWorkflowExecutionResponse)

-- | The response's http status code.
getWorkflowExecutionResponse_httpStatus :: Lens.Lens' GetWorkflowExecutionResponse Prelude.Int
getWorkflowExecutionResponse_httpStatus = Lens.lens (\GetWorkflowExecutionResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowExecutionResponse' {} a -> s {httpStatus = a} :: GetWorkflowExecutionResponse)

instance Prelude.NFData GetWorkflowExecutionResponse where
  rnf GetWorkflowExecutionResponse' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf imageBuildVersionArn
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf totalStepCount
      `Prelude.seq` Prelude.rnf totalStepsFailed
      `Prelude.seq` Prelude.rnf totalStepsSkipped
      `Prelude.seq` Prelude.rnf totalStepsSucceeded
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf workflowBuildVersionArn
      `Prelude.seq` Prelude.rnf workflowExecutionId
      `Prelude.seq` Prelude.rnf httpStatus
