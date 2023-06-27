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
-- Module      : Amazonka.ImageBuilder.GetWorkflowStepExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the runtime information that was logged for a specific runtime
-- instance of the workflow step.
module Amazonka.ImageBuilder.GetWorkflowStepExecution
  ( -- * Creating a Request
    GetWorkflowStepExecution (..),
    newGetWorkflowStepExecution,

    -- * Request Lenses
    getWorkflowStepExecution_stepExecutionId,

    -- * Destructuring the Response
    GetWorkflowStepExecutionResponse (..),
    newGetWorkflowStepExecutionResponse,

    -- * Response Lenses
    getWorkflowStepExecutionResponse_action,
    getWorkflowStepExecutionResponse_description,
    getWorkflowStepExecutionResponse_endTime,
    getWorkflowStepExecutionResponse_imageBuildVersionArn,
    getWorkflowStepExecutionResponse_inputs,
    getWorkflowStepExecutionResponse_message,
    getWorkflowStepExecutionResponse_name,
    getWorkflowStepExecutionResponse_onFailure,
    getWorkflowStepExecutionResponse_outputs,
    getWorkflowStepExecutionResponse_requestId,
    getWorkflowStepExecutionResponse_rollbackStatus,
    getWorkflowStepExecutionResponse_startTime,
    getWorkflowStepExecutionResponse_status,
    getWorkflowStepExecutionResponse_stepExecutionId,
    getWorkflowStepExecutionResponse_timeoutSeconds,
    getWorkflowStepExecutionResponse_workflowBuildVersionArn,
    getWorkflowStepExecutionResponse_workflowExecutionId,
    getWorkflowStepExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkflowStepExecution' smart constructor.
data GetWorkflowStepExecution = GetWorkflowStepExecution'
  { -- | Use the unique identifier for a specific runtime instance of the
    -- workflow step to get runtime details for that step.
    stepExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowStepExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stepExecutionId', 'getWorkflowStepExecution_stepExecutionId' - Use the unique identifier for a specific runtime instance of the
-- workflow step to get runtime details for that step.
newGetWorkflowStepExecution ::
  -- | 'stepExecutionId'
  Prelude.Text ->
  GetWorkflowStepExecution
newGetWorkflowStepExecution pStepExecutionId_ =
  GetWorkflowStepExecution'
    { stepExecutionId =
        pStepExecutionId_
    }

-- | Use the unique identifier for a specific runtime instance of the
-- workflow step to get runtime details for that step.
getWorkflowStepExecution_stepExecutionId :: Lens.Lens' GetWorkflowStepExecution Prelude.Text
getWorkflowStepExecution_stepExecutionId = Lens.lens (\GetWorkflowStepExecution' {stepExecutionId} -> stepExecutionId) (\s@GetWorkflowStepExecution' {} a -> s {stepExecutionId = a} :: GetWorkflowStepExecution)

instance Core.AWSRequest GetWorkflowStepExecution where
  type
    AWSResponse GetWorkflowStepExecution =
      GetWorkflowStepExecutionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowStepExecutionResponse'
            Prelude.<$> (x Data..?> "action")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "endTime")
            Prelude.<*> (x Data..?> "imageBuildVersionArn")
            Prelude.<*> (x Data..?> "inputs")
            Prelude.<*> (x Data..?> "message")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "onFailure")
            Prelude.<*> (x Data..?> "outputs")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (x Data..?> "rollbackStatus")
            Prelude.<*> (x Data..?> "startTime")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "stepExecutionId")
            Prelude.<*> (x Data..?> "timeoutSeconds")
            Prelude.<*> (x Data..?> "workflowBuildVersionArn")
            Prelude.<*> (x Data..?> "workflowExecutionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflowStepExecution where
  hashWithSalt _salt GetWorkflowStepExecution' {..} =
    _salt `Prelude.hashWithSalt` stepExecutionId

instance Prelude.NFData GetWorkflowStepExecution where
  rnf GetWorkflowStepExecution' {..} =
    Prelude.rnf stepExecutionId

instance Data.ToHeaders GetWorkflowStepExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetWorkflowStepExecution where
  toPath = Prelude.const "/GetWorkflowStepExecution"

instance Data.ToQuery GetWorkflowStepExecution where
  toQuery GetWorkflowStepExecution' {..} =
    Prelude.mconcat
      ["stepExecutionId" Data.=: stepExecutionId]

-- | /See:/ 'newGetWorkflowStepExecutionResponse' smart constructor.
data GetWorkflowStepExecutionResponse = GetWorkflowStepExecutionResponse'
  { -- | The name of the action that the specified step performs.
    action :: Prelude.Maybe Prelude.Text,
    -- | Describes the specified workflow step.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the specified runtime instance of the workflow step
    -- finished.
    endTime :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the image resource build version that
    -- the specified runtime instance of the workflow step creates.
    imageBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | Input parameters that Image Builder provided for the specified runtime
    -- instance of the workflow step.
    inputs :: Prelude.Maybe Prelude.Text,
    -- | The output message from the specified runtime instance of the workflow
    -- step, if applicable.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the specified runtime instance of the workflow step.
    name :: Prelude.Maybe Prelude.Text,
    -- | The action to perform if the workflow step fails.
    onFailure :: Prelude.Maybe Prelude.Text,
    -- | The file names that the specified runtime version of the workflow step
    -- created as output.
    outputs :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Reports on the rollback status of the specified runtime version of the
    -- workflow step, if applicable.
    rollbackStatus :: Prelude.Maybe WorkflowStepExecutionRollbackStatus,
    -- | The timestamp when the specified runtime version of the workflow step
    -- started.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The current status for the specified runtime version of the workflow
    -- step.
    status :: Prelude.Maybe WorkflowStepExecutionStatus,
    -- | The unique identifier for the runtime version of the workflow step that
    -- you specified in the request.
    stepExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The maximum duration in seconds for this step to complete its action.
    timeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the build version for the Image
    -- Builder workflow resource that defines this workflow step.
    workflowBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier that Image Builder assigned to keep track of
    -- runtime details when it ran the workflow.
    workflowExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowStepExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'getWorkflowStepExecutionResponse_action' - The name of the action that the specified step performs.
--
-- 'description', 'getWorkflowStepExecutionResponse_description' - Describes the specified workflow step.
--
-- 'endTime', 'getWorkflowStepExecutionResponse_endTime' - The timestamp when the specified runtime instance of the workflow step
-- finished.
--
-- 'imageBuildVersionArn', 'getWorkflowStepExecutionResponse_imageBuildVersionArn' - The Amazon Resource Name (ARN) of the image resource build version that
-- the specified runtime instance of the workflow step creates.
--
-- 'inputs', 'getWorkflowStepExecutionResponse_inputs' - Input parameters that Image Builder provided for the specified runtime
-- instance of the workflow step.
--
-- 'message', 'getWorkflowStepExecutionResponse_message' - The output message from the specified runtime instance of the workflow
-- step, if applicable.
--
-- 'name', 'getWorkflowStepExecutionResponse_name' - The name of the specified runtime instance of the workflow step.
--
-- 'onFailure', 'getWorkflowStepExecutionResponse_onFailure' - The action to perform if the workflow step fails.
--
-- 'outputs', 'getWorkflowStepExecutionResponse_outputs' - The file names that the specified runtime version of the workflow step
-- created as output.
--
-- 'requestId', 'getWorkflowStepExecutionResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'rollbackStatus', 'getWorkflowStepExecutionResponse_rollbackStatus' - Reports on the rollback status of the specified runtime version of the
-- workflow step, if applicable.
--
-- 'startTime', 'getWorkflowStepExecutionResponse_startTime' - The timestamp when the specified runtime version of the workflow step
-- started.
--
-- 'status', 'getWorkflowStepExecutionResponse_status' - The current status for the specified runtime version of the workflow
-- step.
--
-- 'stepExecutionId', 'getWorkflowStepExecutionResponse_stepExecutionId' - The unique identifier for the runtime version of the workflow step that
-- you specified in the request.
--
-- 'timeoutSeconds', 'getWorkflowStepExecutionResponse_timeoutSeconds' - The maximum duration in seconds for this step to complete its action.
--
-- 'workflowBuildVersionArn', 'getWorkflowStepExecutionResponse_workflowBuildVersionArn' - The Amazon Resource Name (ARN) of the build version for the Image
-- Builder workflow resource that defines this workflow step.
--
-- 'workflowExecutionId', 'getWorkflowStepExecutionResponse_workflowExecutionId' - The unique identifier that Image Builder assigned to keep track of
-- runtime details when it ran the workflow.
--
-- 'httpStatus', 'getWorkflowStepExecutionResponse_httpStatus' - The response's http status code.
newGetWorkflowStepExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkflowStepExecutionResponse
newGetWorkflowStepExecutionResponse pHttpStatus_ =
  GetWorkflowStepExecutionResponse'
    { action =
        Prelude.Nothing,
      description = Prelude.Nothing,
      endTime = Prelude.Nothing,
      imageBuildVersionArn = Prelude.Nothing,
      inputs = Prelude.Nothing,
      message = Prelude.Nothing,
      name = Prelude.Nothing,
      onFailure = Prelude.Nothing,
      outputs = Prelude.Nothing,
      requestId = Prelude.Nothing,
      rollbackStatus = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      stepExecutionId = Prelude.Nothing,
      timeoutSeconds = Prelude.Nothing,
      workflowBuildVersionArn = Prelude.Nothing,
      workflowExecutionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the action that the specified step performs.
getWorkflowStepExecutionResponse_action :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepExecutionResponse_action = Lens.lens (\GetWorkflowStepExecutionResponse' {action} -> action) (\s@GetWorkflowStepExecutionResponse' {} a -> s {action = a} :: GetWorkflowStepExecutionResponse)

-- | Describes the specified workflow step.
getWorkflowStepExecutionResponse_description :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepExecutionResponse_description = Lens.lens (\GetWorkflowStepExecutionResponse' {description} -> description) (\s@GetWorkflowStepExecutionResponse' {} a -> s {description = a} :: GetWorkflowStepExecutionResponse)

-- | The timestamp when the specified runtime instance of the workflow step
-- finished.
getWorkflowStepExecutionResponse_endTime :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepExecutionResponse_endTime = Lens.lens (\GetWorkflowStepExecutionResponse' {endTime} -> endTime) (\s@GetWorkflowStepExecutionResponse' {} a -> s {endTime = a} :: GetWorkflowStepExecutionResponse)

-- | The Amazon Resource Name (ARN) of the image resource build version that
-- the specified runtime instance of the workflow step creates.
getWorkflowStepExecutionResponse_imageBuildVersionArn :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepExecutionResponse_imageBuildVersionArn = Lens.lens (\GetWorkflowStepExecutionResponse' {imageBuildVersionArn} -> imageBuildVersionArn) (\s@GetWorkflowStepExecutionResponse' {} a -> s {imageBuildVersionArn = a} :: GetWorkflowStepExecutionResponse)

-- | Input parameters that Image Builder provided for the specified runtime
-- instance of the workflow step.
getWorkflowStepExecutionResponse_inputs :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepExecutionResponse_inputs = Lens.lens (\GetWorkflowStepExecutionResponse' {inputs} -> inputs) (\s@GetWorkflowStepExecutionResponse' {} a -> s {inputs = a} :: GetWorkflowStepExecutionResponse)

-- | The output message from the specified runtime instance of the workflow
-- step, if applicable.
getWorkflowStepExecutionResponse_message :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepExecutionResponse_message = Lens.lens (\GetWorkflowStepExecutionResponse' {message} -> message) (\s@GetWorkflowStepExecutionResponse' {} a -> s {message = a} :: GetWorkflowStepExecutionResponse)

-- | The name of the specified runtime instance of the workflow step.
getWorkflowStepExecutionResponse_name :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepExecutionResponse_name = Lens.lens (\GetWorkflowStepExecutionResponse' {name} -> name) (\s@GetWorkflowStepExecutionResponse' {} a -> s {name = a} :: GetWorkflowStepExecutionResponse)

-- | The action to perform if the workflow step fails.
getWorkflowStepExecutionResponse_onFailure :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepExecutionResponse_onFailure = Lens.lens (\GetWorkflowStepExecutionResponse' {onFailure} -> onFailure) (\s@GetWorkflowStepExecutionResponse' {} a -> s {onFailure = a} :: GetWorkflowStepExecutionResponse)

-- | The file names that the specified runtime version of the workflow step
-- created as output.
getWorkflowStepExecutionResponse_outputs :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepExecutionResponse_outputs = Lens.lens (\GetWorkflowStepExecutionResponse' {outputs} -> outputs) (\s@GetWorkflowStepExecutionResponse' {} a -> s {outputs = a} :: GetWorkflowStepExecutionResponse)

-- | The request ID that uniquely identifies this request.
getWorkflowStepExecutionResponse_requestId :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepExecutionResponse_requestId = Lens.lens (\GetWorkflowStepExecutionResponse' {requestId} -> requestId) (\s@GetWorkflowStepExecutionResponse' {} a -> s {requestId = a} :: GetWorkflowStepExecutionResponse)

-- | Reports on the rollback status of the specified runtime version of the
-- workflow step, if applicable.
getWorkflowStepExecutionResponse_rollbackStatus :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe WorkflowStepExecutionRollbackStatus)
getWorkflowStepExecutionResponse_rollbackStatus = Lens.lens (\GetWorkflowStepExecutionResponse' {rollbackStatus} -> rollbackStatus) (\s@GetWorkflowStepExecutionResponse' {} a -> s {rollbackStatus = a} :: GetWorkflowStepExecutionResponse)

-- | The timestamp when the specified runtime version of the workflow step
-- started.
getWorkflowStepExecutionResponse_startTime :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepExecutionResponse_startTime = Lens.lens (\GetWorkflowStepExecutionResponse' {startTime} -> startTime) (\s@GetWorkflowStepExecutionResponse' {} a -> s {startTime = a} :: GetWorkflowStepExecutionResponse)

-- | The current status for the specified runtime version of the workflow
-- step.
getWorkflowStepExecutionResponse_status :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe WorkflowStepExecutionStatus)
getWorkflowStepExecutionResponse_status = Lens.lens (\GetWorkflowStepExecutionResponse' {status} -> status) (\s@GetWorkflowStepExecutionResponse' {} a -> s {status = a} :: GetWorkflowStepExecutionResponse)

-- | The unique identifier for the runtime version of the workflow step that
-- you specified in the request.
getWorkflowStepExecutionResponse_stepExecutionId :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepExecutionResponse_stepExecutionId = Lens.lens (\GetWorkflowStepExecutionResponse' {stepExecutionId} -> stepExecutionId) (\s@GetWorkflowStepExecutionResponse' {} a -> s {stepExecutionId = a} :: GetWorkflowStepExecutionResponse)

-- | The maximum duration in seconds for this step to complete its action.
getWorkflowStepExecutionResponse_timeoutSeconds :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe Prelude.Natural)
getWorkflowStepExecutionResponse_timeoutSeconds = Lens.lens (\GetWorkflowStepExecutionResponse' {timeoutSeconds} -> timeoutSeconds) (\s@GetWorkflowStepExecutionResponse' {} a -> s {timeoutSeconds = a} :: GetWorkflowStepExecutionResponse)

-- | The Amazon Resource Name (ARN) of the build version for the Image
-- Builder workflow resource that defines this workflow step.
getWorkflowStepExecutionResponse_workflowBuildVersionArn :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepExecutionResponse_workflowBuildVersionArn = Lens.lens (\GetWorkflowStepExecutionResponse' {workflowBuildVersionArn} -> workflowBuildVersionArn) (\s@GetWorkflowStepExecutionResponse' {} a -> s {workflowBuildVersionArn = a} :: GetWorkflowStepExecutionResponse)

-- | The unique identifier that Image Builder assigned to keep track of
-- runtime details when it ran the workflow.
getWorkflowStepExecutionResponse_workflowExecutionId :: Lens.Lens' GetWorkflowStepExecutionResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepExecutionResponse_workflowExecutionId = Lens.lens (\GetWorkflowStepExecutionResponse' {workflowExecutionId} -> workflowExecutionId) (\s@GetWorkflowStepExecutionResponse' {} a -> s {workflowExecutionId = a} :: GetWorkflowStepExecutionResponse)

-- | The response's http status code.
getWorkflowStepExecutionResponse_httpStatus :: Lens.Lens' GetWorkflowStepExecutionResponse Prelude.Int
getWorkflowStepExecutionResponse_httpStatus = Lens.lens (\GetWorkflowStepExecutionResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowStepExecutionResponse' {} a -> s {httpStatus = a} :: GetWorkflowStepExecutionResponse)

instance
  Prelude.NFData
    GetWorkflowStepExecutionResponse
  where
  rnf GetWorkflowStepExecutionResponse' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf imageBuildVersionArn
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf onFailure
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf rollbackStatus
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf stepExecutionId
      `Prelude.seq` Prelude.rnf timeoutSeconds
      `Prelude.seq` Prelude.rnf workflowBuildVersionArn
      `Prelude.seq` Prelude.rnf workflowExecutionId
      `Prelude.seq` Prelude.rnf httpStatus
