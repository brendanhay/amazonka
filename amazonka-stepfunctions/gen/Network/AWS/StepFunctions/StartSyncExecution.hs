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
-- Module      : Network.AWS.StepFunctions.StartSyncExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a Synchronous Express state machine execution.
module Network.AWS.StepFunctions.StartSyncExecution
  ( -- * Creating a Request
    StartSyncExecution (..),
    newStartSyncExecution,

    -- * Request Lenses
    startSyncExecution_input,
    startSyncExecution_name,
    startSyncExecution_traceHeader,
    startSyncExecution_stateMachineArn,

    -- * Destructuring the Response
    StartSyncExecutionResponse (..),
    newStartSyncExecutionResponse,

    -- * Response Lenses
    startSyncExecutionResponse_stateMachineArn,
    startSyncExecutionResponse_inputDetails,
    startSyncExecutionResponse_input,
    startSyncExecutionResponse_name,
    startSyncExecutionResponse_output,
    startSyncExecutionResponse_cause,
    startSyncExecutionResponse_billingDetails,
    startSyncExecutionResponse_traceHeader,
    startSyncExecutionResponse_error,
    startSyncExecutionResponse_outputDetails,
    startSyncExecutionResponse_httpStatus,
    startSyncExecutionResponse_executionArn,
    startSyncExecutionResponse_startDate,
    startSyncExecutionResponse_stopDate,
    startSyncExecutionResponse_status,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newStartSyncExecution' smart constructor.
data StartSyncExecution = StartSyncExecution'
  { -- | The string that contains the JSON input data for the execution, for
    -- example:
    --
    -- @\"input\": \"{\\\"first_name\\\" : \\\"test\\\"}\"@
    --
    -- If you don\'t include any JSON input data, you still must include the
    -- two braces, for example: @\"input\": \"{}\"@
    --
    -- Length constraints apply to the payload size, and are expressed as bytes
    -- in UTF-8 encoding.
    input :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The name of the execution.
    name :: Core.Maybe Core.Text,
    -- | Passes the AWS X-Ray trace header. The trace header can also be passed
    -- in the request payload.
    traceHeader :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the state machine to execute.
    stateMachineArn :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartSyncExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'startSyncExecution_input' - The string that contains the JSON input data for the execution, for
-- example:
--
-- @\"input\": \"{\\\"first_name\\\" : \\\"test\\\"}\"@
--
-- If you don\'t include any JSON input data, you still must include the
-- two braces, for example: @\"input\": \"{}\"@
--
-- Length constraints apply to the payload size, and are expressed as bytes
-- in UTF-8 encoding.
--
-- 'name', 'startSyncExecution_name' - The name of the execution.
--
-- 'traceHeader', 'startSyncExecution_traceHeader' - Passes the AWS X-Ray trace header. The trace header can also be passed
-- in the request payload.
--
-- 'stateMachineArn', 'startSyncExecution_stateMachineArn' - The Amazon Resource Name (ARN) of the state machine to execute.
newStartSyncExecution ::
  -- | 'stateMachineArn'
  Core.Text ->
  StartSyncExecution
newStartSyncExecution pStateMachineArn_ =
  StartSyncExecution'
    { input = Core.Nothing,
      name = Core.Nothing,
      traceHeader = Core.Nothing,
      stateMachineArn = pStateMachineArn_
    }

-- | The string that contains the JSON input data for the execution, for
-- example:
--
-- @\"input\": \"{\\\"first_name\\\" : \\\"test\\\"}\"@
--
-- If you don\'t include any JSON input data, you still must include the
-- two braces, for example: @\"input\": \"{}\"@
--
-- Length constraints apply to the payload size, and are expressed as bytes
-- in UTF-8 encoding.
startSyncExecution_input :: Lens.Lens' StartSyncExecution (Core.Maybe Core.Text)
startSyncExecution_input = Lens.lens (\StartSyncExecution' {input} -> input) (\s@StartSyncExecution' {} a -> s {input = a} :: StartSyncExecution) Core.. Lens.mapping Core._Sensitive

-- | The name of the execution.
startSyncExecution_name :: Lens.Lens' StartSyncExecution (Core.Maybe Core.Text)
startSyncExecution_name = Lens.lens (\StartSyncExecution' {name} -> name) (\s@StartSyncExecution' {} a -> s {name = a} :: StartSyncExecution)

-- | Passes the AWS X-Ray trace header. The trace header can also be passed
-- in the request payload.
startSyncExecution_traceHeader :: Lens.Lens' StartSyncExecution (Core.Maybe Core.Text)
startSyncExecution_traceHeader = Lens.lens (\StartSyncExecution' {traceHeader} -> traceHeader) (\s@StartSyncExecution' {} a -> s {traceHeader = a} :: StartSyncExecution)

-- | The Amazon Resource Name (ARN) of the state machine to execute.
startSyncExecution_stateMachineArn :: Lens.Lens' StartSyncExecution Core.Text
startSyncExecution_stateMachineArn = Lens.lens (\StartSyncExecution' {stateMachineArn} -> stateMachineArn) (\s@StartSyncExecution' {} a -> s {stateMachineArn = a} :: StartSyncExecution)

instance Core.AWSRequest StartSyncExecution where
  type
    AWSResponse StartSyncExecution =
      StartSyncExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSyncExecutionResponse'
            Core.<$> (x Core..?> "stateMachineArn")
            Core.<*> (x Core..?> "inputDetails")
            Core.<*> (x Core..?> "input")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "output")
            Core.<*> (x Core..?> "cause")
            Core.<*> (x Core..?> "billingDetails")
            Core.<*> (x Core..?> "traceHeader")
            Core.<*> (x Core..?> "error")
            Core.<*> (x Core..?> "outputDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "executionArn")
            Core.<*> (x Core..:> "startDate")
            Core.<*> (x Core..:> "stopDate")
            Core.<*> (x Core..:> "status")
      )

instance Core.Hashable StartSyncExecution

instance Core.NFData StartSyncExecution

instance Core.ToHeaders StartSyncExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSStepFunctions.StartSyncExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartSyncExecution where
  toJSON StartSyncExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ ("input" Core..=) Core.<$> input,
            ("name" Core..=) Core.<$> name,
            ("traceHeader" Core..=) Core.<$> traceHeader,
            Core.Just
              ("stateMachineArn" Core..= stateMachineArn)
          ]
      )

instance Core.ToPath StartSyncExecution where
  toPath = Core.const "/"

instance Core.ToQuery StartSyncExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartSyncExecutionResponse' smart constructor.
data StartSyncExecutionResponse = StartSyncExecutionResponse'
  { -- | The Amazon Resource Name (ARN) that identifies the state machine.
    stateMachineArn :: Core.Maybe Core.Text,
    inputDetails :: Core.Maybe CloudWatchEventsExecutionDataDetails,
    -- | The string that contains the JSON input data of the execution. Length
    -- constraints apply to the payload size, and are expressed as bytes in
    -- UTF-8 encoding.
    input :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The name of the execution.
    name :: Core.Maybe Core.Text,
    -- | The JSON output data of the execution. Length constraints apply to the
    -- payload size, and are expressed as bytes in UTF-8 encoding.
    --
    -- This field is set only if the execution succeeds. If the execution
    -- fails, this field is null.
    output :: Core.Maybe (Core.Sensitive Core.Text),
    -- | A more detailed explanation of the cause of the failure.
    cause :: Core.Maybe (Core.Sensitive Core.Text),
    -- | An object that describes workflow billing details, including billed
    -- duration and memory use.
    billingDetails :: Core.Maybe BillingDetails,
    -- | The AWS X-Ray trace header that was passed to the execution.
    traceHeader :: Core.Maybe Core.Text,
    -- | The error code of the failure.
    error :: Core.Maybe (Core.Sensitive Core.Text),
    outputDetails :: Core.Maybe CloudWatchEventsExecutionDataDetails,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) that identifies the execution.
    executionArn :: Core.Text,
    -- | The date the execution is started.
    startDate :: Core.POSIX,
    -- | If the execution has already ended, the date the execution stopped.
    stopDate :: Core.POSIX,
    -- | The current status of the execution.
    status :: SyncExecutionStatus
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartSyncExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateMachineArn', 'startSyncExecutionResponse_stateMachineArn' - The Amazon Resource Name (ARN) that identifies the state machine.
--
-- 'inputDetails', 'startSyncExecutionResponse_inputDetails' - Undocumented member.
--
-- 'input', 'startSyncExecutionResponse_input' - The string that contains the JSON input data of the execution. Length
-- constraints apply to the payload size, and are expressed as bytes in
-- UTF-8 encoding.
--
-- 'name', 'startSyncExecutionResponse_name' - The name of the execution.
--
-- 'output', 'startSyncExecutionResponse_output' - The JSON output data of the execution. Length constraints apply to the
-- payload size, and are expressed as bytes in UTF-8 encoding.
--
-- This field is set only if the execution succeeds. If the execution
-- fails, this field is null.
--
-- 'cause', 'startSyncExecutionResponse_cause' - A more detailed explanation of the cause of the failure.
--
-- 'billingDetails', 'startSyncExecutionResponse_billingDetails' - An object that describes workflow billing details, including billed
-- duration and memory use.
--
-- 'traceHeader', 'startSyncExecutionResponse_traceHeader' - The AWS X-Ray trace header that was passed to the execution.
--
-- 'error', 'startSyncExecutionResponse_error' - The error code of the failure.
--
-- 'outputDetails', 'startSyncExecutionResponse_outputDetails' - Undocumented member.
--
-- 'httpStatus', 'startSyncExecutionResponse_httpStatus' - The response's http status code.
--
-- 'executionArn', 'startSyncExecutionResponse_executionArn' - The Amazon Resource Name (ARN) that identifies the execution.
--
-- 'startDate', 'startSyncExecutionResponse_startDate' - The date the execution is started.
--
-- 'stopDate', 'startSyncExecutionResponse_stopDate' - If the execution has already ended, the date the execution stopped.
--
-- 'status', 'startSyncExecutionResponse_status' - The current status of the execution.
newStartSyncExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'executionArn'
  Core.Text ->
  -- | 'startDate'
  Core.UTCTime ->
  -- | 'stopDate'
  Core.UTCTime ->
  -- | 'status'
  SyncExecutionStatus ->
  StartSyncExecutionResponse
newStartSyncExecutionResponse
  pHttpStatus_
  pExecutionArn_
  pStartDate_
  pStopDate_
  pStatus_ =
    StartSyncExecutionResponse'
      { stateMachineArn =
          Core.Nothing,
        inputDetails = Core.Nothing,
        input = Core.Nothing,
        name = Core.Nothing,
        output = Core.Nothing,
        cause = Core.Nothing,
        billingDetails = Core.Nothing,
        traceHeader = Core.Nothing,
        error = Core.Nothing,
        outputDetails = Core.Nothing,
        httpStatus = pHttpStatus_,
        executionArn = pExecutionArn_,
        startDate = Core._Time Lens.# pStartDate_,
        stopDate = Core._Time Lens.# pStopDate_,
        status = pStatus_
      }

-- | The Amazon Resource Name (ARN) that identifies the state machine.
startSyncExecutionResponse_stateMachineArn :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Core.Text)
startSyncExecutionResponse_stateMachineArn = Lens.lens (\StartSyncExecutionResponse' {stateMachineArn} -> stateMachineArn) (\s@StartSyncExecutionResponse' {} a -> s {stateMachineArn = a} :: StartSyncExecutionResponse)

-- | Undocumented member.
startSyncExecutionResponse_inputDetails :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe CloudWatchEventsExecutionDataDetails)
startSyncExecutionResponse_inputDetails = Lens.lens (\StartSyncExecutionResponse' {inputDetails} -> inputDetails) (\s@StartSyncExecutionResponse' {} a -> s {inputDetails = a} :: StartSyncExecutionResponse)

-- | The string that contains the JSON input data of the execution. Length
-- constraints apply to the payload size, and are expressed as bytes in
-- UTF-8 encoding.
startSyncExecutionResponse_input :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Core.Text)
startSyncExecutionResponse_input = Lens.lens (\StartSyncExecutionResponse' {input} -> input) (\s@StartSyncExecutionResponse' {} a -> s {input = a} :: StartSyncExecutionResponse) Core.. Lens.mapping Core._Sensitive

-- | The name of the execution.
startSyncExecutionResponse_name :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Core.Text)
startSyncExecutionResponse_name = Lens.lens (\StartSyncExecutionResponse' {name} -> name) (\s@StartSyncExecutionResponse' {} a -> s {name = a} :: StartSyncExecutionResponse)

-- | The JSON output data of the execution. Length constraints apply to the
-- payload size, and are expressed as bytes in UTF-8 encoding.
--
-- This field is set only if the execution succeeds. If the execution
-- fails, this field is null.
startSyncExecutionResponse_output :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Core.Text)
startSyncExecutionResponse_output = Lens.lens (\StartSyncExecutionResponse' {output} -> output) (\s@StartSyncExecutionResponse' {} a -> s {output = a} :: StartSyncExecutionResponse) Core.. Lens.mapping Core._Sensitive

-- | A more detailed explanation of the cause of the failure.
startSyncExecutionResponse_cause :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Core.Text)
startSyncExecutionResponse_cause = Lens.lens (\StartSyncExecutionResponse' {cause} -> cause) (\s@StartSyncExecutionResponse' {} a -> s {cause = a} :: StartSyncExecutionResponse) Core.. Lens.mapping Core._Sensitive

-- | An object that describes workflow billing details, including billed
-- duration and memory use.
startSyncExecutionResponse_billingDetails :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe BillingDetails)
startSyncExecutionResponse_billingDetails = Lens.lens (\StartSyncExecutionResponse' {billingDetails} -> billingDetails) (\s@StartSyncExecutionResponse' {} a -> s {billingDetails = a} :: StartSyncExecutionResponse)

-- | The AWS X-Ray trace header that was passed to the execution.
startSyncExecutionResponse_traceHeader :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Core.Text)
startSyncExecutionResponse_traceHeader = Lens.lens (\StartSyncExecutionResponse' {traceHeader} -> traceHeader) (\s@StartSyncExecutionResponse' {} a -> s {traceHeader = a} :: StartSyncExecutionResponse)

-- | The error code of the failure.
startSyncExecutionResponse_error :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Core.Text)
startSyncExecutionResponse_error = Lens.lens (\StartSyncExecutionResponse' {error} -> error) (\s@StartSyncExecutionResponse' {} a -> s {error = a} :: StartSyncExecutionResponse) Core.. Lens.mapping Core._Sensitive

-- | Undocumented member.
startSyncExecutionResponse_outputDetails :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe CloudWatchEventsExecutionDataDetails)
startSyncExecutionResponse_outputDetails = Lens.lens (\StartSyncExecutionResponse' {outputDetails} -> outputDetails) (\s@StartSyncExecutionResponse' {} a -> s {outputDetails = a} :: StartSyncExecutionResponse)

-- | The response's http status code.
startSyncExecutionResponse_httpStatus :: Lens.Lens' StartSyncExecutionResponse Core.Int
startSyncExecutionResponse_httpStatus = Lens.lens (\StartSyncExecutionResponse' {httpStatus} -> httpStatus) (\s@StartSyncExecutionResponse' {} a -> s {httpStatus = a} :: StartSyncExecutionResponse)

-- | The Amazon Resource Name (ARN) that identifies the execution.
startSyncExecutionResponse_executionArn :: Lens.Lens' StartSyncExecutionResponse Core.Text
startSyncExecutionResponse_executionArn = Lens.lens (\StartSyncExecutionResponse' {executionArn} -> executionArn) (\s@StartSyncExecutionResponse' {} a -> s {executionArn = a} :: StartSyncExecutionResponse)

-- | The date the execution is started.
startSyncExecutionResponse_startDate :: Lens.Lens' StartSyncExecutionResponse Core.UTCTime
startSyncExecutionResponse_startDate = Lens.lens (\StartSyncExecutionResponse' {startDate} -> startDate) (\s@StartSyncExecutionResponse' {} a -> s {startDate = a} :: StartSyncExecutionResponse) Core.. Core._Time

-- | If the execution has already ended, the date the execution stopped.
startSyncExecutionResponse_stopDate :: Lens.Lens' StartSyncExecutionResponse Core.UTCTime
startSyncExecutionResponse_stopDate = Lens.lens (\StartSyncExecutionResponse' {stopDate} -> stopDate) (\s@StartSyncExecutionResponse' {} a -> s {stopDate = a} :: StartSyncExecutionResponse) Core.. Core._Time

-- | The current status of the execution.
startSyncExecutionResponse_status :: Lens.Lens' StartSyncExecutionResponse SyncExecutionStatus
startSyncExecutionResponse_status = Lens.lens (\StartSyncExecutionResponse' {status} -> status) (\s@StartSyncExecutionResponse' {} a -> s {status = a} :: StartSyncExecutionResponse)

instance Core.NFData StartSyncExecutionResponse
