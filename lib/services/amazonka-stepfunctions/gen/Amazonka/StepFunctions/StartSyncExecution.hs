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
-- Module      : Amazonka.StepFunctions.StartSyncExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a Synchronous Express state machine execution.
-- @StartSyncExecution@ is not available for @STANDARD@ workflows.
--
-- @StartSyncExecution@ will return a @200 OK@ response, even if your
-- execution fails, because the status code in the API response doesn\'t
-- reflect function errors. Error codes are reserved for errors that
-- prevent your execution from running, such as permissions errors, limit
-- errors, or issues with your state machine code and configuration.
--
-- This API action isn\'t logged in CloudTrail.
module Amazonka.StepFunctions.StartSyncExecution
  ( -- * Creating a Request
    StartSyncExecution (..),
    newStartSyncExecution,

    -- * Request Lenses
    startSyncExecution_name,
    startSyncExecution_input,
    startSyncExecution_traceHeader,
    startSyncExecution_stateMachineArn,

    -- * Destructuring the Response
    StartSyncExecutionResponse (..),
    newStartSyncExecutionResponse,

    -- * Response Lenses
    startSyncExecutionResponse_name,
    startSyncExecutionResponse_billingDetails,
    startSyncExecutionResponse_inputDetails,
    startSyncExecutionResponse_outputDetails,
    startSyncExecutionResponse_stateMachineArn,
    startSyncExecutionResponse_input,
    startSyncExecutionResponse_output,
    startSyncExecutionResponse_error,
    startSyncExecutionResponse_cause,
    startSyncExecutionResponse_traceHeader,
    startSyncExecutionResponse_httpStatus,
    startSyncExecutionResponse_executionArn,
    startSyncExecutionResponse_startDate,
    startSyncExecutionResponse_stopDate,
    startSyncExecutionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newStartSyncExecution' smart constructor.
data StartSyncExecution = StartSyncExecution'
  { -- | The name of the execution.
    name :: Prelude.Maybe Prelude.Text,
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
    input :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Passes the X-Ray trace header. The trace header can also be passed in
    -- the request payload.
    traceHeader :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the state machine to execute.
    stateMachineArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSyncExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'startSyncExecution_name' - The name of the execution.
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
-- 'traceHeader', 'startSyncExecution_traceHeader' - Passes the X-Ray trace header. The trace header can also be passed in
-- the request payload.
--
-- 'stateMachineArn', 'startSyncExecution_stateMachineArn' - The Amazon Resource Name (ARN) of the state machine to execute.
newStartSyncExecution ::
  -- | 'stateMachineArn'
  Prelude.Text ->
  StartSyncExecution
newStartSyncExecution pStateMachineArn_ =
  StartSyncExecution'
    { name = Prelude.Nothing,
      input = Prelude.Nothing,
      traceHeader = Prelude.Nothing,
      stateMachineArn = pStateMachineArn_
    }

-- | The name of the execution.
startSyncExecution_name :: Lens.Lens' StartSyncExecution (Prelude.Maybe Prelude.Text)
startSyncExecution_name = Lens.lens (\StartSyncExecution' {name} -> name) (\s@StartSyncExecution' {} a -> s {name = a} :: StartSyncExecution)

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
startSyncExecution_input :: Lens.Lens' StartSyncExecution (Prelude.Maybe Prelude.Text)
startSyncExecution_input = Lens.lens (\StartSyncExecution' {input} -> input) (\s@StartSyncExecution' {} a -> s {input = a} :: StartSyncExecution) Prelude.. Lens.mapping Data._Sensitive

-- | Passes the X-Ray trace header. The trace header can also be passed in
-- the request payload.
startSyncExecution_traceHeader :: Lens.Lens' StartSyncExecution (Prelude.Maybe Prelude.Text)
startSyncExecution_traceHeader = Lens.lens (\StartSyncExecution' {traceHeader} -> traceHeader) (\s@StartSyncExecution' {} a -> s {traceHeader = a} :: StartSyncExecution)

-- | The Amazon Resource Name (ARN) of the state machine to execute.
startSyncExecution_stateMachineArn :: Lens.Lens' StartSyncExecution Prelude.Text
startSyncExecution_stateMachineArn = Lens.lens (\StartSyncExecution' {stateMachineArn} -> stateMachineArn) (\s@StartSyncExecution' {} a -> s {stateMachineArn = a} :: StartSyncExecution)

instance Core.AWSRequest StartSyncExecution where
  type
    AWSResponse StartSyncExecution =
      StartSyncExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSyncExecutionResponse'
            Prelude.<$> (x Data..?> "name")
            Prelude.<*> (x Data..?> "billingDetails")
            Prelude.<*> (x Data..?> "inputDetails")
            Prelude.<*> (x Data..?> "outputDetails")
            Prelude.<*> (x Data..?> "stateMachineArn")
            Prelude.<*> (x Data..?> "input")
            Prelude.<*> (x Data..?> "output")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (x Data..?> "cause")
            Prelude.<*> (x Data..?> "traceHeader")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "executionArn")
            Prelude.<*> (x Data..:> "startDate")
            Prelude.<*> (x Data..:> "stopDate")
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable StartSyncExecution where
  hashWithSalt _salt StartSyncExecution' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` input
      `Prelude.hashWithSalt` traceHeader
      `Prelude.hashWithSalt` stateMachineArn

instance Prelude.NFData StartSyncExecution where
  rnf StartSyncExecution' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf traceHeader
      `Prelude.seq` Prelude.rnf stateMachineArn

instance Data.ToHeaders StartSyncExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.StartSyncExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartSyncExecution where
  toJSON StartSyncExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("input" Data..=) Prelude.<$> input,
            ("traceHeader" Data..=) Prelude.<$> traceHeader,
            Prelude.Just
              ("stateMachineArn" Data..= stateMachineArn)
          ]
      )

instance Data.ToPath StartSyncExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery StartSyncExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSyncExecutionResponse' smart constructor.
data StartSyncExecutionResponse = StartSyncExecutionResponse'
  { -- | The name of the execution.
    name :: Prelude.Maybe Prelude.Text,
    -- | An object that describes workflow billing details, including billed
    -- duration and memory use.
    billingDetails :: Prelude.Maybe BillingDetails,
    inputDetails :: Prelude.Maybe CloudWatchEventsExecutionDataDetails,
    outputDetails :: Prelude.Maybe CloudWatchEventsExecutionDataDetails,
    -- | The Amazon Resource Name (ARN) that identifies the state machine.
    stateMachineArn :: Prelude.Maybe Prelude.Text,
    -- | The string that contains the JSON input data of the execution. Length
    -- constraints apply to the payload size, and are expressed as bytes in
    -- UTF-8 encoding.
    input :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The JSON output data of the execution. Length constraints apply to the
    -- payload size, and are expressed as bytes in UTF-8 encoding.
    --
    -- This field is set only if the execution succeeds. If the execution
    -- fails, this field is null.
    output :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The error code of the failure.
    error :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The X-Ray trace header that was passed to the execution.
    traceHeader :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) that identifies the execution.
    executionArn :: Prelude.Text,
    -- | The date the execution is started.
    startDate :: Data.POSIX,
    -- | If the execution has already ended, the date the execution stopped.
    stopDate :: Data.POSIX,
    -- | The current status of the execution.
    status :: SyncExecutionStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSyncExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'startSyncExecutionResponse_name' - The name of the execution.
--
-- 'billingDetails', 'startSyncExecutionResponse_billingDetails' - An object that describes workflow billing details, including billed
-- duration and memory use.
--
-- 'inputDetails', 'startSyncExecutionResponse_inputDetails' - Undocumented member.
--
-- 'outputDetails', 'startSyncExecutionResponse_outputDetails' - Undocumented member.
--
-- 'stateMachineArn', 'startSyncExecutionResponse_stateMachineArn' - The Amazon Resource Name (ARN) that identifies the state machine.
--
-- 'input', 'startSyncExecutionResponse_input' - The string that contains the JSON input data of the execution. Length
-- constraints apply to the payload size, and are expressed as bytes in
-- UTF-8 encoding.
--
-- 'output', 'startSyncExecutionResponse_output' - The JSON output data of the execution. Length constraints apply to the
-- payload size, and are expressed as bytes in UTF-8 encoding.
--
-- This field is set only if the execution succeeds. If the execution
-- fails, this field is null.
--
-- 'error', 'startSyncExecutionResponse_error' - The error code of the failure.
--
-- 'cause', 'startSyncExecutionResponse_cause' - A more detailed explanation of the cause of the failure.
--
-- 'traceHeader', 'startSyncExecutionResponse_traceHeader' - The X-Ray trace header that was passed to the execution.
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
  Prelude.Int ->
  -- | 'executionArn'
  Prelude.Text ->
  -- | 'startDate'
  Prelude.UTCTime ->
  -- | 'stopDate'
  Prelude.UTCTime ->
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
      { name = Prelude.Nothing,
        billingDetails = Prelude.Nothing,
        inputDetails = Prelude.Nothing,
        outputDetails = Prelude.Nothing,
        stateMachineArn = Prelude.Nothing,
        input = Prelude.Nothing,
        output = Prelude.Nothing,
        error = Prelude.Nothing,
        cause = Prelude.Nothing,
        traceHeader = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        executionArn = pExecutionArn_,
        startDate = Data._Time Lens.# pStartDate_,
        stopDate = Data._Time Lens.# pStopDate_,
        status = pStatus_
      }

-- | The name of the execution.
startSyncExecutionResponse_name :: Lens.Lens' StartSyncExecutionResponse (Prelude.Maybe Prelude.Text)
startSyncExecutionResponse_name = Lens.lens (\StartSyncExecutionResponse' {name} -> name) (\s@StartSyncExecutionResponse' {} a -> s {name = a} :: StartSyncExecutionResponse)

-- | An object that describes workflow billing details, including billed
-- duration and memory use.
startSyncExecutionResponse_billingDetails :: Lens.Lens' StartSyncExecutionResponse (Prelude.Maybe BillingDetails)
startSyncExecutionResponse_billingDetails = Lens.lens (\StartSyncExecutionResponse' {billingDetails} -> billingDetails) (\s@StartSyncExecutionResponse' {} a -> s {billingDetails = a} :: StartSyncExecutionResponse)

-- | Undocumented member.
startSyncExecutionResponse_inputDetails :: Lens.Lens' StartSyncExecutionResponse (Prelude.Maybe CloudWatchEventsExecutionDataDetails)
startSyncExecutionResponse_inputDetails = Lens.lens (\StartSyncExecutionResponse' {inputDetails} -> inputDetails) (\s@StartSyncExecutionResponse' {} a -> s {inputDetails = a} :: StartSyncExecutionResponse)

-- | Undocumented member.
startSyncExecutionResponse_outputDetails :: Lens.Lens' StartSyncExecutionResponse (Prelude.Maybe CloudWatchEventsExecutionDataDetails)
startSyncExecutionResponse_outputDetails = Lens.lens (\StartSyncExecutionResponse' {outputDetails} -> outputDetails) (\s@StartSyncExecutionResponse' {} a -> s {outputDetails = a} :: StartSyncExecutionResponse)

-- | The Amazon Resource Name (ARN) that identifies the state machine.
startSyncExecutionResponse_stateMachineArn :: Lens.Lens' StartSyncExecutionResponse (Prelude.Maybe Prelude.Text)
startSyncExecutionResponse_stateMachineArn = Lens.lens (\StartSyncExecutionResponse' {stateMachineArn} -> stateMachineArn) (\s@StartSyncExecutionResponse' {} a -> s {stateMachineArn = a} :: StartSyncExecutionResponse)

-- | The string that contains the JSON input data of the execution. Length
-- constraints apply to the payload size, and are expressed as bytes in
-- UTF-8 encoding.
startSyncExecutionResponse_input :: Lens.Lens' StartSyncExecutionResponse (Prelude.Maybe Prelude.Text)
startSyncExecutionResponse_input = Lens.lens (\StartSyncExecutionResponse' {input} -> input) (\s@StartSyncExecutionResponse' {} a -> s {input = a} :: StartSyncExecutionResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The JSON output data of the execution. Length constraints apply to the
-- payload size, and are expressed as bytes in UTF-8 encoding.
--
-- This field is set only if the execution succeeds. If the execution
-- fails, this field is null.
startSyncExecutionResponse_output :: Lens.Lens' StartSyncExecutionResponse (Prelude.Maybe Prelude.Text)
startSyncExecutionResponse_output = Lens.lens (\StartSyncExecutionResponse' {output} -> output) (\s@StartSyncExecutionResponse' {} a -> s {output = a} :: StartSyncExecutionResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The error code of the failure.
startSyncExecutionResponse_error :: Lens.Lens' StartSyncExecutionResponse (Prelude.Maybe Prelude.Text)
startSyncExecutionResponse_error = Lens.lens (\StartSyncExecutionResponse' {error} -> error) (\s@StartSyncExecutionResponse' {} a -> s {error = a} :: StartSyncExecutionResponse) Prelude.. Lens.mapping Data._Sensitive

-- | A more detailed explanation of the cause of the failure.
startSyncExecutionResponse_cause :: Lens.Lens' StartSyncExecutionResponse (Prelude.Maybe Prelude.Text)
startSyncExecutionResponse_cause = Lens.lens (\StartSyncExecutionResponse' {cause} -> cause) (\s@StartSyncExecutionResponse' {} a -> s {cause = a} :: StartSyncExecutionResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The X-Ray trace header that was passed to the execution.
startSyncExecutionResponse_traceHeader :: Lens.Lens' StartSyncExecutionResponse (Prelude.Maybe Prelude.Text)
startSyncExecutionResponse_traceHeader = Lens.lens (\StartSyncExecutionResponse' {traceHeader} -> traceHeader) (\s@StartSyncExecutionResponse' {} a -> s {traceHeader = a} :: StartSyncExecutionResponse)

-- | The response's http status code.
startSyncExecutionResponse_httpStatus :: Lens.Lens' StartSyncExecutionResponse Prelude.Int
startSyncExecutionResponse_httpStatus = Lens.lens (\StartSyncExecutionResponse' {httpStatus} -> httpStatus) (\s@StartSyncExecutionResponse' {} a -> s {httpStatus = a} :: StartSyncExecutionResponse)

-- | The Amazon Resource Name (ARN) that identifies the execution.
startSyncExecutionResponse_executionArn :: Lens.Lens' StartSyncExecutionResponse Prelude.Text
startSyncExecutionResponse_executionArn = Lens.lens (\StartSyncExecutionResponse' {executionArn} -> executionArn) (\s@StartSyncExecutionResponse' {} a -> s {executionArn = a} :: StartSyncExecutionResponse)

-- | The date the execution is started.
startSyncExecutionResponse_startDate :: Lens.Lens' StartSyncExecutionResponse Prelude.UTCTime
startSyncExecutionResponse_startDate = Lens.lens (\StartSyncExecutionResponse' {startDate} -> startDate) (\s@StartSyncExecutionResponse' {} a -> s {startDate = a} :: StartSyncExecutionResponse) Prelude.. Data._Time

-- | If the execution has already ended, the date the execution stopped.
startSyncExecutionResponse_stopDate :: Lens.Lens' StartSyncExecutionResponse Prelude.UTCTime
startSyncExecutionResponse_stopDate = Lens.lens (\StartSyncExecutionResponse' {stopDate} -> stopDate) (\s@StartSyncExecutionResponse' {} a -> s {stopDate = a} :: StartSyncExecutionResponse) Prelude.. Data._Time

-- | The current status of the execution.
startSyncExecutionResponse_status :: Lens.Lens' StartSyncExecutionResponse SyncExecutionStatus
startSyncExecutionResponse_status = Lens.lens (\StartSyncExecutionResponse' {status} -> status) (\s@StartSyncExecutionResponse' {} a -> s {status = a} :: StartSyncExecutionResponse)

instance Prelude.NFData StartSyncExecutionResponse where
  rnf StartSyncExecutionResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf billingDetails
      `Prelude.seq` Prelude.rnf inputDetails
      `Prelude.seq` Prelude.rnf outputDetails
      `Prelude.seq` Prelude.rnf stateMachineArn
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf output
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf cause
      `Prelude.seq` Prelude.rnf traceHeader
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf executionArn
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf stopDate
      `Prelude.seq` Prelude.rnf status
