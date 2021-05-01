{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StepFunctions.StartExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a state machine execution.
--
-- @StartExecution@ is idempotent. If @StartExecution@ is called with the
-- same name and input as a running execution, the call will succeed and
-- return the same response as the original request. If the execution is
-- closed or if the input is different, it will return a 400
-- @ExecutionAlreadyExists@ error. Names can be reused after 90 days.
module Network.AWS.StepFunctions.StartExecution
  ( -- * Creating a Request
    StartExecution (..),
    newStartExecution,

    -- * Request Lenses
    startExecution_input,
    startExecution_name,
    startExecution_traceHeader,
    startExecution_stateMachineArn,

    -- * Destructuring the Response
    StartExecutionResponse (..),
    newStartExecutionResponse,

    -- * Response Lenses
    startExecutionResponse_httpStatus,
    startExecutionResponse_executionArn,
    startExecutionResponse_startDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newStartExecution' smart constructor.
data StartExecution = StartExecution'
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
    input :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The name of the execution. This name must be unique for your AWS
    -- account, region, and state machine for 90 days. For more information,
    -- see
    -- <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions>
    -- in the /AWS Step Functions Developer Guide/.
    --
    -- A name must /not/ contain:
    --
    -- -   white space
    --
    -- -   brackets @\< > { } [ ]@
    --
    -- -   wildcard characters @? *@
    --
    -- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
    --
    -- -   control characters (@U+0000-001F@, @U+007F-009F@)
    --
    -- To enable logging with CloudWatch Logs, the name should only contain
    -- 0-9, A-Z, a-z, - and _.
    name :: Prelude.Maybe Prelude.Text,
    -- | Passes the AWS X-Ray trace header. The trace header can also be passed
    -- in the request payload.
    traceHeader :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the state machine to execute.
    stateMachineArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'startExecution_input' - The string that contains the JSON input data for the execution, for
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
-- 'name', 'startExecution_name' - The name of the execution. This name must be unique for your AWS
-- account, region, and state machine for 90 days. For more information,
-- see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions>
-- in the /AWS Step Functions Developer Guide/.
--
-- A name must /not/ contain:
--
-- -   white space
--
-- -   brackets @\< > { } [ ]@
--
-- -   wildcard characters @? *@
--
-- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
--
-- -   control characters (@U+0000-001F@, @U+007F-009F@)
--
-- To enable logging with CloudWatch Logs, the name should only contain
-- 0-9, A-Z, a-z, - and _.
--
-- 'traceHeader', 'startExecution_traceHeader' - Passes the AWS X-Ray trace header. The trace header can also be passed
-- in the request payload.
--
-- 'stateMachineArn', 'startExecution_stateMachineArn' - The Amazon Resource Name (ARN) of the state machine to execute.
newStartExecution ::
  -- | 'stateMachineArn'
  Prelude.Text ->
  StartExecution
newStartExecution pStateMachineArn_ =
  StartExecution'
    { input = Prelude.Nothing,
      name = Prelude.Nothing,
      traceHeader = Prelude.Nothing,
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
startExecution_input :: Lens.Lens' StartExecution (Prelude.Maybe Prelude.Text)
startExecution_input = Lens.lens (\StartExecution' {input} -> input) (\s@StartExecution' {} a -> s {input = a} :: StartExecution) Prelude.. Lens.mapping Prelude._Sensitive

-- | The name of the execution. This name must be unique for your AWS
-- account, region, and state machine for 90 days. For more information,
-- see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions>
-- in the /AWS Step Functions Developer Guide/.
--
-- A name must /not/ contain:
--
-- -   white space
--
-- -   brackets @\< > { } [ ]@
--
-- -   wildcard characters @? *@
--
-- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
--
-- -   control characters (@U+0000-001F@, @U+007F-009F@)
--
-- To enable logging with CloudWatch Logs, the name should only contain
-- 0-9, A-Z, a-z, - and _.
startExecution_name :: Lens.Lens' StartExecution (Prelude.Maybe Prelude.Text)
startExecution_name = Lens.lens (\StartExecution' {name} -> name) (\s@StartExecution' {} a -> s {name = a} :: StartExecution)

-- | Passes the AWS X-Ray trace header. The trace header can also be passed
-- in the request payload.
startExecution_traceHeader :: Lens.Lens' StartExecution (Prelude.Maybe Prelude.Text)
startExecution_traceHeader = Lens.lens (\StartExecution' {traceHeader} -> traceHeader) (\s@StartExecution' {} a -> s {traceHeader = a} :: StartExecution)

-- | The Amazon Resource Name (ARN) of the state machine to execute.
startExecution_stateMachineArn :: Lens.Lens' StartExecution Prelude.Text
startExecution_stateMachineArn = Lens.lens (\StartExecution' {stateMachineArn} -> stateMachineArn) (\s@StartExecution' {} a -> s {stateMachineArn = a} :: StartExecution)

instance Prelude.AWSRequest StartExecution where
  type Rs StartExecution = StartExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartExecutionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "executionArn")
            Prelude.<*> (x Prelude..:> "startDate")
      )

instance Prelude.Hashable StartExecution

instance Prelude.NFData StartExecution

instance Prelude.ToHeaders StartExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSStepFunctions.StartExecution" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartExecution where
  toJSON StartExecution' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("input" Prelude..=) Prelude.<$> input,
            ("name" Prelude..=) Prelude.<$> name,
            ("traceHeader" Prelude..=) Prelude.<$> traceHeader,
            Prelude.Just
              ("stateMachineArn" Prelude..= stateMachineArn)
          ]
      )

instance Prelude.ToPath StartExecution where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartExecutionResponse' smart constructor.
data StartExecutionResponse = StartExecutionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) that identifies the execution.
    executionArn :: Prelude.Text,
    -- | The date the execution is started.
    startDate :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startExecutionResponse_httpStatus' - The response's http status code.
--
-- 'executionArn', 'startExecutionResponse_executionArn' - The Amazon Resource Name (ARN) that identifies the execution.
--
-- 'startDate', 'startExecutionResponse_startDate' - The date the execution is started.
newStartExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'executionArn'
  Prelude.Text ->
  -- | 'startDate'
  Prelude.UTCTime ->
  StartExecutionResponse
newStartExecutionResponse
  pHttpStatus_
  pExecutionArn_
  pStartDate_ =
    StartExecutionResponse'
      { httpStatus = pHttpStatus_,
        executionArn = pExecutionArn_,
        startDate = Prelude._Time Lens.# pStartDate_
      }

-- | The response's http status code.
startExecutionResponse_httpStatus :: Lens.Lens' StartExecutionResponse Prelude.Int
startExecutionResponse_httpStatus = Lens.lens (\StartExecutionResponse' {httpStatus} -> httpStatus) (\s@StartExecutionResponse' {} a -> s {httpStatus = a} :: StartExecutionResponse)

-- | The Amazon Resource Name (ARN) that identifies the execution.
startExecutionResponse_executionArn :: Lens.Lens' StartExecutionResponse Prelude.Text
startExecutionResponse_executionArn = Lens.lens (\StartExecutionResponse' {executionArn} -> executionArn) (\s@StartExecutionResponse' {} a -> s {executionArn = a} :: StartExecutionResponse)

-- | The date the execution is started.
startExecutionResponse_startDate :: Lens.Lens' StartExecutionResponse Prelude.UTCTime
startExecutionResponse_startDate = Lens.lens (\StartExecutionResponse' {startDate} -> startDate) (\s@StartExecutionResponse' {} a -> s {startDate = a} :: StartExecutionResponse) Prelude.. Prelude._Time

instance Prelude.NFData StartExecutionResponse
