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
-- Module      : Amazonka.StepFunctions.StartExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a state machine execution.
--
-- A qualified state machine ARN can either refer to a /Distributed Map
-- state/ defined within a state machine, a version ARN, or an alias ARN.
--
-- The following are some examples of qualified and unqualified state
-- machine ARNs:
--
-- -   The following qualified state machine ARN refers to a /Distributed
--     Map state/ with a label @mapStateLabel@ in a state machine named
--     @myStateMachine@.
--
--     @arn:partition:states:region:account-id:stateMachine:myStateMachine\/mapStateLabel@
--
--     If you provide a qualified state machine ARN that refers to a
--     /Distributed Map state/, the request fails with
--     @ValidationException@.
--
-- -   The following qualified state machine ARN refers to an alias named
--     @PROD@.
--
--     @arn:\<partition>:states:\<region>:\<account-id>:stateMachine:\<myStateMachine:PROD>@
--
--     If you provide a qualified state machine ARN that refers to a
--     version ARN or an alias ARN, the request starts execution for that
--     version or alias.
--
-- -   The following unqualified state machine ARN refers to a state
--     machine named @myStateMachine@.
--
--     @arn:\<partition>:states:\<region>:\<account-id>:stateMachine:\<myStateMachine>@
--
-- If you start an execution with an unqualified state machine ARN, Step
-- Functions uses the latest revision of the state machine for the
-- execution.
--
-- To start executions of a state machine
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-state-machine-version.html version>,
-- call @StartExecution@ and provide the version ARN or the ARN of an
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-state-machine-alias.html alias>
-- that points to the version.
--
-- @StartExecution@ is idempotent for @STANDARD@ workflows. For a
-- @STANDARD@ workflow, if you call @StartExecution@ with the same name and
-- input as a running execution, the call succeeds and return the same
-- response as the original request. If the execution is closed or if the
-- input is different, it returns a @400 ExecutionAlreadyExists@ error. You
-- can reuse names after 90 days.
--
-- @StartExecution@ isn\'t idempotent for @EXPRESS@ workflows.
module Amazonka.StepFunctions.StartExecution
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

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
    input :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Optional name of the execution. This name must be unique for your Amazon
    -- Web Services account, Region, and state machine for 90 days. For more
    -- information, see
    -- <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions>
    -- in the /Step Functions Developer Guide/.
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
    -- | Passes the X-Ray trace header. The trace header can also be passed in
    -- the request payload.
    traceHeader :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the state machine to execute.
    --
    -- The @stateMachineArn@ parameter accepts one of the following inputs:
    --
    -- -   __An unqualified state machine ARN__ – Refers to a state machine ARN
    --     that isn\'t qualified with a version or alias ARN. The following is
    --     an example of an unqualified state machine ARN.
    --
    --     @arn:\<partition>:states:\<region>:\<account-id>:stateMachine:\<myStateMachine>@
    --
    --     Step Functions doesn\'t associate state machine executions that you
    --     start with an unqualified ARN with a version. This is true even if
    --     that version uses the same revision that the execution used.
    --
    -- -   __A state machine version ARN__ – Refers to a version ARN, which is
    --     a combination of state machine ARN and the version number separated
    --     by a colon (:). The following is an example of the ARN for version
    --     10.
    --
    --     @arn:\<partition>:states:\<region>:\<account-id>:stateMachine:\<myStateMachine>:10@
    --
    --     Step Functions doesn\'t associate executions that you start with a
    --     version ARN with any aliases that point to that version.
    --
    -- -   __A state machine alias ARN__ – Refers to an alias ARN, which is a
    --     combination of state machine ARN and the alias name separated by a
    --     colon (:). The following is an example of the ARN for an alias named
    --     @PROD@.
    --
    --     @arn:\<partition>:states:\<region>:\<account-id>:stateMachine:\<myStateMachine:PROD>@
    --
    --     Step Functions associates executions that you start with an alias
    --     ARN with that alias and the state machine version used for that
    --     execution.
    stateMachineArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'name', 'startExecution_name' - Optional name of the execution. This name must be unique for your Amazon
-- Web Services account, Region, and state machine for 90 days. For more
-- information, see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions>
-- in the /Step Functions Developer Guide/.
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
-- 'traceHeader', 'startExecution_traceHeader' - Passes the X-Ray trace header. The trace header can also be passed in
-- the request payload.
--
-- 'stateMachineArn', 'startExecution_stateMachineArn' - The Amazon Resource Name (ARN) of the state machine to execute.
--
-- The @stateMachineArn@ parameter accepts one of the following inputs:
--
-- -   __An unqualified state machine ARN__ – Refers to a state machine ARN
--     that isn\'t qualified with a version or alias ARN. The following is
--     an example of an unqualified state machine ARN.
--
--     @arn:\<partition>:states:\<region>:\<account-id>:stateMachine:\<myStateMachine>@
--
--     Step Functions doesn\'t associate state machine executions that you
--     start with an unqualified ARN with a version. This is true even if
--     that version uses the same revision that the execution used.
--
-- -   __A state machine version ARN__ – Refers to a version ARN, which is
--     a combination of state machine ARN and the version number separated
--     by a colon (:). The following is an example of the ARN for version
--     10.
--
--     @arn:\<partition>:states:\<region>:\<account-id>:stateMachine:\<myStateMachine>:10@
--
--     Step Functions doesn\'t associate executions that you start with a
--     version ARN with any aliases that point to that version.
--
-- -   __A state machine alias ARN__ – Refers to an alias ARN, which is a
--     combination of state machine ARN and the alias name separated by a
--     colon (:). The following is an example of the ARN for an alias named
--     @PROD@.
--
--     @arn:\<partition>:states:\<region>:\<account-id>:stateMachine:\<myStateMachine:PROD>@
--
--     Step Functions associates executions that you start with an alias
--     ARN with that alias and the state machine version used for that
--     execution.
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
startExecution_input = Lens.lens (\StartExecution' {input} -> input) (\s@StartExecution' {} a -> s {input = a} :: StartExecution) Prelude.. Lens.mapping Data._Sensitive

-- | Optional name of the execution. This name must be unique for your Amazon
-- Web Services account, Region, and state machine for 90 days. For more
-- information, see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions>
-- in the /Step Functions Developer Guide/.
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

-- | Passes the X-Ray trace header. The trace header can also be passed in
-- the request payload.
startExecution_traceHeader :: Lens.Lens' StartExecution (Prelude.Maybe Prelude.Text)
startExecution_traceHeader = Lens.lens (\StartExecution' {traceHeader} -> traceHeader) (\s@StartExecution' {} a -> s {traceHeader = a} :: StartExecution)

-- | The Amazon Resource Name (ARN) of the state machine to execute.
--
-- The @stateMachineArn@ parameter accepts one of the following inputs:
--
-- -   __An unqualified state machine ARN__ – Refers to a state machine ARN
--     that isn\'t qualified with a version or alias ARN. The following is
--     an example of an unqualified state machine ARN.
--
--     @arn:\<partition>:states:\<region>:\<account-id>:stateMachine:\<myStateMachine>@
--
--     Step Functions doesn\'t associate state machine executions that you
--     start with an unqualified ARN with a version. This is true even if
--     that version uses the same revision that the execution used.
--
-- -   __A state machine version ARN__ – Refers to a version ARN, which is
--     a combination of state machine ARN and the version number separated
--     by a colon (:). The following is an example of the ARN for version
--     10.
--
--     @arn:\<partition>:states:\<region>:\<account-id>:stateMachine:\<myStateMachine>:10@
--
--     Step Functions doesn\'t associate executions that you start with a
--     version ARN with any aliases that point to that version.
--
-- -   __A state machine alias ARN__ – Refers to an alias ARN, which is a
--     combination of state machine ARN and the alias name separated by a
--     colon (:). The following is an example of the ARN for an alias named
--     @PROD@.
--
--     @arn:\<partition>:states:\<region>:\<account-id>:stateMachine:\<myStateMachine:PROD>@
--
--     Step Functions associates executions that you start with an alias
--     ARN with that alias and the state machine version used for that
--     execution.
startExecution_stateMachineArn :: Lens.Lens' StartExecution Prelude.Text
startExecution_stateMachineArn = Lens.lens (\StartExecution' {stateMachineArn} -> stateMachineArn) (\s@StartExecution' {} a -> s {stateMachineArn = a} :: StartExecution)

instance Core.AWSRequest StartExecution where
  type
    AWSResponse StartExecution =
      StartExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartExecutionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "executionArn")
            Prelude.<*> (x Data..:> "startDate")
      )

instance Prelude.Hashable StartExecution where
  hashWithSalt _salt StartExecution' {..} =
    _salt
      `Prelude.hashWithSalt` input
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` traceHeader
      `Prelude.hashWithSalt` stateMachineArn

instance Prelude.NFData StartExecution where
  rnf StartExecution' {..} =
    Prelude.rnf input
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf traceHeader
      `Prelude.seq` Prelude.rnf stateMachineArn

instance Data.ToHeaders StartExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.StartExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartExecution where
  toJSON StartExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("input" Data..=) Prelude.<$> input,
            ("name" Data..=) Prelude.<$> name,
            ("traceHeader" Data..=) Prelude.<$> traceHeader,
            Prelude.Just
              ("stateMachineArn" Data..= stateMachineArn)
          ]
      )

instance Data.ToPath StartExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery StartExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartExecutionResponse' smart constructor.
data StartExecutionResponse = StartExecutionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) that identifies the execution.
    executionArn :: Prelude.Text,
    -- | The date the execution is started.
    startDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        startDate = Data._Time Lens.# pStartDate_
      }

-- | The response's http status code.
startExecutionResponse_httpStatus :: Lens.Lens' StartExecutionResponse Prelude.Int
startExecutionResponse_httpStatus = Lens.lens (\StartExecutionResponse' {httpStatus} -> httpStatus) (\s@StartExecutionResponse' {} a -> s {httpStatus = a} :: StartExecutionResponse)

-- | The Amazon Resource Name (ARN) that identifies the execution.
startExecutionResponse_executionArn :: Lens.Lens' StartExecutionResponse Prelude.Text
startExecutionResponse_executionArn = Lens.lens (\StartExecutionResponse' {executionArn} -> executionArn) (\s@StartExecutionResponse' {} a -> s {executionArn = a} :: StartExecutionResponse)

-- | The date the execution is started.
startExecutionResponse_startDate :: Lens.Lens' StartExecutionResponse Prelude.UTCTime
startExecutionResponse_startDate = Lens.lens (\StartExecutionResponse' {startDate} -> startDate) (\s@StartExecutionResponse' {} a -> s {startDate = a} :: StartExecutionResponse) Prelude.. Data._Time

instance Prelude.NFData StartExecutionResponse where
  rnf StartExecutionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf executionArn
      `Prelude.seq` Prelude.rnf startDate
