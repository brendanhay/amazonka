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
-- Module      : Amazonka.SWF.StartWorkflowExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an execution of the workflow type in the specified domain using
-- the provided @workflowId@ and input data.
--
-- This action returns the newly started workflow execution.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--
--     -   @tagList.member.0@: The key is @swf:tagList.member.0@.
--
--     -   @tagList.member.1@: The key is @swf:tagList.member.1@.
--
--     -   @tagList.member.2@: The key is @swf:tagList.member.2@.
--
--     -   @tagList.member.3@: The key is @swf:tagList.member.3@.
--
--     -   @tagList.member.4@: The key is @swf:tagList.member.4@.
--
--     -   @taskList@: String constraint. The key is @swf:taskList.name@.
--
--     -   @workflowType.name@: String constraint. The key is
--         @swf:workflowType.name@.
--
--     -   @workflowType.version@: String constraint. The key is
--         @swf:workflowType.version@.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
module Amazonka.SWF.StartWorkflowExecution
  ( -- * Creating a Request
    StartWorkflowExecution (..),
    newStartWorkflowExecution,

    -- * Request Lenses
    startWorkflowExecution_childPolicy,
    startWorkflowExecution_executionStartToCloseTimeout,
    startWorkflowExecution_input,
    startWorkflowExecution_lambdaRole,
    startWorkflowExecution_tagList,
    startWorkflowExecution_taskList,
    startWorkflowExecution_taskPriority,
    startWorkflowExecution_taskStartToCloseTimeout,
    startWorkflowExecution_domain,
    startWorkflowExecution_workflowId,
    startWorkflowExecution_workflowType,

    -- * Destructuring the Response
    StartWorkflowExecutionResponse (..),
    newStartWorkflowExecutionResponse,

    -- * Response Lenses
    startWorkflowExecutionResponse_runId,
    startWorkflowExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newStartWorkflowExecution' smart constructor.
data StartWorkflowExecution = StartWorkflowExecution'
  { -- | If set, specifies the policy to use for the child workflow executions of
    -- this workflow execution if it is terminated, by calling the
    -- TerminateWorkflowExecution action explicitly or due to an expired
    -- timeout. This policy overrides the default child policy specified when
    -- registering the workflow type using RegisterWorkflowType.
    --
    -- The supported child policies are:
    --
    -- -   @TERMINATE@ – The child executions are terminated.
    --
    -- -   @REQUEST_CANCEL@ – A request to cancel is attempted for each child
    --     execution by recording a @WorkflowExecutionCancelRequested@ event in
    --     its history. It is up to the decider to take appropriate actions
    --     when it receives an execution history with this event.
    --
    -- -   @ABANDON@ – No action is taken. The child executions continue to
    --     run.
    --
    -- A child policy for this workflow execution must be specified either as a
    -- default for the workflow type or through this parameter. If neither this
    -- parameter is set nor a default child policy was specified at
    -- registration time then a fault is returned.
    childPolicy :: Prelude.Maybe ChildPolicy,
    -- | The total duration for this workflow execution. This overrides the
    -- defaultExecutionStartToCloseTimeout specified when registering the
    -- workflow type.
    --
    -- The duration is specified in seconds; an integer greater than or equal
    -- to @0@. Exceeding this limit causes the workflow execution to time out.
    -- Unlike some of the other timeout parameters in Amazon SWF, you cannot
    -- specify a value of \"NONE\" for this timeout; there is a one-year max
    -- limit on the time that a workflow execution can run.
    --
    -- An execution start-to-close timeout must be specified either through
    -- this parameter or as a default when the workflow type is registered. If
    -- neither this parameter nor a default execution start-to-close timeout is
    -- specified, a fault is returned.
    executionStartToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The input for the workflow execution. This is a free form string which
    -- should be meaningful to the workflow you are starting. This @input@ is
    -- made available to the new workflow execution in the
    -- @WorkflowExecutionStarted@ history event.
    input :: Prelude.Maybe Prelude.Text,
    -- | The IAM role to attach to this workflow execution.
    --
    -- Executions of this workflow type need IAM roles to invoke Lambda
    -- functions. If you don\'t attach an IAM role, any attempt to schedule a
    -- Lambda task fails. This results in a @ScheduleLambdaFunctionFailed@
    -- history event. For more information, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html>
    -- in the /Amazon SWF Developer Guide/.
    lambdaRole :: Prelude.Maybe Prelude.Text,
    -- | The list of tags to associate with the workflow execution. You can
    -- specify a maximum of 5 tags. You can list workflow executions with a
    -- specific tag by calling ListOpenWorkflowExecutions or
    -- ListClosedWorkflowExecutions and specifying a TagFilter.
    tagList :: Prelude.Maybe [Prelude.Text],
    -- | The task list to use for the decision tasks generated for this workflow
    -- execution. This overrides the @defaultTaskList@ specified when
    -- registering the workflow type.
    --
    -- A task list for this workflow execution must be specified either as a
    -- default for the workflow type or through this parameter. If neither this
    -- parameter is set nor a default task list was specified at registration
    -- time then a fault is returned.
    --
    -- The specified string must not start or end with whitespace. It must not
    -- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
    -- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
    -- /be/ the literal string @arn@.
    taskList :: Prelude.Maybe TaskList,
    -- | The task priority to use for this workflow execution. This overrides any
    -- default priority that was assigned when the workflow type was
    -- registered. If not set, then the default task priority for the workflow
    -- type is used. Valid values are integers that range from Java\'s
    -- @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647).
    -- Higher numbers indicate higher priority.
    --
    -- For more information about setting task priority, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
    -- in the /Amazon SWF Developer Guide/.
    taskPriority :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum duration of decision tasks for this workflow
    -- execution. This parameter overrides the @defaultTaskStartToCloseTimout@
    -- specified when registering the workflow type using RegisterWorkflowType.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    --
    -- A task start-to-close timeout for this workflow execution must be
    -- specified either as a default for the workflow type or through this
    -- parameter. If neither this parameter is set nor a default task
    -- start-to-close timeout was specified at registration time then a fault
    -- is returned.
    taskStartToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain in which the workflow execution is created.
    domain :: Prelude.Text,
    -- | The user defined identifier associated with the workflow execution. You
    -- can use this to associate a custom identifier with the workflow
    -- execution. You may specify the same identifier if a workflow execution
    -- is logically a /restart/ of a previous execution. You cannot have two
    -- open workflow executions with the same @workflowId@ at the same time
    -- within the same domain.
    --
    -- The specified string must not start or end with whitespace. It must not
    -- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
    -- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
    -- /be/ the literal string @arn@.
    workflowId :: Prelude.Text,
    -- | The type of the workflow to start.
    workflowType :: WorkflowType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartWorkflowExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'childPolicy', 'startWorkflowExecution_childPolicy' - If set, specifies the policy to use for the child workflow executions of
-- this workflow execution if it is terminated, by calling the
-- TerminateWorkflowExecution action explicitly or due to an expired
-- timeout. This policy overrides the default child policy specified when
-- registering the workflow type using RegisterWorkflowType.
--
-- The supported child policies are:
--
-- -   @TERMINATE@ – The child executions are terminated.
--
-- -   @REQUEST_CANCEL@ – A request to cancel is attempted for each child
--     execution by recording a @WorkflowExecutionCancelRequested@ event in
--     its history. It is up to the decider to take appropriate actions
--     when it receives an execution history with this event.
--
-- -   @ABANDON@ – No action is taken. The child executions continue to
--     run.
--
-- A child policy for this workflow execution must be specified either as a
-- default for the workflow type or through this parameter. If neither this
-- parameter is set nor a default child policy was specified at
-- registration time then a fault is returned.
--
-- 'executionStartToCloseTimeout', 'startWorkflowExecution_executionStartToCloseTimeout' - The total duration for this workflow execution. This overrides the
-- defaultExecutionStartToCloseTimeout specified when registering the
-- workflow type.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to @0@. Exceeding this limit causes the workflow execution to time out.
-- Unlike some of the other timeout parameters in Amazon SWF, you cannot
-- specify a value of \"NONE\" for this timeout; there is a one-year max
-- limit on the time that a workflow execution can run.
--
-- An execution start-to-close timeout must be specified either through
-- this parameter or as a default when the workflow type is registered. If
-- neither this parameter nor a default execution start-to-close timeout is
-- specified, a fault is returned.
--
-- 'input', 'startWorkflowExecution_input' - The input for the workflow execution. This is a free form string which
-- should be meaningful to the workflow you are starting. This @input@ is
-- made available to the new workflow execution in the
-- @WorkflowExecutionStarted@ history event.
--
-- 'lambdaRole', 'startWorkflowExecution_lambdaRole' - The IAM role to attach to this workflow execution.
--
-- Executions of this workflow type need IAM roles to invoke Lambda
-- functions. If you don\'t attach an IAM role, any attempt to schedule a
-- Lambda task fails. This results in a @ScheduleLambdaFunctionFailed@
-- history event. For more information, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html>
-- in the /Amazon SWF Developer Guide/.
--
-- 'tagList', 'startWorkflowExecution_tagList' - The list of tags to associate with the workflow execution. You can
-- specify a maximum of 5 tags. You can list workflow executions with a
-- specific tag by calling ListOpenWorkflowExecutions or
-- ListClosedWorkflowExecutions and specifying a TagFilter.
--
-- 'taskList', 'startWorkflowExecution_taskList' - The task list to use for the decision tasks generated for this workflow
-- execution. This overrides the @defaultTaskList@ specified when
-- registering the workflow type.
--
-- A task list for this workflow execution must be specified either as a
-- default for the workflow type or through this parameter. If neither this
-- parameter is set nor a default task list was specified at registration
-- time then a fault is returned.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
--
-- 'taskPriority', 'startWorkflowExecution_taskPriority' - The task priority to use for this workflow execution. This overrides any
-- default priority that was assigned when the workflow type was
-- registered. If not set, then the default task priority for the workflow
-- type is used. Valid values are integers that range from Java\'s
-- @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647).
-- Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
--
-- 'taskStartToCloseTimeout', 'startWorkflowExecution_taskStartToCloseTimeout' - Specifies the maximum duration of decision tasks for this workflow
-- execution. This parameter overrides the @defaultTaskStartToCloseTimout@
-- specified when registering the workflow type using RegisterWorkflowType.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- A task start-to-close timeout for this workflow execution must be
-- specified either as a default for the workflow type or through this
-- parameter. If neither this parameter is set nor a default task
-- start-to-close timeout was specified at registration time then a fault
-- is returned.
--
-- 'domain', 'startWorkflowExecution_domain' - The name of the domain in which the workflow execution is created.
--
-- 'workflowId', 'startWorkflowExecution_workflowId' - The user defined identifier associated with the workflow execution. You
-- can use this to associate a custom identifier with the workflow
-- execution. You may specify the same identifier if a workflow execution
-- is logically a /restart/ of a previous execution. You cannot have two
-- open workflow executions with the same @workflowId@ at the same time
-- within the same domain.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
--
-- 'workflowType', 'startWorkflowExecution_workflowType' - The type of the workflow to start.
newStartWorkflowExecution ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'workflowType'
  WorkflowType ->
  StartWorkflowExecution
newStartWorkflowExecution
  pDomain_
  pWorkflowId_
  pWorkflowType_ =
    StartWorkflowExecution'
      { childPolicy =
          Prelude.Nothing,
        executionStartToCloseTimeout = Prelude.Nothing,
        input = Prelude.Nothing,
        lambdaRole = Prelude.Nothing,
        tagList = Prelude.Nothing,
        taskList = Prelude.Nothing,
        taskPriority = Prelude.Nothing,
        taskStartToCloseTimeout = Prelude.Nothing,
        domain = pDomain_,
        workflowId = pWorkflowId_,
        workflowType = pWorkflowType_
      }

-- | If set, specifies the policy to use for the child workflow executions of
-- this workflow execution if it is terminated, by calling the
-- TerminateWorkflowExecution action explicitly or due to an expired
-- timeout. This policy overrides the default child policy specified when
-- registering the workflow type using RegisterWorkflowType.
--
-- The supported child policies are:
--
-- -   @TERMINATE@ – The child executions are terminated.
--
-- -   @REQUEST_CANCEL@ – A request to cancel is attempted for each child
--     execution by recording a @WorkflowExecutionCancelRequested@ event in
--     its history. It is up to the decider to take appropriate actions
--     when it receives an execution history with this event.
--
-- -   @ABANDON@ – No action is taken. The child executions continue to
--     run.
--
-- A child policy for this workflow execution must be specified either as a
-- default for the workflow type or through this parameter. If neither this
-- parameter is set nor a default child policy was specified at
-- registration time then a fault is returned.
startWorkflowExecution_childPolicy :: Lens.Lens' StartWorkflowExecution (Prelude.Maybe ChildPolicy)
startWorkflowExecution_childPolicy = Lens.lens (\StartWorkflowExecution' {childPolicy} -> childPolicy) (\s@StartWorkflowExecution' {} a -> s {childPolicy = a} :: StartWorkflowExecution)

-- | The total duration for this workflow execution. This overrides the
-- defaultExecutionStartToCloseTimeout specified when registering the
-- workflow type.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to @0@. Exceeding this limit causes the workflow execution to time out.
-- Unlike some of the other timeout parameters in Amazon SWF, you cannot
-- specify a value of \"NONE\" for this timeout; there is a one-year max
-- limit on the time that a workflow execution can run.
--
-- An execution start-to-close timeout must be specified either through
-- this parameter or as a default when the workflow type is registered. If
-- neither this parameter nor a default execution start-to-close timeout is
-- specified, a fault is returned.
startWorkflowExecution_executionStartToCloseTimeout :: Lens.Lens' StartWorkflowExecution (Prelude.Maybe Prelude.Text)
startWorkflowExecution_executionStartToCloseTimeout = Lens.lens (\StartWorkflowExecution' {executionStartToCloseTimeout} -> executionStartToCloseTimeout) (\s@StartWorkflowExecution' {} a -> s {executionStartToCloseTimeout = a} :: StartWorkflowExecution)

-- | The input for the workflow execution. This is a free form string which
-- should be meaningful to the workflow you are starting. This @input@ is
-- made available to the new workflow execution in the
-- @WorkflowExecutionStarted@ history event.
startWorkflowExecution_input :: Lens.Lens' StartWorkflowExecution (Prelude.Maybe Prelude.Text)
startWorkflowExecution_input = Lens.lens (\StartWorkflowExecution' {input} -> input) (\s@StartWorkflowExecution' {} a -> s {input = a} :: StartWorkflowExecution)

-- | The IAM role to attach to this workflow execution.
--
-- Executions of this workflow type need IAM roles to invoke Lambda
-- functions. If you don\'t attach an IAM role, any attempt to schedule a
-- Lambda task fails. This results in a @ScheduleLambdaFunctionFailed@
-- history event. For more information, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html>
-- in the /Amazon SWF Developer Guide/.
startWorkflowExecution_lambdaRole :: Lens.Lens' StartWorkflowExecution (Prelude.Maybe Prelude.Text)
startWorkflowExecution_lambdaRole = Lens.lens (\StartWorkflowExecution' {lambdaRole} -> lambdaRole) (\s@StartWorkflowExecution' {} a -> s {lambdaRole = a} :: StartWorkflowExecution)

-- | The list of tags to associate with the workflow execution. You can
-- specify a maximum of 5 tags. You can list workflow executions with a
-- specific tag by calling ListOpenWorkflowExecutions or
-- ListClosedWorkflowExecutions and specifying a TagFilter.
startWorkflowExecution_tagList :: Lens.Lens' StartWorkflowExecution (Prelude.Maybe [Prelude.Text])
startWorkflowExecution_tagList = Lens.lens (\StartWorkflowExecution' {tagList} -> tagList) (\s@StartWorkflowExecution' {} a -> s {tagList = a} :: StartWorkflowExecution) Prelude.. Lens.mapping Lens.coerced

-- | The task list to use for the decision tasks generated for this workflow
-- execution. This overrides the @defaultTaskList@ specified when
-- registering the workflow type.
--
-- A task list for this workflow execution must be specified either as a
-- default for the workflow type or through this parameter. If neither this
-- parameter is set nor a default task list was specified at registration
-- time then a fault is returned.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
startWorkflowExecution_taskList :: Lens.Lens' StartWorkflowExecution (Prelude.Maybe TaskList)
startWorkflowExecution_taskList = Lens.lens (\StartWorkflowExecution' {taskList} -> taskList) (\s@StartWorkflowExecution' {} a -> s {taskList = a} :: StartWorkflowExecution)

-- | The task priority to use for this workflow execution. This overrides any
-- default priority that was assigned when the workflow type was
-- registered. If not set, then the default task priority for the workflow
-- type is used. Valid values are integers that range from Java\'s
-- @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647).
-- Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
startWorkflowExecution_taskPriority :: Lens.Lens' StartWorkflowExecution (Prelude.Maybe Prelude.Text)
startWorkflowExecution_taskPriority = Lens.lens (\StartWorkflowExecution' {taskPriority} -> taskPriority) (\s@StartWorkflowExecution' {} a -> s {taskPriority = a} :: StartWorkflowExecution)

-- | Specifies the maximum duration of decision tasks for this workflow
-- execution. This parameter overrides the @defaultTaskStartToCloseTimout@
-- specified when registering the workflow type using RegisterWorkflowType.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- A task start-to-close timeout for this workflow execution must be
-- specified either as a default for the workflow type or through this
-- parameter. If neither this parameter is set nor a default task
-- start-to-close timeout was specified at registration time then a fault
-- is returned.
startWorkflowExecution_taskStartToCloseTimeout :: Lens.Lens' StartWorkflowExecution (Prelude.Maybe Prelude.Text)
startWorkflowExecution_taskStartToCloseTimeout = Lens.lens (\StartWorkflowExecution' {taskStartToCloseTimeout} -> taskStartToCloseTimeout) (\s@StartWorkflowExecution' {} a -> s {taskStartToCloseTimeout = a} :: StartWorkflowExecution)

-- | The name of the domain in which the workflow execution is created.
startWorkflowExecution_domain :: Lens.Lens' StartWorkflowExecution Prelude.Text
startWorkflowExecution_domain = Lens.lens (\StartWorkflowExecution' {domain} -> domain) (\s@StartWorkflowExecution' {} a -> s {domain = a} :: StartWorkflowExecution)

-- | The user defined identifier associated with the workflow execution. You
-- can use this to associate a custom identifier with the workflow
-- execution. You may specify the same identifier if a workflow execution
-- is logically a /restart/ of a previous execution. You cannot have two
-- open workflow executions with the same @workflowId@ at the same time
-- within the same domain.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
startWorkflowExecution_workflowId :: Lens.Lens' StartWorkflowExecution Prelude.Text
startWorkflowExecution_workflowId = Lens.lens (\StartWorkflowExecution' {workflowId} -> workflowId) (\s@StartWorkflowExecution' {} a -> s {workflowId = a} :: StartWorkflowExecution)

-- | The type of the workflow to start.
startWorkflowExecution_workflowType :: Lens.Lens' StartWorkflowExecution WorkflowType
startWorkflowExecution_workflowType = Lens.lens (\StartWorkflowExecution' {workflowType} -> workflowType) (\s@StartWorkflowExecution' {} a -> s {workflowType = a} :: StartWorkflowExecution)

instance Core.AWSRequest StartWorkflowExecution where
  type
    AWSResponse StartWorkflowExecution =
      StartWorkflowExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartWorkflowExecutionResponse'
            Prelude.<$> (x Data..?> "runId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartWorkflowExecution where
  hashWithSalt _salt StartWorkflowExecution' {..} =
    _salt `Prelude.hashWithSalt` childPolicy
      `Prelude.hashWithSalt` executionStartToCloseTimeout
      `Prelude.hashWithSalt` input
      `Prelude.hashWithSalt` lambdaRole
      `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` taskList
      `Prelude.hashWithSalt` taskPriority
      `Prelude.hashWithSalt` taskStartToCloseTimeout
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` workflowId
      `Prelude.hashWithSalt` workflowType

instance Prelude.NFData StartWorkflowExecution where
  rnf StartWorkflowExecution' {..} =
    Prelude.rnf childPolicy
      `Prelude.seq` Prelude.rnf executionStartToCloseTimeout
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf lambdaRole
      `Prelude.seq` Prelude.rnf tagList
      `Prelude.seq` Prelude.rnf taskList
      `Prelude.seq` Prelude.rnf taskPriority
      `Prelude.seq` Prelude.rnf taskStartToCloseTimeout
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf workflowType

instance Data.ToHeaders StartWorkflowExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SimpleWorkflowService.StartWorkflowExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartWorkflowExecution where
  toJSON StartWorkflowExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("childPolicy" Data..=) Prelude.<$> childPolicy,
            ("executionStartToCloseTimeout" Data..=)
              Prelude.<$> executionStartToCloseTimeout,
            ("input" Data..=) Prelude.<$> input,
            ("lambdaRole" Data..=) Prelude.<$> lambdaRole,
            ("tagList" Data..=) Prelude.<$> tagList,
            ("taskList" Data..=) Prelude.<$> taskList,
            ("taskPriority" Data..=) Prelude.<$> taskPriority,
            ("taskStartToCloseTimeout" Data..=)
              Prelude.<$> taskStartToCloseTimeout,
            Prelude.Just ("domain" Data..= domain),
            Prelude.Just ("workflowId" Data..= workflowId),
            Prelude.Just ("workflowType" Data..= workflowType)
          ]
      )

instance Data.ToPath StartWorkflowExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery StartWorkflowExecution where
  toQuery = Prelude.const Prelude.mempty

-- | Specifies the @runId@ of a workflow execution.
--
-- /See:/ 'newStartWorkflowExecutionResponse' smart constructor.
data StartWorkflowExecutionResponse = StartWorkflowExecutionResponse'
  { -- | The @runId@ of a workflow execution. This ID is generated by the service
    -- and can be used to uniquely identify the workflow execution within a
    -- domain.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartWorkflowExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'startWorkflowExecutionResponse_runId' - The @runId@ of a workflow execution. This ID is generated by the service
-- and can be used to uniquely identify the workflow execution within a
-- domain.
--
-- 'httpStatus', 'startWorkflowExecutionResponse_httpStatus' - The response's http status code.
newStartWorkflowExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartWorkflowExecutionResponse
newStartWorkflowExecutionResponse pHttpStatus_ =
  StartWorkflowExecutionResponse'
    { runId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @runId@ of a workflow execution. This ID is generated by the service
-- and can be used to uniquely identify the workflow execution within a
-- domain.
startWorkflowExecutionResponse_runId :: Lens.Lens' StartWorkflowExecutionResponse (Prelude.Maybe Prelude.Text)
startWorkflowExecutionResponse_runId = Lens.lens (\StartWorkflowExecutionResponse' {runId} -> runId) (\s@StartWorkflowExecutionResponse' {} a -> s {runId = a} :: StartWorkflowExecutionResponse)

-- | The response's http status code.
startWorkflowExecutionResponse_httpStatus :: Lens.Lens' StartWorkflowExecutionResponse Prelude.Int
startWorkflowExecutionResponse_httpStatus = Lens.lens (\StartWorkflowExecutionResponse' {httpStatus} -> httpStatus) (\s@StartWorkflowExecutionResponse' {} a -> s {httpStatus = a} :: StartWorkflowExecutionResponse)

instance
  Prelude.NFData
    StartWorkflowExecutionResponse
  where
  rnf StartWorkflowExecutionResponse' {..} =
    Prelude.rnf runId
      `Prelude.seq` Prelude.rnf httpStatus
