{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartChildWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartChildWorkflowExecutionDecisionAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @StartChildWorkflowExecution@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
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
--     -   @tagList.member.N@ – The key is \"swf:tagList.N\" where N is the
--         tag number from 0 to 4, inclusive.
--
--     -   @taskList@ – String constraint. The key is @swf:taskList.name@.
--
--     -   @workflowType.name@ – String constraint. The key is
--         @swf:workflowType.name@.
--
--     -   @workflowType.version@ – String constraint. The key is
--         @swf:workflowType.version@.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
--
-- /See:/ 'newStartChildWorkflowExecutionDecisionAttributes' smart constructor.
data StartChildWorkflowExecutionDecisionAttributes = StartChildWorkflowExecutionDecisionAttributes'
  { -- | The input to be provided to the workflow execution.
    input :: Prelude.Maybe Prelude.Text,
    -- | The IAM role attached to the child workflow execution.
    lambdaRole :: Prelude.Maybe Prelude.Text,
    -- | If set, specifies the policy to use for the child workflow executions if
    -- the workflow execution being started is terminated by calling the
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
    -- | The name of the task list to be used for decision tasks of the child
    -- workflow execution.
    --
    -- A task list for this workflow execution must be specified either as a
    -- default for the workflow type or through this parameter. If neither this
    -- parameter is set nor a default task list was specified at registration
    -- time then a fault is returned.
    --
    -- The specified string must not start or end with whitespace. It must not
    -- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
    -- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
    -- contain the literal string @arn@.
    taskList :: Prelude.Maybe TaskList,
    -- | A task priority that, if set, specifies the priority for a decision task
    -- of this workflow execution. This overrides the defaultTaskPriority
    -- specified when registering the workflow type. Valid values are integers
    -- that range from Java\'s @Integer.MIN_VALUE@ (-2147483648) to
    -- @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher
    -- priority.
    --
    -- For more information about setting task priority, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
    -- in the /Amazon SWF Developer Guide/.
    taskPriority :: Prelude.Maybe Prelude.Text,
    -- | The data attached to the event that can be used by the decider in
    -- subsequent workflow tasks. This data isn\'t sent to the child workflow
    -- execution.
    control :: Prelude.Maybe Prelude.Text,
    -- | The total duration for this workflow execution. This overrides the
    -- defaultExecutionStartToCloseTimeout specified when registering the
    -- workflow type.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    --
    -- An execution start-to-close timeout for this workflow execution must be
    -- specified either as a default for the workflow type or through this
    -- parameter. If neither this parameter is set nor a default execution
    -- start-to-close timeout was specified at registration time then a fault
    -- is returned.
    executionStartToCloseTimeout :: Prelude.Maybe Prelude.Text,
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
    -- | The list of tags to associate with the child workflow execution. A
    -- maximum of 5 tags can be specified. You can list workflow executions
    -- with a specific tag by calling ListOpenWorkflowExecutions or
    -- ListClosedWorkflowExecutions and specifying a TagFilter.
    tagList :: Prelude.Maybe [Prelude.Text],
    -- | The type of the workflow execution to be started.
    workflowType :: WorkflowType,
    -- | The @workflowId@ of the workflow execution.
    --
    -- The specified string must not start or end with whitespace. It must not
    -- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
    -- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
    -- contain the literal string @arn@.
    workflowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartChildWorkflowExecutionDecisionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'startChildWorkflowExecutionDecisionAttributes_input' - The input to be provided to the workflow execution.
--
-- 'lambdaRole', 'startChildWorkflowExecutionDecisionAttributes_lambdaRole' - The IAM role attached to the child workflow execution.
--
-- 'childPolicy', 'startChildWorkflowExecutionDecisionAttributes_childPolicy' - If set, specifies the policy to use for the child workflow executions if
-- the workflow execution being started is terminated by calling the
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
-- 'taskList', 'startChildWorkflowExecutionDecisionAttributes_taskList' - The name of the task list to be used for decision tasks of the child
-- workflow execution.
--
-- A task list for this workflow execution must be specified either as a
-- default for the workflow type or through this parameter. If neither this
-- parameter is set nor a default task list was specified at registration
-- time then a fault is returned.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- contain the literal string @arn@.
--
-- 'taskPriority', 'startChildWorkflowExecutionDecisionAttributes_taskPriority' - A task priority that, if set, specifies the priority for a decision task
-- of this workflow execution. This overrides the defaultTaskPriority
-- specified when registering the workflow type. Valid values are integers
-- that range from Java\'s @Integer.MIN_VALUE@ (-2147483648) to
-- @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher
-- priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
--
-- 'control', 'startChildWorkflowExecutionDecisionAttributes_control' - The data attached to the event that can be used by the decider in
-- subsequent workflow tasks. This data isn\'t sent to the child workflow
-- execution.
--
-- 'executionStartToCloseTimeout', 'startChildWorkflowExecutionDecisionAttributes_executionStartToCloseTimeout' - The total duration for this workflow execution. This overrides the
-- defaultExecutionStartToCloseTimeout specified when registering the
-- workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- An execution start-to-close timeout for this workflow execution must be
-- specified either as a default for the workflow type or through this
-- parameter. If neither this parameter is set nor a default execution
-- start-to-close timeout was specified at registration time then a fault
-- is returned.
--
-- 'taskStartToCloseTimeout', 'startChildWorkflowExecutionDecisionAttributes_taskStartToCloseTimeout' - Specifies the maximum duration of decision tasks for this workflow
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
-- 'tagList', 'startChildWorkflowExecutionDecisionAttributes_tagList' - The list of tags to associate with the child workflow execution. A
-- maximum of 5 tags can be specified. You can list workflow executions
-- with a specific tag by calling ListOpenWorkflowExecutions or
-- ListClosedWorkflowExecutions and specifying a TagFilter.
--
-- 'workflowType', 'startChildWorkflowExecutionDecisionAttributes_workflowType' - The type of the workflow execution to be started.
--
-- 'workflowId', 'startChildWorkflowExecutionDecisionAttributes_workflowId' - The @workflowId@ of the workflow execution.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- contain the literal string @arn@.
newStartChildWorkflowExecutionDecisionAttributes ::
  -- | 'workflowType'
  WorkflowType ->
  -- | 'workflowId'
  Prelude.Text ->
  StartChildWorkflowExecutionDecisionAttributes
newStartChildWorkflowExecutionDecisionAttributes
  pWorkflowType_
  pWorkflowId_ =
    StartChildWorkflowExecutionDecisionAttributes'
      { input =
          Prelude.Nothing,
        lambdaRole = Prelude.Nothing,
        childPolicy =
          Prelude.Nothing,
        taskList = Prelude.Nothing,
        taskPriority =
          Prelude.Nothing,
        control = Prelude.Nothing,
        executionStartToCloseTimeout =
          Prelude.Nothing,
        taskStartToCloseTimeout =
          Prelude.Nothing,
        tagList = Prelude.Nothing,
        workflowType =
          pWorkflowType_,
        workflowId = pWorkflowId_
      }

-- | The input to be provided to the workflow execution.
startChildWorkflowExecutionDecisionAttributes_input :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
startChildWorkflowExecutionDecisionAttributes_input = Lens.lens (\StartChildWorkflowExecutionDecisionAttributes' {input} -> input) (\s@StartChildWorkflowExecutionDecisionAttributes' {} a -> s {input = a} :: StartChildWorkflowExecutionDecisionAttributes)

-- | The IAM role attached to the child workflow execution.
startChildWorkflowExecutionDecisionAttributes_lambdaRole :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
startChildWorkflowExecutionDecisionAttributes_lambdaRole = Lens.lens (\StartChildWorkflowExecutionDecisionAttributes' {lambdaRole} -> lambdaRole) (\s@StartChildWorkflowExecutionDecisionAttributes' {} a -> s {lambdaRole = a} :: StartChildWorkflowExecutionDecisionAttributes)

-- | If set, specifies the policy to use for the child workflow executions if
-- the workflow execution being started is terminated by calling the
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
startChildWorkflowExecutionDecisionAttributes_childPolicy :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Prelude.Maybe ChildPolicy)
startChildWorkflowExecutionDecisionAttributes_childPolicy = Lens.lens (\StartChildWorkflowExecutionDecisionAttributes' {childPolicy} -> childPolicy) (\s@StartChildWorkflowExecutionDecisionAttributes' {} a -> s {childPolicy = a} :: StartChildWorkflowExecutionDecisionAttributes)

-- | The name of the task list to be used for decision tasks of the child
-- workflow execution.
--
-- A task list for this workflow execution must be specified either as a
-- default for the workflow type or through this parameter. If neither this
-- parameter is set nor a default task list was specified at registration
-- time then a fault is returned.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- contain the literal string @arn@.
startChildWorkflowExecutionDecisionAttributes_taskList :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Prelude.Maybe TaskList)
startChildWorkflowExecutionDecisionAttributes_taskList = Lens.lens (\StartChildWorkflowExecutionDecisionAttributes' {taskList} -> taskList) (\s@StartChildWorkflowExecutionDecisionAttributes' {} a -> s {taskList = a} :: StartChildWorkflowExecutionDecisionAttributes)

-- | A task priority that, if set, specifies the priority for a decision task
-- of this workflow execution. This overrides the defaultTaskPriority
-- specified when registering the workflow type. Valid values are integers
-- that range from Java\'s @Integer.MIN_VALUE@ (-2147483648) to
-- @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher
-- priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
startChildWorkflowExecutionDecisionAttributes_taskPriority :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
startChildWorkflowExecutionDecisionAttributes_taskPriority = Lens.lens (\StartChildWorkflowExecutionDecisionAttributes' {taskPriority} -> taskPriority) (\s@StartChildWorkflowExecutionDecisionAttributes' {} a -> s {taskPriority = a} :: StartChildWorkflowExecutionDecisionAttributes)

-- | The data attached to the event that can be used by the decider in
-- subsequent workflow tasks. This data isn\'t sent to the child workflow
-- execution.
startChildWorkflowExecutionDecisionAttributes_control :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
startChildWorkflowExecutionDecisionAttributes_control = Lens.lens (\StartChildWorkflowExecutionDecisionAttributes' {control} -> control) (\s@StartChildWorkflowExecutionDecisionAttributes' {} a -> s {control = a} :: StartChildWorkflowExecutionDecisionAttributes)

-- | The total duration for this workflow execution. This overrides the
-- defaultExecutionStartToCloseTimeout specified when registering the
-- workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- An execution start-to-close timeout for this workflow execution must be
-- specified either as a default for the workflow type or through this
-- parameter. If neither this parameter is set nor a default execution
-- start-to-close timeout was specified at registration time then a fault
-- is returned.
startChildWorkflowExecutionDecisionAttributes_executionStartToCloseTimeout :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
startChildWorkflowExecutionDecisionAttributes_executionStartToCloseTimeout = Lens.lens (\StartChildWorkflowExecutionDecisionAttributes' {executionStartToCloseTimeout} -> executionStartToCloseTimeout) (\s@StartChildWorkflowExecutionDecisionAttributes' {} a -> s {executionStartToCloseTimeout = a} :: StartChildWorkflowExecutionDecisionAttributes)

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
startChildWorkflowExecutionDecisionAttributes_taskStartToCloseTimeout :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
startChildWorkflowExecutionDecisionAttributes_taskStartToCloseTimeout = Lens.lens (\StartChildWorkflowExecutionDecisionAttributes' {taskStartToCloseTimeout} -> taskStartToCloseTimeout) (\s@StartChildWorkflowExecutionDecisionAttributes' {} a -> s {taskStartToCloseTimeout = a} :: StartChildWorkflowExecutionDecisionAttributes)

-- | The list of tags to associate with the child workflow execution. A
-- maximum of 5 tags can be specified. You can list workflow executions
-- with a specific tag by calling ListOpenWorkflowExecutions or
-- ListClosedWorkflowExecutions and specifying a TagFilter.
startChildWorkflowExecutionDecisionAttributes_tagList :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Prelude.Maybe [Prelude.Text])
startChildWorkflowExecutionDecisionAttributes_tagList = Lens.lens (\StartChildWorkflowExecutionDecisionAttributes' {tagList} -> tagList) (\s@StartChildWorkflowExecutionDecisionAttributes' {} a -> s {tagList = a} :: StartChildWorkflowExecutionDecisionAttributes) Prelude.. Lens.mapping Prelude._Coerce

-- | The type of the workflow execution to be started.
startChildWorkflowExecutionDecisionAttributes_workflowType :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes WorkflowType
startChildWorkflowExecutionDecisionAttributes_workflowType = Lens.lens (\StartChildWorkflowExecutionDecisionAttributes' {workflowType} -> workflowType) (\s@StartChildWorkflowExecutionDecisionAttributes' {} a -> s {workflowType = a} :: StartChildWorkflowExecutionDecisionAttributes)

-- | The @workflowId@ of the workflow execution.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- contain the literal string @arn@.
startChildWorkflowExecutionDecisionAttributes_workflowId :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes Prelude.Text
startChildWorkflowExecutionDecisionAttributes_workflowId = Lens.lens (\StartChildWorkflowExecutionDecisionAttributes' {workflowId} -> workflowId) (\s@StartChildWorkflowExecutionDecisionAttributes' {} a -> s {workflowId = a} :: StartChildWorkflowExecutionDecisionAttributes)

instance
  Prelude.Hashable
    StartChildWorkflowExecutionDecisionAttributes

instance
  Prelude.NFData
    StartChildWorkflowExecutionDecisionAttributes

instance
  Prelude.ToJSON
    StartChildWorkflowExecutionDecisionAttributes
  where
  toJSON
    StartChildWorkflowExecutionDecisionAttributes' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ ("input" Prelude..=) Prelude.<$> input,
              ("lambdaRole" Prelude..=) Prelude.<$> lambdaRole,
              ("childPolicy" Prelude..=) Prelude.<$> childPolicy,
              ("taskList" Prelude..=) Prelude.<$> taskList,
              ("taskPriority" Prelude..=) Prelude.<$> taskPriority,
              ("control" Prelude..=) Prelude.<$> control,
              ("executionStartToCloseTimeout" Prelude..=)
                Prelude.<$> executionStartToCloseTimeout,
              ("taskStartToCloseTimeout" Prelude..=)
                Prelude.<$> taskStartToCloseTimeout,
              ("tagList" Prelude..=) Prelude.<$> tagList,
              Prelude.Just
                ("workflowType" Prelude..= workflowType),
              Prelude.Just ("workflowId" Prelude..= workflowId)
            ]
        )
