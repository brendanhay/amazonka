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
-- Module      : Amazonka.SWF.Types.ContinueAsNewWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ContinueAsNewWorkflowExecutionDecisionAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.ChildPolicy
import Amazonka.SWF.Types.TaskList

-- | Provides the details of the @ContinueAsNewWorkflowExecution@ decision.
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
--     -   @tag@ – A tag used to identify the workflow execution
--
--     -   @taskList@ – String constraint. The key is @swf:taskList.name@.
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
-- /See:/ 'newContinueAsNewWorkflowExecutionDecisionAttributes' smart constructor.
data ContinueAsNewWorkflowExecutionDecisionAttributes = ContinueAsNewWorkflowExecutionDecisionAttributes'
  { -- | If set, specifies the policy to use for the child workflow executions of
    -- the new execution if it is terminated by calling the
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
    -- | If set, specifies the total duration for this workflow execution. This
    -- overrides the @defaultExecutionStartToCloseTimeout@ specified when
    -- registering the workflow type.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    --
    -- An execution start-to-close timeout for this workflow execution must be
    -- specified either as a default for the workflow type or through this
    -- field. If neither this field is set nor a default execution
    -- start-to-close timeout was specified at registration time then a fault
    -- is returned.
    executionStartToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The input provided to the new workflow execution.
    input :: Prelude.Maybe Prelude.Text,
    -- | The IAM role to attach to the new (continued) execution.
    lambdaRole :: Prelude.Maybe Prelude.Text,
    -- | The list of tags to associate with the new workflow execution. A maximum
    -- of 5 tags can be specified. You can list workflow executions with a
    -- specific tag by calling ListOpenWorkflowExecutions or
    -- ListClosedWorkflowExecutions and specifying a TagFilter.
    tagList :: Prelude.Maybe [Prelude.Text],
    -- | The task list to use for the decisions of the new (continued) workflow
    -- execution.
    taskList :: Prelude.Maybe TaskList,
    -- | The task priority that, if set, specifies the priority for the decision
    -- tasks for this workflow execution. This overrides the
    -- defaultTaskPriority specified when registering the workflow type. Valid
    -- values are integers that range from Java\'s @Integer.MIN_VALUE@
    -- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
    -- indicate higher priority.
    --
    -- For more information about setting task priority, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
    -- in the /Amazon SWF Developer Guide/.
    taskPriority :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum duration of decision tasks for the new workflow
    -- execution. This parameter overrides the @defaultTaskStartToCloseTimout@
    -- specified when registering the workflow type using RegisterWorkflowType.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    --
    -- A task start-to-close timeout for the new workflow execution must be
    -- specified either as a default for the workflow type or through this
    -- parameter. If neither this parameter is set nor a default task
    -- start-to-close timeout was specified at registration time then a fault
    -- is returned.
    taskStartToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The version of the workflow to start.
    workflowTypeVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContinueAsNewWorkflowExecutionDecisionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'childPolicy', 'continueAsNewWorkflowExecutionDecisionAttributes_childPolicy' - If set, specifies the policy to use for the child workflow executions of
-- the new execution if it is terminated by calling the
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
-- 'executionStartToCloseTimeout', 'continueAsNewWorkflowExecutionDecisionAttributes_executionStartToCloseTimeout' - If set, specifies the total duration for this workflow execution. This
-- overrides the @defaultExecutionStartToCloseTimeout@ specified when
-- registering the workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- An execution start-to-close timeout for this workflow execution must be
-- specified either as a default for the workflow type or through this
-- field. If neither this field is set nor a default execution
-- start-to-close timeout was specified at registration time then a fault
-- is returned.
--
-- 'input', 'continueAsNewWorkflowExecutionDecisionAttributes_input' - The input provided to the new workflow execution.
--
-- 'lambdaRole', 'continueAsNewWorkflowExecutionDecisionAttributes_lambdaRole' - The IAM role to attach to the new (continued) execution.
--
-- 'tagList', 'continueAsNewWorkflowExecutionDecisionAttributes_tagList' - The list of tags to associate with the new workflow execution. A maximum
-- of 5 tags can be specified. You can list workflow executions with a
-- specific tag by calling ListOpenWorkflowExecutions or
-- ListClosedWorkflowExecutions and specifying a TagFilter.
--
-- 'taskList', 'continueAsNewWorkflowExecutionDecisionAttributes_taskList' - The task list to use for the decisions of the new (continued) workflow
-- execution.
--
-- 'taskPriority', 'continueAsNewWorkflowExecutionDecisionAttributes_taskPriority' - The task priority that, if set, specifies the priority for the decision
-- tasks for this workflow execution. This overrides the
-- defaultTaskPriority specified when registering the workflow type. Valid
-- values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
--
-- 'taskStartToCloseTimeout', 'continueAsNewWorkflowExecutionDecisionAttributes_taskStartToCloseTimeout' - Specifies the maximum duration of decision tasks for the new workflow
-- execution. This parameter overrides the @defaultTaskStartToCloseTimout@
-- specified when registering the workflow type using RegisterWorkflowType.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- A task start-to-close timeout for the new workflow execution must be
-- specified either as a default for the workflow type or through this
-- parameter. If neither this parameter is set nor a default task
-- start-to-close timeout was specified at registration time then a fault
-- is returned.
--
-- 'workflowTypeVersion', 'continueAsNewWorkflowExecutionDecisionAttributes_workflowTypeVersion' - The version of the workflow to start.
newContinueAsNewWorkflowExecutionDecisionAttributes ::
  ContinueAsNewWorkflowExecutionDecisionAttributes
newContinueAsNewWorkflowExecutionDecisionAttributes =
  ContinueAsNewWorkflowExecutionDecisionAttributes'
    { childPolicy =
        Prelude.Nothing,
      executionStartToCloseTimeout =
        Prelude.Nothing,
      input = Prelude.Nothing,
      lambdaRole =
        Prelude.Nothing,
      tagList = Prelude.Nothing,
      taskList =
        Prelude.Nothing,
      taskPriority =
        Prelude.Nothing,
      taskStartToCloseTimeout =
        Prelude.Nothing,
      workflowTypeVersion =
        Prelude.Nothing
    }

-- | If set, specifies the policy to use for the child workflow executions of
-- the new execution if it is terminated by calling the
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
continueAsNewWorkflowExecutionDecisionAttributes_childPolicy :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Prelude.Maybe ChildPolicy)
continueAsNewWorkflowExecutionDecisionAttributes_childPolicy = Lens.lens (\ContinueAsNewWorkflowExecutionDecisionAttributes' {childPolicy} -> childPolicy) (\s@ContinueAsNewWorkflowExecutionDecisionAttributes' {} a -> s {childPolicy = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)

-- | If set, specifies the total duration for this workflow execution. This
-- overrides the @defaultExecutionStartToCloseTimeout@ specified when
-- registering the workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- An execution start-to-close timeout for this workflow execution must be
-- specified either as a default for the workflow type or through this
-- field. If neither this field is set nor a default execution
-- start-to-close timeout was specified at registration time then a fault
-- is returned.
continueAsNewWorkflowExecutionDecisionAttributes_executionStartToCloseTimeout :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
continueAsNewWorkflowExecutionDecisionAttributes_executionStartToCloseTimeout = Lens.lens (\ContinueAsNewWorkflowExecutionDecisionAttributes' {executionStartToCloseTimeout} -> executionStartToCloseTimeout) (\s@ContinueAsNewWorkflowExecutionDecisionAttributes' {} a -> s {executionStartToCloseTimeout = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)

-- | The input provided to the new workflow execution.
continueAsNewWorkflowExecutionDecisionAttributes_input :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
continueAsNewWorkflowExecutionDecisionAttributes_input = Lens.lens (\ContinueAsNewWorkflowExecutionDecisionAttributes' {input} -> input) (\s@ContinueAsNewWorkflowExecutionDecisionAttributes' {} a -> s {input = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)

-- | The IAM role to attach to the new (continued) execution.
continueAsNewWorkflowExecutionDecisionAttributes_lambdaRole :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
continueAsNewWorkflowExecutionDecisionAttributes_lambdaRole = Lens.lens (\ContinueAsNewWorkflowExecutionDecisionAttributes' {lambdaRole} -> lambdaRole) (\s@ContinueAsNewWorkflowExecutionDecisionAttributes' {} a -> s {lambdaRole = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)

-- | The list of tags to associate with the new workflow execution. A maximum
-- of 5 tags can be specified. You can list workflow executions with a
-- specific tag by calling ListOpenWorkflowExecutions or
-- ListClosedWorkflowExecutions and specifying a TagFilter.
continueAsNewWorkflowExecutionDecisionAttributes_tagList :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Prelude.Maybe [Prelude.Text])
continueAsNewWorkflowExecutionDecisionAttributes_tagList = Lens.lens (\ContinueAsNewWorkflowExecutionDecisionAttributes' {tagList} -> tagList) (\s@ContinueAsNewWorkflowExecutionDecisionAttributes' {} a -> s {tagList = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The task list to use for the decisions of the new (continued) workflow
-- execution.
continueAsNewWorkflowExecutionDecisionAttributes_taskList :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Prelude.Maybe TaskList)
continueAsNewWorkflowExecutionDecisionAttributes_taskList = Lens.lens (\ContinueAsNewWorkflowExecutionDecisionAttributes' {taskList} -> taskList) (\s@ContinueAsNewWorkflowExecutionDecisionAttributes' {} a -> s {taskList = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)

-- | The task priority that, if set, specifies the priority for the decision
-- tasks for this workflow execution. This overrides the
-- defaultTaskPriority specified when registering the workflow type. Valid
-- values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
continueAsNewWorkflowExecutionDecisionAttributes_taskPriority :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
continueAsNewWorkflowExecutionDecisionAttributes_taskPriority = Lens.lens (\ContinueAsNewWorkflowExecutionDecisionAttributes' {taskPriority} -> taskPriority) (\s@ContinueAsNewWorkflowExecutionDecisionAttributes' {} a -> s {taskPriority = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)

-- | Specifies the maximum duration of decision tasks for the new workflow
-- execution. This parameter overrides the @defaultTaskStartToCloseTimout@
-- specified when registering the workflow type using RegisterWorkflowType.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- A task start-to-close timeout for the new workflow execution must be
-- specified either as a default for the workflow type or through this
-- parameter. If neither this parameter is set nor a default task
-- start-to-close timeout was specified at registration time then a fault
-- is returned.
continueAsNewWorkflowExecutionDecisionAttributes_taskStartToCloseTimeout :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
continueAsNewWorkflowExecutionDecisionAttributes_taskStartToCloseTimeout = Lens.lens (\ContinueAsNewWorkflowExecutionDecisionAttributes' {taskStartToCloseTimeout} -> taskStartToCloseTimeout) (\s@ContinueAsNewWorkflowExecutionDecisionAttributes' {} a -> s {taskStartToCloseTimeout = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)

-- | The version of the workflow to start.
continueAsNewWorkflowExecutionDecisionAttributes_workflowTypeVersion :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
continueAsNewWorkflowExecutionDecisionAttributes_workflowTypeVersion = Lens.lens (\ContinueAsNewWorkflowExecutionDecisionAttributes' {workflowTypeVersion} -> workflowTypeVersion) (\s@ContinueAsNewWorkflowExecutionDecisionAttributes' {} a -> s {workflowTypeVersion = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)

instance
  Prelude.Hashable
    ContinueAsNewWorkflowExecutionDecisionAttributes
  where
  hashWithSalt
    _salt
    ContinueAsNewWorkflowExecutionDecisionAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` childPolicy
        `Prelude.hashWithSalt` executionStartToCloseTimeout
        `Prelude.hashWithSalt` input
        `Prelude.hashWithSalt` lambdaRole
        `Prelude.hashWithSalt` tagList
        `Prelude.hashWithSalt` taskList
        `Prelude.hashWithSalt` taskPriority
        `Prelude.hashWithSalt` taskStartToCloseTimeout
        `Prelude.hashWithSalt` workflowTypeVersion

instance
  Prelude.NFData
    ContinueAsNewWorkflowExecutionDecisionAttributes
  where
  rnf
    ContinueAsNewWorkflowExecutionDecisionAttributes' {..} =
      Prelude.rnf childPolicy
        `Prelude.seq` Prelude.rnf executionStartToCloseTimeout
        `Prelude.seq` Prelude.rnf input
        `Prelude.seq` Prelude.rnf lambdaRole
        `Prelude.seq` Prelude.rnf tagList
        `Prelude.seq` Prelude.rnf taskList
        `Prelude.seq` Prelude.rnf taskPriority
        `Prelude.seq` Prelude.rnf taskStartToCloseTimeout
        `Prelude.seq` Prelude.rnf workflowTypeVersion

instance
  Data.ToJSON
    ContinueAsNewWorkflowExecutionDecisionAttributes
  where
  toJSON
    ContinueAsNewWorkflowExecutionDecisionAttributes' {..} =
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
              ("workflowTypeVersion" Data..=)
                Prelude.<$> workflowTypeVersion
            ]
        )
