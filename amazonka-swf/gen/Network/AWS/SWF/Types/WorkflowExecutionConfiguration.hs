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
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList

-- | The configuration settings for a workflow execution including timeout
-- values, tasklist etc. These configuration settings are determined from
-- the defaults specified when registering the workflow type and those
-- specified when starting the workflow execution.
--
-- /See:/ 'newWorkflowExecutionConfiguration' smart constructor.
data WorkflowExecutionConfiguration = WorkflowExecutionConfiguration'
  { -- | The IAM role attached to the child workflow execution.
    lambdaRole :: Core.Maybe Core.Text,
    -- | The priority assigned to decision tasks for this workflow execution.
    -- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
    -- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
    -- indicate higher priority.
    --
    -- For more information about setting task priority, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
    -- in the /Amazon SWF Developer Guide/.
    taskPriority :: Core.Maybe Core.Text,
    -- | The maximum duration allowed for decision tasks for this workflow
    -- execution.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    taskStartToCloseTimeout :: Core.Text,
    -- | The total duration for this workflow execution.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    executionStartToCloseTimeout :: Core.Text,
    -- | The task list used for the decision tasks generated for this workflow
    -- execution.
    taskList :: TaskList,
    -- | The policy to use for the child workflow executions if this workflow
    -- execution is terminated, by calling the TerminateWorkflowExecution
    -- action explicitly or due to an expired timeout.
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
    childPolicy :: ChildPolicy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'WorkflowExecutionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaRole', 'workflowExecutionConfiguration_lambdaRole' - The IAM role attached to the child workflow execution.
--
-- 'taskPriority', 'workflowExecutionConfiguration_taskPriority' - The priority assigned to decision tasks for this workflow execution.
-- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
--
-- 'taskStartToCloseTimeout', 'workflowExecutionConfiguration_taskStartToCloseTimeout' - The maximum duration allowed for decision tasks for this workflow
-- execution.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'executionStartToCloseTimeout', 'workflowExecutionConfiguration_executionStartToCloseTimeout' - The total duration for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'taskList', 'workflowExecutionConfiguration_taskList' - The task list used for the decision tasks generated for this workflow
-- execution.
--
-- 'childPolicy', 'workflowExecutionConfiguration_childPolicy' - The policy to use for the child workflow executions if this workflow
-- execution is terminated, by calling the TerminateWorkflowExecution
-- action explicitly or due to an expired timeout.
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
newWorkflowExecutionConfiguration ::
  -- | 'taskStartToCloseTimeout'
  Core.Text ->
  -- | 'executionStartToCloseTimeout'
  Core.Text ->
  -- | 'taskList'
  TaskList ->
  -- | 'childPolicy'
  ChildPolicy ->
  WorkflowExecutionConfiguration
newWorkflowExecutionConfiguration
  pTaskStartToCloseTimeout_
  pExecutionStartToCloseTimeout_
  pTaskList_
  pChildPolicy_ =
    WorkflowExecutionConfiguration'
      { lambdaRole =
          Core.Nothing,
        taskPriority = Core.Nothing,
        taskStartToCloseTimeout =
          pTaskStartToCloseTimeout_,
        executionStartToCloseTimeout =
          pExecutionStartToCloseTimeout_,
        taskList = pTaskList_,
        childPolicy = pChildPolicy_
      }

-- | The IAM role attached to the child workflow execution.
workflowExecutionConfiguration_lambdaRole :: Lens.Lens' WorkflowExecutionConfiguration (Core.Maybe Core.Text)
workflowExecutionConfiguration_lambdaRole = Lens.lens (\WorkflowExecutionConfiguration' {lambdaRole} -> lambdaRole) (\s@WorkflowExecutionConfiguration' {} a -> s {lambdaRole = a} :: WorkflowExecutionConfiguration)

-- | The priority assigned to decision tasks for this workflow execution.
-- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
workflowExecutionConfiguration_taskPriority :: Lens.Lens' WorkflowExecutionConfiguration (Core.Maybe Core.Text)
workflowExecutionConfiguration_taskPriority = Lens.lens (\WorkflowExecutionConfiguration' {taskPriority} -> taskPriority) (\s@WorkflowExecutionConfiguration' {} a -> s {taskPriority = a} :: WorkflowExecutionConfiguration)

-- | The maximum duration allowed for decision tasks for this workflow
-- execution.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
workflowExecutionConfiguration_taskStartToCloseTimeout :: Lens.Lens' WorkflowExecutionConfiguration Core.Text
workflowExecutionConfiguration_taskStartToCloseTimeout = Lens.lens (\WorkflowExecutionConfiguration' {taskStartToCloseTimeout} -> taskStartToCloseTimeout) (\s@WorkflowExecutionConfiguration' {} a -> s {taskStartToCloseTimeout = a} :: WorkflowExecutionConfiguration)

-- | The total duration for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
workflowExecutionConfiguration_executionStartToCloseTimeout :: Lens.Lens' WorkflowExecutionConfiguration Core.Text
workflowExecutionConfiguration_executionStartToCloseTimeout = Lens.lens (\WorkflowExecutionConfiguration' {executionStartToCloseTimeout} -> executionStartToCloseTimeout) (\s@WorkflowExecutionConfiguration' {} a -> s {executionStartToCloseTimeout = a} :: WorkflowExecutionConfiguration)

-- | The task list used for the decision tasks generated for this workflow
-- execution.
workflowExecutionConfiguration_taskList :: Lens.Lens' WorkflowExecutionConfiguration TaskList
workflowExecutionConfiguration_taskList = Lens.lens (\WorkflowExecutionConfiguration' {taskList} -> taskList) (\s@WorkflowExecutionConfiguration' {} a -> s {taskList = a} :: WorkflowExecutionConfiguration)

-- | The policy to use for the child workflow executions if this workflow
-- execution is terminated, by calling the TerminateWorkflowExecution
-- action explicitly or due to an expired timeout.
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
workflowExecutionConfiguration_childPolicy :: Lens.Lens' WorkflowExecutionConfiguration ChildPolicy
workflowExecutionConfiguration_childPolicy = Lens.lens (\WorkflowExecutionConfiguration' {childPolicy} -> childPolicy) (\s@WorkflowExecutionConfiguration' {} a -> s {childPolicy = a} :: WorkflowExecutionConfiguration)

instance Core.FromJSON WorkflowExecutionConfiguration where
  parseJSON =
    Core.withObject
      "WorkflowExecutionConfiguration"
      ( \x ->
          WorkflowExecutionConfiguration'
            Core.<$> (x Core..:? "lambdaRole")
            Core.<*> (x Core..:? "taskPriority")
            Core.<*> (x Core..: "taskStartToCloseTimeout")
            Core.<*> (x Core..: "executionStartToCloseTimeout")
            Core.<*> (x Core..: "taskList")
            Core.<*> (x Core..: "childPolicy")
      )

instance Core.Hashable WorkflowExecutionConfiguration

instance Core.NFData WorkflowExecutionConfiguration
