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
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionStartedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionStartedEventAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Provides details of @WorkflowExecutionStarted@ event.
--
-- /See:/ 'newWorkflowExecutionStartedEventAttributes' smart constructor.
data WorkflowExecutionStartedEventAttributes = WorkflowExecutionStartedEventAttributes'
  { -- | The input provided to the workflow execution.
    input :: Core.Maybe Core.Text,
    -- | If this workflow execution was started due to a
    -- @ContinueAsNewWorkflowExecution@ decision, then it contains the @runId@
    -- of the previous workflow execution that was closed and continued as this
    -- execution.
    continuedExecutionRunId :: Core.Maybe Core.Text,
    -- | The IAM role attached to the workflow execution.
    lambdaRole :: Core.Maybe Core.Text,
    -- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
    -- to the @StartChildWorkflowExecution@ Decision to start this workflow
    -- execution. The source event with this ID can be found in the history of
    -- the source workflow execution. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    parentInitiatedEventId :: Core.Maybe Core.Integer,
    -- | The priority of the decision tasks in the workflow execution.
    taskPriority :: Core.Maybe Core.Text,
    -- | The maximum duration for this workflow execution.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    executionStartToCloseTimeout :: Core.Maybe Core.Text,
    -- | The maximum duration of decision tasks for this workflow type.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    taskStartToCloseTimeout :: Core.Maybe Core.Text,
    -- | The list of tags associated with this workflow execution. An execution
    -- can have up to 5 tags.
    tagList :: Core.Maybe [Core.Text],
    -- | The source workflow execution that started this workflow execution. The
    -- member isn\'t set if the workflow execution was not started by a
    -- workflow.
    parentWorkflowExecution :: Core.Maybe WorkflowExecution,
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
    childPolicy :: ChildPolicy,
    -- | The name of the task list for scheduling the decision tasks for this
    -- workflow execution.
    taskList :: TaskList,
    -- | The workflow type of this execution.
    workflowType :: WorkflowType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'WorkflowExecutionStartedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'workflowExecutionStartedEventAttributes_input' - The input provided to the workflow execution.
--
-- 'continuedExecutionRunId', 'workflowExecutionStartedEventAttributes_continuedExecutionRunId' - If this workflow execution was started due to a
-- @ContinueAsNewWorkflowExecution@ decision, then it contains the @runId@
-- of the previous workflow execution that was closed and continued as this
-- execution.
--
-- 'lambdaRole', 'workflowExecutionStartedEventAttributes_lambdaRole' - The IAM role attached to the workflow execution.
--
-- 'parentInitiatedEventId', 'workflowExecutionStartedEventAttributes_parentInitiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this workflow
-- execution. The source event with this ID can be found in the history of
-- the source workflow execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
--
-- 'taskPriority', 'workflowExecutionStartedEventAttributes_taskPriority' - The priority of the decision tasks in the workflow execution.
--
-- 'executionStartToCloseTimeout', 'workflowExecutionStartedEventAttributes_executionStartToCloseTimeout' - The maximum duration for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'taskStartToCloseTimeout', 'workflowExecutionStartedEventAttributes_taskStartToCloseTimeout' - The maximum duration of decision tasks for this workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'tagList', 'workflowExecutionStartedEventAttributes_tagList' - The list of tags associated with this workflow execution. An execution
-- can have up to 5 tags.
--
-- 'parentWorkflowExecution', 'workflowExecutionStartedEventAttributes_parentWorkflowExecution' - The source workflow execution that started this workflow execution. The
-- member isn\'t set if the workflow execution was not started by a
-- workflow.
--
-- 'childPolicy', 'workflowExecutionStartedEventAttributes_childPolicy' - The policy to use for the child workflow executions if this workflow
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
--
-- 'taskList', 'workflowExecutionStartedEventAttributes_taskList' - The name of the task list for scheduling the decision tasks for this
-- workflow execution.
--
-- 'workflowType', 'workflowExecutionStartedEventAttributes_workflowType' - The workflow type of this execution.
newWorkflowExecutionStartedEventAttributes ::
  -- | 'childPolicy'
  ChildPolicy ->
  -- | 'taskList'
  TaskList ->
  -- | 'workflowType'
  WorkflowType ->
  WorkflowExecutionStartedEventAttributes
newWorkflowExecutionStartedEventAttributes
  pChildPolicy_
  pTaskList_
  pWorkflowType_ =
    WorkflowExecutionStartedEventAttributes'
      { input =
          Core.Nothing,
        continuedExecutionRunId =
          Core.Nothing,
        lambdaRole = Core.Nothing,
        parentInitiatedEventId =
          Core.Nothing,
        taskPriority = Core.Nothing,
        executionStartToCloseTimeout =
          Core.Nothing,
        taskStartToCloseTimeout =
          Core.Nothing,
        tagList = Core.Nothing,
        parentWorkflowExecution =
          Core.Nothing,
        childPolicy = pChildPolicy_,
        taskList = pTaskList_,
        workflowType = pWorkflowType_
      }

-- | The input provided to the workflow execution.
workflowExecutionStartedEventAttributes_input :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe Core.Text)
workflowExecutionStartedEventAttributes_input = Lens.lens (\WorkflowExecutionStartedEventAttributes' {input} -> input) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {input = a} :: WorkflowExecutionStartedEventAttributes)

-- | If this workflow execution was started due to a
-- @ContinueAsNewWorkflowExecution@ decision, then it contains the @runId@
-- of the previous workflow execution that was closed and continued as this
-- execution.
workflowExecutionStartedEventAttributes_continuedExecutionRunId :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe Core.Text)
workflowExecutionStartedEventAttributes_continuedExecutionRunId = Lens.lens (\WorkflowExecutionStartedEventAttributes' {continuedExecutionRunId} -> continuedExecutionRunId) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {continuedExecutionRunId = a} :: WorkflowExecutionStartedEventAttributes)

-- | The IAM role attached to the workflow execution.
workflowExecutionStartedEventAttributes_lambdaRole :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe Core.Text)
workflowExecutionStartedEventAttributes_lambdaRole = Lens.lens (\WorkflowExecutionStartedEventAttributes' {lambdaRole} -> lambdaRole) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {lambdaRole = a} :: WorkflowExecutionStartedEventAttributes)

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this workflow
-- execution. The source event with this ID can be found in the history of
-- the source workflow execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
workflowExecutionStartedEventAttributes_parentInitiatedEventId :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe Core.Integer)
workflowExecutionStartedEventAttributes_parentInitiatedEventId = Lens.lens (\WorkflowExecutionStartedEventAttributes' {parentInitiatedEventId} -> parentInitiatedEventId) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {parentInitiatedEventId = a} :: WorkflowExecutionStartedEventAttributes)

-- | The priority of the decision tasks in the workflow execution.
workflowExecutionStartedEventAttributes_taskPriority :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe Core.Text)
workflowExecutionStartedEventAttributes_taskPriority = Lens.lens (\WorkflowExecutionStartedEventAttributes' {taskPriority} -> taskPriority) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {taskPriority = a} :: WorkflowExecutionStartedEventAttributes)

-- | The maximum duration for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
workflowExecutionStartedEventAttributes_executionStartToCloseTimeout :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe Core.Text)
workflowExecutionStartedEventAttributes_executionStartToCloseTimeout = Lens.lens (\WorkflowExecutionStartedEventAttributes' {executionStartToCloseTimeout} -> executionStartToCloseTimeout) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {executionStartToCloseTimeout = a} :: WorkflowExecutionStartedEventAttributes)

-- | The maximum duration of decision tasks for this workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
workflowExecutionStartedEventAttributes_taskStartToCloseTimeout :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe Core.Text)
workflowExecutionStartedEventAttributes_taskStartToCloseTimeout = Lens.lens (\WorkflowExecutionStartedEventAttributes' {taskStartToCloseTimeout} -> taskStartToCloseTimeout) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {taskStartToCloseTimeout = a} :: WorkflowExecutionStartedEventAttributes)

-- | The list of tags associated with this workflow execution. An execution
-- can have up to 5 tags.
workflowExecutionStartedEventAttributes_tagList :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe [Core.Text])
workflowExecutionStartedEventAttributes_tagList = Lens.lens (\WorkflowExecutionStartedEventAttributes' {tagList} -> tagList) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {tagList = a} :: WorkflowExecutionStartedEventAttributes) Core.. Lens.mapping Lens._Coerce

-- | The source workflow execution that started this workflow execution. The
-- member isn\'t set if the workflow execution was not started by a
-- workflow.
workflowExecutionStartedEventAttributes_parentWorkflowExecution :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe WorkflowExecution)
workflowExecutionStartedEventAttributes_parentWorkflowExecution = Lens.lens (\WorkflowExecutionStartedEventAttributes' {parentWorkflowExecution} -> parentWorkflowExecution) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {parentWorkflowExecution = a} :: WorkflowExecutionStartedEventAttributes)

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
workflowExecutionStartedEventAttributes_childPolicy :: Lens.Lens' WorkflowExecutionStartedEventAttributes ChildPolicy
workflowExecutionStartedEventAttributes_childPolicy = Lens.lens (\WorkflowExecutionStartedEventAttributes' {childPolicy} -> childPolicy) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {childPolicy = a} :: WorkflowExecutionStartedEventAttributes)

-- | The name of the task list for scheduling the decision tasks for this
-- workflow execution.
workflowExecutionStartedEventAttributes_taskList :: Lens.Lens' WorkflowExecutionStartedEventAttributes TaskList
workflowExecutionStartedEventAttributes_taskList = Lens.lens (\WorkflowExecutionStartedEventAttributes' {taskList} -> taskList) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {taskList = a} :: WorkflowExecutionStartedEventAttributes)

-- | The workflow type of this execution.
workflowExecutionStartedEventAttributes_workflowType :: Lens.Lens' WorkflowExecutionStartedEventAttributes WorkflowType
workflowExecutionStartedEventAttributes_workflowType = Lens.lens (\WorkflowExecutionStartedEventAttributes' {workflowType} -> workflowType) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {workflowType = a} :: WorkflowExecutionStartedEventAttributes)

instance
  Core.FromJSON
    WorkflowExecutionStartedEventAttributes
  where
  parseJSON =
    Core.withObject
      "WorkflowExecutionStartedEventAttributes"
      ( \x ->
          WorkflowExecutionStartedEventAttributes'
            Core.<$> (x Core..:? "input")
            Core.<*> (x Core..:? "continuedExecutionRunId")
            Core.<*> (x Core..:? "lambdaRole")
            Core.<*> (x Core..:? "parentInitiatedEventId")
            Core.<*> (x Core..:? "taskPriority")
            Core.<*> (x Core..:? "executionStartToCloseTimeout")
            Core.<*> (x Core..:? "taskStartToCloseTimeout")
            Core.<*> (x Core..:? "tagList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "parentWorkflowExecution")
            Core.<*> (x Core..: "childPolicy")
            Core.<*> (x Core..: "taskList")
            Core.<*> (x Core..: "workflowType")
      )

instance
  Core.Hashable
    WorkflowExecutionStartedEventAttributes

instance
  Core.NFData
    WorkflowExecutionStartedEventAttributes
