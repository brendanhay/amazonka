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
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionContinuedAsNewEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionContinuedAsNewEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @WorkflowExecutionContinuedAsNew@ event.
--
-- /See:/ 'newWorkflowExecutionContinuedAsNewEventAttributes' smart constructor.
data WorkflowExecutionContinuedAsNewEventAttributes = WorkflowExecutionContinuedAsNewEventAttributes'
  { -- | The input provided to the new workflow execution.
    input :: Prelude.Maybe Prelude.Text,
    -- | The IAM role to attach to the new (continued) workflow execution.
    lambdaRole :: Prelude.Maybe Prelude.Text,
    -- | The priority of the task to use for the decisions of the new (continued)
    -- workflow execution.
    taskPriority :: Prelude.Maybe Prelude.Text,
    -- | The total duration allowed for the new workflow execution.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    executionStartToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The maximum duration of decision tasks for the new workflow execution.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    taskStartToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The list of tags associated with the new workflow execution.
    tagList :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision task that resulted in the @ContinueAsNewWorkflowExecution@
    -- decision that started this execution. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    decisionTaskCompletedEventId :: Prelude.Integer,
    -- | The @runId@ of the new workflow execution.
    newExecutionRunId' :: Prelude.Text,
    -- | The task list to use for the decisions of the new (continued) workflow
    -- execution.
    taskList :: TaskList,
    -- | The policy to use for the child workflow executions of the new execution
    -- if it is terminated by calling the TerminateWorkflowExecution action
    -- explicitly or due to an expired timeout.
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
    -- | The workflow type of this execution.
    workflowType :: WorkflowType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionContinuedAsNewEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'workflowExecutionContinuedAsNewEventAttributes_input' - The input provided to the new workflow execution.
--
-- 'lambdaRole', 'workflowExecutionContinuedAsNewEventAttributes_lambdaRole' - The IAM role to attach to the new (continued) workflow execution.
--
-- 'taskPriority', 'workflowExecutionContinuedAsNewEventAttributes_taskPriority' - The priority of the task to use for the decisions of the new (continued)
-- workflow execution.
--
-- 'executionStartToCloseTimeout', 'workflowExecutionContinuedAsNewEventAttributes_executionStartToCloseTimeout' - The total duration allowed for the new workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'taskStartToCloseTimeout', 'workflowExecutionContinuedAsNewEventAttributes_taskStartToCloseTimeout' - The maximum duration of decision tasks for the new workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'tagList', 'workflowExecutionContinuedAsNewEventAttributes_tagList' - The list of tags associated with the new workflow execution.
--
-- 'decisionTaskCompletedEventId', 'workflowExecutionContinuedAsNewEventAttributes_decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @ContinueAsNewWorkflowExecution@
-- decision that started this execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
--
-- 'newExecutionRunId'', 'workflowExecutionContinuedAsNewEventAttributes_newExecutionRunId' - The @runId@ of the new workflow execution.
--
-- 'taskList', 'workflowExecutionContinuedAsNewEventAttributes_taskList' - The task list to use for the decisions of the new (continued) workflow
-- execution.
--
-- 'childPolicy', 'workflowExecutionContinuedAsNewEventAttributes_childPolicy' - The policy to use for the child workflow executions of the new execution
-- if it is terminated by calling the TerminateWorkflowExecution action
-- explicitly or due to an expired timeout.
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
-- 'workflowType', 'workflowExecutionContinuedAsNewEventAttributes_workflowType' - The workflow type of this execution.
newWorkflowExecutionContinuedAsNewEventAttributes ::
  -- | 'decisionTaskCompletedEventId'
  Prelude.Integer ->
  -- | 'newExecutionRunId''
  Prelude.Text ->
  -- | 'taskList'
  TaskList ->
  -- | 'childPolicy'
  ChildPolicy ->
  -- | 'workflowType'
  WorkflowType ->
  WorkflowExecutionContinuedAsNewEventAttributes
newWorkflowExecutionContinuedAsNewEventAttributes
  pDecisionTaskCompletedEventId_
  pNewExecutionRunId_
  pTaskList_
  pChildPolicy_
  pWorkflowType_ =
    WorkflowExecutionContinuedAsNewEventAttributes'
      { input =
          Prelude.Nothing,
        lambdaRole =
          Prelude.Nothing,
        taskPriority =
          Prelude.Nothing,
        executionStartToCloseTimeout =
          Prelude.Nothing,
        taskStartToCloseTimeout =
          Prelude.Nothing,
        tagList = Prelude.Nothing,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_,
        newExecutionRunId' =
          pNewExecutionRunId_,
        taskList = pTaskList_,
        childPolicy = pChildPolicy_,
        workflowType =
          pWorkflowType_
      }

-- | The input provided to the new workflow execution.
workflowExecutionContinuedAsNewEventAttributes_input :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionContinuedAsNewEventAttributes_input = Lens.lens (\WorkflowExecutionContinuedAsNewEventAttributes' {input} -> input) (\s@WorkflowExecutionContinuedAsNewEventAttributes' {} a -> s {input = a} :: WorkflowExecutionContinuedAsNewEventAttributes)

-- | The IAM role to attach to the new (continued) workflow execution.
workflowExecutionContinuedAsNewEventAttributes_lambdaRole :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionContinuedAsNewEventAttributes_lambdaRole = Lens.lens (\WorkflowExecutionContinuedAsNewEventAttributes' {lambdaRole} -> lambdaRole) (\s@WorkflowExecutionContinuedAsNewEventAttributes' {} a -> s {lambdaRole = a} :: WorkflowExecutionContinuedAsNewEventAttributes)

-- | The priority of the task to use for the decisions of the new (continued)
-- workflow execution.
workflowExecutionContinuedAsNewEventAttributes_taskPriority :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionContinuedAsNewEventAttributes_taskPriority = Lens.lens (\WorkflowExecutionContinuedAsNewEventAttributes' {taskPriority} -> taskPriority) (\s@WorkflowExecutionContinuedAsNewEventAttributes' {} a -> s {taskPriority = a} :: WorkflowExecutionContinuedAsNewEventAttributes)

-- | The total duration allowed for the new workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
workflowExecutionContinuedAsNewEventAttributes_executionStartToCloseTimeout :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionContinuedAsNewEventAttributes_executionStartToCloseTimeout = Lens.lens (\WorkflowExecutionContinuedAsNewEventAttributes' {executionStartToCloseTimeout} -> executionStartToCloseTimeout) (\s@WorkflowExecutionContinuedAsNewEventAttributes' {} a -> s {executionStartToCloseTimeout = a} :: WorkflowExecutionContinuedAsNewEventAttributes)

-- | The maximum duration of decision tasks for the new workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
workflowExecutionContinuedAsNewEventAttributes_taskStartToCloseTimeout :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionContinuedAsNewEventAttributes_taskStartToCloseTimeout = Lens.lens (\WorkflowExecutionContinuedAsNewEventAttributes' {taskStartToCloseTimeout} -> taskStartToCloseTimeout) (\s@WorkflowExecutionContinuedAsNewEventAttributes' {} a -> s {taskStartToCloseTimeout = a} :: WorkflowExecutionContinuedAsNewEventAttributes)

-- | The list of tags associated with the new workflow execution.
workflowExecutionContinuedAsNewEventAttributes_tagList :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes (Prelude.Maybe [Prelude.Text])
workflowExecutionContinuedAsNewEventAttributes_tagList = Lens.lens (\WorkflowExecutionContinuedAsNewEventAttributes' {tagList} -> tagList) (\s@WorkflowExecutionContinuedAsNewEventAttributes' {} a -> s {tagList = a} :: WorkflowExecutionContinuedAsNewEventAttributes) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @ContinueAsNewWorkflowExecution@
-- decision that started this execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
workflowExecutionContinuedAsNewEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes Prelude.Integer
workflowExecutionContinuedAsNewEventAttributes_decisionTaskCompletedEventId = Lens.lens (\WorkflowExecutionContinuedAsNewEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@WorkflowExecutionContinuedAsNewEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: WorkflowExecutionContinuedAsNewEventAttributes)

-- | The @runId@ of the new workflow execution.
workflowExecutionContinuedAsNewEventAttributes_newExecutionRunId :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes Prelude.Text
workflowExecutionContinuedAsNewEventAttributes_newExecutionRunId = Lens.lens (\WorkflowExecutionContinuedAsNewEventAttributes' {newExecutionRunId'} -> newExecutionRunId') (\s@WorkflowExecutionContinuedAsNewEventAttributes' {} a -> s {newExecutionRunId' = a} :: WorkflowExecutionContinuedAsNewEventAttributes)

-- | The task list to use for the decisions of the new (continued) workflow
-- execution.
workflowExecutionContinuedAsNewEventAttributes_taskList :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes TaskList
workflowExecutionContinuedAsNewEventAttributes_taskList = Lens.lens (\WorkflowExecutionContinuedAsNewEventAttributes' {taskList} -> taskList) (\s@WorkflowExecutionContinuedAsNewEventAttributes' {} a -> s {taskList = a} :: WorkflowExecutionContinuedAsNewEventAttributes)

-- | The policy to use for the child workflow executions of the new execution
-- if it is terminated by calling the TerminateWorkflowExecution action
-- explicitly or due to an expired timeout.
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
workflowExecutionContinuedAsNewEventAttributes_childPolicy :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes ChildPolicy
workflowExecutionContinuedAsNewEventAttributes_childPolicy = Lens.lens (\WorkflowExecutionContinuedAsNewEventAttributes' {childPolicy} -> childPolicy) (\s@WorkflowExecutionContinuedAsNewEventAttributes' {} a -> s {childPolicy = a} :: WorkflowExecutionContinuedAsNewEventAttributes)

-- | The workflow type of this execution.
workflowExecutionContinuedAsNewEventAttributes_workflowType :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes WorkflowType
workflowExecutionContinuedAsNewEventAttributes_workflowType = Lens.lens (\WorkflowExecutionContinuedAsNewEventAttributes' {workflowType} -> workflowType) (\s@WorkflowExecutionContinuedAsNewEventAttributes' {} a -> s {workflowType = a} :: WorkflowExecutionContinuedAsNewEventAttributes)

instance
  Prelude.FromJSON
    WorkflowExecutionContinuedAsNewEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "WorkflowExecutionContinuedAsNewEventAttributes"
      ( \x ->
          WorkflowExecutionContinuedAsNewEventAttributes'
            Prelude.<$> (x Prelude..:? "input")
              Prelude.<*> (x Prelude..:? "lambdaRole")
              Prelude.<*> (x Prelude..:? "taskPriority")
              Prelude.<*> (x Prelude..:? "executionStartToCloseTimeout")
              Prelude.<*> (x Prelude..:? "taskStartToCloseTimeout")
              Prelude.<*> (x Prelude..:? "tagList" Prelude..!= Prelude.mempty)
              Prelude.<*> (x Prelude..: "decisionTaskCompletedEventId")
              Prelude.<*> (x Prelude..: "newExecutionRunId")
              Prelude.<*> (x Prelude..: "taskList")
              Prelude.<*> (x Prelude..: "childPolicy")
              Prelude.<*> (x Prelude..: "workflowType")
      )

instance
  Prelude.Hashable
    WorkflowExecutionContinuedAsNewEventAttributes

instance
  Prelude.NFData
    WorkflowExecutionContinuedAsNewEventAttributes
