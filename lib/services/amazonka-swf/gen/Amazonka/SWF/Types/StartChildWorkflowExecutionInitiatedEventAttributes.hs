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
-- Module      : Amazonka.SWF.Types.StartChildWorkflowExecutionInitiatedEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.StartChildWorkflowExecutionInitiatedEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.ChildPolicy
import Amazonka.SWF.Types.TaskList
import Amazonka.SWF.Types.WorkflowType

-- | Provides the details of the @StartChildWorkflowExecutionInitiated@
-- event.
--
-- /See:/ 'newStartChildWorkflowExecutionInitiatedEventAttributes' smart constructor.
data StartChildWorkflowExecutionInitiatedEventAttributes = StartChildWorkflowExecutionInitiatedEventAttributes'
  { -- | Data attached to the event that can be used by the decider in subsequent
    -- decision tasks. This data isn\'t sent to the activity.
    control :: Prelude.Maybe Prelude.Text,
    -- | The maximum duration for the child workflow execution. If the workflow
    -- execution isn\'t closed within this duration, it is timed out and
    -- force-terminated.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    executionStartToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The inputs provided to the child workflow execution.
    input :: Prelude.Maybe Prelude.Text,
    -- | The IAM role to attach to the child workflow execution.
    lambdaRole :: Prelude.Maybe Prelude.Text,
    -- | The list of tags to associated with the child workflow execution.
    tagList :: Prelude.Maybe [Prelude.Text],
    -- | The priority assigned for the decision tasks for this workflow
    -- execution. Valid values are integers that range from Java\'s
    -- @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647).
    -- Higher numbers indicate higher priority.
    --
    -- For more information about setting task priority, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
    -- in the /Amazon SWF Developer Guide/.
    taskPriority :: Prelude.Maybe Prelude.Text,
    -- | The maximum duration allowed for the decision tasks for this workflow
    -- execution.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    taskStartToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The @workflowId@ of the child workflow execution.
    workflowId :: Prelude.Text,
    -- | The type of the child workflow execution.
    workflowType :: WorkflowType,
    -- | The name of the task list used for the decision tasks of the child
    -- workflow execution.
    taskList :: TaskList,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision task that resulted in the @StartChildWorkflowExecution@
    -- Decision to request this child workflow execution. This information can
    -- be useful for diagnosing problems by tracing back the cause of events.
    decisionTaskCompletedEventId :: Prelude.Integer,
    -- | The policy to use for the child workflow executions if this execution
    -- gets terminated by explicitly calling the TerminateWorkflowExecution
    -- action or due to an expired timeout.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartChildWorkflowExecutionInitiatedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'control', 'startChildWorkflowExecutionInitiatedEventAttributes_control' - Data attached to the event that can be used by the decider in subsequent
-- decision tasks. This data isn\'t sent to the activity.
--
-- 'executionStartToCloseTimeout', 'startChildWorkflowExecutionInitiatedEventAttributes_executionStartToCloseTimeout' - The maximum duration for the child workflow execution. If the workflow
-- execution isn\'t closed within this duration, it is timed out and
-- force-terminated.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'input', 'startChildWorkflowExecutionInitiatedEventAttributes_input' - The inputs provided to the child workflow execution.
--
-- 'lambdaRole', 'startChildWorkflowExecutionInitiatedEventAttributes_lambdaRole' - The IAM role to attach to the child workflow execution.
--
-- 'tagList', 'startChildWorkflowExecutionInitiatedEventAttributes_tagList' - The list of tags to associated with the child workflow execution.
--
-- 'taskPriority', 'startChildWorkflowExecutionInitiatedEventAttributes_taskPriority' - The priority assigned for the decision tasks for this workflow
-- execution. Valid values are integers that range from Java\'s
-- @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647).
-- Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
--
-- 'taskStartToCloseTimeout', 'startChildWorkflowExecutionInitiatedEventAttributes_taskStartToCloseTimeout' - The maximum duration allowed for the decision tasks for this workflow
-- execution.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'workflowId', 'startChildWorkflowExecutionInitiatedEventAttributes_workflowId' - The @workflowId@ of the child workflow execution.
--
-- 'workflowType', 'startChildWorkflowExecutionInitiatedEventAttributes_workflowType' - The type of the child workflow execution.
--
-- 'taskList', 'startChildWorkflowExecutionInitiatedEventAttributes_taskList' - The name of the task list used for the decision tasks of the child
-- workflow execution.
--
-- 'decisionTaskCompletedEventId', 'startChildWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @StartChildWorkflowExecution@
-- Decision to request this child workflow execution. This information can
-- be useful for diagnosing problems by tracing back the cause of events.
--
-- 'childPolicy', 'startChildWorkflowExecutionInitiatedEventAttributes_childPolicy' - The policy to use for the child workflow executions if this execution
-- gets terminated by explicitly calling the TerminateWorkflowExecution
-- action or due to an expired timeout.
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
newStartChildWorkflowExecutionInitiatedEventAttributes ::
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'workflowType'
  WorkflowType ->
  -- | 'taskList'
  TaskList ->
  -- | 'decisionTaskCompletedEventId'
  Prelude.Integer ->
  -- | 'childPolicy'
  ChildPolicy ->
  StartChildWorkflowExecutionInitiatedEventAttributes
newStartChildWorkflowExecutionInitiatedEventAttributes
  pWorkflowId_
  pWorkflowType_
  pTaskList_
  pDecisionTaskCompletedEventId_
  pChildPolicy_ =
    StartChildWorkflowExecutionInitiatedEventAttributes'
      { control =
          Prelude.Nothing,
        executionStartToCloseTimeout =
          Prelude.Nothing,
        input =
          Prelude.Nothing,
        lambdaRole =
          Prelude.Nothing,
        tagList =
          Prelude.Nothing,
        taskPriority =
          Prelude.Nothing,
        taskStartToCloseTimeout =
          Prelude.Nothing,
        workflowId =
          pWorkflowId_,
        workflowType =
          pWorkflowType_,
        taskList = pTaskList_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_,
        childPolicy =
          pChildPolicy_
      }

-- | Data attached to the event that can be used by the decider in subsequent
-- decision tasks. This data isn\'t sent to the activity.
startChildWorkflowExecutionInitiatedEventAttributes_control :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Prelude.Maybe Prelude.Text)
startChildWorkflowExecutionInitiatedEventAttributes_control = Lens.lens (\StartChildWorkflowExecutionInitiatedEventAttributes' {control} -> control) (\s@StartChildWorkflowExecutionInitiatedEventAttributes' {} a -> s {control = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)

-- | The maximum duration for the child workflow execution. If the workflow
-- execution isn\'t closed within this duration, it is timed out and
-- force-terminated.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
startChildWorkflowExecutionInitiatedEventAttributes_executionStartToCloseTimeout :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Prelude.Maybe Prelude.Text)
startChildWorkflowExecutionInitiatedEventAttributes_executionStartToCloseTimeout = Lens.lens (\StartChildWorkflowExecutionInitiatedEventAttributes' {executionStartToCloseTimeout} -> executionStartToCloseTimeout) (\s@StartChildWorkflowExecutionInitiatedEventAttributes' {} a -> s {executionStartToCloseTimeout = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)

-- | The inputs provided to the child workflow execution.
startChildWorkflowExecutionInitiatedEventAttributes_input :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Prelude.Maybe Prelude.Text)
startChildWorkflowExecutionInitiatedEventAttributes_input = Lens.lens (\StartChildWorkflowExecutionInitiatedEventAttributes' {input} -> input) (\s@StartChildWorkflowExecutionInitiatedEventAttributes' {} a -> s {input = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)

-- | The IAM role to attach to the child workflow execution.
startChildWorkflowExecutionInitiatedEventAttributes_lambdaRole :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Prelude.Maybe Prelude.Text)
startChildWorkflowExecutionInitiatedEventAttributes_lambdaRole = Lens.lens (\StartChildWorkflowExecutionInitiatedEventAttributes' {lambdaRole} -> lambdaRole) (\s@StartChildWorkflowExecutionInitiatedEventAttributes' {} a -> s {lambdaRole = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)

-- | The list of tags to associated with the child workflow execution.
startChildWorkflowExecutionInitiatedEventAttributes_tagList :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Prelude.Maybe [Prelude.Text])
startChildWorkflowExecutionInitiatedEventAttributes_tagList = Lens.lens (\StartChildWorkflowExecutionInitiatedEventAttributes' {tagList} -> tagList) (\s@StartChildWorkflowExecutionInitiatedEventAttributes' {} a -> s {tagList = a} :: StartChildWorkflowExecutionInitiatedEventAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The priority assigned for the decision tasks for this workflow
-- execution. Valid values are integers that range from Java\'s
-- @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647).
-- Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
startChildWorkflowExecutionInitiatedEventAttributes_taskPriority :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Prelude.Maybe Prelude.Text)
startChildWorkflowExecutionInitiatedEventAttributes_taskPriority = Lens.lens (\StartChildWorkflowExecutionInitiatedEventAttributes' {taskPriority} -> taskPriority) (\s@StartChildWorkflowExecutionInitiatedEventAttributes' {} a -> s {taskPriority = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)

-- | The maximum duration allowed for the decision tasks for this workflow
-- execution.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
startChildWorkflowExecutionInitiatedEventAttributes_taskStartToCloseTimeout :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Prelude.Maybe Prelude.Text)
startChildWorkflowExecutionInitiatedEventAttributes_taskStartToCloseTimeout = Lens.lens (\StartChildWorkflowExecutionInitiatedEventAttributes' {taskStartToCloseTimeout} -> taskStartToCloseTimeout) (\s@StartChildWorkflowExecutionInitiatedEventAttributes' {} a -> s {taskStartToCloseTimeout = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)

-- | The @workflowId@ of the child workflow execution.
startChildWorkflowExecutionInitiatedEventAttributes_workflowId :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes Prelude.Text
startChildWorkflowExecutionInitiatedEventAttributes_workflowId = Lens.lens (\StartChildWorkflowExecutionInitiatedEventAttributes' {workflowId} -> workflowId) (\s@StartChildWorkflowExecutionInitiatedEventAttributes' {} a -> s {workflowId = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)

-- | The type of the child workflow execution.
startChildWorkflowExecutionInitiatedEventAttributes_workflowType :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes WorkflowType
startChildWorkflowExecutionInitiatedEventAttributes_workflowType = Lens.lens (\StartChildWorkflowExecutionInitiatedEventAttributes' {workflowType} -> workflowType) (\s@StartChildWorkflowExecutionInitiatedEventAttributes' {} a -> s {workflowType = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)

-- | The name of the task list used for the decision tasks of the child
-- workflow execution.
startChildWorkflowExecutionInitiatedEventAttributes_taskList :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes TaskList
startChildWorkflowExecutionInitiatedEventAttributes_taskList = Lens.lens (\StartChildWorkflowExecutionInitiatedEventAttributes' {taskList} -> taskList) (\s@StartChildWorkflowExecutionInitiatedEventAttributes' {} a -> s {taskList = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @StartChildWorkflowExecution@
-- Decision to request this child workflow execution. This information can
-- be useful for diagnosing problems by tracing back the cause of events.
startChildWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes Prelude.Integer
startChildWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId = Lens.lens (\StartChildWorkflowExecutionInitiatedEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@StartChildWorkflowExecutionInitiatedEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)

-- | The policy to use for the child workflow executions if this execution
-- gets terminated by explicitly calling the TerminateWorkflowExecution
-- action or due to an expired timeout.
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
startChildWorkflowExecutionInitiatedEventAttributes_childPolicy :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes ChildPolicy
startChildWorkflowExecutionInitiatedEventAttributes_childPolicy = Lens.lens (\StartChildWorkflowExecutionInitiatedEventAttributes' {childPolicy} -> childPolicy) (\s@StartChildWorkflowExecutionInitiatedEventAttributes' {} a -> s {childPolicy = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)

instance
  Data.FromJSON
    StartChildWorkflowExecutionInitiatedEventAttributes
  where
  parseJSON =
    Data.withObject
      "StartChildWorkflowExecutionInitiatedEventAttributes"
      ( \x ->
          StartChildWorkflowExecutionInitiatedEventAttributes'
            Prelude.<$> (x Data..:? "control")
            Prelude.<*> (x Data..:? "executionStartToCloseTimeout")
            Prelude.<*> (x Data..:? "input")
            Prelude.<*> (x Data..:? "lambdaRole")
            Prelude.<*> (x Data..:? "tagList" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "taskPriority")
            Prelude.<*> (x Data..:? "taskStartToCloseTimeout")
            Prelude.<*> (x Data..: "workflowId")
            Prelude.<*> (x Data..: "workflowType")
            Prelude.<*> (x Data..: "taskList")
            Prelude.<*> (x Data..: "decisionTaskCompletedEventId")
            Prelude.<*> (x Data..: "childPolicy")
      )

instance
  Prelude.Hashable
    StartChildWorkflowExecutionInitiatedEventAttributes
  where
  hashWithSalt
    _salt
    StartChildWorkflowExecutionInitiatedEventAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` control
        `Prelude.hashWithSalt` executionStartToCloseTimeout
        `Prelude.hashWithSalt` input
        `Prelude.hashWithSalt` lambdaRole
        `Prelude.hashWithSalt` tagList
        `Prelude.hashWithSalt` taskPriority
        `Prelude.hashWithSalt` taskStartToCloseTimeout
        `Prelude.hashWithSalt` workflowId
        `Prelude.hashWithSalt` workflowType
        `Prelude.hashWithSalt` taskList
        `Prelude.hashWithSalt` decisionTaskCompletedEventId
        `Prelude.hashWithSalt` childPolicy

instance
  Prelude.NFData
    StartChildWorkflowExecutionInitiatedEventAttributes
  where
  rnf
    StartChildWorkflowExecutionInitiatedEventAttributes' {..} =
      Prelude.rnf control
        `Prelude.seq` Prelude.rnf executionStartToCloseTimeout
        `Prelude.seq` Prelude.rnf input
        `Prelude.seq` Prelude.rnf lambdaRole
        `Prelude.seq` Prelude.rnf tagList
        `Prelude.seq` Prelude.rnf taskPriority
        `Prelude.seq` Prelude.rnf taskStartToCloseTimeout
        `Prelude.seq` Prelude.rnf workflowId
        `Prelude.seq` Prelude.rnf workflowType
        `Prelude.seq` Prelude.rnf taskList
        `Prelude.seq` Prelude.rnf decisionTaskCompletedEventId
        `Prelude.seq` Prelude.rnf childPolicy
