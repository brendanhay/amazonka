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
-- Module      : Amazonka.SWF.Types.WorkflowExecutionStartedEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowExecutionStartedEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.ChildPolicy
import Amazonka.SWF.Types.TaskList
import Amazonka.SWF.Types.WorkflowExecution
import Amazonka.SWF.Types.WorkflowType

-- | Provides details of @WorkflowExecutionStarted@ event.
--
-- /See:/ 'newWorkflowExecutionStartedEventAttributes' smart constructor.
data WorkflowExecutionStartedEventAttributes = WorkflowExecutionStartedEventAttributes'
  { -- | If this workflow execution was started due to a
    -- @ContinueAsNewWorkflowExecution@ decision, then it contains the @runId@
    -- of the previous workflow execution that was closed and continued as this
    -- execution.
    continuedExecutionRunId :: Prelude.Maybe Prelude.Text,
    -- | The maximum duration for this workflow execution.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    executionStartToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The input provided to the workflow execution.
    input :: Prelude.Maybe Prelude.Text,
    -- | The IAM role attached to the workflow execution.
    lambdaRole :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
    -- to the @StartChildWorkflowExecution@ Decision to start this workflow
    -- execution. The source event with this ID can be found in the history of
    -- the source workflow execution. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    parentInitiatedEventId :: Prelude.Maybe Prelude.Integer,
    -- | The source workflow execution that started this workflow execution. The
    -- member isn\'t set if the workflow execution was not started by a
    -- workflow.
    parentWorkflowExecution :: Prelude.Maybe WorkflowExecution,
    -- | The list of tags associated with this workflow execution. An execution
    -- can have up to 5 tags.
    tagList :: Prelude.Maybe [Prelude.Text],
    -- | The priority of the decision tasks in the workflow execution.
    taskPriority :: Prelude.Maybe Prelude.Text,
    -- | The maximum duration of decision tasks for this workflow type.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    taskStartToCloseTimeout :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionStartedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continuedExecutionRunId', 'workflowExecutionStartedEventAttributes_continuedExecutionRunId' - If this workflow execution was started due to a
-- @ContinueAsNewWorkflowExecution@ decision, then it contains the @runId@
-- of the previous workflow execution that was closed and continued as this
-- execution.
--
-- 'executionStartToCloseTimeout', 'workflowExecutionStartedEventAttributes_executionStartToCloseTimeout' - The maximum duration for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'input', 'workflowExecutionStartedEventAttributes_input' - The input provided to the workflow execution.
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
-- 'parentWorkflowExecution', 'workflowExecutionStartedEventAttributes_parentWorkflowExecution' - The source workflow execution that started this workflow execution. The
-- member isn\'t set if the workflow execution was not started by a
-- workflow.
--
-- 'tagList', 'workflowExecutionStartedEventAttributes_tagList' - The list of tags associated with this workflow execution. An execution
-- can have up to 5 tags.
--
-- 'taskPriority', 'workflowExecutionStartedEventAttributes_taskPriority' - The priority of the decision tasks in the workflow execution.
--
-- 'taskStartToCloseTimeout', 'workflowExecutionStartedEventAttributes_taskStartToCloseTimeout' - The maximum duration of decision tasks for this workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
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
      { continuedExecutionRunId =
          Prelude.Nothing,
        executionStartToCloseTimeout =
          Prelude.Nothing,
        input = Prelude.Nothing,
        lambdaRole = Prelude.Nothing,
        parentInitiatedEventId =
          Prelude.Nothing,
        parentWorkflowExecution =
          Prelude.Nothing,
        tagList = Prelude.Nothing,
        taskPriority = Prelude.Nothing,
        taskStartToCloseTimeout =
          Prelude.Nothing,
        childPolicy = pChildPolicy_,
        taskList = pTaskList_,
        workflowType = pWorkflowType_
      }

-- | If this workflow execution was started due to a
-- @ContinueAsNewWorkflowExecution@ decision, then it contains the @runId@
-- of the previous workflow execution that was closed and continued as this
-- execution.
workflowExecutionStartedEventAttributes_continuedExecutionRunId :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionStartedEventAttributes_continuedExecutionRunId = Lens.lens (\WorkflowExecutionStartedEventAttributes' {continuedExecutionRunId} -> continuedExecutionRunId) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {continuedExecutionRunId = a} :: WorkflowExecutionStartedEventAttributes)

-- | The maximum duration for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
workflowExecutionStartedEventAttributes_executionStartToCloseTimeout :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionStartedEventAttributes_executionStartToCloseTimeout = Lens.lens (\WorkflowExecutionStartedEventAttributes' {executionStartToCloseTimeout} -> executionStartToCloseTimeout) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {executionStartToCloseTimeout = a} :: WorkflowExecutionStartedEventAttributes)

-- | The input provided to the workflow execution.
workflowExecutionStartedEventAttributes_input :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionStartedEventAttributes_input = Lens.lens (\WorkflowExecutionStartedEventAttributes' {input} -> input) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {input = a} :: WorkflowExecutionStartedEventAttributes)

-- | The IAM role attached to the workflow execution.
workflowExecutionStartedEventAttributes_lambdaRole :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionStartedEventAttributes_lambdaRole = Lens.lens (\WorkflowExecutionStartedEventAttributes' {lambdaRole} -> lambdaRole) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {lambdaRole = a} :: WorkflowExecutionStartedEventAttributes)

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this workflow
-- execution. The source event with this ID can be found in the history of
-- the source workflow execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
workflowExecutionStartedEventAttributes_parentInitiatedEventId :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Prelude.Maybe Prelude.Integer)
workflowExecutionStartedEventAttributes_parentInitiatedEventId = Lens.lens (\WorkflowExecutionStartedEventAttributes' {parentInitiatedEventId} -> parentInitiatedEventId) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {parentInitiatedEventId = a} :: WorkflowExecutionStartedEventAttributes)

-- | The source workflow execution that started this workflow execution. The
-- member isn\'t set if the workflow execution was not started by a
-- workflow.
workflowExecutionStartedEventAttributes_parentWorkflowExecution :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Prelude.Maybe WorkflowExecution)
workflowExecutionStartedEventAttributes_parentWorkflowExecution = Lens.lens (\WorkflowExecutionStartedEventAttributes' {parentWorkflowExecution} -> parentWorkflowExecution) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {parentWorkflowExecution = a} :: WorkflowExecutionStartedEventAttributes)

-- | The list of tags associated with this workflow execution. An execution
-- can have up to 5 tags.
workflowExecutionStartedEventAttributes_tagList :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Prelude.Maybe [Prelude.Text])
workflowExecutionStartedEventAttributes_tagList = Lens.lens (\WorkflowExecutionStartedEventAttributes' {tagList} -> tagList) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {tagList = a} :: WorkflowExecutionStartedEventAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The priority of the decision tasks in the workflow execution.
workflowExecutionStartedEventAttributes_taskPriority :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionStartedEventAttributes_taskPriority = Lens.lens (\WorkflowExecutionStartedEventAttributes' {taskPriority} -> taskPriority) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {taskPriority = a} :: WorkflowExecutionStartedEventAttributes)

-- | The maximum duration of decision tasks for this workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
workflowExecutionStartedEventAttributes_taskStartToCloseTimeout :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Prelude.Maybe Prelude.Text)
workflowExecutionStartedEventAttributes_taskStartToCloseTimeout = Lens.lens (\WorkflowExecutionStartedEventAttributes' {taskStartToCloseTimeout} -> taskStartToCloseTimeout) (\s@WorkflowExecutionStartedEventAttributes' {} a -> s {taskStartToCloseTimeout = a} :: WorkflowExecutionStartedEventAttributes)

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
  Data.FromJSON
    WorkflowExecutionStartedEventAttributes
  where
  parseJSON =
    Data.withObject
      "WorkflowExecutionStartedEventAttributes"
      ( \x ->
          WorkflowExecutionStartedEventAttributes'
            Prelude.<$> (x Data..:? "continuedExecutionRunId")
            Prelude.<*> (x Data..:? "executionStartToCloseTimeout")
            Prelude.<*> (x Data..:? "input")
            Prelude.<*> (x Data..:? "lambdaRole")
            Prelude.<*> (x Data..:? "parentInitiatedEventId")
            Prelude.<*> (x Data..:? "parentWorkflowExecution")
            Prelude.<*> (x Data..:? "tagList" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "taskPriority")
            Prelude.<*> (x Data..:? "taskStartToCloseTimeout")
            Prelude.<*> (x Data..: "childPolicy")
            Prelude.<*> (x Data..: "taskList")
            Prelude.<*> (x Data..: "workflowType")
      )

instance
  Prelude.Hashable
    WorkflowExecutionStartedEventAttributes
  where
  hashWithSalt
    _salt
    WorkflowExecutionStartedEventAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` continuedExecutionRunId
        `Prelude.hashWithSalt` executionStartToCloseTimeout
        `Prelude.hashWithSalt` input
        `Prelude.hashWithSalt` lambdaRole
        `Prelude.hashWithSalt` parentInitiatedEventId
        `Prelude.hashWithSalt` parentWorkflowExecution
        `Prelude.hashWithSalt` tagList
        `Prelude.hashWithSalt` taskPriority
        `Prelude.hashWithSalt` taskStartToCloseTimeout
        `Prelude.hashWithSalt` childPolicy
        `Prelude.hashWithSalt` taskList
        `Prelude.hashWithSalt` workflowType

instance
  Prelude.NFData
    WorkflowExecutionStartedEventAttributes
  where
  rnf WorkflowExecutionStartedEventAttributes' {..} =
    Prelude.rnf continuedExecutionRunId `Prelude.seq`
      Prelude.rnf executionStartToCloseTimeout `Prelude.seq`
        Prelude.rnf input `Prelude.seq`
          Prelude.rnf lambdaRole `Prelude.seq`
            Prelude.rnf parentInitiatedEventId `Prelude.seq`
              Prelude.rnf parentWorkflowExecution `Prelude.seq`
                Prelude.rnf tagList `Prelude.seq`
                  Prelude.rnf taskPriority `Prelude.seq`
                    Prelude.rnf taskStartToCloseTimeout `Prelude.seq`
                      Prelude.rnf childPolicy `Prelude.seq`
                        Prelude.rnf taskList `Prelude.seq`
                          Prelude.rnf workflowType
