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
-- Module      : Amazonka.SWF.Types.WorkflowTypeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowTypeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.ChildPolicy
import Amazonka.SWF.Types.TaskList

-- | The configuration settings of a workflow type.
--
-- /See:/ 'newWorkflowTypeConfiguration' smart constructor.
data WorkflowTypeConfiguration = WorkflowTypeConfiguration'
  { -- | The default policy to use for the child workflow executions when a
    -- workflow execution of this type is terminated, by calling the
    -- TerminateWorkflowExecution action explicitly or due to an expired
    -- timeout. This default can be overridden when starting a workflow
    -- execution using the StartWorkflowExecution action or the
    -- @StartChildWorkflowExecution@ Decision.
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
    defaultChildPolicy :: Prelude.Maybe ChildPolicy,
    -- | The default maximum duration, specified when registering the workflow
    -- type, for executions of this workflow type. This default can be
    -- overridden when starting a workflow execution using the
    -- StartWorkflowExecution action or the @StartChildWorkflowExecution@
    -- Decision.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    defaultExecutionStartToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The default IAM role attached to this workflow type.
    --
    -- Executions of this workflow type need IAM roles to invoke Lambda
    -- functions. If you don\'t specify an IAM role when starting this workflow
    -- type, the default Lambda role is attached to the execution. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html>
    -- in the /Amazon SWF Developer Guide/.
    defaultLambdaRole :: Prelude.Maybe Prelude.Text,
    -- | The default task list, specified when registering the workflow type, for
    -- decisions tasks scheduled for workflow executions of this type. This
    -- default can be overridden when starting a workflow execution using the
    -- StartWorkflowExecution action or the @StartChildWorkflowExecution@
    -- Decision.
    defaultTaskList :: Prelude.Maybe TaskList,
    -- | The default task priority, specified when registering the workflow type,
    -- for all decision tasks of this workflow type. This default can be
    -- overridden when starting a workflow execution using the
    -- StartWorkflowExecution action or the @StartChildWorkflowExecution@
    -- decision.
    --
    -- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
    -- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
    -- indicate higher priority.
    --
    -- For more information about setting task priority, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
    -- in the /Amazon SWF Developer Guide/.
    defaultTaskPriority :: Prelude.Maybe Prelude.Text,
    -- | The default maximum duration, specified when registering the workflow
    -- type, that a decision task for executions of this workflow type might
    -- take before returning completion or failure. If the task doesn\'tdo
    -- close in the specified time then the task is automatically timed out and
    -- rescheduled. If the decider eventually reports a completion or failure,
    -- it is ignored. This default can be overridden when starting a workflow
    -- execution using the StartWorkflowExecution action or the
    -- @StartChildWorkflowExecution@ Decision.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    defaultTaskStartToCloseTimeout :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowTypeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultChildPolicy', 'workflowTypeConfiguration_defaultChildPolicy' - The default policy to use for the child workflow executions when a
-- workflow execution of this type is terminated, by calling the
-- TerminateWorkflowExecution action explicitly or due to an expired
-- timeout. This default can be overridden when starting a workflow
-- execution using the StartWorkflowExecution action or the
-- @StartChildWorkflowExecution@ Decision.
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
-- 'defaultExecutionStartToCloseTimeout', 'workflowTypeConfiguration_defaultExecutionStartToCloseTimeout' - The default maximum duration, specified when registering the workflow
-- type, for executions of this workflow type. This default can be
-- overridden when starting a workflow execution using the
-- StartWorkflowExecution action or the @StartChildWorkflowExecution@
-- Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'defaultLambdaRole', 'workflowTypeConfiguration_defaultLambdaRole' - The default IAM role attached to this workflow type.
--
-- Executions of this workflow type need IAM roles to invoke Lambda
-- functions. If you don\'t specify an IAM role when starting this workflow
-- type, the default Lambda role is attached to the execution. For more
-- information, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html>
-- in the /Amazon SWF Developer Guide/.
--
-- 'defaultTaskList', 'workflowTypeConfiguration_defaultTaskList' - The default task list, specified when registering the workflow type, for
-- decisions tasks scheduled for workflow executions of this type. This
-- default can be overridden when starting a workflow execution using the
-- StartWorkflowExecution action or the @StartChildWorkflowExecution@
-- Decision.
--
-- 'defaultTaskPriority', 'workflowTypeConfiguration_defaultTaskPriority' - The default task priority, specified when registering the workflow type,
-- for all decision tasks of this workflow type. This default can be
-- overridden when starting a workflow execution using the
-- StartWorkflowExecution action or the @StartChildWorkflowExecution@
-- decision.
--
-- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
--
-- 'defaultTaskStartToCloseTimeout', 'workflowTypeConfiguration_defaultTaskStartToCloseTimeout' - The default maximum duration, specified when registering the workflow
-- type, that a decision task for executions of this workflow type might
-- take before returning completion or failure. If the task doesn\'tdo
-- close in the specified time then the task is automatically timed out and
-- rescheduled. If the decider eventually reports a completion or failure,
-- it is ignored. This default can be overridden when starting a workflow
-- execution using the StartWorkflowExecution action or the
-- @StartChildWorkflowExecution@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
newWorkflowTypeConfiguration ::
  WorkflowTypeConfiguration
newWorkflowTypeConfiguration =
  WorkflowTypeConfiguration'
    { defaultChildPolicy =
        Prelude.Nothing,
      defaultExecutionStartToCloseTimeout =
        Prelude.Nothing,
      defaultLambdaRole = Prelude.Nothing,
      defaultTaskList = Prelude.Nothing,
      defaultTaskPriority = Prelude.Nothing,
      defaultTaskStartToCloseTimeout = Prelude.Nothing
    }

-- | The default policy to use for the child workflow executions when a
-- workflow execution of this type is terminated, by calling the
-- TerminateWorkflowExecution action explicitly or due to an expired
-- timeout. This default can be overridden when starting a workflow
-- execution using the StartWorkflowExecution action or the
-- @StartChildWorkflowExecution@ Decision.
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
workflowTypeConfiguration_defaultChildPolicy :: Lens.Lens' WorkflowTypeConfiguration (Prelude.Maybe ChildPolicy)
workflowTypeConfiguration_defaultChildPolicy = Lens.lens (\WorkflowTypeConfiguration' {defaultChildPolicy} -> defaultChildPolicy) (\s@WorkflowTypeConfiguration' {} a -> s {defaultChildPolicy = a} :: WorkflowTypeConfiguration)

-- | The default maximum duration, specified when registering the workflow
-- type, for executions of this workflow type. This default can be
-- overridden when starting a workflow execution using the
-- StartWorkflowExecution action or the @StartChildWorkflowExecution@
-- Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
workflowTypeConfiguration_defaultExecutionStartToCloseTimeout :: Lens.Lens' WorkflowTypeConfiguration (Prelude.Maybe Prelude.Text)
workflowTypeConfiguration_defaultExecutionStartToCloseTimeout = Lens.lens (\WorkflowTypeConfiguration' {defaultExecutionStartToCloseTimeout} -> defaultExecutionStartToCloseTimeout) (\s@WorkflowTypeConfiguration' {} a -> s {defaultExecutionStartToCloseTimeout = a} :: WorkflowTypeConfiguration)

-- | The default IAM role attached to this workflow type.
--
-- Executions of this workflow type need IAM roles to invoke Lambda
-- functions. If you don\'t specify an IAM role when starting this workflow
-- type, the default Lambda role is attached to the execution. For more
-- information, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html>
-- in the /Amazon SWF Developer Guide/.
workflowTypeConfiguration_defaultLambdaRole :: Lens.Lens' WorkflowTypeConfiguration (Prelude.Maybe Prelude.Text)
workflowTypeConfiguration_defaultLambdaRole = Lens.lens (\WorkflowTypeConfiguration' {defaultLambdaRole} -> defaultLambdaRole) (\s@WorkflowTypeConfiguration' {} a -> s {defaultLambdaRole = a} :: WorkflowTypeConfiguration)

-- | The default task list, specified when registering the workflow type, for
-- decisions tasks scheduled for workflow executions of this type. This
-- default can be overridden when starting a workflow execution using the
-- StartWorkflowExecution action or the @StartChildWorkflowExecution@
-- Decision.
workflowTypeConfiguration_defaultTaskList :: Lens.Lens' WorkflowTypeConfiguration (Prelude.Maybe TaskList)
workflowTypeConfiguration_defaultTaskList = Lens.lens (\WorkflowTypeConfiguration' {defaultTaskList} -> defaultTaskList) (\s@WorkflowTypeConfiguration' {} a -> s {defaultTaskList = a} :: WorkflowTypeConfiguration)

-- | The default task priority, specified when registering the workflow type,
-- for all decision tasks of this workflow type. This default can be
-- overridden when starting a workflow execution using the
-- StartWorkflowExecution action or the @StartChildWorkflowExecution@
-- decision.
--
-- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
workflowTypeConfiguration_defaultTaskPriority :: Lens.Lens' WorkflowTypeConfiguration (Prelude.Maybe Prelude.Text)
workflowTypeConfiguration_defaultTaskPriority = Lens.lens (\WorkflowTypeConfiguration' {defaultTaskPriority} -> defaultTaskPriority) (\s@WorkflowTypeConfiguration' {} a -> s {defaultTaskPriority = a} :: WorkflowTypeConfiguration)

-- | The default maximum duration, specified when registering the workflow
-- type, that a decision task for executions of this workflow type might
-- take before returning completion or failure. If the task doesn\'tdo
-- close in the specified time then the task is automatically timed out and
-- rescheduled. If the decider eventually reports a completion or failure,
-- it is ignored. This default can be overridden when starting a workflow
-- execution using the StartWorkflowExecution action or the
-- @StartChildWorkflowExecution@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
workflowTypeConfiguration_defaultTaskStartToCloseTimeout :: Lens.Lens' WorkflowTypeConfiguration (Prelude.Maybe Prelude.Text)
workflowTypeConfiguration_defaultTaskStartToCloseTimeout = Lens.lens (\WorkflowTypeConfiguration' {defaultTaskStartToCloseTimeout} -> defaultTaskStartToCloseTimeout) (\s@WorkflowTypeConfiguration' {} a -> s {defaultTaskStartToCloseTimeout = a} :: WorkflowTypeConfiguration)

instance Data.FromJSON WorkflowTypeConfiguration where
  parseJSON =
    Data.withObject
      "WorkflowTypeConfiguration"
      ( \x ->
          WorkflowTypeConfiguration'
            Prelude.<$> (x Data..:? "defaultChildPolicy")
            Prelude.<*> (x Data..:? "defaultExecutionStartToCloseTimeout")
            Prelude.<*> (x Data..:? "defaultLambdaRole")
            Prelude.<*> (x Data..:? "defaultTaskList")
            Prelude.<*> (x Data..:? "defaultTaskPriority")
            Prelude.<*> (x Data..:? "defaultTaskStartToCloseTimeout")
      )

instance Prelude.Hashable WorkflowTypeConfiguration where
  hashWithSalt _salt WorkflowTypeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` defaultChildPolicy
      `Prelude.hashWithSalt` defaultExecutionStartToCloseTimeout
      `Prelude.hashWithSalt` defaultLambdaRole
      `Prelude.hashWithSalt` defaultTaskList
      `Prelude.hashWithSalt` defaultTaskPriority
      `Prelude.hashWithSalt` defaultTaskStartToCloseTimeout

instance Prelude.NFData WorkflowTypeConfiguration where
  rnf WorkflowTypeConfiguration' {..} =
    Prelude.rnf defaultChildPolicy
      `Prelude.seq` Prelude.rnf defaultExecutionStartToCloseTimeout
      `Prelude.seq` Prelude.rnf defaultLambdaRole
      `Prelude.seq` Prelude.rnf defaultTaskList
      `Prelude.seq` Prelude.rnf defaultTaskPriority
      `Prelude.seq` Prelude.rnf defaultTaskStartToCloseTimeout
