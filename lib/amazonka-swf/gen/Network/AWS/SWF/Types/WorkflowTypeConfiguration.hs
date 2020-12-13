{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowTypeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowTypeConfiguration
  ( WorkflowTypeConfiguration (..),

    -- * Smart constructor
    mkWorkflowTypeConfiguration,

    -- * Lenses
    wtcDefaultLambdaRole,
    wtcDefaultChildPolicy,
    wtcDefaultTaskList,
    wtcDefaultTaskPriority,
    wtcDefaultExecutionStartToCloseTimeout,
    wtcDefaultTaskStartToCloseTimeout,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList

-- | The configuration settings of a workflow type.
--
-- /See:/ 'mkWorkflowTypeConfiguration' smart constructor.
data WorkflowTypeConfiguration = WorkflowTypeConfiguration'
  { -- | The default IAM role attached to this workflow type.
    defaultLambdaRole :: Lude.Maybe Lude.Text,
    -- | The default policy to use for the child workflow executions when a workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
    --
    -- The supported child policies are:
    --
    --     * @TERMINATE@ – The child executions are terminated.
    --
    --
    --     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.
    --
    --
    --     * @ABANDON@ – No action is taken. The child executions continue to run.
    defaultChildPolicy :: Lude.Maybe ChildPolicy,
    -- | The default task list, specified when registering the workflow type, for decisions tasks scheduled for workflow executions of this type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
    defaultTaskList :: Lude.Maybe TaskList,
    -- | The default task priority, specified when registering the workflow type, for all decision tasks of this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ decision.
    --
    -- Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
    -- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
    defaultTaskPriority :: Lude.Maybe Lude.Text,
    -- | The default maximum duration, specified when registering the workflow type, for executions of this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    defaultExecutionStartToCloseTimeout :: Lude.Maybe Lude.Text,
    -- | The default maximum duration, specified when registering the workflow type, that a decision task for executions of this workflow type might take before returning completion or failure. If the task doesn'tdo close in the specified time then the task is automatically timed out and rescheduled. If the decider eventually reports a completion or failure, it is ignored. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    defaultTaskStartToCloseTimeout :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowTypeConfiguration' with the minimum fields required to make a request.
--
-- * 'defaultLambdaRole' - The default IAM role attached to this workflow type.
-- * 'defaultChildPolicy' - The default policy to use for the child workflow executions when a workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- The supported child policies are:
--
--     * @TERMINATE@ – The child executions are terminated.
--
--
--     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.
--
--
--     * @ABANDON@ – No action is taken. The child executions continue to run.
--
--
-- * 'defaultTaskList' - The default task list, specified when registering the workflow type, for decisions tasks scheduled for workflow executions of this type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
-- * 'defaultTaskPriority' - The default task priority, specified when registering the workflow type, for all decision tasks of this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ decision.
--
-- Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
-- * 'defaultExecutionStartToCloseTimeout' - The default maximum duration, specified when registering the workflow type, for executions of this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'defaultTaskStartToCloseTimeout' - The default maximum duration, specified when registering the workflow type, that a decision task for executions of this workflow type might take before returning completion or failure. If the task doesn'tdo close in the specified time then the task is automatically timed out and rescheduled. If the decider eventually reports a completion or failure, it is ignored. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
mkWorkflowTypeConfiguration ::
  WorkflowTypeConfiguration
mkWorkflowTypeConfiguration =
  WorkflowTypeConfiguration'
    { defaultLambdaRole = Lude.Nothing,
      defaultChildPolicy = Lude.Nothing,
      defaultTaskList = Lude.Nothing,
      defaultTaskPriority = Lude.Nothing,
      defaultExecutionStartToCloseTimeout = Lude.Nothing,
      defaultTaskStartToCloseTimeout = Lude.Nothing
    }

-- | The default IAM role attached to this workflow type.
--
-- /Note:/ Consider using 'defaultLambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtcDefaultLambdaRole :: Lens.Lens' WorkflowTypeConfiguration (Lude.Maybe Lude.Text)
wtcDefaultLambdaRole = Lens.lens (defaultLambdaRole :: WorkflowTypeConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {defaultLambdaRole = a} :: WorkflowTypeConfiguration)
{-# DEPRECATED wtcDefaultLambdaRole "Use generic-lens or generic-optics with 'defaultLambdaRole' instead." #-}

-- | The default policy to use for the child workflow executions when a workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- The supported child policies are:
--
--     * @TERMINATE@ – The child executions are terminated.
--
--
--     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.
--
--
--     * @ABANDON@ – No action is taken. The child executions continue to run.
--
--
--
-- /Note:/ Consider using 'defaultChildPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtcDefaultChildPolicy :: Lens.Lens' WorkflowTypeConfiguration (Lude.Maybe ChildPolicy)
wtcDefaultChildPolicy = Lens.lens (defaultChildPolicy :: WorkflowTypeConfiguration -> Lude.Maybe ChildPolicy) (\s a -> s {defaultChildPolicy = a} :: WorkflowTypeConfiguration)
{-# DEPRECATED wtcDefaultChildPolicy "Use generic-lens or generic-optics with 'defaultChildPolicy' instead." #-}

-- | The default task list, specified when registering the workflow type, for decisions tasks scheduled for workflow executions of this type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- /Note:/ Consider using 'defaultTaskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtcDefaultTaskList :: Lens.Lens' WorkflowTypeConfiguration (Lude.Maybe TaskList)
wtcDefaultTaskList = Lens.lens (defaultTaskList :: WorkflowTypeConfiguration -> Lude.Maybe TaskList) (\s a -> s {defaultTaskList = a} :: WorkflowTypeConfiguration)
{-# DEPRECATED wtcDefaultTaskList "Use generic-lens or generic-optics with 'defaultTaskList' instead." #-}

-- | The default task priority, specified when registering the workflow type, for all decision tasks of this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ decision.
--
-- Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'defaultTaskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtcDefaultTaskPriority :: Lens.Lens' WorkflowTypeConfiguration (Lude.Maybe Lude.Text)
wtcDefaultTaskPriority = Lens.lens (defaultTaskPriority :: WorkflowTypeConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {defaultTaskPriority = a} :: WorkflowTypeConfiguration)
{-# DEPRECATED wtcDefaultTaskPriority "Use generic-lens or generic-optics with 'defaultTaskPriority' instead." #-}

-- | The default maximum duration, specified when registering the workflow type, for executions of this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultExecutionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtcDefaultExecutionStartToCloseTimeout :: Lens.Lens' WorkflowTypeConfiguration (Lude.Maybe Lude.Text)
wtcDefaultExecutionStartToCloseTimeout = Lens.lens (defaultExecutionStartToCloseTimeout :: WorkflowTypeConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {defaultExecutionStartToCloseTimeout = a} :: WorkflowTypeConfiguration)
{-# DEPRECATED wtcDefaultExecutionStartToCloseTimeout "Use generic-lens or generic-optics with 'defaultExecutionStartToCloseTimeout' instead." #-}

-- | The default maximum duration, specified when registering the workflow type, that a decision task for executions of this workflow type might take before returning completion or failure. If the task doesn'tdo close in the specified time then the task is automatically timed out and rescheduled. If the decider eventually reports a completion or failure, it is ignored. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtcDefaultTaskStartToCloseTimeout :: Lens.Lens' WorkflowTypeConfiguration (Lude.Maybe Lude.Text)
wtcDefaultTaskStartToCloseTimeout = Lens.lens (defaultTaskStartToCloseTimeout :: WorkflowTypeConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {defaultTaskStartToCloseTimeout = a} :: WorkflowTypeConfiguration)
{-# DEPRECATED wtcDefaultTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'defaultTaskStartToCloseTimeout' instead." #-}

instance Lude.FromJSON WorkflowTypeConfiguration where
  parseJSON =
    Lude.withObject
      "WorkflowTypeConfiguration"
      ( \x ->
          WorkflowTypeConfiguration'
            Lude.<$> (x Lude..:? "defaultLambdaRole")
            Lude.<*> (x Lude..:? "defaultChildPolicy")
            Lude.<*> (x Lude..:? "defaultTaskList")
            Lude.<*> (x Lude..:? "defaultTaskPriority")
            Lude.<*> (x Lude..:? "defaultExecutionStartToCloseTimeout")
            Lude.<*> (x Lude..:? "defaultTaskStartToCloseTimeout")
      )
