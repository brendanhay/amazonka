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
    wtcDefaultChildPolicy,
    wtcDefaultExecutionStartToCloseTimeout,
    wtcDefaultLambdaRole,
    wtcDefaultTaskList,
    wtcDefaultTaskPriority,
    wtcDefaultTaskStartToCloseTimeout,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Arn as Types
import qualified Network.AWS.SWF.Types.ChildPolicy as Types
import qualified Network.AWS.SWF.Types.DefaultExecutionStartToCloseTimeout as Types
import qualified Network.AWS.SWF.Types.DefaultTaskPriority as Types
import qualified Network.AWS.SWF.Types.DefaultTaskStartToCloseTimeout as Types
import qualified Network.AWS.SWF.Types.TaskList as Types

-- | The configuration settings of a workflow type.
--
-- /See:/ 'mkWorkflowTypeConfiguration' smart constructor.
data WorkflowTypeConfiguration = WorkflowTypeConfiguration'
  { -- | The default policy to use for the child workflow executions when a workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
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
    defaultChildPolicy :: Core.Maybe Types.ChildPolicy,
    -- | The default maximum duration, specified when registering the workflow type, for executions of this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    defaultExecutionStartToCloseTimeout :: Core.Maybe Types.DefaultExecutionStartToCloseTimeout,
    -- | The default IAM role attached to this workflow type.
    defaultLambdaRole :: Core.Maybe Types.Arn,
    -- | The default task list, specified when registering the workflow type, for decisions tasks scheduled for workflow executions of this type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
    defaultTaskList :: Core.Maybe Types.TaskList,
    -- | The default task priority, specified when registering the workflow type, for all decision tasks of this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ decision.
    --
    -- Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
    -- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
    defaultTaskPriority :: Core.Maybe Types.DefaultTaskPriority,
    -- | The default maximum duration, specified when registering the workflow type, that a decision task for executions of this workflow type might take before returning completion or failure. If the task doesn'tdo close in the specified time then the task is automatically timed out and rescheduled. If the decider eventually reports a completion or failure, it is ignored. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    defaultTaskStartToCloseTimeout :: Core.Maybe Types.DefaultTaskStartToCloseTimeout
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowTypeConfiguration' value with any optional fields omitted.
mkWorkflowTypeConfiguration ::
  WorkflowTypeConfiguration
mkWorkflowTypeConfiguration =
  WorkflowTypeConfiguration'
    { defaultChildPolicy = Core.Nothing,
      defaultExecutionStartToCloseTimeout = Core.Nothing,
      defaultLambdaRole = Core.Nothing,
      defaultTaskList = Core.Nothing,
      defaultTaskPriority = Core.Nothing,
      defaultTaskStartToCloseTimeout = Core.Nothing
    }

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
wtcDefaultChildPolicy :: Lens.Lens' WorkflowTypeConfiguration (Core.Maybe Types.ChildPolicy)
wtcDefaultChildPolicy = Lens.field @"defaultChildPolicy"
{-# DEPRECATED wtcDefaultChildPolicy "Use generic-lens or generic-optics with 'defaultChildPolicy' instead." #-}

-- | The default maximum duration, specified when registering the workflow type, for executions of this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultExecutionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtcDefaultExecutionStartToCloseTimeout :: Lens.Lens' WorkflowTypeConfiguration (Core.Maybe Types.DefaultExecutionStartToCloseTimeout)
wtcDefaultExecutionStartToCloseTimeout = Lens.field @"defaultExecutionStartToCloseTimeout"
{-# DEPRECATED wtcDefaultExecutionStartToCloseTimeout "Use generic-lens or generic-optics with 'defaultExecutionStartToCloseTimeout' instead." #-}

-- | The default IAM role attached to this workflow type.
--
-- /Note:/ Consider using 'defaultLambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtcDefaultLambdaRole :: Lens.Lens' WorkflowTypeConfiguration (Core.Maybe Types.Arn)
wtcDefaultLambdaRole = Lens.field @"defaultLambdaRole"
{-# DEPRECATED wtcDefaultLambdaRole "Use generic-lens or generic-optics with 'defaultLambdaRole' instead." #-}

-- | The default task list, specified when registering the workflow type, for decisions tasks scheduled for workflow executions of this type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- /Note:/ Consider using 'defaultTaskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtcDefaultTaskList :: Lens.Lens' WorkflowTypeConfiguration (Core.Maybe Types.TaskList)
wtcDefaultTaskList = Lens.field @"defaultTaskList"
{-# DEPRECATED wtcDefaultTaskList "Use generic-lens or generic-optics with 'defaultTaskList' instead." #-}

-- | The default task priority, specified when registering the workflow type, for all decision tasks of this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ decision.
--
-- Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'defaultTaskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtcDefaultTaskPriority :: Lens.Lens' WorkflowTypeConfiguration (Core.Maybe Types.DefaultTaskPriority)
wtcDefaultTaskPriority = Lens.field @"defaultTaskPriority"
{-# DEPRECATED wtcDefaultTaskPriority "Use generic-lens or generic-optics with 'defaultTaskPriority' instead." #-}

-- | The default maximum duration, specified when registering the workflow type, that a decision task for executions of this workflow type might take before returning completion or failure. If the task doesn'tdo close in the specified time then the task is automatically timed out and rescheduled. If the decider eventually reports a completion or failure, it is ignored. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtcDefaultTaskStartToCloseTimeout :: Lens.Lens' WorkflowTypeConfiguration (Core.Maybe Types.DefaultTaskStartToCloseTimeout)
wtcDefaultTaskStartToCloseTimeout = Lens.field @"defaultTaskStartToCloseTimeout"
{-# DEPRECATED wtcDefaultTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'defaultTaskStartToCloseTimeout' instead." #-}

instance Core.FromJSON WorkflowTypeConfiguration where
  parseJSON =
    Core.withObject "WorkflowTypeConfiguration" Core.$
      \x ->
        WorkflowTypeConfiguration'
          Core.<$> (x Core..:? "defaultChildPolicy")
          Core.<*> (x Core..:? "defaultExecutionStartToCloseTimeout")
          Core.<*> (x Core..:? "defaultLambdaRole")
          Core.<*> (x Core..:? "defaultTaskList")
          Core.<*> (x Core..:? "defaultTaskPriority")
          Core.<*> (x Core..:? "defaultTaskStartToCloseTimeout")
