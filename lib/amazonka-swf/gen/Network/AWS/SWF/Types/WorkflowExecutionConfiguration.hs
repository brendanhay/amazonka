{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.WorkflowExecutionConfiguration
  ( WorkflowExecutionConfiguration (..)
  -- * Smart constructor
  , mkWorkflowExecutionConfiguration
  -- * Lenses
  , wecTaskStartToCloseTimeout
  , wecExecutionStartToCloseTimeout
  , wecTaskList
  , wecChildPolicy
  , wecLambdaRole
  , wecTaskPriority
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Arn as Types
import qualified Network.AWS.SWF.Types.ChildPolicy as Types
import qualified Network.AWS.SWF.Types.DurationInSeconds as Types
import qualified Network.AWS.SWF.Types.TaskList as Types
import qualified Network.AWS.SWF.Types.TaskPriority as Types

-- | The configuration settings for a workflow execution including timeout values, tasklist etc. These configuration settings are determined from the defaults specified when registering the workflow type and those specified when starting the workflow execution.
--
-- /See:/ 'mkWorkflowExecutionConfiguration' smart constructor.
data WorkflowExecutionConfiguration = WorkflowExecutionConfiguration'
  { taskStartToCloseTimeout :: Types.DurationInSeconds
    -- ^ The maximum duration allowed for decision tasks for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
  , executionStartToCloseTimeout :: Types.DurationInSeconds
    -- ^ The total duration for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
  , taskList :: Types.TaskList
    -- ^ The task list used for the decision tasks generated for this workflow execution.
  , childPolicy :: Types.ChildPolicy
    -- ^ The policy to use for the child workflow executions if this workflow execution is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout.
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
  , lambdaRole :: Core.Maybe Types.Arn
    -- ^ The IAM role attached to the child workflow execution.
  , taskPriority :: Core.Maybe Types.TaskPriority
    -- ^ The priority assigned to decision tasks for this workflow execution. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowExecutionConfiguration' value with any optional fields omitted.
mkWorkflowExecutionConfiguration
    :: Types.DurationInSeconds -- ^ 'taskStartToCloseTimeout'
    -> Types.DurationInSeconds -- ^ 'executionStartToCloseTimeout'
    -> Types.TaskList -- ^ 'taskList'
    -> Types.ChildPolicy -- ^ 'childPolicy'
    -> WorkflowExecutionConfiguration
mkWorkflowExecutionConfiguration taskStartToCloseTimeout
  executionStartToCloseTimeout taskList childPolicy
  = WorkflowExecutionConfiguration'{taskStartToCloseTimeout,
                                    executionStartToCloseTimeout, taskList, childPolicy,
                                    lambdaRole = Core.Nothing, taskPriority = Core.Nothing}

-- | The maximum duration allowed for decision tasks for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'taskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecTaskStartToCloseTimeout :: Lens.Lens' WorkflowExecutionConfiguration Types.DurationInSeconds
wecTaskStartToCloseTimeout = Lens.field @"taskStartToCloseTimeout"
{-# INLINEABLE wecTaskStartToCloseTimeout #-}
{-# DEPRECATED taskStartToCloseTimeout "Use generic-lens or generic-optics with 'taskStartToCloseTimeout' instead"  #-}

-- | The total duration for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'executionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecExecutionStartToCloseTimeout :: Lens.Lens' WorkflowExecutionConfiguration Types.DurationInSeconds
wecExecutionStartToCloseTimeout = Lens.field @"executionStartToCloseTimeout"
{-# INLINEABLE wecExecutionStartToCloseTimeout #-}
{-# DEPRECATED executionStartToCloseTimeout "Use generic-lens or generic-optics with 'executionStartToCloseTimeout' instead"  #-}

-- | The task list used for the decision tasks generated for this workflow execution.
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecTaskList :: Lens.Lens' WorkflowExecutionConfiguration Types.TaskList
wecTaskList = Lens.field @"taskList"
{-# INLINEABLE wecTaskList #-}
{-# DEPRECATED taskList "Use generic-lens or generic-optics with 'taskList' instead"  #-}

-- | The policy to use for the child workflow executions if this workflow execution is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout.
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
-- /Note:/ Consider using 'childPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecChildPolicy :: Lens.Lens' WorkflowExecutionConfiguration Types.ChildPolicy
wecChildPolicy = Lens.field @"childPolicy"
{-# INLINEABLE wecChildPolicy #-}
{-# DEPRECATED childPolicy "Use generic-lens or generic-optics with 'childPolicy' instead"  #-}

-- | The IAM role attached to the child workflow execution.
--
-- /Note:/ Consider using 'lambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecLambdaRole :: Lens.Lens' WorkflowExecutionConfiguration (Core.Maybe Types.Arn)
wecLambdaRole = Lens.field @"lambdaRole"
{-# INLINEABLE wecLambdaRole #-}
{-# DEPRECATED lambdaRole "Use generic-lens or generic-optics with 'lambdaRole' instead"  #-}

-- | The priority assigned to decision tasks for this workflow execution. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecTaskPriority :: Lens.Lens' WorkflowExecutionConfiguration (Core.Maybe Types.TaskPriority)
wecTaskPriority = Lens.field @"taskPriority"
{-# INLINEABLE wecTaskPriority #-}
{-# DEPRECATED taskPriority "Use generic-lens or generic-optics with 'taskPriority' instead"  #-}

instance Core.FromJSON WorkflowExecutionConfiguration where
        parseJSON
          = Core.withObject "WorkflowExecutionConfiguration" Core.$
              \ x ->
                WorkflowExecutionConfiguration' Core.<$>
                  (x Core..: "taskStartToCloseTimeout") Core.<*>
                    x Core..: "executionStartToCloseTimeout"
                    Core.<*> x Core..: "taskList"
                    Core.<*> x Core..: "childPolicy"
                    Core.<*> x Core..:? "lambdaRole"
                    Core.<*> x Core..:? "taskPriority"
