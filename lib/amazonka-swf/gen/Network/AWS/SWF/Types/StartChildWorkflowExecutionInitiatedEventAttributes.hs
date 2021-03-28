{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartChildWorkflowExecutionInitiatedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.StartChildWorkflowExecutionInitiatedEventAttributes
  ( StartChildWorkflowExecutionInitiatedEventAttributes (..)
  -- * Smart constructor
  , mkStartChildWorkflowExecutionInitiatedEventAttributes
  -- * Lenses
  , scweieaWorkflowId
  , scweieaWorkflowType
  , scweieaTaskList
  , scweieaDecisionTaskCompletedEventId
  , scweieaChildPolicy
  , scweieaControl
  , scweieaExecutionStartToCloseTimeout
  , scweieaInput
  , scweieaLambdaRole
  , scweieaTagList
  , scweieaTaskPriority
  , scweieaTaskStartToCloseTimeout
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Arn as Types
import qualified Network.AWS.SWF.Types.ChildPolicy as Types
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.DurationInSecondsOptional as Types
import qualified Network.AWS.SWF.Types.Tag as Types
import qualified Network.AWS.SWF.Types.TaskList as Types
import qualified Network.AWS.SWF.Types.TaskPriority as Types
import qualified Network.AWS.SWF.Types.WorkflowId as Types
import qualified Network.AWS.SWF.Types.WorkflowType as Types

-- | Provides the details of the @StartChildWorkflowExecutionInitiated@ event.
--
-- /See:/ 'mkStartChildWorkflowExecutionInitiatedEventAttributes' smart constructor.
data StartChildWorkflowExecutionInitiatedEventAttributes = StartChildWorkflowExecutionInitiatedEventAttributes'
  { workflowId :: Types.WorkflowId
    -- ^ The @workflowId@ of the child workflow execution.
  , workflowType :: Types.WorkflowType
    -- ^ The type of the child workflow execution.
  , taskList :: Types.TaskList
    -- ^ The name of the task list used for the decision tasks of the child workflow execution.
  , decisionTaskCompletedEventId :: Core.Integer
    -- ^ The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @StartChildWorkflowExecution@ 'Decision' to request this child workflow execution. This information can be useful for diagnosing problems by tracing back the cause of events.
  , childPolicy :: Types.ChildPolicy
    -- ^ The policy to use for the child workflow executions if this execution gets terminated by explicitly calling the 'TerminateWorkflowExecution' action or due to an expired timeout.
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
  , control :: Core.Maybe Types.Data
    -- ^ Data attached to the event that can be used by the decider in subsequent decision tasks. This data isn't sent to the activity.
  , executionStartToCloseTimeout :: Core.Maybe Types.DurationInSecondsOptional
    -- ^ The maximum duration for the child workflow execution. If the workflow execution isn't closed within this duration, it is timed out and force-terminated.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
  , input :: Core.Maybe Types.Data
    -- ^ The inputs provided to the child workflow execution.
  , lambdaRole :: Core.Maybe Types.Arn
    -- ^ The IAM role to attach to the child workflow execution.
  , tagList :: Core.Maybe [Types.Tag]
    -- ^ The list of tags to associated with the child workflow execution.
  , taskPriority :: Core.Maybe Types.TaskPriority
    -- ^ The priority assigned for the decision tasks for this workflow execution. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
  , taskStartToCloseTimeout :: Core.Maybe Types.DurationInSecondsOptional
    -- ^ The maximum duration allowed for the decision tasks for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartChildWorkflowExecutionInitiatedEventAttributes' value with any optional fields omitted.
mkStartChildWorkflowExecutionInitiatedEventAttributes
    :: Types.WorkflowId -- ^ 'workflowId'
    -> Types.WorkflowType -- ^ 'workflowType'
    -> Types.TaskList -- ^ 'taskList'
    -> Core.Integer -- ^ 'decisionTaskCompletedEventId'
    -> Types.ChildPolicy -- ^ 'childPolicy'
    -> StartChildWorkflowExecutionInitiatedEventAttributes
mkStartChildWorkflowExecutionInitiatedEventAttributes workflowId
  workflowType taskList decisionTaskCompletedEventId childPolicy
  = StartChildWorkflowExecutionInitiatedEventAttributes'{workflowId,
                                                         workflowType, taskList,
                                                         decisionTaskCompletedEventId, childPolicy,
                                                         control = Core.Nothing,
                                                         executionStartToCloseTimeout =
                                                           Core.Nothing,
                                                         input = Core.Nothing,
                                                         lambdaRole = Core.Nothing,
                                                         tagList = Core.Nothing,
                                                         taskPriority = Core.Nothing,
                                                         taskStartToCloseTimeout = Core.Nothing}

-- | The @workflowId@ of the child workflow execution.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaWorkflowId :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes Types.WorkflowId
scweieaWorkflowId = Lens.field @"workflowId"
{-# INLINEABLE scweieaWorkflowId #-}
{-# DEPRECATED workflowId "Use generic-lens or generic-optics with 'workflowId' instead"  #-}

-- | The type of the child workflow execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaWorkflowType :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes Types.WorkflowType
scweieaWorkflowType = Lens.field @"workflowType"
{-# INLINEABLE scweieaWorkflowType #-}
{-# DEPRECATED workflowType "Use generic-lens or generic-optics with 'workflowType' instead"  #-}

-- | The name of the task list used for the decision tasks of the child workflow execution.
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaTaskList :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes Types.TaskList
scweieaTaskList = Lens.field @"taskList"
{-# INLINEABLE scweieaTaskList #-}
{-# DEPRECATED taskList "Use generic-lens or generic-optics with 'taskList' instead"  #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @StartChildWorkflowExecution@ 'Decision' to request this child workflow execution. This information can be useful for diagnosing problems by tracing back the cause of events.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaDecisionTaskCompletedEventId :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes Core.Integer
scweieaDecisionTaskCompletedEventId = Lens.field @"decisionTaskCompletedEventId"
{-# INLINEABLE scweieaDecisionTaskCompletedEventId #-}
{-# DEPRECATED decisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead"  #-}

-- | The policy to use for the child workflow executions if this execution gets terminated by explicitly calling the 'TerminateWorkflowExecution' action or due to an expired timeout.
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
scweieaChildPolicy :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes Types.ChildPolicy
scweieaChildPolicy = Lens.field @"childPolicy"
{-# INLINEABLE scweieaChildPolicy #-}
{-# DEPRECATED childPolicy "Use generic-lens or generic-optics with 'childPolicy' instead"  #-}

-- | Data attached to the event that can be used by the decider in subsequent decision tasks. This data isn't sent to the activity.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaControl :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Core.Maybe Types.Data)
scweieaControl = Lens.field @"control"
{-# INLINEABLE scweieaControl #-}
{-# DEPRECATED control "Use generic-lens or generic-optics with 'control' instead"  #-}

-- | The maximum duration for the child workflow execution. If the workflow execution isn't closed within this duration, it is timed out and force-terminated.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'executionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaExecutionStartToCloseTimeout :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Core.Maybe Types.DurationInSecondsOptional)
scweieaExecutionStartToCloseTimeout = Lens.field @"executionStartToCloseTimeout"
{-# INLINEABLE scweieaExecutionStartToCloseTimeout #-}
{-# DEPRECATED executionStartToCloseTimeout "Use generic-lens or generic-optics with 'executionStartToCloseTimeout' instead"  #-}

-- | The inputs provided to the child workflow execution.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaInput :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Core.Maybe Types.Data)
scweieaInput = Lens.field @"input"
{-# INLINEABLE scweieaInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

-- | The IAM role to attach to the child workflow execution.
--
-- /Note:/ Consider using 'lambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaLambdaRole :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Core.Maybe Types.Arn)
scweieaLambdaRole = Lens.field @"lambdaRole"
{-# INLINEABLE scweieaLambdaRole #-}
{-# DEPRECATED lambdaRole "Use generic-lens or generic-optics with 'lambdaRole' instead"  #-}

-- | The list of tags to associated with the child workflow execution.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaTagList :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Core.Maybe [Types.Tag])
scweieaTagList = Lens.field @"tagList"
{-# INLINEABLE scweieaTagList #-}
{-# DEPRECATED tagList "Use generic-lens or generic-optics with 'tagList' instead"  #-}

-- | The priority assigned for the decision tasks for this workflow execution. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaTaskPriority :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Core.Maybe Types.TaskPriority)
scweieaTaskPriority = Lens.field @"taskPriority"
{-# INLINEABLE scweieaTaskPriority #-}
{-# DEPRECATED taskPriority "Use generic-lens or generic-optics with 'taskPriority' instead"  #-}

-- | The maximum duration allowed for the decision tasks for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'taskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaTaskStartToCloseTimeout :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Core.Maybe Types.DurationInSecondsOptional)
scweieaTaskStartToCloseTimeout = Lens.field @"taskStartToCloseTimeout"
{-# INLINEABLE scweieaTaskStartToCloseTimeout #-}
{-# DEPRECATED taskStartToCloseTimeout "Use generic-lens or generic-optics with 'taskStartToCloseTimeout' instead"  #-}

instance Core.FromJSON
           StartChildWorkflowExecutionInitiatedEventAttributes
         where
        parseJSON
          = Core.withObject
              "StartChildWorkflowExecutionInitiatedEventAttributes"
              Core.$
              \ x ->
                StartChildWorkflowExecutionInitiatedEventAttributes' Core.<$>
                  (x Core..: "workflowId") Core.<*> x Core..: "workflowType" Core.<*>
                    x Core..: "taskList"
                    Core.<*> x Core..: "decisionTaskCompletedEventId"
                    Core.<*> x Core..: "childPolicy"
                    Core.<*> x Core..:? "control"
                    Core.<*> x Core..:? "executionStartToCloseTimeout"
                    Core.<*> x Core..:? "input"
                    Core.<*> x Core..:? "lambdaRole"
                    Core.<*> x Core..:? "tagList"
                    Core.<*> x Core..:? "taskPriority"
                    Core.<*> x Core..:? "taskStartToCloseTimeout"
