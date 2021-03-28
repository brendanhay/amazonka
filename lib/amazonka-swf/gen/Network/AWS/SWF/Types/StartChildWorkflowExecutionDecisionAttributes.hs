{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartChildWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.StartChildWorkflowExecutionDecisionAttributes
  ( StartChildWorkflowExecutionDecisionAttributes (..)
  -- * Smart constructor
  , mkStartChildWorkflowExecutionDecisionAttributes
  -- * Lenses
  , scwedaWorkflowType
  , scwedaWorkflowId
  , scwedaChildPolicy
  , scwedaControl
  , scwedaExecutionStartToCloseTimeout
  , scwedaInput
  , scwedaLambdaRole
  , scwedaTagList
  , scwedaTaskList
  , scwedaTaskPriority
  , scwedaTaskStartToCloseTimeout
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Arn as Types
import qualified Network.AWS.SWF.Types.ChildPolicy as Types
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.ExecutionStartToCloseTimeout as Types
import qualified Network.AWS.SWF.Types.Tag as Types
import qualified Network.AWS.SWF.Types.TaskList as Types
import qualified Network.AWS.SWF.Types.TaskPriority as Types
import qualified Network.AWS.SWF.Types.TaskStartToCloseTimeout as Types
import qualified Network.AWS.SWF.Types.WorkflowId as Types
import qualified Network.AWS.SWF.Types.WorkflowType as Types

-- | Provides the details of the @StartChildWorkflowExecution@ decision.
--
-- __Access Control__ 
-- You can use IAM policies to control this decision's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @tagList.member.N@ – The key is "swf:tagList.N" where N is the tag number from 0 to 4, inclusive.
--
--
--     * @taskList@ – String constraint. The key is @swf:taskList.name@ .
--
--
--     * @workflowType.name@ – String constraint. The key is @swf:workflowType.name@ .
--
--
--     * @workflowType.version@ – String constraint. The key is @swf:workflowType.version@ .
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
-- /See:/ 'mkStartChildWorkflowExecutionDecisionAttributes' smart constructor.
data StartChildWorkflowExecutionDecisionAttributes = StartChildWorkflowExecutionDecisionAttributes'
  { workflowType :: Types.WorkflowType
    -- ^ The type of the workflow execution to be started.
  , workflowId :: Types.WorkflowId
    -- ^ The @workflowId@ of the workflow execution.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
  , childPolicy :: Core.Maybe Types.ChildPolicy
    -- ^ If set, specifies the policy to use for the child workflow executions if the workflow execution being started is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' .
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
    -- ^ The data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the child workflow execution.
  , executionStartToCloseTimeout :: Core.Maybe Types.ExecutionStartToCloseTimeout
    -- ^ The total duration for this workflow execution. This overrides the defaultExecutionStartToCloseTimeout specified when registering the workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
  , input :: Core.Maybe Types.Data
    -- ^ The input to be provided to the workflow execution.
  , lambdaRole :: Core.Maybe Types.Arn
    -- ^ The IAM role attached to the child workflow execution.
  , tagList :: Core.Maybe [Types.Tag]
    -- ^ The list of tags to associate with the child workflow execution. A maximum of 5 tags can be specified. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
  , taskList :: Core.Maybe Types.TaskList
    -- ^ The name of the task list to be used for decision tasks of the child workflow execution.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
  , taskPriority :: Core.Maybe Types.TaskPriority
    -- ^ A task priority that, if set, specifies the priority for a decision task of this workflow execution. This overrides the defaultTaskPriority specified when registering the workflow type. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
  , taskStartToCloseTimeout :: Core.Maybe Types.TaskStartToCloseTimeout
    -- ^ Specifies the maximum duration of decision tasks for this workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartChildWorkflowExecutionDecisionAttributes' value with any optional fields omitted.
mkStartChildWorkflowExecutionDecisionAttributes
    :: Types.WorkflowType -- ^ 'workflowType'
    -> Types.WorkflowId -- ^ 'workflowId'
    -> StartChildWorkflowExecutionDecisionAttributes
mkStartChildWorkflowExecutionDecisionAttributes workflowType
  workflowId
  = StartChildWorkflowExecutionDecisionAttributes'{workflowType,
                                                   workflowId, childPolicy = Core.Nothing,
                                                   control = Core.Nothing,
                                                   executionStartToCloseTimeout = Core.Nothing,
                                                   input = Core.Nothing, lambdaRole = Core.Nothing,
                                                   tagList = Core.Nothing, taskList = Core.Nothing,
                                                   taskPriority = Core.Nothing,
                                                   taskStartToCloseTimeout = Core.Nothing}

-- | The type of the workflow execution to be started.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaWorkflowType :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes Types.WorkflowType
scwedaWorkflowType = Lens.field @"workflowType"
{-# INLINEABLE scwedaWorkflowType #-}
{-# DEPRECATED workflowType "Use generic-lens or generic-optics with 'workflowType' instead"  #-}

-- | The @workflowId@ of the workflow execution.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaWorkflowId :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes Types.WorkflowId
scwedaWorkflowId = Lens.field @"workflowId"
{-# INLINEABLE scwedaWorkflowId #-}
{-# DEPRECATED workflowId "Use generic-lens or generic-optics with 'workflowId' instead"  #-}

-- | If set, specifies the policy to use for the child workflow executions if the workflow execution being started is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' .
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
scwedaChildPolicy :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Core.Maybe Types.ChildPolicy)
scwedaChildPolicy = Lens.field @"childPolicy"
{-# INLINEABLE scwedaChildPolicy #-}
{-# DEPRECATED childPolicy "Use generic-lens or generic-optics with 'childPolicy' instead"  #-}

-- | The data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the child workflow execution.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaControl :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Core.Maybe Types.Data)
scwedaControl = Lens.field @"control"
{-# INLINEABLE scwedaControl #-}
{-# DEPRECATED control "Use generic-lens or generic-optics with 'control' instead"  #-}

-- | The total duration for this workflow execution. This overrides the defaultExecutionStartToCloseTimeout specified when registering the workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'executionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaExecutionStartToCloseTimeout :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Core.Maybe Types.ExecutionStartToCloseTimeout)
scwedaExecutionStartToCloseTimeout = Lens.field @"executionStartToCloseTimeout"
{-# INLINEABLE scwedaExecutionStartToCloseTimeout #-}
{-# DEPRECATED executionStartToCloseTimeout "Use generic-lens or generic-optics with 'executionStartToCloseTimeout' instead"  #-}

-- | The input to be provided to the workflow execution.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaInput :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Core.Maybe Types.Data)
scwedaInput = Lens.field @"input"
{-# INLINEABLE scwedaInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

-- | The IAM role attached to the child workflow execution.
--
-- /Note:/ Consider using 'lambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaLambdaRole :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Core.Maybe Types.Arn)
scwedaLambdaRole = Lens.field @"lambdaRole"
{-# INLINEABLE scwedaLambdaRole #-}
{-# DEPRECATED lambdaRole "Use generic-lens or generic-optics with 'lambdaRole' instead"  #-}

-- | The list of tags to associate with the child workflow execution. A maximum of 5 tags can be specified. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaTagList :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Core.Maybe [Types.Tag])
scwedaTagList = Lens.field @"tagList"
{-# INLINEABLE scwedaTagList #-}
{-# DEPRECATED tagList "Use generic-lens or generic-optics with 'tagList' instead"  #-}

-- | The name of the task list to be used for decision tasks of the child workflow execution.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaTaskList :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Core.Maybe Types.TaskList)
scwedaTaskList = Lens.field @"taskList"
{-# INLINEABLE scwedaTaskList #-}
{-# DEPRECATED taskList "Use generic-lens or generic-optics with 'taskList' instead"  #-}

-- | A task priority that, if set, specifies the priority for a decision task of this workflow execution. This overrides the defaultTaskPriority specified when registering the workflow type. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaTaskPriority :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Core.Maybe Types.TaskPriority)
scwedaTaskPriority = Lens.field @"taskPriority"
{-# INLINEABLE scwedaTaskPriority #-}
{-# DEPRECATED taskPriority "Use generic-lens or generic-optics with 'taskPriority' instead"  #-}

-- | Specifies the maximum duration of decision tasks for this workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'taskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaTaskStartToCloseTimeout :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Core.Maybe Types.TaskStartToCloseTimeout)
scwedaTaskStartToCloseTimeout = Lens.field @"taskStartToCloseTimeout"
{-# INLINEABLE scwedaTaskStartToCloseTimeout #-}
{-# DEPRECATED taskStartToCloseTimeout "Use generic-lens or generic-optics with 'taskStartToCloseTimeout' instead"  #-}

instance Core.FromJSON
           StartChildWorkflowExecutionDecisionAttributes
         where
        toJSON StartChildWorkflowExecutionDecisionAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("workflowType" Core..= workflowType),
                  Core.Just ("workflowId" Core..= workflowId),
                  ("childPolicy" Core..=) Core.<$> childPolicy,
                  ("control" Core..=) Core.<$> control,
                  ("executionStartToCloseTimeout" Core..=) Core.<$>
                    executionStartToCloseTimeout,
                  ("input" Core..=) Core.<$> input,
                  ("lambdaRole" Core..=) Core.<$> lambdaRole,
                  ("tagList" Core..=) Core.<$> tagList,
                  ("taskList" Core..=) Core.<$> taskList,
                  ("taskPriority" Core..=) Core.<$> taskPriority,
                  ("taskStartToCloseTimeout" Core..=) Core.<$>
                    taskStartToCloseTimeout])
