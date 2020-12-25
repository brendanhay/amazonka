{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionDecisionAttributes
  ( ContinueAsNewWorkflowExecutionDecisionAttributes (..),

    -- * Smart constructor
    mkContinueAsNewWorkflowExecutionDecisionAttributes,

    -- * Lenses
    canwedaChildPolicy,
    canwedaExecutionStartToCloseTimeout,
    canwedaInput,
    canwedaLambdaRole,
    canwedaTagList,
    canwedaTaskList,
    canwedaTaskPriority,
    canwedaTaskStartToCloseTimeout,
    canwedaWorkflowTypeVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Arn as Types
import qualified Network.AWS.SWF.Types.ChildPolicy as Types
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.DurationInSecondsOptional as Types
import qualified Network.AWS.SWF.Types.Tag as Types
import qualified Network.AWS.SWF.Types.TaskList as Types
import qualified Network.AWS.SWF.Types.TaskPriority as Types
import qualified Network.AWS.SWF.Types.Version as Types

-- | Provides the details of the @ContinueAsNewWorkflowExecution@ decision.
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
--     * @tag@ – A tag used to identify the workflow execution
--
--
--     * @taskList@ – String constraint. The key is @swf:taskList.name@ .
--
--
--     * @workflowType.version@ – String constraint. The key is @swf:workflowType.version@ .
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
-- /See:/ 'mkContinueAsNewWorkflowExecutionDecisionAttributes' smart constructor.
data ContinueAsNewWorkflowExecutionDecisionAttributes = ContinueAsNewWorkflowExecutionDecisionAttributes'
  { -- | If set, specifies the policy to use for the child workflow executions of the new execution if it is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' .
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
    childPolicy :: Core.Maybe Types.ChildPolicy,
    -- | If set, specifies the total duration for this workflow execution. This overrides the @defaultExecutionStartToCloseTimeout@ specified when registering the workflow type.
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    executionStartToCloseTimeout :: Core.Maybe Types.DurationInSecondsOptional,
    -- | The input provided to the new workflow execution.
    input :: Core.Maybe Types.Data,
    -- | The IAM role to attach to the new (continued) execution.
    lambdaRole :: Core.Maybe Types.Arn,
    -- | The list of tags to associate with the new workflow execution. A maximum of 5 tags can be specified. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
    tagList :: Core.Maybe [Types.Tag],
    -- | The task list to use for the decisions of the new (continued) workflow execution.
    taskList :: Core.Maybe Types.TaskList,
    -- | The task priority that, if set, specifies the priority for the decision tasks for this workflow execution. This overrides the defaultTaskPriority specified when registering the workflow type. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
    --
    -- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
    taskPriority :: Core.Maybe Types.TaskPriority,
    -- | Specifies the maximum duration of decision tasks for the new workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' .
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    taskStartToCloseTimeout :: Core.Maybe Types.DurationInSecondsOptional,
    -- | The version of the workflow to start.
    workflowTypeVersion :: Core.Maybe Types.Version
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContinueAsNewWorkflowExecutionDecisionAttributes' value with any optional fields omitted.
mkContinueAsNewWorkflowExecutionDecisionAttributes ::
  ContinueAsNewWorkflowExecutionDecisionAttributes
mkContinueAsNewWorkflowExecutionDecisionAttributes =
  ContinueAsNewWorkflowExecutionDecisionAttributes'
    { childPolicy =
        Core.Nothing,
      executionStartToCloseTimeout = Core.Nothing,
      input = Core.Nothing,
      lambdaRole = Core.Nothing,
      tagList = Core.Nothing,
      taskList = Core.Nothing,
      taskPriority = Core.Nothing,
      taskStartToCloseTimeout = Core.Nothing,
      workflowTypeVersion = Core.Nothing
    }

-- | If set, specifies the policy to use for the child workflow executions of the new execution if it is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' .
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
canwedaChildPolicy :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Core.Maybe Types.ChildPolicy)
canwedaChildPolicy = Lens.field @"childPolicy"
{-# DEPRECATED canwedaChildPolicy "Use generic-lens or generic-optics with 'childPolicy' instead." #-}

-- | If set, specifies the total duration for this workflow execution. This overrides the @defaultExecutionStartToCloseTimeout@ specified when registering the workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'executionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaExecutionStartToCloseTimeout :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Core.Maybe Types.DurationInSecondsOptional)
canwedaExecutionStartToCloseTimeout = Lens.field @"executionStartToCloseTimeout"
{-# DEPRECATED canwedaExecutionStartToCloseTimeout "Use generic-lens or generic-optics with 'executionStartToCloseTimeout' instead." #-}

-- | The input provided to the new workflow execution.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaInput :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Core.Maybe Types.Data)
canwedaInput = Lens.field @"input"
{-# DEPRECATED canwedaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The IAM role to attach to the new (continued) execution.
--
-- /Note:/ Consider using 'lambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaLambdaRole :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Core.Maybe Types.Arn)
canwedaLambdaRole = Lens.field @"lambdaRole"
{-# DEPRECATED canwedaLambdaRole "Use generic-lens or generic-optics with 'lambdaRole' instead." #-}

-- | The list of tags to associate with the new workflow execution. A maximum of 5 tags can be specified. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaTagList :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Core.Maybe [Types.Tag])
canwedaTagList = Lens.field @"tagList"
{-# DEPRECATED canwedaTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The task list to use for the decisions of the new (continued) workflow execution.
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaTaskList :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Core.Maybe Types.TaskList)
canwedaTaskList = Lens.field @"taskList"
{-# DEPRECATED canwedaTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

-- | The task priority that, if set, specifies the priority for the decision tasks for this workflow execution. This overrides the defaultTaskPriority specified when registering the workflow type. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaTaskPriority :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Core.Maybe Types.TaskPriority)
canwedaTaskPriority = Lens.field @"taskPriority"
{-# DEPRECATED canwedaTaskPriority "Use generic-lens or generic-optics with 'taskPriority' instead." #-}

-- | Specifies the maximum duration of decision tasks for the new workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'taskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaTaskStartToCloseTimeout :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Core.Maybe Types.DurationInSecondsOptional)
canwedaTaskStartToCloseTimeout = Lens.field @"taskStartToCloseTimeout"
{-# DEPRECATED canwedaTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'taskStartToCloseTimeout' instead." #-}

-- | The version of the workflow to start.
--
-- /Note:/ Consider using 'workflowTypeVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaWorkflowTypeVersion :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Core.Maybe Types.Version)
canwedaWorkflowTypeVersion = Lens.field @"workflowTypeVersion"
{-# DEPRECATED canwedaWorkflowTypeVersion "Use generic-lens or generic-optics with 'workflowTypeVersion' instead." #-}

instance
  Core.FromJSON
    ContinueAsNewWorkflowExecutionDecisionAttributes
  where
  toJSON ContinueAsNewWorkflowExecutionDecisionAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ ("childPolicy" Core..=) Core.<$> childPolicy,
            ("executionStartToCloseTimeout" Core..=)
              Core.<$> executionStartToCloseTimeout,
            ("input" Core..=) Core.<$> input,
            ("lambdaRole" Core..=) Core.<$> lambdaRole,
            ("tagList" Core..=) Core.<$> tagList,
            ("taskList" Core..=) Core.<$> taskList,
            ("taskPriority" Core..=) Core.<$> taskPriority,
            ("taskStartToCloseTimeout" Core..=)
              Core.<$> taskStartToCloseTimeout,
            ("workflowTypeVersion" Core..=) Core.<$> workflowTypeVersion
          ]
      )
