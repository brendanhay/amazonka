{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.StartWorkflowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an execution of the workflow type in the specified domain using the provided @workflowId@ and input data.
--
-- This action returns the newly started workflow execution.
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @tagList.member.0@ : The key is @swf:tagList.member.0@ .
--
--
--     * @tagList.member.1@ : The key is @swf:tagList.member.1@ .
--
--
--     * @tagList.member.2@ : The key is @swf:tagList.member.2@ .
--
--
--     * @tagList.member.3@ : The key is @swf:tagList.member.3@ .
--
--
--     * @tagList.member.4@ : The key is @swf:tagList.member.4@ .
--
--
--     * @taskList@ : String constraint. The key is @swf:taskList.name@ .
--
--
--     * @workflowType.name@ : String constraint. The key is @swf:workflowType.name@ .
--
--
--     * @workflowType.version@ : String constraint. The key is @swf:workflowType.version@ .
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.StartWorkflowExecution
  ( -- * Creating a request
    StartWorkflowExecution (..),
    mkStartWorkflowExecution,

    -- ** Request lenses
    sDomain,
    sWorkflowId,
    sWorkflowType,
    sChildPolicy,
    sExecutionStartToCloseTimeout,
    sInput,
    sLambdaRole,
    sTagList,
    sTaskList,
    sTaskPriority,
    sTaskStartToCloseTimeout,

    -- * Destructuring the response
    StartWorkflowExecutionResponse (..),
    mkStartWorkflowExecutionResponse,

    -- ** Response lenses
    swerrsRunId,
    swerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkStartWorkflowExecution' smart constructor.
data StartWorkflowExecution = StartWorkflowExecution'
  { -- | The name of the domain in which the workflow execution is created.
    domain :: Types.DomainName,
    -- | The user defined identifier associated with the workflow execution. You can use this to associate a custom identifier with the workflow execution. You may specify the same identifier if a workflow execution is logically a /restart/ of a previous execution. You cannot have two open workflow executions with the same @workflowId@ at the same time within the same domain.
    --
    -- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
    workflowId :: Types.WorkflowId,
    -- | The type of the workflow to start.
    workflowType :: Types.WorkflowType,
    -- | If set, specifies the policy to use for the child workflow executions of this workflow execution if it is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' .
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
    -- | The total duration for this workflow execution. This overrides the defaultExecutionStartToCloseTimeout specified when registering the workflow type.
    --
    -- The duration is specified in seconds; an integer greater than or equal to @0@ . Exceeding this limit causes the workflow execution to time out. Unlike some of the other timeout parameters in Amazon SWF, you cannot specify a value of "NONE" for this timeout; there is a one-year max limit on the time that a workflow execution can run.
    executionStartToCloseTimeout :: Core.Maybe Types.DurationInSecondsOptional,
    -- | The input for the workflow execution. This is a free form string which should be meaningful to the workflow you are starting. This @input@ is made available to the new workflow execution in the @WorkflowExecutionStarted@ history event.
    input :: Core.Maybe Types.Data,
    -- | The IAM role to attach to this workflow execution.
    lambdaRole :: Core.Maybe Types.Arn,
    -- | The list of tags to associate with the workflow execution. You can specify a maximum of 5 tags. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
    tagList :: Core.Maybe [Types.Tag],
    -- | The task list to use for the decision tasks generated for this workflow execution. This overrides the @defaultTaskList@ specified when registering the workflow type.
    --
    -- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
    taskList :: Core.Maybe Types.TaskList,
    -- | The task priority to use for this workflow execution. This overrides any default priority that was assigned when the workflow type was registered. If not set, then the default task priority for the workflow type is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
    --
    -- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
    taskPriority :: Core.Maybe Types.TaskPriority,
    -- | Specifies the maximum duration of decision tasks for this workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' .
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    taskStartToCloseTimeout :: Core.Maybe Types.DurationInSecondsOptional
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartWorkflowExecution' value with any optional fields omitted.
mkStartWorkflowExecution ::
  -- | 'domain'
  Types.DomainName ->
  -- | 'workflowId'
  Types.WorkflowId ->
  -- | 'workflowType'
  Types.WorkflowType ->
  StartWorkflowExecution
mkStartWorkflowExecution domain workflowId workflowType =
  StartWorkflowExecution'
    { domain,
      workflowId,
      workflowType,
      childPolicy = Core.Nothing,
      executionStartToCloseTimeout = Core.Nothing,
      input = Core.Nothing,
      lambdaRole = Core.Nothing,
      tagList = Core.Nothing,
      taskList = Core.Nothing,
      taskPriority = Core.Nothing,
      taskStartToCloseTimeout = Core.Nothing
    }

-- | The name of the domain in which the workflow execution is created.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDomain :: Lens.Lens' StartWorkflowExecution Types.DomainName
sDomain = Lens.field @"domain"
{-# DEPRECATED sDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The user defined identifier associated with the workflow execution. You can use this to associate a custom identifier with the workflow execution. You may specify the same identifier if a workflow execution is logically a /restart/ of a previous execution. You cannot have two open workflow executions with the same @workflowId@ at the same time within the same domain.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sWorkflowId :: Lens.Lens' StartWorkflowExecution Types.WorkflowId
sWorkflowId = Lens.field @"workflowId"
{-# DEPRECATED sWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

-- | The type of the workflow to start.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sWorkflowType :: Lens.Lens' StartWorkflowExecution Types.WorkflowType
sWorkflowType = Lens.field @"workflowType"
{-# DEPRECATED sWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | If set, specifies the policy to use for the child workflow executions of this workflow execution if it is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' .
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
sChildPolicy :: Lens.Lens' StartWorkflowExecution (Core.Maybe Types.ChildPolicy)
sChildPolicy = Lens.field @"childPolicy"
{-# DEPRECATED sChildPolicy "Use generic-lens or generic-optics with 'childPolicy' instead." #-}

-- | The total duration for this workflow execution. This overrides the defaultExecutionStartToCloseTimeout specified when registering the workflow type.
--
-- The duration is specified in seconds; an integer greater than or equal to @0@ . Exceeding this limit causes the workflow execution to time out. Unlike some of the other timeout parameters in Amazon SWF, you cannot specify a value of "NONE" for this timeout; there is a one-year max limit on the time that a workflow execution can run.
--
-- /Note:/ Consider using 'executionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sExecutionStartToCloseTimeout :: Lens.Lens' StartWorkflowExecution (Core.Maybe Types.DurationInSecondsOptional)
sExecutionStartToCloseTimeout = Lens.field @"executionStartToCloseTimeout"
{-# DEPRECATED sExecutionStartToCloseTimeout "Use generic-lens or generic-optics with 'executionStartToCloseTimeout' instead." #-}

-- | The input for the workflow execution. This is a free form string which should be meaningful to the workflow you are starting. This @input@ is made available to the new workflow execution in the @WorkflowExecutionStarted@ history event.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInput :: Lens.Lens' StartWorkflowExecution (Core.Maybe Types.Data)
sInput = Lens.field @"input"
{-# DEPRECATED sInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The IAM role to attach to this workflow execution.
--
-- /Note:/ Consider using 'lambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLambdaRole :: Lens.Lens' StartWorkflowExecution (Core.Maybe Types.Arn)
sLambdaRole = Lens.field @"lambdaRole"
{-# DEPRECATED sLambdaRole "Use generic-lens or generic-optics with 'lambdaRole' instead." #-}

-- | The list of tags to associate with the workflow execution. You can specify a maximum of 5 tags. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTagList :: Lens.Lens' StartWorkflowExecution (Core.Maybe [Types.Tag])
sTagList = Lens.field @"tagList"
{-# DEPRECATED sTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The task list to use for the decision tasks generated for this workflow execution. This overrides the @defaultTaskList@ specified when registering the workflow type.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTaskList :: Lens.Lens' StartWorkflowExecution (Core.Maybe Types.TaskList)
sTaskList = Lens.field @"taskList"
{-# DEPRECATED sTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

-- | The task priority to use for this workflow execution. This overrides any default priority that was assigned when the workflow type was registered. If not set, then the default task priority for the workflow type is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTaskPriority :: Lens.Lens' StartWorkflowExecution (Core.Maybe Types.TaskPriority)
sTaskPriority = Lens.field @"taskPriority"
{-# DEPRECATED sTaskPriority "Use generic-lens or generic-optics with 'taskPriority' instead." #-}

-- | Specifies the maximum duration of decision tasks for this workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'taskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTaskStartToCloseTimeout :: Lens.Lens' StartWorkflowExecution (Core.Maybe Types.DurationInSecondsOptional)
sTaskStartToCloseTimeout = Lens.field @"taskStartToCloseTimeout"
{-# DEPRECATED sTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'taskStartToCloseTimeout' instead." #-}

instance Core.FromJSON StartWorkflowExecution where
  toJSON StartWorkflowExecution {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domain" Core..= domain),
            Core.Just ("workflowId" Core..= workflowId),
            Core.Just ("workflowType" Core..= workflowType),
            ("childPolicy" Core..=) Core.<$> childPolicy,
            ("executionStartToCloseTimeout" Core..=)
              Core.<$> executionStartToCloseTimeout,
            ("input" Core..=) Core.<$> input,
            ("lambdaRole" Core..=) Core.<$> lambdaRole,
            ("tagList" Core..=) Core.<$> tagList,
            ("taskList" Core..=) Core.<$> taskList,
            ("taskPriority" Core..=) Core.<$> taskPriority,
            ("taskStartToCloseTimeout" Core..=)
              Core.<$> taskStartToCloseTimeout
          ]
      )

instance Core.AWSRequest StartWorkflowExecution where
  type Rs StartWorkflowExecution = StartWorkflowExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "SimpleWorkflowService.StartWorkflowExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartWorkflowExecutionResponse'
            Core.<$> (x Core..:? "runId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Specifies the @runId@ of a workflow execution.
--
-- /See:/ 'mkStartWorkflowExecutionResponse' smart constructor.
data StartWorkflowExecutionResponse = StartWorkflowExecutionResponse'
  { -- | The @runId@ of a workflow execution. This ID is generated by the service and can be used to uniquely identify the workflow execution within a domain.
    runId :: Core.Maybe Types.RunId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartWorkflowExecutionResponse' value with any optional fields omitted.
mkStartWorkflowExecutionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartWorkflowExecutionResponse
mkStartWorkflowExecutionResponse responseStatus =
  StartWorkflowExecutionResponse'
    { runId = Core.Nothing,
      responseStatus
    }

-- | The @runId@ of a workflow execution. This ID is generated by the service and can be used to uniquely identify the workflow execution within a domain.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swerrsRunId :: Lens.Lens' StartWorkflowExecutionResponse (Core.Maybe Types.RunId)
swerrsRunId = Lens.field @"runId"
{-# DEPRECATED swerrsRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swerrsResponseStatus :: Lens.Lens' StartWorkflowExecutionResponse Core.Int
swerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED swerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
