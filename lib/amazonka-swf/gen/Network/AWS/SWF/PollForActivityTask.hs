{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.PollForActivityTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by workers to get an 'ActivityTask' from the specified activity @taskList@ . This initiates a long poll, where the service holds the HTTP connection open and responds as soon as a task becomes available. The maximum time the service holds on to the request before responding is 60 seconds. If no task is available within 60 seconds, the poll returns an empty result. An empty result, in this context, means that an ActivityTask is returned, but that the value of taskToken is an empty string. If a task is returned, the worker should use its type to identify and process it correctly.
--
-- /Important:/ Workers should set their client side socket timeout to at least 70 seconds (10 seconds higher than the maximum time service may hold the poll request).
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * Constrain the @taskList.name@ parameter by using a @Condition@ element with the @swf:taskList.name@ key to allow the action to access only certain task lists.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.PollForActivityTask
  ( -- * Creating a request
    PollForActivityTask (..),
    mkPollForActivityTask,

    -- ** Request lenses
    pfatDomain,
    pfatTaskList,
    pfatIdentity,

    -- * Destructuring the response
    PollForActivityTaskResponse (..),
    mkPollForActivityTaskResponse,

    -- ** Response lenses
    pfatrrsTaskToken,
    pfatrrsActivityId,
    pfatrrsStartedEventId,
    pfatrrsWorkflowExecution,
    pfatrrsActivityType,
    pfatrrsInput,
    pfatrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkPollForActivityTask' smart constructor.
data PollForActivityTask = PollForActivityTask'
  { -- | The name of the domain that contains the task lists being polled.
    domain :: Types.Domain,
    -- | Specifies the task list to poll for activity tasks.
    --
    -- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
    taskList :: Types.TaskList,
    -- | Identity of the worker making the request, recorded in the @ActivityTaskStarted@ event in the workflow history. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
    identity :: Core.Maybe Types.Identity
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PollForActivityTask' value with any optional fields omitted.
mkPollForActivityTask ::
  -- | 'domain'
  Types.Domain ->
  -- | 'taskList'
  Types.TaskList ->
  PollForActivityTask
mkPollForActivityTask domain taskList =
  PollForActivityTask' {domain, taskList, identity = Core.Nothing}

-- | The name of the domain that contains the task lists being polled.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatDomain :: Lens.Lens' PollForActivityTask Types.Domain
pfatDomain = Lens.field @"domain"
{-# DEPRECATED pfatDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | Specifies the task list to poll for activity tasks.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatTaskList :: Lens.Lens' PollForActivityTask Types.TaskList
pfatTaskList = Lens.field @"taskList"
{-# DEPRECATED pfatTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

-- | Identity of the worker making the request, recorded in the @ActivityTaskStarted@ event in the workflow history. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatIdentity :: Lens.Lens' PollForActivityTask (Core.Maybe Types.Identity)
pfatIdentity = Lens.field @"identity"
{-# DEPRECATED pfatIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

instance Core.FromJSON PollForActivityTask where
  toJSON PollForActivityTask {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domain" Core..= domain),
            Core.Just ("taskList" Core..= taskList),
            ("identity" Core..=) Core.<$> identity
          ]
      )

instance Core.AWSRequest PollForActivityTask where
  type Rs PollForActivityTask = PollForActivityTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "SimpleWorkflowService.PollForActivityTask")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PollForActivityTaskResponse'
            Core.<$> (x Core..:? "taskToken")
            Core.<*> (x Core..:? "activityId")
            Core.<*> (x Core..: "startedEventId")
            Core.<*> (x Core..:? "workflowExecution")
            Core.<*> (x Core..:? "activityType")
            Core.<*> (x Core..:? "input")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Unit of work sent to an activity worker.
--
-- /See:/ 'mkPollForActivityTaskResponse' smart constructor.
data PollForActivityTaskResponse = PollForActivityTaskResponse'
  { -- | The opaque string used as a handle on the task. This token is used by workers to communicate progress and response information back to the system about the task.
    taskToken :: Core.Maybe Types.TaskToken,
    -- | The unique ID of the task.
    activityId :: Core.Maybe Types.ActivityId,
    -- | The ID of the @ActivityTaskStarted@ event recorded in the history.
    startedEventId :: Core.Integer,
    -- | The workflow execution that started this activity task.
    workflowExecution :: Core.Maybe Types.WorkflowExecution,
    -- | The type of this activity task.
    activityType :: Core.Maybe Types.ActivityType,
    -- | The inputs provided when the activity task was scheduled. The form of the input is user defined and should be meaningful to the activity implementation.
    input :: Core.Maybe Types.Data,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PollForActivityTaskResponse' value with any optional fields omitted.
mkPollForActivityTaskResponse ::
  -- | 'startedEventId'
  Core.Integer ->
  -- | 'responseStatus'
  Core.Int ->
  PollForActivityTaskResponse
mkPollForActivityTaskResponse startedEventId responseStatus =
  PollForActivityTaskResponse'
    { taskToken = Core.Nothing,
      activityId = Core.Nothing,
      startedEventId,
      workflowExecution = Core.Nothing,
      activityType = Core.Nothing,
      input = Core.Nothing,
      responseStatus
    }

-- | The opaque string used as a handle on the task. This token is used by workers to communicate progress and response information back to the system about the task.
--
-- /Note:/ Consider using 'taskToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatrrsTaskToken :: Lens.Lens' PollForActivityTaskResponse (Core.Maybe Types.TaskToken)
pfatrrsTaskToken = Lens.field @"taskToken"
{-# DEPRECATED pfatrrsTaskToken "Use generic-lens or generic-optics with 'taskToken' instead." #-}

-- | The unique ID of the task.
--
-- /Note:/ Consider using 'activityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatrrsActivityId :: Lens.Lens' PollForActivityTaskResponse (Core.Maybe Types.ActivityId)
pfatrrsActivityId = Lens.field @"activityId"
{-# DEPRECATED pfatrrsActivityId "Use generic-lens or generic-optics with 'activityId' instead." #-}

-- | The ID of the @ActivityTaskStarted@ event recorded in the history.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatrrsStartedEventId :: Lens.Lens' PollForActivityTaskResponse Core.Integer
pfatrrsStartedEventId = Lens.field @"startedEventId"
{-# DEPRECATED pfatrrsStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

-- | The workflow execution that started this activity task.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatrrsWorkflowExecution :: Lens.Lens' PollForActivityTaskResponse (Core.Maybe Types.WorkflowExecution)
pfatrrsWorkflowExecution = Lens.field @"workflowExecution"
{-# DEPRECATED pfatrrsWorkflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead." #-}

-- | The type of this activity task.
--
-- /Note:/ Consider using 'activityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatrrsActivityType :: Lens.Lens' PollForActivityTaskResponse (Core.Maybe Types.ActivityType)
pfatrrsActivityType = Lens.field @"activityType"
{-# DEPRECATED pfatrrsActivityType "Use generic-lens or generic-optics with 'activityType' instead." #-}

-- | The inputs provided when the activity task was scheduled. The form of the input is user defined and should be meaningful to the activity implementation.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatrrsInput :: Lens.Lens' PollForActivityTaskResponse (Core.Maybe Types.Data)
pfatrrsInput = Lens.field @"input"
{-# DEPRECATED pfatrrsInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatrrsResponseStatus :: Lens.Lens' PollForActivityTaskResponse Core.Int
pfatrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pfatrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
