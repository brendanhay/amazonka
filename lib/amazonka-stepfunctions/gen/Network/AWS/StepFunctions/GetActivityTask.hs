{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.GetActivityTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by workers to retrieve a task (with the specified activity ARN) which has been scheduled for execution by a running state machine. This initiates a long poll, where the service holds the HTTP connection open and responds as soon as a task becomes available (i.e. an execution of a task of this type is needed.) The maximum time the service holds on to the request before responding is 60 seconds. If no task is available within 60 seconds, the poll returns a @taskToken@ with a null string.
--
-- /Important:/ Workers should set their client side socket timeout to at least 65 seconds (5 seconds higher than the maximum time the service may hold the poll request).
-- Polling with @GetActivityTask@ can cause latency in some implementations. See <https://docs.aws.amazon.com/step-functions/latest/dg/bp-activity-pollers.html Avoid Latency When Polling for Activity Tasks> in the Step Functions Developer Guide.
module Network.AWS.StepFunctions.GetActivityTask
  ( -- * Creating a request
    GetActivityTask (..),
    mkGetActivityTask,

    -- ** Request lenses
    gatActivityArn,
    gatWorkerName,

    -- * Destructuring the response
    GetActivityTaskResponse (..),
    mkGetActivityTaskResponse,

    -- ** Response lenses
    gatrrsInput,
    gatrrsTaskToken,
    gatrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkGetActivityTask' smart constructor.
data GetActivityTask = GetActivityTask'
  { -- | The Amazon Resource Name (ARN) of the activity to retrieve tasks from (assigned when you create the task using 'CreateActivity' .)
    activityArn :: Types.Arn,
    -- | You can provide an arbitrary name in order to identify the worker that the task is assigned to. This name is used when it is logged in the execution history.
    workerName :: Core.Maybe Types.WorkerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetActivityTask' value with any optional fields omitted.
mkGetActivityTask ::
  -- | 'activityArn'
  Types.Arn ->
  GetActivityTask
mkGetActivityTask activityArn =
  GetActivityTask' {activityArn, workerName = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the activity to retrieve tasks from (assigned when you create the task using 'CreateActivity' .)
--
-- /Note:/ Consider using 'activityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatActivityArn :: Lens.Lens' GetActivityTask Types.Arn
gatActivityArn = Lens.field @"activityArn"
{-# DEPRECATED gatActivityArn "Use generic-lens or generic-optics with 'activityArn' instead." #-}

-- | You can provide an arbitrary name in order to identify the worker that the task is assigned to. This name is used when it is logged in the execution history.
--
-- /Note:/ Consider using 'workerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatWorkerName :: Lens.Lens' GetActivityTask (Core.Maybe Types.WorkerName)
gatWorkerName = Lens.field @"workerName"
{-# DEPRECATED gatWorkerName "Use generic-lens or generic-optics with 'workerName' instead." #-}

instance Core.FromJSON GetActivityTask where
  toJSON GetActivityTask {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("activityArn" Core..= activityArn),
            ("workerName" Core..=) Core.<$> workerName
          ]
      )

instance Core.AWSRequest GetActivityTask where
  type Rs GetActivityTask = GetActivityTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSStepFunctions.GetActivityTask")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetActivityTaskResponse'
            Core.<$> (x Core..:? "input")
            Core.<*> (x Core..:? "taskToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetActivityTaskResponse' smart constructor.
data GetActivityTaskResponse = GetActivityTaskResponse'
  { -- | The string that contains the JSON input data for the task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    input :: Core.Maybe Types.Input,
    -- | A token that identifies the scheduled task. This token must be copied and included in subsequent calls to 'SendTaskHeartbeat' , 'SendTaskSuccess' or 'SendTaskFailure' in order to report the progress or completion of the task.
    taskToken :: Core.Maybe Types.TaskToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetActivityTaskResponse' value with any optional fields omitted.
mkGetActivityTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetActivityTaskResponse
mkGetActivityTaskResponse responseStatus =
  GetActivityTaskResponse'
    { input = Core.Nothing,
      taskToken = Core.Nothing,
      responseStatus
    }

-- | The string that contains the JSON input data for the task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatrrsInput :: Lens.Lens' GetActivityTaskResponse (Core.Maybe Types.Input)
gatrrsInput = Lens.field @"input"
{-# DEPRECATED gatrrsInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | A token that identifies the scheduled task. This token must be copied and included in subsequent calls to 'SendTaskHeartbeat' , 'SendTaskSuccess' or 'SendTaskFailure' in order to report the progress or completion of the task.
--
-- /Note:/ Consider using 'taskToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatrrsTaskToken :: Lens.Lens' GetActivityTaskResponse (Core.Maybe Types.TaskToken)
gatrrsTaskToken = Lens.field @"taskToken"
{-# DEPRECATED gatrrsTaskToken "Use generic-lens or generic-optics with 'taskToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatrrsResponseStatus :: Lens.Lens' GetActivityTaskResponse Core.Int
gatrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gatrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
