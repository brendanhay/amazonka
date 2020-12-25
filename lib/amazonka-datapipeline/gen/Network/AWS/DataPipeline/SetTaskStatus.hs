{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.SetTaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @SetTaskStatus@ to notify AWS Data Pipeline that a task is completed and provide information about the final status. A task runner makes this call regardless of whether the task was sucessful. A task runner does not need to call @SetTaskStatus@ for tasks that are canceled by the web service during a call to 'ReportTaskProgress' .
module Network.AWS.DataPipeline.SetTaskStatus
  ( -- * Creating a request
    SetTaskStatus (..),
    mkSetTaskStatus,

    -- ** Request lenses
    stsTaskId,
    stsTaskStatus,
    stsErrorId,
    stsErrorMessage,
    stsErrorStackTrace,

    -- * Destructuring the response
    SetTaskStatusResponse (..),
    mkSetTaskStatusResponse,

    -- ** Response lenses
    stsrrsResponseStatus,
  )
where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for SetTaskStatus.
--
-- /See:/ 'mkSetTaskStatus' smart constructor.
data SetTaskStatus = SetTaskStatus'
  { -- | The ID of the task assigned to the task runner. This value is provided in the response for 'PollForTask' .
    taskId :: Types.TaskId,
    -- | If @FINISHED@ , the task successfully completed. If @FAILED@ , the task ended unsuccessfully. Preconditions use false.
    taskStatus :: Types.TaskStatus,
    -- | If an error occurred during the task, this value specifies the error code. This value is set on the physical attempt object. It is used to display error information to the user. It should not start with string "Service_" which is reserved by the system.
    errorId :: Core.Maybe Types.String,
    -- | If an error occurred during the task, this value specifies a text description of the error. This value is set on the physical attempt object. It is used to display error information to the user. The web service does not parse this value.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | If an error occurred during the task, this value specifies the stack trace associated with the error. This value is set on the physical attempt object. It is used to display error information to the user. The web service does not parse this value.
    errorStackTrace :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetTaskStatus' value with any optional fields omitted.
mkSetTaskStatus ::
  -- | 'taskId'
  Types.TaskId ->
  -- | 'taskStatus'
  Types.TaskStatus ->
  SetTaskStatus
mkSetTaskStatus taskId taskStatus =
  SetTaskStatus'
    { taskId,
      taskStatus,
      errorId = Core.Nothing,
      errorMessage = Core.Nothing,
      errorStackTrace = Core.Nothing
    }

-- | The ID of the task assigned to the task runner. This value is provided in the response for 'PollForTask' .
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsTaskId :: Lens.Lens' SetTaskStatus Types.TaskId
stsTaskId = Lens.field @"taskId"
{-# DEPRECATED stsTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | If @FINISHED@ , the task successfully completed. If @FAILED@ , the task ended unsuccessfully. Preconditions use false.
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsTaskStatus :: Lens.Lens' SetTaskStatus Types.TaskStatus
stsTaskStatus = Lens.field @"taskStatus"
{-# DEPRECATED stsTaskStatus "Use generic-lens or generic-optics with 'taskStatus' instead." #-}

-- | If an error occurred during the task, this value specifies the error code. This value is set on the physical attempt object. It is used to display error information to the user. It should not start with string "Service_" which is reserved by the system.
--
-- /Note:/ Consider using 'errorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsErrorId :: Lens.Lens' SetTaskStatus (Core.Maybe Types.String)
stsErrorId = Lens.field @"errorId"
{-# DEPRECATED stsErrorId "Use generic-lens or generic-optics with 'errorId' instead." #-}

-- | If an error occurred during the task, this value specifies a text description of the error. This value is set on the physical attempt object. It is used to display error information to the user. The web service does not parse this value.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsErrorMessage :: Lens.Lens' SetTaskStatus (Core.Maybe Types.ErrorMessage)
stsErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED stsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | If an error occurred during the task, this value specifies the stack trace associated with the error. This value is set on the physical attempt object. It is used to display error information to the user. The web service does not parse this value.
--
-- /Note:/ Consider using 'errorStackTrace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsErrorStackTrace :: Lens.Lens' SetTaskStatus (Core.Maybe Types.String)
stsErrorStackTrace = Lens.field @"errorStackTrace"
{-# DEPRECATED stsErrorStackTrace "Use generic-lens or generic-optics with 'errorStackTrace' instead." #-}

instance Core.FromJSON SetTaskStatus where
  toJSON SetTaskStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("taskId" Core..= taskId),
            Core.Just ("taskStatus" Core..= taskStatus),
            ("errorId" Core..=) Core.<$> errorId,
            ("errorMessage" Core..=) Core.<$> errorMessage,
            ("errorStackTrace" Core..=) Core.<$> errorStackTrace
          ]
      )

instance Core.AWSRequest SetTaskStatus where
  type Rs SetTaskStatus = SetTaskStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DataPipeline.SetTaskStatus")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          SetTaskStatusResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of SetTaskStatus.
--
-- /See:/ 'mkSetTaskStatusResponse' smart constructor.
newtype SetTaskStatusResponse = SetTaskStatusResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetTaskStatusResponse' value with any optional fields omitted.
mkSetTaskStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SetTaskStatusResponse
mkSetTaskStatusResponse responseStatus =
  SetTaskStatusResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsrrsResponseStatus :: Lens.Lens' SetTaskStatusResponse Core.Int
stsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED stsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
