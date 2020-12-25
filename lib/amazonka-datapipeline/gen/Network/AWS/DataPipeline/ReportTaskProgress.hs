{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.ReportTaskProgress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @ReportTaskProgress@ when assigned a task to acknowledge that it has the task. If the web service does not receive this acknowledgement within 2 minutes, it assigns the task in a subsequent 'PollForTask' call. After this initial acknowledgement, the task runner only needs to report progress every 15 minutes to maintain its ownership of the task. You can change this reporting time from 15 minutes by specifying a @reportProgressTimeout@ field in your pipeline.
--
-- If a task runner does not report its status after 5 minutes, AWS Data Pipeline assumes that the task runner is unable to process the task and reassigns the task in a subsequent response to 'PollForTask' . Task runners should call @ReportTaskProgress@ every 60 seconds.
module Network.AWS.DataPipeline.ReportTaskProgress
  ( -- * Creating a request
    ReportTaskProgress (..),
    mkReportTaskProgress,

    -- ** Request lenses
    rtpTaskId,
    rtpFields,

    -- * Destructuring the response
    ReportTaskProgressResponse (..),
    mkReportTaskProgressResponse,

    -- ** Response lenses
    rtprrsCanceled,
    rtprrsResponseStatus,
  )
where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ReportTaskProgress.
--
-- /See:/ 'mkReportTaskProgress' smart constructor.
data ReportTaskProgress = ReportTaskProgress'
  { -- | The ID of the task assigned to the task runner. This value is provided in the response for 'PollForTask' .
    taskId :: Types.TaskId,
    -- | Key-value pairs that define the properties of the ReportTaskProgressInput object.
    fields :: Core.Maybe [Types.Field]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReportTaskProgress' value with any optional fields omitted.
mkReportTaskProgress ::
  -- | 'taskId'
  Types.TaskId ->
  ReportTaskProgress
mkReportTaskProgress taskId =
  ReportTaskProgress' {taskId, fields = Core.Nothing}

-- | The ID of the task assigned to the task runner. This value is provided in the response for 'PollForTask' .
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtpTaskId :: Lens.Lens' ReportTaskProgress Types.TaskId
rtpTaskId = Lens.field @"taskId"
{-# DEPRECATED rtpTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | Key-value pairs that define the properties of the ReportTaskProgressInput object.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtpFields :: Lens.Lens' ReportTaskProgress (Core.Maybe [Types.Field])
rtpFields = Lens.field @"fields"
{-# DEPRECATED rtpFields "Use generic-lens or generic-optics with 'fields' instead." #-}

instance Core.FromJSON ReportTaskProgress where
  toJSON ReportTaskProgress {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("taskId" Core..= taskId),
            ("fields" Core..=) Core.<$> fields
          ]
      )

instance Core.AWSRequest ReportTaskProgress where
  type Rs ReportTaskProgress = ReportTaskProgressResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DataPipeline.ReportTaskProgress")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ReportTaskProgressResponse'
            Core.<$> (x Core..: "canceled") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of ReportTaskProgress.
--
-- /See:/ 'mkReportTaskProgressResponse' smart constructor.
data ReportTaskProgressResponse = ReportTaskProgressResponse'
  { -- | If true, the calling task runner should cancel processing of the task. The task runner does not need to call 'SetTaskStatus' for canceled tasks.
    canceled :: Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReportTaskProgressResponse' value with any optional fields omitted.
mkReportTaskProgressResponse ::
  -- | 'canceled'
  Core.Bool ->
  -- | 'responseStatus'
  Core.Int ->
  ReportTaskProgressResponse
mkReportTaskProgressResponse canceled responseStatus =
  ReportTaskProgressResponse' {canceled, responseStatus}

-- | If true, the calling task runner should cancel processing of the task. The task runner does not need to call 'SetTaskStatus' for canceled tasks.
--
-- /Note:/ Consider using 'canceled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtprrsCanceled :: Lens.Lens' ReportTaskProgressResponse Core.Bool
rtprrsCanceled = Lens.field @"canceled"
{-# DEPRECATED rtprrsCanceled "Use generic-lens or generic-optics with 'canceled' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtprrsResponseStatus :: Lens.Lens' ReportTaskProgressResponse Core.Int
rtprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
