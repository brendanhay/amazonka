{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ReportTaskProgress (..)
    , mkReportTaskProgress
    -- ** Request lenses
    , rtpTaskId
    , rtpFields

    -- * Destructuring the response
    , ReportTaskProgressResponse (..)
    , mkReportTaskProgressResponse
    -- ** Response lenses
    , rtprrsCanceled
    , rtprrsResponseStatus
    ) where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ReportTaskProgress.
--
-- /See:/ 'mkReportTaskProgress' smart constructor.
data ReportTaskProgress = ReportTaskProgress'
  { taskId :: Types.TaskId
    -- ^ The ID of the task assigned to the task runner. This value is provided in the response for 'PollForTask' .
  , fields :: Core.Maybe [Types.Field]
    -- ^ Key-value pairs that define the properties of the ReportTaskProgressInput object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReportTaskProgress' value with any optional fields omitted.
mkReportTaskProgress
    :: Types.TaskId -- ^ 'taskId'
    -> ReportTaskProgress
mkReportTaskProgress taskId
  = ReportTaskProgress'{taskId, fields = Core.Nothing}

-- | The ID of the task assigned to the task runner. This value is provided in the response for 'PollForTask' .
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtpTaskId :: Lens.Lens' ReportTaskProgress Types.TaskId
rtpTaskId = Lens.field @"taskId"
{-# INLINEABLE rtpTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

-- | Key-value pairs that define the properties of the ReportTaskProgressInput object.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtpFields :: Lens.Lens' ReportTaskProgress (Core.Maybe [Types.Field])
rtpFields = Lens.field @"fields"
{-# INLINEABLE rtpFields #-}
{-# DEPRECATED fields "Use generic-lens or generic-optics with 'fields' instead"  #-}

instance Core.ToQuery ReportTaskProgress where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ReportTaskProgress where
        toHeaders ReportTaskProgress{..}
          = Core.pure ("X-Amz-Target", "DataPipeline.ReportTaskProgress")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ReportTaskProgress where
        toJSON ReportTaskProgress{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("taskId" Core..= taskId),
                  ("fields" Core..=) Core.<$> fields])

instance Core.AWSRequest ReportTaskProgress where
        type Rs ReportTaskProgress = ReportTaskProgressResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ReportTaskProgressResponse' Core.<$>
                   (x Core..: "canceled") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of ReportTaskProgress.
--
-- /See:/ 'mkReportTaskProgressResponse' smart constructor.
data ReportTaskProgressResponse = ReportTaskProgressResponse'
  { canceled :: Core.Bool
    -- ^ If true, the calling task runner should cancel processing of the task. The task runner does not need to call 'SetTaskStatus' for canceled tasks.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReportTaskProgressResponse' value with any optional fields omitted.
mkReportTaskProgressResponse
    :: Core.Bool -- ^ 'canceled'
    -> Core.Int -- ^ 'responseStatus'
    -> ReportTaskProgressResponse
mkReportTaskProgressResponse canceled responseStatus
  = ReportTaskProgressResponse'{canceled, responseStatus}

-- | If true, the calling task runner should cancel processing of the task. The task runner does not need to call 'SetTaskStatus' for canceled tasks.
--
-- /Note:/ Consider using 'canceled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtprrsCanceled :: Lens.Lens' ReportTaskProgressResponse Core.Bool
rtprrsCanceled = Lens.field @"canceled"
{-# INLINEABLE rtprrsCanceled #-}
{-# DEPRECATED canceled "Use generic-lens or generic-optics with 'canceled' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtprrsResponseStatus :: Lens.Lens' ReportTaskProgressResponse Core.Int
rtprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
