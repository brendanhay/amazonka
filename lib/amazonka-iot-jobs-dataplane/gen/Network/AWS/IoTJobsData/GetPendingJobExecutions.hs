{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.GetPendingJobExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the list of all jobs for a thing that are not in a terminal status.
module Network.AWS.IoTJobsData.GetPendingJobExecutions
    (
    -- * Creating a request
      GetPendingJobExecutions (..)
    , mkGetPendingJobExecutions
    -- ** Request lenses
    , gpjeThingName

    -- * Destructuring the response
    , GetPendingJobExecutionsResponse (..)
    , mkGetPendingJobExecutionsResponse
    -- ** Response lenses
    , gpjerrsInProgressJobs
    , gpjerrsQueuedJobs
    , gpjerrsResponseStatus
    ) where

import qualified Network.AWS.IoTJobsData.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPendingJobExecutions' smart constructor.
newtype GetPendingJobExecutions = GetPendingJobExecutions'
  { thingName :: Types.ThingName
    -- ^ The name of the thing that is executing the job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetPendingJobExecutions' value with any optional fields omitted.
mkGetPendingJobExecutions
    :: Types.ThingName -- ^ 'thingName'
    -> GetPendingJobExecutions
mkGetPendingJobExecutions thingName
  = GetPendingJobExecutions'{thingName}

-- | The name of the thing that is executing the job.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpjeThingName :: Lens.Lens' GetPendingJobExecutions Types.ThingName
gpjeThingName = Lens.field @"thingName"
{-# INLINEABLE gpjeThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

instance Core.ToQuery GetPendingJobExecutions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetPendingJobExecutions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetPendingJobExecutions where
        type Rs GetPendingJobExecutions = GetPendingJobExecutionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/things/" Core.<> Core.toText thingName Core.<> "/jobs",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetPendingJobExecutionsResponse' Core.<$>
                   (x Core..:? "inProgressJobs") Core.<*> x Core..:? "queuedJobs"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetPendingJobExecutionsResponse' smart constructor.
data GetPendingJobExecutionsResponse = GetPendingJobExecutionsResponse'
  { inProgressJobs :: Core.Maybe [Types.JobExecutionSummary]
    -- ^ A list of JobExecutionSummary objects with status IN_PROGRESS.
  , queuedJobs :: Core.Maybe [Types.JobExecutionSummary]
    -- ^ A list of JobExecutionSummary objects with status QUEUED.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPendingJobExecutionsResponse' value with any optional fields omitted.
mkGetPendingJobExecutionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPendingJobExecutionsResponse
mkGetPendingJobExecutionsResponse responseStatus
  = GetPendingJobExecutionsResponse'{inProgressJobs = Core.Nothing,
                                     queuedJobs = Core.Nothing, responseStatus}

-- | A list of JobExecutionSummary objects with status IN_PROGRESS.
--
-- /Note:/ Consider using 'inProgressJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpjerrsInProgressJobs :: Lens.Lens' GetPendingJobExecutionsResponse (Core.Maybe [Types.JobExecutionSummary])
gpjerrsInProgressJobs = Lens.field @"inProgressJobs"
{-# INLINEABLE gpjerrsInProgressJobs #-}
{-# DEPRECATED inProgressJobs "Use generic-lens or generic-optics with 'inProgressJobs' instead"  #-}

-- | A list of JobExecutionSummary objects with status QUEUED.
--
-- /Note:/ Consider using 'queuedJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpjerrsQueuedJobs :: Lens.Lens' GetPendingJobExecutionsResponse (Core.Maybe [Types.JobExecutionSummary])
gpjerrsQueuedJobs = Lens.field @"queuedJobs"
{-# INLINEABLE gpjerrsQueuedJobs #-}
{-# DEPRECATED queuedJobs "Use generic-lens or generic-optics with 'queuedJobs' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpjerrsResponseStatus :: Lens.Lens' GetPendingJobExecutionsResponse Core.Int
gpjerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpjerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
