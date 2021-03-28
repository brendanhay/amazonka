{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeEventsDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status and details of an events detection job.
module Network.AWS.Comprehend.DescribeEventsDetectionJob
    (
    -- * Creating a request
      DescribeEventsDetectionJob (..)
    , mkDescribeEventsDetectionJob
    -- ** Request lenses
    , dedjJobId

    -- * Destructuring the response
    , DescribeEventsDetectionJobResponse (..)
    , mkDescribeEventsDetectionJobResponse
    -- ** Response lenses
    , dedjrrsEventsDetectionJobProperties
    , dedjrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEventsDetectionJob' smart constructor.
newtype DescribeEventsDetectionJob = DescribeEventsDetectionJob'
  { jobId :: Types.JobId
    -- ^ The identifier of the events detection job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventsDetectionJob' value with any optional fields omitted.
mkDescribeEventsDetectionJob
    :: Types.JobId -- ^ 'jobId'
    -> DescribeEventsDetectionJob
mkDescribeEventsDetectionJob jobId
  = DescribeEventsDetectionJob'{jobId}

-- | The identifier of the events detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedjJobId :: Lens.Lens' DescribeEventsDetectionJob Types.JobId
dedjJobId = Lens.field @"jobId"
{-# INLINEABLE dedjJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery DescribeEventsDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEventsDetectionJob where
        toHeaders DescribeEventsDetectionJob{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.DescribeEventsDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEventsDetectionJob where
        toJSON DescribeEventsDetectionJob{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest DescribeEventsDetectionJob where
        type Rs DescribeEventsDetectionJob =
             DescribeEventsDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEventsDetectionJobResponse' Core.<$>
                   (x Core..:? "EventsDetectionJobProperties") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeEventsDetectionJobResponse' smart constructor.
data DescribeEventsDetectionJobResponse = DescribeEventsDetectionJobResponse'
  { eventsDetectionJobProperties :: Core.Maybe Types.EventsDetectionJobProperties
    -- ^ An object that contains the properties associated with an event detection job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEventsDetectionJobResponse' value with any optional fields omitted.
mkDescribeEventsDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEventsDetectionJobResponse
mkDescribeEventsDetectionJobResponse responseStatus
  = DescribeEventsDetectionJobResponse'{eventsDetectionJobProperties
                                          = Core.Nothing,
                                        responseStatus}

-- | An object that contains the properties associated with an event detection job.
--
-- /Note:/ Consider using 'eventsDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedjrrsEventsDetectionJobProperties :: Lens.Lens' DescribeEventsDetectionJobResponse (Core.Maybe Types.EventsDetectionJobProperties)
dedjrrsEventsDetectionJobProperties = Lens.field @"eventsDetectionJobProperties"
{-# INLINEABLE dedjrrsEventsDetectionJobProperties #-}
{-# DEPRECATED eventsDetectionJobProperties "Use generic-lens or generic-optics with 'eventsDetectionJobProperties' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedjrrsResponseStatus :: Lens.Lens' DescribeEventsDetectionJobResponse Core.Int
dedjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dedjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
