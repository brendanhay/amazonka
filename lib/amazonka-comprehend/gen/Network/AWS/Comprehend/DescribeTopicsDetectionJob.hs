{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeTopicsDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a topic detection job. Use this operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeTopicsDetectionJob
    (
    -- * Creating a request
      DescribeTopicsDetectionJob (..)
    , mkDescribeTopicsDetectionJob
    -- ** Request lenses
    , dtdjJobId

    -- * Destructuring the response
    , DescribeTopicsDetectionJobResponse (..)
    , mkDescribeTopicsDetectionJobResponse
    -- ** Response lenses
    , dtdjrrsTopicsDetectionJobProperties
    , dtdjrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTopicsDetectionJob' smart constructor.
newtype DescribeTopicsDetectionJob = DescribeTopicsDetectionJob'
  { jobId :: Types.JobId
    -- ^ The identifier assigned by the user to the detection job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTopicsDetectionJob' value with any optional fields omitted.
mkDescribeTopicsDetectionJob
    :: Types.JobId -- ^ 'jobId'
    -> DescribeTopicsDetectionJob
mkDescribeTopicsDetectionJob jobId
  = DescribeTopicsDetectionJob'{jobId}

-- | The identifier assigned by the user to the detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdjJobId :: Lens.Lens' DescribeTopicsDetectionJob Types.JobId
dtdjJobId = Lens.field @"jobId"
{-# INLINEABLE dtdjJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery DescribeTopicsDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeTopicsDetectionJob where
        toHeaders DescribeTopicsDetectionJob{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.DescribeTopicsDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeTopicsDetectionJob where
        toJSON DescribeTopicsDetectionJob{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest DescribeTopicsDetectionJob where
        type Rs DescribeTopicsDetectionJob =
             DescribeTopicsDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeTopicsDetectionJobResponse' Core.<$>
                   (x Core..:? "TopicsDetectionJobProperties") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeTopicsDetectionJobResponse' smart constructor.
data DescribeTopicsDetectionJobResponse = DescribeTopicsDetectionJobResponse'
  { topicsDetectionJobProperties :: Core.Maybe Types.TopicsDetectionJobProperties
    -- ^ The list of properties for the requested job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeTopicsDetectionJobResponse' value with any optional fields omitted.
mkDescribeTopicsDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTopicsDetectionJobResponse
mkDescribeTopicsDetectionJobResponse responseStatus
  = DescribeTopicsDetectionJobResponse'{topicsDetectionJobProperties
                                          = Core.Nothing,
                                        responseStatus}

-- | The list of properties for the requested job.
--
-- /Note:/ Consider using 'topicsDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdjrrsTopicsDetectionJobProperties :: Lens.Lens' DescribeTopicsDetectionJobResponse (Core.Maybe Types.TopicsDetectionJobProperties)
dtdjrrsTopicsDetectionJobProperties = Lens.field @"topicsDetectionJobProperties"
{-# INLINEABLE dtdjrrsTopicsDetectionJobProperties #-}
{-# DEPRECATED topicsDetectionJobProperties "Use generic-lens or generic-optics with 'topicsDetectionJobProperties' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdjrrsResponseStatus :: Lens.Lens' DescribeTopicsDetectionJobResponse Core.Int
dtdjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtdjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
