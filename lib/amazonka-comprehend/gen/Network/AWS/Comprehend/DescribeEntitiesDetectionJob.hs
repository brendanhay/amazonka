{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with an entities detection job. Use this operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeEntitiesDetectionJob
    (
    -- * Creating a request
      DescribeEntitiesDetectionJob (..)
    , mkDescribeEntitiesDetectionJob
    -- ** Request lenses
    , dJobId

    -- * Destructuring the response
    , DescribeEntitiesDetectionJobResponse (..)
    , mkDescribeEntitiesDetectionJobResponse
    -- ** Response lenses
    , dedjrfrsEntitiesDetectionJobProperties
    , dedjrfrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEntitiesDetectionJob' smart constructor.
newtype DescribeEntitiesDetectionJob = DescribeEntitiesDetectionJob'
  { jobId :: Types.JobId
    -- ^ The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEntitiesDetectionJob' value with any optional fields omitted.
mkDescribeEntitiesDetectionJob
    :: Types.JobId -- ^ 'jobId'
    -> DescribeEntitiesDetectionJob
mkDescribeEntitiesDetectionJob jobId
  = DescribeEntitiesDetectionJob'{jobId}

-- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dJobId :: Lens.Lens' DescribeEntitiesDetectionJob Types.JobId
dJobId = Lens.field @"jobId"
{-# INLINEABLE dJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery DescribeEntitiesDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEntitiesDetectionJob where
        toHeaders DescribeEntitiesDetectionJob{..}
          = Core.pure
              ("X-Amz-Target",
               "Comprehend_20171127.DescribeEntitiesDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEntitiesDetectionJob where
        toJSON DescribeEntitiesDetectionJob{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest DescribeEntitiesDetectionJob where
        type Rs DescribeEntitiesDetectionJob =
             DescribeEntitiesDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEntitiesDetectionJobResponse' Core.<$>
                   (x Core..:? "EntitiesDetectionJobProperties") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeEntitiesDetectionJobResponse' smart constructor.
data DescribeEntitiesDetectionJobResponse = DescribeEntitiesDetectionJobResponse'
  { entitiesDetectionJobProperties :: Core.Maybe Types.EntitiesDetectionJobProperties
    -- ^ An object that contains the properties associated with an entities detection job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEntitiesDetectionJobResponse' value with any optional fields omitted.
mkDescribeEntitiesDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEntitiesDetectionJobResponse
mkDescribeEntitiesDetectionJobResponse responseStatus
  = DescribeEntitiesDetectionJobResponse'{entitiesDetectionJobProperties
                                            = Core.Nothing,
                                          responseStatus}

-- | An object that contains the properties associated with an entities detection job.
--
-- /Note:/ Consider using 'entitiesDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedjrfrsEntitiesDetectionJobProperties :: Lens.Lens' DescribeEntitiesDetectionJobResponse (Core.Maybe Types.EntitiesDetectionJobProperties)
dedjrfrsEntitiesDetectionJobProperties = Lens.field @"entitiesDetectionJobProperties"
{-# INLINEABLE dedjrfrsEntitiesDetectionJobProperties #-}
{-# DEPRECATED entitiesDetectionJobProperties "Use generic-lens or generic-optics with 'entitiesDetectionJobProperties' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedjrfrsResponseStatus :: Lens.Lens' DescribeEntitiesDetectionJobResponse Core.Int
dedjrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dedjrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
