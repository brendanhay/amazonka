{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopPiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a PII entities detection job in progress.
module Network.AWS.Comprehend.StopPiiEntitiesDetectionJob
    (
    -- * Creating a request
      StopPiiEntitiesDetectionJob (..)
    , mkStopPiiEntitiesDetectionJob
    -- ** Request lenses
    , spedjJobId

    -- * Destructuring the response
    , StopPiiEntitiesDetectionJobResponse (..)
    , mkStopPiiEntitiesDetectionJobResponse
    -- ** Response lenses
    , spedjrfrsJobId
    , spedjrfrsJobStatus
    , spedjrfrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopPiiEntitiesDetectionJob' smart constructor.
newtype StopPiiEntitiesDetectionJob = StopPiiEntitiesDetectionJob'
  { jobId :: Types.JobId
    -- ^ The identifier of the PII entities detection job to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopPiiEntitiesDetectionJob' value with any optional fields omitted.
mkStopPiiEntitiesDetectionJob
    :: Types.JobId -- ^ 'jobId'
    -> StopPiiEntitiesDetectionJob
mkStopPiiEntitiesDetectionJob jobId
  = StopPiiEntitiesDetectionJob'{jobId}

-- | The identifier of the PII entities detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjJobId :: Lens.Lens' StopPiiEntitiesDetectionJob Types.JobId
spedjJobId = Lens.field @"jobId"
{-# INLINEABLE spedjJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery StopPiiEntitiesDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopPiiEntitiesDetectionJob where
        toHeaders StopPiiEntitiesDetectionJob{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.StopPiiEntitiesDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopPiiEntitiesDetectionJob where
        toJSON StopPiiEntitiesDetectionJob{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest StopPiiEntitiesDetectionJob where
        type Rs StopPiiEntitiesDetectionJob =
             StopPiiEntitiesDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopPiiEntitiesDetectionJobResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> x Core..:? "JobStatus" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopPiiEntitiesDetectionJobResponse' smart constructor.
data StopPiiEntitiesDetectionJobResponse = StopPiiEntitiesDetectionJobResponse'
  { jobId :: Core.Maybe Types.JobId
    -- ^ The identifier of the PII entities detection job to stop.
  , jobStatus :: Core.Maybe Types.JobStatus
    -- ^ The status of the PII entities detection job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopPiiEntitiesDetectionJobResponse' value with any optional fields omitted.
mkStopPiiEntitiesDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopPiiEntitiesDetectionJobResponse
mkStopPiiEntitiesDetectionJobResponse responseStatus
  = StopPiiEntitiesDetectionJobResponse'{jobId = Core.Nothing,
                                         jobStatus = Core.Nothing, responseStatus}

-- | The identifier of the PII entities detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjrfrsJobId :: Lens.Lens' StopPiiEntitiesDetectionJobResponse (Core.Maybe Types.JobId)
spedjrfrsJobId = Lens.field @"jobId"
{-# INLINEABLE spedjrfrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The status of the PII entities detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjrfrsJobStatus :: Lens.Lens' StopPiiEntitiesDetectionJobResponse (Core.Maybe Types.JobStatus)
spedjrfrsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE spedjrfrsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjrfrsResponseStatus :: Lens.Lens' StopPiiEntitiesDetectionJobResponse Core.Int
spedjrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE spedjrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
