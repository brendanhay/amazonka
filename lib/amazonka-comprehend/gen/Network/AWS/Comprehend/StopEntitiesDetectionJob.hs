{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an entities detection job in progress.
--
-- If the job state is @IN_PROGRESS@ the job is marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state; otherwise the job is stopped and put into the @STOPPED@ state.
-- If the job is in the @COMPLETED@ or @FAILED@ state when you call the @StopDominantLanguageDetectionJob@ operation, the operation returns a 400 Internal Request Exception. 
-- When a job is stopped, any documents already processed are written to the output location.
module Network.AWS.Comprehend.StopEntitiesDetectionJob
    (
    -- * Creating a request
      StopEntitiesDetectionJob (..)
    , mkStopEntitiesDetectionJob
    -- ** Request lenses
    , sJobId

    -- * Destructuring the response
    , StopEntitiesDetectionJobResponse (..)
    , mkStopEntitiesDetectionJobResponse
    -- ** Response lenses
    , srsJobId
    , srsJobStatus
    , srsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopEntitiesDetectionJob' smart constructor.
newtype StopEntitiesDetectionJob = StopEntitiesDetectionJob'
  { jobId :: Types.JobId
    -- ^ The identifier of the entities detection job to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopEntitiesDetectionJob' value with any optional fields omitted.
mkStopEntitiesDetectionJob
    :: Types.JobId -- ^ 'jobId'
    -> StopEntitiesDetectionJob
mkStopEntitiesDetectionJob jobId = StopEntitiesDetectionJob'{jobId}

-- | The identifier of the entities detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sJobId :: Lens.Lens' StopEntitiesDetectionJob Types.JobId
sJobId = Lens.field @"jobId"
{-# INLINEABLE sJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery StopEntitiesDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopEntitiesDetectionJob where
        toHeaders StopEntitiesDetectionJob{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.StopEntitiesDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopEntitiesDetectionJob where
        toJSON StopEntitiesDetectionJob{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest StopEntitiesDetectionJob where
        type Rs StopEntitiesDetectionJob = StopEntitiesDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopEntitiesDetectionJobResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> x Core..:? "JobStatus" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopEntitiesDetectionJobResponse' smart constructor.
data StopEntitiesDetectionJobResponse = StopEntitiesDetectionJobResponse'
  { jobId :: Core.Maybe Types.JobId
    -- ^ The identifier of the entities detection job to stop.
  , jobStatus :: Core.Maybe Types.JobStatus
    -- ^ Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopEntitiesDetectionJob@ operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopEntitiesDetectionJobResponse' value with any optional fields omitted.
mkStopEntitiesDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopEntitiesDetectionJobResponse
mkStopEntitiesDetectionJobResponse responseStatus
  = StopEntitiesDetectionJobResponse'{jobId = Core.Nothing,
                                      jobStatus = Core.Nothing, responseStatus}

-- | The identifier of the entities detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsJobId :: Lens.Lens' StopEntitiesDetectionJobResponse (Core.Maybe Types.JobId)
srsJobId = Lens.field @"jobId"
{-# INLINEABLE srsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopEntitiesDetectionJob@ operation.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsJobStatus :: Lens.Lens' StopEntitiesDetectionJobResponse (Core.Maybe Types.JobStatus)
srsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE srsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopEntitiesDetectionJobResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
