{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopDominantLanguageDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a dominant language detection job in progress.
--
-- If the job state is @IN_PROGRESS@ the job is marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state; otherwise the job is stopped and put into the @STOPPED@ state.
-- If the job is in the @COMPLETED@ or @FAILED@ state when you call the @StopDominantLanguageDetectionJob@ operation, the operation returns a 400 Internal Request Exception. 
-- When a job is stopped, any documents already processed are written to the output location.
module Network.AWS.Comprehend.StopDominantLanguageDetectionJob
    (
    -- * Creating a request
      StopDominantLanguageDetectionJob (..)
    , mkStopDominantLanguageDetectionJob
    -- ** Request lenses
    , sdldjJobId

    -- * Destructuring the response
    , StopDominantLanguageDetectionJobResponse (..)
    , mkStopDominantLanguageDetectionJobResponse
    -- ** Response lenses
    , sdldjrfrsJobId
    , sdldjrfrsJobStatus
    , sdldjrfrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopDominantLanguageDetectionJob' smart constructor.
newtype StopDominantLanguageDetectionJob = StopDominantLanguageDetectionJob'
  { jobId :: Types.JobId
    -- ^ The identifier of the dominant language detection job to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopDominantLanguageDetectionJob' value with any optional fields omitted.
mkStopDominantLanguageDetectionJob
    :: Types.JobId -- ^ 'jobId'
    -> StopDominantLanguageDetectionJob
mkStopDominantLanguageDetectionJob jobId
  = StopDominantLanguageDetectionJob'{jobId}

-- | The identifier of the dominant language detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjJobId :: Lens.Lens' StopDominantLanguageDetectionJob Types.JobId
sdldjJobId = Lens.field @"jobId"
{-# INLINEABLE sdldjJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery StopDominantLanguageDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopDominantLanguageDetectionJob where
        toHeaders StopDominantLanguageDetectionJob{..}
          = Core.pure
              ("X-Amz-Target",
               "Comprehend_20171127.StopDominantLanguageDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopDominantLanguageDetectionJob where
        toJSON StopDominantLanguageDetectionJob{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest StopDominantLanguageDetectionJob where
        type Rs StopDominantLanguageDetectionJob =
             StopDominantLanguageDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopDominantLanguageDetectionJobResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> x Core..:? "JobStatus" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopDominantLanguageDetectionJobResponse' smart constructor.
data StopDominantLanguageDetectionJobResponse = StopDominantLanguageDetectionJobResponse'
  { jobId :: Core.Maybe Types.JobId
    -- ^ The identifier of the dominant language detection job to stop.
  , jobStatus :: Core.Maybe Types.JobStatus
    -- ^ Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopDominantLanguageDetectionJob@ operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopDominantLanguageDetectionJobResponse' value with any optional fields omitted.
mkStopDominantLanguageDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopDominantLanguageDetectionJobResponse
mkStopDominantLanguageDetectionJobResponse responseStatus
  = StopDominantLanguageDetectionJobResponse'{jobId = Core.Nothing,
                                              jobStatus = Core.Nothing, responseStatus}

-- | The identifier of the dominant language detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjrfrsJobId :: Lens.Lens' StopDominantLanguageDetectionJobResponse (Core.Maybe Types.JobId)
sdldjrfrsJobId = Lens.field @"jobId"
{-# INLINEABLE sdldjrfrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopDominantLanguageDetectionJob@ operation.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjrfrsJobStatus :: Lens.Lens' StopDominantLanguageDetectionJobResponse (Core.Maybe Types.JobStatus)
sdldjrfrsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE sdldjrfrsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjrfrsResponseStatus :: Lens.Lens' StopDominantLanguageDetectionJobResponse Core.Int
sdldjrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sdldjrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
