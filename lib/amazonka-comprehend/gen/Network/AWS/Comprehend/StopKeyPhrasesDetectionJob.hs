{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopKeyPhrasesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a key phrases detection job in progress.
--
-- If the job state is @IN_PROGRESS@ the job is marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state; otherwise the job is stopped and put into the @STOPPED@ state.
-- If the job is in the @COMPLETED@ or @FAILED@ state when you call the @StopDominantLanguageDetectionJob@ operation, the operation returns a 400 Internal Request Exception. 
-- When a job is stopped, any documents already processed are written to the output location.
module Network.AWS.Comprehend.StopKeyPhrasesDetectionJob
    (
    -- * Creating a request
      StopKeyPhrasesDetectionJob (..)
    , mkStopKeyPhrasesDetectionJob
    -- ** Request lenses
    , skpdjJobId

    -- * Destructuring the response
    , StopKeyPhrasesDetectionJobResponse (..)
    , mkStopKeyPhrasesDetectionJobResponse
    -- ** Response lenses
    , skpdjrrsJobId
    , skpdjrrsJobStatus
    , skpdjrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopKeyPhrasesDetectionJob' smart constructor.
newtype StopKeyPhrasesDetectionJob = StopKeyPhrasesDetectionJob'
  { jobId :: Types.JobId
    -- ^ The identifier of the key phrases detection job to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopKeyPhrasesDetectionJob' value with any optional fields omitted.
mkStopKeyPhrasesDetectionJob
    :: Types.JobId -- ^ 'jobId'
    -> StopKeyPhrasesDetectionJob
mkStopKeyPhrasesDetectionJob jobId
  = StopKeyPhrasesDetectionJob'{jobId}

-- | The identifier of the key phrases detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjJobId :: Lens.Lens' StopKeyPhrasesDetectionJob Types.JobId
skpdjJobId = Lens.field @"jobId"
{-# INLINEABLE skpdjJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery StopKeyPhrasesDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopKeyPhrasesDetectionJob where
        toHeaders StopKeyPhrasesDetectionJob{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.StopKeyPhrasesDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopKeyPhrasesDetectionJob where
        toJSON StopKeyPhrasesDetectionJob{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest StopKeyPhrasesDetectionJob where
        type Rs StopKeyPhrasesDetectionJob =
             StopKeyPhrasesDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopKeyPhrasesDetectionJobResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> x Core..:? "JobStatus" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopKeyPhrasesDetectionJobResponse' smart constructor.
data StopKeyPhrasesDetectionJobResponse = StopKeyPhrasesDetectionJobResponse'
  { jobId :: Core.Maybe Types.JobId
    -- ^ The identifier of the key phrases detection job to stop.
  , jobStatus :: Core.Maybe Types.JobStatus
    -- ^ Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopKeyPhrasesDetectionJob@ operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopKeyPhrasesDetectionJobResponse' value with any optional fields omitted.
mkStopKeyPhrasesDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopKeyPhrasesDetectionJobResponse
mkStopKeyPhrasesDetectionJobResponse responseStatus
  = StopKeyPhrasesDetectionJobResponse'{jobId = Core.Nothing,
                                        jobStatus = Core.Nothing, responseStatus}

-- | The identifier of the key phrases detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjrrsJobId :: Lens.Lens' StopKeyPhrasesDetectionJobResponse (Core.Maybe Types.JobId)
skpdjrrsJobId = Lens.field @"jobId"
{-# INLINEABLE skpdjrrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopKeyPhrasesDetectionJob@ operation.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjrrsJobStatus :: Lens.Lens' StopKeyPhrasesDetectionJobResponse (Core.Maybe Types.JobStatus)
skpdjrrsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE skpdjrrsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjrrsResponseStatus :: Lens.Lens' StopKeyPhrasesDetectionJobResponse Core.Int
skpdjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE skpdjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
