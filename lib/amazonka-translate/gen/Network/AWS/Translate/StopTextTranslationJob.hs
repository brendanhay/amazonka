{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.StopTextTranslationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an asynchronous batch translation job that is in progress.
--
-- If the job's state is @IN_PROGRESS@ , the job will be marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state. Otherwise, the job is put into the @STOPPED@ state.
-- Asynchronous batch translation jobs are started with the 'StartTextTranslationJob' operation. You can use the 'DescribeTextTranslationJob' or 'ListTextTranslationJobs' operations to get a batch translation job's @JobId@ .
module Network.AWS.Translate.StopTextTranslationJob
    (
    -- * Creating a request
      StopTextTranslationJob (..)
    , mkStopTextTranslationJob
    -- ** Request lenses
    , sttjJobId

    -- * Destructuring the response
    , StopTextTranslationJobResponse (..)
    , mkStopTextTranslationJobResponse
    -- ** Response lenses
    , sttjrrsJobId
    , sttjrrsJobStatus
    , sttjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Translate.Types as Types

-- | /See:/ 'mkStopTextTranslationJob' smart constructor.
newtype StopTextTranslationJob = StopTextTranslationJob'
  { jobId :: Types.JobId
    -- ^ The job ID of the job to be stopped.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopTextTranslationJob' value with any optional fields omitted.
mkStopTextTranslationJob
    :: Types.JobId -- ^ 'jobId'
    -> StopTextTranslationJob
mkStopTextTranslationJob jobId = StopTextTranslationJob'{jobId}

-- | The job ID of the job to be stopped.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjJobId :: Lens.Lens' StopTextTranslationJob Types.JobId
sttjJobId = Lens.field @"jobId"
{-# INLINEABLE sttjJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery StopTextTranslationJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopTextTranslationJob where
        toHeaders StopTextTranslationJob{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSShineFrontendService_20170701.StopTextTranslationJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopTextTranslationJob where
        toJSON StopTextTranslationJob{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest StopTextTranslationJob where
        type Rs StopTextTranslationJob = StopTextTranslationJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopTextTranslationJobResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> x Core..:? "JobStatus" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopTextTranslationJobResponse' smart constructor.
data StopTextTranslationJobResponse = StopTextTranslationJobResponse'
  { jobId :: Core.Maybe Types.JobId
    -- ^ The job ID of the stopped batch translation job.
  , jobStatus :: Core.Maybe Types.JobStatus
    -- ^ The status of the designated job. Upon successful completion, the job's status will be @STOPPED@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopTextTranslationJobResponse' value with any optional fields omitted.
mkStopTextTranslationJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopTextTranslationJobResponse
mkStopTextTranslationJobResponse responseStatus
  = StopTextTranslationJobResponse'{jobId = Core.Nothing,
                                    jobStatus = Core.Nothing, responseStatus}

-- | The job ID of the stopped batch translation job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjrrsJobId :: Lens.Lens' StopTextTranslationJobResponse (Core.Maybe Types.JobId)
sttjrrsJobId = Lens.field @"jobId"
{-# INLINEABLE sttjrrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The status of the designated job. Upon successful completion, the job's status will be @STOPPED@ .
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjrrsJobStatus :: Lens.Lens' StopTextTranslationJobResponse (Core.Maybe Types.JobStatus)
sttjrrsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE sttjrrsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjrrsResponseStatus :: Lens.Lens' StopTextTranslationJobResponse Core.Int
sttjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sttjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
