{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeSentimentDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a sentiment detection job. Use this operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeSentimentDetectionJob
    (
    -- * Creating a request
      DescribeSentimentDetectionJob (..)
    , mkDescribeSentimentDetectionJob
    -- ** Request lenses
    , dsdjJobId

    -- * Destructuring the response
    , DescribeSentimentDetectionJobResponse (..)
    , mkDescribeSentimentDetectionJobResponse
    -- ** Response lenses
    , dsdjrrsSentimentDetectionJobProperties
    , dsdjrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSentimentDetectionJob' smart constructor.
newtype DescribeSentimentDetectionJob = DescribeSentimentDetectionJob'
  { jobId :: Types.JobId
    -- ^ The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSentimentDetectionJob' value with any optional fields omitted.
mkDescribeSentimentDetectionJob
    :: Types.JobId -- ^ 'jobId'
    -> DescribeSentimentDetectionJob
mkDescribeSentimentDetectionJob jobId
  = DescribeSentimentDetectionJob'{jobId}

-- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdjJobId :: Lens.Lens' DescribeSentimentDetectionJob Types.JobId
dsdjJobId = Lens.field @"jobId"
{-# INLINEABLE dsdjJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery DescribeSentimentDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeSentimentDetectionJob where
        toHeaders DescribeSentimentDetectionJob{..}
          = Core.pure
              ("X-Amz-Target",
               "Comprehend_20171127.DescribeSentimentDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeSentimentDetectionJob where
        toJSON DescribeSentimentDetectionJob{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest DescribeSentimentDetectionJob where
        type Rs DescribeSentimentDetectionJob =
             DescribeSentimentDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeSentimentDetectionJobResponse' Core.<$>
                   (x Core..:? "SentimentDetectionJobProperties") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeSentimentDetectionJobResponse' smart constructor.
data DescribeSentimentDetectionJobResponse = DescribeSentimentDetectionJobResponse'
  { sentimentDetectionJobProperties :: Core.Maybe Types.SentimentDetectionJobProperties
    -- ^ An object that contains the properties associated with a sentiment detection job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeSentimentDetectionJobResponse' value with any optional fields omitted.
mkDescribeSentimentDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSentimentDetectionJobResponse
mkDescribeSentimentDetectionJobResponse responseStatus
  = DescribeSentimentDetectionJobResponse'{sentimentDetectionJobProperties
                                             = Core.Nothing,
                                           responseStatus}

-- | An object that contains the properties associated with a sentiment detection job.
--
-- /Note:/ Consider using 'sentimentDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdjrrsSentimentDetectionJobProperties :: Lens.Lens' DescribeSentimentDetectionJobResponse (Core.Maybe Types.SentimentDetectionJobProperties)
dsdjrrsSentimentDetectionJobProperties = Lens.field @"sentimentDetectionJobProperties"
{-# INLINEABLE dsdjrrsSentimentDetectionJobProperties #-}
{-# DEPRECATED sentimentDetectionJobProperties "Use generic-lens or generic-optics with 'sentimentDetectionJobProperties' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdjrrsResponseStatus :: Lens.Lens' DescribeSentimentDetectionJobResponse Core.Int
dsdjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsdjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
