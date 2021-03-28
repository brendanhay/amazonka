{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeKeyPhrasesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a key phrases detection job. Use this operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeKeyPhrasesDetectionJob
    (
    -- * Creating a request
      DescribeKeyPhrasesDetectionJob (..)
    , mkDescribeKeyPhrasesDetectionJob
    -- ** Request lenses
    , dkpdjJobId

    -- * Destructuring the response
    , DescribeKeyPhrasesDetectionJobResponse (..)
    , mkDescribeKeyPhrasesDetectionJobResponse
    -- ** Response lenses
    , dkpdjrrsKeyPhrasesDetectionJobProperties
    , dkpdjrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeKeyPhrasesDetectionJob' smart constructor.
newtype DescribeKeyPhrasesDetectionJob = DescribeKeyPhrasesDetectionJob'
  { jobId :: Types.JobId
    -- ^ The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeKeyPhrasesDetectionJob' value with any optional fields omitted.
mkDescribeKeyPhrasesDetectionJob
    :: Types.JobId -- ^ 'jobId'
    -> DescribeKeyPhrasesDetectionJob
mkDescribeKeyPhrasesDetectionJob jobId
  = DescribeKeyPhrasesDetectionJob'{jobId}

-- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpdjJobId :: Lens.Lens' DescribeKeyPhrasesDetectionJob Types.JobId
dkpdjJobId = Lens.field @"jobId"
{-# INLINEABLE dkpdjJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery DescribeKeyPhrasesDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeKeyPhrasesDetectionJob where
        toHeaders DescribeKeyPhrasesDetectionJob{..}
          = Core.pure
              ("X-Amz-Target",
               "Comprehend_20171127.DescribeKeyPhrasesDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeKeyPhrasesDetectionJob where
        toJSON DescribeKeyPhrasesDetectionJob{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest DescribeKeyPhrasesDetectionJob where
        type Rs DescribeKeyPhrasesDetectionJob =
             DescribeKeyPhrasesDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeKeyPhrasesDetectionJobResponse' Core.<$>
                   (x Core..:? "KeyPhrasesDetectionJobProperties") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeKeyPhrasesDetectionJobResponse' smart constructor.
data DescribeKeyPhrasesDetectionJobResponse = DescribeKeyPhrasesDetectionJobResponse'
  { keyPhrasesDetectionJobProperties :: Core.Maybe Types.KeyPhrasesDetectionJobProperties
    -- ^ An object that contains the properties associated with a key phrases detection job. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeKeyPhrasesDetectionJobResponse' value with any optional fields omitted.
mkDescribeKeyPhrasesDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeKeyPhrasesDetectionJobResponse
mkDescribeKeyPhrasesDetectionJobResponse responseStatus
  = DescribeKeyPhrasesDetectionJobResponse'{keyPhrasesDetectionJobProperties
                                              = Core.Nothing,
                                            responseStatus}

-- | An object that contains the properties associated with a key phrases detection job. 
--
-- /Note:/ Consider using 'keyPhrasesDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpdjrrsKeyPhrasesDetectionJobProperties :: Lens.Lens' DescribeKeyPhrasesDetectionJobResponse (Core.Maybe Types.KeyPhrasesDetectionJobProperties)
dkpdjrrsKeyPhrasesDetectionJobProperties = Lens.field @"keyPhrasesDetectionJobProperties"
{-# INLINEABLE dkpdjrrsKeyPhrasesDetectionJobProperties #-}
{-# DEPRECATED keyPhrasesDetectionJobProperties "Use generic-lens or generic-optics with 'keyPhrasesDetectionJobProperties' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpdjrrsResponseStatus :: Lens.Lens' DescribeKeyPhrasesDetectionJobResponse Core.Int
dkpdjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dkpdjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
