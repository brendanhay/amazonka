{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.DescribeJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific job including shipping information, job status, and other important metadata. 
module Network.AWS.Snowball.DescribeJob
    (
    -- * Creating a request
      DescribeJob (..)
    , mkDescribeJob
    -- ** Request lenses
    , djJobId

    -- * Destructuring the response
    , DescribeJobResponse (..)
    , mkDescribeJobResponse
    -- ** Response lenses
    , djrrsJobMetadata
    , djrrsSubJobMetadata
    , djrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkDescribeJob' smart constructor.
newtype DescribeJob = DescribeJob'
  { jobId :: Types.JobId
    -- ^ The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeJob' value with any optional fields omitted.
mkDescribeJob
    :: Types.JobId -- ^ 'jobId'
    -> DescribeJob
mkDescribeJob jobId = DescribeJob'{jobId}

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djJobId :: Lens.Lens' DescribeJob Types.JobId
djJobId = Lens.field @"jobId"
{-# INLINEABLE djJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery DescribeJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeJob where
        toHeaders DescribeJob{..}
          = Core.pure
              ("X-Amz-Target", "AWSIESnowballJobManagementService.DescribeJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeJob where
        toJSON DescribeJob{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest DescribeJob where
        type Rs DescribeJob = DescribeJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeJobResponse' Core.<$>
                   (x Core..:? "JobMetadata") Core.<*> x Core..:? "SubJobMetadata"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeJobResponse' smart constructor.
data DescribeJobResponse = DescribeJobResponse'
  { jobMetadata :: Core.Maybe Types.JobMetadata
    -- ^ Information about a specific job, including shipping information, job status, and other important metadata.
  , subJobMetadata :: Core.Maybe [Types.JobMetadata]
    -- ^ Information about a specific job part (in the case of an export job), including shipping information, job status, and other important metadata.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeJobResponse' value with any optional fields omitted.
mkDescribeJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeJobResponse
mkDescribeJobResponse responseStatus
  = DescribeJobResponse'{jobMetadata = Core.Nothing,
                         subJobMetadata = Core.Nothing, responseStatus}

-- | Information about a specific job, including shipping information, job status, and other important metadata.
--
-- /Note:/ Consider using 'jobMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrrsJobMetadata :: Lens.Lens' DescribeJobResponse (Core.Maybe Types.JobMetadata)
djrrsJobMetadata = Lens.field @"jobMetadata"
{-# INLINEABLE djrrsJobMetadata #-}
{-# DEPRECATED jobMetadata "Use generic-lens or generic-optics with 'jobMetadata' instead"  #-}

-- | Information about a specific job part (in the case of an export job), including shipping information, job status, and other important metadata.
--
-- /Note:/ Consider using 'subJobMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrrsSubJobMetadata :: Lens.Lens' DescribeJobResponse (Core.Maybe [Types.JobMetadata])
djrrsSubJobMetadata = Lens.field @"subJobMetadata"
{-# INLINEABLE djrrsSubJobMetadata #-}
{-# DEPRECATED subJobMetadata "Use generic-lens or generic-optics with 'subJobMetadata' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrrsResponseStatus :: Lens.Lens' DescribeJobResponse Core.Int
djrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE djrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
