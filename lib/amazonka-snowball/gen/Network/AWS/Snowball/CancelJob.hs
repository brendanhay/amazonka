{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.CancelJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified job. You can only cancel a job before its @JobState@ value changes to @PreparingAppliance@ . Requesting the @ListJobs@ or @DescribeJob@ action returns a job's @JobState@ as part of the response element data returned.
module Network.AWS.Snowball.CancelJob
    (
    -- * Creating a request
      CancelJob (..)
    , mkCancelJob
    -- ** Request lenses
    , cjJobId

    -- * Destructuring the response
    , CancelJobResponse (..)
    , mkCancelJobResponse
    -- ** Response lenses
    , cjrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkCancelJob' smart constructor.
newtype CancelJob = CancelJob'
  { jobId :: Types.JobId
    -- ^ The 39-character job ID for the job that you want to cancel, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelJob' value with any optional fields omitted.
mkCancelJob
    :: Types.JobId -- ^ 'jobId'
    -> CancelJob
mkCancelJob jobId = CancelJob'{jobId}

-- | The 39-character job ID for the job that you want to cancel, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjJobId :: Lens.Lens' CancelJob Types.JobId
cjJobId = Lens.field @"jobId"
{-# INLINEABLE cjJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery CancelJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelJob where
        toHeaders CancelJob{..}
          = Core.pure
              ("X-Amz-Target", "AWSIESnowballJobManagementService.CancelJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CancelJob where
        toJSON CancelJob{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest CancelJob where
        type Rs CancelJob = CancelJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CancelJobResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelJobResponse' smart constructor.
newtype CancelJobResponse = CancelJobResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelJobResponse' value with any optional fields omitted.
mkCancelJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelJobResponse
mkCancelJobResponse responseStatus
  = CancelJobResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrfrsResponseStatus :: Lens.Lens' CancelJobResponse Core.Int
cjrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cjrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
