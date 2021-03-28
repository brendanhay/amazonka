{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribePiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a PII entities detection job. For example, you can use this operation to get the job status.
module Network.AWS.Comprehend.DescribePiiEntitiesDetectionJob
    (
    -- * Creating a request
      DescribePiiEntitiesDetectionJob (..)
    , mkDescribePiiEntitiesDetectionJob
    -- ** Request lenses
    , dpedjJobId

    -- * Destructuring the response
    , DescribePiiEntitiesDetectionJobResponse (..)
    , mkDescribePiiEntitiesDetectionJobResponse
    -- ** Response lenses
    , dpedjrrsPiiEntitiesDetectionJobProperties
    , dpedjrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePiiEntitiesDetectionJob' smart constructor.
newtype DescribePiiEntitiesDetectionJob = DescribePiiEntitiesDetectionJob'
  { jobId :: Types.JobId
    -- ^ The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePiiEntitiesDetectionJob' value with any optional fields omitted.
mkDescribePiiEntitiesDetectionJob
    :: Types.JobId -- ^ 'jobId'
    -> DescribePiiEntitiesDetectionJob
mkDescribePiiEntitiesDetectionJob jobId
  = DescribePiiEntitiesDetectionJob'{jobId}

-- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpedjJobId :: Lens.Lens' DescribePiiEntitiesDetectionJob Types.JobId
dpedjJobId = Lens.field @"jobId"
{-# INLINEABLE dpedjJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery DescribePiiEntitiesDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribePiiEntitiesDetectionJob where
        toHeaders DescribePiiEntitiesDetectionJob{..}
          = Core.pure
              ("X-Amz-Target",
               "Comprehend_20171127.DescribePiiEntitiesDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribePiiEntitiesDetectionJob where
        toJSON DescribePiiEntitiesDetectionJob{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest DescribePiiEntitiesDetectionJob where
        type Rs DescribePiiEntitiesDetectionJob =
             DescribePiiEntitiesDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribePiiEntitiesDetectionJobResponse' Core.<$>
                   (x Core..:? "PiiEntitiesDetectionJobProperties") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribePiiEntitiesDetectionJobResponse' smart constructor.
data DescribePiiEntitiesDetectionJobResponse = DescribePiiEntitiesDetectionJobResponse'
  { piiEntitiesDetectionJobProperties :: Core.Maybe Types.PiiEntitiesDetectionJobProperties
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribePiiEntitiesDetectionJobResponse' value with any optional fields omitted.
mkDescribePiiEntitiesDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePiiEntitiesDetectionJobResponse
mkDescribePiiEntitiesDetectionJobResponse responseStatus
  = DescribePiiEntitiesDetectionJobResponse'{piiEntitiesDetectionJobProperties
                                               = Core.Nothing,
                                             responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'piiEntitiesDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpedjrrsPiiEntitiesDetectionJobProperties :: Lens.Lens' DescribePiiEntitiesDetectionJobResponse (Core.Maybe Types.PiiEntitiesDetectionJobProperties)
dpedjrrsPiiEntitiesDetectionJobProperties = Lens.field @"piiEntitiesDetectionJobProperties"
{-# INLINEABLE dpedjrrsPiiEntitiesDetectionJobProperties #-}
{-# DEPRECATED piiEntitiesDetectionJobProperties "Use generic-lens or generic-optics with 'piiEntitiesDetectionJobProperties' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpedjrrsResponseStatus :: Lens.Lens' DescribePiiEntitiesDetectionJobResponse Core.Int
dpedjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpedjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
