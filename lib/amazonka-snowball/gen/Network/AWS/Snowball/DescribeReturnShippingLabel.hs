{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.DescribeReturnShippingLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information on the shipping label of a Snow device that is being returned to AWS.
module Network.AWS.Snowball.DescribeReturnShippingLabel
    (
    -- * Creating a request
      DescribeReturnShippingLabel (..)
    , mkDescribeReturnShippingLabel
    -- ** Request lenses
    , drslJobId

    -- * Destructuring the response
    , DescribeReturnShippingLabelResponse (..)
    , mkDescribeReturnShippingLabelResponse
    -- ** Response lenses
    , drslrrsExpirationDate
    , drslrrsStatus
    , drslrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkDescribeReturnShippingLabel' smart constructor.
newtype DescribeReturnShippingLabel = DescribeReturnShippingLabel'
  { jobId :: Core.Maybe Types.JobId
    -- ^ The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReturnShippingLabel' value with any optional fields omitted.
mkDescribeReturnShippingLabel
    :: DescribeReturnShippingLabel
mkDescribeReturnShippingLabel
  = DescribeReturnShippingLabel'{jobId = Core.Nothing}

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drslJobId :: Lens.Lens' DescribeReturnShippingLabel (Core.Maybe Types.JobId)
drslJobId = Lens.field @"jobId"
{-# INLINEABLE drslJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery DescribeReturnShippingLabel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeReturnShippingLabel where
        toHeaders DescribeReturnShippingLabel{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSIESnowballJobManagementService.DescribeReturnShippingLabel")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeReturnShippingLabel where
        toJSON DescribeReturnShippingLabel{..}
          = Core.object (Core.catMaybes [("JobId" Core..=) Core.<$> jobId])

instance Core.AWSRequest DescribeReturnShippingLabel where
        type Rs DescribeReturnShippingLabel =
             DescribeReturnShippingLabelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeReturnShippingLabelResponse' Core.<$>
                   (x Core..:? "ExpirationDate") Core.<*> x Core..:? "Status" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeReturnShippingLabelResponse' smart constructor.
data DescribeReturnShippingLabelResponse = DescribeReturnShippingLabelResponse'
  { expirationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The expiration date of the current return shipping label.
  , status :: Core.Maybe Types.ShippingLabelStatus
    -- ^ The status information of the task on a Snow device that is being returned to AWS.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeReturnShippingLabelResponse' value with any optional fields omitted.
mkDescribeReturnShippingLabelResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeReturnShippingLabelResponse
mkDescribeReturnShippingLabelResponse responseStatus
  = DescribeReturnShippingLabelResponse'{expirationDate =
                                           Core.Nothing,
                                         status = Core.Nothing, responseStatus}

-- | The expiration date of the current return shipping label.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drslrrsExpirationDate :: Lens.Lens' DescribeReturnShippingLabelResponse (Core.Maybe Core.NominalDiffTime)
drslrrsExpirationDate = Lens.field @"expirationDate"
{-# INLINEABLE drslrrsExpirationDate #-}
{-# DEPRECATED expirationDate "Use generic-lens or generic-optics with 'expirationDate' instead"  #-}

-- | The status information of the task on a Snow device that is being returned to AWS.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drslrrsStatus :: Lens.Lens' DescribeReturnShippingLabelResponse (Core.Maybe Types.ShippingLabelStatus)
drslrrsStatus = Lens.field @"status"
{-# INLINEABLE drslrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drslrrsResponseStatus :: Lens.Lens' DescribeReturnShippingLabelResponse Core.Int
drslrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drslrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
