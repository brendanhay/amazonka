{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetOfferingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the current status and future status of all offerings purchased by an AWS account. The response indicates how many offerings are currently available and the offerings that will be available in the next period. The API returns a @NotEligible@ error if the user is not permitted to invoke the operation. If you must be able to invoke this operation, contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> .
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.GetOfferingStatus
    (
    -- * Creating a request
      GetOfferingStatus (..)
    , mkGetOfferingStatus
    -- ** Request lenses
    , gosNextToken

    -- * Destructuring the response
    , GetOfferingStatusResponse (..)
    , mkGetOfferingStatusResponse
    -- ** Response lenses
    , gosrrsCurrent
    , gosrrsNextPeriod
    , gosrrsNextToken
    , gosrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to retrieve the offering status for the specified customer or account.
--
-- /See:/ 'mkGetOfferingStatus' smart constructor.
newtype GetOfferingStatus = GetOfferingStatus'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetOfferingStatus' value with any optional fields omitted.
mkGetOfferingStatus
    :: GetOfferingStatus
mkGetOfferingStatus = GetOfferingStatus'{nextToken = Core.Nothing}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosNextToken :: Lens.Lens' GetOfferingStatus (Core.Maybe Types.PaginationToken)
gosNextToken = Lens.field @"nextToken"
{-# INLINEABLE gosNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetOfferingStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetOfferingStatus where
        toHeaders GetOfferingStatus{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.GetOfferingStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetOfferingStatus where
        toJSON GetOfferingStatus{..}
          = Core.object
              (Core.catMaybes [("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetOfferingStatus where
        type Rs GetOfferingStatus = GetOfferingStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetOfferingStatusResponse' Core.<$>
                   (x Core..:? "current") Core.<*> x Core..:? "nextPeriod" Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetOfferingStatus where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"current" Core.. Lens._Just) =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"nextPeriod" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Returns the status result for a device offering.
--
-- /See:/ 'mkGetOfferingStatusResponse' smart constructor.
data GetOfferingStatusResponse = GetOfferingStatusResponse'
  { current :: Core.Maybe (Core.HashMap Types.OfferingIdentifier Types.OfferingStatus)
    -- ^ When specified, gets the offering status for the current period.
  , nextPeriod :: Core.Maybe (Core.HashMap Types.OfferingIdentifier Types.OfferingStatus)
    -- ^ When specified, gets the offering status for the next period.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetOfferingStatusResponse' value with any optional fields omitted.
mkGetOfferingStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetOfferingStatusResponse
mkGetOfferingStatusResponse responseStatus
  = GetOfferingStatusResponse'{current = Core.Nothing,
                               nextPeriod = Core.Nothing, nextToken = Core.Nothing,
                               responseStatus}

-- | When specified, gets the offering status for the current period.
--
-- /Note:/ Consider using 'current' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrrsCurrent :: Lens.Lens' GetOfferingStatusResponse (Core.Maybe (Core.HashMap Types.OfferingIdentifier Types.OfferingStatus))
gosrrsCurrent = Lens.field @"current"
{-# INLINEABLE gosrrsCurrent #-}
{-# DEPRECATED current "Use generic-lens or generic-optics with 'current' instead"  #-}

-- | When specified, gets the offering status for the next period.
--
-- /Note:/ Consider using 'nextPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrrsNextPeriod :: Lens.Lens' GetOfferingStatusResponse (Core.Maybe (Core.HashMap Types.OfferingIdentifier Types.OfferingStatus))
gosrrsNextPeriod = Lens.field @"nextPeriod"
{-# INLINEABLE gosrrsNextPeriod #-}
{-# DEPRECATED nextPeriod "Use generic-lens or generic-optics with 'nextPeriod' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrrsNextToken :: Lens.Lens' GetOfferingStatusResponse (Core.Maybe Types.PaginationToken)
gosrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gosrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrrsResponseStatus :: Lens.Lens' GetOfferingStatusResponse Core.Int
gosrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gosrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
