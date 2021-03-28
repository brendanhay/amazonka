{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListPublishingDestinations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of publishing destinations associated with the specified @dectectorId@ .
module Network.AWS.GuardDuty.ListPublishingDestinations
    (
    -- * Creating a request
      ListPublishingDestinations (..)
    , mkListPublishingDestinations
    -- ** Request lenses
    , lpdDetectorId
    , lpdMaxResults
    , lpdNextToken

    -- * Destructuring the response
    , ListPublishingDestinationsResponse (..)
    , mkListPublishingDestinationsResponse
    -- ** Response lenses
    , lpdrrsDestinations
    , lpdrrsNextToken
    , lpdrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPublishingDestinations' smart constructor.
data ListPublishingDestinations = ListPublishingDestinations'
  { detectorId :: Types.DetectorId
    -- ^ The ID of the detector to retrieve publishing destinations for.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in the response.
  , nextToken :: Core.Maybe Core.Text
    -- ^ A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the @NextToken@ value returned from the previous request to continue listing results after the first page.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPublishingDestinations' value with any optional fields omitted.
mkListPublishingDestinations
    :: Types.DetectorId -- ^ 'detectorId'
    -> ListPublishingDestinations
mkListPublishingDestinations detectorId
  = ListPublishingDestinations'{detectorId,
                                maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the detector to retrieve publishing destinations for.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdDetectorId :: Lens.Lens' ListPublishingDestinations Types.DetectorId
lpdDetectorId = Lens.field @"detectorId"
{-# INLINEABLE lpdDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The maximum number of results to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdMaxResults :: Lens.Lens' ListPublishingDestinations (Core.Maybe Core.Natural)
lpdMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lpdMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the @NextToken@ value returned from the previous request to continue listing results after the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdNextToken :: Lens.Lens' ListPublishingDestinations (Core.Maybe Core.Text)
lpdNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListPublishingDestinations where
        toQuery ListPublishingDestinations{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListPublishingDestinations where
        toHeaders ListPublishingDestinations{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListPublishingDestinations where
        type Rs ListPublishingDestinations =
             ListPublishingDestinationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<>
                             "/publishingDestination",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPublishingDestinationsResponse' Core.<$>
                   (x Core..:? "destinations" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListPublishingDestinationsResponse' smart constructor.
data ListPublishingDestinationsResponse = ListPublishingDestinationsResponse'
  { destinations :: [Types.Destination]
    -- ^ A @Destinations@ object that includes information about each publishing destination returned.
  , nextToken :: Core.Maybe Core.Text
    -- ^ A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the @NextToken@ value returned from the previous request to continue listing results after the first page.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPublishingDestinationsResponse' value with any optional fields omitted.
mkListPublishingDestinationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPublishingDestinationsResponse
mkListPublishingDestinationsResponse responseStatus
  = ListPublishingDestinationsResponse'{destinations = Core.mempty,
                                        nextToken = Core.Nothing, responseStatus}

-- | A @Destinations@ object that includes information about each publishing destination returned.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdrrsDestinations :: Lens.Lens' ListPublishingDestinationsResponse [Types.Destination]
lpdrrsDestinations = Lens.field @"destinations"
{-# INLINEABLE lpdrrsDestinations #-}
{-# DEPRECATED destinations "Use generic-lens or generic-optics with 'destinations' instead"  #-}

-- | A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the @NextToken@ value returned from the previous request to continue listing results after the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdrrsNextToken :: Lens.Lens' ListPublishingDestinationsResponse (Core.Maybe Core.Text)
lpdrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpdrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdrrsResponseStatus :: Lens.Lens' ListPublishingDestinationsResponse Core.Int
lpdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lpdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
