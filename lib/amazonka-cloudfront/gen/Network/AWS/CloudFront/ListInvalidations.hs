{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListInvalidations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists invalidation batches. 
--
-- This operation returns paginated results.
module Network.AWS.CloudFront.ListInvalidations
    (
    -- * Creating a request
      ListInvalidations (..)
    , mkListInvalidations
    -- ** Request lenses
    , liDistributionId
    , liMarker
    , liMaxItems

    -- * Destructuring the response
    , ListInvalidationsResponse (..)
    , mkListInvalidationsResponse
    -- ** Response lenses
    , lirrsInvalidationList
    , lirrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to list invalidations. 
--
-- /See:/ 'mkListInvalidations' smart constructor.
data ListInvalidations = ListInvalidations'
  { distributionId :: Core.Text
    -- ^ The distribution's ID.
  , marker :: Core.Maybe Core.Text
    -- ^ Use this parameter when paginating results to indicate where to begin in your list of invalidation batches. Because the results are returned in decreasing order from most recent to oldest, the most recent results are on the first page, the second page will contain earlier results, and so on. To get the next page of results, set @Marker@ to the value of the @NextMarker@ from the current page's response. This value is the same as the ID of the last invalidation batch on that page. 
  , maxItems :: Core.Maybe Core.Text
    -- ^ The maximum number of invalidation batches that you want in the response body.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInvalidations' value with any optional fields omitted.
mkListInvalidations
    :: Core.Text -- ^ 'distributionId'
    -> ListInvalidations
mkListInvalidations distributionId
  = ListInvalidations'{distributionId, marker = Core.Nothing,
                       maxItems = Core.Nothing}

-- | The distribution's ID.
--
-- /Note:/ Consider using 'distributionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liDistributionId :: Lens.Lens' ListInvalidations Core.Text
liDistributionId = Lens.field @"distributionId"
{-# INLINEABLE liDistributionId #-}
{-# DEPRECATED distributionId "Use generic-lens or generic-optics with 'distributionId' instead"  #-}

-- | Use this parameter when paginating results to indicate where to begin in your list of invalidation batches. Because the results are returned in decreasing order from most recent to oldest, the most recent results are on the first page, the second page will contain earlier results, and so on. To get the next page of results, set @Marker@ to the value of the @NextMarker@ from the current page's response. This value is the same as the ID of the last invalidation batch on that page. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMarker :: Lens.Lens' ListInvalidations (Core.Maybe Core.Text)
liMarker = Lens.field @"marker"
{-# INLINEABLE liMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of invalidation batches that you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxItems :: Lens.Lens' ListInvalidations (Core.Maybe Core.Text)
liMaxItems = Lens.field @"maxItems"
{-# INLINEABLE liMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListInvalidations where
        toQuery ListInvalidations{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Marker") marker Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListInvalidations where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListInvalidations where
        type Rs ListInvalidations = ListInvalidationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/distribution/" Core.<> Core.toText distributionId
                             Core.<> "/invalidation",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListInvalidationsResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListInvalidations where
        page rq rs
          | Pager.stop
              (rs Lens.^.
                 Lens.field @"invalidationList" Core.. Lens.field @"isTruncated")
            = Core.Nothing
          | Core.isNothing
              (rs Lens.^.
                 Lens.field @"invalidationList" Core.. Lens.field @"nextMarker")
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~
                   rs Lens.^.
                     Lens.field @"invalidationList" Core.. Lens.field @"nextMarker")

-- | The returned result of the corresponding request. 
--
-- /See:/ 'mkListInvalidationsResponse' smart constructor.
data ListInvalidationsResponse = ListInvalidationsResponse'
  { invalidationList :: Types.InvalidationList
    -- ^ Information about invalidation batches. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListInvalidationsResponse' value with any optional fields omitted.
mkListInvalidationsResponse
    :: Types.InvalidationList -- ^ 'invalidationList'
    -> Core.Int -- ^ 'responseStatus'
    -> ListInvalidationsResponse
mkListInvalidationsResponse invalidationList responseStatus
  = ListInvalidationsResponse'{invalidationList, responseStatus}

-- | Information about invalidation batches. 
--
-- /Note:/ Consider using 'invalidationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsInvalidationList :: Lens.Lens' ListInvalidationsResponse Types.InvalidationList
lirrsInvalidationList = Lens.field @"invalidationList"
{-# INLINEABLE lirrsInvalidationList #-}
{-# DEPRECATED invalidationList "Use generic-lens or generic-optics with 'invalidationList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsResponseStatus :: Lens.Lens' ListInvalidationsResponse Core.Int
lirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
