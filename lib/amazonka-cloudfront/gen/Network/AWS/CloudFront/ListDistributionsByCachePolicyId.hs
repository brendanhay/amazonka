{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListDistributionsByCachePolicyId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distribution IDs for distributions that have a cache behavior that’s associated with the specified cache policy.
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByCachePolicyId
    (
    -- * Creating a request
      ListDistributionsByCachePolicyId (..)
    , mkListDistributionsByCachePolicyId
    -- ** Request lenses
    , ldbcpiCachePolicyId
    , ldbcpiMarker
    , ldbcpiMaxItems

    -- * Destructuring the response
    , ListDistributionsByCachePolicyIdResponse (..)
    , mkListDistributionsByCachePolicyIdResponse
    -- ** Response lenses
    , ldbcpirrsDistributionIdList
    , ldbcpirrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDistributionsByCachePolicyId' smart constructor.
data ListDistributionsByCachePolicyId = ListDistributionsByCachePolicyId'
  { cachePolicyId :: Core.Text
    -- ^ The ID of the cache policy whose associated distribution IDs you want to list.
  , marker :: Core.Maybe Core.Text
    -- ^ Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
  , maxItems :: Core.Maybe Core.Text
    -- ^ The maximum number of distribution IDs that you want in the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDistributionsByCachePolicyId' value with any optional fields omitted.
mkListDistributionsByCachePolicyId
    :: Core.Text -- ^ 'cachePolicyId'
    -> ListDistributionsByCachePolicyId
mkListDistributionsByCachePolicyId cachePolicyId
  = ListDistributionsByCachePolicyId'{cachePolicyId,
                                      marker = Core.Nothing, maxItems = Core.Nothing}

-- | The ID of the cache policy whose associated distribution IDs you want to list.
--
-- /Note:/ Consider using 'cachePolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbcpiCachePolicyId :: Lens.Lens' ListDistributionsByCachePolicyId Core.Text
ldbcpiCachePolicyId = Lens.field @"cachePolicyId"
{-# INLINEABLE ldbcpiCachePolicyId #-}
{-# DEPRECATED cachePolicyId "Use generic-lens or generic-optics with 'cachePolicyId' instead"  #-}

-- | Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbcpiMarker :: Lens.Lens' ListDistributionsByCachePolicyId (Core.Maybe Core.Text)
ldbcpiMarker = Lens.field @"marker"
{-# INLINEABLE ldbcpiMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of distribution IDs that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbcpiMaxItems :: Lens.Lens' ListDistributionsByCachePolicyId (Core.Maybe Core.Text)
ldbcpiMaxItems = Lens.field @"maxItems"
{-# INLINEABLE ldbcpiMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListDistributionsByCachePolicyId where
        toQuery ListDistributionsByCachePolicyId{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Marker") marker Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListDistributionsByCachePolicyId where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListDistributionsByCachePolicyId where
        type Rs ListDistributionsByCachePolicyId =
             ListDistributionsByCachePolicyIdResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/distributionsByCachePolicyId/" Core.<>
                             Core.toText cachePolicyId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListDistributionsByCachePolicyIdResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListDistributionsByCachePolicyIdResponse' smart constructor.
data ListDistributionsByCachePolicyIdResponse = ListDistributionsByCachePolicyIdResponse'
  { distributionIdList :: Core.Maybe Types.DistributionIdList
    -- ^ A list of distribution IDs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDistributionsByCachePolicyIdResponse' value with any optional fields omitted.
mkListDistributionsByCachePolicyIdResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDistributionsByCachePolicyIdResponse
mkListDistributionsByCachePolicyIdResponse responseStatus
  = ListDistributionsByCachePolicyIdResponse'{distributionIdList =
                                                Core.Nothing,
                                              responseStatus}

-- | A list of distribution IDs.
--
-- /Note:/ Consider using 'distributionIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbcpirrsDistributionIdList :: Lens.Lens' ListDistributionsByCachePolicyIdResponse (Core.Maybe Types.DistributionIdList)
ldbcpirrsDistributionIdList = Lens.field @"distributionIdList"
{-# INLINEABLE ldbcpirrsDistributionIdList #-}
{-# DEPRECATED distributionIdList "Use generic-lens or generic-optics with 'distributionIdList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbcpirrsResponseStatus :: Lens.Lens' ListDistributionsByCachePolicyIdResponse Core.Int
ldbcpirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldbcpirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
