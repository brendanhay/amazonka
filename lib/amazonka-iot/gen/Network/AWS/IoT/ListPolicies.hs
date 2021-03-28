{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your policies.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListPolicies
    (
    -- * Creating a request
      ListPolicies (..)
    , mkListPolicies
    -- ** Request lenses
    , lpAscendingOrder
    , lpMarker
    , lpPageSize

    -- * Destructuring the response
    , ListPoliciesResponse (..)
    , mkListPoliciesResponse
    -- ** Response lenses
    , lprrsNextMarker
    , lprrsPolicies
    , lprrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListPolicies operation.
--
-- /See:/ 'mkListPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { ascendingOrder :: Core.Maybe Core.Bool
    -- ^ Specifies the order for results. If true, the results are returned in ascending creation order.
  , marker :: Core.Maybe Types.Marker
    -- ^ The marker for the next set of results.
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The result page size.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPolicies' value with any optional fields omitted.
mkListPolicies
    :: ListPolicies
mkListPolicies
  = ListPolicies'{ascendingOrder = Core.Nothing,
                  marker = Core.Nothing, pageSize = Core.Nothing}

-- | Specifies the order for results. If true, the results are returned in ascending creation order.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpAscendingOrder :: Lens.Lens' ListPolicies (Core.Maybe Core.Bool)
lpAscendingOrder = Lens.field @"ascendingOrder"
{-# INLINEABLE lpAscendingOrder #-}
{-# DEPRECATED ascendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead"  #-}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMarker :: Lens.Lens' ListPolicies (Core.Maybe Types.Marker)
lpMarker = Lens.field @"marker"
{-# INLINEABLE lpMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPageSize :: Lens.Lens' ListPolicies (Core.Maybe Core.Natural)
lpPageSize = Lens.field @"pageSize"
{-# INLINEABLE lpPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

instance Core.ToQuery ListPolicies where
        toQuery ListPolicies{..}
          = Core.maybe Core.mempty (Core.toQueryPair "isAscendingOrder")
              ascendingOrder
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "pageSize") pageSize

instance Core.ToHeaders ListPolicies where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListPolicies where
        type Rs ListPolicies = ListPoliciesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/policies",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPoliciesResponse' Core.<$>
                   (x Core..:? "nextMarker") Core.<*> x Core..:? "policies" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPolicies where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"policies" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | The output from the ListPolicies operation.
--
-- /See:/ 'mkListPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { nextMarker :: Core.Maybe Types.NextMarker
    -- ^ The marker for the next set of results, or null if there are no additional results.
  , policies :: Core.Maybe [Types.Policy]
    -- ^ The descriptions of the policies.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPoliciesResponse' value with any optional fields omitted.
mkListPoliciesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPoliciesResponse
mkListPoliciesResponse responseStatus
  = ListPoliciesResponse'{nextMarker = Core.Nothing,
                          policies = Core.Nothing, responseStatus}

-- | The marker for the next set of results, or null if there are no additional results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextMarker :: Lens.Lens' ListPoliciesResponse (Core.Maybe Types.NextMarker)
lprrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lprrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The descriptions of the policies.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsPolicies :: Lens.Lens' ListPoliciesResponse (Core.Maybe [Types.Policy])
lprrsPolicies = Lens.field @"policies"
{-# INLINEABLE lprrsPolicies #-}
{-# DEPRECATED policies "Use generic-lens or generic-optics with 'policies' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListPoliciesResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
