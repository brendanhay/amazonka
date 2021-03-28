{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListAuthorizers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the authorizers registered in your account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuthorizers
    (
    -- * Creating a request
      ListAuthorizers (..)
    , mkListAuthorizers
    -- ** Request lenses
    , laAscendingOrder
    , laMarker
    , laPageSize
    , laStatus

    -- * Destructuring the response
    , ListAuthorizersResponse (..)
    , mkListAuthorizersResponse
    -- ** Response lenses
    , larrsAuthorizers
    , larrsNextMarker
    , larrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAuthorizers' smart constructor.
data ListAuthorizers = ListAuthorizers'
  { ascendingOrder :: Core.Maybe Core.Bool
    -- ^ Return the list of authorizers in ascending alphabetical order.
  , marker :: Core.Maybe Types.Marker
    -- ^ A marker used to get the next set of results.
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return at one time.
  , status :: Core.Maybe Types.AuthorizerStatus
    -- ^ The status of the list authorizers request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAuthorizers' value with any optional fields omitted.
mkListAuthorizers
    :: ListAuthorizers
mkListAuthorizers
  = ListAuthorizers'{ascendingOrder = Core.Nothing,
                     marker = Core.Nothing, pageSize = Core.Nothing,
                     status = Core.Nothing}

-- | Return the list of authorizers in ascending alphabetical order.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAscendingOrder :: Lens.Lens' ListAuthorizers (Core.Maybe Core.Bool)
laAscendingOrder = Lens.field @"ascendingOrder"
{-# INLINEABLE laAscendingOrder #-}
{-# DEPRECATED ascendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead"  #-}

-- | A marker used to get the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMarker :: Lens.Lens' ListAuthorizers (Core.Maybe Types.Marker)
laMarker = Lens.field @"marker"
{-# INLINEABLE laMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laPageSize :: Lens.Lens' ListAuthorizers (Core.Maybe Core.Natural)
laPageSize = Lens.field @"pageSize"
{-# INLINEABLE laPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The status of the list authorizers request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laStatus :: Lens.Lens' ListAuthorizers (Core.Maybe Types.AuthorizerStatus)
laStatus = Lens.field @"status"
{-# INLINEABLE laStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToQuery ListAuthorizers where
        toQuery ListAuthorizers{..}
          = Core.maybe Core.mempty (Core.toQueryPair "isAscendingOrder")
              ascendingOrder
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "pageSize") pageSize
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "status") status

instance Core.ToHeaders ListAuthorizers where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListAuthorizers where
        type Rs ListAuthorizers = ListAuthorizersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/authorizers/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAuthorizersResponse' Core.<$>
                   (x Core..:? "authorizers") Core.<*> x Core..:? "nextMarker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAuthorizers where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"authorizers" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkListAuthorizersResponse' smart constructor.
data ListAuthorizersResponse = ListAuthorizersResponse'
  { authorizers :: Core.Maybe [Types.AuthorizerSummary]
    -- ^ The authorizers.
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ A marker used to get the next set of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAuthorizersResponse' value with any optional fields omitted.
mkListAuthorizersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAuthorizersResponse
mkListAuthorizersResponse responseStatus
  = ListAuthorizersResponse'{authorizers = Core.Nothing,
                             nextMarker = Core.Nothing, responseStatus}

-- | The authorizers.
--
-- /Note:/ Consider using 'authorizers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsAuthorizers :: Lens.Lens' ListAuthorizersResponse (Core.Maybe [Types.AuthorizerSummary])
larrsAuthorizers = Lens.field @"authorizers"
{-# INLINEABLE larrsAuthorizers #-}
{-# DEPRECATED authorizers "Use generic-lens or generic-optics with 'authorizers' instead"  #-}

-- | A marker used to get the next set of results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsNextMarker :: Lens.Lens' ListAuthorizersResponse (Core.Maybe Types.NextMarker)
larrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE larrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsResponseStatus :: Lens.Lens' ListAuthorizersResponse Core.Int
larrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE larrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
