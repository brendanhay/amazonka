{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.ListOriginEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of OriginEndpoint records.
--
-- This operation returns paginated results.
module Network.AWS.MediaPackage.ListOriginEndpoints
    (
    -- * Creating a request
      ListOriginEndpoints (..)
    , mkListOriginEndpoints
    -- ** Request lenses
    , loeChannelId
    , loeMaxResults
    , loeNextToken

    -- * Destructuring the response
    , ListOriginEndpointsResponse (..)
    , mkListOriginEndpointsResponse
    -- ** Response lenses
    , loerrsNextToken
    , loerrsOriginEndpoints
    , loerrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListOriginEndpoints' smart constructor.
data ListOriginEndpoints = ListOriginEndpoints'
  { channelId :: Core.Maybe Core.Text
    -- ^ When specified, the request will return only OriginEndpoints associated with the given Channel ID.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The upper bound on the number of records to return.
  , nextToken :: Core.Maybe Core.Text
    -- ^ A token used to resume pagination from the end of a previous request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOriginEndpoints' value with any optional fields omitted.
mkListOriginEndpoints
    :: ListOriginEndpoints
mkListOriginEndpoints
  = ListOriginEndpoints'{channelId = Core.Nothing,
                         maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | When specified, the request will return only OriginEndpoints associated with the given Channel ID.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loeChannelId :: Lens.Lens' ListOriginEndpoints (Core.Maybe Core.Text)
loeChannelId = Lens.field @"channelId"
{-# INLINEABLE loeChannelId #-}
{-# DEPRECATED channelId "Use generic-lens or generic-optics with 'channelId' instead"  #-}

-- | The upper bound on the number of records to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loeMaxResults :: Lens.Lens' ListOriginEndpoints (Core.Maybe Core.Natural)
loeMaxResults = Lens.field @"maxResults"
{-# INLINEABLE loeMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token used to resume pagination from the end of a previous request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loeNextToken :: Lens.Lens' ListOriginEndpoints (Core.Maybe Core.Text)
loeNextToken = Lens.field @"nextToken"
{-# INLINEABLE loeNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListOriginEndpoints where
        toQuery ListOriginEndpoints{..}
          = Core.maybe Core.mempty (Core.toQueryPair "channelId") channelId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListOriginEndpoints where
        toHeaders ListOriginEndpoints{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListOriginEndpoints where
        type Rs ListOriginEndpoints = ListOriginEndpointsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/origin_endpoints",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListOriginEndpointsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "originEndpoints"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListOriginEndpoints where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"originEndpoints" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListOriginEndpointsResponse' smart constructor.
data ListOriginEndpointsResponse = ListOriginEndpointsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ A token that can be used to resume pagination from the end of the collection.
  , originEndpoints :: Core.Maybe [Types.OriginEndpoint]
    -- ^ A list of OriginEndpoint records.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOriginEndpointsResponse' value with any optional fields omitted.
mkListOriginEndpointsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListOriginEndpointsResponse
mkListOriginEndpointsResponse responseStatus
  = ListOriginEndpointsResponse'{nextToken = Core.Nothing,
                                 originEndpoints = Core.Nothing, responseStatus}

-- | A token that can be used to resume pagination from the end of the collection.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loerrsNextToken :: Lens.Lens' ListOriginEndpointsResponse (Core.Maybe Core.Text)
loerrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE loerrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of OriginEndpoint records.
--
-- /Note:/ Consider using 'originEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loerrsOriginEndpoints :: Lens.Lens' ListOriginEndpointsResponse (Core.Maybe [Types.OriginEndpoint])
loerrsOriginEndpoints = Lens.field @"originEndpoints"
{-# INLINEABLE loerrsOriginEndpoints #-}
{-# DEPRECATED originEndpoints "Use generic-lens or generic-optics with 'originEndpoints' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loerrsResponseStatus :: Lens.Lens' ListOriginEndpointsResponse Core.Int
loerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE loerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
