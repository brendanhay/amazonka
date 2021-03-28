{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the streams in your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListStreams
    (
    -- * Creating a request
      ListStreams (..)
    , mkListStreams
    -- ** Request lenses
    , lsAscendingOrder
    , lsMaxResults
    , lsNextToken

    -- * Destructuring the response
    , ListStreamsResponse (..)
    , mkListStreamsResponse
    -- ** Response lenses
    , lsrrsNextToken
    , lsrrsStreams
    , lsrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListStreams' smart constructor.
data ListStreams = ListStreams'
  { ascendingOrder :: Core.Maybe Core.Bool
    -- ^ Set to true to return the list of streams in ascending order.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return at a time.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token used to get the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStreams' value with any optional fields omitted.
mkListStreams
    :: ListStreams
mkListStreams
  = ListStreams'{ascendingOrder = Core.Nothing,
                 maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Set to true to return the list of streams in ascending order.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsAscendingOrder :: Lens.Lens' ListStreams (Core.Maybe Core.Bool)
lsAscendingOrder = Lens.field @"ascendingOrder"
{-# INLINEABLE lsAscendingOrder #-}
{-# DEPRECATED ascendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead"  #-}

-- | The maximum number of results to return at a time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMaxResults :: Lens.Lens' ListStreams (Core.Maybe Core.Natural)
lsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token used to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListStreams (Core.Maybe Types.NextToken)
lsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListStreams where
        toQuery ListStreams{..}
          = Core.maybe Core.mempty (Core.toQueryPair "isAscendingOrder")
              ascendingOrder
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListStreams where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListStreams where
        type Rs ListStreams = ListStreamsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/streams",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListStreamsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "streams" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListStreams where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"streams" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A token used to get the next set of results.
  , streams :: Core.Maybe [Types.StreamSummary]
    -- ^ A list of streams.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStreamsResponse' value with any optional fields omitted.
mkListStreamsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListStreamsResponse
mkListStreamsResponse responseStatus
  = ListStreamsResponse'{nextToken = Core.Nothing,
                         streams = Core.Nothing, responseStatus}

-- | A token used to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsNextToken :: Lens.Lens' ListStreamsResponse (Core.Maybe Types.NextToken)
lsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of streams.
--
-- /Note:/ Consider using 'streams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsStreams :: Lens.Lens' ListStreamsResponse (Core.Maybe [Types.StreamSummary])
lsrrsStreams = Lens.field @"streams"
{-# INLINEABLE lsrrsStreams #-}
{-# DEPRECATED streams "Use generic-lens or generic-optics with 'streams' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsResponseStatus :: Lens.Lens' ListStreamsResponse Core.Int
lsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
