{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListMultiplexPrograms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the programs that currently exist for a specific multiplex.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListMultiplexPrograms
    (
    -- * Creating a request
      ListMultiplexPrograms (..)
    , mkListMultiplexPrograms
    -- ** Request lenses
    , lmpMultiplexId
    , lmpMaxResults
    , lmpNextToken

    -- * Destructuring the response
    , ListMultiplexProgramsResponse (..)
    , mkListMultiplexProgramsResponse
    -- ** Response lenses
    , lmprrsMultiplexPrograms
    , lmprrsNextToken
    , lmprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListMultiplexProgramsRequest
--
-- /See:/ 'mkListMultiplexPrograms' smart constructor.
data ListMultiplexPrograms = ListMultiplexPrograms'
  { multiplexId :: Core.Text
    -- ^ The ID of the multiplex that the programs belong to.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to retrieve the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMultiplexPrograms' value with any optional fields omitted.
mkListMultiplexPrograms
    :: Core.Text -- ^ 'multiplexId'
    -> ListMultiplexPrograms
mkListMultiplexPrograms multiplexId
  = ListMultiplexPrograms'{multiplexId, maxResults = Core.Nothing,
                           nextToken = Core.Nothing}

-- | The ID of the multiplex that the programs belong to.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpMultiplexId :: Lens.Lens' ListMultiplexPrograms Core.Text
lmpMultiplexId = Lens.field @"multiplexId"
{-# INLINEABLE lmpMultiplexId #-}
{-# DEPRECATED multiplexId "Use generic-lens or generic-optics with 'multiplexId' instead"  #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpMaxResults :: Lens.Lens' ListMultiplexPrograms (Core.Maybe Core.Natural)
lmpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lmpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpNextToken :: Lens.Lens' ListMultiplexPrograms (Core.Maybe Core.Text)
lmpNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListMultiplexPrograms where
        toQuery ListMultiplexPrograms{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListMultiplexPrograms where
        toHeaders ListMultiplexPrograms{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListMultiplexPrograms where
        type Rs ListMultiplexPrograms = ListMultiplexProgramsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/prod/multiplexes/" Core.<> Core.toText multiplexId Core.<>
                             "/programs",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListMultiplexProgramsResponse' Core.<$>
                   (x Core..:? "multiplexPrograms") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListMultiplexPrograms where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"multiplexPrograms" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Placeholder documentation for ListMultiplexProgramsResponse
--
-- /See:/ 'mkListMultiplexProgramsResponse' smart constructor.
data ListMultiplexProgramsResponse = ListMultiplexProgramsResponse'
  { multiplexPrograms :: Core.Maybe [Types.MultiplexProgramSummary]
    -- ^ List of multiplex programs.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Token for the next ListMultiplexProgram request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMultiplexProgramsResponse' value with any optional fields omitted.
mkListMultiplexProgramsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListMultiplexProgramsResponse
mkListMultiplexProgramsResponse responseStatus
  = ListMultiplexProgramsResponse'{multiplexPrograms = Core.Nothing,
                                   nextToken = Core.Nothing, responseStatus}

-- | List of multiplex programs.
--
-- /Note:/ Consider using 'multiplexPrograms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprrsMultiplexPrograms :: Lens.Lens' ListMultiplexProgramsResponse (Core.Maybe [Types.MultiplexProgramSummary])
lmprrsMultiplexPrograms = Lens.field @"multiplexPrograms"
{-# INLINEABLE lmprrsMultiplexPrograms #-}
{-# DEPRECATED multiplexPrograms "Use generic-lens or generic-optics with 'multiplexPrograms' instead"  #-}

-- | Token for the next ListMultiplexProgram request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprrsNextToken :: Lens.Lens' ListMultiplexProgramsResponse (Core.Maybe Core.Text)
lmprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprrsResponseStatus :: Lens.Lens' ListMultiplexProgramsResponse Core.Int
lmprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lmprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
