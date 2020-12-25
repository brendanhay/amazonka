{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListMultiplexes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of the existing multiplexes.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListMultiplexes
  ( -- * Creating a request
    ListMultiplexes (..),
    mkListMultiplexes,

    -- ** Request lenses
    lmMaxResults,
    lmNextToken,

    -- * Destructuring the response
    ListMultiplexesResponse (..),
    mkListMultiplexesResponse,

    -- ** Response lenses
    lmrrsMultiplexes,
    lmrrsNextToken,
    lmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListMultiplexesRequest
--
-- /See:/ 'mkListMultiplexes' smart constructor.
data ListMultiplexes = ListMultiplexes'
  { -- | The maximum number of items to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to retrieve the next page of results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMultiplexes' value with any optional fields omitted.
mkListMultiplexes ::
  ListMultiplexes
mkListMultiplexes =
  ListMultiplexes'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmMaxResults :: Lens.Lens' ListMultiplexes (Core.Maybe Core.Natural)
lmMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmNextToken :: Lens.Lens' ListMultiplexes (Core.Maybe Core.Text)
lmNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListMultiplexes where
  type Rs ListMultiplexes = ListMultiplexesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/prod/multiplexes",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMultiplexesResponse'
            Core.<$> (x Core..:? "multiplexes")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListMultiplexes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"multiplexes" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Placeholder documentation for ListMultiplexesResponse
--
-- /See:/ 'mkListMultiplexesResponse' smart constructor.
data ListMultiplexesResponse = ListMultiplexesResponse'
  { -- | List of multiplexes.
    multiplexes :: Core.Maybe [Types.MultiplexSummary],
    -- | Token for the next ListMultiplexes request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMultiplexesResponse' value with any optional fields omitted.
mkListMultiplexesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListMultiplexesResponse
mkListMultiplexesResponse responseStatus =
  ListMultiplexesResponse'
    { multiplexes = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | List of multiplexes.
--
-- /Note:/ Consider using 'multiplexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrrsMultiplexes :: Lens.Lens' ListMultiplexesResponse (Core.Maybe [Types.MultiplexSummary])
lmrrsMultiplexes = Lens.field @"multiplexes"
{-# DEPRECATED lmrrsMultiplexes "Use generic-lens or generic-optics with 'multiplexes' instead." #-}

-- | Token for the next ListMultiplexes request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrrsNextToken :: Lens.Lens' ListMultiplexesResponse (Core.Maybe Core.Text)
lmrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrrsResponseStatus :: Lens.Lens' ListMultiplexesResponse Core.Int
lmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
