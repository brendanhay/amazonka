{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.ListChannels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of channels.
--
-- This operation returns paginated results.
module Network.AWS.IoTAnalytics.ListChannels
  ( -- * Creating a request
    ListChannels (..),
    mkListChannels,

    -- ** Request lenses
    lcMaxResults,
    lcNextToken,

    -- * Destructuring the response
    ListChannelsResponse (..),
    mkListChannelsResponse,

    -- ** Response lenses
    lcrrsChannelSummaries,
    lcrrsNextToken,
    lcrrsResponseStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListChannels' smart constructor.
data ListChannels = ListChannels'
  { -- | The maximum number of results to return in this request.
    --
    -- The default value is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListChannels' value with any optional fields omitted.
mkListChannels ::
  ListChannels
mkListChannels =
  ListChannels'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to return in this request.
--
-- The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListChannels (Core.Maybe Core.Natural)
lcMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListChannels (Core.Maybe Types.NextToken)
lcNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListChannels where
  type Rs ListChannels = ListChannelsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/channels",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChannelsResponse'
            Core.<$> (x Core..:? "channelSummaries")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListChannels where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"channelSummaries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListChannelsResponse' smart constructor.
data ListChannelsResponse = ListChannelsResponse'
  { -- | A list of @ChannelSummary@ objects.
    channelSummaries :: Core.Maybe [Types.ChannelSummary],
    -- | The token to retrieve the next set of results, or @null@ if there are no more results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListChannelsResponse' value with any optional fields omitted.
mkListChannelsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListChannelsResponse
mkListChannelsResponse responseStatus =
  ListChannelsResponse'
    { channelSummaries = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of @ChannelSummary@ objects.
--
-- /Note:/ Consider using 'channelSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsChannelSummaries :: Lens.Lens' ListChannelsResponse (Core.Maybe [Types.ChannelSummary])
lcrrsChannelSummaries = Lens.field @"channelSummaries"
{-# DEPRECATED lcrrsChannelSummaries "Use generic-lens or generic-optics with 'channelSummaries' instead." #-}

-- | The token to retrieve the next set of results, or @null@ if there are no more results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsNextToken :: Lens.Lens' ListChannelsResponse (Core.Maybe Types.NextToken)
lcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsResponseStatus :: Lens.Lens' ListChannelsResponse Core.Int
lcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
