{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.ListSignalingChannels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ChannelInfo@ objects. Each object describes a signaling channel. To retrieve only those channels that satisfy a specific condition, you can specify a @ChannelNameCondition@ .
--
-- This operation returns paginated results.
module Network.AWS.KinesisVideo.ListSignalingChannels
  ( -- * Creating a request
    ListSignalingChannels (..),
    mkListSignalingChannels,

    -- ** Request lenses
    lscChannelNameCondition,
    lscMaxResults,
    lscNextToken,

    -- * Destructuring the response
    ListSignalingChannelsResponse (..),
    mkListSignalingChannelsResponse,

    -- ** Response lenses
    lscrrsChannelInfoList,
    lscrrsNextToken,
    lscrrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSignalingChannels' smart constructor.
data ListSignalingChannels = ListSignalingChannels'
  { -- | Optional: Returns only the channels that satisfy a specific condition.
    channelNameCondition :: Core.Maybe Types.ChannelNameCondition,
    -- | The maximum number of channels to return in the response. The default is 500.
    maxResults :: Core.Maybe Core.Natural,
    -- | If you specify this parameter, when the result of a @ListSignalingChannels@ operation is truncated, the call returns the @NextToken@ in the response. To get another batch of channels, provide this token in your next request.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSignalingChannels' value with any optional fields omitted.
mkListSignalingChannels ::
  ListSignalingChannels
mkListSignalingChannels =
  ListSignalingChannels'
    { channelNameCondition = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Optional: Returns only the channels that satisfy a specific condition.
--
-- /Note:/ Consider using 'channelNameCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscChannelNameCondition :: Lens.Lens' ListSignalingChannels (Core.Maybe Types.ChannelNameCondition)
lscChannelNameCondition = Lens.field @"channelNameCondition"
{-# DEPRECATED lscChannelNameCondition "Use generic-lens or generic-optics with 'channelNameCondition' instead." #-}

-- | The maximum number of channels to return in the response. The default is 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscMaxResults :: Lens.Lens' ListSignalingChannels (Core.Maybe Core.Natural)
lscMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lscMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If you specify this parameter, when the result of a @ListSignalingChannels@ operation is truncated, the call returns the @NextToken@ in the response. To get another batch of channels, provide this token in your next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscNextToken :: Lens.Lens' ListSignalingChannels (Core.Maybe Types.NextToken)
lscNextToken = Lens.field @"nextToken"
{-# DEPRECATED lscNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListSignalingChannels where
  toJSON ListSignalingChannels {..} =
    Core.object
      ( Core.catMaybes
          [ ("ChannelNameCondition" Core..=) Core.<$> channelNameCondition,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListSignalingChannels where
  type Rs ListSignalingChannels = ListSignalingChannelsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/listSignalingChannels",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSignalingChannelsResponse'
            Core.<$> (x Core..:? "ChannelInfoList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListSignalingChannels where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"channelInfoList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListSignalingChannelsResponse' smart constructor.
data ListSignalingChannelsResponse = ListSignalingChannelsResponse'
  { -- | An array of @ChannelInfo@ objects.
    channelInfoList :: Core.Maybe [Types.ChannelInfo],
    -- | If the response is truncated, the call returns this element with a token. To get the next batch of streams, use this token in your next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListSignalingChannelsResponse' value with any optional fields omitted.
mkListSignalingChannelsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListSignalingChannelsResponse
mkListSignalingChannelsResponse responseStatus =
  ListSignalingChannelsResponse'
    { channelInfoList = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of @ChannelInfo@ objects.
--
-- /Note:/ Consider using 'channelInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrrsChannelInfoList :: Lens.Lens' ListSignalingChannelsResponse (Core.Maybe [Types.ChannelInfo])
lscrrsChannelInfoList = Lens.field @"channelInfoList"
{-# DEPRECATED lscrrsChannelInfoList "Use generic-lens or generic-optics with 'channelInfoList' instead." #-}

-- | If the response is truncated, the call returns this element with a token. To get the next batch of streams, use this token in your next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrrsNextToken :: Lens.Lens' ListSignalingChannelsResponse (Core.Maybe Types.NextToken)
lscrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lscrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrrsResponseStatus :: Lens.Lens' ListSignalingChannelsResponse Core.Int
lscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
