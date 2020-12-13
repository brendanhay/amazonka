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
    lscNextToken,
    lscMaxResults,

    -- * Destructuring the response
    ListSignalingChannelsResponse (..),
    mkListSignalingChannelsResponse,

    -- ** Response lenses
    lscrsChannelInfoList,
    lscrsNextToken,
    lscrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSignalingChannels' smart constructor.
data ListSignalingChannels = ListSignalingChannels'
  { -- | Optional: Returns only the channels that satisfy a specific condition.
    channelNameCondition :: Lude.Maybe ChannelNameCondition,
    -- | If you specify this parameter, when the result of a @ListSignalingChannels@ operation is truncated, the call returns the @NextToken@ in the response. To get another batch of channels, provide this token in your next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of channels to return in the response. The default is 500.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSignalingChannels' with the minimum fields required to make a request.
--
-- * 'channelNameCondition' - Optional: Returns only the channels that satisfy a specific condition.
-- * 'nextToken' - If you specify this parameter, when the result of a @ListSignalingChannels@ operation is truncated, the call returns the @NextToken@ in the response. To get another batch of channels, provide this token in your next request.
-- * 'maxResults' - The maximum number of channels to return in the response. The default is 500.
mkListSignalingChannels ::
  ListSignalingChannels
mkListSignalingChannels =
  ListSignalingChannels'
    { channelNameCondition = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Optional: Returns only the channels that satisfy a specific condition.
--
-- /Note:/ Consider using 'channelNameCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscChannelNameCondition :: Lens.Lens' ListSignalingChannels (Lude.Maybe ChannelNameCondition)
lscChannelNameCondition = Lens.lens (channelNameCondition :: ListSignalingChannels -> Lude.Maybe ChannelNameCondition) (\s a -> s {channelNameCondition = a} :: ListSignalingChannels)
{-# DEPRECATED lscChannelNameCondition "Use generic-lens or generic-optics with 'channelNameCondition' instead." #-}

-- | If you specify this parameter, when the result of a @ListSignalingChannels@ operation is truncated, the call returns the @NextToken@ in the response. To get another batch of channels, provide this token in your next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscNextToken :: Lens.Lens' ListSignalingChannels (Lude.Maybe Lude.Text)
lscNextToken = Lens.lens (nextToken :: ListSignalingChannels -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSignalingChannels)
{-# DEPRECATED lscNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of channels to return in the response. The default is 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscMaxResults :: Lens.Lens' ListSignalingChannels (Lude.Maybe Lude.Natural)
lscMaxResults = Lens.lens (maxResults :: ListSignalingChannels -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSignalingChannels)
{-# DEPRECATED lscMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListSignalingChannels where
  page rq rs
    | Page.stop (rs Lens.^. lscrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lscrsChannelInfoList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lscNextToken Lens..~ rs Lens.^. lscrsNextToken

instance Lude.AWSRequest ListSignalingChannels where
  type Rs ListSignalingChannels = ListSignalingChannelsResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSignalingChannelsResponse'
            Lude.<$> (x Lude..?> "ChannelInfoList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSignalingChannels where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListSignalingChannels where
  toJSON ListSignalingChannels' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ChannelNameCondition" Lude..=) Lude.<$> channelNameCondition,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListSignalingChannels where
  toPath = Lude.const "/listSignalingChannels"

instance Lude.ToQuery ListSignalingChannels where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSignalingChannelsResponse' smart constructor.
data ListSignalingChannelsResponse = ListSignalingChannelsResponse'
  { -- | An array of @ChannelInfo@ objects.
    channelInfoList :: Lude.Maybe [ChannelInfo],
    -- | If the response is truncated, the call returns this element with a token. To get the next batch of streams, use this token in your next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSignalingChannelsResponse' with the minimum fields required to make a request.
--
-- * 'channelInfoList' - An array of @ChannelInfo@ objects.
-- * 'nextToken' - If the response is truncated, the call returns this element with a token. To get the next batch of streams, use this token in your next request.
-- * 'responseStatus' - The response status code.
mkListSignalingChannelsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSignalingChannelsResponse
mkListSignalingChannelsResponse pResponseStatus_ =
  ListSignalingChannelsResponse'
    { channelInfoList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @ChannelInfo@ objects.
--
-- /Note:/ Consider using 'channelInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrsChannelInfoList :: Lens.Lens' ListSignalingChannelsResponse (Lude.Maybe [ChannelInfo])
lscrsChannelInfoList = Lens.lens (channelInfoList :: ListSignalingChannelsResponse -> Lude.Maybe [ChannelInfo]) (\s a -> s {channelInfoList = a} :: ListSignalingChannelsResponse)
{-# DEPRECATED lscrsChannelInfoList "Use generic-lens or generic-optics with 'channelInfoList' instead." #-}

-- | If the response is truncated, the call returns this element with a token. To get the next batch of streams, use this token in your next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrsNextToken :: Lens.Lens' ListSignalingChannelsResponse (Lude.Maybe Lude.Text)
lscrsNextToken = Lens.lens (nextToken :: ListSignalingChannelsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSignalingChannelsResponse)
{-# DEPRECATED lscrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrsResponseStatus :: Lens.Lens' ListSignalingChannelsResponse Lude.Int
lscrsResponseStatus = Lens.lens (responseStatus :: ListSignalingChannelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSignalingChannelsResponse)
{-# DEPRECATED lscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
