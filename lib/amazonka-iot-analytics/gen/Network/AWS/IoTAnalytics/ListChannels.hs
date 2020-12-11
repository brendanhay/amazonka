{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    lcNextToken,
    lcMaxResults,

    -- * Destructuring the response
    ListChannelsResponse (..),
    mkListChannelsResponse,

    -- ** Response lenses
    lcrsChannelSummaries,
    lcrsNextToken,
    lcrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListChannels' smart constructor.
data ListChannels = ListChannels'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListChannels' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return in this request.
--
-- The default value is 100.
-- * 'nextToken' - The token for the next set of results.
mkListChannels ::
  ListChannels
mkListChannels =
  ListChannels'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListChannels (Lude.Maybe Lude.Text)
lcNextToken = Lens.lens (nextToken :: ListChannels -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListChannels)
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in this request.
--
-- The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListChannels (Lude.Maybe Lude.Natural)
lcMaxResults = Lens.lens (maxResults :: ListChannels -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListChannels)
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListChannels where
  page rq rs
    | Page.stop (rs Lens.^. lcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcrsChannelSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcNextToken Lens..~ rs Lens.^. lcrsNextToken

instance Lude.AWSRequest ListChannels where
  type Rs ListChannels = ListChannelsResponse
  request = Req.get ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListChannelsResponse'
            Lude.<$> (x Lude..?> "channelSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListChannels where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListChannels where
  toPath = Lude.const "/channels"

instance Lude.ToQuery ListChannels where
  toQuery ListChannels' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListChannelsResponse' smart constructor.
data ListChannelsResponse = ListChannelsResponse'
  { channelSummaries ::
      Lude.Maybe [ChannelSummary],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListChannelsResponse' with the minimum fields required to make a request.
--
-- * 'channelSummaries' - A list of @ChannelSummary@ objects.
-- * 'nextToken' - The token to retrieve the next set of results, or @null@ if there are no more results.
-- * 'responseStatus' - The response status code.
mkListChannelsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListChannelsResponse
mkListChannelsResponse pResponseStatus_ =
  ListChannelsResponse'
    { channelSummaries = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @ChannelSummary@ objects.
--
-- /Note:/ Consider using 'channelSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsChannelSummaries :: Lens.Lens' ListChannelsResponse (Lude.Maybe [ChannelSummary])
lcrsChannelSummaries = Lens.lens (channelSummaries :: ListChannelsResponse -> Lude.Maybe [ChannelSummary]) (\s a -> s {channelSummaries = a} :: ListChannelsResponse)
{-# DEPRECATED lcrsChannelSummaries "Use generic-lens or generic-optics with 'channelSummaries' instead." #-}

-- | The token to retrieve the next set of results, or @null@ if there are no more results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsNextToken :: Lens.Lens' ListChannelsResponse (Lude.Maybe Lude.Text)
lcrsNextToken = Lens.lens (nextToken :: ListChannelsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListChannelsResponse)
{-# DEPRECATED lcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsResponseStatus :: Lens.Lens' ListChannelsResponse Lude.Int
lcrsResponseStatus = Lens.lens (responseStatus :: ListChannelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListChannelsResponse)
{-# DEPRECATED lcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
