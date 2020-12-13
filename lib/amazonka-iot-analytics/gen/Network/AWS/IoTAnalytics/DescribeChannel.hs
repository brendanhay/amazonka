{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DescribeChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a channel.
module Network.AWS.IoTAnalytics.DescribeChannel
  ( -- * Creating a request
    DescribeChannel (..),
    mkDescribeChannel,

    -- ** Request lenses
    dcChannelName,
    dcIncludeStatistics,

    -- * Destructuring the response
    DescribeChannelResponse (..),
    mkDescribeChannelResponse,

    -- ** Response lenses
    dcrsChannel,
    dcrsStatistics,
    dcrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeChannel' smart constructor.
data DescribeChannel = DescribeChannel'
  { -- | The name of the channel whose information is retrieved.
    channelName :: Lude.Text,
    -- | If true, additional statistical information about the channel is included in the response. This feature cannot be used with a channel whose S3 storage is customer-managed.
    includeStatistics :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeChannel' with the minimum fields required to make a request.
--
-- * 'channelName' - The name of the channel whose information is retrieved.
-- * 'includeStatistics' - If true, additional statistical information about the channel is included in the response. This feature cannot be used with a channel whose S3 storage is customer-managed.
mkDescribeChannel ::
  -- | 'channelName'
  Lude.Text ->
  DescribeChannel
mkDescribeChannel pChannelName_ =
  DescribeChannel'
    { channelName = pChannelName_,
      includeStatistics = Lude.Nothing
    }

-- | The name of the channel whose information is retrieved.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcChannelName :: Lens.Lens' DescribeChannel Lude.Text
dcChannelName = Lens.lens (channelName :: DescribeChannel -> Lude.Text) (\s a -> s {channelName = a} :: DescribeChannel)
{-# DEPRECATED dcChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | If true, additional statistical information about the channel is included in the response. This feature cannot be used with a channel whose S3 storage is customer-managed.
--
-- /Note:/ Consider using 'includeStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcIncludeStatistics :: Lens.Lens' DescribeChannel (Lude.Maybe Lude.Bool)
dcIncludeStatistics = Lens.lens (includeStatistics :: DescribeChannel -> Lude.Maybe Lude.Bool) (\s a -> s {includeStatistics = a} :: DescribeChannel)
{-# DEPRECATED dcIncludeStatistics "Use generic-lens or generic-optics with 'includeStatistics' instead." #-}

instance Lude.AWSRequest DescribeChannel where
  type Rs DescribeChannel = DescribeChannelResponse
  request = Req.get ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeChannelResponse'
            Lude.<$> (x Lude..?> "channel")
            Lude.<*> (x Lude..?> "statistics")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeChannel where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeChannel where
  toPath DescribeChannel' {..} =
    Lude.mconcat ["/channels/", Lude.toBS channelName]

instance Lude.ToQuery DescribeChannel where
  toQuery DescribeChannel' {..} =
    Lude.mconcat ["includeStatistics" Lude.=: includeStatistics]

-- | /See:/ 'mkDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { -- | An object that contains information about the channel.
    channel :: Lude.Maybe Channel,
    -- | Statistics about the channel. Included if the @includeStatistics@ parameter is set to @true@ in the request.
    statistics :: Lude.Maybe ChannelStatistics,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeChannelResponse' with the minimum fields required to make a request.
--
-- * 'channel' - An object that contains information about the channel.
-- * 'statistics' - Statistics about the channel. Included if the @includeStatistics@ parameter is set to @true@ in the request.
-- * 'responseStatus' - The response status code.
mkDescribeChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeChannelResponse
mkDescribeChannelResponse pResponseStatus_ =
  DescribeChannelResponse'
    { channel = Lude.Nothing,
      statistics = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains information about the channel.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsChannel :: Lens.Lens' DescribeChannelResponse (Lude.Maybe Channel)
dcrsChannel = Lens.lens (channel :: DescribeChannelResponse -> Lude.Maybe Channel) (\s a -> s {channel = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsChannel "Use generic-lens or generic-optics with 'channel' instead." #-}

-- | Statistics about the channel. Included if the @includeStatistics@ parameter is set to @true@ in the request.
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsStatistics :: Lens.Lens' DescribeChannelResponse (Lude.Maybe ChannelStatistics)
dcrsStatistics = Lens.lens (statistics :: DescribeChannelResponse -> Lude.Maybe ChannelStatistics) (\s a -> s {statistics = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsStatistics "Use generic-lens or generic-optics with 'statistics' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeChannelResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
