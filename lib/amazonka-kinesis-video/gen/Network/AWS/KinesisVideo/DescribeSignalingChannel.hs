{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.DescribeSignalingChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the most current information about the signaling channel. You must specify either the name or the Amazon Resource Name (ARN) of the channel that you want to describe.
module Network.AWS.KinesisVideo.DescribeSignalingChannel
  ( -- * Creating a request
    DescribeSignalingChannel (..),
    mkDescribeSignalingChannel,

    -- ** Request lenses
    dChannelARN,
    dChannelName,

    -- * Destructuring the response
    DescribeSignalingChannelResponse (..),
    mkDescribeSignalingChannelResponse,

    -- ** Response lenses
    dscrsChannelInfo,
    dscrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSignalingChannel' smart constructor.
data DescribeSignalingChannel = DescribeSignalingChannel'
  { -- | The ARN of the signaling channel that you want to describe.
    channelARN :: Lude.Maybe Lude.Text,
    -- | The name of the signaling channel that you want to describe.
    channelName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSignalingChannel' with the minimum fields required to make a request.
--
-- * 'channelARN' - The ARN of the signaling channel that you want to describe.
-- * 'channelName' - The name of the signaling channel that you want to describe.
mkDescribeSignalingChannel ::
  DescribeSignalingChannel
mkDescribeSignalingChannel =
  DescribeSignalingChannel'
    { channelARN = Lude.Nothing,
      channelName = Lude.Nothing
    }

-- | The ARN of the signaling channel that you want to describe.
--
-- /Note:/ Consider using 'channelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dChannelARN :: Lens.Lens' DescribeSignalingChannel (Lude.Maybe Lude.Text)
dChannelARN = Lens.lens (channelARN :: DescribeSignalingChannel -> Lude.Maybe Lude.Text) (\s a -> s {channelARN = a} :: DescribeSignalingChannel)
{-# DEPRECATED dChannelARN "Use generic-lens or generic-optics with 'channelARN' instead." #-}

-- | The name of the signaling channel that you want to describe.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dChannelName :: Lens.Lens' DescribeSignalingChannel (Lude.Maybe Lude.Text)
dChannelName = Lens.lens (channelName :: DescribeSignalingChannel -> Lude.Maybe Lude.Text) (\s a -> s {channelName = a} :: DescribeSignalingChannel)
{-# DEPRECATED dChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

instance Lude.AWSRequest DescribeSignalingChannel where
  type Rs DescribeSignalingChannel = DescribeSignalingChannelResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSignalingChannelResponse'
            Lude.<$> (x Lude..?> "ChannelInfo") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSignalingChannel where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DescribeSignalingChannel where
  toJSON DescribeSignalingChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ChannelARN" Lude..=) Lude.<$> channelARN,
            ("ChannelName" Lude..=) Lude.<$> channelName
          ]
      )

instance Lude.ToPath DescribeSignalingChannel where
  toPath = Lude.const "/describeSignalingChannel"

instance Lude.ToQuery DescribeSignalingChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeSignalingChannelResponse' smart constructor.
data DescribeSignalingChannelResponse = DescribeSignalingChannelResponse'
  { -- | A structure that encapsulates the specified signaling channel's metadata and properties.
    channelInfo :: Lude.Maybe ChannelInfo,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSignalingChannelResponse' with the minimum fields required to make a request.
--
-- * 'channelInfo' - A structure that encapsulates the specified signaling channel's metadata and properties.
-- * 'responseStatus' - The response status code.
mkDescribeSignalingChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSignalingChannelResponse
mkDescribeSignalingChannelResponse pResponseStatus_ =
  DescribeSignalingChannelResponse'
    { channelInfo = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that encapsulates the specified signaling channel's metadata and properties.
--
-- /Note:/ Consider using 'channelInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrsChannelInfo :: Lens.Lens' DescribeSignalingChannelResponse (Lude.Maybe ChannelInfo)
dscrsChannelInfo = Lens.lens (channelInfo :: DescribeSignalingChannelResponse -> Lude.Maybe ChannelInfo) (\s a -> s {channelInfo = a} :: DescribeSignalingChannelResponse)
{-# DEPRECATED dscrsChannelInfo "Use generic-lens or generic-optics with 'channelInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrsResponseStatus :: Lens.Lens' DescribeSignalingChannelResponse Lude.Int
dscrsResponseStatus = Lens.lens (responseStatus :: DescribeSignalingChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSignalingChannelResponse)
{-# DEPRECATED dscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
