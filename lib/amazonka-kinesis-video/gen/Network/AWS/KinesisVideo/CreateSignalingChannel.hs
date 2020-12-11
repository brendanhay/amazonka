{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.CreateSignalingChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a signaling channel.
--
-- @CreateSignalingChannel@ is an asynchronous operation.
module Network.AWS.KinesisVideo.CreateSignalingChannel
  ( -- * Creating a request
    CreateSignalingChannel (..),
    mkCreateSignalingChannel,

    -- ** Request lenses
    cscSingleMasterConfiguration,
    cscChannelType,
    cscTags,
    cscChannelName,

    -- * Destructuring the response
    CreateSignalingChannelResponse (..),
    mkCreateSignalingChannelResponse,

    -- ** Response lenses
    cscrsChannelARN,
    cscrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSignalingChannel' smart constructor.
data CreateSignalingChannel = CreateSignalingChannel'
  { singleMasterConfiguration ::
      Lude.Maybe SingleMasterConfiguration,
    channelType :: Lude.Maybe ChannelType,
    tags :: Lude.Maybe [Tag],
    channelName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSignalingChannel' with the minimum fields required to make a request.
--
-- * 'channelName' - A name for the signaling channel that you are creating. It must be unique for each AWS account and AWS Region.
-- * 'channelType' - A type of the signaling channel that you are creating. Currently, @SINGLE_MASTER@ is the only supported channel type.
-- * 'singleMasterConfiguration' - A structure containing the configuration for the @SINGLE_MASTER@ channel type.
-- * 'tags' - A set of tags (key-value pairs) that you want to associate with this channel.
mkCreateSignalingChannel ::
  -- | 'channelName'
  Lude.Text ->
  CreateSignalingChannel
mkCreateSignalingChannel pChannelName_ =
  CreateSignalingChannel'
    { singleMasterConfiguration = Lude.Nothing,
      channelType = Lude.Nothing,
      tags = Lude.Nothing,
      channelName = pChannelName_
    }

-- | A structure containing the configuration for the @SINGLE_MASTER@ channel type.
--
-- /Note:/ Consider using 'singleMasterConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscSingleMasterConfiguration :: Lens.Lens' CreateSignalingChannel (Lude.Maybe SingleMasterConfiguration)
cscSingleMasterConfiguration = Lens.lens (singleMasterConfiguration :: CreateSignalingChannel -> Lude.Maybe SingleMasterConfiguration) (\s a -> s {singleMasterConfiguration = a} :: CreateSignalingChannel)
{-# DEPRECATED cscSingleMasterConfiguration "Use generic-lens or generic-optics with 'singleMasterConfiguration' instead." #-}

-- | A type of the signaling channel that you are creating. Currently, @SINGLE_MASTER@ is the only supported channel type.
--
-- /Note:/ Consider using 'channelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscChannelType :: Lens.Lens' CreateSignalingChannel (Lude.Maybe ChannelType)
cscChannelType = Lens.lens (channelType :: CreateSignalingChannel -> Lude.Maybe ChannelType) (\s a -> s {channelType = a} :: CreateSignalingChannel)
{-# DEPRECATED cscChannelType "Use generic-lens or generic-optics with 'channelType' instead." #-}

-- | A set of tags (key-value pairs) that you want to associate with this channel.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscTags :: Lens.Lens' CreateSignalingChannel (Lude.Maybe [Tag])
cscTags = Lens.lens (tags :: CreateSignalingChannel -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateSignalingChannel)
{-# DEPRECATED cscTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A name for the signaling channel that you are creating. It must be unique for each AWS account and AWS Region.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscChannelName :: Lens.Lens' CreateSignalingChannel Lude.Text
cscChannelName = Lens.lens (channelName :: CreateSignalingChannel -> Lude.Text) (\s a -> s {channelName = a} :: CreateSignalingChannel)
{-# DEPRECATED cscChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

instance Lude.AWSRequest CreateSignalingChannel where
  type Rs CreateSignalingChannel = CreateSignalingChannelResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSignalingChannelResponse'
            Lude.<$> (x Lude..?> "ChannelARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSignalingChannel where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateSignalingChannel where
  toJSON CreateSignalingChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SingleMasterConfiguration" Lude..=)
              Lude.<$> singleMasterConfiguration,
            ("ChannelType" Lude..=) Lude.<$> channelType,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("ChannelName" Lude..= channelName)
          ]
      )

instance Lude.ToPath CreateSignalingChannel where
  toPath = Lude.const "/createSignalingChannel"

instance Lude.ToQuery CreateSignalingChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSignalingChannelResponse' smart constructor.
data CreateSignalingChannelResponse = CreateSignalingChannelResponse'
  { channelARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateSignalingChannelResponse' with the minimum fields required to make a request.
--
-- * 'channelARN' - The Amazon Resource Name (ARN) of the created channel.
-- * 'responseStatus' - The response status code.
mkCreateSignalingChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSignalingChannelResponse
mkCreateSignalingChannelResponse pResponseStatus_ =
  CreateSignalingChannelResponse'
    { channelARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the created channel.
--
-- /Note:/ Consider using 'channelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrsChannelARN :: Lens.Lens' CreateSignalingChannelResponse (Lude.Maybe Lude.Text)
cscrsChannelARN = Lens.lens (channelARN :: CreateSignalingChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {channelARN = a} :: CreateSignalingChannelResponse)
{-# DEPRECATED cscrsChannelARN "Use generic-lens or generic-optics with 'channelARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrsResponseStatus :: Lens.Lens' CreateSignalingChannelResponse Lude.Int
cscrsResponseStatus = Lens.lens (responseStatus :: CreateSignalingChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSignalingChannelResponse)
{-# DEPRECATED cscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
