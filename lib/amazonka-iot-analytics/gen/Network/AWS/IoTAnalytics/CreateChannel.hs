{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.CreateChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a channel. A channel collects data from an MQTT topic and archives the raw, unprocessed messages before publishing the data to a pipeline.
module Network.AWS.IoTAnalytics.CreateChannel
  ( -- * Creating a request
    CreateChannel (..),
    mkCreateChannel,

    -- ** Request lenses
    ccRetentionPeriod,
    ccChannelStorage,
    ccTags,
    ccChannelName,

    -- * Destructuring the response
    CreateChannelResponse (..),
    mkCreateChannelResponse,

    -- ** Response lenses
    ccrsChannelARN,
    ccrsRetentionPeriod,
    ccrsChannelName,
    ccrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { retentionPeriod ::
      Lude.Maybe RetentionPeriod,
    channelStorage :: Lude.Maybe ChannelStorage,
    tags :: Lude.Maybe (Lude.NonEmpty Tag),
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

-- | Creates a value of 'CreateChannel' with the minimum fields required to make a request.
--
-- * 'channelName' - The name of the channel.
-- * 'channelStorage' - Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
-- * 'retentionPeriod' - How long, in days, message data is kept for the channel. When @customerManagedS3@ storage is selected, this parameter is ignored.
-- * 'tags' - Metadata which can be used to manage the channel.
mkCreateChannel ::
  -- | 'channelName'
  Lude.Text ->
  CreateChannel
mkCreateChannel pChannelName_ =
  CreateChannel'
    { retentionPeriod = Lude.Nothing,
      channelStorage = Lude.Nothing,
      tags = Lude.Nothing,
      channelName = pChannelName_
    }

-- | How long, in days, message data is kept for the channel. When @customerManagedS3@ storage is selected, this parameter is ignored.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccRetentionPeriod :: Lens.Lens' CreateChannel (Lude.Maybe RetentionPeriod)
ccRetentionPeriod = Lens.lens (retentionPeriod :: CreateChannel -> Lude.Maybe RetentionPeriod) (\s a -> s {retentionPeriod = a} :: CreateChannel)
{-# DEPRECATED ccRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
--
-- /Note:/ Consider using 'channelStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccChannelStorage :: Lens.Lens' CreateChannel (Lude.Maybe ChannelStorage)
ccChannelStorage = Lens.lens (channelStorage :: CreateChannel -> Lude.Maybe ChannelStorage) (\s a -> s {channelStorage = a} :: CreateChannel)
{-# DEPRECATED ccChannelStorage "Use generic-lens or generic-optics with 'channelStorage' instead." #-}

-- | Metadata which can be used to manage the channel.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateChannel (Lude.Maybe (Lude.NonEmpty Tag))
ccTags = Lens.lens (tags :: CreateChannel -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateChannel)
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the channel.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccChannelName :: Lens.Lens' CreateChannel Lude.Text
ccChannelName = Lens.lens (channelName :: CreateChannel -> Lude.Text) (\s a -> s {channelName = a} :: CreateChannel)
{-# DEPRECATED ccChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

instance Lude.AWSRequest CreateChannel where
  type Rs CreateChannel = CreateChannelResponse
  request = Req.postJSON ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            Lude.<$> (x Lude..?> "channelArn")
            Lude.<*> (x Lude..?> "retentionPeriod")
            Lude.<*> (x Lude..?> "channelName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateChannel where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateChannel where
  toJSON CreateChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("retentionPeriod" Lude..=) Lude.<$> retentionPeriod,
            ("channelStorage" Lude..=) Lude.<$> channelStorage,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("channelName" Lude..= channelName)
          ]
      )

instance Lude.ToPath CreateChannel where
  toPath = Lude.const "/channels"

instance Lude.ToQuery CreateChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { channelARN ::
      Lude.Maybe Lude.Text,
    retentionPeriod :: Lude.Maybe RetentionPeriod,
    channelName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateChannelResponse' with the minimum fields required to make a request.
--
-- * 'channelARN' - The ARN of the channel.
-- * 'channelName' - The name of the channel.
-- * 'responseStatus' - The response status code.
-- * 'retentionPeriod' - How long, in days, message data is kept for the channel.
mkCreateChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateChannelResponse
mkCreateChannelResponse pResponseStatus_ =
  CreateChannelResponse'
    { channelARN = Lude.Nothing,
      retentionPeriod = Lude.Nothing,
      channelName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the channel.
--
-- /Note:/ Consider using 'channelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsChannelARN :: Lens.Lens' CreateChannelResponse (Lude.Maybe Lude.Text)
ccrsChannelARN = Lens.lens (channelARN :: CreateChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {channelARN = a} :: CreateChannelResponse)
{-# DEPRECATED ccrsChannelARN "Use generic-lens or generic-optics with 'channelARN' instead." #-}

-- | How long, in days, message data is kept for the channel.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsRetentionPeriod :: Lens.Lens' CreateChannelResponse (Lude.Maybe RetentionPeriod)
ccrsRetentionPeriod = Lens.lens (retentionPeriod :: CreateChannelResponse -> Lude.Maybe RetentionPeriod) (\s a -> s {retentionPeriod = a} :: CreateChannelResponse)
{-# DEPRECATED ccrsRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | The name of the channel.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsChannelName :: Lens.Lens' CreateChannelResponse (Lude.Maybe Lude.Text)
ccrsChannelName = Lens.lens (channelName :: CreateChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {channelName = a} :: CreateChannelResponse)
{-# DEPRECATED ccrsChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateChannelResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateChannelResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
