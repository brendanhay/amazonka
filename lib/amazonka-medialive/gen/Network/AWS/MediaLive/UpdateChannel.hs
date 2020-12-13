{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.UpdateChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a channel.
module Network.AWS.MediaLive.UpdateChannel
  ( -- * Creating a request
    UpdateChannel (..),
    mkUpdateChannel,

    -- ** Request lenses
    ucLogLevel,
    ucInputSpecification,
    ucChannelId,
    ucInputAttachments,
    ucDestinations,
    ucName,
    ucCdiInputSpecification,
    ucEncoderSettings,
    ucRoleARN,

    -- * Destructuring the response
    UpdateChannelResponse (..),
    mkUpdateChannelResponse,

    -- ** Response lenses
    ucrsChannel,
    ucrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to update a channel.
--
-- /See:/ 'mkUpdateChannel' smart constructor.
data UpdateChannel = UpdateChannel'
  { -- | The log level to write to CloudWatch Logs.
    logLevel :: Lude.Maybe LogLevel,
    -- | Specification of network and file inputs for this channel
    inputSpecification :: Lude.Maybe InputSpecification,
    -- | channel ID
    channelId :: Lude.Text,
    inputAttachments :: Lude.Maybe [InputAttachment],
    -- | A list of output destinations for this channel.
    destinations :: Lude.Maybe [OutputDestination],
    -- | The name of the channel.
    name :: Lude.Maybe Lude.Text,
    -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Lude.Maybe CdiInputSpecification,
    -- | The encoder settings for this channel.
    encoderSettings :: Lude.Maybe EncoderSettings,
    -- | An optional Amazon Resource Name (ARN) of the role to assume when running the Channel. If you do not specify this on an update call but the role was previously set that role will be removed.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateChannel' with the minimum fields required to make a request.
--
-- * 'logLevel' - The log level to write to CloudWatch Logs.
-- * 'inputSpecification' - Specification of network and file inputs for this channel
-- * 'channelId' - channel ID
-- * 'inputAttachments' -
-- * 'destinations' - A list of output destinations for this channel.
-- * 'name' - The name of the channel.
-- * 'cdiInputSpecification' - Specification of CDI inputs for this channel
-- * 'encoderSettings' - The encoder settings for this channel.
-- * 'roleARN' - An optional Amazon Resource Name (ARN) of the role to assume when running the Channel. If you do not specify this on an update call but the role was previously set that role will be removed.
mkUpdateChannel ::
  -- | 'channelId'
  Lude.Text ->
  UpdateChannel
mkUpdateChannel pChannelId_ =
  UpdateChannel'
    { logLevel = Lude.Nothing,
      inputSpecification = Lude.Nothing,
      channelId = pChannelId_,
      inputAttachments = Lude.Nothing,
      destinations = Lude.Nothing,
      name = Lude.Nothing,
      cdiInputSpecification = Lude.Nothing,
      encoderSettings = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The log level to write to CloudWatch Logs.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucLogLevel :: Lens.Lens' UpdateChannel (Lude.Maybe LogLevel)
ucLogLevel = Lens.lens (logLevel :: UpdateChannel -> Lude.Maybe LogLevel) (\s a -> s {logLevel = a} :: UpdateChannel)
{-# DEPRECATED ucLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | Specification of network and file inputs for this channel
--
-- /Note:/ Consider using 'inputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucInputSpecification :: Lens.Lens' UpdateChannel (Lude.Maybe InputSpecification)
ucInputSpecification = Lens.lens (inputSpecification :: UpdateChannel -> Lude.Maybe InputSpecification) (\s a -> s {inputSpecification = a} :: UpdateChannel)
{-# DEPRECATED ucInputSpecification "Use generic-lens or generic-optics with 'inputSpecification' instead." #-}

-- | channel ID
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucChannelId :: Lens.Lens' UpdateChannel Lude.Text
ucChannelId = Lens.lens (channelId :: UpdateChannel -> Lude.Text) (\s a -> s {channelId = a} :: UpdateChannel)
{-# DEPRECATED ucChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'inputAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucInputAttachments :: Lens.Lens' UpdateChannel (Lude.Maybe [InputAttachment])
ucInputAttachments = Lens.lens (inputAttachments :: UpdateChannel -> Lude.Maybe [InputAttachment]) (\s a -> s {inputAttachments = a} :: UpdateChannel)
{-# DEPRECATED ucInputAttachments "Use generic-lens or generic-optics with 'inputAttachments' instead." #-}

-- | A list of output destinations for this channel.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDestinations :: Lens.Lens' UpdateChannel (Lude.Maybe [OutputDestination])
ucDestinations = Lens.lens (destinations :: UpdateChannel -> Lude.Maybe [OutputDestination]) (\s a -> s {destinations = a} :: UpdateChannel)
{-# DEPRECATED ucDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the channel.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucName :: Lens.Lens' UpdateChannel (Lude.Maybe Lude.Text)
ucName = Lens.lens (name :: UpdateChannel -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateChannel)
{-# DEPRECATED ucName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specification of CDI inputs for this channel
--
-- /Note:/ Consider using 'cdiInputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucCdiInputSpecification :: Lens.Lens' UpdateChannel (Lude.Maybe CdiInputSpecification)
ucCdiInputSpecification = Lens.lens (cdiInputSpecification :: UpdateChannel -> Lude.Maybe CdiInputSpecification) (\s a -> s {cdiInputSpecification = a} :: UpdateChannel)
{-# DEPRECATED ucCdiInputSpecification "Use generic-lens or generic-optics with 'cdiInputSpecification' instead." #-}

-- | The encoder settings for this channel.
--
-- /Note:/ Consider using 'encoderSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucEncoderSettings :: Lens.Lens' UpdateChannel (Lude.Maybe EncoderSettings)
ucEncoderSettings = Lens.lens (encoderSettings :: UpdateChannel -> Lude.Maybe EncoderSettings) (\s a -> s {encoderSettings = a} :: UpdateChannel)
{-# DEPRECATED ucEncoderSettings "Use generic-lens or generic-optics with 'encoderSettings' instead." #-}

-- | An optional Amazon Resource Name (ARN) of the role to assume when running the Channel. If you do not specify this on an update call but the role was previously set that role will be removed.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucRoleARN :: Lens.Lens' UpdateChannel (Lude.Maybe Lude.Text)
ucRoleARN = Lens.lens (roleARN :: UpdateChannel -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: UpdateChannel)
{-# DEPRECATED ucRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest UpdateChannel where
  type Rs UpdateChannel = UpdateChannelResponse
  request = Req.putJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateChannelResponse'
            Lude.<$> (x Lude..?> "channel") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateChannel where
  toJSON UpdateChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("logLevel" Lude..=) Lude.<$> logLevel,
            ("inputSpecification" Lude..=) Lude.<$> inputSpecification,
            ("inputAttachments" Lude..=) Lude.<$> inputAttachments,
            ("destinations" Lude..=) Lude.<$> destinations,
            ("name" Lude..=) Lude.<$> name,
            ("cdiInputSpecification" Lude..=) Lude.<$> cdiInputSpecification,
            ("encoderSettings" Lude..=) Lude.<$> encoderSettings,
            ("roleArn" Lude..=) Lude.<$> roleARN
          ]
      )

instance Lude.ToPath UpdateChannel where
  toPath UpdateChannel' {..} =
    Lude.mconcat ["/prod/channels/", Lude.toBS channelId]

instance Lude.ToQuery UpdateChannel where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for UpdateChannelResponse
--
-- /See:/ 'mkUpdateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
  { channel :: Lude.Maybe Channel,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateChannelResponse' with the minimum fields required to make a request.
--
-- * 'channel' -
-- * 'responseStatus' - The response status code.
mkUpdateChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateChannelResponse
mkUpdateChannelResponse pResponseStatus_ =
  UpdateChannelResponse'
    { channel = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsChannel :: Lens.Lens' UpdateChannelResponse (Lude.Maybe Channel)
ucrsChannel = Lens.lens (channel :: UpdateChannelResponse -> Lude.Maybe Channel) (\s a -> s {channel = a} :: UpdateChannelResponse)
{-# DEPRECATED ucrsChannel "Use generic-lens or generic-optics with 'channel' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsResponseStatus :: Lens.Lens' UpdateChannelResponse Lude.Int
ucrsResponseStatus = Lens.lens (responseStatus :: UpdateChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateChannelResponse)
{-# DEPRECATED ucrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
