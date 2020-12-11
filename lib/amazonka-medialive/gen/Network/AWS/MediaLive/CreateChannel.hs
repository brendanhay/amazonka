{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.CreateChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new channel
module Network.AWS.MediaLive.CreateChannel
  ( -- * Creating a request
    CreateChannel (..),
    mkCreateChannel,

    -- ** Request lenses
    ccRequestId,
    ccLogLevel,
    ccInputSpecification,
    ccInputAttachments,
    ccReserved,
    ccDestinations,
    ccName,
    ccCdiInputSpecification,
    ccChannelClass,
    ccTags,
    ccEncoderSettings,
    ccRoleARN,

    -- * Destructuring the response
    CreateChannelResponse (..),
    mkCreateChannelResponse,

    -- ** Response lenses
    ccrsChannel,
    ccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to create a channel
--
-- /See:/ 'mkCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { requestId ::
      Lude.Maybe Lude.Text,
    logLevel :: Lude.Maybe LogLevel,
    inputSpecification :: Lude.Maybe InputSpecification,
    inputAttachments :: Lude.Maybe [InputAttachment],
    reserved :: Lude.Maybe Lude.Text,
    destinations :: Lude.Maybe [OutputDestination],
    name :: Lude.Maybe Lude.Text,
    cdiInputSpecification :: Lude.Maybe CdiInputSpecification,
    channelClass :: Lude.Maybe ChannelClass,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    encoderSettings :: Lude.Maybe EncoderSettings,
    roleARN :: Lude.Maybe Lude.Text
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
-- * 'cdiInputSpecification' - Specification of CDI inputs for this channel
-- * 'channelClass' - The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
-- * 'destinations' - Undocumented field.
-- * 'encoderSettings' - Undocumented field.
-- * 'inputAttachments' - List of input attachments for channel.
-- * 'inputSpecification' - Specification of network and file inputs for this channel
-- * 'logLevel' - The log level to write to CloudWatch Logs.
-- * 'name' - Name of channel.
-- * 'requestId' - Unique request ID to be specified. This is needed to prevent retries from
--
-- creating multiple resources.
-- * 'reserved' - Deprecated field that's only usable by whitelisted customers.
-- * 'roleARN' - An optional Amazon Resource Name (ARN) of the role to assume when running the Channel.
-- * 'tags' - A collection of key-value pairs.
mkCreateChannel ::
  CreateChannel
mkCreateChannel =
  CreateChannel'
    { requestId = Lude.Nothing,
      logLevel = Lude.Nothing,
      inputSpecification = Lude.Nothing,
      inputAttachments = Lude.Nothing,
      reserved = Lude.Nothing,
      destinations = Lude.Nothing,
      name = Lude.Nothing,
      cdiInputSpecification = Lude.Nothing,
      channelClass = Lude.Nothing,
      tags = Lude.Nothing,
      encoderSettings = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Unique request ID to be specified. This is needed to prevent retries from
--
-- creating multiple resources.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccRequestId :: Lens.Lens' CreateChannel (Lude.Maybe Lude.Text)
ccRequestId = Lens.lens (requestId :: CreateChannel -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: CreateChannel)
{-# DEPRECATED ccRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | The log level to write to CloudWatch Logs.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLogLevel :: Lens.Lens' CreateChannel (Lude.Maybe LogLevel)
ccLogLevel = Lens.lens (logLevel :: CreateChannel -> Lude.Maybe LogLevel) (\s a -> s {logLevel = a} :: CreateChannel)
{-# DEPRECATED ccLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | Specification of network and file inputs for this channel
--
-- /Note:/ Consider using 'inputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccInputSpecification :: Lens.Lens' CreateChannel (Lude.Maybe InputSpecification)
ccInputSpecification = Lens.lens (inputSpecification :: CreateChannel -> Lude.Maybe InputSpecification) (\s a -> s {inputSpecification = a} :: CreateChannel)
{-# DEPRECATED ccInputSpecification "Use generic-lens or generic-optics with 'inputSpecification' instead." #-}

-- | List of input attachments for channel.
--
-- /Note:/ Consider using 'inputAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccInputAttachments :: Lens.Lens' CreateChannel (Lude.Maybe [InputAttachment])
ccInputAttachments = Lens.lens (inputAttachments :: CreateChannel -> Lude.Maybe [InputAttachment]) (\s a -> s {inputAttachments = a} :: CreateChannel)
{-# DEPRECATED ccInputAttachments "Use generic-lens or generic-optics with 'inputAttachments' instead." #-}

-- | Deprecated field that's only usable by whitelisted customers.
--
-- /Note:/ Consider using 'reserved' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccReserved :: Lens.Lens' CreateChannel (Lude.Maybe Lude.Text)
ccReserved = Lens.lens (reserved :: CreateChannel -> Lude.Maybe Lude.Text) (\s a -> s {reserved = a} :: CreateChannel)
{-# DEPRECATED ccReserved "Use generic-lens or generic-optics with 'reserved' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDestinations :: Lens.Lens' CreateChannel (Lude.Maybe [OutputDestination])
ccDestinations = Lens.lens (destinations :: CreateChannel -> Lude.Maybe [OutputDestination]) (\s a -> s {destinations = a} :: CreateChannel)
{-# DEPRECATED ccDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | Name of channel.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccName :: Lens.Lens' CreateChannel (Lude.Maybe Lude.Text)
ccName = Lens.lens (name :: CreateChannel -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateChannel)
{-# DEPRECATED ccName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specification of CDI inputs for this channel
--
-- /Note:/ Consider using 'cdiInputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCdiInputSpecification :: Lens.Lens' CreateChannel (Lude.Maybe CdiInputSpecification)
ccCdiInputSpecification = Lens.lens (cdiInputSpecification :: CreateChannel -> Lude.Maybe CdiInputSpecification) (\s a -> s {cdiInputSpecification = a} :: CreateChannel)
{-# DEPRECATED ccCdiInputSpecification "Use generic-lens or generic-optics with 'cdiInputSpecification' instead." #-}

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccChannelClass :: Lens.Lens' CreateChannel (Lude.Maybe ChannelClass)
ccChannelClass = Lens.lens (channelClass :: CreateChannel -> Lude.Maybe ChannelClass) (\s a -> s {channelClass = a} :: CreateChannel)
{-# DEPRECATED ccChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateChannel (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ccTags = Lens.lens (tags :: CreateChannel -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateChannel)
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encoderSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEncoderSettings :: Lens.Lens' CreateChannel (Lude.Maybe EncoderSettings)
ccEncoderSettings = Lens.lens (encoderSettings :: CreateChannel -> Lude.Maybe EncoderSettings) (\s a -> s {encoderSettings = a} :: CreateChannel)
{-# DEPRECATED ccEncoderSettings "Use generic-lens or generic-optics with 'encoderSettings' instead." #-}

-- | An optional Amazon Resource Name (ARN) of the role to assume when running the Channel.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccRoleARN :: Lens.Lens' CreateChannel (Lude.Maybe Lude.Text)
ccRoleARN = Lens.lens (roleARN :: CreateChannel -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: CreateChannel)
{-# DEPRECATED ccRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateChannel where
  type Rs CreateChannel = CreateChannelResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            Lude.<$> (x Lude..?> "channel") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateChannel where
  toJSON CreateChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("requestId" Lude..=) Lude.<$> requestId,
            ("logLevel" Lude..=) Lude.<$> logLevel,
            ("inputSpecification" Lude..=) Lude.<$> inputSpecification,
            ("inputAttachments" Lude..=) Lude.<$> inputAttachments,
            ("reserved" Lude..=) Lude.<$> reserved,
            ("destinations" Lude..=) Lude.<$> destinations,
            ("name" Lude..=) Lude.<$> name,
            ("cdiInputSpecification" Lude..=) Lude.<$> cdiInputSpecification,
            ("channelClass" Lude..=) Lude.<$> channelClass,
            ("tags" Lude..=) Lude.<$> tags,
            ("encoderSettings" Lude..=) Lude.<$> encoderSettings,
            ("roleArn" Lude..=) Lude.<$> roleARN
          ]
      )

instance Lude.ToPath CreateChannel where
  toPath = Lude.const "/prod/channels"

instance Lude.ToQuery CreateChannel where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for CreateChannelResponse
--
-- /See:/ 'mkCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { channel ::
      Lude.Maybe Channel,
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
-- * 'channel' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreateChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateChannelResponse
mkCreateChannelResponse pResponseStatus_ =
  CreateChannelResponse'
    { channel = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsChannel :: Lens.Lens' CreateChannelResponse (Lude.Maybe Channel)
ccrsChannel = Lens.lens (channel :: CreateChannelResponse -> Lude.Maybe Channel) (\s a -> s {channel = a} :: CreateChannelResponse)
{-# DEPRECATED ccrsChannel "Use generic-lens or generic-optics with 'channel' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateChannelResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateChannelResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
