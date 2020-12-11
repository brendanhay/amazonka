{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.StartChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an existing channel
module Network.AWS.MediaLive.StartChannel
  ( -- * Creating a request
    StartChannel (..),
    mkStartChannel,

    -- ** Request lenses
    scChannelId,

    -- * Destructuring the response
    StartChannelResponse (..),
    mkStartChannelResponse,

    -- ** Response lenses
    scrsState,
    scrsLogLevel,
    scrsARN,
    scrsPipelinesRunningCount,
    scrsPipelineDetails,
    scrsInputSpecification,
    scrsInputAttachments,
    scrsDestinations,
    scrsName,
    scrsCdiInputSpecification,
    scrsId,
    scrsChannelClass,
    scrsEgressEndpoints,
    scrsTags,
    scrsEncoderSettings,
    scrsRoleARN,
    scrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for StartChannelRequest
--
-- /See:/ 'mkStartChannel' smart constructor.
newtype StartChannel = StartChannel' {channelId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartChannel' with the minimum fields required to make a request.
--
-- * 'channelId' - A request to start a channel
mkStartChannel ::
  -- | 'channelId'
  Lude.Text ->
  StartChannel
mkStartChannel pChannelId_ = StartChannel' {channelId = pChannelId_}

-- | A request to start a channel
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scChannelId :: Lens.Lens' StartChannel Lude.Text
scChannelId = Lens.lens (channelId :: StartChannel -> Lude.Text) (\s a -> s {channelId = a} :: StartChannel)
{-# DEPRECATED scChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

instance Lude.AWSRequest StartChannel where
  type Rs StartChannel = StartChannelResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartChannelResponse'
            Lude.<$> (x Lude..?> "state")
            Lude.<*> (x Lude..?> "logLevel")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "pipelinesRunningCount")
            Lude.<*> (x Lude..?> "pipelineDetails" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "inputSpecification")
            Lude.<*> (x Lude..?> "inputAttachments" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "destinations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "cdiInputSpecification")
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (x Lude..?> "channelClass")
            Lude.<*> (x Lude..?> "egressEndpoints" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "encoderSettings")
            Lude.<*> (x Lude..?> "roleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartChannel where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath StartChannel where
  toPath StartChannel' {..} =
    Lude.mconcat ["/prod/channels/", Lude.toBS channelId, "/start"]

instance Lude.ToQuery StartChannel where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for StartChannelResponse
--
-- /See:/ 'mkStartChannelResponse' smart constructor.
data StartChannelResponse = StartChannelResponse'
  { state ::
      Lude.Maybe ChannelState,
    logLevel :: Lude.Maybe LogLevel,
    arn :: Lude.Maybe Lude.Text,
    pipelinesRunningCount :: Lude.Maybe Lude.Int,
    pipelineDetails :: Lude.Maybe [PipelineDetail],
    inputSpecification ::
      Lude.Maybe InputSpecification,
    inputAttachments :: Lude.Maybe [InputAttachment],
    destinations :: Lude.Maybe [OutputDestination],
    name :: Lude.Maybe Lude.Text,
    cdiInputSpecification ::
      Lude.Maybe CdiInputSpecification,
    id :: Lude.Maybe Lude.Text,
    channelClass :: Lude.Maybe ChannelClass,
    egressEndpoints ::
      Lude.Maybe [ChannelEgressEndpoint],
    tags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    encoderSettings :: Lude.Maybe EncoderSettings,
    roleARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'StartChannelResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The unique arn of the channel.
-- * 'cdiInputSpecification' - Specification of CDI inputs for this channel
-- * 'channelClass' - The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
-- * 'destinations' - A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
-- * 'egressEndpoints' - The endpoints where outgoing connections initiate from
-- * 'encoderSettings' - Undocumented field.
-- * 'id' - The unique id of the channel.
-- * 'inputAttachments' - List of input attachments for channel.
-- * 'inputSpecification' - Specification of network and file inputs for this channel
-- * 'logLevel' - The log level being written to CloudWatch Logs.
-- * 'name' - The name of the channel. (user-mutable)
-- * 'pipelineDetails' - Runtime details for the pipelines of a running channel.
-- * 'pipelinesRunningCount' - The number of currently healthy pipelines.
-- * 'responseStatus' - The response status code.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
-- * 'state' - Undocumented field.
-- * 'tags' - A collection of key-value pairs.
mkStartChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartChannelResponse
mkStartChannelResponse pResponseStatus_ =
  StartChannelResponse'
    { state = Lude.Nothing,
      logLevel = Lude.Nothing,
      arn = Lude.Nothing,
      pipelinesRunningCount = Lude.Nothing,
      pipelineDetails = Lude.Nothing,
      inputSpecification = Lude.Nothing,
      inputAttachments = Lude.Nothing,
      destinations = Lude.Nothing,
      name = Lude.Nothing,
      cdiInputSpecification = Lude.Nothing,
      id = Lude.Nothing,
      channelClass = Lude.Nothing,
      egressEndpoints = Lude.Nothing,
      tags = Lude.Nothing,
      encoderSettings = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsState :: Lens.Lens' StartChannelResponse (Lude.Maybe ChannelState)
scrsState = Lens.lens (state :: StartChannelResponse -> Lude.Maybe ChannelState) (\s a -> s {state = a} :: StartChannelResponse)
{-# DEPRECATED scrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The log level being written to CloudWatch Logs.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsLogLevel :: Lens.Lens' StartChannelResponse (Lude.Maybe LogLevel)
scrsLogLevel = Lens.lens (logLevel :: StartChannelResponse -> Lude.Maybe LogLevel) (\s a -> s {logLevel = a} :: StartChannelResponse)
{-# DEPRECATED scrsLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | The unique arn of the channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsARN :: Lens.Lens' StartChannelResponse (Lude.Maybe Lude.Text)
scrsARN = Lens.lens (arn :: StartChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: StartChannelResponse)
{-# DEPRECATED scrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsPipelinesRunningCount :: Lens.Lens' StartChannelResponse (Lude.Maybe Lude.Int)
scrsPipelinesRunningCount = Lens.lens (pipelinesRunningCount :: StartChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {pipelinesRunningCount = a} :: StartChannelResponse)
{-# DEPRECATED scrsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | Runtime details for the pipelines of a running channel.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsPipelineDetails :: Lens.Lens' StartChannelResponse (Lude.Maybe [PipelineDetail])
scrsPipelineDetails = Lens.lens (pipelineDetails :: StartChannelResponse -> Lude.Maybe [PipelineDetail]) (\s a -> s {pipelineDetails = a} :: StartChannelResponse)
{-# DEPRECATED scrsPipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead." #-}

-- | Specification of network and file inputs for this channel
--
-- /Note:/ Consider using 'inputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsInputSpecification :: Lens.Lens' StartChannelResponse (Lude.Maybe InputSpecification)
scrsInputSpecification = Lens.lens (inputSpecification :: StartChannelResponse -> Lude.Maybe InputSpecification) (\s a -> s {inputSpecification = a} :: StartChannelResponse)
{-# DEPRECATED scrsInputSpecification "Use generic-lens or generic-optics with 'inputSpecification' instead." #-}

-- | List of input attachments for channel.
--
-- /Note:/ Consider using 'inputAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsInputAttachments :: Lens.Lens' StartChannelResponse (Lude.Maybe [InputAttachment])
scrsInputAttachments = Lens.lens (inputAttachments :: StartChannelResponse -> Lude.Maybe [InputAttachment]) (\s a -> s {inputAttachments = a} :: StartChannelResponse)
{-# DEPRECATED scrsInputAttachments "Use generic-lens or generic-optics with 'inputAttachments' instead." #-}

-- | A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsDestinations :: Lens.Lens' StartChannelResponse (Lude.Maybe [OutputDestination])
scrsDestinations = Lens.lens (destinations :: StartChannelResponse -> Lude.Maybe [OutputDestination]) (\s a -> s {destinations = a} :: StartChannelResponse)
{-# DEPRECATED scrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the channel. (user-mutable)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsName :: Lens.Lens' StartChannelResponse (Lude.Maybe Lude.Text)
scrsName = Lens.lens (name :: StartChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StartChannelResponse)
{-# DEPRECATED scrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specification of CDI inputs for this channel
--
-- /Note:/ Consider using 'cdiInputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsCdiInputSpecification :: Lens.Lens' StartChannelResponse (Lude.Maybe CdiInputSpecification)
scrsCdiInputSpecification = Lens.lens (cdiInputSpecification :: StartChannelResponse -> Lude.Maybe CdiInputSpecification) (\s a -> s {cdiInputSpecification = a} :: StartChannelResponse)
{-# DEPRECATED scrsCdiInputSpecification "Use generic-lens or generic-optics with 'cdiInputSpecification' instead." #-}

-- | The unique id of the channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsId :: Lens.Lens' StartChannelResponse (Lude.Maybe Lude.Text)
scrsId = Lens.lens (id :: StartChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: StartChannelResponse)
{-# DEPRECATED scrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsChannelClass :: Lens.Lens' StartChannelResponse (Lude.Maybe ChannelClass)
scrsChannelClass = Lens.lens (channelClass :: StartChannelResponse -> Lude.Maybe ChannelClass) (\s a -> s {channelClass = a} :: StartChannelResponse)
{-# DEPRECATED scrsChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | The endpoints where outgoing connections initiate from
--
-- /Note:/ Consider using 'egressEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsEgressEndpoints :: Lens.Lens' StartChannelResponse (Lude.Maybe [ChannelEgressEndpoint])
scrsEgressEndpoints = Lens.lens (egressEndpoints :: StartChannelResponse -> Lude.Maybe [ChannelEgressEndpoint]) (\s a -> s {egressEndpoints = a} :: StartChannelResponse)
{-# DEPRECATED scrsEgressEndpoints "Use generic-lens or generic-optics with 'egressEndpoints' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsTags :: Lens.Lens' StartChannelResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
scrsTags = Lens.lens (tags :: StartChannelResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: StartChannelResponse)
{-# DEPRECATED scrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encoderSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsEncoderSettings :: Lens.Lens' StartChannelResponse (Lude.Maybe EncoderSettings)
scrsEncoderSettings = Lens.lens (encoderSettings :: StartChannelResponse -> Lude.Maybe EncoderSettings) (\s a -> s {encoderSettings = a} :: StartChannelResponse)
{-# DEPRECATED scrsEncoderSettings "Use generic-lens or generic-optics with 'encoderSettings' instead." #-}

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsRoleARN :: Lens.Lens' StartChannelResponse (Lude.Maybe Lude.Text)
scrsRoleARN = Lens.lens (roleARN :: StartChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: StartChannelResponse)
{-# DEPRECATED scrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsResponseStatus :: Lens.Lens' StartChannelResponse Lude.Int
scrsResponseStatus = Lens.lens (responseStatus :: StartChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartChannelResponse)
{-# DEPRECATED scrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
