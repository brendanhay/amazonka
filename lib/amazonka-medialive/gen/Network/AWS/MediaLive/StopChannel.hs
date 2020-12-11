{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.StopChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running channel
module Network.AWS.MediaLive.StopChannel
  ( -- * Creating a request
    StopChannel (..),
    mkStopChannel,

    -- ** Request lenses
    sChannelId,

    -- * Destructuring the response
    StopChannelResponse (..),
    mkStopChannelResponse,

    -- ** Response lenses
    srsState,
    srsLogLevel,
    srsARN,
    srsPipelinesRunningCount,
    srsPipelineDetails,
    srsInputSpecification,
    srsInputAttachments,
    srsDestinations,
    srsName,
    srsCdiInputSpecification,
    srsId,
    srsChannelClass,
    srsEgressEndpoints,
    srsTags,
    srsEncoderSettings,
    srsRoleARN,
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for StopChannelRequest
--
-- /See:/ 'mkStopChannel' smart constructor.
newtype StopChannel = StopChannel' {channelId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopChannel' with the minimum fields required to make a request.
--
-- * 'channelId' - A request to stop a running channel
mkStopChannel ::
  -- | 'channelId'
  Lude.Text ->
  StopChannel
mkStopChannel pChannelId_ = StopChannel' {channelId = pChannelId_}

-- | A request to stop a running channel
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sChannelId :: Lens.Lens' StopChannel Lude.Text
sChannelId = Lens.lens (channelId :: StopChannel -> Lude.Text) (\s a -> s {channelId = a} :: StopChannel)
{-# DEPRECATED sChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

instance Lude.AWSRequest StopChannel where
  type Rs StopChannel = StopChannelResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopChannelResponse'
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

instance Lude.ToHeaders StopChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopChannel where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath StopChannel where
  toPath StopChannel' {..} =
    Lude.mconcat ["/prod/channels/", Lude.toBS channelId, "/stop"]

instance Lude.ToQuery StopChannel where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for StopChannelResponse
--
-- /See:/ 'mkStopChannelResponse' smart constructor.
data StopChannelResponse = StopChannelResponse'
  { state ::
      Lude.Maybe ChannelState,
    logLevel :: Lude.Maybe LogLevel,
    arn :: Lude.Maybe Lude.Text,
    pipelinesRunningCount :: Lude.Maybe Lude.Int,
    pipelineDetails :: Lude.Maybe [PipelineDetail],
    inputSpecification :: Lude.Maybe InputSpecification,
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

-- | Creates a value of 'StopChannelResponse' with the minimum fields required to make a request.
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
mkStopChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopChannelResponse
mkStopChannelResponse pResponseStatus_ =
  StopChannelResponse'
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
srsState :: Lens.Lens' StopChannelResponse (Lude.Maybe ChannelState)
srsState = Lens.lens (state :: StopChannelResponse -> Lude.Maybe ChannelState) (\s a -> s {state = a} :: StopChannelResponse)
{-# DEPRECATED srsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The log level being written to CloudWatch Logs.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsLogLevel :: Lens.Lens' StopChannelResponse (Lude.Maybe LogLevel)
srsLogLevel = Lens.lens (logLevel :: StopChannelResponse -> Lude.Maybe LogLevel) (\s a -> s {logLevel = a} :: StopChannelResponse)
{-# DEPRECATED srsLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | The unique arn of the channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsARN :: Lens.Lens' StopChannelResponse (Lude.Maybe Lude.Text)
srsARN = Lens.lens (arn :: StopChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: StopChannelResponse)
{-# DEPRECATED srsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsPipelinesRunningCount :: Lens.Lens' StopChannelResponse (Lude.Maybe Lude.Int)
srsPipelinesRunningCount = Lens.lens (pipelinesRunningCount :: StopChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {pipelinesRunningCount = a} :: StopChannelResponse)
{-# DEPRECATED srsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | Runtime details for the pipelines of a running channel.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsPipelineDetails :: Lens.Lens' StopChannelResponse (Lude.Maybe [PipelineDetail])
srsPipelineDetails = Lens.lens (pipelineDetails :: StopChannelResponse -> Lude.Maybe [PipelineDetail]) (\s a -> s {pipelineDetails = a} :: StopChannelResponse)
{-# DEPRECATED srsPipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead." #-}

-- | Specification of network and file inputs for this channel
--
-- /Note:/ Consider using 'inputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsInputSpecification :: Lens.Lens' StopChannelResponse (Lude.Maybe InputSpecification)
srsInputSpecification = Lens.lens (inputSpecification :: StopChannelResponse -> Lude.Maybe InputSpecification) (\s a -> s {inputSpecification = a} :: StopChannelResponse)
{-# DEPRECATED srsInputSpecification "Use generic-lens or generic-optics with 'inputSpecification' instead." #-}

-- | List of input attachments for channel.
--
-- /Note:/ Consider using 'inputAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsInputAttachments :: Lens.Lens' StopChannelResponse (Lude.Maybe [InputAttachment])
srsInputAttachments = Lens.lens (inputAttachments :: StopChannelResponse -> Lude.Maybe [InputAttachment]) (\s a -> s {inputAttachments = a} :: StopChannelResponse)
{-# DEPRECATED srsInputAttachments "Use generic-lens or generic-optics with 'inputAttachments' instead." #-}

-- | A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsDestinations :: Lens.Lens' StopChannelResponse (Lude.Maybe [OutputDestination])
srsDestinations = Lens.lens (destinations :: StopChannelResponse -> Lude.Maybe [OutputDestination]) (\s a -> s {destinations = a} :: StopChannelResponse)
{-# DEPRECATED srsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the channel. (user-mutable)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsName :: Lens.Lens' StopChannelResponse (Lude.Maybe Lude.Text)
srsName = Lens.lens (name :: StopChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StopChannelResponse)
{-# DEPRECATED srsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specification of CDI inputs for this channel
--
-- /Note:/ Consider using 'cdiInputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsCdiInputSpecification :: Lens.Lens' StopChannelResponse (Lude.Maybe CdiInputSpecification)
srsCdiInputSpecification = Lens.lens (cdiInputSpecification :: StopChannelResponse -> Lude.Maybe CdiInputSpecification) (\s a -> s {cdiInputSpecification = a} :: StopChannelResponse)
{-# DEPRECATED srsCdiInputSpecification "Use generic-lens or generic-optics with 'cdiInputSpecification' instead." #-}

-- | The unique id of the channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsId :: Lens.Lens' StopChannelResponse (Lude.Maybe Lude.Text)
srsId = Lens.lens (id :: StopChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: StopChannelResponse)
{-# DEPRECATED srsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsChannelClass :: Lens.Lens' StopChannelResponse (Lude.Maybe ChannelClass)
srsChannelClass = Lens.lens (channelClass :: StopChannelResponse -> Lude.Maybe ChannelClass) (\s a -> s {channelClass = a} :: StopChannelResponse)
{-# DEPRECATED srsChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | The endpoints where outgoing connections initiate from
--
-- /Note:/ Consider using 'egressEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsEgressEndpoints :: Lens.Lens' StopChannelResponse (Lude.Maybe [ChannelEgressEndpoint])
srsEgressEndpoints = Lens.lens (egressEndpoints :: StopChannelResponse -> Lude.Maybe [ChannelEgressEndpoint]) (\s a -> s {egressEndpoints = a} :: StopChannelResponse)
{-# DEPRECATED srsEgressEndpoints "Use generic-lens or generic-optics with 'egressEndpoints' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsTags :: Lens.Lens' StopChannelResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
srsTags = Lens.lens (tags :: StopChannelResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: StopChannelResponse)
{-# DEPRECATED srsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encoderSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsEncoderSettings :: Lens.Lens' StopChannelResponse (Lude.Maybe EncoderSettings)
srsEncoderSettings = Lens.lens (encoderSettings :: StopChannelResponse -> Lude.Maybe EncoderSettings) (\s a -> s {encoderSettings = a} :: StopChannelResponse)
{-# DEPRECATED srsEncoderSettings "Use generic-lens or generic-optics with 'encoderSettings' instead." #-}

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsRoleARN :: Lens.Lens' StopChannelResponse (Lude.Maybe Lude.Text)
srsRoleARN = Lens.lens (roleARN :: StopChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: StopChannelResponse)
{-# DEPRECATED srsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopChannelResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StopChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopChannelResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
