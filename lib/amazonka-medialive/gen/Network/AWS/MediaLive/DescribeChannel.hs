{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a channel
module Network.AWS.MediaLive.DescribeChannel
  ( -- * Creating a request
    DescribeChannel (..),
    mkDescribeChannel,

    -- ** Request lenses
    dcChannelId,

    -- * Destructuring the response
    DescribeChannelResponse (..),
    mkDescribeChannelResponse,

    -- ** Response lenses
    dcrsState,
    dcrsLogLevel,
    dcrsARN,
    dcrsPipelinesRunningCount,
    dcrsPipelineDetails,
    dcrsInputSpecification,
    dcrsInputAttachments,
    dcrsDestinations,
    dcrsName,
    dcrsCdiInputSpecification,
    dcrsId,
    dcrsChannelClass,
    dcrsEgressEndpoints,
    dcrsTags,
    dcrsEncoderSettings,
    dcrsRoleARN,
    dcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DescribeChannelRequest
--
-- /See:/ 'mkDescribeChannel' smart constructor.
newtype DescribeChannel = DescribeChannel'
  { -- | channel ID
    channelId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeChannel' with the minimum fields required to make a request.
--
-- * 'channelId' - channel ID
mkDescribeChannel ::
  -- | 'channelId'
  Lude.Text ->
  DescribeChannel
mkDescribeChannel pChannelId_ =
  DescribeChannel' {channelId = pChannelId_}

-- | channel ID
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcChannelId :: Lens.Lens' DescribeChannel Lude.Text
dcChannelId = Lens.lens (channelId :: DescribeChannel -> Lude.Text) (\s a -> s {channelId = a} :: DescribeChannel)
{-# DEPRECATED dcChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

instance Lude.AWSRequest DescribeChannel where
  type Rs DescribeChannel = DescribeChannelResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeChannelResponse'
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

instance Lude.ToHeaders DescribeChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeChannel where
  toPath DescribeChannel' {..} =
    Lude.mconcat ["/prod/channels/", Lude.toBS channelId]

instance Lude.ToQuery DescribeChannel where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DescribeChannelResponse
--
-- /See:/ 'mkDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { state :: Lude.Maybe ChannelState,
    -- | The log level being written to CloudWatch Logs.
    logLevel :: Lude.Maybe LogLevel,
    -- | The unique arn of the channel.
    arn :: Lude.Maybe Lude.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Lude.Maybe Lude.Int,
    -- | Runtime details for the pipelines of a running channel.
    pipelineDetails :: Lude.Maybe [PipelineDetail],
    -- | Specification of network and file inputs for this channel
    inputSpecification :: Lude.Maybe InputSpecification,
    -- | List of input attachments for channel.
    inputAttachments :: Lude.Maybe [InputAttachment],
    -- | A list of destinations of the channel. For UDP outputs, there is one
    --
    -- destination per output. For other types (HLS, for example), there is
    -- one destination per packager.
    destinations :: Lude.Maybe [OutputDestination],
    -- | The name of the channel. (user-mutable)
    name :: Lude.Maybe Lude.Text,
    -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Lude.Maybe CdiInputSpecification,
    -- | The unique id of the channel.
    id :: Lude.Maybe Lude.Text,
    -- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
    channelClass :: Lude.Maybe ChannelClass,
    -- | The endpoints where outgoing connections initiate from
    egressEndpoints :: Lude.Maybe [ChannelEgressEndpoint],
    -- | A collection of key-value pairs.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    encoderSettings :: Lude.Maybe EncoderSettings,
    -- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeChannelResponse' with the minimum fields required to make a request.
--
-- * 'state' -
-- * 'logLevel' - The log level being written to CloudWatch Logs.
-- * 'arn' - The unique arn of the channel.
-- * 'pipelinesRunningCount' - The number of currently healthy pipelines.
-- * 'pipelineDetails' - Runtime details for the pipelines of a running channel.
-- * 'inputSpecification' - Specification of network and file inputs for this channel
-- * 'inputAttachments' - List of input attachments for channel.
-- * 'destinations' - A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
-- * 'name' - The name of the channel. (user-mutable)
-- * 'cdiInputSpecification' - Specification of CDI inputs for this channel
-- * 'id' - The unique id of the channel.
-- * 'channelClass' - The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
-- * 'egressEndpoints' - The endpoints where outgoing connections initiate from
-- * 'tags' - A collection of key-value pairs.
-- * 'encoderSettings' -
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
-- * 'responseStatus' - The response status code.
mkDescribeChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeChannelResponse
mkDescribeChannelResponse pResponseStatus_ =
  DescribeChannelResponse'
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
dcrsState :: Lens.Lens' DescribeChannelResponse (Lude.Maybe ChannelState)
dcrsState = Lens.lens (state :: DescribeChannelResponse -> Lude.Maybe ChannelState) (\s a -> s {state = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The log level being written to CloudWatch Logs.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsLogLevel :: Lens.Lens' DescribeChannelResponse (Lude.Maybe LogLevel)
dcrsLogLevel = Lens.lens (logLevel :: DescribeChannelResponse -> Lude.Maybe LogLevel) (\s a -> s {logLevel = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | The unique arn of the channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsARN :: Lens.Lens' DescribeChannelResponse (Lude.Maybe Lude.Text)
dcrsARN = Lens.lens (arn :: DescribeChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsPipelinesRunningCount :: Lens.Lens' DescribeChannelResponse (Lude.Maybe Lude.Int)
dcrsPipelinesRunningCount = Lens.lens (pipelinesRunningCount :: DescribeChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {pipelinesRunningCount = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | Runtime details for the pipelines of a running channel.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsPipelineDetails :: Lens.Lens' DescribeChannelResponse (Lude.Maybe [PipelineDetail])
dcrsPipelineDetails = Lens.lens (pipelineDetails :: DescribeChannelResponse -> Lude.Maybe [PipelineDetail]) (\s a -> s {pipelineDetails = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsPipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead." #-}

-- | Specification of network and file inputs for this channel
--
-- /Note:/ Consider using 'inputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsInputSpecification :: Lens.Lens' DescribeChannelResponse (Lude.Maybe InputSpecification)
dcrsInputSpecification = Lens.lens (inputSpecification :: DescribeChannelResponse -> Lude.Maybe InputSpecification) (\s a -> s {inputSpecification = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsInputSpecification "Use generic-lens or generic-optics with 'inputSpecification' instead." #-}

-- | List of input attachments for channel.
--
-- /Note:/ Consider using 'inputAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsInputAttachments :: Lens.Lens' DescribeChannelResponse (Lude.Maybe [InputAttachment])
dcrsInputAttachments = Lens.lens (inputAttachments :: DescribeChannelResponse -> Lude.Maybe [InputAttachment]) (\s a -> s {inputAttachments = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsInputAttachments "Use generic-lens or generic-optics with 'inputAttachments' instead." #-}

-- | A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsDestinations :: Lens.Lens' DescribeChannelResponse (Lude.Maybe [OutputDestination])
dcrsDestinations = Lens.lens (destinations :: DescribeChannelResponse -> Lude.Maybe [OutputDestination]) (\s a -> s {destinations = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the channel. (user-mutable)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsName :: Lens.Lens' DescribeChannelResponse (Lude.Maybe Lude.Text)
dcrsName = Lens.lens (name :: DescribeChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specification of CDI inputs for this channel
--
-- /Note:/ Consider using 'cdiInputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsCdiInputSpecification :: Lens.Lens' DescribeChannelResponse (Lude.Maybe CdiInputSpecification)
dcrsCdiInputSpecification = Lens.lens (cdiInputSpecification :: DescribeChannelResponse -> Lude.Maybe CdiInputSpecification) (\s a -> s {cdiInputSpecification = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsCdiInputSpecification "Use generic-lens or generic-optics with 'cdiInputSpecification' instead." #-}

-- | The unique id of the channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsId :: Lens.Lens' DescribeChannelResponse (Lude.Maybe Lude.Text)
dcrsId = Lens.lens (id :: DescribeChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsChannelClass :: Lens.Lens' DescribeChannelResponse (Lude.Maybe ChannelClass)
dcrsChannelClass = Lens.lens (channelClass :: DescribeChannelResponse -> Lude.Maybe ChannelClass) (\s a -> s {channelClass = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | The endpoints where outgoing connections initiate from
--
-- /Note:/ Consider using 'egressEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsEgressEndpoints :: Lens.Lens' DescribeChannelResponse (Lude.Maybe [ChannelEgressEndpoint])
dcrsEgressEndpoints = Lens.lens (egressEndpoints :: DescribeChannelResponse -> Lude.Maybe [ChannelEgressEndpoint]) (\s a -> s {egressEndpoints = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsEgressEndpoints "Use generic-lens or generic-optics with 'egressEndpoints' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsTags :: Lens.Lens' DescribeChannelResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dcrsTags = Lens.lens (tags :: DescribeChannelResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encoderSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsEncoderSettings :: Lens.Lens' DescribeChannelResponse (Lude.Maybe EncoderSettings)
dcrsEncoderSettings = Lens.lens (encoderSettings :: DescribeChannelResponse -> Lude.Maybe EncoderSettings) (\s a -> s {encoderSettings = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsEncoderSettings "Use generic-lens or generic-optics with 'encoderSettings' instead." #-}

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsRoleARN :: Lens.Lens' DescribeChannelResponse (Lude.Maybe Lude.Text)
dcrsRoleARN = Lens.lens (roleARN :: DescribeChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeChannelResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
