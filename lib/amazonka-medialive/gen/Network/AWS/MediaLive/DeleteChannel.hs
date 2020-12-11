{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DeleteChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts deletion of channel. The associated outputs are also deleted.
module Network.AWS.MediaLive.DeleteChannel
  ( -- * Creating a request
    DeleteChannel (..),
    mkDeleteChannel,

    -- ** Request lenses
    dcChannelId,

    -- * Destructuring the response
    DeleteChannelResponse (..),
    mkDeleteChannelResponse,

    -- ** Response lenses
    drsState,
    drsLogLevel,
    drsARN,
    drsPipelinesRunningCount,
    drsPipelineDetails,
    drsInputSpecification,
    drsInputAttachments,
    drsDestinations,
    drsName,
    drsCdiInputSpecification,
    drsId,
    drsChannelClass,
    drsEgressEndpoints,
    drsTags,
    drsEncoderSettings,
    drsRoleARN,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DeleteChannelRequest
--
-- /See:/ 'mkDeleteChannel' smart constructor.
newtype DeleteChannel = DeleteChannel' {channelId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteChannel' with the minimum fields required to make a request.
--
-- * 'channelId' - Unique ID of the channel.
mkDeleteChannel ::
  -- | 'channelId'
  Lude.Text ->
  DeleteChannel
mkDeleteChannel pChannelId_ =
  DeleteChannel' {channelId = pChannelId_}

-- | Unique ID of the channel.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcChannelId :: Lens.Lens' DeleteChannel Lude.Text
dcChannelId = Lens.lens (channelId :: DeleteChannel -> Lude.Text) (\s a -> s {channelId = a} :: DeleteChannel)
{-# DEPRECATED dcChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

instance Lude.AWSRequest DeleteChannel where
  type Rs DeleteChannel = DeleteChannelResponse
  request = Req.delete mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteChannelResponse'
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

instance Lude.ToHeaders DeleteChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteChannel where
  toPath DeleteChannel' {..} =
    Lude.mconcat ["/prod/channels/", Lude.toBS channelId]

instance Lude.ToQuery DeleteChannel where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DeleteChannelResponse
--
-- /See:/ 'mkDeleteChannelResponse' smart constructor.
data DeleteChannelResponse = DeleteChannelResponse'
  { state ::
      Lude.Maybe ChannelState,
    logLevel :: Lude.Maybe LogLevel,
    arn :: Lude.Maybe Lude.Text,
    pipelinesRunningCount :: Lude.Maybe Lude.Int,
    pipelineDetails :: Lude.Maybe [PipelineDetail],
    inputSpecification ::
      Lude.Maybe InputSpecification,
    inputAttachments ::
      Lude.Maybe [InputAttachment],
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

-- | Creates a value of 'DeleteChannelResponse' with the minimum fields required to make a request.
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
mkDeleteChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteChannelResponse
mkDeleteChannelResponse pResponseStatus_ =
  DeleteChannelResponse'
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
drsState :: Lens.Lens' DeleteChannelResponse (Lude.Maybe ChannelState)
drsState = Lens.lens (state :: DeleteChannelResponse -> Lude.Maybe ChannelState) (\s a -> s {state = a} :: DeleteChannelResponse)
{-# DEPRECATED drsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The log level being written to CloudWatch Logs.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsLogLevel :: Lens.Lens' DeleteChannelResponse (Lude.Maybe LogLevel)
drsLogLevel = Lens.lens (logLevel :: DeleteChannelResponse -> Lude.Maybe LogLevel) (\s a -> s {logLevel = a} :: DeleteChannelResponse)
{-# DEPRECATED drsLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | The unique arn of the channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsARN :: Lens.Lens' DeleteChannelResponse (Lude.Maybe Lude.Text)
drsARN = Lens.lens (arn :: DeleteChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DeleteChannelResponse)
{-# DEPRECATED drsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsPipelinesRunningCount :: Lens.Lens' DeleteChannelResponse (Lude.Maybe Lude.Int)
drsPipelinesRunningCount = Lens.lens (pipelinesRunningCount :: DeleteChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {pipelinesRunningCount = a} :: DeleteChannelResponse)
{-# DEPRECATED drsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | Runtime details for the pipelines of a running channel.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsPipelineDetails :: Lens.Lens' DeleteChannelResponse (Lude.Maybe [PipelineDetail])
drsPipelineDetails = Lens.lens (pipelineDetails :: DeleteChannelResponse -> Lude.Maybe [PipelineDetail]) (\s a -> s {pipelineDetails = a} :: DeleteChannelResponse)
{-# DEPRECATED drsPipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead." #-}

-- | Specification of network and file inputs for this channel
--
-- /Note:/ Consider using 'inputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsInputSpecification :: Lens.Lens' DeleteChannelResponse (Lude.Maybe InputSpecification)
drsInputSpecification = Lens.lens (inputSpecification :: DeleteChannelResponse -> Lude.Maybe InputSpecification) (\s a -> s {inputSpecification = a} :: DeleteChannelResponse)
{-# DEPRECATED drsInputSpecification "Use generic-lens or generic-optics with 'inputSpecification' instead." #-}

-- | List of input attachments for channel.
--
-- /Note:/ Consider using 'inputAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsInputAttachments :: Lens.Lens' DeleteChannelResponse (Lude.Maybe [InputAttachment])
drsInputAttachments = Lens.lens (inputAttachments :: DeleteChannelResponse -> Lude.Maybe [InputAttachment]) (\s a -> s {inputAttachments = a} :: DeleteChannelResponse)
{-# DEPRECATED drsInputAttachments "Use generic-lens or generic-optics with 'inputAttachments' instead." #-}

-- | A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDestinations :: Lens.Lens' DeleteChannelResponse (Lude.Maybe [OutputDestination])
drsDestinations = Lens.lens (destinations :: DeleteChannelResponse -> Lude.Maybe [OutputDestination]) (\s a -> s {destinations = a} :: DeleteChannelResponse)
{-# DEPRECATED drsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the channel. (user-mutable)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsName :: Lens.Lens' DeleteChannelResponse (Lude.Maybe Lude.Text)
drsName = Lens.lens (name :: DeleteChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DeleteChannelResponse)
{-# DEPRECATED drsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specification of CDI inputs for this channel
--
-- /Note:/ Consider using 'cdiInputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCdiInputSpecification :: Lens.Lens' DeleteChannelResponse (Lude.Maybe CdiInputSpecification)
drsCdiInputSpecification = Lens.lens (cdiInputSpecification :: DeleteChannelResponse -> Lude.Maybe CdiInputSpecification) (\s a -> s {cdiInputSpecification = a} :: DeleteChannelResponse)
{-# DEPRECATED drsCdiInputSpecification "Use generic-lens or generic-optics with 'cdiInputSpecification' instead." #-}

-- | The unique id of the channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsId :: Lens.Lens' DeleteChannelResponse (Lude.Maybe Lude.Text)
drsId = Lens.lens (id :: DeleteChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DeleteChannelResponse)
{-# DEPRECATED drsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsChannelClass :: Lens.Lens' DeleteChannelResponse (Lude.Maybe ChannelClass)
drsChannelClass = Lens.lens (channelClass :: DeleteChannelResponse -> Lude.Maybe ChannelClass) (\s a -> s {channelClass = a} :: DeleteChannelResponse)
{-# DEPRECATED drsChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | The endpoints where outgoing connections initiate from
--
-- /Note:/ Consider using 'egressEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEgressEndpoints :: Lens.Lens' DeleteChannelResponse (Lude.Maybe [ChannelEgressEndpoint])
drsEgressEndpoints = Lens.lens (egressEndpoints :: DeleteChannelResponse -> Lude.Maybe [ChannelEgressEndpoint]) (\s a -> s {egressEndpoints = a} :: DeleteChannelResponse)
{-# DEPRECATED drsEgressEndpoints "Use generic-lens or generic-optics with 'egressEndpoints' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTags :: Lens.Lens' DeleteChannelResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
drsTags = Lens.lens (tags :: DeleteChannelResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DeleteChannelResponse)
{-# DEPRECATED drsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encoderSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEncoderSettings :: Lens.Lens' DeleteChannelResponse (Lude.Maybe EncoderSettings)
drsEncoderSettings = Lens.lens (encoderSettings :: DeleteChannelResponse -> Lude.Maybe EncoderSettings) (\s a -> s {encoderSettings = a} :: DeleteChannelResponse)
{-# DEPRECATED drsEncoderSettings "Use generic-lens or generic-optics with 'encoderSettings' instead." #-}

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRoleARN :: Lens.Lens' DeleteChannelResponse (Lude.Maybe Lude.Text)
drsRoleARN = Lens.lens (roleARN :: DeleteChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DeleteChannelResponse)
{-# DEPRECATED drsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteChannelResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteChannelResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
