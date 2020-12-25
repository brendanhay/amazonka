{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    drsArn,
    drsCdiInputSpecification,
    drsChannelClass,
    drsDestinations,
    drsEgressEndpoints,
    drsEncoderSettings,
    drsId,
    drsInputAttachments,
    drsInputSpecification,
    drsLogLevel,
    drsName,
    drsPipelineDetails,
    drsPipelinesRunningCount,
    drsRoleArn,
    drsState,
    drsTags,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteChannelRequest
--
-- /See:/ 'mkDeleteChannel' smart constructor.
newtype DeleteChannel = DeleteChannel'
  { -- | Unique ID of the channel.
    channelId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteChannel' value with any optional fields omitted.
mkDeleteChannel ::
  -- | 'channelId'
  Core.Text ->
  DeleteChannel
mkDeleteChannel channelId = DeleteChannel' {channelId}

-- | Unique ID of the channel.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcChannelId :: Lens.Lens' DeleteChannel Core.Text
dcChannelId = Lens.field @"channelId"
{-# DEPRECATED dcChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

instance Core.AWSRequest DeleteChannel where
  type Rs DeleteChannel = DeleteChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath ("/prod/channels/" Core.<> (Core.toText channelId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteChannelResponse'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "cdiInputSpecification")
            Core.<*> (x Core..:? "channelClass")
            Core.<*> (x Core..:? "destinations")
            Core.<*> (x Core..:? "egressEndpoints")
            Core.<*> (x Core..:? "encoderSettings")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "inputAttachments")
            Core.<*> (x Core..:? "inputSpecification")
            Core.<*> (x Core..:? "logLevel")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "pipelineDetails")
            Core.<*> (x Core..:? "pipelinesRunningCount")
            Core.<*> (x Core..:? "roleArn")
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for DeleteChannelResponse
--
-- /See:/ 'mkDeleteChannelResponse' smart constructor.
data DeleteChannelResponse = DeleteChannelResponse'
  { -- | The unique arn of the channel.
    arn :: Core.Maybe Core.Text,
    -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Core.Maybe Types.CdiInputSpecification,
    -- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
    channelClass :: Core.Maybe Types.ChannelClass,
    -- | A list of destinations of the channel. For UDP outputs, there is one
    --
    -- destination per output. For other types (HLS, for example), there is
    -- one destination per packager.
    destinations :: Core.Maybe [Types.OutputDestination],
    -- | The endpoints where outgoing connections initiate from
    egressEndpoints :: Core.Maybe [Types.ChannelEgressEndpoint],
    encoderSettings :: Core.Maybe Types.EncoderSettings,
    -- | The unique id of the channel.
    id :: Core.Maybe Core.Text,
    -- | List of input attachments for channel.
    inputAttachments :: Core.Maybe [Types.InputAttachment],
    -- | Specification of network and file inputs for this channel
    inputSpecification :: Core.Maybe Types.InputSpecification,
    -- | The log level being written to CloudWatch Logs.
    logLevel :: Core.Maybe Types.LogLevel,
    -- | The name of the channel. (user-mutable)
    name :: Core.Maybe Core.Text,
    -- | Runtime details for the pipelines of a running channel.
    pipelineDetails :: Core.Maybe [Types.PipelineDetail],
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
    roleArn :: Core.Maybe Core.Text,
    state :: Core.Maybe Types.ChannelState,
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteChannelResponse' value with any optional fields omitted.
mkDeleteChannelResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteChannelResponse
mkDeleteChannelResponse responseStatus =
  DeleteChannelResponse'
    { arn = Core.Nothing,
      cdiInputSpecification = Core.Nothing,
      channelClass = Core.Nothing,
      destinations = Core.Nothing,
      egressEndpoints = Core.Nothing,
      encoderSettings = Core.Nothing,
      id = Core.Nothing,
      inputAttachments = Core.Nothing,
      inputSpecification = Core.Nothing,
      logLevel = Core.Nothing,
      name = Core.Nothing,
      pipelineDetails = Core.Nothing,
      pipelinesRunningCount = Core.Nothing,
      roleArn = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | The unique arn of the channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsArn :: Lens.Lens' DeleteChannelResponse (Core.Maybe Core.Text)
drsArn = Lens.field @"arn"
{-# DEPRECATED drsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Specification of CDI inputs for this channel
--
-- /Note:/ Consider using 'cdiInputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCdiInputSpecification :: Lens.Lens' DeleteChannelResponse (Core.Maybe Types.CdiInputSpecification)
drsCdiInputSpecification = Lens.field @"cdiInputSpecification"
{-# DEPRECATED drsCdiInputSpecification "Use generic-lens or generic-optics with 'cdiInputSpecification' instead." #-}

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsChannelClass :: Lens.Lens' DeleteChannelResponse (Core.Maybe Types.ChannelClass)
drsChannelClass = Lens.field @"channelClass"
{-# DEPRECATED drsChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDestinations :: Lens.Lens' DeleteChannelResponse (Core.Maybe [Types.OutputDestination])
drsDestinations = Lens.field @"destinations"
{-# DEPRECATED drsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The endpoints where outgoing connections initiate from
--
-- /Note:/ Consider using 'egressEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEgressEndpoints :: Lens.Lens' DeleteChannelResponse (Core.Maybe [Types.ChannelEgressEndpoint])
drsEgressEndpoints = Lens.field @"egressEndpoints"
{-# DEPRECATED drsEgressEndpoints "Use generic-lens or generic-optics with 'egressEndpoints' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encoderSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEncoderSettings :: Lens.Lens' DeleteChannelResponse (Core.Maybe Types.EncoderSettings)
drsEncoderSettings = Lens.field @"encoderSettings"
{-# DEPRECATED drsEncoderSettings "Use generic-lens or generic-optics with 'encoderSettings' instead." #-}

-- | The unique id of the channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsId :: Lens.Lens' DeleteChannelResponse (Core.Maybe Core.Text)
drsId = Lens.field @"id"
{-# DEPRECATED drsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | List of input attachments for channel.
--
-- /Note:/ Consider using 'inputAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsInputAttachments :: Lens.Lens' DeleteChannelResponse (Core.Maybe [Types.InputAttachment])
drsInputAttachments = Lens.field @"inputAttachments"
{-# DEPRECATED drsInputAttachments "Use generic-lens or generic-optics with 'inputAttachments' instead." #-}

-- | Specification of network and file inputs for this channel
--
-- /Note:/ Consider using 'inputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsInputSpecification :: Lens.Lens' DeleteChannelResponse (Core.Maybe Types.InputSpecification)
drsInputSpecification = Lens.field @"inputSpecification"
{-# DEPRECATED drsInputSpecification "Use generic-lens or generic-optics with 'inputSpecification' instead." #-}

-- | The log level being written to CloudWatch Logs.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsLogLevel :: Lens.Lens' DeleteChannelResponse (Core.Maybe Types.LogLevel)
drsLogLevel = Lens.field @"logLevel"
{-# DEPRECATED drsLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | The name of the channel. (user-mutable)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsName :: Lens.Lens' DeleteChannelResponse (Core.Maybe Core.Text)
drsName = Lens.field @"name"
{-# DEPRECATED drsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Runtime details for the pipelines of a running channel.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsPipelineDetails :: Lens.Lens' DeleteChannelResponse (Core.Maybe [Types.PipelineDetail])
drsPipelineDetails = Lens.field @"pipelineDetails"
{-# DEPRECATED drsPipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsPipelinesRunningCount :: Lens.Lens' DeleteChannelResponse (Core.Maybe Core.Int)
drsPipelinesRunningCount = Lens.field @"pipelinesRunningCount"
{-# DEPRECATED drsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRoleArn :: Lens.Lens' DeleteChannelResponse (Core.Maybe Core.Text)
drsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED drsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsState :: Lens.Lens' DeleteChannelResponse (Core.Maybe Types.ChannelState)
drsState = Lens.field @"state"
{-# DEPRECATED drsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTags :: Lens.Lens' DeleteChannelResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
drsTags = Lens.field @"tags"
{-# DEPRECATED drsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteChannelResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
