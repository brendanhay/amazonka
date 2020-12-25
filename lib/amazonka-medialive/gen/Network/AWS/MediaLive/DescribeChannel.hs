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
    dcfChannelId,

    -- * Destructuring the response
    DescribeChannelResponse (..),
    mkDescribeChannelResponse,

    -- ** Response lenses
    dcrrsArn,
    dcrrsCdiInputSpecification,
    dcrrsChannelClass,
    dcrrsDestinations,
    dcrrsEgressEndpoints,
    dcrrsEncoderSettings,
    dcrrsId,
    dcrrsInputAttachments,
    dcrrsInputSpecification,
    dcrrsLogLevel,
    dcrrsName,
    dcrrsPipelineDetails,
    dcrrsPipelinesRunningCount,
    dcrrsRoleArn,
    dcrrsState,
    dcrrsTags,
    dcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeChannelRequest
--
-- /See:/ 'mkDescribeChannel' smart constructor.
newtype DescribeChannel = DescribeChannel'
  { -- | channel ID
    channelId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeChannel' value with any optional fields omitted.
mkDescribeChannel ::
  -- | 'channelId'
  Core.Text ->
  DescribeChannel
mkDescribeChannel channelId = DescribeChannel' {channelId}

-- | channel ID
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfChannelId :: Lens.Lens' DescribeChannel Core.Text
dcfChannelId = Lens.field @"channelId"
{-# DEPRECATED dcfChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

instance Core.AWSRequest DescribeChannel where
  type Rs DescribeChannel = DescribeChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
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
          DescribeChannelResponse'
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

-- | Placeholder documentation for DescribeChannelResponse
--
-- /See:/ 'mkDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
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

-- | Creates a 'DescribeChannelResponse' value with any optional fields omitted.
mkDescribeChannelResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeChannelResponse
mkDescribeChannelResponse responseStatus =
  DescribeChannelResponse'
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
dcrrsArn :: Lens.Lens' DescribeChannelResponse (Core.Maybe Core.Text)
dcrrsArn = Lens.field @"arn"
{-# DEPRECATED dcrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Specification of CDI inputs for this channel
--
-- /Note:/ Consider using 'cdiInputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCdiInputSpecification :: Lens.Lens' DescribeChannelResponse (Core.Maybe Types.CdiInputSpecification)
dcrrsCdiInputSpecification = Lens.field @"cdiInputSpecification"
{-# DEPRECATED dcrrsCdiInputSpecification "Use generic-lens or generic-optics with 'cdiInputSpecification' instead." #-}

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsChannelClass :: Lens.Lens' DescribeChannelResponse (Core.Maybe Types.ChannelClass)
dcrrsChannelClass = Lens.field @"channelClass"
{-# DEPRECATED dcrrsChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsDestinations :: Lens.Lens' DescribeChannelResponse (Core.Maybe [Types.OutputDestination])
dcrrsDestinations = Lens.field @"destinations"
{-# DEPRECATED dcrrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The endpoints where outgoing connections initiate from
--
-- /Note:/ Consider using 'egressEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsEgressEndpoints :: Lens.Lens' DescribeChannelResponse (Core.Maybe [Types.ChannelEgressEndpoint])
dcrrsEgressEndpoints = Lens.field @"egressEndpoints"
{-# DEPRECATED dcrrsEgressEndpoints "Use generic-lens or generic-optics with 'egressEndpoints' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encoderSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsEncoderSettings :: Lens.Lens' DescribeChannelResponse (Core.Maybe Types.EncoderSettings)
dcrrsEncoderSettings = Lens.field @"encoderSettings"
{-# DEPRECATED dcrrsEncoderSettings "Use generic-lens or generic-optics with 'encoderSettings' instead." #-}

-- | The unique id of the channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsId :: Lens.Lens' DescribeChannelResponse (Core.Maybe Core.Text)
dcrrsId = Lens.field @"id"
{-# DEPRECATED dcrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | List of input attachments for channel.
--
-- /Note:/ Consider using 'inputAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsInputAttachments :: Lens.Lens' DescribeChannelResponse (Core.Maybe [Types.InputAttachment])
dcrrsInputAttachments = Lens.field @"inputAttachments"
{-# DEPRECATED dcrrsInputAttachments "Use generic-lens or generic-optics with 'inputAttachments' instead." #-}

-- | Specification of network and file inputs for this channel
--
-- /Note:/ Consider using 'inputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsInputSpecification :: Lens.Lens' DescribeChannelResponse (Core.Maybe Types.InputSpecification)
dcrrsInputSpecification = Lens.field @"inputSpecification"
{-# DEPRECATED dcrrsInputSpecification "Use generic-lens or generic-optics with 'inputSpecification' instead." #-}

-- | The log level being written to CloudWatch Logs.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsLogLevel :: Lens.Lens' DescribeChannelResponse (Core.Maybe Types.LogLevel)
dcrrsLogLevel = Lens.field @"logLevel"
{-# DEPRECATED dcrrsLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | The name of the channel. (user-mutable)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsName :: Lens.Lens' DescribeChannelResponse (Core.Maybe Core.Text)
dcrrsName = Lens.field @"name"
{-# DEPRECATED dcrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Runtime details for the pipelines of a running channel.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsPipelineDetails :: Lens.Lens' DescribeChannelResponse (Core.Maybe [Types.PipelineDetail])
dcrrsPipelineDetails = Lens.field @"pipelineDetails"
{-# DEPRECATED dcrrsPipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsPipelinesRunningCount :: Lens.Lens' DescribeChannelResponse (Core.Maybe Core.Int)
dcrrsPipelinesRunningCount = Lens.field @"pipelinesRunningCount"
{-# DEPRECATED dcrrsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsRoleArn :: Lens.Lens' DescribeChannelResponse (Core.Maybe Core.Text)
dcrrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED dcrrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsState :: Lens.Lens' DescribeChannelResponse (Core.Maybe Types.ChannelState)
dcrrsState = Lens.field @"state"
{-# DEPRECATED dcrrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsTags :: Lens.Lens' DescribeChannelResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
dcrrsTags = Lens.field @"tags"
{-# DEPRECATED dcrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeChannelResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
