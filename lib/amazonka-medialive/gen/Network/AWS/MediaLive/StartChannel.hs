{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    scrrsArn,
    scrrsCdiInputSpecification,
    scrrsChannelClass,
    scrrsDestinations,
    scrrsEgressEndpoints,
    scrrsEncoderSettings,
    scrrsId,
    scrrsInputAttachments,
    scrrsInputSpecification,
    scrrsLogLevel,
    scrrsName,
    scrrsPipelineDetails,
    scrrsPipelinesRunningCount,
    scrrsRoleArn,
    scrrsState,
    scrrsTags,
    scrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for StartChannelRequest
--
-- /See:/ 'mkStartChannel' smart constructor.
newtype StartChannel = StartChannel'
  { -- | A request to start a channel
    channelId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartChannel' value with any optional fields omitted.
mkStartChannel ::
  -- | 'channelId'
  Core.Text ->
  StartChannel
mkStartChannel channelId = StartChannel' {channelId}

-- | A request to start a channel
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scChannelId :: Lens.Lens' StartChannel Core.Text
scChannelId = Lens.field @"channelId"
{-# DEPRECATED scChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

instance Core.FromJSON StartChannel where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest StartChannel where
  type Rs StartChannel = StartChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/prod/channels/" Core.<> (Core.toText channelId)
                Core.<> ("/start")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartChannelResponse'
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

-- | Placeholder documentation for StartChannelResponse
--
-- /See:/ 'mkStartChannelResponse' smart constructor.
data StartChannelResponse = StartChannelResponse'
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

-- | Creates a 'StartChannelResponse' value with any optional fields omitted.
mkStartChannelResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartChannelResponse
mkStartChannelResponse responseStatus =
  StartChannelResponse'
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
scrrsArn :: Lens.Lens' StartChannelResponse (Core.Maybe Core.Text)
scrrsArn = Lens.field @"arn"
{-# DEPRECATED scrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Specification of CDI inputs for this channel
--
-- /Note:/ Consider using 'cdiInputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsCdiInputSpecification :: Lens.Lens' StartChannelResponse (Core.Maybe Types.CdiInputSpecification)
scrrsCdiInputSpecification = Lens.field @"cdiInputSpecification"
{-# DEPRECATED scrrsCdiInputSpecification "Use generic-lens or generic-optics with 'cdiInputSpecification' instead." #-}

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsChannelClass :: Lens.Lens' StartChannelResponse (Core.Maybe Types.ChannelClass)
scrrsChannelClass = Lens.field @"channelClass"
{-# DEPRECATED scrrsChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsDestinations :: Lens.Lens' StartChannelResponse (Core.Maybe [Types.OutputDestination])
scrrsDestinations = Lens.field @"destinations"
{-# DEPRECATED scrrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The endpoints where outgoing connections initiate from
--
-- /Note:/ Consider using 'egressEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsEgressEndpoints :: Lens.Lens' StartChannelResponse (Core.Maybe [Types.ChannelEgressEndpoint])
scrrsEgressEndpoints = Lens.field @"egressEndpoints"
{-# DEPRECATED scrrsEgressEndpoints "Use generic-lens or generic-optics with 'egressEndpoints' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encoderSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsEncoderSettings :: Lens.Lens' StartChannelResponse (Core.Maybe Types.EncoderSettings)
scrrsEncoderSettings = Lens.field @"encoderSettings"
{-# DEPRECATED scrrsEncoderSettings "Use generic-lens or generic-optics with 'encoderSettings' instead." #-}

-- | The unique id of the channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsId :: Lens.Lens' StartChannelResponse (Core.Maybe Core.Text)
scrrsId = Lens.field @"id"
{-# DEPRECATED scrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | List of input attachments for channel.
--
-- /Note:/ Consider using 'inputAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsInputAttachments :: Lens.Lens' StartChannelResponse (Core.Maybe [Types.InputAttachment])
scrrsInputAttachments = Lens.field @"inputAttachments"
{-# DEPRECATED scrrsInputAttachments "Use generic-lens or generic-optics with 'inputAttachments' instead." #-}

-- | Specification of network and file inputs for this channel
--
-- /Note:/ Consider using 'inputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsInputSpecification :: Lens.Lens' StartChannelResponse (Core.Maybe Types.InputSpecification)
scrrsInputSpecification = Lens.field @"inputSpecification"
{-# DEPRECATED scrrsInputSpecification "Use generic-lens or generic-optics with 'inputSpecification' instead." #-}

-- | The log level being written to CloudWatch Logs.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsLogLevel :: Lens.Lens' StartChannelResponse (Core.Maybe Types.LogLevel)
scrrsLogLevel = Lens.field @"logLevel"
{-# DEPRECATED scrrsLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | The name of the channel. (user-mutable)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsName :: Lens.Lens' StartChannelResponse (Core.Maybe Core.Text)
scrrsName = Lens.field @"name"
{-# DEPRECATED scrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Runtime details for the pipelines of a running channel.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsPipelineDetails :: Lens.Lens' StartChannelResponse (Core.Maybe [Types.PipelineDetail])
scrrsPipelineDetails = Lens.field @"pipelineDetails"
{-# DEPRECATED scrrsPipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsPipelinesRunningCount :: Lens.Lens' StartChannelResponse (Core.Maybe Core.Int)
scrrsPipelinesRunningCount = Lens.field @"pipelinesRunningCount"
{-# DEPRECATED scrrsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsRoleArn :: Lens.Lens' StartChannelResponse (Core.Maybe Core.Text)
scrrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED scrrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsState :: Lens.Lens' StartChannelResponse (Core.Maybe Types.ChannelState)
scrrsState = Lens.field @"state"
{-# DEPRECATED scrrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsTags :: Lens.Lens' StartChannelResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
scrrsTags = Lens.field @"tags"
{-# DEPRECATED scrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsResponseStatus :: Lens.Lens' StartChannelResponse Core.Int
scrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED scrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
