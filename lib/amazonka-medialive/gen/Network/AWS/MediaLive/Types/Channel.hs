{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Channel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Channel
  ( Channel (..),

    -- * Smart constructor
    mkChannel,

    -- * Lenses
    cArn,
    cCdiInputSpecification,
    cChannelClass,
    cDestinations,
    cEgressEndpoints,
    cEncoderSettings,
    cId,
    cInputAttachments,
    cInputSpecification,
    cLogLevel,
    cName,
    cPipelineDetails,
    cPipelinesRunningCount,
    cRoleArn,
    cState,
    cTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.CdiInputSpecification as Types
import qualified Network.AWS.MediaLive.Types.ChannelClass as Types
import qualified Network.AWS.MediaLive.Types.ChannelEgressEndpoint as Types
import qualified Network.AWS.MediaLive.Types.ChannelState as Types
import qualified Network.AWS.MediaLive.Types.EncoderSettings as Types
import qualified Network.AWS.MediaLive.Types.InputAttachment as Types
import qualified Network.AWS.MediaLive.Types.InputSpecification as Types
import qualified Network.AWS.MediaLive.Types.LogLevel as Types
import qualified Network.AWS.MediaLive.Types.OutputDestination as Types
import qualified Network.AWS.MediaLive.Types.PipelineDetail as Types
import qualified Network.AWS.Prelude as Core

-- | Placeholder documentation for Channel
--
-- /See:/ 'mkChannel' smart constructor.
data Channel = Channel'
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
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Channel' value with any optional fields omitted.
mkChannel ::
  Channel
mkChannel =
  Channel'
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
      tags = Core.Nothing
    }

-- | The unique arn of the channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cArn :: Lens.Lens' Channel (Core.Maybe Core.Text)
cArn = Lens.field @"arn"
{-# DEPRECATED cArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Specification of CDI inputs for this channel
--
-- /Note:/ Consider using 'cdiInputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCdiInputSpecification :: Lens.Lens' Channel (Core.Maybe Types.CdiInputSpecification)
cCdiInputSpecification = Lens.field @"cdiInputSpecification"
{-# DEPRECATED cCdiInputSpecification "Use generic-lens or generic-optics with 'cdiInputSpecification' instead." #-}

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cChannelClass :: Lens.Lens' Channel (Core.Maybe Types.ChannelClass)
cChannelClass = Lens.field @"channelClass"
{-# DEPRECATED cChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDestinations :: Lens.Lens' Channel (Core.Maybe [Types.OutputDestination])
cDestinations = Lens.field @"destinations"
{-# DEPRECATED cDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The endpoints where outgoing connections initiate from
--
-- /Note:/ Consider using 'egressEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEgressEndpoints :: Lens.Lens' Channel (Core.Maybe [Types.ChannelEgressEndpoint])
cEgressEndpoints = Lens.field @"egressEndpoints"
{-# DEPRECATED cEgressEndpoints "Use generic-lens or generic-optics with 'egressEndpoints' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encoderSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEncoderSettings :: Lens.Lens' Channel (Core.Maybe Types.EncoderSettings)
cEncoderSettings = Lens.field @"encoderSettings"
{-# DEPRECATED cEncoderSettings "Use generic-lens or generic-optics with 'encoderSettings' instead." #-}

-- | The unique id of the channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' Channel (Core.Maybe Core.Text)
cId = Lens.field @"id"
{-# DEPRECATED cId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | List of input attachments for channel.
--
-- /Note:/ Consider using 'inputAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInputAttachments :: Lens.Lens' Channel (Core.Maybe [Types.InputAttachment])
cInputAttachments = Lens.field @"inputAttachments"
{-# DEPRECATED cInputAttachments "Use generic-lens or generic-optics with 'inputAttachments' instead." #-}

-- | Specification of network and file inputs for this channel
--
-- /Note:/ Consider using 'inputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInputSpecification :: Lens.Lens' Channel (Core.Maybe Types.InputSpecification)
cInputSpecification = Lens.field @"inputSpecification"
{-# DEPRECATED cInputSpecification "Use generic-lens or generic-optics with 'inputSpecification' instead." #-}

-- | The log level being written to CloudWatch Logs.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLogLevel :: Lens.Lens' Channel (Core.Maybe Types.LogLevel)
cLogLevel = Lens.field @"logLevel"
{-# DEPRECATED cLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | The name of the channel. (user-mutable)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Channel (Core.Maybe Core.Text)
cName = Lens.field @"name"
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Runtime details for the pipelines of a running channel.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPipelineDetails :: Lens.Lens' Channel (Core.Maybe [Types.PipelineDetail])
cPipelineDetails = Lens.field @"pipelineDetails"
{-# DEPRECATED cPipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPipelinesRunningCount :: Lens.Lens' Channel (Core.Maybe Core.Int)
cPipelinesRunningCount = Lens.field @"pipelinesRunningCount"
{-# DEPRECATED cPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRoleArn :: Lens.Lens' Channel (Core.Maybe Core.Text)
cRoleArn = Lens.field @"roleArn"
{-# DEPRECATED cRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cState :: Lens.Lens' Channel (Core.Maybe Types.ChannelState)
cState = Lens.field @"state"
{-# DEPRECATED cState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' Channel (Core.Maybe (Core.HashMap Core.Text Core.Text))
cTags = Lens.field @"tags"
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON Channel where
  parseJSON =
    Core.withObject "Channel" Core.$
      \x ->
        Channel'
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
