{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ChannelSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ChannelSummary
  ( ChannelSummary (..)
  -- * Smart constructor
  , mkChannelSummary
  -- * Lenses
  , csfArn
  , csfCdiInputSpecification
  , csfChannelClass
  , csfDestinations
  , csfEgressEndpoints
  , csfId
  , csfInputAttachments
  , csfInputSpecification
  , csfLogLevel
  , csfName
  , csfPipelinesRunningCount
  , csfRoleArn
  , csfState
  , csfTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.CdiInputSpecification as Types
import qualified Network.AWS.MediaLive.Types.ChannelClass as Types
import qualified Network.AWS.MediaLive.Types.ChannelEgressEndpoint as Types
import qualified Network.AWS.MediaLive.Types.ChannelState as Types
import qualified Network.AWS.MediaLive.Types.InputAttachment as Types
import qualified Network.AWS.MediaLive.Types.InputSpecification as Types
import qualified Network.AWS.MediaLive.Types.LogLevel as Types
import qualified Network.AWS.MediaLive.Types.OutputDestination as Types
import qualified Network.AWS.Prelude as Core

-- | Placeholder documentation for ChannelSummary
--
-- /See:/ 'mkChannelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { arn :: Core.Maybe Core.Text
    -- ^ The unique arn of the channel.
  , cdiInputSpecification :: Core.Maybe Types.CdiInputSpecification
    -- ^ Specification of CDI inputs for this channel
  , channelClass :: Core.Maybe Types.ChannelClass
    -- ^ The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
  , destinations :: Core.Maybe [Types.OutputDestination]
    -- ^ A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
  , egressEndpoints :: Core.Maybe [Types.ChannelEgressEndpoint]
    -- ^ The endpoints where outgoing connections initiate from
  , id :: Core.Maybe Core.Text
    -- ^ The unique id of the channel.
  , inputAttachments :: Core.Maybe [Types.InputAttachment]
    -- ^ List of input attachments for channel.
  , inputSpecification :: Core.Maybe Types.InputSpecification
    -- ^ Specification of network and file inputs for this channel
  , logLevel :: Core.Maybe Types.LogLevel
    -- ^ The log level being written to CloudWatch Logs.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the channel. (user-mutable)
  , pipelinesRunningCount :: Core.Maybe Core.Int
    -- ^ The number of currently healthy pipelines.
  , roleArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the role assumed when running the Channel.
  , state :: Core.Maybe Types.ChannelState
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A collection of key-value pairs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChannelSummary' value with any optional fields omitted.
mkChannelSummary
    :: ChannelSummary
mkChannelSummary
  = ChannelSummary'{arn = Core.Nothing,
                    cdiInputSpecification = Core.Nothing, channelClass = Core.Nothing,
                    destinations = Core.Nothing, egressEndpoints = Core.Nothing,
                    id = Core.Nothing, inputAttachments = Core.Nothing,
                    inputSpecification = Core.Nothing, logLevel = Core.Nothing,
                    name = Core.Nothing, pipelinesRunningCount = Core.Nothing,
                    roleArn = Core.Nothing, state = Core.Nothing, tags = Core.Nothing}

-- | The unique arn of the channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfArn :: Lens.Lens' ChannelSummary (Core.Maybe Core.Text)
csfArn = Lens.field @"arn"
{-# INLINEABLE csfArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Specification of CDI inputs for this channel
--
-- /Note:/ Consider using 'cdiInputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfCdiInputSpecification :: Lens.Lens' ChannelSummary (Core.Maybe Types.CdiInputSpecification)
csfCdiInputSpecification = Lens.field @"cdiInputSpecification"
{-# INLINEABLE csfCdiInputSpecification #-}
{-# DEPRECATED cdiInputSpecification "Use generic-lens or generic-optics with 'cdiInputSpecification' instead"  #-}

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfChannelClass :: Lens.Lens' ChannelSummary (Core.Maybe Types.ChannelClass)
csfChannelClass = Lens.field @"channelClass"
{-# INLINEABLE csfChannelClass #-}
{-# DEPRECATED channelClass "Use generic-lens or generic-optics with 'channelClass' instead"  #-}

-- | A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDestinations :: Lens.Lens' ChannelSummary (Core.Maybe [Types.OutputDestination])
csfDestinations = Lens.field @"destinations"
{-# INLINEABLE csfDestinations #-}
{-# DEPRECATED destinations "Use generic-lens or generic-optics with 'destinations' instead"  #-}

-- | The endpoints where outgoing connections initiate from
--
-- /Note:/ Consider using 'egressEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfEgressEndpoints :: Lens.Lens' ChannelSummary (Core.Maybe [Types.ChannelEgressEndpoint])
csfEgressEndpoints = Lens.field @"egressEndpoints"
{-# INLINEABLE csfEgressEndpoints #-}
{-# DEPRECATED egressEndpoints "Use generic-lens or generic-optics with 'egressEndpoints' instead"  #-}

-- | The unique id of the channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfId :: Lens.Lens' ChannelSummary (Core.Maybe Core.Text)
csfId = Lens.field @"id"
{-# INLINEABLE csfId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | List of input attachments for channel.
--
-- /Note:/ Consider using 'inputAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfInputAttachments :: Lens.Lens' ChannelSummary (Core.Maybe [Types.InputAttachment])
csfInputAttachments = Lens.field @"inputAttachments"
{-# INLINEABLE csfInputAttachments #-}
{-# DEPRECATED inputAttachments "Use generic-lens or generic-optics with 'inputAttachments' instead"  #-}

-- | Specification of network and file inputs for this channel
--
-- /Note:/ Consider using 'inputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfInputSpecification :: Lens.Lens' ChannelSummary (Core.Maybe Types.InputSpecification)
csfInputSpecification = Lens.field @"inputSpecification"
{-# INLINEABLE csfInputSpecification #-}
{-# DEPRECATED inputSpecification "Use generic-lens or generic-optics with 'inputSpecification' instead"  #-}

-- | The log level being written to CloudWatch Logs.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfLogLevel :: Lens.Lens' ChannelSummary (Core.Maybe Types.LogLevel)
csfLogLevel = Lens.field @"logLevel"
{-# INLINEABLE csfLogLevel #-}
{-# DEPRECATED logLevel "Use generic-lens or generic-optics with 'logLevel' instead"  #-}

-- | The name of the channel. (user-mutable)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfName :: Lens.Lens' ChannelSummary (Core.Maybe Core.Text)
csfName = Lens.field @"name"
{-# INLINEABLE csfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfPipelinesRunningCount :: Lens.Lens' ChannelSummary (Core.Maybe Core.Int)
csfPipelinesRunningCount = Lens.field @"pipelinesRunningCount"
{-# INLINEABLE csfPipelinesRunningCount #-}
{-# DEPRECATED pipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead"  #-}

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfRoleArn :: Lens.Lens' ChannelSummary (Core.Maybe Core.Text)
csfRoleArn = Lens.field @"roleArn"
{-# INLINEABLE csfRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfState :: Lens.Lens' ChannelSummary (Core.Maybe Types.ChannelState)
csfState = Lens.field @"state"
{-# INLINEABLE csfState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfTags :: Lens.Lens' ChannelSummary (Core.Maybe (Core.HashMap Core.Text Core.Text))
csfTags = Lens.field @"tags"
{-# INLINEABLE csfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON ChannelSummary where
        parseJSON
          = Core.withObject "ChannelSummary" Core.$
              \ x ->
                ChannelSummary' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "cdiInputSpecification"
                    Core.<*> x Core..:? "channelClass"
                    Core.<*> x Core..:? "destinations"
                    Core.<*> x Core..:? "egressEndpoints"
                    Core.<*> x Core..:? "id"
                    Core.<*> x Core..:? "inputAttachments"
                    Core.<*> x Core..:? "inputSpecification"
                    Core.<*> x Core..:? "logLevel"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "pipelinesRunningCount"
                    Core.<*> x Core..:? "roleArn"
                    Core.<*> x Core..:? "state"
                    Core.<*> x Core..:? "tags"
