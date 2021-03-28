{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MultiplexProgram
  ( MultiplexProgram (..)
  -- * Smart constructor
  , mkMultiplexProgram
  -- * Lenses
  , mpChannelId
  , mpMultiplexProgramSettings
  , mpPacketIdentifiersMap
  , mpPipelineDetails
  , mpProgramName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.MultiplexProgramPacketIdentifiersMap as Types
import qualified Network.AWS.MediaLive.Types.MultiplexProgramPipelineDetail as Types
import qualified Network.AWS.MediaLive.Types.MultiplexProgramSettings as Types
import qualified Network.AWS.Prelude as Core

-- | The multiplex program object.
--
-- /See:/ 'mkMultiplexProgram' smart constructor.
data MultiplexProgram = MultiplexProgram'
  { channelId :: Core.Maybe Core.Text
    -- ^ The MediaLive channel associated with the program.
  , multiplexProgramSettings :: Core.Maybe Types.MultiplexProgramSettings
    -- ^ The settings for this multiplex program.
  , packetIdentifiersMap :: Core.Maybe Types.MultiplexProgramPacketIdentifiersMap
    -- ^ The packet identifier map for this multiplex program.
  , pipelineDetails :: Core.Maybe [Types.MultiplexProgramPipelineDetail]
    -- ^ Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
  , programName :: Core.Maybe Core.Text
    -- ^ The name of the multiplex program.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MultiplexProgram' value with any optional fields omitted.
mkMultiplexProgram
    :: MultiplexProgram
mkMultiplexProgram
  = MultiplexProgram'{channelId = Core.Nothing,
                      multiplexProgramSettings = Core.Nothing,
                      packetIdentifiersMap = Core.Nothing,
                      pipelineDetails = Core.Nothing, programName = Core.Nothing}

-- | The MediaLive channel associated with the program.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpChannelId :: Lens.Lens' MultiplexProgram (Core.Maybe Core.Text)
mpChannelId = Lens.field @"channelId"
{-# INLINEABLE mpChannelId #-}
{-# DEPRECATED channelId "Use generic-lens or generic-optics with 'channelId' instead"  #-}

-- | The settings for this multiplex program.
--
-- /Note:/ Consider using 'multiplexProgramSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpMultiplexProgramSettings :: Lens.Lens' MultiplexProgram (Core.Maybe Types.MultiplexProgramSettings)
mpMultiplexProgramSettings = Lens.field @"multiplexProgramSettings"
{-# INLINEABLE mpMultiplexProgramSettings #-}
{-# DEPRECATED multiplexProgramSettings "Use generic-lens or generic-optics with 'multiplexProgramSettings' instead"  #-}

-- | The packet identifier map for this multiplex program.
--
-- /Note:/ Consider using 'packetIdentifiersMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpPacketIdentifiersMap :: Lens.Lens' MultiplexProgram (Core.Maybe Types.MultiplexProgramPacketIdentifiersMap)
mpPacketIdentifiersMap = Lens.field @"packetIdentifiersMap"
{-# INLINEABLE mpPacketIdentifiersMap #-}
{-# DEPRECATED packetIdentifiersMap "Use generic-lens or generic-optics with 'packetIdentifiersMap' instead"  #-}

-- | Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpPipelineDetails :: Lens.Lens' MultiplexProgram (Core.Maybe [Types.MultiplexProgramPipelineDetail])
mpPipelineDetails = Lens.field @"pipelineDetails"
{-# INLINEABLE mpPipelineDetails #-}
{-# DEPRECATED pipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead"  #-}

-- | The name of the multiplex program.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpProgramName :: Lens.Lens' MultiplexProgram (Core.Maybe Core.Text)
mpProgramName = Lens.field @"programName"
{-# INLINEABLE mpProgramName #-}
{-# DEPRECATED programName "Use generic-lens or generic-optics with 'programName' instead"  #-}

instance Core.FromJSON MultiplexProgram where
        parseJSON
          = Core.withObject "MultiplexProgram" Core.$
              \ x ->
                MultiplexProgram' Core.<$>
                  (x Core..:? "channelId") Core.<*>
                    x Core..:? "multiplexProgramSettings"
                    Core.<*> x Core..:? "packetIdentifiersMap"
                    Core.<*> x Core..:? "pipelineDetails"
                    Core.<*> x Core..:? "programName"
