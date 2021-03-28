{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramPipelineDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MultiplexProgramPipelineDetail
  ( MultiplexProgramPipelineDetail (..)
  -- * Smart constructor
  , mkMultiplexProgramPipelineDetail
  -- * Lenses
  , mppdActiveChannelPipeline
  , mppdPipelineId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The current source for one of the pipelines in the multiplex.
--
-- /See:/ 'mkMultiplexProgramPipelineDetail' smart constructor.
data MultiplexProgramPipelineDetail = MultiplexProgramPipelineDetail'
  { activeChannelPipeline :: Core.Maybe Core.Text
    -- ^ Identifies the channel pipeline that is currently active for the pipeline (identified by PipelineId) in the multiplex.
  , pipelineId :: Core.Maybe Core.Text
    -- ^ Identifies a specific pipeline in the multiplex.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MultiplexProgramPipelineDetail' value with any optional fields omitted.
mkMultiplexProgramPipelineDetail
    :: MultiplexProgramPipelineDetail
mkMultiplexProgramPipelineDetail
  = MultiplexProgramPipelineDetail'{activeChannelPipeline =
                                      Core.Nothing,
                                    pipelineId = Core.Nothing}

-- | Identifies the channel pipeline that is currently active for the pipeline (identified by PipelineId) in the multiplex.
--
-- /Note:/ Consider using 'activeChannelPipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppdActiveChannelPipeline :: Lens.Lens' MultiplexProgramPipelineDetail (Core.Maybe Core.Text)
mppdActiveChannelPipeline = Lens.field @"activeChannelPipeline"
{-# INLINEABLE mppdActiveChannelPipeline #-}
{-# DEPRECATED activeChannelPipeline "Use generic-lens or generic-optics with 'activeChannelPipeline' instead"  #-}

-- | Identifies a specific pipeline in the multiplex.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppdPipelineId :: Lens.Lens' MultiplexProgramPipelineDetail (Core.Maybe Core.Text)
mppdPipelineId = Lens.field @"pipelineId"
{-# INLINEABLE mppdPipelineId #-}
{-# DEPRECATED pipelineId "Use generic-lens or generic-optics with 'pipelineId' instead"  #-}

instance Core.FromJSON MultiplexProgramPipelineDetail where
        parseJSON
          = Core.withObject "MultiplexProgramPipelineDetail" Core.$
              \ x ->
                MultiplexProgramPipelineDetail' Core.<$>
                  (x Core..:? "activeChannelPipeline") Core.<*>
                    x Core..:? "pipelineId"
