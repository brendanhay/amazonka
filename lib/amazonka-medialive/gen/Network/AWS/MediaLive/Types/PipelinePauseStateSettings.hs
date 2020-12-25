{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PipelinePauseStateSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PipelinePauseStateSettings
  ( PipelinePauseStateSettings (..),

    -- * Smart constructor
    mkPipelinePauseStateSettings,

    -- * Lenses
    ppssPipelineId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.PipelineId as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for pausing a pipeline.
--
-- /See:/ 'mkPipelinePauseStateSettings' smart constructor.
newtype PipelinePauseStateSettings = PipelinePauseStateSettings'
  { -- | Pipeline ID to pause ("PIPELINE_0" or "PIPELINE_1").
    pipelineId :: Types.PipelineId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PipelinePauseStateSettings' value with any optional fields omitted.
mkPipelinePauseStateSettings ::
  -- | 'pipelineId'
  Types.PipelineId ->
  PipelinePauseStateSettings
mkPipelinePauseStateSettings pipelineId =
  PipelinePauseStateSettings' {pipelineId}

-- | Pipeline ID to pause ("PIPELINE_0" or "PIPELINE_1").
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppssPipelineId :: Lens.Lens' PipelinePauseStateSettings Types.PipelineId
ppssPipelineId = Lens.field @"pipelineId"
{-# DEPRECATED ppssPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

instance Core.FromJSON PipelinePauseStateSettings where
  toJSON PipelinePauseStateSettings {..} =
    Core.object
      (Core.catMaybes [Core.Just ("pipelineId" Core..= pipelineId)])

instance Core.FromJSON PipelinePauseStateSettings where
  parseJSON =
    Core.withObject "PipelinePauseStateSettings" Core.$
      \x ->
        PipelinePauseStateSettings' Core.<$> (x Core..: "pipelineId")
