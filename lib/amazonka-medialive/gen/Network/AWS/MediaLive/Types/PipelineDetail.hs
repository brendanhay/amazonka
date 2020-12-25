{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PipelineDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PipelineDetail
  ( PipelineDetail (..),

    -- * Smart constructor
    mkPipelineDetail,

    -- * Lenses
    pdActiveInputAttachmentName,
    pdActiveInputSwitchActionName,
    pdPipelineId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Runtime details of a pipeline when a channel is running.
--
-- /See:/ 'mkPipelineDetail' smart constructor.
data PipelineDetail = PipelineDetail'
  { -- | The name of the active input attachment currently being ingested by this pipeline.
    activeInputAttachmentName :: Core.Maybe Core.Text,
    -- | The name of the input switch schedule action that occurred most recently and that resulted in the switch to the current input attachment for this pipeline.
    activeInputSwitchActionName :: Core.Maybe Core.Text,
    -- | Pipeline ID
    pipelineId :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PipelineDetail' value with any optional fields omitted.
mkPipelineDetail ::
  PipelineDetail
mkPipelineDetail =
  PipelineDetail'
    { activeInputAttachmentName = Core.Nothing,
      activeInputSwitchActionName = Core.Nothing,
      pipelineId = Core.Nothing
    }

-- | The name of the active input attachment currently being ingested by this pipeline.
--
-- /Note:/ Consider using 'activeInputAttachmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdActiveInputAttachmentName :: Lens.Lens' PipelineDetail (Core.Maybe Core.Text)
pdActiveInputAttachmentName = Lens.field @"activeInputAttachmentName"
{-# DEPRECATED pdActiveInputAttachmentName "Use generic-lens or generic-optics with 'activeInputAttachmentName' instead." #-}

-- | The name of the input switch schedule action that occurred most recently and that resulted in the switch to the current input attachment for this pipeline.
--
-- /Note:/ Consider using 'activeInputSwitchActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdActiveInputSwitchActionName :: Lens.Lens' PipelineDetail (Core.Maybe Core.Text)
pdActiveInputSwitchActionName = Lens.field @"activeInputSwitchActionName"
{-# DEPRECATED pdActiveInputSwitchActionName "Use generic-lens or generic-optics with 'activeInputSwitchActionName' instead." #-}

-- | Pipeline ID
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPipelineId :: Lens.Lens' PipelineDetail (Core.Maybe Core.Text)
pdPipelineId = Lens.field @"pipelineId"
{-# DEPRECATED pdPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

instance Core.FromJSON PipelineDetail where
  parseJSON =
    Core.withObject "PipelineDetail" Core.$
      \x ->
        PipelineDetail'
          Core.<$> (x Core..:? "activeInputAttachmentName")
          Core.<*> (x Core..:? "activeInputSwitchActionName")
          Core.<*> (x Core..:? "pipelineId")
