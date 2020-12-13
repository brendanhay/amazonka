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
    pdPipelineId,
    pdActiveInputSwitchActionName,
    pdActiveInputAttachmentName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Runtime details of a pipeline when a channel is running.
--
-- /See:/ 'mkPipelineDetail' smart constructor.
data PipelineDetail = PipelineDetail'
  { -- | Pipeline ID
    pipelineId :: Lude.Maybe Lude.Text,
    -- | The name of the input switch schedule action that occurred most recently and that resulted in the switch to the current input attachment for this pipeline.
    activeInputSwitchActionName :: Lude.Maybe Lude.Text,
    -- | The name of the active input attachment currently being ingested by this pipeline.
    activeInputAttachmentName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PipelineDetail' with the minimum fields required to make a request.
--
-- * 'pipelineId' - Pipeline ID
-- * 'activeInputSwitchActionName' - The name of the input switch schedule action that occurred most recently and that resulted in the switch to the current input attachment for this pipeline.
-- * 'activeInputAttachmentName' - The name of the active input attachment currently being ingested by this pipeline.
mkPipelineDetail ::
  PipelineDetail
mkPipelineDetail =
  PipelineDetail'
    { pipelineId = Lude.Nothing,
      activeInputSwitchActionName = Lude.Nothing,
      activeInputAttachmentName = Lude.Nothing
    }

-- | Pipeline ID
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPipelineId :: Lens.Lens' PipelineDetail (Lude.Maybe Lude.Text)
pdPipelineId = Lens.lens (pipelineId :: PipelineDetail -> Lude.Maybe Lude.Text) (\s a -> s {pipelineId = a} :: PipelineDetail)
{-# DEPRECATED pdPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The name of the input switch schedule action that occurred most recently and that resulted in the switch to the current input attachment for this pipeline.
--
-- /Note:/ Consider using 'activeInputSwitchActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdActiveInputSwitchActionName :: Lens.Lens' PipelineDetail (Lude.Maybe Lude.Text)
pdActiveInputSwitchActionName = Lens.lens (activeInputSwitchActionName :: PipelineDetail -> Lude.Maybe Lude.Text) (\s a -> s {activeInputSwitchActionName = a} :: PipelineDetail)
{-# DEPRECATED pdActiveInputSwitchActionName "Use generic-lens or generic-optics with 'activeInputSwitchActionName' instead." #-}

-- | The name of the active input attachment currently being ingested by this pipeline.
--
-- /Note:/ Consider using 'activeInputAttachmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdActiveInputAttachmentName :: Lens.Lens' PipelineDetail (Lude.Maybe Lude.Text)
pdActiveInputAttachmentName = Lens.lens (activeInputAttachmentName :: PipelineDetail -> Lude.Maybe Lude.Text) (\s a -> s {activeInputAttachmentName = a} :: PipelineDetail)
{-# DEPRECATED pdActiveInputAttachmentName "Use generic-lens or generic-optics with 'activeInputAttachmentName' instead." #-}

instance Lude.FromJSON PipelineDetail where
  parseJSON =
    Lude.withObject
      "PipelineDetail"
      ( \x ->
          PipelineDetail'
            Lude.<$> (x Lude..:? "pipelineId")
            Lude.<*> (x Lude..:? "activeInputSwitchActionName")
            Lude.<*> (x Lude..:? "activeInputAttachmentName")
      )
