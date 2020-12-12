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
import Network.AWS.MediaLive.Types.PipelineId
import qualified Network.AWS.Prelude as Lude

-- | Settings for pausing a pipeline.
--
-- /See:/ 'mkPipelinePauseStateSettings' smart constructor.
newtype PipelinePauseStateSettings = PipelinePauseStateSettings'
  { pipelineId ::
      PipelineId
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PipelinePauseStateSettings' with the minimum fields required to make a request.
--
-- * 'pipelineId' - Pipeline ID to pause ("PIPELINE_0" or "PIPELINE_1").
mkPipelinePauseStateSettings ::
  -- | 'pipelineId'
  PipelineId ->
  PipelinePauseStateSettings
mkPipelinePauseStateSettings pPipelineId_ =
  PipelinePauseStateSettings' {pipelineId = pPipelineId_}

-- | Pipeline ID to pause ("PIPELINE_0" or "PIPELINE_1").
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppssPipelineId :: Lens.Lens' PipelinePauseStateSettings PipelineId
ppssPipelineId = Lens.lens (pipelineId :: PipelinePauseStateSettings -> PipelineId) (\s a -> s {pipelineId = a} :: PipelinePauseStateSettings)
{-# DEPRECATED ppssPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

instance Lude.FromJSON PipelinePauseStateSettings where
  parseJSON =
    Lude.withObject
      "PipelinePauseStateSettings"
      ( \x ->
          PipelinePauseStateSettings' Lude.<$> (x Lude..: "pipelineId")
      )

instance Lude.ToJSON PipelinePauseStateSettings where
  toJSON PipelinePauseStateSettings' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("pipelineId" Lude..= pipelineId)])
