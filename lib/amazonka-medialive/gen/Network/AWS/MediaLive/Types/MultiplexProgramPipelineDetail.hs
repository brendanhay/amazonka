{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramPipelineDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramPipelineDetail
  ( MultiplexProgramPipelineDetail (..),

    -- * Smart constructor
    mkMultiplexProgramPipelineDetail,

    -- * Lenses
    mppdPipelineId,
    mppdActiveChannelPipeline,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The current source for one of the pipelines in the multiplex.
--
-- /See:/ 'mkMultiplexProgramPipelineDetail' smart constructor.
data MultiplexProgramPipelineDetail = MultiplexProgramPipelineDetail'
  { -- | Identifies a specific pipeline in the multiplex.
    pipelineId :: Lude.Maybe Lude.Text,
    -- | Identifies the channel pipeline that is currently active for the pipeline (identified by PipelineId) in the multiplex.
    activeChannelPipeline :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiplexProgramPipelineDetail' with the minimum fields required to make a request.
--
-- * 'pipelineId' - Identifies a specific pipeline in the multiplex.
-- * 'activeChannelPipeline' - Identifies the channel pipeline that is currently active for the pipeline (identified by PipelineId) in the multiplex.
mkMultiplexProgramPipelineDetail ::
  MultiplexProgramPipelineDetail
mkMultiplexProgramPipelineDetail =
  MultiplexProgramPipelineDetail'
    { pipelineId = Lude.Nothing,
      activeChannelPipeline = Lude.Nothing
    }

-- | Identifies a specific pipeline in the multiplex.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppdPipelineId :: Lens.Lens' MultiplexProgramPipelineDetail (Lude.Maybe Lude.Text)
mppdPipelineId = Lens.lens (pipelineId :: MultiplexProgramPipelineDetail -> Lude.Maybe Lude.Text) (\s a -> s {pipelineId = a} :: MultiplexProgramPipelineDetail)
{-# DEPRECATED mppdPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | Identifies the channel pipeline that is currently active for the pipeline (identified by PipelineId) in the multiplex.
--
-- /Note:/ Consider using 'activeChannelPipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mppdActiveChannelPipeline :: Lens.Lens' MultiplexProgramPipelineDetail (Lude.Maybe Lude.Text)
mppdActiveChannelPipeline = Lens.lens (activeChannelPipeline :: MultiplexProgramPipelineDetail -> Lude.Maybe Lude.Text) (\s a -> s {activeChannelPipeline = a} :: MultiplexProgramPipelineDetail)
{-# DEPRECATED mppdActiveChannelPipeline "Use generic-lens or generic-optics with 'activeChannelPipeline' instead." #-}

instance Lude.FromJSON MultiplexProgramPipelineDetail where
  parseJSON =
    Lude.withObject
      "MultiplexProgramPipelineDetail"
      ( \x ->
          MultiplexProgramPipelineDetail'
            Lude.<$> (x Lude..:? "pipelineId")
            Lude.<*> (x Lude..:? "activeChannelPipeline")
      )
