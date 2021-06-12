{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramPipelineDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramPipelineDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The current source for one of the pipelines in the multiplex.
--
-- /See:/ 'newMultiplexProgramPipelineDetail' smart constructor.
data MultiplexProgramPipelineDetail = MultiplexProgramPipelineDetail'
  { -- | Identifies a specific pipeline in the multiplex.
    pipelineId :: Core.Maybe Core.Text,
    -- | Identifies the channel pipeline that is currently active for the
    -- pipeline (identified by PipelineId) in the multiplex.
    activeChannelPipeline :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MultiplexProgramPipelineDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineId', 'multiplexProgramPipelineDetail_pipelineId' - Identifies a specific pipeline in the multiplex.
--
-- 'activeChannelPipeline', 'multiplexProgramPipelineDetail_activeChannelPipeline' - Identifies the channel pipeline that is currently active for the
-- pipeline (identified by PipelineId) in the multiplex.
newMultiplexProgramPipelineDetail ::
  MultiplexProgramPipelineDetail
newMultiplexProgramPipelineDetail =
  MultiplexProgramPipelineDetail'
    { pipelineId =
        Core.Nothing,
      activeChannelPipeline = Core.Nothing
    }

-- | Identifies a specific pipeline in the multiplex.
multiplexProgramPipelineDetail_pipelineId :: Lens.Lens' MultiplexProgramPipelineDetail (Core.Maybe Core.Text)
multiplexProgramPipelineDetail_pipelineId = Lens.lens (\MultiplexProgramPipelineDetail' {pipelineId} -> pipelineId) (\s@MultiplexProgramPipelineDetail' {} a -> s {pipelineId = a} :: MultiplexProgramPipelineDetail)

-- | Identifies the channel pipeline that is currently active for the
-- pipeline (identified by PipelineId) in the multiplex.
multiplexProgramPipelineDetail_activeChannelPipeline :: Lens.Lens' MultiplexProgramPipelineDetail (Core.Maybe Core.Text)
multiplexProgramPipelineDetail_activeChannelPipeline = Lens.lens (\MultiplexProgramPipelineDetail' {activeChannelPipeline} -> activeChannelPipeline) (\s@MultiplexProgramPipelineDetail' {} a -> s {activeChannelPipeline = a} :: MultiplexProgramPipelineDetail)

instance Core.FromJSON MultiplexProgramPipelineDetail where
  parseJSON =
    Core.withObject
      "MultiplexProgramPipelineDetail"
      ( \x ->
          MultiplexProgramPipelineDetail'
            Core.<$> (x Core..:? "pipelineId")
            Core.<*> (x Core..:? "activeChannelPipeline")
      )

instance Core.Hashable MultiplexProgramPipelineDetail

instance Core.NFData MultiplexProgramPipelineDetail
