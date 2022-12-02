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
-- Module      : Amazonka.MediaLive.Types.MultiplexProgramPipelineDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MultiplexProgramPipelineDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The current source for one of the pipelines in the multiplex.
--
-- /See:/ 'newMultiplexProgramPipelineDetail' smart constructor.
data MultiplexProgramPipelineDetail = MultiplexProgramPipelineDetail'
  { -- | Identifies the channel pipeline that is currently active for the
    -- pipeline (identified by PipelineId) in the multiplex.
    activeChannelPipeline :: Prelude.Maybe Prelude.Text,
    -- | Identifies a specific pipeline in the multiplex.
    pipelineId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiplexProgramPipelineDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeChannelPipeline', 'multiplexProgramPipelineDetail_activeChannelPipeline' - Identifies the channel pipeline that is currently active for the
-- pipeline (identified by PipelineId) in the multiplex.
--
-- 'pipelineId', 'multiplexProgramPipelineDetail_pipelineId' - Identifies a specific pipeline in the multiplex.
newMultiplexProgramPipelineDetail ::
  MultiplexProgramPipelineDetail
newMultiplexProgramPipelineDetail =
  MultiplexProgramPipelineDetail'
    { activeChannelPipeline =
        Prelude.Nothing,
      pipelineId = Prelude.Nothing
    }

-- | Identifies the channel pipeline that is currently active for the
-- pipeline (identified by PipelineId) in the multiplex.
multiplexProgramPipelineDetail_activeChannelPipeline :: Lens.Lens' MultiplexProgramPipelineDetail (Prelude.Maybe Prelude.Text)
multiplexProgramPipelineDetail_activeChannelPipeline = Lens.lens (\MultiplexProgramPipelineDetail' {activeChannelPipeline} -> activeChannelPipeline) (\s@MultiplexProgramPipelineDetail' {} a -> s {activeChannelPipeline = a} :: MultiplexProgramPipelineDetail)

-- | Identifies a specific pipeline in the multiplex.
multiplexProgramPipelineDetail_pipelineId :: Lens.Lens' MultiplexProgramPipelineDetail (Prelude.Maybe Prelude.Text)
multiplexProgramPipelineDetail_pipelineId = Lens.lens (\MultiplexProgramPipelineDetail' {pipelineId} -> pipelineId) (\s@MultiplexProgramPipelineDetail' {} a -> s {pipelineId = a} :: MultiplexProgramPipelineDetail)

instance Data.FromJSON MultiplexProgramPipelineDetail where
  parseJSON =
    Data.withObject
      "MultiplexProgramPipelineDetail"
      ( \x ->
          MultiplexProgramPipelineDetail'
            Prelude.<$> (x Data..:? "activeChannelPipeline")
            Prelude.<*> (x Data..:? "pipelineId")
      )

instance
  Prelude.Hashable
    MultiplexProgramPipelineDetail
  where
  hashWithSalt
    _salt
    MultiplexProgramPipelineDetail' {..} =
      _salt `Prelude.hashWithSalt` activeChannelPipeline
        `Prelude.hashWithSalt` pipelineId

instance
  Prelude.NFData
    MultiplexProgramPipelineDetail
  where
  rnf MultiplexProgramPipelineDetail' {..} =
    Prelude.rnf activeChannelPipeline
      `Prelude.seq` Prelude.rnf pipelineId
