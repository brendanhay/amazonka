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
-- Module      : Amazonka.MediaLive.Types.PipelinePauseStateSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.PipelinePauseStateSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.PipelineId
import qualified Amazonka.Prelude as Prelude

-- | Settings for pausing a pipeline.
--
-- /See:/ 'newPipelinePauseStateSettings' smart constructor.
data PipelinePauseStateSettings = PipelinePauseStateSettings'
  { -- | Pipeline ID to pause (\"PIPELINE_0\" or \"PIPELINE_1\").
    pipelineId :: PipelineId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelinePauseStateSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineId', 'pipelinePauseStateSettings_pipelineId' - Pipeline ID to pause (\"PIPELINE_0\" or \"PIPELINE_1\").
newPipelinePauseStateSettings ::
  -- | 'pipelineId'
  PipelineId ->
  PipelinePauseStateSettings
newPipelinePauseStateSettings pPipelineId_ =
  PipelinePauseStateSettings'
    { pipelineId =
        pPipelineId_
    }

-- | Pipeline ID to pause (\"PIPELINE_0\" or \"PIPELINE_1\").
pipelinePauseStateSettings_pipelineId :: Lens.Lens' PipelinePauseStateSettings PipelineId
pipelinePauseStateSettings_pipelineId = Lens.lens (\PipelinePauseStateSettings' {pipelineId} -> pipelineId) (\s@PipelinePauseStateSettings' {} a -> s {pipelineId = a} :: PipelinePauseStateSettings)

instance Data.FromJSON PipelinePauseStateSettings where
  parseJSON =
    Data.withObject
      "PipelinePauseStateSettings"
      ( \x ->
          PipelinePauseStateSettings'
            Prelude.<$> (x Data..: "pipelineId")
      )

instance Prelude.Hashable PipelinePauseStateSettings where
  hashWithSalt _salt PipelinePauseStateSettings' {..} =
    _salt `Prelude.hashWithSalt` pipelineId

instance Prelude.NFData PipelinePauseStateSettings where
  rnf PipelinePauseStateSettings' {..} =
    Prelude.rnf pipelineId

instance Data.ToJSON PipelinePauseStateSettings where
  toJSON PipelinePauseStateSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("pipelineId" Data..= pipelineId)]
      )
