{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.PipelinePauseStateSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PipelinePauseStateSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.PipelineId
import qualified Network.AWS.Prelude as Prelude

-- | Settings for pausing a pipeline.
--
-- /See:/ 'newPipelinePauseStateSettings' smart constructor.
data PipelinePauseStateSettings = PipelinePauseStateSettings'
  { -- | Pipeline ID to pause (\"PIPELINE_0\" or \"PIPELINE_1\").
    pipelineId :: PipelineId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON PipelinePauseStateSettings where
  parseJSON =
    Prelude.withObject
      "PipelinePauseStateSettings"
      ( \x ->
          PipelinePauseStateSettings'
            Prelude.<$> (x Prelude..: "pipelineId")
      )

instance Prelude.Hashable PipelinePauseStateSettings

instance Prelude.NFData PipelinePauseStateSettings

instance Prelude.ToJSON PipelinePauseStateSettings where
  toJSON PipelinePauseStateSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("pipelineId" Prelude..= pipelineId)]
      )
