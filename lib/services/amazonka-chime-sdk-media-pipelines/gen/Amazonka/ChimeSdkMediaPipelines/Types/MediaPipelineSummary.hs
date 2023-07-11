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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The summary of the media pipeline.
--
-- /See:/ 'newMediaPipelineSummary' smart constructor.
data MediaPipelineSummary = MediaPipelineSummary'
  { -- | The ARN of the media pipeline in the summary.
    mediaPipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the media pipeline in the summary.
    mediaPipelineId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaPipelineSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaPipelineArn', 'mediaPipelineSummary_mediaPipelineArn' - The ARN of the media pipeline in the summary.
--
-- 'mediaPipelineId', 'mediaPipelineSummary_mediaPipelineId' - The ID of the media pipeline in the summary.
newMediaPipelineSummary ::
  MediaPipelineSummary
newMediaPipelineSummary =
  MediaPipelineSummary'
    { mediaPipelineArn =
        Prelude.Nothing,
      mediaPipelineId = Prelude.Nothing
    }

-- | The ARN of the media pipeline in the summary.
mediaPipelineSummary_mediaPipelineArn :: Lens.Lens' MediaPipelineSummary (Prelude.Maybe Prelude.Text)
mediaPipelineSummary_mediaPipelineArn = Lens.lens (\MediaPipelineSummary' {mediaPipelineArn} -> mediaPipelineArn) (\s@MediaPipelineSummary' {} a -> s {mediaPipelineArn = a} :: MediaPipelineSummary)

-- | The ID of the media pipeline in the summary.
mediaPipelineSummary_mediaPipelineId :: Lens.Lens' MediaPipelineSummary (Prelude.Maybe Prelude.Text)
mediaPipelineSummary_mediaPipelineId = Lens.lens (\MediaPipelineSummary' {mediaPipelineId} -> mediaPipelineId) (\s@MediaPipelineSummary' {} a -> s {mediaPipelineId = a} :: MediaPipelineSummary)

instance Data.FromJSON MediaPipelineSummary where
  parseJSON =
    Data.withObject
      "MediaPipelineSummary"
      ( \x ->
          MediaPipelineSummary'
            Prelude.<$> (x Data..:? "MediaPipelineArn")
            Prelude.<*> (x Data..:? "MediaPipelineId")
      )

instance Prelude.Hashable MediaPipelineSummary where
  hashWithSalt _salt MediaPipelineSummary' {..} =
    _salt
      `Prelude.hashWithSalt` mediaPipelineArn
      `Prelude.hashWithSalt` mediaPipelineId

instance Prelude.NFData MediaPipelineSummary where
  rnf MediaPipelineSummary' {..} =
    Prelude.rnf mediaPipelineArn
      `Prelude.seq` Prelude.rnf mediaPipelineId
