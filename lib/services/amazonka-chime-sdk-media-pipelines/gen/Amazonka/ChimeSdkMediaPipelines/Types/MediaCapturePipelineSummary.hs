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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.MediaCapturePipelineSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.MediaCapturePipelineSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The summary data of a media capture pipeline.
--
-- /See:/ 'newMediaCapturePipelineSummary' smart constructor.
data MediaCapturePipelineSummary = MediaCapturePipelineSummary'
  { -- | The ARN of the media pipeline in the summary.
    mediaPipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the media pipeline in the summary.
    mediaPipelineId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaCapturePipelineSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaPipelineArn', 'mediaCapturePipelineSummary_mediaPipelineArn' - The ARN of the media pipeline in the summary.
--
-- 'mediaPipelineId', 'mediaCapturePipelineSummary_mediaPipelineId' - The ID of the media pipeline in the summary.
newMediaCapturePipelineSummary ::
  MediaCapturePipelineSummary
newMediaCapturePipelineSummary =
  MediaCapturePipelineSummary'
    { mediaPipelineArn =
        Prelude.Nothing,
      mediaPipelineId = Prelude.Nothing
    }

-- | The ARN of the media pipeline in the summary.
mediaCapturePipelineSummary_mediaPipelineArn :: Lens.Lens' MediaCapturePipelineSummary (Prelude.Maybe Prelude.Text)
mediaCapturePipelineSummary_mediaPipelineArn = Lens.lens (\MediaCapturePipelineSummary' {mediaPipelineArn} -> mediaPipelineArn) (\s@MediaCapturePipelineSummary' {} a -> s {mediaPipelineArn = a} :: MediaCapturePipelineSummary)

-- | The ID of the media pipeline in the summary.
mediaCapturePipelineSummary_mediaPipelineId :: Lens.Lens' MediaCapturePipelineSummary (Prelude.Maybe Prelude.Text)
mediaCapturePipelineSummary_mediaPipelineId = Lens.lens (\MediaCapturePipelineSummary' {mediaPipelineId} -> mediaPipelineId) (\s@MediaCapturePipelineSummary' {} a -> s {mediaPipelineId = a} :: MediaCapturePipelineSummary)

instance Data.FromJSON MediaCapturePipelineSummary where
  parseJSON =
    Data.withObject
      "MediaCapturePipelineSummary"
      ( \x ->
          MediaCapturePipelineSummary'
            Prelude.<$> (x Data..:? "MediaPipelineArn")
            Prelude.<*> (x Data..:? "MediaPipelineId")
      )

instance Prelude.Hashable MediaCapturePipelineSummary where
  hashWithSalt _salt MediaCapturePipelineSummary' {..} =
    _salt `Prelude.hashWithSalt` mediaPipelineArn
      `Prelude.hashWithSalt` mediaPipelineId

instance Prelude.NFData MediaCapturePipelineSummary where
  rnf MediaCapturePipelineSummary' {..} =
    Prelude.rnf mediaPipelineArn
      `Prelude.seq` Prelude.rnf mediaPipelineId
