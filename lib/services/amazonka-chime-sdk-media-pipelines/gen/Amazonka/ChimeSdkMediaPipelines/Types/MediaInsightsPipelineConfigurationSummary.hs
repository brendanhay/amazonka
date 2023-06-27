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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipelineConfigurationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipelineConfigurationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of the media insights pipeline configuration.
--
-- /See:/ 'newMediaInsightsPipelineConfigurationSummary' smart constructor.
data MediaInsightsPipelineConfigurationSummary = MediaInsightsPipelineConfigurationSummary'
  { -- | The ARN of the media insights pipeline configuration.
    mediaInsightsPipelineConfigurationArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the media insights pipeline configuration.
    mediaInsightsPipelineConfigurationId :: Prelude.Maybe Prelude.Text,
    -- | The name of the media insights pipeline configuration.
    mediaInsightsPipelineConfigurationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaInsightsPipelineConfigurationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaInsightsPipelineConfigurationArn', 'mediaInsightsPipelineConfigurationSummary_mediaInsightsPipelineConfigurationArn' - The ARN of the media insights pipeline configuration.
--
-- 'mediaInsightsPipelineConfigurationId', 'mediaInsightsPipelineConfigurationSummary_mediaInsightsPipelineConfigurationId' - The ID of the media insights pipeline configuration.
--
-- 'mediaInsightsPipelineConfigurationName', 'mediaInsightsPipelineConfigurationSummary_mediaInsightsPipelineConfigurationName' - The name of the media insights pipeline configuration.
newMediaInsightsPipelineConfigurationSummary ::
  MediaInsightsPipelineConfigurationSummary
newMediaInsightsPipelineConfigurationSummary =
  MediaInsightsPipelineConfigurationSummary'
    { mediaInsightsPipelineConfigurationArn =
        Prelude.Nothing,
      mediaInsightsPipelineConfigurationId =
        Prelude.Nothing,
      mediaInsightsPipelineConfigurationName =
        Prelude.Nothing
    }

-- | The ARN of the media insights pipeline configuration.
mediaInsightsPipelineConfigurationSummary_mediaInsightsPipelineConfigurationArn :: Lens.Lens' MediaInsightsPipelineConfigurationSummary (Prelude.Maybe Prelude.Text)
mediaInsightsPipelineConfigurationSummary_mediaInsightsPipelineConfigurationArn = Lens.lens (\MediaInsightsPipelineConfigurationSummary' {mediaInsightsPipelineConfigurationArn} -> mediaInsightsPipelineConfigurationArn) (\s@MediaInsightsPipelineConfigurationSummary' {} a -> s {mediaInsightsPipelineConfigurationArn = a} :: MediaInsightsPipelineConfigurationSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the media insights pipeline configuration.
mediaInsightsPipelineConfigurationSummary_mediaInsightsPipelineConfigurationId :: Lens.Lens' MediaInsightsPipelineConfigurationSummary (Prelude.Maybe Prelude.Text)
mediaInsightsPipelineConfigurationSummary_mediaInsightsPipelineConfigurationId = Lens.lens (\MediaInsightsPipelineConfigurationSummary' {mediaInsightsPipelineConfigurationId} -> mediaInsightsPipelineConfigurationId) (\s@MediaInsightsPipelineConfigurationSummary' {} a -> s {mediaInsightsPipelineConfigurationId = a} :: MediaInsightsPipelineConfigurationSummary)

-- | The name of the media insights pipeline configuration.
mediaInsightsPipelineConfigurationSummary_mediaInsightsPipelineConfigurationName :: Lens.Lens' MediaInsightsPipelineConfigurationSummary (Prelude.Maybe Prelude.Text)
mediaInsightsPipelineConfigurationSummary_mediaInsightsPipelineConfigurationName = Lens.lens (\MediaInsightsPipelineConfigurationSummary' {mediaInsightsPipelineConfigurationName} -> mediaInsightsPipelineConfigurationName) (\s@MediaInsightsPipelineConfigurationSummary' {} a -> s {mediaInsightsPipelineConfigurationName = a} :: MediaInsightsPipelineConfigurationSummary)

instance
  Data.FromJSON
    MediaInsightsPipelineConfigurationSummary
  where
  parseJSON =
    Data.withObject
      "MediaInsightsPipelineConfigurationSummary"
      ( \x ->
          MediaInsightsPipelineConfigurationSummary'
            Prelude.<$> (x Data..:? "MediaInsightsPipelineConfigurationArn")
            Prelude.<*> (x Data..:? "MediaInsightsPipelineConfigurationId")
            Prelude.<*> ( x
                            Data..:? "MediaInsightsPipelineConfigurationName"
                        )
      )

instance
  Prelude.Hashable
    MediaInsightsPipelineConfigurationSummary
  where
  hashWithSalt
    _salt
    MediaInsightsPipelineConfigurationSummary' {..} =
      _salt
        `Prelude.hashWithSalt` mediaInsightsPipelineConfigurationArn
        `Prelude.hashWithSalt` mediaInsightsPipelineConfigurationId
        `Prelude.hashWithSalt` mediaInsightsPipelineConfigurationName

instance
  Prelude.NFData
    MediaInsightsPipelineConfigurationSummary
  where
  rnf MediaInsightsPipelineConfigurationSummary' {..} =
    Prelude.rnf mediaInsightsPipelineConfigurationArn
      `Prelude.seq` Prelude.rnf mediaInsightsPipelineConfigurationId
      `Prelude.seq` Prelude.rnf mediaInsightsPipelineConfigurationName
