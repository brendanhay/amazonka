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
-- Module      : Amazonka.SageMaker.Types.RecommendationJobPayloadConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RecommendationJobPayloadConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the payload for a recommendation job.
--
-- /See:/ 'newRecommendationJobPayloadConfig' smart constructor.
data RecommendationJobPayloadConfig = RecommendationJobPayloadConfig'
  { -- | The Amazon Simple Storage Service (Amazon S3) path where the sample
    -- payload is stored. This path must point to a single gzip compressed tar
    -- archive (.tar.gz suffix).
    samplePayloadUrl :: Prelude.Maybe Prelude.Text,
    -- | The supported MIME types for the input data.
    supportedContentTypes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationJobPayloadConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samplePayloadUrl', 'recommendationJobPayloadConfig_samplePayloadUrl' - The Amazon Simple Storage Service (Amazon S3) path where the sample
-- payload is stored. This path must point to a single gzip compressed tar
-- archive (.tar.gz suffix).
--
-- 'supportedContentTypes', 'recommendationJobPayloadConfig_supportedContentTypes' - The supported MIME types for the input data.
newRecommendationJobPayloadConfig ::
  RecommendationJobPayloadConfig
newRecommendationJobPayloadConfig =
  RecommendationJobPayloadConfig'
    { samplePayloadUrl =
        Prelude.Nothing,
      supportedContentTypes = Prelude.Nothing
    }

-- | The Amazon Simple Storage Service (Amazon S3) path where the sample
-- payload is stored. This path must point to a single gzip compressed tar
-- archive (.tar.gz suffix).
recommendationJobPayloadConfig_samplePayloadUrl :: Lens.Lens' RecommendationJobPayloadConfig (Prelude.Maybe Prelude.Text)
recommendationJobPayloadConfig_samplePayloadUrl = Lens.lens (\RecommendationJobPayloadConfig' {samplePayloadUrl} -> samplePayloadUrl) (\s@RecommendationJobPayloadConfig' {} a -> s {samplePayloadUrl = a} :: RecommendationJobPayloadConfig)

-- | The supported MIME types for the input data.
recommendationJobPayloadConfig_supportedContentTypes :: Lens.Lens' RecommendationJobPayloadConfig (Prelude.Maybe [Prelude.Text])
recommendationJobPayloadConfig_supportedContentTypes = Lens.lens (\RecommendationJobPayloadConfig' {supportedContentTypes} -> supportedContentTypes) (\s@RecommendationJobPayloadConfig' {} a -> s {supportedContentTypes = a} :: RecommendationJobPayloadConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RecommendationJobPayloadConfig where
  parseJSON =
    Data.withObject
      "RecommendationJobPayloadConfig"
      ( \x ->
          RecommendationJobPayloadConfig'
            Prelude.<$> (x Data..:? "SamplePayloadUrl")
            Prelude.<*> ( x
                            Data..:? "SupportedContentTypes"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    RecommendationJobPayloadConfig
  where
  hashWithSalt
    _salt
    RecommendationJobPayloadConfig' {..} =
      _salt
        `Prelude.hashWithSalt` samplePayloadUrl
        `Prelude.hashWithSalt` supportedContentTypes

instance
  Prelude.NFData
    RecommendationJobPayloadConfig
  where
  rnf RecommendationJobPayloadConfig' {..} =
    Prelude.rnf samplePayloadUrl `Prelude.seq`
      Prelude.rnf supportedContentTypes

instance Data.ToJSON RecommendationJobPayloadConfig where
  toJSON RecommendationJobPayloadConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SamplePayloadUrl" Data..=)
              Prelude.<$> samplePayloadUrl,
            ("SupportedContentTypes" Data..=)
              Prelude.<$> supportedContentTypes
          ]
      )
