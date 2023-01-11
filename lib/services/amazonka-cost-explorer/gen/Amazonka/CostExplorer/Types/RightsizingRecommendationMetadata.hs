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
-- Module      : Amazonka.CostExplorer.Types.RightsizingRecommendationMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.RightsizingRecommendationMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.LookbackPeriodInDays
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata for this recommendation set.
--
-- /See:/ 'newRightsizingRecommendationMetadata' smart constructor.
data RightsizingRecommendationMetadata = RightsizingRecommendationMetadata'
  { -- | Additional metadata that might be applicable to the recommendation.
    additionalMetadata :: Prelude.Maybe Prelude.Text,
    -- | The timestamp for when Amazon Web Services made this recommendation.
    generationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The number of days of previous usage that Amazon Web Services considers
    -- when making this recommendation.
    lookbackPeriodInDays :: Prelude.Maybe LookbackPeriodInDays,
    -- | The ID for this specific recommendation.
    recommendationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RightsizingRecommendationMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalMetadata', 'rightsizingRecommendationMetadata_additionalMetadata' - Additional metadata that might be applicable to the recommendation.
--
-- 'generationTimestamp', 'rightsizingRecommendationMetadata_generationTimestamp' - The timestamp for when Amazon Web Services made this recommendation.
--
-- 'lookbackPeriodInDays', 'rightsizingRecommendationMetadata_lookbackPeriodInDays' - The number of days of previous usage that Amazon Web Services considers
-- when making this recommendation.
--
-- 'recommendationId', 'rightsizingRecommendationMetadata_recommendationId' - The ID for this specific recommendation.
newRightsizingRecommendationMetadata ::
  RightsizingRecommendationMetadata
newRightsizingRecommendationMetadata =
  RightsizingRecommendationMetadata'
    { additionalMetadata =
        Prelude.Nothing,
      generationTimestamp = Prelude.Nothing,
      lookbackPeriodInDays = Prelude.Nothing,
      recommendationId = Prelude.Nothing
    }

-- | Additional metadata that might be applicable to the recommendation.
rightsizingRecommendationMetadata_additionalMetadata :: Lens.Lens' RightsizingRecommendationMetadata (Prelude.Maybe Prelude.Text)
rightsizingRecommendationMetadata_additionalMetadata = Lens.lens (\RightsizingRecommendationMetadata' {additionalMetadata} -> additionalMetadata) (\s@RightsizingRecommendationMetadata' {} a -> s {additionalMetadata = a} :: RightsizingRecommendationMetadata)

-- | The timestamp for when Amazon Web Services made this recommendation.
rightsizingRecommendationMetadata_generationTimestamp :: Lens.Lens' RightsizingRecommendationMetadata (Prelude.Maybe Prelude.Text)
rightsizingRecommendationMetadata_generationTimestamp = Lens.lens (\RightsizingRecommendationMetadata' {generationTimestamp} -> generationTimestamp) (\s@RightsizingRecommendationMetadata' {} a -> s {generationTimestamp = a} :: RightsizingRecommendationMetadata)

-- | The number of days of previous usage that Amazon Web Services considers
-- when making this recommendation.
rightsizingRecommendationMetadata_lookbackPeriodInDays :: Lens.Lens' RightsizingRecommendationMetadata (Prelude.Maybe LookbackPeriodInDays)
rightsizingRecommendationMetadata_lookbackPeriodInDays = Lens.lens (\RightsizingRecommendationMetadata' {lookbackPeriodInDays} -> lookbackPeriodInDays) (\s@RightsizingRecommendationMetadata' {} a -> s {lookbackPeriodInDays = a} :: RightsizingRecommendationMetadata)

-- | The ID for this specific recommendation.
rightsizingRecommendationMetadata_recommendationId :: Lens.Lens' RightsizingRecommendationMetadata (Prelude.Maybe Prelude.Text)
rightsizingRecommendationMetadata_recommendationId = Lens.lens (\RightsizingRecommendationMetadata' {recommendationId} -> recommendationId) (\s@RightsizingRecommendationMetadata' {} a -> s {recommendationId = a} :: RightsizingRecommendationMetadata)

instance
  Data.FromJSON
    RightsizingRecommendationMetadata
  where
  parseJSON =
    Data.withObject
      "RightsizingRecommendationMetadata"
      ( \x ->
          RightsizingRecommendationMetadata'
            Prelude.<$> (x Data..:? "AdditionalMetadata")
            Prelude.<*> (x Data..:? "GenerationTimestamp")
            Prelude.<*> (x Data..:? "LookbackPeriodInDays")
            Prelude.<*> (x Data..:? "RecommendationId")
      )

instance
  Prelude.Hashable
    RightsizingRecommendationMetadata
  where
  hashWithSalt
    _salt
    RightsizingRecommendationMetadata' {..} =
      _salt `Prelude.hashWithSalt` additionalMetadata
        `Prelude.hashWithSalt` generationTimestamp
        `Prelude.hashWithSalt` lookbackPeriodInDays
        `Prelude.hashWithSalt` recommendationId

instance
  Prelude.NFData
    RightsizingRecommendationMetadata
  where
  rnf RightsizingRecommendationMetadata' {..} =
    Prelude.rnf additionalMetadata
      `Prelude.seq` Prelude.rnf generationTimestamp
      `Prelude.seq` Prelude.rnf lookbackPeriodInDays
      `Prelude.seq` Prelude.rnf recommendationId
