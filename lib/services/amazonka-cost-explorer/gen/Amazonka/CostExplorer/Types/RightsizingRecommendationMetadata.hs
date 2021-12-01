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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.RightsizingRecommendationMetadata where

import qualified Amazonka.Core as Core
import Amazonka.CostExplorer.Types.LookbackPeriodInDays
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Metadata for this recommendation set.
--
-- /See:/ 'newRightsizingRecommendationMetadata' smart constructor.
data RightsizingRecommendationMetadata = RightsizingRecommendationMetadata'
  { -- | The ID for this specific recommendation.
    recommendationId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp for when Amazon Web Services made this recommendation.
    generationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | Additional metadata that might be applicable to the recommendation.
    additionalMetadata :: Prelude.Maybe Prelude.Text,
    -- | The number of days of previous usage that Amazon Web Services considers
    -- when making this recommendation.
    lookbackPeriodInDays :: Prelude.Maybe LookbackPeriodInDays
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
-- 'recommendationId', 'rightsizingRecommendationMetadata_recommendationId' - The ID for this specific recommendation.
--
-- 'generationTimestamp', 'rightsizingRecommendationMetadata_generationTimestamp' - The timestamp for when Amazon Web Services made this recommendation.
--
-- 'additionalMetadata', 'rightsizingRecommendationMetadata_additionalMetadata' - Additional metadata that might be applicable to the recommendation.
--
-- 'lookbackPeriodInDays', 'rightsizingRecommendationMetadata_lookbackPeriodInDays' - The number of days of previous usage that Amazon Web Services considers
-- when making this recommendation.
newRightsizingRecommendationMetadata ::
  RightsizingRecommendationMetadata
newRightsizingRecommendationMetadata =
  RightsizingRecommendationMetadata'
    { recommendationId =
        Prelude.Nothing,
      generationTimestamp = Prelude.Nothing,
      additionalMetadata = Prelude.Nothing,
      lookbackPeriodInDays = Prelude.Nothing
    }

-- | The ID for this specific recommendation.
rightsizingRecommendationMetadata_recommendationId :: Lens.Lens' RightsizingRecommendationMetadata (Prelude.Maybe Prelude.Text)
rightsizingRecommendationMetadata_recommendationId = Lens.lens (\RightsizingRecommendationMetadata' {recommendationId} -> recommendationId) (\s@RightsizingRecommendationMetadata' {} a -> s {recommendationId = a} :: RightsizingRecommendationMetadata)

-- | The timestamp for when Amazon Web Services made this recommendation.
rightsizingRecommendationMetadata_generationTimestamp :: Lens.Lens' RightsizingRecommendationMetadata (Prelude.Maybe Prelude.Text)
rightsizingRecommendationMetadata_generationTimestamp = Lens.lens (\RightsizingRecommendationMetadata' {generationTimestamp} -> generationTimestamp) (\s@RightsizingRecommendationMetadata' {} a -> s {generationTimestamp = a} :: RightsizingRecommendationMetadata)

-- | Additional metadata that might be applicable to the recommendation.
rightsizingRecommendationMetadata_additionalMetadata :: Lens.Lens' RightsizingRecommendationMetadata (Prelude.Maybe Prelude.Text)
rightsizingRecommendationMetadata_additionalMetadata = Lens.lens (\RightsizingRecommendationMetadata' {additionalMetadata} -> additionalMetadata) (\s@RightsizingRecommendationMetadata' {} a -> s {additionalMetadata = a} :: RightsizingRecommendationMetadata)

-- | The number of days of previous usage that Amazon Web Services considers
-- when making this recommendation.
rightsizingRecommendationMetadata_lookbackPeriodInDays :: Lens.Lens' RightsizingRecommendationMetadata (Prelude.Maybe LookbackPeriodInDays)
rightsizingRecommendationMetadata_lookbackPeriodInDays = Lens.lens (\RightsizingRecommendationMetadata' {lookbackPeriodInDays} -> lookbackPeriodInDays) (\s@RightsizingRecommendationMetadata' {} a -> s {lookbackPeriodInDays = a} :: RightsizingRecommendationMetadata)

instance
  Core.FromJSON
    RightsizingRecommendationMetadata
  where
  parseJSON =
    Core.withObject
      "RightsizingRecommendationMetadata"
      ( \x ->
          RightsizingRecommendationMetadata'
            Prelude.<$> (x Core..:? "RecommendationId")
            Prelude.<*> (x Core..:? "GenerationTimestamp")
            Prelude.<*> (x Core..:? "AdditionalMetadata")
            Prelude.<*> (x Core..:? "LookbackPeriodInDays")
      )

instance
  Prelude.Hashable
    RightsizingRecommendationMetadata
  where
  hashWithSalt
    salt'
    RightsizingRecommendationMetadata' {..} =
      salt' `Prelude.hashWithSalt` lookbackPeriodInDays
        `Prelude.hashWithSalt` additionalMetadata
        `Prelude.hashWithSalt` generationTimestamp
        `Prelude.hashWithSalt` recommendationId

instance
  Prelude.NFData
    RightsizingRecommendationMetadata
  where
  rnf RightsizingRecommendationMetadata' {..} =
    Prelude.rnf recommendationId
      `Prelude.seq` Prelude.rnf lookbackPeriodInDays
      `Prelude.seq` Prelude.rnf additionalMetadata
      `Prelude.seq` Prelude.rnf generationTimestamp
