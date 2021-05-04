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
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendationMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingRecommendationMetadata where

import Network.AWS.CostExplorer.Types.LookbackPeriodInDays
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Metadata for this recommendation set.
--
-- /See:/ 'newRightsizingRecommendationMetadata' smart constructor.
data RightsizingRecommendationMetadata = RightsizingRecommendationMetadata'
  { -- | The ID for this specific recommendation.
    recommendationId :: Prelude.Maybe Prelude.Text,
    -- | Additional metadata that may be applicable to the recommendation.
    additionalMetadata :: Prelude.Maybe Prelude.Text,
    -- | How many days of previous usage that AWS considers when making this
    -- recommendation.
    lookbackPeriodInDays :: Prelude.Maybe LookbackPeriodInDays,
    -- | The timestamp for when AWS made this recommendation.
    generationTimestamp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'additionalMetadata', 'rightsizingRecommendationMetadata_additionalMetadata' - Additional metadata that may be applicable to the recommendation.
--
-- 'lookbackPeriodInDays', 'rightsizingRecommendationMetadata_lookbackPeriodInDays' - How many days of previous usage that AWS considers when making this
-- recommendation.
--
-- 'generationTimestamp', 'rightsizingRecommendationMetadata_generationTimestamp' - The timestamp for when AWS made this recommendation.
newRightsizingRecommendationMetadata ::
  RightsizingRecommendationMetadata
newRightsizingRecommendationMetadata =
  RightsizingRecommendationMetadata'
    { recommendationId =
        Prelude.Nothing,
      additionalMetadata = Prelude.Nothing,
      lookbackPeriodInDays = Prelude.Nothing,
      generationTimestamp = Prelude.Nothing
    }

-- | The ID for this specific recommendation.
rightsizingRecommendationMetadata_recommendationId :: Lens.Lens' RightsizingRecommendationMetadata (Prelude.Maybe Prelude.Text)
rightsizingRecommendationMetadata_recommendationId = Lens.lens (\RightsizingRecommendationMetadata' {recommendationId} -> recommendationId) (\s@RightsizingRecommendationMetadata' {} a -> s {recommendationId = a} :: RightsizingRecommendationMetadata)

-- | Additional metadata that may be applicable to the recommendation.
rightsizingRecommendationMetadata_additionalMetadata :: Lens.Lens' RightsizingRecommendationMetadata (Prelude.Maybe Prelude.Text)
rightsizingRecommendationMetadata_additionalMetadata = Lens.lens (\RightsizingRecommendationMetadata' {additionalMetadata} -> additionalMetadata) (\s@RightsizingRecommendationMetadata' {} a -> s {additionalMetadata = a} :: RightsizingRecommendationMetadata)

-- | How many days of previous usage that AWS considers when making this
-- recommendation.
rightsizingRecommendationMetadata_lookbackPeriodInDays :: Lens.Lens' RightsizingRecommendationMetadata (Prelude.Maybe LookbackPeriodInDays)
rightsizingRecommendationMetadata_lookbackPeriodInDays = Lens.lens (\RightsizingRecommendationMetadata' {lookbackPeriodInDays} -> lookbackPeriodInDays) (\s@RightsizingRecommendationMetadata' {} a -> s {lookbackPeriodInDays = a} :: RightsizingRecommendationMetadata)

-- | The timestamp for when AWS made this recommendation.
rightsizingRecommendationMetadata_generationTimestamp :: Lens.Lens' RightsizingRecommendationMetadata (Prelude.Maybe Prelude.Text)
rightsizingRecommendationMetadata_generationTimestamp = Lens.lens (\RightsizingRecommendationMetadata' {generationTimestamp} -> generationTimestamp) (\s@RightsizingRecommendationMetadata' {} a -> s {generationTimestamp = a} :: RightsizingRecommendationMetadata)

instance
  Prelude.FromJSON
    RightsizingRecommendationMetadata
  where
  parseJSON =
    Prelude.withObject
      "RightsizingRecommendationMetadata"
      ( \x ->
          RightsizingRecommendationMetadata'
            Prelude.<$> (x Prelude..:? "RecommendationId")
            Prelude.<*> (x Prelude..:? "AdditionalMetadata")
            Prelude.<*> (x Prelude..:? "LookbackPeriodInDays")
            Prelude.<*> (x Prelude..:? "GenerationTimestamp")
      )

instance
  Prelude.Hashable
    RightsizingRecommendationMetadata

instance
  Prelude.NFData
    RightsizingRecommendationMetadata
