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
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Metadata about your Savings Plans Purchase Recommendations.
--
-- /See:/ 'newSavingsPlansPurchaseRecommendationMetadata' smart constructor.
data SavingsPlansPurchaseRecommendationMetadata = SavingsPlansPurchaseRecommendationMetadata'
  { -- | The unique identifier for the recommendation set.
    recommendationId :: Core.Maybe Core.Text,
    -- | Additional metadata that may be applicable to the recommendation.
    additionalMetadata :: Core.Maybe Core.Text,
    -- | The timestamp showing when the recommendations were generated.
    generationTimestamp :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SavingsPlansPurchaseRecommendationMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendationId', 'savingsPlansPurchaseRecommendationMetadata_recommendationId' - The unique identifier for the recommendation set.
--
-- 'additionalMetadata', 'savingsPlansPurchaseRecommendationMetadata_additionalMetadata' - Additional metadata that may be applicable to the recommendation.
--
-- 'generationTimestamp', 'savingsPlansPurchaseRecommendationMetadata_generationTimestamp' - The timestamp showing when the recommendations were generated.
newSavingsPlansPurchaseRecommendationMetadata ::
  SavingsPlansPurchaseRecommendationMetadata
newSavingsPlansPurchaseRecommendationMetadata =
  SavingsPlansPurchaseRecommendationMetadata'
    { recommendationId =
        Core.Nothing,
      additionalMetadata =
        Core.Nothing,
      generationTimestamp =
        Core.Nothing
    }

-- | The unique identifier for the recommendation set.
savingsPlansPurchaseRecommendationMetadata_recommendationId :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationMetadata_recommendationId = Lens.lens (\SavingsPlansPurchaseRecommendationMetadata' {recommendationId} -> recommendationId) (\s@SavingsPlansPurchaseRecommendationMetadata' {} a -> s {recommendationId = a} :: SavingsPlansPurchaseRecommendationMetadata)

-- | Additional metadata that may be applicable to the recommendation.
savingsPlansPurchaseRecommendationMetadata_additionalMetadata :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationMetadata_additionalMetadata = Lens.lens (\SavingsPlansPurchaseRecommendationMetadata' {additionalMetadata} -> additionalMetadata) (\s@SavingsPlansPurchaseRecommendationMetadata' {} a -> s {additionalMetadata = a} :: SavingsPlansPurchaseRecommendationMetadata)

-- | The timestamp showing when the recommendations were generated.
savingsPlansPurchaseRecommendationMetadata_generationTimestamp :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationMetadata_generationTimestamp = Lens.lens (\SavingsPlansPurchaseRecommendationMetadata' {generationTimestamp} -> generationTimestamp) (\s@SavingsPlansPurchaseRecommendationMetadata' {} a -> s {generationTimestamp = a} :: SavingsPlansPurchaseRecommendationMetadata)

instance
  Core.FromJSON
    SavingsPlansPurchaseRecommendationMetadata
  where
  parseJSON =
    Core.withObject
      "SavingsPlansPurchaseRecommendationMetadata"
      ( \x ->
          SavingsPlansPurchaseRecommendationMetadata'
            Core.<$> (x Core..:? "RecommendationId")
            Core.<*> (x Core..:? "AdditionalMetadata")
            Core.<*> (x Core..:? "GenerationTimestamp")
      )

instance
  Core.Hashable
    SavingsPlansPurchaseRecommendationMetadata

instance
  Core.NFData
    SavingsPlansPurchaseRecommendationMetadata
