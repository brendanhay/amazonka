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
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Metadata about your Savings Plans Purchase Recommendations.
--
-- /See:/ 'newSavingsPlansPurchaseRecommendationMetadata' smart constructor.
data SavingsPlansPurchaseRecommendationMetadata = SavingsPlansPurchaseRecommendationMetadata'
  { -- | The unique identifier for the recommendation set.
    recommendationId :: Prelude.Maybe Prelude.Text,
    -- | Additional metadata that may be applicable to the recommendation.
    additionalMetadata :: Prelude.Maybe Prelude.Text,
    -- | The timestamp showing when the recommendations were generated.
    generationTimestamp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      additionalMetadata =
        Prelude.Nothing,
      generationTimestamp =
        Prelude.Nothing
    }

-- | The unique identifier for the recommendation set.
savingsPlansPurchaseRecommendationMetadata_recommendationId :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationMetadata_recommendationId = Lens.lens (\SavingsPlansPurchaseRecommendationMetadata' {recommendationId} -> recommendationId) (\s@SavingsPlansPurchaseRecommendationMetadata' {} a -> s {recommendationId = a} :: SavingsPlansPurchaseRecommendationMetadata)

-- | Additional metadata that may be applicable to the recommendation.
savingsPlansPurchaseRecommendationMetadata_additionalMetadata :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationMetadata_additionalMetadata = Lens.lens (\SavingsPlansPurchaseRecommendationMetadata' {additionalMetadata} -> additionalMetadata) (\s@SavingsPlansPurchaseRecommendationMetadata' {} a -> s {additionalMetadata = a} :: SavingsPlansPurchaseRecommendationMetadata)

-- | The timestamp showing when the recommendations were generated.
savingsPlansPurchaseRecommendationMetadata_generationTimestamp :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationMetadata_generationTimestamp = Lens.lens (\SavingsPlansPurchaseRecommendationMetadata' {generationTimestamp} -> generationTimestamp) (\s@SavingsPlansPurchaseRecommendationMetadata' {} a -> s {generationTimestamp = a} :: SavingsPlansPurchaseRecommendationMetadata)

instance
  Prelude.FromJSON
    SavingsPlansPurchaseRecommendationMetadata
  where
  parseJSON =
    Prelude.withObject
      "SavingsPlansPurchaseRecommendationMetadata"
      ( \x ->
          SavingsPlansPurchaseRecommendationMetadata'
            Prelude.<$> (x Prelude..:? "RecommendationId")
              Prelude.<*> (x Prelude..:? "AdditionalMetadata")
              Prelude.<*> (x Prelude..:? "GenerationTimestamp")
      )

instance
  Prelude.Hashable
    SavingsPlansPurchaseRecommendationMetadata

instance
  Prelude.NFData
    SavingsPlansPurchaseRecommendationMetadata
