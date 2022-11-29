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
-- Module      : Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Metadata about your Savings Plans Purchase Recommendations.
--
-- /See:/ 'newSavingsPlansPurchaseRecommendationMetadata' smart constructor.
data SavingsPlansPurchaseRecommendationMetadata = SavingsPlansPurchaseRecommendationMetadata'
  { -- | The unique identifier for the recommendation set.
    recommendationId :: Prelude.Maybe Prelude.Text,
    -- | Additional metadata that might be applicable to the recommendation.
    additionalMetadata :: Prelude.Maybe Prelude.Text,
    -- | The timestamp that shows when the recommendations were generated.
    generationTimestamp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'additionalMetadata', 'savingsPlansPurchaseRecommendationMetadata_additionalMetadata' - Additional metadata that might be applicable to the recommendation.
--
-- 'generationTimestamp', 'savingsPlansPurchaseRecommendationMetadata_generationTimestamp' - The timestamp that shows when the recommendations were generated.
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

-- | Additional metadata that might be applicable to the recommendation.
savingsPlansPurchaseRecommendationMetadata_additionalMetadata :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationMetadata_additionalMetadata = Lens.lens (\SavingsPlansPurchaseRecommendationMetadata' {additionalMetadata} -> additionalMetadata) (\s@SavingsPlansPurchaseRecommendationMetadata' {} a -> s {additionalMetadata = a} :: SavingsPlansPurchaseRecommendationMetadata)

-- | The timestamp that shows when the recommendations were generated.
savingsPlansPurchaseRecommendationMetadata_generationTimestamp :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Prelude.Maybe Prelude.Text)
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
            Prelude.<$> (x Core..:? "RecommendationId")
              Prelude.<*> (x Core..:? "AdditionalMetadata")
              Prelude.<*> (x Core..:? "GenerationTimestamp")
      )

instance
  Prelude.Hashable
    SavingsPlansPurchaseRecommendationMetadata
  where
  hashWithSalt
    _salt
    SavingsPlansPurchaseRecommendationMetadata' {..} =
      _salt `Prelude.hashWithSalt` recommendationId
        `Prelude.hashWithSalt` additionalMetadata
        `Prelude.hashWithSalt` generationTimestamp

instance
  Prelude.NFData
    SavingsPlansPurchaseRecommendationMetadata
  where
  rnf SavingsPlansPurchaseRecommendationMetadata' {..} =
    Prelude.rnf recommendationId
      `Prelude.seq` Prelude.rnf additionalMetadata
      `Prelude.seq` Prelude.rnf generationTimestamp
