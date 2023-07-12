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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata about your Savings Plans Purchase Recommendations.
--
-- /See:/ 'newSavingsPlansPurchaseRecommendationMetadata' smart constructor.
data SavingsPlansPurchaseRecommendationMetadata = SavingsPlansPurchaseRecommendationMetadata'
  { -- | Additional metadata that might be applicable to the recommendation.
    additionalMetadata :: Prelude.Maybe Prelude.Text,
    -- | The timestamp that shows when the recommendations were generated.
    generationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the recommendation set.
    recommendationId :: Prelude.Maybe Prelude.Text
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
-- 'additionalMetadata', 'savingsPlansPurchaseRecommendationMetadata_additionalMetadata' - Additional metadata that might be applicable to the recommendation.
--
-- 'generationTimestamp', 'savingsPlansPurchaseRecommendationMetadata_generationTimestamp' - The timestamp that shows when the recommendations were generated.
--
-- 'recommendationId', 'savingsPlansPurchaseRecommendationMetadata_recommendationId' - The unique identifier for the recommendation set.
newSavingsPlansPurchaseRecommendationMetadata ::
  SavingsPlansPurchaseRecommendationMetadata
newSavingsPlansPurchaseRecommendationMetadata =
  SavingsPlansPurchaseRecommendationMetadata'
    { additionalMetadata =
        Prelude.Nothing,
      generationTimestamp =
        Prelude.Nothing,
      recommendationId =
        Prelude.Nothing
    }

-- | Additional metadata that might be applicable to the recommendation.
savingsPlansPurchaseRecommendationMetadata_additionalMetadata :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationMetadata_additionalMetadata = Lens.lens (\SavingsPlansPurchaseRecommendationMetadata' {additionalMetadata} -> additionalMetadata) (\s@SavingsPlansPurchaseRecommendationMetadata' {} a -> s {additionalMetadata = a} :: SavingsPlansPurchaseRecommendationMetadata)

-- | The timestamp that shows when the recommendations were generated.
savingsPlansPurchaseRecommendationMetadata_generationTimestamp :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationMetadata_generationTimestamp = Lens.lens (\SavingsPlansPurchaseRecommendationMetadata' {generationTimestamp} -> generationTimestamp) (\s@SavingsPlansPurchaseRecommendationMetadata' {} a -> s {generationTimestamp = a} :: SavingsPlansPurchaseRecommendationMetadata)

-- | The unique identifier for the recommendation set.
savingsPlansPurchaseRecommendationMetadata_recommendationId :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationMetadata_recommendationId = Lens.lens (\SavingsPlansPurchaseRecommendationMetadata' {recommendationId} -> recommendationId) (\s@SavingsPlansPurchaseRecommendationMetadata' {} a -> s {recommendationId = a} :: SavingsPlansPurchaseRecommendationMetadata)

instance
  Data.FromJSON
    SavingsPlansPurchaseRecommendationMetadata
  where
  parseJSON =
    Data.withObject
      "SavingsPlansPurchaseRecommendationMetadata"
      ( \x ->
          SavingsPlansPurchaseRecommendationMetadata'
            Prelude.<$> (x Data..:? "AdditionalMetadata")
            Prelude.<*> (x Data..:? "GenerationTimestamp")
            Prelude.<*> (x Data..:? "RecommendationId")
      )

instance
  Prelude.Hashable
    SavingsPlansPurchaseRecommendationMetadata
  where
  hashWithSalt
    _salt
    SavingsPlansPurchaseRecommendationMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` additionalMetadata
        `Prelude.hashWithSalt` generationTimestamp
        `Prelude.hashWithSalt` recommendationId

instance
  Prelude.NFData
    SavingsPlansPurchaseRecommendationMetadata
  where
  rnf SavingsPlansPurchaseRecommendationMetadata' {..} =
    Prelude.rnf additionalMetadata
      `Prelude.seq` Prelude.rnf generationTimestamp
      `Prelude.seq` Prelude.rnf recommendationId
