-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata
  ( SavingsPlansPurchaseRecommendationMetadata (..),

    -- * Smart constructor
    mkSavingsPlansPurchaseRecommendationMetadata,

    -- * Lenses
    spprmRecommendationId,
    spprmGenerationTimestamp,
    spprmAdditionalMetadata,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Metadata about your Savings Plans Purchase Recommendations.
--
-- /See:/ 'mkSavingsPlansPurchaseRecommendationMetadata' smart constructor.
data SavingsPlansPurchaseRecommendationMetadata = SavingsPlansPurchaseRecommendationMetadata'
  { recommendationId ::
      Lude.Maybe
        Lude.Text,
    generationTimestamp ::
      Lude.Maybe
        Lude.Text,
    additionalMetadata ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SavingsPlansPurchaseRecommendationMetadata' with the minimum fields required to make a request.
--
-- * 'additionalMetadata' - Additional metadata that may be applicable to the recommendation.
-- * 'generationTimestamp' - The timestamp showing when the recommendations were generated.
-- * 'recommendationId' - The unique identifier for the recommendation set.
mkSavingsPlansPurchaseRecommendationMetadata ::
  SavingsPlansPurchaseRecommendationMetadata
mkSavingsPlansPurchaseRecommendationMetadata =
  SavingsPlansPurchaseRecommendationMetadata'
    { recommendationId =
        Lude.Nothing,
      generationTimestamp = Lude.Nothing,
      additionalMetadata = Lude.Nothing
    }

-- | The unique identifier for the recommendation set.
--
-- /Note:/ Consider using 'recommendationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprmRecommendationId :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Lude.Maybe Lude.Text)
spprmRecommendationId = Lens.lens (recommendationId :: SavingsPlansPurchaseRecommendationMetadata -> Lude.Maybe Lude.Text) (\s a -> s {recommendationId = a} :: SavingsPlansPurchaseRecommendationMetadata)
{-# DEPRECATED spprmRecommendationId "Use generic-lens or generic-optics with 'recommendationId' instead." #-}

-- | The timestamp showing when the recommendations were generated.
--
-- /Note:/ Consider using 'generationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprmGenerationTimestamp :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Lude.Maybe Lude.Text)
spprmGenerationTimestamp = Lens.lens (generationTimestamp :: SavingsPlansPurchaseRecommendationMetadata -> Lude.Maybe Lude.Text) (\s a -> s {generationTimestamp = a} :: SavingsPlansPurchaseRecommendationMetadata)
{-# DEPRECATED spprmGenerationTimestamp "Use generic-lens or generic-optics with 'generationTimestamp' instead." #-}

-- | Additional metadata that may be applicable to the recommendation.
--
-- /Note:/ Consider using 'additionalMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprmAdditionalMetadata :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Lude.Maybe Lude.Text)
spprmAdditionalMetadata = Lens.lens (additionalMetadata :: SavingsPlansPurchaseRecommendationMetadata -> Lude.Maybe Lude.Text) (\s a -> s {additionalMetadata = a} :: SavingsPlansPurchaseRecommendationMetadata)
{-# DEPRECATED spprmAdditionalMetadata "Use generic-lens or generic-optics with 'additionalMetadata' instead." #-}

instance Lude.FromJSON SavingsPlansPurchaseRecommendationMetadata where
  parseJSON =
    Lude.withObject
      "SavingsPlansPurchaseRecommendationMetadata"
      ( \x ->
          SavingsPlansPurchaseRecommendationMetadata'
            Lude.<$> (x Lude..:? "RecommendationId")
            Lude.<*> (x Lude..:? "GenerationTimestamp")
            Lude.<*> (x Lude..:? "AdditionalMetadata")
      )
