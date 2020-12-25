{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    spprmAdditionalMetadata,
    spprmGenerationTimestamp,
    spprmRecommendationId,
  )
where

import qualified Network.AWS.CostExplorer.Types.GenericString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Metadata about your Savings Plans Purchase Recommendations.
--
-- /See:/ 'mkSavingsPlansPurchaseRecommendationMetadata' smart constructor.
data SavingsPlansPurchaseRecommendationMetadata = SavingsPlansPurchaseRecommendationMetadata'
  { -- | Additional metadata that may be applicable to the recommendation.
    additionalMetadata :: Core.Maybe Types.GenericString,
    -- | The timestamp showing when the recommendations were generated.
    generationTimestamp :: Core.Maybe Types.GenericString,
    -- | The unique identifier for the recommendation set.
    recommendationId :: Core.Maybe Types.GenericString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SavingsPlansPurchaseRecommendationMetadata' value with any optional fields omitted.
mkSavingsPlansPurchaseRecommendationMetadata ::
  SavingsPlansPurchaseRecommendationMetadata
mkSavingsPlansPurchaseRecommendationMetadata =
  SavingsPlansPurchaseRecommendationMetadata'
    { additionalMetadata =
        Core.Nothing,
      generationTimestamp = Core.Nothing,
      recommendationId = Core.Nothing
    }

-- | Additional metadata that may be applicable to the recommendation.
--
-- /Note:/ Consider using 'additionalMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprmAdditionalMetadata :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Core.Maybe Types.GenericString)
spprmAdditionalMetadata = Lens.field @"additionalMetadata"
{-# DEPRECATED spprmAdditionalMetadata "Use generic-lens or generic-optics with 'additionalMetadata' instead." #-}

-- | The timestamp showing when the recommendations were generated.
--
-- /Note:/ Consider using 'generationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprmGenerationTimestamp :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Core.Maybe Types.GenericString)
spprmGenerationTimestamp = Lens.field @"generationTimestamp"
{-# DEPRECATED spprmGenerationTimestamp "Use generic-lens or generic-optics with 'generationTimestamp' instead." #-}

-- | The unique identifier for the recommendation set.
--
-- /Note:/ Consider using 'recommendationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprmRecommendationId :: Lens.Lens' SavingsPlansPurchaseRecommendationMetadata (Core.Maybe Types.GenericString)
spprmRecommendationId = Lens.field @"recommendationId"
{-# DEPRECATED spprmRecommendationId "Use generic-lens or generic-optics with 'recommendationId' instead." #-}

instance Core.FromJSON SavingsPlansPurchaseRecommendationMetadata where
  parseJSON =
    Core.withObject "SavingsPlansPurchaseRecommendationMetadata" Core.$
      \x ->
        SavingsPlansPurchaseRecommendationMetadata'
          Core.<$> (x Core..:? "AdditionalMetadata")
          Core.<*> (x Core..:? "GenerationTimestamp")
          Core.<*> (x Core..:? "RecommendationId")
