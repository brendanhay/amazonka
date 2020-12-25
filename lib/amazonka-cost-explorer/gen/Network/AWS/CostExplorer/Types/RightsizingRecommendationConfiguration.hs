{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingRecommendationConfiguration
  ( RightsizingRecommendationConfiguration (..),

    -- * Smart constructor
    mkRightsizingRecommendationConfiguration,

    -- * Lenses
    rrcRecommendationTarget,
    rrcBenefitsConsidered,
  )
where

import qualified Network.AWS.CostExplorer.Types.RecommendationTarget as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither.
--
-- /See:/ 'mkRightsizingRecommendationConfiguration' smart constructor.
data RightsizingRecommendationConfiguration = RightsizingRecommendationConfiguration'
  { -- | The option to see recommendations within the same instance family, or recommendations for instances across other families. The default value is @SAME_INSTANCE_FAMILY@ .
    recommendationTarget :: Types.RecommendationTarget,
    -- | The option to consider RI or Savings Plans discount benefits in your savings calculation. The default value is @TRUE@ .
    benefitsConsidered :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RightsizingRecommendationConfiguration' value with any optional fields omitted.
mkRightsizingRecommendationConfiguration ::
  -- | 'recommendationTarget'
  Types.RecommendationTarget ->
  -- | 'benefitsConsidered'
  Core.Bool ->
  RightsizingRecommendationConfiguration
mkRightsizingRecommendationConfiguration
  recommendationTarget
  benefitsConsidered =
    RightsizingRecommendationConfiguration'
      { recommendationTarget,
        benefitsConsidered
      }

-- | The option to see recommendations within the same instance family, or recommendations for instances across other families. The default value is @SAME_INSTANCE_FAMILY@ .
--
-- /Note:/ Consider using 'recommendationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrcRecommendationTarget :: Lens.Lens' RightsizingRecommendationConfiguration Types.RecommendationTarget
rrcRecommendationTarget = Lens.field @"recommendationTarget"
{-# DEPRECATED rrcRecommendationTarget "Use generic-lens or generic-optics with 'recommendationTarget' instead." #-}

-- | The option to consider RI or Savings Plans discount benefits in your savings calculation. The default value is @TRUE@ .
--
-- /Note:/ Consider using 'benefitsConsidered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrcBenefitsConsidered :: Lens.Lens' RightsizingRecommendationConfiguration Core.Bool
rrcBenefitsConsidered = Lens.field @"benefitsConsidered"
{-# DEPRECATED rrcBenefitsConsidered "Use generic-lens or generic-optics with 'benefitsConsidered' instead." #-}

instance Core.FromJSON RightsizingRecommendationConfiguration where
  toJSON RightsizingRecommendationConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RecommendationTarget" Core..= recommendationTarget),
            Core.Just ("BenefitsConsidered" Core..= benefitsConsidered)
          ]
      )

instance Core.FromJSON RightsizingRecommendationConfiguration where
  parseJSON =
    Core.withObject "RightsizingRecommendationConfiguration" Core.$
      \x ->
        RightsizingRecommendationConfiguration'
          Core.<$> (x Core..: "RecommendationTarget")
          Core.<*> (x Core..: "BenefitsConsidered")
