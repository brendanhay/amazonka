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

import Network.AWS.CostExplorer.Types.RecommendationTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither.
--
-- /See:/ 'mkRightsizingRecommendationConfiguration' smart constructor.
data RightsizingRecommendationConfiguration = RightsizingRecommendationConfiguration'
  { recommendationTarget ::
      RecommendationTarget,
    benefitsConsidered ::
      Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RightsizingRecommendationConfiguration' with the minimum fields required to make a request.
--
-- * 'benefitsConsidered' - The option to consider RI or Savings Plans discount benefits in your savings calculation. The default value is @TRUE@ .
-- * 'recommendationTarget' - The option to see recommendations within the same instance family, or recommendations for instances across other families. The default value is @SAME_INSTANCE_FAMILY@ .
mkRightsizingRecommendationConfiguration ::
  -- | 'recommendationTarget'
  RecommendationTarget ->
  -- | 'benefitsConsidered'
  Lude.Bool ->
  RightsizingRecommendationConfiguration
mkRightsizingRecommendationConfiguration
  pRecommendationTarget_
  pBenefitsConsidered_ =
    RightsizingRecommendationConfiguration'
      { recommendationTarget =
          pRecommendationTarget_,
        benefitsConsidered = pBenefitsConsidered_
      }

-- | The option to see recommendations within the same instance family, or recommendations for instances across other families. The default value is @SAME_INSTANCE_FAMILY@ .
--
-- /Note:/ Consider using 'recommendationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrcRecommendationTarget :: Lens.Lens' RightsizingRecommendationConfiguration RecommendationTarget
rrcRecommendationTarget = Lens.lens (recommendationTarget :: RightsizingRecommendationConfiguration -> RecommendationTarget) (\s a -> s {recommendationTarget = a} :: RightsizingRecommendationConfiguration)
{-# DEPRECATED rrcRecommendationTarget "Use generic-lens or generic-optics with 'recommendationTarget' instead." #-}

-- | The option to consider RI or Savings Plans discount benefits in your savings calculation. The default value is @TRUE@ .
--
-- /Note:/ Consider using 'benefitsConsidered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrcBenefitsConsidered :: Lens.Lens' RightsizingRecommendationConfiguration Lude.Bool
rrcBenefitsConsidered = Lens.lens (benefitsConsidered :: RightsizingRecommendationConfiguration -> Lude.Bool) (\s a -> s {benefitsConsidered = a} :: RightsizingRecommendationConfiguration)
{-# DEPRECATED rrcBenefitsConsidered "Use generic-lens or generic-optics with 'benefitsConsidered' instead." #-}

instance Lude.FromJSON RightsizingRecommendationConfiguration where
  parseJSON =
    Lude.withObject
      "RightsizingRecommendationConfiguration"
      ( \x ->
          RightsizingRecommendationConfiguration'
            Lude.<$> (x Lude..: "RecommendationTarget")
            Lude.<*> (x Lude..: "BenefitsConsidered")
      )

instance Lude.ToJSON RightsizingRecommendationConfiguration where
  toJSON RightsizingRecommendationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RecommendationTarget" Lude..= recommendationTarget),
            Lude.Just ("BenefitsConsidered" Lude..= benefitsConsidered)
          ]
      )
