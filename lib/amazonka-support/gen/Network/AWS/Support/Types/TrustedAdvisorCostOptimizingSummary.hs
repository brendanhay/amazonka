{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCostOptimizingSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorCostOptimizingSummary
  ( TrustedAdvisorCostOptimizingSummary (..),

    -- * Smart constructor
    mkTrustedAdvisorCostOptimizingSummary,

    -- * Lenses
    tacosEstimatedMonthlySavings,
    tacosEstimatedPercentMonthlySavings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The estimated cost savings that might be realized if the recommended operations are taken.
--
-- /See:/ 'mkTrustedAdvisorCostOptimizingSummary' smart constructor.
data TrustedAdvisorCostOptimizingSummary = TrustedAdvisorCostOptimizingSummary'
  { estimatedMonthlySavings ::
      Lude.Double,
    estimatedPercentMonthlySavings ::
      Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrustedAdvisorCostOptimizingSummary' with the minimum fields required to make a request.
--
-- * 'estimatedMonthlySavings' - The estimated monthly savings that might be realized if the recommended operations are taken.
-- * 'estimatedPercentMonthlySavings' - The estimated percentage of savings that might be realized if the recommended operations are taken.
mkTrustedAdvisorCostOptimizingSummary ::
  -- | 'estimatedMonthlySavings'
  Lude.Double ->
  -- | 'estimatedPercentMonthlySavings'
  Lude.Double ->
  TrustedAdvisorCostOptimizingSummary
mkTrustedAdvisorCostOptimizingSummary
  pEstimatedMonthlySavings_
  pEstimatedPercentMonthlySavings_ =
    TrustedAdvisorCostOptimizingSummary'
      { estimatedMonthlySavings =
          pEstimatedMonthlySavings_,
        estimatedPercentMonthlySavings =
          pEstimatedPercentMonthlySavings_
      }

-- | The estimated monthly savings that might be realized if the recommended operations are taken.
--
-- /Note:/ Consider using 'estimatedMonthlySavings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacosEstimatedMonthlySavings :: Lens.Lens' TrustedAdvisorCostOptimizingSummary Lude.Double
tacosEstimatedMonthlySavings = Lens.lens (estimatedMonthlySavings :: TrustedAdvisorCostOptimizingSummary -> Lude.Double) (\s a -> s {estimatedMonthlySavings = a} :: TrustedAdvisorCostOptimizingSummary)
{-# DEPRECATED tacosEstimatedMonthlySavings "Use generic-lens or generic-optics with 'estimatedMonthlySavings' instead." #-}

-- | The estimated percentage of savings that might be realized if the recommended operations are taken.
--
-- /Note:/ Consider using 'estimatedPercentMonthlySavings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacosEstimatedPercentMonthlySavings :: Lens.Lens' TrustedAdvisorCostOptimizingSummary Lude.Double
tacosEstimatedPercentMonthlySavings = Lens.lens (estimatedPercentMonthlySavings :: TrustedAdvisorCostOptimizingSummary -> Lude.Double) (\s a -> s {estimatedPercentMonthlySavings = a} :: TrustedAdvisorCostOptimizingSummary)
{-# DEPRECATED tacosEstimatedPercentMonthlySavings "Use generic-lens or generic-optics with 'estimatedPercentMonthlySavings' instead." #-}

instance Lude.FromJSON TrustedAdvisorCostOptimizingSummary where
  parseJSON =
    Lude.withObject
      "TrustedAdvisorCostOptimizingSummary"
      ( \x ->
          TrustedAdvisorCostOptimizingSummary'
            Lude.<$> (x Lude..: "estimatedMonthlySavings")
            Lude.<*> (x Lude..: "estimatedPercentMonthlySavings")
      )
