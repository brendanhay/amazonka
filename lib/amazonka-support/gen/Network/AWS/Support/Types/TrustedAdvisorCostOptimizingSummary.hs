{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCostOptimizingSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Support.Types.TrustedAdvisorCostOptimizingSummary
  ( TrustedAdvisorCostOptimizingSummary (..)
  -- * Smart constructor
  , mkTrustedAdvisorCostOptimizingSummary
  -- * Lenses
  , tacosEstimatedMonthlySavings
  , tacosEstimatedPercentMonthlySavings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The estimated cost savings that might be realized if the recommended operations are taken.
--
-- /See:/ 'mkTrustedAdvisorCostOptimizingSummary' smart constructor.
data TrustedAdvisorCostOptimizingSummary = TrustedAdvisorCostOptimizingSummary'
  { estimatedMonthlySavings :: Core.Double
    -- ^ The estimated monthly savings that might be realized if the recommended operations are taken.
  , estimatedPercentMonthlySavings :: Core.Double
    -- ^ The estimated percentage of savings that might be realized if the recommended operations are taken.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrustedAdvisorCostOptimizingSummary' value with any optional fields omitted.
mkTrustedAdvisorCostOptimizingSummary
    :: Core.Double -- ^ 'estimatedMonthlySavings'
    -> Core.Double -- ^ 'estimatedPercentMonthlySavings'
    -> TrustedAdvisorCostOptimizingSummary
mkTrustedAdvisorCostOptimizingSummary estimatedMonthlySavings
  estimatedPercentMonthlySavings
  = TrustedAdvisorCostOptimizingSummary'{estimatedMonthlySavings,
                                         estimatedPercentMonthlySavings}

-- | The estimated monthly savings that might be realized if the recommended operations are taken.
--
-- /Note:/ Consider using 'estimatedMonthlySavings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacosEstimatedMonthlySavings :: Lens.Lens' TrustedAdvisorCostOptimizingSummary Core.Double
tacosEstimatedMonthlySavings = Lens.field @"estimatedMonthlySavings"
{-# INLINEABLE tacosEstimatedMonthlySavings #-}
{-# DEPRECATED estimatedMonthlySavings "Use generic-lens or generic-optics with 'estimatedMonthlySavings' instead"  #-}

-- | The estimated percentage of savings that might be realized if the recommended operations are taken.
--
-- /Note:/ Consider using 'estimatedPercentMonthlySavings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacosEstimatedPercentMonthlySavings :: Lens.Lens' TrustedAdvisorCostOptimizingSummary Core.Double
tacosEstimatedPercentMonthlySavings = Lens.field @"estimatedPercentMonthlySavings"
{-# INLINEABLE tacosEstimatedPercentMonthlySavings #-}
{-# DEPRECATED estimatedPercentMonthlySavings "Use generic-lens or generic-optics with 'estimatedPercentMonthlySavings' instead"  #-}

instance Core.FromJSON TrustedAdvisorCostOptimizingSummary where
        parseJSON
          = Core.withObject "TrustedAdvisorCostOptimizingSummary" Core.$
              \ x ->
                TrustedAdvisorCostOptimizingSummary' Core.<$>
                  (x Core..: "estimatedMonthlySavings") Core.<*>
                    x Core..: "estimatedPercentMonthlySavings"
