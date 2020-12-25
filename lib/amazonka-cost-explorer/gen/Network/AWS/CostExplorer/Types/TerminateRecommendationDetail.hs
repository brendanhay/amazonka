{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.TerminateRecommendationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TerminateRecommendationDetail
  ( TerminateRecommendationDetail (..),

    -- * Smart constructor
    mkTerminateRecommendationDetail,

    -- * Lenses
    trdCurrencyCode,
    trdEstimatedMonthlySavings,
  )
where

import qualified Network.AWS.CostExplorer.Types.GenericString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details on termination recommendation.
--
-- /See:/ 'mkTerminateRecommendationDetail' smart constructor.
data TerminateRecommendationDetail = TerminateRecommendationDetail'
  { -- | The currency code that AWS used to calculate the costs for this instance.
    currencyCode :: Core.Maybe Types.GenericString,
    -- | Estimated savings resulting from modification, on a monthly basis.
    estimatedMonthlySavings :: Core.Maybe Types.GenericString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateRecommendationDetail' value with any optional fields omitted.
mkTerminateRecommendationDetail ::
  TerminateRecommendationDetail
mkTerminateRecommendationDetail =
  TerminateRecommendationDetail'
    { currencyCode = Core.Nothing,
      estimatedMonthlySavings = Core.Nothing
    }

-- | The currency code that AWS used to calculate the costs for this instance.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdCurrencyCode :: Lens.Lens' TerminateRecommendationDetail (Core.Maybe Types.GenericString)
trdCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED trdCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Estimated savings resulting from modification, on a monthly basis.
--
-- /Note:/ Consider using 'estimatedMonthlySavings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdEstimatedMonthlySavings :: Lens.Lens' TerminateRecommendationDetail (Core.Maybe Types.GenericString)
trdEstimatedMonthlySavings = Lens.field @"estimatedMonthlySavings"
{-# DEPRECATED trdEstimatedMonthlySavings "Use generic-lens or generic-optics with 'estimatedMonthlySavings' instead." #-}

instance Core.FromJSON TerminateRecommendationDetail where
  parseJSON =
    Core.withObject "TerminateRecommendationDetail" Core.$
      \x ->
        TerminateRecommendationDetail'
          Core.<$> (x Core..:? "CurrencyCode")
          Core.<*> (x Core..:? "EstimatedMonthlySavings")
