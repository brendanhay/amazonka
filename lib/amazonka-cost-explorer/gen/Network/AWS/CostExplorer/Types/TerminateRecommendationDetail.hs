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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details on termination recommendation.
--
-- /See:/ 'mkTerminateRecommendationDetail' smart constructor.
data TerminateRecommendationDetail = TerminateRecommendationDetail'
  { currencyCode ::
      Lude.Maybe Lude.Text,
    estimatedMonthlySavings ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateRecommendationDetail' with the minimum fields required to make a request.
--
-- * 'currencyCode' - The currency code that AWS used to calculate the costs for this instance.
-- * 'estimatedMonthlySavings' - Estimated savings resulting from modification, on a monthly basis.
mkTerminateRecommendationDetail ::
  TerminateRecommendationDetail
mkTerminateRecommendationDetail =
  TerminateRecommendationDetail'
    { currencyCode = Lude.Nothing,
      estimatedMonthlySavings = Lude.Nothing
    }

-- | The currency code that AWS used to calculate the costs for this instance.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdCurrencyCode :: Lens.Lens' TerminateRecommendationDetail (Lude.Maybe Lude.Text)
trdCurrencyCode = Lens.lens (currencyCode :: TerminateRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: TerminateRecommendationDetail)
{-# DEPRECATED trdCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Estimated savings resulting from modification, on a monthly basis.
--
-- /Note:/ Consider using 'estimatedMonthlySavings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdEstimatedMonthlySavings :: Lens.Lens' TerminateRecommendationDetail (Lude.Maybe Lude.Text)
trdEstimatedMonthlySavings = Lens.lens (estimatedMonthlySavings :: TerminateRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {estimatedMonthlySavings = a} :: TerminateRecommendationDetail)
{-# DEPRECATED trdEstimatedMonthlySavings "Use generic-lens or generic-optics with 'estimatedMonthlySavings' instead." #-}

instance Lude.FromJSON TerminateRecommendationDetail where
  parseJSON =
    Lude.withObject
      "TerminateRecommendationDetail"
      ( \x ->
          TerminateRecommendationDetail'
            Lude.<$> (x Lude..:? "CurrencyCode")
            Lude.<*> (x Lude..:? "EstimatedMonthlySavings")
      )
