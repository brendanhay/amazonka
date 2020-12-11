-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.CalculatedSpend
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.CalculatedSpend
  ( CalculatedSpend (..),

    -- * Smart constructor
    mkCalculatedSpend,

    -- * Lenses
    csForecastedSpend,
    csActualSpend,
  )
where

import Network.AWS.Budgets.Types.Spend
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The spend objects that are associated with this budget. The @actualSpend@ tracks how much you've used, cost, usage, RI units, or Savings Plans units and the @forecastedSpend@ tracks how much you are predicted to spend based on your historical usage profile.
--
-- For example, if it is the 20th of the month and you have spent @50@ dollars on Amazon EC2, your @actualSpend@ is @50 USD@ , and your @forecastedSpend@ is @75 USD@ .
--
-- /See:/ 'mkCalculatedSpend' smart constructor.
data CalculatedSpend = CalculatedSpend'
  { forecastedSpend ::
      Lude.Maybe Spend,
    actualSpend :: Spend
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CalculatedSpend' with the minimum fields required to make a request.
--
-- * 'actualSpend' - The amount of cost, usage, RI units, or Savings Plans units that you have used.
-- * 'forecastedSpend' - The amount of cost, usage, RI units, or Savings Plans units that you are forecasted to use.
mkCalculatedSpend ::
  -- | 'actualSpend'
  Spend ->
  CalculatedSpend
mkCalculatedSpend pActualSpend_ =
  CalculatedSpend'
    { forecastedSpend = Lude.Nothing,
      actualSpend = pActualSpend_
    }

-- | The amount of cost, usage, RI units, or Savings Plans units that you are forecasted to use.
--
-- /Note:/ Consider using 'forecastedSpend' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csForecastedSpend :: Lens.Lens' CalculatedSpend (Lude.Maybe Spend)
csForecastedSpend = Lens.lens (forecastedSpend :: CalculatedSpend -> Lude.Maybe Spend) (\s a -> s {forecastedSpend = a} :: CalculatedSpend)
{-# DEPRECATED csForecastedSpend "Use generic-lens or generic-optics with 'forecastedSpend' instead." #-}

-- | The amount of cost, usage, RI units, or Savings Plans units that you have used.
--
-- /Note:/ Consider using 'actualSpend' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csActualSpend :: Lens.Lens' CalculatedSpend Spend
csActualSpend = Lens.lens (actualSpend :: CalculatedSpend -> Spend) (\s a -> s {actualSpend = a} :: CalculatedSpend)
{-# DEPRECATED csActualSpend "Use generic-lens or generic-optics with 'actualSpend' instead." #-}

instance Lude.FromJSON CalculatedSpend where
  parseJSON =
    Lude.withObject
      "CalculatedSpend"
      ( \x ->
          CalculatedSpend'
            Lude.<$> (x Lude..:? "ForecastedSpend") Lude.<*> (x Lude..: "ActualSpend")
      )

instance Lude.ToJSON CalculatedSpend where
  toJSON CalculatedSpend' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ForecastedSpend" Lude..=) Lude.<$> forecastedSpend,
            Lude.Just ("ActualSpend" Lude..= actualSpend)
          ]
      )
