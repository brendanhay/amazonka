-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.USD
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.USD
  ( USD (..),

    -- * Smart constructor
    mkUSD,

    -- * Lenses
    usdCents,
    usdDollars,
    usdTenthFractionsOfACent,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an amount of money in United States dollars/
--
-- /See:/ 'mkUSD' smart constructor.
data USD = USD'
  { cents :: Lude.Maybe Lude.Natural,
    dollars :: Lude.Maybe Lude.Natural,
    tenthFractionsOfACent :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'USD' with the minimum fields required to make a request.
--
-- * 'cents' - The fractional portion, in cents, of the amount.
-- * 'dollars' - The whole number of dollars in the amount.
-- * 'tenthFractionsOfACent' - Fractions of a cent, in tenths.
mkUSD ::
  USD
mkUSD =
  USD'
    { cents = Lude.Nothing,
      dollars = Lude.Nothing,
      tenthFractionsOfACent = Lude.Nothing
    }

-- | The fractional portion, in cents, of the amount.
--
-- /Note:/ Consider using 'cents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdCents :: Lens.Lens' USD (Lude.Maybe Lude.Natural)
usdCents = Lens.lens (cents :: USD -> Lude.Maybe Lude.Natural) (\s a -> s {cents = a} :: USD)
{-# DEPRECATED usdCents "Use generic-lens or generic-optics with 'cents' instead." #-}

-- | The whole number of dollars in the amount.
--
-- /Note:/ Consider using 'dollars' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdDollars :: Lens.Lens' USD (Lude.Maybe Lude.Natural)
usdDollars = Lens.lens (dollars :: USD -> Lude.Maybe Lude.Natural) (\s a -> s {dollars = a} :: USD)
{-# DEPRECATED usdDollars "Use generic-lens or generic-optics with 'dollars' instead." #-}

-- | Fractions of a cent, in tenths.
--
-- /Note:/ Consider using 'tenthFractionsOfACent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdTenthFractionsOfACent :: Lens.Lens' USD (Lude.Maybe Lude.Natural)
usdTenthFractionsOfACent = Lens.lens (tenthFractionsOfACent :: USD -> Lude.Maybe Lude.Natural) (\s a -> s {tenthFractionsOfACent = a} :: USD)
{-# DEPRECATED usdTenthFractionsOfACent "Use generic-lens or generic-optics with 'tenthFractionsOfACent' instead." #-}

instance Lude.FromJSON USD where
  parseJSON =
    Lude.withObject
      "USD"
      ( \x ->
          USD'
            Lude.<$> (x Lude..:? "Cents")
            Lude.<*> (x Lude..:? "Dollars")
            Lude.<*> (x Lude..:? "TenthFractionsOfACent")
      )

instance Lude.ToJSON USD where
  toJSON USD' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Cents" Lude..=) Lude.<$> cents,
            ("Dollars" Lude..=) Lude.<$> dollars,
            ("TenthFractionsOfACent" Lude..=) Lude.<$> tenthFractionsOfACent
          ]
      )
