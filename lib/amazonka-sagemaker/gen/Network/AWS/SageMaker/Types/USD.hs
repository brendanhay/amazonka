{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core

-- | Represents an amount of money in United States dollars/
--
-- /See:/ 'mkUSD' smart constructor.
data USD = USD'
  { -- | The fractional portion, in cents, of the amount.
    cents :: Core.Maybe Core.Natural,
    -- | The whole number of dollars in the amount.
    dollars :: Core.Maybe Core.Natural,
    -- | Fractions of a cent, in tenths.
    tenthFractionsOfACent :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'USD' value with any optional fields omitted.
mkUSD ::
  USD
mkUSD =
  USD'
    { cents = Core.Nothing,
      dollars = Core.Nothing,
      tenthFractionsOfACent = Core.Nothing
    }

-- | The fractional portion, in cents, of the amount.
--
-- /Note:/ Consider using 'cents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdCents :: Lens.Lens' USD (Core.Maybe Core.Natural)
usdCents = Lens.field @"cents"
{-# DEPRECATED usdCents "Use generic-lens or generic-optics with 'cents' instead." #-}

-- | The whole number of dollars in the amount.
--
-- /Note:/ Consider using 'dollars' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdDollars :: Lens.Lens' USD (Core.Maybe Core.Natural)
usdDollars = Lens.field @"dollars"
{-# DEPRECATED usdDollars "Use generic-lens or generic-optics with 'dollars' instead." #-}

-- | Fractions of a cent, in tenths.
--
-- /Note:/ Consider using 'tenthFractionsOfACent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdTenthFractionsOfACent :: Lens.Lens' USD (Core.Maybe Core.Natural)
usdTenthFractionsOfACent = Lens.field @"tenthFractionsOfACent"
{-# DEPRECATED usdTenthFractionsOfACent "Use generic-lens or generic-optics with 'tenthFractionsOfACent' instead." #-}

instance Core.FromJSON USD where
  toJSON USD {..} =
    Core.object
      ( Core.catMaybes
          [ ("Cents" Core..=) Core.<$> cents,
            ("Dollars" Core..=) Core.<$> dollars,
            ("TenthFractionsOfACent" Core..=) Core.<$> tenthFractionsOfACent
          ]
      )

instance Core.FromJSON USD where
  parseJSON =
    Core.withObject "USD" Core.$
      \x ->
        USD'
          Core.<$> (x Core..:? "Cents")
          Core.<*> (x Core..:? "Dollars")
          Core.<*> (x Core..:? "TenthFractionsOfACent")
