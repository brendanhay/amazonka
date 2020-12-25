{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.MonetaryAmount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.MonetaryAmount
  ( MonetaryAmount (..),

    -- * Smart constructor
    mkMonetaryAmount,

    -- * Lenses
    maAmount,
    maCurrencyCode,
  )
where

import qualified Network.AWS.DeviceFarm.Types.CurrencyCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A number that represents the monetary amount for an offering or transaction.
--
-- /See:/ 'mkMonetaryAmount' smart constructor.
data MonetaryAmount = MonetaryAmount'
  { -- | The numerical amount of an offering or transaction.
    amount :: Core.Maybe Core.Double,
    -- | The currency code of a monetary amount. For example, @USD@ means U.S. dollars.
    currencyCode :: Core.Maybe Types.CurrencyCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MonetaryAmount' value with any optional fields omitted.
mkMonetaryAmount ::
  MonetaryAmount
mkMonetaryAmount =
  MonetaryAmount'
    { amount = Core.Nothing,
      currencyCode = Core.Nothing
    }

-- | The numerical amount of an offering or transaction.
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maAmount :: Lens.Lens' MonetaryAmount (Core.Maybe Core.Double)
maAmount = Lens.field @"amount"
{-# DEPRECATED maAmount "Use generic-lens or generic-optics with 'amount' instead." #-}

-- | The currency code of a monetary amount. For example, @USD@ means U.S. dollars.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maCurrencyCode :: Lens.Lens' MonetaryAmount (Core.Maybe Types.CurrencyCode)
maCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED maCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

instance Core.FromJSON MonetaryAmount where
  parseJSON =
    Core.withObject "MonetaryAmount" Core.$
      \x ->
        MonetaryAmount'
          Core.<$> (x Core..:? "amount") Core.<*> (x Core..:? "currencyCode")
