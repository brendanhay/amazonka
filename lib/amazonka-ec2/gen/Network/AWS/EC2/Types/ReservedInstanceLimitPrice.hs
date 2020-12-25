{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstanceLimitPrice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstanceLimitPrice
  ( ReservedInstanceLimitPrice (..),

    -- * Smart constructor
    mkReservedInstanceLimitPrice,

    -- * Lenses
    rilpAmount,
    rilpCurrencyCode,
  )
where

import qualified Network.AWS.EC2.Types.CurrencyCodeValues as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the limit price of a Reserved Instance offering.
--
-- /See:/ 'mkReservedInstanceLimitPrice' smart constructor.
data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice'
  { -- | Used for Reserved Instance Marketplace offerings. Specifies the limit price on the total order (instanceCount * price).
    amount :: Core.Maybe Core.Double,
    -- | The currency in which the @limitPrice@ amount is specified. At this time, the only supported currency is @USD@ .
    currencyCode :: Core.Maybe Types.CurrencyCodeValues
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservedInstanceLimitPrice' value with any optional fields omitted.
mkReservedInstanceLimitPrice ::
  ReservedInstanceLimitPrice
mkReservedInstanceLimitPrice =
  ReservedInstanceLimitPrice'
    { amount = Core.Nothing,
      currencyCode = Core.Nothing
    }

-- | Used for Reserved Instance Marketplace offerings. Specifies the limit price on the total order (instanceCount * price).
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilpAmount :: Lens.Lens' ReservedInstanceLimitPrice (Core.Maybe Core.Double)
rilpAmount = Lens.field @"amount"
{-# DEPRECATED rilpAmount "Use generic-lens or generic-optics with 'amount' instead." #-}

-- | The currency in which the @limitPrice@ amount is specified. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilpCurrencyCode :: Lens.Lens' ReservedInstanceLimitPrice (Core.Maybe Types.CurrencyCodeValues)
rilpCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED rilpCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}
