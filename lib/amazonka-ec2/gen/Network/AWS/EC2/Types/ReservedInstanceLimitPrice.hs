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

import Network.AWS.EC2.Types.CurrencyCodeValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the limit price of a Reserved Instance offering.
--
-- /See:/ 'mkReservedInstanceLimitPrice' smart constructor.
data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice'
  { -- | Used for Reserved Instance Marketplace offerings. Specifies the limit price on the total order (instanceCount * price).
    amount :: Lude.Maybe Lude.Double,
    -- | The currency in which the @limitPrice@ amount is specified. At this time, the only supported currency is @USD@ .
    currencyCode :: Lude.Maybe CurrencyCodeValues
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedInstanceLimitPrice' with the minimum fields required to make a request.
--
-- * 'amount' - Used for Reserved Instance Marketplace offerings. Specifies the limit price on the total order (instanceCount * price).
-- * 'currencyCode' - The currency in which the @limitPrice@ amount is specified. At this time, the only supported currency is @USD@ .
mkReservedInstanceLimitPrice ::
  ReservedInstanceLimitPrice
mkReservedInstanceLimitPrice =
  ReservedInstanceLimitPrice'
    { amount = Lude.Nothing,
      currencyCode = Lude.Nothing
    }

-- | Used for Reserved Instance Marketplace offerings. Specifies the limit price on the total order (instanceCount * price).
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilpAmount :: Lens.Lens' ReservedInstanceLimitPrice (Lude.Maybe Lude.Double)
rilpAmount = Lens.lens (amount :: ReservedInstanceLimitPrice -> Lude.Maybe Lude.Double) (\s a -> s {amount = a} :: ReservedInstanceLimitPrice)
{-# DEPRECATED rilpAmount "Use generic-lens or generic-optics with 'amount' instead." #-}

-- | The currency in which the @limitPrice@ amount is specified. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rilpCurrencyCode :: Lens.Lens' ReservedInstanceLimitPrice (Lude.Maybe CurrencyCodeValues)
rilpCurrencyCode = Lens.lens (currencyCode :: ReservedInstanceLimitPrice -> Lude.Maybe CurrencyCodeValues) (\s a -> s {currencyCode = a} :: ReservedInstanceLimitPrice)
{-# DEPRECATED rilpCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

instance Lude.ToQuery ReservedInstanceLimitPrice where
  toQuery ReservedInstanceLimitPrice' {..} =
    Lude.mconcat
      ["Amount" Lude.=: amount, "CurrencyCode" Lude.=: currencyCode]
