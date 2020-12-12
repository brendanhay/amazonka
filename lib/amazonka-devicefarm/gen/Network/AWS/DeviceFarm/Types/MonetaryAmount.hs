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

import Network.AWS.DeviceFarm.Types.CurrencyCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A number that represents the monetary amount for an offering or transaction.
--
-- /See:/ 'mkMonetaryAmount' smart constructor.
data MonetaryAmount = MonetaryAmount'
  { amount ::
      Lude.Maybe Lude.Double,
    currencyCode :: Lude.Maybe CurrencyCode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonetaryAmount' with the minimum fields required to make a request.
--
-- * 'amount' - The numerical amount of an offering or transaction.
-- * 'currencyCode' - The currency code of a monetary amount. For example, @USD@ means U.S. dollars.
mkMonetaryAmount ::
  MonetaryAmount
mkMonetaryAmount =
  MonetaryAmount'
    { amount = Lude.Nothing,
      currencyCode = Lude.Nothing
    }

-- | The numerical amount of an offering or transaction.
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maAmount :: Lens.Lens' MonetaryAmount (Lude.Maybe Lude.Double)
maAmount = Lens.lens (amount :: MonetaryAmount -> Lude.Maybe Lude.Double) (\s a -> s {amount = a} :: MonetaryAmount)
{-# DEPRECATED maAmount "Use generic-lens or generic-optics with 'amount' instead." #-}

-- | The currency code of a monetary amount. For example, @USD@ means U.S. dollars.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maCurrencyCode :: Lens.Lens' MonetaryAmount (Lude.Maybe CurrencyCode)
maCurrencyCode = Lens.lens (currencyCode :: MonetaryAmount -> Lude.Maybe CurrencyCode) (\s a -> s {currencyCode = a} :: MonetaryAmount)
{-# DEPRECATED maCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

instance Lude.FromJSON MonetaryAmount where
  parseJSON =
    Lude.withObject
      "MonetaryAmount"
      ( \x ->
          MonetaryAmount'
            Lude.<$> (x Lude..:? "amount") Lude.<*> (x Lude..:? "currencyCode")
      )
