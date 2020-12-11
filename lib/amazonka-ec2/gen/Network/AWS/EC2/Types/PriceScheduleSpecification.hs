-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PriceScheduleSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PriceScheduleSpecification
  ( PriceScheduleSpecification (..),

    -- * Smart constructor
    mkPriceScheduleSpecification,

    -- * Lenses
    pssCurrencyCode,
    pssTerm,
    pssPrice,
  )
where

import Network.AWS.EC2.Types.CurrencyCodeValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the price for a Reserved Instance.
--
-- /See:/ 'mkPriceScheduleSpecification' smart constructor.
data PriceScheduleSpecification = PriceScheduleSpecification'
  { currencyCode ::
      Lude.Maybe CurrencyCodeValues,
    term :: Lude.Maybe Lude.Integer,
    price :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PriceScheduleSpecification' with the minimum fields required to make a request.
--
-- * 'currencyCode' - The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
-- * 'price' - The fixed price for the term.
-- * 'term' - The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
mkPriceScheduleSpecification ::
  PriceScheduleSpecification
mkPriceScheduleSpecification =
  PriceScheduleSpecification'
    { currencyCode = Lude.Nothing,
      term = Lude.Nothing,
      price = Lude.Nothing
    }

-- | The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pssCurrencyCode :: Lens.Lens' PriceScheduleSpecification (Lude.Maybe CurrencyCodeValues)
pssCurrencyCode = Lens.lens (currencyCode :: PriceScheduleSpecification -> Lude.Maybe CurrencyCodeValues) (\s a -> s {currencyCode = a} :: PriceScheduleSpecification)
{-# DEPRECATED pssCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
--
-- /Note:/ Consider using 'term' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pssTerm :: Lens.Lens' PriceScheduleSpecification (Lude.Maybe Lude.Integer)
pssTerm = Lens.lens (term :: PriceScheduleSpecification -> Lude.Maybe Lude.Integer) (\s a -> s {term = a} :: PriceScheduleSpecification)
{-# DEPRECATED pssTerm "Use generic-lens or generic-optics with 'term' instead." #-}

-- | The fixed price for the term.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pssPrice :: Lens.Lens' PriceScheduleSpecification (Lude.Maybe Lude.Double)
pssPrice = Lens.lens (price :: PriceScheduleSpecification -> Lude.Maybe Lude.Double) (\s a -> s {price = a} :: PriceScheduleSpecification)
{-# DEPRECATED pssPrice "Use generic-lens or generic-optics with 'price' instead." #-}

instance Lude.ToQuery PriceScheduleSpecification where
  toQuery PriceScheduleSpecification' {..} =
    Lude.mconcat
      [ "CurrencyCode" Lude.=: currencyCode,
        "Term" Lude.=: term,
        "Price" Lude.=: price
      ]
