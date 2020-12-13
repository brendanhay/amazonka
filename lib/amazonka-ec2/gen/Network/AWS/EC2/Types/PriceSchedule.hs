{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PriceSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PriceSchedule
  ( PriceSchedule (..),

    -- * Smart constructor
    mkPriceSchedule,

    -- * Lenses
    psCurrencyCode,
    psTerm,
    psActive,
    psPrice,
  )
where

import Network.AWS.EC2.Types.CurrencyCodeValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the price for a Reserved Instance.
--
-- /See:/ 'mkPriceSchedule' smart constructor.
data PriceSchedule = PriceSchedule'
  { -- | The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
    currencyCode :: Lude.Maybe CurrencyCodeValues,
    -- | The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
    term :: Lude.Maybe Lude.Integer,
    -- | The current price schedule, as determined by the term remaining for the Reserved Instance in the listing.
    --
    -- A specific price schedule is always in effect, but only one price schedule can be active at any time. Take, for example, a Reserved Instance listing that has five months remaining in its term. When you specify price schedules for five months and two months, this means that schedule 1, covering the first three months of the remaining term, will be active during months 5, 4, and 3. Then schedule 2, covering the last two months of the term, will be active for months 2 and 1.
    active :: Lude.Maybe Lude.Bool,
    -- | The fixed price for the term.
    price :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PriceSchedule' with the minimum fields required to make a request.
--
-- * 'currencyCode' - The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
-- * 'term' - The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
-- * 'active' - The current price schedule, as determined by the term remaining for the Reserved Instance in the listing.
--
-- A specific price schedule is always in effect, but only one price schedule can be active at any time. Take, for example, a Reserved Instance listing that has five months remaining in its term. When you specify price schedules for five months and two months, this means that schedule 1, covering the first three months of the remaining term, will be active during months 5, 4, and 3. Then schedule 2, covering the last two months of the term, will be active for months 2 and 1.
-- * 'price' - The fixed price for the term.
mkPriceSchedule ::
  PriceSchedule
mkPriceSchedule =
  PriceSchedule'
    { currencyCode = Lude.Nothing,
      term = Lude.Nothing,
      active = Lude.Nothing,
      price = Lude.Nothing
    }

-- | The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psCurrencyCode :: Lens.Lens' PriceSchedule (Lude.Maybe CurrencyCodeValues)
psCurrencyCode = Lens.lens (currencyCode :: PriceSchedule -> Lude.Maybe CurrencyCodeValues) (\s a -> s {currencyCode = a} :: PriceSchedule)
{-# DEPRECATED psCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
--
-- /Note:/ Consider using 'term' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psTerm :: Lens.Lens' PriceSchedule (Lude.Maybe Lude.Integer)
psTerm = Lens.lens (term :: PriceSchedule -> Lude.Maybe Lude.Integer) (\s a -> s {term = a} :: PriceSchedule)
{-# DEPRECATED psTerm "Use generic-lens or generic-optics with 'term' instead." #-}

-- | The current price schedule, as determined by the term remaining for the Reserved Instance in the listing.
--
-- A specific price schedule is always in effect, but only one price schedule can be active at any time. Take, for example, a Reserved Instance listing that has five months remaining in its term. When you specify price schedules for five months and two months, this means that schedule 1, covering the first three months of the remaining term, will be active during months 5, 4, and 3. Then schedule 2, covering the last two months of the term, will be active for months 2 and 1.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psActive :: Lens.Lens' PriceSchedule (Lude.Maybe Lude.Bool)
psActive = Lens.lens (active :: PriceSchedule -> Lude.Maybe Lude.Bool) (\s a -> s {active = a} :: PriceSchedule)
{-# DEPRECATED psActive "Use generic-lens or generic-optics with 'active' instead." #-}

-- | The fixed price for the term.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPrice :: Lens.Lens' PriceSchedule (Lude.Maybe Lude.Double)
psPrice = Lens.lens (price :: PriceSchedule -> Lude.Maybe Lude.Double) (\s a -> s {price = a} :: PriceSchedule)
{-# DEPRECATED psPrice "Use generic-lens or generic-optics with 'price' instead." #-}

instance Lude.FromXML PriceSchedule where
  parseXML x =
    PriceSchedule'
      Lude.<$> (x Lude..@? "currencyCode")
      Lude.<*> (x Lude..@? "term")
      Lude.<*> (x Lude..@? "active")
      Lude.<*> (x Lude..@? "price")
