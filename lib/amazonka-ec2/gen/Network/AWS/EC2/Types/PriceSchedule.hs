{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PriceSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PriceSchedule
  ( PriceSchedule (..)
  -- * Smart constructor
  , mkPriceSchedule
  -- * Lenses
  , psActive
  , psCurrencyCode
  , psPrice
  , psTerm
  ) where

import qualified Network.AWS.EC2.Types.CurrencyCodeValues as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the price for a Reserved Instance.
--
-- /See:/ 'mkPriceSchedule' smart constructor.
data PriceSchedule = PriceSchedule'
  { active :: Core.Maybe Core.Bool
    -- ^ The current price schedule, as determined by the term remaining for the Reserved Instance in the listing.
--
-- A specific price schedule is always in effect, but only one price schedule can be active at any time. Take, for example, a Reserved Instance listing that has five months remaining in its term. When you specify price schedules for five months and two months, this means that schedule 1, covering the first three months of the remaining term, will be active during months 5, 4, and 3. Then schedule 2, covering the last two months of the term, will be active for months 2 and 1.
  , currencyCode :: Core.Maybe Types.CurrencyCodeValues
    -- ^ The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
  , price :: Core.Maybe Core.Double
    -- ^ The fixed price for the term.
  , term :: Core.Maybe Core.Integer
    -- ^ The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PriceSchedule' value with any optional fields omitted.
mkPriceSchedule
    :: PriceSchedule
mkPriceSchedule
  = PriceSchedule'{active = Core.Nothing,
                   currencyCode = Core.Nothing, price = Core.Nothing,
                   term = Core.Nothing}

-- | The current price schedule, as determined by the term remaining for the Reserved Instance in the listing.
--
-- A specific price schedule is always in effect, but only one price schedule can be active at any time. Take, for example, a Reserved Instance listing that has five months remaining in its term. When you specify price schedules for five months and two months, this means that schedule 1, covering the first three months of the remaining term, will be active during months 5, 4, and 3. Then schedule 2, covering the last two months of the term, will be active for months 2 and 1.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psActive :: Lens.Lens' PriceSchedule (Core.Maybe Core.Bool)
psActive = Lens.field @"active"
{-# INLINEABLE psActive #-}
{-# DEPRECATED active "Use generic-lens or generic-optics with 'active' instead"  #-}

-- | The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psCurrencyCode :: Lens.Lens' PriceSchedule (Core.Maybe Types.CurrencyCodeValues)
psCurrencyCode = Lens.field @"currencyCode"
{-# INLINEABLE psCurrencyCode #-}
{-# DEPRECATED currencyCode "Use generic-lens or generic-optics with 'currencyCode' instead"  #-}

-- | The fixed price for the term.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPrice :: Lens.Lens' PriceSchedule (Core.Maybe Core.Double)
psPrice = Lens.field @"price"
{-# INLINEABLE psPrice #-}
{-# DEPRECATED price "Use generic-lens or generic-optics with 'price' instead"  #-}

-- | The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
--
-- /Note:/ Consider using 'term' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psTerm :: Lens.Lens' PriceSchedule (Core.Maybe Core.Integer)
psTerm = Lens.field @"term"
{-# INLINEABLE psTerm #-}
{-# DEPRECATED term "Use generic-lens or generic-optics with 'term' instead"  #-}

instance Core.FromXML PriceSchedule where
        parseXML x
          = PriceSchedule' Core.<$>
              (x Core..@? "active") Core.<*> x Core..@? "currencyCode" Core.<*>
                x Core..@? "price"
                Core.<*> x Core..@? "term"
