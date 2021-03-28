{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PriceScheduleSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PriceScheduleSpecification
  ( PriceScheduleSpecification (..)
  -- * Smart constructor
  , mkPriceScheduleSpecification
  -- * Lenses
  , pssCurrencyCode
  , pssPrice
  , pssTerm
  ) where

import qualified Network.AWS.EC2.Types.CurrencyCodeValues as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the price for a Reserved Instance.
--
-- /See:/ 'mkPriceScheduleSpecification' smart constructor.
data PriceScheduleSpecification = PriceScheduleSpecification'
  { currencyCode :: Core.Maybe Types.CurrencyCodeValues
    -- ^ The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
  , price :: Core.Maybe Core.Double
    -- ^ The fixed price for the term.
  , term :: Core.Maybe Core.Integer
    -- ^ The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PriceScheduleSpecification' value with any optional fields omitted.
mkPriceScheduleSpecification
    :: PriceScheduleSpecification
mkPriceScheduleSpecification
  = PriceScheduleSpecification'{currencyCode = Core.Nothing,
                                price = Core.Nothing, term = Core.Nothing}

-- | The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pssCurrencyCode :: Lens.Lens' PriceScheduleSpecification (Core.Maybe Types.CurrencyCodeValues)
pssCurrencyCode = Lens.field @"currencyCode"
{-# INLINEABLE pssCurrencyCode #-}
{-# DEPRECATED currencyCode "Use generic-lens or generic-optics with 'currencyCode' instead"  #-}

-- | The fixed price for the term.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pssPrice :: Lens.Lens' PriceScheduleSpecification (Core.Maybe Core.Double)
pssPrice = Lens.field @"price"
{-# INLINEABLE pssPrice #-}
{-# DEPRECATED price "Use generic-lens or generic-optics with 'price' instead"  #-}

-- | The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
--
-- /Note:/ Consider using 'term' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pssTerm :: Lens.Lens' PriceScheduleSpecification (Core.Maybe Core.Integer)
pssTerm = Lens.field @"term"
{-# INLINEABLE pssTerm #-}
{-# DEPRECATED term "Use generic-lens or generic-optics with 'term' instead"  #-}

instance Core.ToQuery PriceScheduleSpecification where
        toQuery PriceScheduleSpecification{..}
          = Core.maybe Core.mempty (Core.toQueryPair "CurrencyCode")
              currencyCode
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Price") price
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Term") term
