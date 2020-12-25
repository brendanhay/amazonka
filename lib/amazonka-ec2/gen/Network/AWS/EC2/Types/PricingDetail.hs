{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PricingDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PricingDetail
  ( PricingDetail (..),

    -- * Smart constructor
    mkPricingDetail,

    -- * Lenses
    pdCount,
    pdPrice,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Reserved Instance offering.
--
-- /See:/ 'mkPricingDetail' smart constructor.
data PricingDetail = PricingDetail'
  { -- | The number of reservations available for the price.
    count :: Core.Maybe Core.Int,
    -- | The price per instance.
    price :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PricingDetail' value with any optional fields omitted.
mkPricingDetail ::
  PricingDetail
mkPricingDetail =
  PricingDetail' {count = Core.Nothing, price = Core.Nothing}

-- | The number of reservations available for the price.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdCount :: Lens.Lens' PricingDetail (Core.Maybe Core.Int)
pdCount = Lens.field @"count"
{-# DEPRECATED pdCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The price per instance.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPrice :: Lens.Lens' PricingDetail (Core.Maybe Core.Double)
pdPrice = Lens.field @"price"
{-# DEPRECATED pdPrice "Use generic-lens or generic-optics with 'price' instead." #-}

instance Core.FromXML PricingDetail where
  parseXML x =
    PricingDetail'
      Core.<$> (x Core..@? "count") Core.<*> (x Core..@? "price")
