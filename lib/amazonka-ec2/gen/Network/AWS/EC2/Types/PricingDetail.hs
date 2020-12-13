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
import qualified Network.AWS.Prelude as Lude

-- | Describes a Reserved Instance offering.
--
-- /See:/ 'mkPricingDetail' smart constructor.
data PricingDetail = PricingDetail'
  { -- | The number of reservations available for the price.
    count :: Lude.Maybe Lude.Int,
    -- | The price per instance.
    price :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PricingDetail' with the minimum fields required to make a request.
--
-- * 'count' - The number of reservations available for the price.
-- * 'price' - The price per instance.
mkPricingDetail ::
  PricingDetail
mkPricingDetail =
  PricingDetail' {count = Lude.Nothing, price = Lude.Nothing}

-- | The number of reservations available for the price.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdCount :: Lens.Lens' PricingDetail (Lude.Maybe Lude.Int)
pdCount = Lens.lens (count :: PricingDetail -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: PricingDetail)
{-# DEPRECATED pdCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The price per instance.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPrice :: Lens.Lens' PricingDetail (Lude.Maybe Lude.Double)
pdPrice = Lens.lens (price :: PricingDetail -> Lude.Maybe Lude.Double) (\s a -> s {price = a} :: PricingDetail)
{-# DEPRECATED pdPrice "Use generic-lens or generic-optics with 'price' instead." #-}

instance Lude.FromXML PricingDetail where
  parseXML x =
    PricingDetail'
      Lude.<$> (x Lude..@? "count") Lude.<*> (x Lude..@? "price")
