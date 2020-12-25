{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CacheBehaviors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CacheBehaviors
  ( CacheBehaviors (..),

    -- * Smart constructor
    mkCacheBehaviors,

    -- * Lenses
    cbQuantity,
    cbItems,
  )
where

import qualified Network.AWS.CloudFront.Types.CacheBehavior as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that contains zero or more @CacheBehavior@ elements.
--
-- /See:/ 'mkCacheBehaviors' smart constructor.
data CacheBehaviors = CacheBehaviors'
  { -- | The number of cache behaviors for this distribution.
    quantity :: Core.Int,
    -- | Optional: A complex type that contains cache behaviors for this distribution. If @Quantity@ is @0@ , you can omit @Items@ .
    items :: Core.Maybe [Types.CacheBehavior]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CacheBehaviors' value with any optional fields omitted.
mkCacheBehaviors ::
  -- | 'quantity'
  Core.Int ->
  CacheBehaviors
mkCacheBehaviors quantity =
  CacheBehaviors' {quantity, items = Core.Nothing}

-- | The number of cache behaviors for this distribution.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbQuantity :: Lens.Lens' CacheBehaviors Core.Int
cbQuantity = Lens.field @"quantity"
{-# DEPRECATED cbQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | Optional: A complex type that contains cache behaviors for this distribution. If @Quantity@ is @0@ , you can omit @Items@ .
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbItems :: Lens.Lens' CacheBehaviors (Core.Maybe [Types.CacheBehavior])
cbItems = Lens.field @"items"
{-# DEPRECATED cbItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML CacheBehaviors where
  toXML CacheBehaviors {..} =
    Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode
        "Items"
        (Core.toXMLList "CacheBehavior" Core.<$> items)

instance Core.FromXML CacheBehaviors where
  parseXML x =
    CacheBehaviors'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> (x Core..@? "Items" Core..<@> Core.parseXMLList "CacheBehavior")
