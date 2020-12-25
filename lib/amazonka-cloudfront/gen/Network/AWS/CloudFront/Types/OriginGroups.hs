{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroups
  ( OriginGroups (..),

    -- * Smart constructor
    mkOriginGroups,

    -- * Lenses
    ogQuantity,
    ogItems,
  )
where

import qualified Network.AWS.CloudFront.Types.OriginGroup as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex data type for the origin groups specified for a distribution.
--
-- /See:/ 'mkOriginGroups' smart constructor.
data OriginGroups = OriginGroups'
  { -- | The number of origin groups.
    quantity :: Core.Int,
    -- | The items (origin groups) in a distribution.
    items :: Core.Maybe [Types.OriginGroup]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OriginGroups' value with any optional fields omitted.
mkOriginGroups ::
  -- | 'quantity'
  Core.Int ->
  OriginGroups
mkOriginGroups quantity =
  OriginGroups' {quantity, items = Core.Nothing}

-- | The number of origin groups.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogQuantity :: Lens.Lens' OriginGroups Core.Int
ogQuantity = Lens.field @"quantity"
{-# DEPRECATED ogQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | The items (origin groups) in a distribution.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogItems :: Lens.Lens' OriginGroups (Core.Maybe [Types.OriginGroup])
ogItems = Lens.field @"items"
{-# DEPRECATED ogItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML OriginGroups where
  toXML OriginGroups {..} =
    Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode
        "Items"
        (Core.toXMLList "OriginGroup" Core.<$> items)

instance Core.FromXML OriginGroups where
  parseXML x =
    OriginGroups'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> (x Core..@? "Items" Core..<@> Core.parseXMLList "OriginGroup")
