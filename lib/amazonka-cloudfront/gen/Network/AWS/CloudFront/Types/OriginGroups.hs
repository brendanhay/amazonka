{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.OriginGroups
  ( OriginGroups (..)
  -- * Smart constructor
  , mkOriginGroups
  -- * Lenses
  , ogQuantity
  , ogItems
  ) where

import qualified Network.AWS.CloudFront.Types.OriginGroup as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex data type for the origin groups specified for a distribution.
--
-- /See:/ 'mkOriginGroups' smart constructor.
data OriginGroups = OriginGroups'
  { quantity :: Core.Int
    -- ^ The number of origin groups.
  , items :: Core.Maybe [Types.OriginGroup]
    -- ^ The items (origin groups) in a distribution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OriginGroups' value with any optional fields omitted.
mkOriginGroups
    :: Core.Int -- ^ 'quantity'
    -> OriginGroups
mkOriginGroups quantity
  = OriginGroups'{quantity, items = Core.Nothing}

-- | The number of origin groups.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogQuantity :: Lens.Lens' OriginGroups Core.Int
ogQuantity = Lens.field @"quantity"
{-# INLINEABLE ogQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | The items (origin groups) in a distribution.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogItems :: Lens.Lens' OriginGroups (Core.Maybe [Types.OriginGroup])
ogItems = Lens.field @"items"
{-# INLINEABLE ogItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.ToXML OriginGroups where
        toXML OriginGroups{..}
          = Core.toXMLElement "Quantity" quantity Core.<>
              Core.toXMLElement "Items"
                (Core.maybe Core.mempty (Core.toXMLList "OriginGroup") items)

instance Core.FromXML OriginGroups where
        parseXML x
          = OriginGroups' Core.<$>
              (x Core..@ "Quantity") Core.<*>
                x Core..@? "Items" Core..<@> Core.parseXMLList "OriginGroup"
