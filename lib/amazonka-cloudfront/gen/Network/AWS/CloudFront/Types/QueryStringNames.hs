{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryStringNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.QueryStringNames
  ( QueryStringNames (..)
  -- * Smart constructor
  , mkQueryStringNames
  -- * Lenses
  , qsnQuantity
  , qsnItems
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains a list of query string names.
--
-- /See:/ 'mkQueryStringNames' smart constructor.
data QueryStringNames = QueryStringNames'
  { quantity :: Core.Int
    -- ^ The number of query string names in the @Items@ list.
  , items :: Core.Maybe [Core.Text]
    -- ^ A list of query string names.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryStringNames' value with any optional fields omitted.
mkQueryStringNames
    :: Core.Int -- ^ 'quantity'
    -> QueryStringNames
mkQueryStringNames quantity
  = QueryStringNames'{quantity, items = Core.Nothing}

-- | The number of query string names in the @Items@ list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsnQuantity :: Lens.Lens' QueryStringNames Core.Int
qsnQuantity = Lens.field @"quantity"
{-# INLINEABLE qsnQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | A list of query string names.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsnItems :: Lens.Lens' QueryStringNames (Core.Maybe [Core.Text])
qsnItems = Lens.field @"items"
{-# INLINEABLE qsnItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.ToXML QueryStringNames where
        toXML QueryStringNames{..}
          = Core.toXMLElement "Quantity" quantity Core.<>
              Core.toXMLElement "Items"
                (Core.maybe Core.mempty (Core.toXMLList "Name") items)

instance Core.FromXML QueryStringNames where
        parseXML x
          = QueryStringNames' Core.<$>
              (x Core..@ "Quantity") Core.<*>
                x Core..@? "Items" Core..<@> Core.parseXMLList "Name"
