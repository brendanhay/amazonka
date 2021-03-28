{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CookieNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.CookieNames
  ( CookieNames (..)
  -- * Smart constructor
  , mkCookieNames
  -- * Lenses
  , cnQuantity
  , cnItems
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains a list of cookie names.
--
-- /See:/ 'mkCookieNames' smart constructor.
data CookieNames = CookieNames'
  { quantity :: Core.Int
    -- ^ The number of cookie names in the @Items@ list.
  , items :: Core.Maybe [Core.Text]
    -- ^ A list of cookie names.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CookieNames' value with any optional fields omitted.
mkCookieNames
    :: Core.Int -- ^ 'quantity'
    -> CookieNames
mkCookieNames quantity
  = CookieNames'{quantity, items = Core.Nothing}

-- | The number of cookie names in the @Items@ list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnQuantity :: Lens.Lens' CookieNames Core.Int
cnQuantity = Lens.field @"quantity"
{-# INLINEABLE cnQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | A list of cookie names.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnItems :: Lens.Lens' CookieNames (Core.Maybe [Core.Text])
cnItems = Lens.field @"items"
{-# INLINEABLE cnItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.ToXML CookieNames where
        toXML CookieNames{..}
          = Core.toXMLElement "Quantity" quantity Core.<>
              Core.toXMLElement "Items"
                (Core.maybe Core.mempty (Core.toXMLList "Name") items)

instance Core.FromXML CookieNames where
        parseXML x
          = CookieNames' Core.<$>
              (x Core..@ "Quantity") Core.<*>
                x Core..@? "Items" Core..<@> Core.parseXMLList "Name"
