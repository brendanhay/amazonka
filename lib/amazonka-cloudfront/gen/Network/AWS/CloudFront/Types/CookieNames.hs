{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CookieNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CookieNames
  ( CookieNames (..),

    -- * Smart constructor
    mkCookieNames,

    -- * Lenses
    cnQuantity,
    cnItems,
  )
where

import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains a list of cookie names.
--
-- /See:/ 'mkCookieNames' smart constructor.
data CookieNames = CookieNames'
  { -- | The number of cookie names in the @Items@ list.
    quantity :: Core.Int,
    -- | A list of cookie names.
    items :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CookieNames' value with any optional fields omitted.
mkCookieNames ::
  -- | 'quantity'
  Core.Int ->
  CookieNames
mkCookieNames quantity =
  CookieNames' {quantity, items = Core.Nothing}

-- | The number of cookie names in the @Items@ list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnQuantity :: Lens.Lens' CookieNames Core.Int
cnQuantity = Lens.field @"quantity"
{-# DEPRECATED cnQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A list of cookie names.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnItems :: Lens.Lens' CookieNames (Core.Maybe [Types.String])
cnItems = Lens.field @"items"
{-# DEPRECATED cnItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML CookieNames where
  toXML CookieNames {..} =
    Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode "Items" (Core.toXMLList "Name" Core.<$> items)

instance Core.FromXML CookieNames where
  parseXML x =
    CookieNames'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> (x Core..@? "Items" Core..<@> Core.parseXMLList "Name")
