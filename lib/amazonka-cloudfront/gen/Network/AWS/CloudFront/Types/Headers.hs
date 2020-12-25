{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Headers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Headers
  ( Headers (..),

    -- * Smart constructor
    mkHeaders,

    -- * Lenses
    hQuantity,
    hItems,
  )
where

import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains a list of HTTP header names.
--
-- /See:/ 'mkHeaders' smart constructor.
data Headers = Headers'
  { -- | The number of header names in the @Items@ list.
    quantity :: Core.Int,
    -- | A list of HTTP header names.
    items :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Headers' value with any optional fields omitted.
mkHeaders ::
  -- | 'quantity'
  Core.Int ->
  Headers
mkHeaders quantity = Headers' {quantity, items = Core.Nothing}

-- | The number of header names in the @Items@ list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hQuantity :: Lens.Lens' Headers Core.Int
hQuantity = Lens.field @"quantity"
{-# DEPRECATED hQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A list of HTTP header names.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hItems :: Lens.Lens' Headers (Core.Maybe [Types.String])
hItems = Lens.field @"items"
{-# DEPRECATED hItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML Headers where
  toXML Headers {..} =
    Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode "Items" (Core.toXMLList "Name" Core.<$> items)

instance Core.FromXML Headers where
  parseXML x =
    Headers'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> (x Core..@? "Items" Core..<@> Core.parseXMLList "Name")
