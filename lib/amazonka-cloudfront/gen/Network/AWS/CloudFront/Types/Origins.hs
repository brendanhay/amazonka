{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Origins
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Origins
  ( Origins (..),

    -- * Smart constructor
    mkOrigins,

    -- * Lenses
    oQuantity,
    oItems,
  )
where

import qualified Network.AWS.CloudFront.Types.Origin as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the origins for this distribution.
--
-- /See:/ 'mkOrigins' smart constructor.
data Origins = Origins'
  { -- | The number of origins for this distribution.
    quantity :: Core.Int,
    -- | A list of origins.
    items :: Core.NonEmpty Types.Origin
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Origins' value with any optional fields omitted.
mkOrigins ::
  -- | 'quantity'
  Core.Int ->
  -- | 'items'
  Core.NonEmpty Types.Origin ->
  Origins
mkOrigins quantity items = Origins' {quantity, items}

-- | The number of origins for this distribution.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oQuantity :: Lens.Lens' Origins Core.Int
oQuantity = Lens.field @"quantity"
{-# DEPRECATED oQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A list of origins.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oItems :: Lens.Lens' Origins (Core.NonEmpty Types.Origin)
oItems = Lens.field @"items"
{-# DEPRECATED oItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML Origins where
  toXML Origins {..} =
    Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode "Items" (Core.toXMLList "Origin" items)

instance Core.FromXML Origins where
  parseXML x =
    Origins'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> (x Core..@ "Items" Core..<@> Core.parseXMLNonEmpty "Origin")
