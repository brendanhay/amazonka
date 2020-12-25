{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldPatterns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldPatterns
  ( FieldPatterns (..),

    -- * Smart constructor
    mkFieldPatterns,

    -- * Lenses
    fpQuantity,
    fpItems,
  )
where

import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex data type that includes the field patterns to match for field-level encryption.
--
-- /See:/ 'mkFieldPatterns' smart constructor.
data FieldPatterns = FieldPatterns'
  { -- | The number of field-level encryption field patterns.
    quantity :: Core.Int,
    -- | An array of the field-level encryption field patterns.
    items :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FieldPatterns' value with any optional fields omitted.
mkFieldPatterns ::
  -- | 'quantity'
  Core.Int ->
  FieldPatterns
mkFieldPatterns quantity =
  FieldPatterns' {quantity, items = Core.Nothing}

-- | The number of field-level encryption field patterns.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fpQuantity :: Lens.Lens' FieldPatterns Core.Int
fpQuantity = Lens.field @"quantity"
{-# DEPRECATED fpQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | An array of the field-level encryption field patterns.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fpItems :: Lens.Lens' FieldPatterns (Core.Maybe [Types.String])
fpItems = Lens.field @"items"
{-# DEPRECATED fpItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML FieldPatterns where
  toXML FieldPatterns {..} =
    Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode
        "Items"
        (Core.toXMLList "FieldPattern" Core.<$> items)

instance Core.FromXML FieldPatterns where
  parseXML x =
    FieldPatterns'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> (x Core..@? "Items" Core..<@> Core.parseXMLList "FieldPattern")
