{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ContentTypeProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ContentTypeProfiles
  ( ContentTypeProfiles (..),

    -- * Smart constructor
    mkContentTypeProfiles,

    -- * Lenses
    ctpQuantity,
    ctpItems,
  )
where

import qualified Network.AWS.CloudFront.Types.ContentTypeProfile as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Field-level encryption content type-profile.
--
-- /See:/ 'mkContentTypeProfiles' smart constructor.
data ContentTypeProfiles = ContentTypeProfiles'
  { -- | The number of field-level encryption content type-profile mappings.
    quantity :: Core.Int,
    -- | Items in a field-level encryption content type-profile mapping.
    items :: Core.Maybe [Types.ContentTypeProfile]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContentTypeProfiles' value with any optional fields omitted.
mkContentTypeProfiles ::
  -- | 'quantity'
  Core.Int ->
  ContentTypeProfiles
mkContentTypeProfiles quantity =
  ContentTypeProfiles' {quantity, items = Core.Nothing}

-- | The number of field-level encryption content type-profile mappings.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpQuantity :: Lens.Lens' ContentTypeProfiles Core.Int
ctpQuantity = Lens.field @"quantity"
{-# DEPRECATED ctpQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | Items in a field-level encryption content type-profile mapping.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpItems :: Lens.Lens' ContentTypeProfiles (Core.Maybe [Types.ContentTypeProfile])
ctpItems = Lens.field @"items"
{-# DEPRECATED ctpItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML ContentTypeProfiles where
  toXML ContentTypeProfiles {..} =
    Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode
        "Items"
        (Core.toXMLList "ContentTypeProfile" Core.<$> items)

instance Core.FromXML ContentTypeProfiles where
  parseXML x =
    ContentTypeProfiles'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> ( x Core..@? "Items"
                   Core..<@> Core.parseXMLList "ContentTypeProfile"
               )
