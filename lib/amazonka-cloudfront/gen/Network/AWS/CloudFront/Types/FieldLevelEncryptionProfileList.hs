{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileList
  ( FieldLevelEncryptionProfileList (..),

    -- * Smart constructor
    mkFieldLevelEncryptionProfileList,

    -- * Lenses
    fleplMaxItems,
    fleplQuantity,
    fleplItems,
    fleplNextMarker,
  )
where

import qualified Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileSummary as Types
import qualified Network.AWS.CloudFront.Types.NextMarker as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | List of field-level encryption profiles.
--
-- /See:/ 'mkFieldLevelEncryptionProfileList' smart constructor.
data FieldLevelEncryptionProfileList = FieldLevelEncryptionProfileList'
  { -- | The maximum number of field-level encryption profiles you want in the response body.
    maxItems :: Core.Int,
    -- | The number of field-level encryption profiles.
    quantity :: Core.Int,
    -- | The field-level encryption profile items.
    items :: Core.Maybe [Types.FieldLevelEncryptionProfileSummary],
    -- | If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your profiles where you left off.
    nextMarker :: Core.Maybe Types.NextMarker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'FieldLevelEncryptionProfileList' value with any optional fields omitted.
mkFieldLevelEncryptionProfileList ::
  -- | 'maxItems'
  Core.Int ->
  -- | 'quantity'
  Core.Int ->
  FieldLevelEncryptionProfileList
mkFieldLevelEncryptionProfileList maxItems quantity =
  FieldLevelEncryptionProfileList'
    { maxItems,
      quantity,
      items = Core.Nothing,
      nextMarker = Core.Nothing
    }

-- | The maximum number of field-level encryption profiles you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleplMaxItems :: Lens.Lens' FieldLevelEncryptionProfileList Core.Int
fleplMaxItems = Lens.field @"maxItems"
{-# DEPRECATED fleplMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The number of field-level encryption profiles.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleplQuantity :: Lens.Lens' FieldLevelEncryptionProfileList Core.Int
fleplQuantity = Lens.field @"quantity"
{-# DEPRECATED fleplQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | The field-level encryption profile items.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleplItems :: Lens.Lens' FieldLevelEncryptionProfileList (Core.Maybe [Types.FieldLevelEncryptionProfileSummary])
fleplItems = Lens.field @"items"
{-# DEPRECATED fleplItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your profiles where you left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleplNextMarker :: Lens.Lens' FieldLevelEncryptionProfileList (Core.Maybe Types.NextMarker)
fleplNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED fleplNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

instance Core.FromXML FieldLevelEncryptionProfileList where
  parseXML x =
    FieldLevelEncryptionProfileList'
      Core.<$> (x Core..@ "MaxItems")
      Core.<*> (x Core..@ "Quantity")
      Core.<*> ( x Core..@? "Items"
                   Core..<@> Core.parseXMLList "FieldLevelEncryptionProfileSummary"
               )
      Core.<*> (x Core..@? "NextMarker")
