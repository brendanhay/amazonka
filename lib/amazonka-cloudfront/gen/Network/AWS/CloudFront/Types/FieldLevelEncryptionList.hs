{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.FieldLevelEncryptionList
  ( FieldLevelEncryptionList (..)
  -- * Smart constructor
  , mkFieldLevelEncryptionList
  -- * Lenses
  , flelMaxItems
  , flelQuantity
  , flelItems
  , flelNextMarker
  ) where

import qualified Network.AWS.CloudFront.Types.FieldLevelEncryptionSummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | List of field-level encrpytion configurations.
--
-- /See:/ 'mkFieldLevelEncryptionList' smart constructor.
data FieldLevelEncryptionList = FieldLevelEncryptionList'
  { maxItems :: Core.Int
    -- ^ The maximum number of elements you want in the response body. 
  , quantity :: Core.Int
    -- ^ The number of field-level encryption items.
  , items :: Core.Maybe [Types.FieldLevelEncryptionSummary]
    -- ^ An array of field-level encryption items.
  , nextMarker :: Core.Maybe Core.Text
    -- ^ If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your configurations where you left off.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'FieldLevelEncryptionList' value with any optional fields omitted.
mkFieldLevelEncryptionList
    :: Core.Int -- ^ 'maxItems'
    -> Core.Int -- ^ 'quantity'
    -> FieldLevelEncryptionList
mkFieldLevelEncryptionList maxItems quantity
  = FieldLevelEncryptionList'{maxItems, quantity,
                              items = Core.Nothing, nextMarker = Core.Nothing}

-- | The maximum number of elements you want in the response body. 
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flelMaxItems :: Lens.Lens' FieldLevelEncryptionList Core.Int
flelMaxItems = Lens.field @"maxItems"
{-# INLINEABLE flelMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | The number of field-level encryption items.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flelQuantity :: Lens.Lens' FieldLevelEncryptionList Core.Int
flelQuantity = Lens.field @"quantity"
{-# INLINEABLE flelQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | An array of field-level encryption items.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flelItems :: Lens.Lens' FieldLevelEncryptionList (Core.Maybe [Types.FieldLevelEncryptionSummary])
flelItems = Lens.field @"items"
{-# INLINEABLE flelItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your configurations where you left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flelNextMarker :: Lens.Lens' FieldLevelEncryptionList (Core.Maybe Core.Text)
flelNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE flelNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.FromXML FieldLevelEncryptionList where
        parseXML x
          = FieldLevelEncryptionList' Core.<$>
              (x Core..@ "MaxItems") Core.<*> x Core..@ "Quantity" Core.<*>
                x Core..@? "Items" Core..<@>
                  Core.parseXMLList "FieldLevelEncryptionSummary"
                Core.<*> x Core..@? "NextMarker"
