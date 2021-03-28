{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyGroupList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.KeyGroupList
  ( KeyGroupList (..)
  -- * Smart constructor
  , mkKeyGroupList
  -- * Lenses
  , kglMaxItems
  , kglQuantity
  , kglItems
  , kglNextMarker
  ) where

import qualified Network.AWS.CloudFront.Types.KeyGroupSummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of key groups.
--
-- /See:/ 'mkKeyGroupList' smart constructor.
data KeyGroupList = KeyGroupList'
  { maxItems :: Core.Int
    -- ^ The maximum number of key groups requested.
  , quantity :: Core.Int
    -- ^ The number of key groups returned in the response.
  , items :: Core.Maybe [Types.KeyGroupSummary]
    -- ^ A list of key groups.
  , nextMarker :: Core.Maybe Core.Text
    -- ^ If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing key groups.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'KeyGroupList' value with any optional fields omitted.
mkKeyGroupList
    :: Core.Int -- ^ 'maxItems'
    -> Core.Int -- ^ 'quantity'
    -> KeyGroupList
mkKeyGroupList maxItems quantity
  = KeyGroupList'{maxItems, quantity, items = Core.Nothing,
                  nextMarker = Core.Nothing}

-- | The maximum number of key groups requested.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kglMaxItems :: Lens.Lens' KeyGroupList Core.Int
kglMaxItems = Lens.field @"maxItems"
{-# INLINEABLE kglMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | The number of key groups returned in the response.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kglQuantity :: Lens.Lens' KeyGroupList Core.Int
kglQuantity = Lens.field @"quantity"
{-# INLINEABLE kglQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | A list of key groups.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kglItems :: Lens.Lens' KeyGroupList (Core.Maybe [Types.KeyGroupSummary])
kglItems = Lens.field @"items"
{-# INLINEABLE kglItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing key groups.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kglNextMarker :: Lens.Lens' KeyGroupList (Core.Maybe Core.Text)
kglNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE kglNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.FromXML KeyGroupList where
        parseXML x
          = KeyGroupList' Core.<$>
              (x Core..@ "MaxItems") Core.<*> x Core..@ "Quantity" Core.<*>
                x Core..@? "Items" Core..<@> Core.parseXMLList "KeyGroupSummary"
                Core.<*> x Core..@? "NextMarker"
