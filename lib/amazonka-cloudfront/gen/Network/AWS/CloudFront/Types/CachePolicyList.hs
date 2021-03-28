{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.CachePolicyList
  ( CachePolicyList (..)
  -- * Smart constructor
  , mkCachePolicyList
  -- * Lenses
  , cplMaxItems
  , cplQuantity
  , cplItems
  , cplNextMarker
  ) where

import qualified Network.AWS.CloudFront.Types.CachePolicySummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of cache policies.
--
-- /See:/ 'mkCachePolicyList' smart constructor.
data CachePolicyList = CachePolicyList'
  { maxItems :: Core.Int
    -- ^ The maximum number of cache policies requested.
  , quantity :: Core.Int
    -- ^ The total number of cache policies returned in the response.
  , items :: Core.Maybe [Types.CachePolicySummary]
    -- ^ Contains the cache policies in the list.
  , nextMarker :: Core.Maybe Core.Text
    -- ^ If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing cache policies where you left off.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CachePolicyList' value with any optional fields omitted.
mkCachePolicyList
    :: Core.Int -- ^ 'maxItems'
    -> Core.Int -- ^ 'quantity'
    -> CachePolicyList
mkCachePolicyList maxItems quantity
  = CachePolicyList'{maxItems, quantity, items = Core.Nothing,
                     nextMarker = Core.Nothing}

-- | The maximum number of cache policies requested.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cplMaxItems :: Lens.Lens' CachePolicyList Core.Int
cplMaxItems = Lens.field @"maxItems"
{-# INLINEABLE cplMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | The total number of cache policies returned in the response.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cplQuantity :: Lens.Lens' CachePolicyList Core.Int
cplQuantity = Lens.field @"quantity"
{-# INLINEABLE cplQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | Contains the cache policies in the list.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cplItems :: Lens.Lens' CachePolicyList (Core.Maybe [Types.CachePolicySummary])
cplItems = Lens.field @"items"
{-# INLINEABLE cplItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing cache policies where you left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cplNextMarker :: Lens.Lens' CachePolicyList (Core.Maybe Core.Text)
cplNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE cplNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.FromXML CachePolicyList where
        parseXML x
          = CachePolicyList' Core.<$>
              (x Core..@ "MaxItems") Core.<*> x Core..@ "Quantity" Core.<*>
                x Core..@? "Items" Core..<@> Core.parseXMLList "CachePolicySummary"
                Core.<*> x Core..@? "NextMarker"
