{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.OriginRequestPolicyList
  ( OriginRequestPolicyList (..)
  -- * Smart constructor
  , mkOriginRequestPolicyList
  -- * Lenses
  , orplMaxItems
  , orplQuantity
  , orplItems
  , orplNextMarker
  ) where

import qualified Network.AWS.CloudFront.Types.OriginRequestPolicySummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of origin request policies.
--
-- /See:/ 'mkOriginRequestPolicyList' smart constructor.
data OriginRequestPolicyList = OriginRequestPolicyList'
  { maxItems :: Core.Int
    -- ^ The maximum number of origin request policies requested.
  , quantity :: Core.Int
    -- ^ The total number of origin request policies returned in the response.
  , items :: Core.Maybe [Types.OriginRequestPolicySummary]
    -- ^ Contains the origin request policies in the list.
  , nextMarker :: Core.Maybe Core.Text
    -- ^ If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing origin request policies where you left off.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'OriginRequestPolicyList' value with any optional fields omitted.
mkOriginRequestPolicyList
    :: Core.Int -- ^ 'maxItems'
    -> Core.Int -- ^ 'quantity'
    -> OriginRequestPolicyList
mkOriginRequestPolicyList maxItems quantity
  = OriginRequestPolicyList'{maxItems, quantity,
                             items = Core.Nothing, nextMarker = Core.Nothing}

-- | The maximum number of origin request policies requested.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orplMaxItems :: Lens.Lens' OriginRequestPolicyList Core.Int
orplMaxItems = Lens.field @"maxItems"
{-# INLINEABLE orplMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | The total number of origin request policies returned in the response.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orplQuantity :: Lens.Lens' OriginRequestPolicyList Core.Int
orplQuantity = Lens.field @"quantity"
{-# INLINEABLE orplQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | Contains the origin request policies in the list.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orplItems :: Lens.Lens' OriginRequestPolicyList (Core.Maybe [Types.OriginRequestPolicySummary])
orplItems = Lens.field @"items"
{-# INLINEABLE orplItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing origin request policies where you left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orplNextMarker :: Lens.Lens' OriginRequestPolicyList (Core.Maybe Core.Text)
orplNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE orplNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.FromXML OriginRequestPolicyList where
        parseXML x
          = OriginRequestPolicyList' Core.<$>
              (x Core..@ "MaxItems") Core.<*> x Core..@ "Quantity" Core.<*>
                x Core..@? "Items" Core..<@>
                  Core.parseXMLList "OriginRequestPolicySummary"
                Core.<*> x Core..@? "NextMarker"
