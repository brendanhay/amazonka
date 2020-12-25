{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyList
  ( OriginRequestPolicyList (..),

    -- * Smart constructor
    mkOriginRequestPolicyList,

    -- * Lenses
    orplMaxItems,
    orplQuantity,
    orplItems,
    orplNextMarker,
  )
where

import qualified Network.AWS.CloudFront.Types.NextMarker as Types
import qualified Network.AWS.CloudFront.Types.OriginRequestPolicySummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of origin request policies.
--
-- /See:/ 'mkOriginRequestPolicyList' smart constructor.
data OriginRequestPolicyList = OriginRequestPolicyList'
  { -- | The maximum number of origin request policies requested.
    maxItems :: Core.Int,
    -- | The total number of origin request policies returned in the response.
    quantity :: Core.Int,
    -- | Contains the origin request policies in the list.
    items :: Core.Maybe [Types.OriginRequestPolicySummary],
    -- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing origin request policies where you left off.
    nextMarker :: Core.Maybe Types.NextMarker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'OriginRequestPolicyList' value with any optional fields omitted.
mkOriginRequestPolicyList ::
  -- | 'maxItems'
  Core.Int ->
  -- | 'quantity'
  Core.Int ->
  OriginRequestPolicyList
mkOriginRequestPolicyList maxItems quantity =
  OriginRequestPolicyList'
    { maxItems,
      quantity,
      items = Core.Nothing,
      nextMarker = Core.Nothing
    }

-- | The maximum number of origin request policies requested.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orplMaxItems :: Lens.Lens' OriginRequestPolicyList Core.Int
orplMaxItems = Lens.field @"maxItems"
{-# DEPRECATED orplMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The total number of origin request policies returned in the response.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orplQuantity :: Lens.Lens' OriginRequestPolicyList Core.Int
orplQuantity = Lens.field @"quantity"
{-# DEPRECATED orplQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | Contains the origin request policies in the list.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orplItems :: Lens.Lens' OriginRequestPolicyList (Core.Maybe [Types.OriginRequestPolicySummary])
orplItems = Lens.field @"items"
{-# DEPRECATED orplItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing origin request policies where you left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orplNextMarker :: Lens.Lens' OriginRequestPolicyList (Core.Maybe Types.NextMarker)
orplNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED orplNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

instance Core.FromXML OriginRequestPolicyList where
  parseXML x =
    OriginRequestPolicyList'
      Core.<$> (x Core..@ "MaxItems")
      Core.<*> (x Core..@ "Quantity")
      Core.<*> ( x Core..@? "Items"
                   Core..<@> Core.parseXMLList "OriginRequestPolicySummary"
               )
      Core.<*> (x Core..@? "NextMarker")
