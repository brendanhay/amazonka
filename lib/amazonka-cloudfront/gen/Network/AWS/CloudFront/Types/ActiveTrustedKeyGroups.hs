{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ActiveTrustedKeyGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.ActiveTrustedKeyGroups
  ( ActiveTrustedKeyGroups (..)
  -- * Smart constructor
  , mkActiveTrustedKeyGroups
  -- * Lenses
  , atkgEnabled
  , atkgQuantity
  , atkgItems
  ) where

import qualified Network.AWS.CloudFront.Types.KGKeyPairIds as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of key groups, and the public keys in each key group, that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'mkActiveTrustedKeyGroups' smart constructor.
data ActiveTrustedKeyGroups = ActiveTrustedKeyGroups'
  { enabled :: Core.Bool
    -- ^ This field is @true@ if any of the key groups have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
  , quantity :: Core.Int
    -- ^ The number of key groups in the list.
  , items :: Core.Maybe [Types.KGKeyPairIds]
    -- ^ A list of key groups, including the identifiers of the public keys in each key group that CloudFront can use to verify the signatures of signed URLs and signed cookies.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActiveTrustedKeyGroups' value with any optional fields omitted.
mkActiveTrustedKeyGroups
    :: Core.Bool -- ^ 'enabled'
    -> Core.Int -- ^ 'quantity'
    -> ActiveTrustedKeyGroups
mkActiveTrustedKeyGroups enabled quantity
  = ActiveTrustedKeyGroups'{enabled, quantity, items = Core.Nothing}

-- | This field is @true@ if any of the key groups have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atkgEnabled :: Lens.Lens' ActiveTrustedKeyGroups Core.Bool
atkgEnabled = Lens.field @"enabled"
{-# INLINEABLE atkgEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The number of key groups in the list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atkgQuantity :: Lens.Lens' ActiveTrustedKeyGroups Core.Int
atkgQuantity = Lens.field @"quantity"
{-# INLINEABLE atkgQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | A list of key groups, including the identifiers of the public keys in each key group that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atkgItems :: Lens.Lens' ActiveTrustedKeyGroups (Core.Maybe [Types.KGKeyPairIds])
atkgItems = Lens.field @"items"
{-# INLINEABLE atkgItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.FromXML ActiveTrustedKeyGroups where
        parseXML x
          = ActiveTrustedKeyGroups' Core.<$>
              (x Core..@ "Enabled") Core.<*> x Core..@ "Quantity" Core.<*>
                x Core..@? "Items" Core..<@> Core.parseXMLList "KeyGroup"
