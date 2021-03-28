{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.TrustedKeyGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.TrustedKeyGroups
  ( TrustedKeyGroups (..)
  -- * Smart constructor
  , mkTrustedKeyGroups
  -- * Lenses
  , tkgEnabled
  , tkgQuantity
  , tkgItems
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of key groups whose public keys CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'mkTrustedKeyGroups' smart constructor.
data TrustedKeyGroups = TrustedKeyGroups'
  { enabled :: Core.Bool
    -- ^ This field is @true@ if any of the key groups in the list have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
  , quantity :: Core.Int
    -- ^ The number of key groups in the list.
  , items :: Core.Maybe [Core.Text]
    -- ^ A list of key groups identifiers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrustedKeyGroups' value with any optional fields omitted.
mkTrustedKeyGroups
    :: Core.Bool -- ^ 'enabled'
    -> Core.Int -- ^ 'quantity'
    -> TrustedKeyGroups
mkTrustedKeyGroups enabled quantity
  = TrustedKeyGroups'{enabled, quantity, items = Core.Nothing}

-- | This field is @true@ if any of the key groups in the list have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tkgEnabled :: Lens.Lens' TrustedKeyGroups Core.Bool
tkgEnabled = Lens.field @"enabled"
{-# INLINEABLE tkgEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The number of key groups in the list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tkgQuantity :: Lens.Lens' TrustedKeyGroups Core.Int
tkgQuantity = Lens.field @"quantity"
{-# INLINEABLE tkgQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | A list of key groups identifiers.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tkgItems :: Lens.Lens' TrustedKeyGroups (Core.Maybe [Core.Text])
tkgItems = Lens.field @"items"
{-# INLINEABLE tkgItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.ToXML TrustedKeyGroups where
        toXML TrustedKeyGroups{..}
          = Core.toXMLElement "Enabled" enabled Core.<>
              Core.toXMLElement "Quantity" quantity
              Core.<>
              Core.toXMLElement "Items"
                (Core.maybe Core.mempty (Core.toXMLList "KeyGroup") items)

instance Core.FromXML TrustedKeyGroups where
        parseXML x
          = TrustedKeyGroups' Core.<$>
              (x Core..@ "Enabled") Core.<*> x Core..@ "Quantity" Core.<*>
                x Core..@? "Items" Core..<@> Core.parseXMLList "KeyGroup"
