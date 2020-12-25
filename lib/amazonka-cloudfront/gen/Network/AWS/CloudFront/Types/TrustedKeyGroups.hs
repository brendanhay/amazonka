{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.TrustedKeyGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.TrustedKeyGroups
  ( TrustedKeyGroups (..),

    -- * Smart constructor
    mkTrustedKeyGroups,

    -- * Lenses
    tkgEnabled,
    tkgQuantity,
    tkgItems,
  )
where

import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of key groups whose public keys CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'mkTrustedKeyGroups' smart constructor.
data TrustedKeyGroups = TrustedKeyGroups'
  { -- | This field is @true@ if any of the key groups in the list have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
    enabled :: Core.Bool,
    -- | The number of key groups in the list.
    quantity :: Core.Int,
    -- | A list of key groups identifiers.
    items :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrustedKeyGroups' value with any optional fields omitted.
mkTrustedKeyGroups ::
  -- | 'enabled'
  Core.Bool ->
  -- | 'quantity'
  Core.Int ->
  TrustedKeyGroups
mkTrustedKeyGroups enabled quantity =
  TrustedKeyGroups' {enabled, quantity, items = Core.Nothing}

-- | This field is @true@ if any of the key groups in the list have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tkgEnabled :: Lens.Lens' TrustedKeyGroups Core.Bool
tkgEnabled = Lens.field @"enabled"
{-# DEPRECATED tkgEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The number of key groups in the list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tkgQuantity :: Lens.Lens' TrustedKeyGroups Core.Int
tkgQuantity = Lens.field @"quantity"
{-# DEPRECATED tkgQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A list of key groups identifiers.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tkgItems :: Lens.Lens' TrustedKeyGroups (Core.Maybe [Types.String])
tkgItems = Lens.field @"items"
{-# DEPRECATED tkgItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML TrustedKeyGroups where
  toXML TrustedKeyGroups {..} =
    Core.toXMLNode "Enabled" enabled
      Core.<> Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode "Items" (Core.toXMLList "KeyGroup" Core.<$> items)

instance Core.FromXML TrustedKeyGroups where
  parseXML x =
    TrustedKeyGroups'
      Core.<$> (x Core..@ "Enabled")
      Core.<*> (x Core..@ "Quantity")
      Core.<*> (x Core..@? "Items" Core..<@> Core.parseXMLList "KeyGroup")
