{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.TrustedSigners
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.TrustedSigners
  ( TrustedSigners (..),

    -- * Smart constructor
    mkTrustedSigners,

    -- * Lenses
    tsEnabled,
    tsQuantity,
    tsItems,
  )
where

import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of AWS accounts whose public keys CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'mkTrustedSigners' smart constructor.
data TrustedSigners = TrustedSigners'
  { -- | This field is @true@ if any of the AWS accounts have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
    enabled :: Core.Bool,
    -- | The number of AWS accounts in the list.
    quantity :: Core.Int,
    -- | A list of AWS account identifiers.
    items :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrustedSigners' value with any optional fields omitted.
mkTrustedSigners ::
  -- | 'enabled'
  Core.Bool ->
  -- | 'quantity'
  Core.Int ->
  TrustedSigners
mkTrustedSigners enabled quantity =
  TrustedSigners' {enabled, quantity, items = Core.Nothing}

-- | This field is @true@ if any of the AWS accounts have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsEnabled :: Lens.Lens' TrustedSigners Core.Bool
tsEnabled = Lens.field @"enabled"
{-# DEPRECATED tsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The number of AWS accounts in the list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsQuantity :: Lens.Lens' TrustedSigners Core.Int
tsQuantity = Lens.field @"quantity"
{-# DEPRECATED tsQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A list of AWS account identifiers.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsItems :: Lens.Lens' TrustedSigners (Core.Maybe [Types.String])
tsItems = Lens.field @"items"
{-# DEPRECATED tsItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML TrustedSigners where
  toXML TrustedSigners {..} =
    Core.toXMLNode "Enabled" enabled
      Core.<> Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode
        "Items"
        (Core.toXMLList "AwsAccountNumber" Core.<$> items)

instance Core.FromXML TrustedSigners where
  parseXML x =
    TrustedSigners'
      Core.<$> (x Core..@ "Enabled")
      Core.<*> (x Core..@ "Quantity")
      Core.<*> (x Core..@? "Items" Core..<@> Core.parseXMLList "AwsAccountNumber")
