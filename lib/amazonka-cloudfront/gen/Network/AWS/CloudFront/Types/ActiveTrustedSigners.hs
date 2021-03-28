{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ActiveTrustedSigners
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.ActiveTrustedSigners
  ( ActiveTrustedSigners (..)
  -- * Smart constructor
  , mkActiveTrustedSigners
  -- * Lenses
  , atsEnabled
  , atsQuantity
  , atsItems
  ) where

import qualified Network.AWS.CloudFront.Types.Signer as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of AWS accounts and the active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'mkActiveTrustedSigners' smart constructor.
data ActiveTrustedSigners = ActiveTrustedSigners'
  { enabled :: Core.Bool
    -- ^ This field is @true@ if any of the AWS accounts in the list have active CloudFront key pairs that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
  , quantity :: Core.Int
    -- ^ The number of AWS accounts in the list.
  , items :: Core.Maybe [Types.Signer]
    -- ^ A list of AWS accounts and the identifiers of active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs and signed cookies.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActiveTrustedSigners' value with any optional fields omitted.
mkActiveTrustedSigners
    :: Core.Bool -- ^ 'enabled'
    -> Core.Int -- ^ 'quantity'
    -> ActiveTrustedSigners
mkActiveTrustedSigners enabled quantity
  = ActiveTrustedSigners'{enabled, quantity, items = Core.Nothing}

-- | This field is @true@ if any of the AWS accounts in the list have active CloudFront key pairs that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsEnabled :: Lens.Lens' ActiveTrustedSigners Core.Bool
atsEnabled = Lens.field @"enabled"
{-# INLINEABLE atsEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The number of AWS accounts in the list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsQuantity :: Lens.Lens' ActiveTrustedSigners Core.Int
atsQuantity = Lens.field @"quantity"
{-# INLINEABLE atsQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | A list of AWS accounts and the identifiers of active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsItems :: Lens.Lens' ActiveTrustedSigners (Core.Maybe [Types.Signer])
atsItems = Lens.field @"items"
{-# INLINEABLE atsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.FromXML ActiveTrustedSigners where
        parseXML x
          = ActiveTrustedSigners' Core.<$>
              (x Core..@ "Enabled") Core.<*> x Core..@ "Quantity" Core.<*>
                x Core..@? "Items" Core..<@> Core.parseXMLList "Signer"
