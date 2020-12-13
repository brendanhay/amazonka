{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ActiveTrustedSigners
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ActiveTrustedSigners
  ( ActiveTrustedSigners (..),

    -- * Smart constructor
    mkActiveTrustedSigners,

    -- * Lenses
    atsEnabled,
    atsQuantity,
    atsItems,
  )
where

import Network.AWS.CloudFront.Types.Signer
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of AWS accounts and the active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'mkActiveTrustedSigners' smart constructor.
data ActiveTrustedSigners = ActiveTrustedSigners'
  { -- | This field is @true@ if any of the AWS accounts in the list have active CloudFront key pairs that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
    enabled :: Lude.Bool,
    -- | The number of AWS accounts in the list.
    quantity :: Lude.Int,
    -- | A list of AWS accounts and the identifiers of active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs and signed cookies.
    items :: Lude.Maybe [Signer]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActiveTrustedSigners' with the minimum fields required to make a request.
--
-- * 'enabled' - This field is @true@ if any of the AWS accounts in the list have active CloudFront key pairs that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
-- * 'quantity' - The number of AWS accounts in the list.
-- * 'items' - A list of AWS accounts and the identifiers of active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs and signed cookies.
mkActiveTrustedSigners ::
  -- | 'enabled'
  Lude.Bool ->
  -- | 'quantity'
  Lude.Int ->
  ActiveTrustedSigners
mkActiveTrustedSigners pEnabled_ pQuantity_ =
  ActiveTrustedSigners'
    { enabled = pEnabled_,
      quantity = pQuantity_,
      items = Lude.Nothing
    }

-- | This field is @true@ if any of the AWS accounts in the list have active CloudFront key pairs that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsEnabled :: Lens.Lens' ActiveTrustedSigners Lude.Bool
atsEnabled = Lens.lens (enabled :: ActiveTrustedSigners -> Lude.Bool) (\s a -> s {enabled = a} :: ActiveTrustedSigners)
{-# DEPRECATED atsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The number of AWS accounts in the list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsQuantity :: Lens.Lens' ActiveTrustedSigners Lude.Int
atsQuantity = Lens.lens (quantity :: ActiveTrustedSigners -> Lude.Int) (\s a -> s {quantity = a} :: ActiveTrustedSigners)
{-# DEPRECATED atsQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A list of AWS accounts and the identifiers of active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsItems :: Lens.Lens' ActiveTrustedSigners (Lude.Maybe [Signer])
atsItems = Lens.lens (items :: ActiveTrustedSigners -> Lude.Maybe [Signer]) (\s a -> s {items = a} :: ActiveTrustedSigners)
{-# DEPRECATED atsItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML ActiveTrustedSigners where
  parseXML x =
    ActiveTrustedSigners'
      Lude.<$> (x Lude..@ "Enabled")
      Lude.<*> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Signer")
               )
