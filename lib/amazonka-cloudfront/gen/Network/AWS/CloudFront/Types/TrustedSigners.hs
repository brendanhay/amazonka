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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of AWS accounts whose public keys CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'mkTrustedSigners' smart constructor.
data TrustedSigners = TrustedSigners'
  { -- | This field is @true@ if any of the AWS accounts have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
    enabled :: Lude.Bool,
    -- | The number of AWS accounts in the list.
    quantity :: Lude.Int,
    -- | A list of AWS account identifiers.
    items :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrustedSigners' with the minimum fields required to make a request.
--
-- * 'enabled' - This field is @true@ if any of the AWS accounts have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
-- * 'quantity' - The number of AWS accounts in the list.
-- * 'items' - A list of AWS account identifiers.
mkTrustedSigners ::
  -- | 'enabled'
  Lude.Bool ->
  -- | 'quantity'
  Lude.Int ->
  TrustedSigners
mkTrustedSigners pEnabled_ pQuantity_ =
  TrustedSigners'
    { enabled = pEnabled_,
      quantity = pQuantity_,
      items = Lude.Nothing
    }

-- | This field is @true@ if any of the AWS accounts have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsEnabled :: Lens.Lens' TrustedSigners Lude.Bool
tsEnabled = Lens.lens (enabled :: TrustedSigners -> Lude.Bool) (\s a -> s {enabled = a} :: TrustedSigners)
{-# DEPRECATED tsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The number of AWS accounts in the list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsQuantity :: Lens.Lens' TrustedSigners Lude.Int
tsQuantity = Lens.lens (quantity :: TrustedSigners -> Lude.Int) (\s a -> s {quantity = a} :: TrustedSigners)
{-# DEPRECATED tsQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A list of AWS account identifiers.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsItems :: Lens.Lens' TrustedSigners (Lude.Maybe [Lude.Text])
tsItems = Lens.lens (items :: TrustedSigners -> Lude.Maybe [Lude.Text]) (\s a -> s {items = a} :: TrustedSigners)
{-# DEPRECATED tsItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML TrustedSigners where
  parseXML x =
    TrustedSigners'
      Lude.<$> (x Lude..@ "Enabled")
      Lude.<*> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "AwsAccountNumber")
               )

instance Lude.ToXML TrustedSigners where
  toXML TrustedSigners' {..} =
    Lude.mconcat
      [ "Enabled" Lude.@= enabled,
        "Quantity" Lude.@= quantity,
        "Items"
          Lude.@= Lude.toXML (Lude.toXMLList "AwsAccountNumber" Lude.<$> items)
      ]
