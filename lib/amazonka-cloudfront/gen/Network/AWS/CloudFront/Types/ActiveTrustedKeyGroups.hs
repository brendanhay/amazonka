{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ActiveTrustedKeyGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ActiveTrustedKeyGroups
  ( ActiveTrustedKeyGroups (..),

    -- * Smart constructor
    mkActiveTrustedKeyGroups,

    -- * Lenses
    atkgItems,
    atkgEnabled,
    atkgQuantity,
  )
where

import Network.AWS.CloudFront.Types.KGKeyPairIds
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of key groups, and the public keys in each key group, that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'mkActiveTrustedKeyGroups' smart constructor.
data ActiveTrustedKeyGroups = ActiveTrustedKeyGroups'
  { items ::
      Lude.Maybe [KGKeyPairIds],
    enabled :: Lude.Bool,
    quantity :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActiveTrustedKeyGroups' with the minimum fields required to make a request.
--
-- * 'enabled' - This field is @true@ if any of the key groups have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
-- * 'items' - A list of key groups, including the identifiers of the public keys in each key group that CloudFront can use to verify the signatures of signed URLs and signed cookies.
-- * 'quantity' - The number of key groups in the list.
mkActiveTrustedKeyGroups ::
  -- | 'enabled'
  Lude.Bool ->
  -- | 'quantity'
  Lude.Int ->
  ActiveTrustedKeyGroups
mkActiveTrustedKeyGroups pEnabled_ pQuantity_ =
  ActiveTrustedKeyGroups'
    { items = Lude.Nothing,
      enabled = pEnabled_,
      quantity = pQuantity_
    }

-- | A list of key groups, including the identifiers of the public keys in each key group that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atkgItems :: Lens.Lens' ActiveTrustedKeyGroups (Lude.Maybe [KGKeyPairIds])
atkgItems = Lens.lens (items :: ActiveTrustedKeyGroups -> Lude.Maybe [KGKeyPairIds]) (\s a -> s {items = a} :: ActiveTrustedKeyGroups)
{-# DEPRECATED atkgItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | This field is @true@ if any of the key groups have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atkgEnabled :: Lens.Lens' ActiveTrustedKeyGroups Lude.Bool
atkgEnabled = Lens.lens (enabled :: ActiveTrustedKeyGroups -> Lude.Bool) (\s a -> s {enabled = a} :: ActiveTrustedKeyGroups)
{-# DEPRECATED atkgEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The number of key groups in the list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atkgQuantity :: Lens.Lens' ActiveTrustedKeyGroups Lude.Int
atkgQuantity = Lens.lens (quantity :: ActiveTrustedKeyGroups -> Lude.Int) (\s a -> s {quantity = a} :: ActiveTrustedKeyGroups)
{-# DEPRECATED atkgQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

instance Lude.FromXML ActiveTrustedKeyGroups where
  parseXML x =
    ActiveTrustedKeyGroups'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "KeyGroup")
               )
      Lude.<*> (x Lude..@ "Enabled")
      Lude.<*> (x Lude..@ "Quantity")
