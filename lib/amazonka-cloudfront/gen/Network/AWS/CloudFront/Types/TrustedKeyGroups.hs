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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of key groups whose public keys CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'mkTrustedKeyGroups' smart constructor.
data TrustedKeyGroups = TrustedKeyGroups'
  { -- | This field is @true@ if any of the key groups in the list have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
    enabled :: Lude.Bool,
    -- | The number of key groups in the list.
    quantity :: Lude.Int,
    -- | A list of key groups identifiers.
    items :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrustedKeyGroups' with the minimum fields required to make a request.
--
-- * 'enabled' - This field is @true@ if any of the key groups in the list have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
-- * 'quantity' - The number of key groups in the list.
-- * 'items' - A list of key groups identifiers.
mkTrustedKeyGroups ::
  -- | 'enabled'
  Lude.Bool ->
  -- | 'quantity'
  Lude.Int ->
  TrustedKeyGroups
mkTrustedKeyGroups pEnabled_ pQuantity_ =
  TrustedKeyGroups'
    { enabled = pEnabled_,
      quantity = pQuantity_,
      items = Lude.Nothing
    }

-- | This field is @true@ if any of the key groups in the list have public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies. If not, this field is @false@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tkgEnabled :: Lens.Lens' TrustedKeyGroups Lude.Bool
tkgEnabled = Lens.lens (enabled :: TrustedKeyGroups -> Lude.Bool) (\s a -> s {enabled = a} :: TrustedKeyGroups)
{-# DEPRECATED tkgEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The number of key groups in the list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tkgQuantity :: Lens.Lens' TrustedKeyGroups Lude.Int
tkgQuantity = Lens.lens (quantity :: TrustedKeyGroups -> Lude.Int) (\s a -> s {quantity = a} :: TrustedKeyGroups)
{-# DEPRECATED tkgQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A list of key groups identifiers.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tkgItems :: Lens.Lens' TrustedKeyGroups (Lude.Maybe [Lude.Text])
tkgItems = Lens.lens (items :: TrustedKeyGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {items = a} :: TrustedKeyGroups)
{-# DEPRECATED tkgItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML TrustedKeyGroups where
  parseXML x =
    TrustedKeyGroups'
      Lude.<$> (x Lude..@ "Enabled")
      Lude.<*> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "KeyGroup")
               )

instance Lude.ToXML TrustedKeyGroups where
  toXML TrustedKeyGroups' {..} =
    Lude.mconcat
      [ "Enabled" Lude.@= enabled,
        "Quantity" Lude.@= quantity,
        "Items"
          Lude.@= Lude.toXML (Lude.toXMLList "KeyGroup" Lude.<$> items)
      ]
