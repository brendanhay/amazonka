{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ContentTypeProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ContentTypeProfiles
  ( ContentTypeProfiles (..),

    -- * Smart constructor
    mkContentTypeProfiles,

    -- * Lenses
    ctpItems,
    ctpQuantity,
  )
where

import Network.AWS.CloudFront.Types.ContentTypeProfile
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Field-level encryption content type-profile.
--
-- /See:/ 'mkContentTypeProfiles' smart constructor.
data ContentTypeProfiles = ContentTypeProfiles'
  { items ::
      Lude.Maybe [ContentTypeProfile],
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

-- | Creates a value of 'ContentTypeProfiles' with the minimum fields required to make a request.
--
-- * 'items' - Items in a field-level encryption content type-profile mapping.
-- * 'quantity' - The number of field-level encryption content type-profile mappings.
mkContentTypeProfiles ::
  -- | 'quantity'
  Lude.Int ->
  ContentTypeProfiles
mkContentTypeProfiles pQuantity_ =
  ContentTypeProfiles' {items = Lude.Nothing, quantity = pQuantity_}

-- | Items in a field-level encryption content type-profile mapping.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpItems :: Lens.Lens' ContentTypeProfiles (Lude.Maybe [ContentTypeProfile])
ctpItems = Lens.lens (items :: ContentTypeProfiles -> Lude.Maybe [ContentTypeProfile]) (\s a -> s {items = a} :: ContentTypeProfiles)
{-# DEPRECATED ctpItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The number of field-level encryption content type-profile mappings.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpQuantity :: Lens.Lens' ContentTypeProfiles Lude.Int
ctpQuantity = Lens.lens (quantity :: ContentTypeProfiles -> Lude.Int) (\s a -> s {quantity = a} :: ContentTypeProfiles)
{-# DEPRECATED ctpQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

instance Lude.FromXML ContentTypeProfiles where
  parseXML x =
    ContentTypeProfiles'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ContentTypeProfile")
               )
      Lude.<*> (x Lude..@ "Quantity")

instance Lude.ToXML ContentTypeProfiles where
  toXML ContentTypeProfiles' {..} =
    Lude.mconcat
      [ "Items"
          Lude.@= Lude.toXML (Lude.toXMLList "ContentTypeProfile" Lude.<$> items),
        "Quantity" Lude.@= quantity
      ]
