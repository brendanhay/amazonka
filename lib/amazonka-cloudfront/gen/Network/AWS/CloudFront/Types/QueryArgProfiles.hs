{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryArgProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryArgProfiles
  ( QueryArgProfiles (..),

    -- * Smart constructor
    mkQueryArgProfiles,

    -- * Lenses
    qapQuantity,
    qapItems,
  )
where

import Network.AWS.CloudFront.Types.QueryArgProfile
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Query argument-profile mapping for field-level encryption.
--
-- /See:/ 'mkQueryArgProfiles' smart constructor.
data QueryArgProfiles = QueryArgProfiles'
  { -- | Number of profiles for query argument-profile mapping for field-level encryption.
    quantity :: Lude.Int,
    -- | Number of items for query argument-profile mapping for field-level encryption.
    items :: Lude.Maybe [QueryArgProfile]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryArgProfiles' with the minimum fields required to make a request.
--
-- * 'quantity' - Number of profiles for query argument-profile mapping for field-level encryption.
-- * 'items' - Number of items for query argument-profile mapping for field-level encryption.
mkQueryArgProfiles ::
  -- | 'quantity'
  Lude.Int ->
  QueryArgProfiles
mkQueryArgProfiles pQuantity_ =
  QueryArgProfiles' {quantity = pQuantity_, items = Lude.Nothing}

-- | Number of profiles for query argument-profile mapping for field-level encryption.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapQuantity :: Lens.Lens' QueryArgProfiles Lude.Int
qapQuantity = Lens.lens (quantity :: QueryArgProfiles -> Lude.Int) (\s a -> s {quantity = a} :: QueryArgProfiles)
{-# DEPRECATED qapQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | Number of items for query argument-profile mapping for field-level encryption.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapItems :: Lens.Lens' QueryArgProfiles (Lude.Maybe [QueryArgProfile])
qapItems = Lens.lens (items :: QueryArgProfiles -> Lude.Maybe [QueryArgProfile]) (\s a -> s {items = a} :: QueryArgProfiles)
{-# DEPRECATED qapItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML QueryArgProfiles where
  parseXML x =
    QueryArgProfiles'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "QueryArgProfile")
               )

instance Lude.ToXML QueryArgProfiles where
  toXML QueryArgProfiles' {..} =
    Lude.mconcat
      [ "Quantity" Lude.@= quantity,
        "Items"
          Lude.@= Lude.toXML (Lude.toXMLList "QueryArgProfile" Lude.<$> items)
      ]
