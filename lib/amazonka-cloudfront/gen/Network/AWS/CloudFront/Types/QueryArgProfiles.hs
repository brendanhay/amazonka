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
    qapItems,
    qapQuantity,
  )
where

import Network.AWS.CloudFront.Types.QueryArgProfile
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Query argument-profile mapping for field-level encryption.
--
-- /See:/ 'mkQueryArgProfiles' smart constructor.
data QueryArgProfiles = QueryArgProfiles'
  { items ::
      Lude.Maybe [QueryArgProfile],
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

-- | Creates a value of 'QueryArgProfiles' with the minimum fields required to make a request.
--
-- * 'items' - Number of items for query argument-profile mapping for field-level encryption.
-- * 'quantity' - Number of profiles for query argument-profile mapping for field-level encryption.
mkQueryArgProfiles ::
  -- | 'quantity'
  Lude.Int ->
  QueryArgProfiles
mkQueryArgProfiles pQuantity_ =
  QueryArgProfiles' {items = Lude.Nothing, quantity = pQuantity_}

-- | Number of items for query argument-profile mapping for field-level encryption.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapItems :: Lens.Lens' QueryArgProfiles (Lude.Maybe [QueryArgProfile])
qapItems = Lens.lens (items :: QueryArgProfiles -> Lude.Maybe [QueryArgProfile]) (\s a -> s {items = a} :: QueryArgProfiles)
{-# DEPRECATED qapItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Number of profiles for query argument-profile mapping for field-level encryption.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapQuantity :: Lens.Lens' QueryArgProfiles Lude.Int
qapQuantity = Lens.lens (quantity :: QueryArgProfiles -> Lude.Int) (\s a -> s {quantity = a} :: QueryArgProfiles)
{-# DEPRECATED qapQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

instance Lude.FromXML QueryArgProfiles where
  parseXML x =
    QueryArgProfiles'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "QueryArgProfile")
               )
      Lude.<*> (x Lude..@ "Quantity")

instance Lude.ToXML QueryArgProfiles where
  toXML QueryArgProfiles' {..} =
    Lude.mconcat
      [ "Items"
          Lude.@= Lude.toXML (Lude.toXMLList "QueryArgProfile" Lude.<$> items),
        "Quantity" Lude.@= quantity
      ]
