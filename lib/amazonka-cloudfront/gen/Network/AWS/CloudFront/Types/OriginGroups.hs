{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroups
  ( OriginGroups (..),

    -- * Smart constructor
    mkOriginGroups,

    -- * Lenses
    ogItems,
    ogQuantity,
  )
where

import Network.AWS.CloudFront.Types.OriginGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex data type for the origin groups specified for a distribution.
--
-- /See:/ 'mkOriginGroups' smart constructor.
data OriginGroups = OriginGroups'
  { items ::
      Lude.Maybe [OriginGroup],
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

-- | Creates a value of 'OriginGroups' with the minimum fields required to make a request.
--
-- * 'items' - The items (origin groups) in a distribution.
-- * 'quantity' - The number of origin groups.
mkOriginGroups ::
  -- | 'quantity'
  Lude.Int ->
  OriginGroups
mkOriginGroups pQuantity_ =
  OriginGroups' {items = Lude.Nothing, quantity = pQuantity_}

-- | The items (origin groups) in a distribution.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogItems :: Lens.Lens' OriginGroups (Lude.Maybe [OriginGroup])
ogItems = Lens.lens (items :: OriginGroups -> Lude.Maybe [OriginGroup]) (\s a -> s {items = a} :: OriginGroups)
{-# DEPRECATED ogItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The number of origin groups.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogQuantity :: Lens.Lens' OriginGroups Lude.Int
ogQuantity = Lens.lens (quantity :: OriginGroups -> Lude.Int) (\s a -> s {quantity = a} :: OriginGroups)
{-# DEPRECATED ogQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

instance Lude.FromXML OriginGroups where
  parseXML x =
    OriginGroups'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "OriginGroup")
               )
      Lude.<*> (x Lude..@ "Quantity")

instance Lude.ToXML OriginGroups where
  toXML OriginGroups' {..} =
    Lude.mconcat
      [ "Items"
          Lude.@= Lude.toXML (Lude.toXMLList "OriginGroup" Lude.<$> items),
        "Quantity" Lude.@= quantity
      ]
