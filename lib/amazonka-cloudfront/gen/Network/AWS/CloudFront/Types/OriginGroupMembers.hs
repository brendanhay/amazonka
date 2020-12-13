{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginGroupMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroupMembers
  ( OriginGroupMembers (..),

    -- * Smart constructor
    mkOriginGroupMembers,

    -- * Lenses
    ogmQuantity,
    ogmItems,
  )
where

import Network.AWS.CloudFront.Types.OriginGroupMember
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex data type for the origins included in an origin group.
--
-- /See:/ 'mkOriginGroupMembers' smart constructor.
data OriginGroupMembers = OriginGroupMembers'
  { -- | The number of origins in an origin group.
    quantity :: Lude.Int,
    -- | Items (origins) in an origin group.
    items :: Lude.NonEmpty OriginGroupMember
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginGroupMembers' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of origins in an origin group.
-- * 'items' - Items (origins) in an origin group.
mkOriginGroupMembers ::
  -- | 'quantity'
  Lude.Int ->
  -- | 'items'
  Lude.NonEmpty OriginGroupMember ->
  OriginGroupMembers
mkOriginGroupMembers pQuantity_ pItems_ =
  OriginGroupMembers' {quantity = pQuantity_, items = pItems_}

-- | The number of origins in an origin group.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogmQuantity :: Lens.Lens' OriginGroupMembers Lude.Int
ogmQuantity = Lens.lens (quantity :: OriginGroupMembers -> Lude.Int) (\s a -> s {quantity = a} :: OriginGroupMembers)
{-# DEPRECATED ogmQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | Items (origins) in an origin group.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogmItems :: Lens.Lens' OriginGroupMembers (Lude.NonEmpty OriginGroupMember)
ogmItems = Lens.lens (items :: OriginGroupMembers -> Lude.NonEmpty OriginGroupMember) (\s a -> s {items = a} :: OriginGroupMembers)
{-# DEPRECATED ogmItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML OriginGroupMembers where
  parseXML x =
    OriginGroupMembers'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.parseXMLNonEmpty "OriginGroupMember"
               )

instance Lude.ToXML OriginGroupMembers where
  toXML OriginGroupMembers' {..} =
    Lude.mconcat
      [ "Quantity" Lude.@= quantity,
        "Items" Lude.@= Lude.toXMLList "OriginGroupMember" items
      ]
