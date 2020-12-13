{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyGroupList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyGroupList
  ( KeyGroupList (..),

    -- * Smart constructor
    mkKeyGroupList,

    -- * Lenses
    kglQuantity,
    kglItems,
    kglMaxItems,
    kglNextMarker,
  )
where

import Network.AWS.CloudFront.Types.KeyGroupSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of key groups.
--
-- /See:/ 'mkKeyGroupList' smart constructor.
data KeyGroupList = KeyGroupList'
  { -- | The number of key groups returned in the response.
    quantity :: Lude.Int,
    -- | A list of key groups.
    items :: Lude.Maybe [KeyGroupSummary],
    -- | The maximum number of key groups requested.
    maxItems :: Lude.Int,
    -- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing key groups.
    nextMarker :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeyGroupList' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of key groups returned in the response.
-- * 'items' - A list of key groups.
-- * 'maxItems' - The maximum number of key groups requested.
-- * 'nextMarker' - If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing key groups.
mkKeyGroupList ::
  -- | 'quantity'
  Lude.Int ->
  -- | 'maxItems'
  Lude.Int ->
  KeyGroupList
mkKeyGroupList pQuantity_ pMaxItems_ =
  KeyGroupList'
    { quantity = pQuantity_,
      items = Lude.Nothing,
      maxItems = pMaxItems_,
      nextMarker = Lude.Nothing
    }

-- | The number of key groups returned in the response.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kglQuantity :: Lens.Lens' KeyGroupList Lude.Int
kglQuantity = Lens.lens (quantity :: KeyGroupList -> Lude.Int) (\s a -> s {quantity = a} :: KeyGroupList)
{-# DEPRECATED kglQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A list of key groups.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kglItems :: Lens.Lens' KeyGroupList (Lude.Maybe [KeyGroupSummary])
kglItems = Lens.lens (items :: KeyGroupList -> Lude.Maybe [KeyGroupSummary]) (\s a -> s {items = a} :: KeyGroupList)
{-# DEPRECATED kglItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The maximum number of key groups requested.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kglMaxItems :: Lens.Lens' KeyGroupList Lude.Int
kglMaxItems = Lens.lens (maxItems :: KeyGroupList -> Lude.Int) (\s a -> s {maxItems = a} :: KeyGroupList)
{-# DEPRECATED kglMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing key groups.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kglNextMarker :: Lens.Lens' KeyGroupList (Lude.Maybe Lude.Text)
kglNextMarker = Lens.lens (nextMarker :: KeyGroupList -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: KeyGroupList)
{-# DEPRECATED kglNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

instance Lude.FromXML KeyGroupList where
  parseXML x =
    KeyGroupList'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "KeyGroupSummary")
               )
      Lude.<*> (x Lude..@ "MaxItems")
      Lude.<*> (x Lude..@? "NextMarker")
