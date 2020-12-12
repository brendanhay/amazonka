{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyList
  ( CachePolicyList (..),

    -- * Smart constructor
    mkCachePolicyList,

    -- * Lenses
    cplItems,
    cplNextMarker,
    cplMaxItems,
    cplQuantity,
  )
where

import Network.AWS.CloudFront.Types.CachePolicySummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of cache policies.
--
-- /See:/ 'mkCachePolicyList' smart constructor.
data CachePolicyList = CachePolicyList'
  { items ::
      Lude.Maybe [CachePolicySummary],
    nextMarker :: Lude.Maybe Lude.Text,
    maxItems :: Lude.Int,
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

-- | Creates a value of 'CachePolicyList' with the minimum fields required to make a request.
--
-- * 'items' - Contains the cache policies in the list.
-- * 'maxItems' - The maximum number of cache policies requested.
-- * 'nextMarker' - If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing cache policies where you left off.
-- * 'quantity' - The total number of cache policies returned in the response.
mkCachePolicyList ::
  -- | 'maxItems'
  Lude.Int ->
  -- | 'quantity'
  Lude.Int ->
  CachePolicyList
mkCachePolicyList pMaxItems_ pQuantity_ =
  CachePolicyList'
    { items = Lude.Nothing,
      nextMarker = Lude.Nothing,
      maxItems = pMaxItems_,
      quantity = pQuantity_
    }

-- | Contains the cache policies in the list.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cplItems :: Lens.Lens' CachePolicyList (Lude.Maybe [CachePolicySummary])
cplItems = Lens.lens (items :: CachePolicyList -> Lude.Maybe [CachePolicySummary]) (\s a -> s {items = a} :: CachePolicyList)
{-# DEPRECATED cplItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing cache policies where you left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cplNextMarker :: Lens.Lens' CachePolicyList (Lude.Maybe Lude.Text)
cplNextMarker = Lens.lens (nextMarker :: CachePolicyList -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: CachePolicyList)
{-# DEPRECATED cplNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The maximum number of cache policies requested.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cplMaxItems :: Lens.Lens' CachePolicyList Lude.Int
cplMaxItems = Lens.lens (maxItems :: CachePolicyList -> Lude.Int) (\s a -> s {maxItems = a} :: CachePolicyList)
{-# DEPRECATED cplMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The total number of cache policies returned in the response.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cplQuantity :: Lens.Lens' CachePolicyList Lude.Int
cplQuantity = Lens.lens (quantity :: CachePolicyList -> Lude.Int) (\s a -> s {quantity = a} :: CachePolicyList)
{-# DEPRECATED cplQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

instance Lude.FromXML CachePolicyList where
  parseXML x =
    CachePolicyList'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "CachePolicySummary")
               )
      Lude.<*> (x Lude..@? "NextMarker")
      Lude.<*> (x Lude..@ "MaxItems")
      Lude.<*> (x Lude..@ "Quantity")
