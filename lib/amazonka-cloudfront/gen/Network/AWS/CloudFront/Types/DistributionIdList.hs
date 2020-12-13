{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DistributionIdList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionIdList
  ( DistributionIdList (..),

    -- * Smart constructor
    mkDistributionIdList,

    -- * Lenses
    dilQuantity,
    dilItems,
    dilMarker,
    dilMaxItems,
    dilNextMarker,
    dilIsTruncated,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of distribution IDs.
--
-- /See:/ 'mkDistributionIdList' smart constructor.
data DistributionIdList = DistributionIdList'
  { -- | The total number of distribution IDs returned in the response.
    quantity :: Lude.Int,
    -- | Contains the distribution IDs in the list.
    items :: Lude.Maybe [Lude.Text],
    -- | The value provided in the @Marker@ request field.
    marker :: Lude.Text,
    -- | The maximum number of distribution IDs requested.
    maxItems :: Lude.Int,
    -- | Contains the value that you should use in the @Marker@ field of a subsequent request to continue listing distribution IDs where you left off.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether more distribution IDs remain to be listed. If your results were truncated, you can make a subsequent request using the @Marker@ request field to retrieve more distribution IDs in the list.
    isTruncated :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DistributionIdList' with the minimum fields required to make a request.
--
-- * 'quantity' - The total number of distribution IDs returned in the response.
-- * 'items' - Contains the distribution IDs in the list.
-- * 'marker' - The value provided in the @Marker@ request field.
-- * 'maxItems' - The maximum number of distribution IDs requested.
-- * 'nextMarker' - Contains the value that you should use in the @Marker@ field of a subsequent request to continue listing distribution IDs where you left off.
-- * 'isTruncated' - A flag that indicates whether more distribution IDs remain to be listed. If your results were truncated, you can make a subsequent request using the @Marker@ request field to retrieve more distribution IDs in the list.
mkDistributionIdList ::
  -- | 'quantity'
  Lude.Int ->
  -- | 'marker'
  Lude.Text ->
  -- | 'maxItems'
  Lude.Int ->
  -- | 'isTruncated'
  Lude.Bool ->
  DistributionIdList
mkDistributionIdList pQuantity_ pMarker_ pMaxItems_ pIsTruncated_ =
  DistributionIdList'
    { quantity = pQuantity_,
      items = Lude.Nothing,
      marker = pMarker_,
      maxItems = pMaxItems_,
      nextMarker = Lude.Nothing,
      isTruncated = pIsTruncated_
    }

-- | The total number of distribution IDs returned in the response.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dilQuantity :: Lens.Lens' DistributionIdList Lude.Int
dilQuantity = Lens.lens (quantity :: DistributionIdList -> Lude.Int) (\s a -> s {quantity = a} :: DistributionIdList)
{-# DEPRECATED dilQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | Contains the distribution IDs in the list.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dilItems :: Lens.Lens' DistributionIdList (Lude.Maybe [Lude.Text])
dilItems = Lens.lens (items :: DistributionIdList -> Lude.Maybe [Lude.Text]) (\s a -> s {items = a} :: DistributionIdList)
{-# DEPRECATED dilItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The value provided in the @Marker@ request field.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dilMarker :: Lens.Lens' DistributionIdList Lude.Text
dilMarker = Lens.lens (marker :: DistributionIdList -> Lude.Text) (\s a -> s {marker = a} :: DistributionIdList)
{-# DEPRECATED dilMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of distribution IDs requested.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dilMaxItems :: Lens.Lens' DistributionIdList Lude.Int
dilMaxItems = Lens.lens (maxItems :: DistributionIdList -> Lude.Int) (\s a -> s {maxItems = a} :: DistributionIdList)
{-# DEPRECATED dilMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | Contains the value that you should use in the @Marker@ field of a subsequent request to continue listing distribution IDs where you left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dilNextMarker :: Lens.Lens' DistributionIdList (Lude.Maybe Lude.Text)
dilNextMarker = Lens.lens (nextMarker :: DistributionIdList -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: DistributionIdList)
{-# DEPRECATED dilNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | A flag that indicates whether more distribution IDs remain to be listed. If your results were truncated, you can make a subsequent request using the @Marker@ request field to retrieve more distribution IDs in the list.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dilIsTruncated :: Lens.Lens' DistributionIdList Lude.Bool
dilIsTruncated = Lens.lens (isTruncated :: DistributionIdList -> Lude.Bool) (\s a -> s {isTruncated = a} :: DistributionIdList)
{-# DEPRECATED dilIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

instance Lude.FromXML DistributionIdList where
  parseXML x =
    DistributionIdList'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DistributionId")
               )
      Lude.<*> (x Lude..@ "Marker")
      Lude.<*> (x Lude..@ "MaxItems")
      Lude.<*> (x Lude..@? "NextMarker")
      Lude.<*> (x Lude..@ "IsTruncated")
