{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DistributionList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionList
  ( DistributionList (..),

    -- * Smart constructor
    mkDistributionList,

    -- * Lenses
    dlQuantity,
    dlItems,
    dlMarker,
    dlMaxItems,
    dlNextMarker,
    dlIsTruncated,
  )
where

import Network.AWS.CloudFront.Types.DistributionSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A distribution list.
--
-- /See:/ 'mkDistributionList' smart constructor.
data DistributionList = DistributionList'
  { -- | The number of distributions that were created by the current AWS account.
    quantity :: Lude.Int,
    -- | A complex type that contains one @DistributionSummary@ element for each distribution that was created by the current AWS account.
    items :: Lude.Maybe [DistributionSummary],
    -- | The value you provided for the @Marker@ request parameter.
    marker :: Lude.Text,
    -- | The value you provided for the @MaxItems@ request parameter.
    maxItems :: Lude.Int,
    -- | If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your distributions where they left off.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether more distributions remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more distributions in the list.
    isTruncated :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DistributionList' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of distributions that were created by the current AWS account.
-- * 'items' - A complex type that contains one @DistributionSummary@ element for each distribution that was created by the current AWS account.
-- * 'marker' - The value you provided for the @Marker@ request parameter.
-- * 'maxItems' - The value you provided for the @MaxItems@ request parameter.
-- * 'nextMarker' - If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your distributions where they left off.
-- * 'isTruncated' - A flag that indicates whether more distributions remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more distributions in the list.
mkDistributionList ::
  -- | 'quantity'
  Lude.Int ->
  -- | 'marker'
  Lude.Text ->
  -- | 'maxItems'
  Lude.Int ->
  -- | 'isTruncated'
  Lude.Bool ->
  DistributionList
mkDistributionList pQuantity_ pMarker_ pMaxItems_ pIsTruncated_ =
  DistributionList'
    { quantity = pQuantity_,
      items = Lude.Nothing,
      marker = pMarker_,
      maxItems = pMaxItems_,
      nextMarker = Lude.Nothing,
      isTruncated = pIsTruncated_
    }

-- | The number of distributions that were created by the current AWS account.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlQuantity :: Lens.Lens' DistributionList Lude.Int
dlQuantity = Lens.lens (quantity :: DistributionList -> Lude.Int) (\s a -> s {quantity = a} :: DistributionList)
{-# DEPRECATED dlQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A complex type that contains one @DistributionSummary@ element for each distribution that was created by the current AWS account.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlItems :: Lens.Lens' DistributionList (Lude.Maybe [DistributionSummary])
dlItems = Lens.lens (items :: DistributionList -> Lude.Maybe [DistributionSummary]) (\s a -> s {items = a} :: DistributionList)
{-# DEPRECATED dlItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The value you provided for the @Marker@ request parameter.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlMarker :: Lens.Lens' DistributionList Lude.Text
dlMarker = Lens.lens (marker :: DistributionList -> Lude.Text) (\s a -> s {marker = a} :: DistributionList)
{-# DEPRECATED dlMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The value you provided for the @MaxItems@ request parameter.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlMaxItems :: Lens.Lens' DistributionList Lude.Int
dlMaxItems = Lens.lens (maxItems :: DistributionList -> Lude.Int) (\s a -> s {maxItems = a} :: DistributionList)
{-# DEPRECATED dlMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your distributions where they left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlNextMarker :: Lens.Lens' DistributionList (Lude.Maybe Lude.Text)
dlNextMarker = Lens.lens (nextMarker :: DistributionList -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: DistributionList)
{-# DEPRECATED dlNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | A flag that indicates whether more distributions remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more distributions in the list.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlIsTruncated :: Lens.Lens' DistributionList Lude.Bool
dlIsTruncated = Lens.lens (isTruncated :: DistributionList -> Lude.Bool) (\s a -> s {isTruncated = a} :: DistributionList)
{-# DEPRECATED dlIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

instance Lude.FromXML DistributionList where
  parseXML x =
    DistributionList'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DistributionSummary")
               )
      Lude.<*> (x Lude..@ "Marker")
      Lude.<*> (x Lude..@ "MaxItems")
      Lude.<*> (x Lude..@? "NextMarker")
      Lude.<*> (x Lude..@ "IsTruncated")
