{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityList
  ( CloudFrontOriginAccessIdentityList (..),

    -- * Smart constructor
    mkCloudFrontOriginAccessIdentityList,

    -- * Lenses
    cfoailQuantity,
    cfoailItems,
    cfoailMarker,
    cfoailMaxItems,
    cfoailNextMarker,
    cfoailIsTruncated,
  )
where

import Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentitySummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Lists the origin access identities for CloudFront.Send a @GET@ request to the @//CloudFront API version/ /origin-access-identity/cloudfront@ resource. The response includes a @CloudFrontOriginAccessIdentityList@ element with zero or more @CloudFrontOriginAccessIdentitySummary@ child elements. By default, your entire list of origin access identities is returned in one single page. If the list is long, you can paginate it using the @MaxItems@ and @Marker@ parameters.
--
-- /See:/ 'mkCloudFrontOriginAccessIdentityList' smart constructor.
data CloudFrontOriginAccessIdentityList = CloudFrontOriginAccessIdentityList'
  { -- | The number of CloudFront origin access identities that were created by the current AWS account.
    quantity :: Lude.Int,
    -- | A complex type that contains one @CloudFrontOriginAccessIdentitySummary@ element for each origin access identity that was created by the current AWS account.
    items :: Lude.Maybe [CloudFrontOriginAccessIdentitySummary],
    -- | Use this when paginating results to indicate where to begin in your list of origin access identities. The results include identities in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last identity on that page).
    marker :: Lude.Text,
    -- | The maximum number of origin access identities you want in the response body.
    maxItems :: Lude.Int,
    -- | If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your origin access identities where they left off.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether more origin access identities remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more items in the list.
    isTruncated :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudFrontOriginAccessIdentityList' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of CloudFront origin access identities that were created by the current AWS account.
-- * 'items' - A complex type that contains one @CloudFrontOriginAccessIdentitySummary@ element for each origin access identity that was created by the current AWS account.
-- * 'marker' - Use this when paginating results to indicate where to begin in your list of origin access identities. The results include identities in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last identity on that page).
-- * 'maxItems' - The maximum number of origin access identities you want in the response body.
-- * 'nextMarker' - If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your origin access identities where they left off.
-- * 'isTruncated' - A flag that indicates whether more origin access identities remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more items in the list.
mkCloudFrontOriginAccessIdentityList ::
  -- | 'quantity'
  Lude.Int ->
  -- | 'marker'
  Lude.Text ->
  -- | 'maxItems'
  Lude.Int ->
  -- | 'isTruncated'
  Lude.Bool ->
  CloudFrontOriginAccessIdentityList
mkCloudFrontOriginAccessIdentityList
  pQuantity_
  pMarker_
  pMaxItems_
  pIsTruncated_ =
    CloudFrontOriginAccessIdentityList'
      { quantity = pQuantity_,
        items = Lude.Nothing,
        marker = pMarker_,
        maxItems = pMaxItems_,
        nextMarker = Lude.Nothing,
        isTruncated = pIsTruncated_
      }

-- | The number of CloudFront origin access identities that were created by the current AWS account.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoailQuantity :: Lens.Lens' CloudFrontOriginAccessIdentityList Lude.Int
cfoailQuantity = Lens.lens (quantity :: CloudFrontOriginAccessIdentityList -> Lude.Int) (\s a -> s {quantity = a} :: CloudFrontOriginAccessIdentityList)
{-# DEPRECATED cfoailQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A complex type that contains one @CloudFrontOriginAccessIdentitySummary@ element for each origin access identity that was created by the current AWS account.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoailItems :: Lens.Lens' CloudFrontOriginAccessIdentityList (Lude.Maybe [CloudFrontOriginAccessIdentitySummary])
cfoailItems = Lens.lens (items :: CloudFrontOriginAccessIdentityList -> Lude.Maybe [CloudFrontOriginAccessIdentitySummary]) (\s a -> s {items = a} :: CloudFrontOriginAccessIdentityList)
{-# DEPRECATED cfoailItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Use this when paginating results to indicate where to begin in your list of origin access identities. The results include identities in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last identity on that page).
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoailMarker :: Lens.Lens' CloudFrontOriginAccessIdentityList Lude.Text
cfoailMarker = Lens.lens (marker :: CloudFrontOriginAccessIdentityList -> Lude.Text) (\s a -> s {marker = a} :: CloudFrontOriginAccessIdentityList)
{-# DEPRECATED cfoailMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of origin access identities you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoailMaxItems :: Lens.Lens' CloudFrontOriginAccessIdentityList Lude.Int
cfoailMaxItems = Lens.lens (maxItems :: CloudFrontOriginAccessIdentityList -> Lude.Int) (\s a -> s {maxItems = a} :: CloudFrontOriginAccessIdentityList)
{-# DEPRECATED cfoailMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your origin access identities where they left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoailNextMarker :: Lens.Lens' CloudFrontOriginAccessIdentityList (Lude.Maybe Lude.Text)
cfoailNextMarker = Lens.lens (nextMarker :: CloudFrontOriginAccessIdentityList -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: CloudFrontOriginAccessIdentityList)
{-# DEPRECATED cfoailNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | A flag that indicates whether more origin access identities remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more items in the list.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoailIsTruncated :: Lens.Lens' CloudFrontOriginAccessIdentityList Lude.Bool
cfoailIsTruncated = Lens.lens (isTruncated :: CloudFrontOriginAccessIdentityList -> Lude.Bool) (\s a -> s {isTruncated = a} :: CloudFrontOriginAccessIdentityList)
{-# DEPRECATED cfoailIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

instance Lude.FromXML CloudFrontOriginAccessIdentityList where
  parseXML x =
    CloudFrontOriginAccessIdentityList'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may
                     (Lude.parseXMLList "CloudFrontOriginAccessIdentitySummary")
               )
      Lude.<*> (x Lude..@ "Marker")
      Lude.<*> (x Lude..@ "MaxItems")
      Lude.<*> (x Lude..@? "NextMarker")
      Lude.<*> (x Lude..@ "IsTruncated")
