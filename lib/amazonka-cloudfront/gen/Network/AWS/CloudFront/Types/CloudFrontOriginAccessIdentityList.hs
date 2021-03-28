{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityList
  ( CloudFrontOriginAccessIdentityList (..)
  -- * Smart constructor
  , mkCloudFrontOriginAccessIdentityList
  -- * Lenses
  , cfoailMarker
  , cfoailMaxItems
  , cfoailIsTruncated
  , cfoailQuantity
  , cfoailItems
  , cfoailNextMarker
  ) where

import qualified Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentitySummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Lists the origin access identities for CloudFront.Send a @GET@ request to the @//CloudFront API version/ /origin-access-identity/cloudfront@ resource. The response includes a @CloudFrontOriginAccessIdentityList@ element with zero or more @CloudFrontOriginAccessIdentitySummary@ child elements. By default, your entire list of origin access identities is returned in one single page. If the list is long, you can paginate it using the @MaxItems@ and @Marker@ parameters.
--
-- /See:/ 'mkCloudFrontOriginAccessIdentityList' smart constructor.
data CloudFrontOriginAccessIdentityList = CloudFrontOriginAccessIdentityList'
  { marker :: Core.Text
    -- ^ Use this when paginating results to indicate where to begin in your list of origin access identities. The results include identities in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last identity on that page). 
  , maxItems :: Core.Int
    -- ^ The maximum number of origin access identities you want in the response body. 
  , isTruncated :: Core.Bool
    -- ^ A flag that indicates whether more origin access identities remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more items in the list.
  , quantity :: Core.Int
    -- ^ The number of CloudFront origin access identities that were created by the current AWS account. 
  , items :: Core.Maybe [Types.CloudFrontOriginAccessIdentitySummary]
    -- ^ A complex type that contains one @CloudFrontOriginAccessIdentitySummary@ element for each origin access identity that was created by the current AWS account.
  , nextMarker :: Core.Maybe Core.Text
    -- ^ If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your origin access identities where they left off. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudFrontOriginAccessIdentityList' value with any optional fields omitted.
mkCloudFrontOriginAccessIdentityList
    :: Core.Text -- ^ 'marker'
    -> Core.Int -- ^ 'maxItems'
    -> Core.Bool -- ^ 'isTruncated'
    -> Core.Int -- ^ 'quantity'
    -> CloudFrontOriginAccessIdentityList
mkCloudFrontOriginAccessIdentityList marker maxItems isTruncated
  quantity
  = CloudFrontOriginAccessIdentityList'{marker, maxItems,
                                        isTruncated, quantity, items = Core.Nothing,
                                        nextMarker = Core.Nothing}

-- | Use this when paginating results to indicate where to begin in your list of origin access identities. The results include identities in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last identity on that page). 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoailMarker :: Lens.Lens' CloudFrontOriginAccessIdentityList Core.Text
cfoailMarker = Lens.field @"marker"
{-# INLINEABLE cfoailMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of origin access identities you want in the response body. 
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoailMaxItems :: Lens.Lens' CloudFrontOriginAccessIdentityList Core.Int
cfoailMaxItems = Lens.field @"maxItems"
{-# INLINEABLE cfoailMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | A flag that indicates whether more origin access identities remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more items in the list.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoailIsTruncated :: Lens.Lens' CloudFrontOriginAccessIdentityList Core.Bool
cfoailIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE cfoailIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | The number of CloudFront origin access identities that were created by the current AWS account. 
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoailQuantity :: Lens.Lens' CloudFrontOriginAccessIdentityList Core.Int
cfoailQuantity = Lens.field @"quantity"
{-# INLINEABLE cfoailQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | A complex type that contains one @CloudFrontOriginAccessIdentitySummary@ element for each origin access identity that was created by the current AWS account.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoailItems :: Lens.Lens' CloudFrontOriginAccessIdentityList (Core.Maybe [Types.CloudFrontOriginAccessIdentitySummary])
cfoailItems = Lens.field @"items"
{-# INLINEABLE cfoailItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your origin access identities where they left off. 
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoailNextMarker :: Lens.Lens' CloudFrontOriginAccessIdentityList (Core.Maybe Core.Text)
cfoailNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE cfoailNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.FromXML CloudFrontOriginAccessIdentityList where
        parseXML x
          = CloudFrontOriginAccessIdentityList' Core.<$>
              (x Core..@ "Marker") Core.<*> x Core..@ "MaxItems" Core.<*>
                x Core..@ "IsTruncated"
                Core.<*> x Core..@ "Quantity"
                Core.<*>
                x Core..@? "Items" Core..<@>
                  Core.parseXMLList "CloudFrontOriginAccessIdentitySummary"
                Core.<*> x Core..@? "NextMarker"
