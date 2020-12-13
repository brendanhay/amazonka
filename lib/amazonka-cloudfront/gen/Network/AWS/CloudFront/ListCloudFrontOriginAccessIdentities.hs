{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists origin access identities.
--
-- This operation returns paginated results.
module Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
  ( -- * Creating a request
    ListCloudFrontOriginAccessIdentities (..),
    mkListCloudFrontOriginAccessIdentities,

    -- ** Request lenses
    lcfoaiMarker,
    lcfoaiMaxItems,

    -- * Destructuring the response
    ListCloudFrontOriginAccessIdentitiesResponse (..),
    mkListCloudFrontOriginAccessIdentitiesResponse,

    -- ** Response lenses
    lcfoairsCloudFrontOriginAccessIdentityList,
    lcfoairsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to list origin access identities.
--
-- /See:/ 'mkListCloudFrontOriginAccessIdentities' smart constructor.
data ListCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentities'
  { -- | Use this when paginating results to indicate where to begin in your list of origin access identities. The results include identities in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last identity on that page).
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of origin access identities you want in the response body.
    maxItems :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCloudFrontOriginAccessIdentities' with the minimum fields required to make a request.
--
-- * 'marker' - Use this when paginating results to indicate where to begin in your list of origin access identities. The results include identities in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last identity on that page).
-- * 'maxItems' - The maximum number of origin access identities you want in the response body.
mkListCloudFrontOriginAccessIdentities ::
  ListCloudFrontOriginAccessIdentities
mkListCloudFrontOriginAccessIdentities =
  ListCloudFrontOriginAccessIdentities'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list of origin access identities. The results include identities in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last identity on that page).
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfoaiMarker :: Lens.Lens' ListCloudFrontOriginAccessIdentities (Lude.Maybe Lude.Text)
lcfoaiMarker = Lens.lens (marker :: ListCloudFrontOriginAccessIdentities -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListCloudFrontOriginAccessIdentities)
{-# DEPRECATED lcfoaiMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of origin access identities you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfoaiMaxItems :: Lens.Lens' ListCloudFrontOriginAccessIdentities (Lude.Maybe Lude.Text)
lcfoaiMaxItems = Lens.lens (maxItems :: ListCloudFrontOriginAccessIdentities -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListCloudFrontOriginAccessIdentities)
{-# DEPRECATED lcfoaiMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListCloudFrontOriginAccessIdentities where
  page rq rs
    | Page.stop
        ( rs
            Lens.^. lcfoairsCloudFrontOriginAccessIdentityList
              Lude.. cfoailIsTruncated
        ) =
      Lude.Nothing
    | Lude.isNothing
        ( rs
            Lens.^? lcfoairsCloudFrontOriginAccessIdentityList
              Lude.. cfoailNextMarker
              Lude.. Lens._Just
        ) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcfoaiMarker
          Lens..~ rs
          Lens.^? lcfoairsCloudFrontOriginAccessIdentityList
            Lude.. cfoailNextMarker
            Lude.. Lens._Just

instance Lude.AWSRequest ListCloudFrontOriginAccessIdentities where
  type
    Rs ListCloudFrontOriginAccessIdentities =
      ListCloudFrontOriginAccessIdentitiesResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListCloudFrontOriginAccessIdentitiesResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCloudFrontOriginAccessIdentities where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListCloudFrontOriginAccessIdentities where
  toPath = Lude.const "/2020-05-31/origin-access-identity/cloudfront"

instance Lude.ToQuery ListCloudFrontOriginAccessIdentities where
  toQuery ListCloudFrontOriginAccessIdentities' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkListCloudFrontOriginAccessIdentitiesResponse' smart constructor.
data ListCloudFrontOriginAccessIdentitiesResponse = ListCloudFrontOriginAccessIdentitiesResponse'
  { -- | The @CloudFrontOriginAccessIdentityList@ type.
    cloudFrontOriginAccessIdentityList :: CloudFrontOriginAccessIdentityList,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCloudFrontOriginAccessIdentitiesResponse' with the minimum fields required to make a request.
--
-- * 'cloudFrontOriginAccessIdentityList' - The @CloudFrontOriginAccessIdentityList@ type.
-- * 'responseStatus' - The response status code.
mkListCloudFrontOriginAccessIdentitiesResponse ::
  -- | 'cloudFrontOriginAccessIdentityList'
  CloudFrontOriginAccessIdentityList ->
  -- | 'responseStatus'
  Lude.Int ->
  ListCloudFrontOriginAccessIdentitiesResponse
mkListCloudFrontOriginAccessIdentitiesResponse
  pCloudFrontOriginAccessIdentityList_
  pResponseStatus_ =
    ListCloudFrontOriginAccessIdentitiesResponse'
      { cloudFrontOriginAccessIdentityList =
          pCloudFrontOriginAccessIdentityList_,
        responseStatus = pResponseStatus_
      }

-- | The @CloudFrontOriginAccessIdentityList@ type.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentityList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfoairsCloudFrontOriginAccessIdentityList :: Lens.Lens' ListCloudFrontOriginAccessIdentitiesResponse CloudFrontOriginAccessIdentityList
lcfoairsCloudFrontOriginAccessIdentityList = Lens.lens (cloudFrontOriginAccessIdentityList :: ListCloudFrontOriginAccessIdentitiesResponse -> CloudFrontOriginAccessIdentityList) (\s a -> s {cloudFrontOriginAccessIdentityList = a} :: ListCloudFrontOriginAccessIdentitiesResponse)
{-# DEPRECATED lcfoairsCloudFrontOriginAccessIdentityList "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentityList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfoairsResponseStatus :: Lens.Lens' ListCloudFrontOriginAccessIdentitiesResponse Lude.Int
lcfoairsResponseStatus = Lens.lens (responseStatus :: ListCloudFrontOriginAccessIdentitiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCloudFrontOriginAccessIdentitiesResponse)
{-# DEPRECATED lcfoairsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
