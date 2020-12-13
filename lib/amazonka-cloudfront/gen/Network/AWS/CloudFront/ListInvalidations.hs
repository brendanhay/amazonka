{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListInvalidations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists invalidation batches.
--
-- This operation returns paginated results.
module Network.AWS.CloudFront.ListInvalidations
  ( -- * Creating a request
    ListInvalidations (..),
    mkListInvalidations,

    -- ** Request lenses
    liDistributionId,
    liMarker,
    liMaxItems,

    -- * Destructuring the response
    ListInvalidationsResponse (..),
    mkListInvalidationsResponse,

    -- ** Response lenses
    lirsInvalidationList,
    lirsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to list invalidations.
--
-- /See:/ 'mkListInvalidations' smart constructor.
data ListInvalidations = ListInvalidations'
  { -- | The distribution's ID.
    distributionId :: Lude.Text,
    -- | Use this parameter when paginating results to indicate where to begin in your list of invalidation batches. Because the results are returned in decreasing order from most recent to oldest, the most recent results are on the first page, the second page will contain earlier results, and so on. To get the next page of results, set @Marker@ to the value of the @NextMarker@ from the current page's response. This value is the same as the ID of the last invalidation batch on that page.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of invalidation batches that you want in the response body.
    maxItems :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInvalidations' with the minimum fields required to make a request.
--
-- * 'distributionId' - The distribution's ID.
-- * 'marker' - Use this parameter when paginating results to indicate where to begin in your list of invalidation batches. Because the results are returned in decreasing order from most recent to oldest, the most recent results are on the first page, the second page will contain earlier results, and so on. To get the next page of results, set @Marker@ to the value of the @NextMarker@ from the current page's response. This value is the same as the ID of the last invalidation batch on that page.
-- * 'maxItems' - The maximum number of invalidation batches that you want in the response body.
mkListInvalidations ::
  -- | 'distributionId'
  Lude.Text ->
  ListInvalidations
mkListInvalidations pDistributionId_ =
  ListInvalidations'
    { distributionId = pDistributionId_,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The distribution's ID.
--
-- /Note:/ Consider using 'distributionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liDistributionId :: Lens.Lens' ListInvalidations Lude.Text
liDistributionId = Lens.lens (distributionId :: ListInvalidations -> Lude.Text) (\s a -> s {distributionId = a} :: ListInvalidations)
{-# DEPRECATED liDistributionId "Use generic-lens or generic-optics with 'distributionId' instead." #-}

-- | Use this parameter when paginating results to indicate where to begin in your list of invalidation batches. Because the results are returned in decreasing order from most recent to oldest, the most recent results are on the first page, the second page will contain earlier results, and so on. To get the next page of results, set @Marker@ to the value of the @NextMarker@ from the current page's response. This value is the same as the ID of the last invalidation batch on that page.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMarker :: Lens.Lens' ListInvalidations (Lude.Maybe Lude.Text)
liMarker = Lens.lens (marker :: ListInvalidations -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListInvalidations)
{-# DEPRECATED liMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of invalidation batches that you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxItems :: Lens.Lens' ListInvalidations (Lude.Maybe Lude.Text)
liMaxItems = Lens.lens (maxItems :: ListInvalidations -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListInvalidations)
{-# DEPRECATED liMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListInvalidations where
  page rq rs
    | Page.stop (rs Lens.^. lirsInvalidationList Lude.. ilIsTruncated) =
      Lude.Nothing
    | Lude.isNothing
        ( rs
            Lens.^? lirsInvalidationList Lude.. ilNextMarker Lude.. Lens._Just
        ) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& liMarker
          Lens..~ rs
          Lens.^? lirsInvalidationList Lude.. ilNextMarker Lude.. Lens._Just

instance Lude.AWSRequest ListInvalidations where
  type Rs ListInvalidations = ListInvalidationsResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListInvalidationsResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInvalidations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListInvalidations where
  toPath ListInvalidations' {..} =
    Lude.mconcat
      [ "/2020-05-31/distribution/",
        Lude.toBS distributionId,
        "/invalidation"
      ]

instance Lude.ToQuery ListInvalidations where
  toQuery ListInvalidations' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkListInvalidationsResponse' smart constructor.
data ListInvalidationsResponse = ListInvalidationsResponse'
  { -- | Information about invalidation batches.
    invalidationList :: InvalidationList,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInvalidationsResponse' with the minimum fields required to make a request.
--
-- * 'invalidationList' - Information about invalidation batches.
-- * 'responseStatus' - The response status code.
mkListInvalidationsResponse ::
  -- | 'invalidationList'
  InvalidationList ->
  -- | 'responseStatus'
  Lude.Int ->
  ListInvalidationsResponse
mkListInvalidationsResponse pInvalidationList_ pResponseStatus_ =
  ListInvalidationsResponse'
    { invalidationList = pInvalidationList_,
      responseStatus = pResponseStatus_
    }

-- | Information about invalidation batches.
--
-- /Note:/ Consider using 'invalidationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsInvalidationList :: Lens.Lens' ListInvalidationsResponse InvalidationList
lirsInvalidationList = Lens.lens (invalidationList :: ListInvalidationsResponse -> InvalidationList) (\s a -> s {invalidationList = a} :: ListInvalidationsResponse)
{-# DEPRECATED lirsInvalidationList "Use generic-lens or generic-optics with 'invalidationList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsResponseStatus :: Lens.Lens' ListInvalidationsResponse Lude.Int
lirsResponseStatus = Lens.lens (responseStatus :: ListInvalidationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInvalidationsResponse)
{-# DEPRECATED lirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
