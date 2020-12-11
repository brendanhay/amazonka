{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListDistributions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List CloudFront distributions.
--
-- This operation returns paginated results.
module Network.AWS.CloudFront.ListDistributions
  ( -- * Creating a request
    ListDistributions (..),
    mkListDistributions,

    -- ** Request lenses
    ldMarker,
    ldMaxItems,

    -- * Destructuring the response
    ListDistributionsResponse (..),
    mkListDistributionsResponse,

    -- ** Response lenses
    ldrsResponseStatus,
    ldrsDistributionList,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to list your distributions.
--
-- /See:/ 'mkListDistributions' smart constructor.
data ListDistributions = ListDistributions'
  { marker ::
      Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDistributions' with the minimum fields required to make a request.
--
-- * 'marker' - Use this when paginating results to indicate where to begin in your list of distributions. The results include distributions in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last distribution on that page).
-- * 'maxItems' - The maximum number of distributions you want in the response body.
mkListDistributions ::
  ListDistributions
mkListDistributions =
  ListDistributions'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list of distributions. The results include distributions in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last distribution on that page).
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMarker :: Lens.Lens' ListDistributions (Lude.Maybe Lude.Text)
ldMarker = Lens.lens (marker :: ListDistributions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListDistributions)
{-# DEPRECATED ldMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of distributions you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxItems :: Lens.Lens' ListDistributions (Lude.Maybe Lude.Text)
ldMaxItems = Lens.lens (maxItems :: ListDistributions -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListDistributions)
{-# DEPRECATED ldMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListDistributions where
  page rq rs
    | Page.stop (rs Lens.^. ldrsDistributionList Lude.. dlIsTruncated) =
      Lude.Nothing
    | Lude.isNothing
        ( rs
            Lens.^? ldrsDistributionList Lude.. dlNextMarker Lude.. Lens._Just
        ) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldMarker
          Lens..~ rs
          Lens.^? ldrsDistributionList Lude.. dlNextMarker Lude.. Lens._Just

instance Lude.AWSRequest ListDistributions where
  type Rs ListDistributions = ListDistributionsResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListDistributionsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.parseXML x)
      )

instance Lude.ToHeaders ListDistributions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDistributions where
  toPath = Lude.const "/2020-05-31/distribution"

instance Lude.ToQuery ListDistributions where
  toQuery ListDistributions' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkListDistributionsResponse' smart constructor.
data ListDistributionsResponse = ListDistributionsResponse'
  { responseStatus ::
      Lude.Int,
    distributionList :: DistributionList
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDistributionsResponse' with the minimum fields required to make a request.
--
-- * 'distributionList' - The @DistributionList@ type.
-- * 'responseStatus' - The response status code.
mkListDistributionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'distributionList'
  DistributionList ->
  ListDistributionsResponse
mkListDistributionsResponse pResponseStatus_ pDistributionList_ =
  ListDistributionsResponse'
    { responseStatus = pResponseStatus_,
      distributionList = pDistributionList_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDistributionsResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDistributionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDistributionsResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The @DistributionList@ type.
--
-- /Note:/ Consider using 'distributionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDistributionList :: Lens.Lens' ListDistributionsResponse DistributionList
ldrsDistributionList = Lens.lens (distributionList :: ListDistributionsResponse -> DistributionList) (\s a -> s {distributionList = a} :: ListDistributionsResponse)
{-# DEPRECATED ldrsDistributionList "Use generic-lens or generic-optics with 'distributionList' instead." #-}
