{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListDistributionsByCachePolicyId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distribution IDs for distributions that have a cache behavior that’s associated with the specified cache policy.
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByCachePolicyId
  ( -- * Creating a request
    ListDistributionsByCachePolicyId (..),
    mkListDistributionsByCachePolicyId,

    -- ** Request lenses
    ldbcpiMarker,
    ldbcpiMaxItems,
    ldbcpiCachePolicyId,

    -- * Destructuring the response
    ListDistributionsByCachePolicyIdResponse (..),
    mkListDistributionsByCachePolicyIdResponse,

    -- ** Response lenses
    ldbcpirsDistributionIdList,
    ldbcpirsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDistributionsByCachePolicyId' smart constructor.
data ListDistributionsByCachePolicyId = ListDistributionsByCachePolicyId'
  { -- | Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of distribution IDs that you want in the response.
    maxItems :: Lude.Maybe Lude.Text,
    -- | The ID of the cache policy whose associated distribution IDs you want to list.
    cachePolicyId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDistributionsByCachePolicyId' with the minimum fields required to make a request.
--
-- * 'marker' - Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
-- * 'maxItems' - The maximum number of distribution IDs that you want in the response.
-- * 'cachePolicyId' - The ID of the cache policy whose associated distribution IDs you want to list.
mkListDistributionsByCachePolicyId ::
  -- | 'cachePolicyId'
  Lude.Text ->
  ListDistributionsByCachePolicyId
mkListDistributionsByCachePolicyId pCachePolicyId_ =
  ListDistributionsByCachePolicyId'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      cachePolicyId = pCachePolicyId_
    }

-- | Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbcpiMarker :: Lens.Lens' ListDistributionsByCachePolicyId (Lude.Maybe Lude.Text)
ldbcpiMarker = Lens.lens (marker :: ListDistributionsByCachePolicyId -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListDistributionsByCachePolicyId)
{-# DEPRECATED ldbcpiMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of distribution IDs that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbcpiMaxItems :: Lens.Lens' ListDistributionsByCachePolicyId (Lude.Maybe Lude.Text)
ldbcpiMaxItems = Lens.lens (maxItems :: ListDistributionsByCachePolicyId -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListDistributionsByCachePolicyId)
{-# DEPRECATED ldbcpiMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The ID of the cache policy whose associated distribution IDs you want to list.
--
-- /Note:/ Consider using 'cachePolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbcpiCachePolicyId :: Lens.Lens' ListDistributionsByCachePolicyId Lude.Text
ldbcpiCachePolicyId = Lens.lens (cachePolicyId :: ListDistributionsByCachePolicyId -> Lude.Text) (\s a -> s {cachePolicyId = a} :: ListDistributionsByCachePolicyId)
{-# DEPRECATED ldbcpiCachePolicyId "Use generic-lens or generic-optics with 'cachePolicyId' instead." #-}

instance Lude.AWSRequest ListDistributionsByCachePolicyId where
  type
    Rs ListDistributionsByCachePolicyId =
      ListDistributionsByCachePolicyIdResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListDistributionsByCachePolicyIdResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDistributionsByCachePolicyId where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDistributionsByCachePolicyId where
  toPath ListDistributionsByCachePolicyId' {..} =
    Lude.mconcat
      [ "/2020-05-31/distributionsByCachePolicyId/",
        Lude.toBS cachePolicyId
      ]

instance Lude.ToQuery ListDistributionsByCachePolicyId where
  toQuery ListDistributionsByCachePolicyId' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | /See:/ 'mkListDistributionsByCachePolicyIdResponse' smart constructor.
data ListDistributionsByCachePolicyIdResponse = ListDistributionsByCachePolicyIdResponse'
  { -- | A list of distribution IDs.
    distributionIdList :: Lude.Maybe DistributionIdList,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDistributionsByCachePolicyIdResponse' with the minimum fields required to make a request.
--
-- * 'distributionIdList' - A list of distribution IDs.
-- * 'responseStatus' - The response status code.
mkListDistributionsByCachePolicyIdResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDistributionsByCachePolicyIdResponse
mkListDistributionsByCachePolicyIdResponse pResponseStatus_ =
  ListDistributionsByCachePolicyIdResponse'
    { distributionIdList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of distribution IDs.
--
-- /Note:/ Consider using 'distributionIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbcpirsDistributionIdList :: Lens.Lens' ListDistributionsByCachePolicyIdResponse (Lude.Maybe DistributionIdList)
ldbcpirsDistributionIdList = Lens.lens (distributionIdList :: ListDistributionsByCachePolicyIdResponse -> Lude.Maybe DistributionIdList) (\s a -> s {distributionIdList = a} :: ListDistributionsByCachePolicyIdResponse)
{-# DEPRECATED ldbcpirsDistributionIdList "Use generic-lens or generic-optics with 'distributionIdList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbcpirsResponseStatus :: Lens.Lens' ListDistributionsByCachePolicyIdResponse Lude.Int
ldbcpirsResponseStatus = Lens.lens (responseStatus :: ListDistributionsByCachePolicyIdResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDistributionsByCachePolicyIdResponse)
{-# DEPRECATED ldbcpirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
