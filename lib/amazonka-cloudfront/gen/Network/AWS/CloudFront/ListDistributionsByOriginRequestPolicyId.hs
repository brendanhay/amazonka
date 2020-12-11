{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListDistributionsByOriginRequestPolicyId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distribution IDs for distributions that have a cache behavior that’s associated with the specified origin request policy.
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByOriginRequestPolicyId
  ( -- * Creating a request
    ListDistributionsByOriginRequestPolicyId (..),
    mkListDistributionsByOriginRequestPolicyId,

    -- ** Request lenses
    ldborpiMarker,
    ldborpiMaxItems,
    ldborpiOriginRequestPolicyId,

    -- * Destructuring the response
    ListDistributionsByOriginRequestPolicyIdResponse (..),
    mkListDistributionsByOriginRequestPolicyIdResponse,

    -- ** Response lenses
    ldborpirsDistributionIdList,
    ldborpirsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDistributionsByOriginRequestPolicyId' smart constructor.
data ListDistributionsByOriginRequestPolicyId = ListDistributionsByOriginRequestPolicyId'
  { marker ::
      Lude.Maybe
        Lude.Text,
    maxItems ::
      Lude.Maybe
        Lude.Text,
    originRequestPolicyId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDistributionsByOriginRequestPolicyId' with the minimum fields required to make a request.
--
-- * 'marker' - Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
-- * 'maxItems' - The maximum number of distribution IDs that you want in the response.
-- * 'originRequestPolicyId' - The ID of the origin request policy whose associated distribution IDs you want to list.
mkListDistributionsByOriginRequestPolicyId ::
  -- | 'originRequestPolicyId'
  Lude.Text ->
  ListDistributionsByOriginRequestPolicyId
mkListDistributionsByOriginRequestPolicyId pOriginRequestPolicyId_ =
  ListDistributionsByOriginRequestPolicyId'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      originRequestPolicyId = pOriginRequestPolicyId_
    }

-- | Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldborpiMarker :: Lens.Lens' ListDistributionsByOriginRequestPolicyId (Lude.Maybe Lude.Text)
ldborpiMarker = Lens.lens (marker :: ListDistributionsByOriginRequestPolicyId -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListDistributionsByOriginRequestPolicyId)
{-# DEPRECATED ldborpiMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of distribution IDs that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldborpiMaxItems :: Lens.Lens' ListDistributionsByOriginRequestPolicyId (Lude.Maybe Lude.Text)
ldborpiMaxItems = Lens.lens (maxItems :: ListDistributionsByOriginRequestPolicyId -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListDistributionsByOriginRequestPolicyId)
{-# DEPRECATED ldborpiMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The ID of the origin request policy whose associated distribution IDs you want to list.
--
-- /Note:/ Consider using 'originRequestPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldborpiOriginRequestPolicyId :: Lens.Lens' ListDistributionsByOriginRequestPolicyId Lude.Text
ldborpiOriginRequestPolicyId = Lens.lens (originRequestPolicyId :: ListDistributionsByOriginRequestPolicyId -> Lude.Text) (\s a -> s {originRequestPolicyId = a} :: ListDistributionsByOriginRequestPolicyId)
{-# DEPRECATED ldborpiOriginRequestPolicyId "Use generic-lens or generic-optics with 'originRequestPolicyId' instead." #-}

instance Lude.AWSRequest ListDistributionsByOriginRequestPolicyId where
  type
    Rs ListDistributionsByOriginRequestPolicyId =
      ListDistributionsByOriginRequestPolicyIdResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListDistributionsByOriginRequestPolicyIdResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDistributionsByOriginRequestPolicyId where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDistributionsByOriginRequestPolicyId where
  toPath ListDistributionsByOriginRequestPolicyId' {..} =
    Lude.mconcat
      [ "/2020-05-31/distributionsByOriginRequestPolicyId/",
        Lude.toBS originRequestPolicyId
      ]

instance Lude.ToQuery ListDistributionsByOriginRequestPolicyId where
  toQuery ListDistributionsByOriginRequestPolicyId' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | /See:/ 'mkListDistributionsByOriginRequestPolicyIdResponse' smart constructor.
data ListDistributionsByOriginRequestPolicyIdResponse = ListDistributionsByOriginRequestPolicyIdResponse'
  { distributionIdList ::
      Lude.Maybe
        DistributionIdList,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'ListDistributionsByOriginRequestPolicyIdResponse' with the minimum fields required to make a request.
--
-- * 'distributionIdList' - A list of distribution IDs.
-- * 'responseStatus' - The response status code.
mkListDistributionsByOriginRequestPolicyIdResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDistributionsByOriginRequestPolicyIdResponse
mkListDistributionsByOriginRequestPolicyIdResponse pResponseStatus_ =
  ListDistributionsByOriginRequestPolicyIdResponse'
    { distributionIdList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of distribution IDs.
--
-- /Note:/ Consider using 'distributionIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldborpirsDistributionIdList :: Lens.Lens' ListDistributionsByOriginRequestPolicyIdResponse (Lude.Maybe DistributionIdList)
ldborpirsDistributionIdList = Lens.lens (distributionIdList :: ListDistributionsByOriginRequestPolicyIdResponse -> Lude.Maybe DistributionIdList) (\s a -> s {distributionIdList = a} :: ListDistributionsByOriginRequestPolicyIdResponse)
{-# DEPRECATED ldborpirsDistributionIdList "Use generic-lens or generic-optics with 'distributionIdList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldborpirsResponseStatus :: Lens.Lens' ListDistributionsByOriginRequestPolicyIdResponse Lude.Int
ldborpirsResponseStatus = Lens.lens (responseStatus :: ListDistributionsByOriginRequestPolicyIdResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDistributionsByOriginRequestPolicyIdResponse)
{-# DEPRECATED ldborpirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
