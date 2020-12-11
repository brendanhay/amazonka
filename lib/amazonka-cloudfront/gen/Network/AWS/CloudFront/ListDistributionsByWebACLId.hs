{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListDistributionsByWebACLId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the distributions that are associated with a specified AWS WAF web ACL.
module Network.AWS.CloudFront.ListDistributionsByWebACLId
  ( -- * Creating a request
    ListDistributionsByWebACLId (..),
    mkListDistributionsByWebACLId,

    -- ** Request lenses
    ldbwaiMarker,
    ldbwaiMaxItems,
    ldbwaiWebACLId,

    -- * Destructuring the response
    ListDistributionsByWebACLIdResponse (..),
    mkListDistributionsByWebACLIdResponse,

    -- ** Response lenses
    ldbwairsDistributionList,
    ldbwairsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to list distributions that are associated with a specified AWS WAF web ACL.
--
-- /See:/ 'mkListDistributionsByWebACLId' smart constructor.
data ListDistributionsByWebACLId = ListDistributionsByWebACLId'
  { marker ::
      Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Text,
    webACLId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDistributionsByWebACLId' with the minimum fields required to make a request.
--
-- * 'marker' - Use @Marker@ and @MaxItems@ to control pagination of results. If you have more than @MaxItems@ distributions that satisfy the request, the response includes a @NextMarker@ element. To get the next page of results, submit another request. For the value of @Marker@ , specify the value of @NextMarker@ from the last response. (For the first request, omit @Marker@ .)
-- * 'maxItems' - The maximum number of distributions that you want CloudFront to return in the response body. The maximum and default values are both 100.
-- * 'webACLId' - The ID of the AWS WAF web ACL that you want to list the associated distributions. If you specify "null" for the ID, the request returns a list of the distributions that aren't associated with a web ACL.
mkListDistributionsByWebACLId ::
  -- | 'webACLId'
  Lude.Text ->
  ListDistributionsByWebACLId
mkListDistributionsByWebACLId pWebACLId_ =
  ListDistributionsByWebACLId'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      webACLId = pWebACLId_
    }

-- | Use @Marker@ and @MaxItems@ to control pagination of results. If you have more than @MaxItems@ distributions that satisfy the request, the response includes a @NextMarker@ element. To get the next page of results, submit another request. For the value of @Marker@ , specify the value of @NextMarker@ from the last response. (For the first request, omit @Marker@ .)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbwaiMarker :: Lens.Lens' ListDistributionsByWebACLId (Lude.Maybe Lude.Text)
ldbwaiMarker = Lens.lens (marker :: ListDistributionsByWebACLId -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListDistributionsByWebACLId)
{-# DEPRECATED ldbwaiMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of distributions that you want CloudFront to return in the response body. The maximum and default values are both 100.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbwaiMaxItems :: Lens.Lens' ListDistributionsByWebACLId (Lude.Maybe Lude.Text)
ldbwaiMaxItems = Lens.lens (maxItems :: ListDistributionsByWebACLId -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListDistributionsByWebACLId)
{-# DEPRECATED ldbwaiMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The ID of the AWS WAF web ACL that you want to list the associated distributions. If you specify "null" for the ID, the request returns a list of the distributions that aren't associated with a web ACL.
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbwaiWebACLId :: Lens.Lens' ListDistributionsByWebACLId Lude.Text
ldbwaiWebACLId = Lens.lens (webACLId :: ListDistributionsByWebACLId -> Lude.Text) (\s a -> s {webACLId = a} :: ListDistributionsByWebACLId)
{-# DEPRECATED ldbwaiWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

instance Lude.AWSRequest ListDistributionsByWebACLId where
  type
    Rs ListDistributionsByWebACLId =
      ListDistributionsByWebACLIdResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListDistributionsByWebACLIdResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDistributionsByWebACLId where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDistributionsByWebACLId where
  toPath ListDistributionsByWebACLId' {..} =
    Lude.mconcat
      ["/2020-05-31/distributionsByWebACLId/", Lude.toBS webACLId]

instance Lude.ToQuery ListDistributionsByWebACLId where
  toQuery ListDistributionsByWebACLId' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | The response to a request to list the distributions that are associated with a specified AWS WAF web ACL.
--
-- /See:/ 'mkListDistributionsByWebACLIdResponse' smart constructor.
data ListDistributionsByWebACLIdResponse = ListDistributionsByWebACLIdResponse'
  { distributionList ::
      Lude.Maybe
        DistributionList,
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDistributionsByWebACLIdResponse' with the minimum fields required to make a request.
--
-- * 'distributionList' - The @DistributionList@ type.
-- * 'responseStatus' - The response status code.
mkListDistributionsByWebACLIdResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDistributionsByWebACLIdResponse
mkListDistributionsByWebACLIdResponse pResponseStatus_ =
  ListDistributionsByWebACLIdResponse'
    { distributionList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @DistributionList@ type.
--
-- /Note:/ Consider using 'distributionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbwairsDistributionList :: Lens.Lens' ListDistributionsByWebACLIdResponse (Lude.Maybe DistributionList)
ldbwairsDistributionList = Lens.lens (distributionList :: ListDistributionsByWebACLIdResponse -> Lude.Maybe DistributionList) (\s a -> s {distributionList = a} :: ListDistributionsByWebACLIdResponse)
{-# DEPRECATED ldbwairsDistributionList "Use generic-lens or generic-optics with 'distributionList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbwairsResponseStatus :: Lens.Lens' ListDistributionsByWebACLIdResponse Lude.Int
ldbwairsResponseStatus = Lens.lens (responseStatus :: ListDistributionsByWebACLIdResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDistributionsByWebACLIdResponse)
{-# DEPRECATED ldbwairsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
