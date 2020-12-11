{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListDistributionsByKeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distribution IDs for distributions that have a cache behavior that references the specified key group.
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByKeyGroup
  ( -- * Creating a request
    ListDistributionsByKeyGroup (..),
    mkListDistributionsByKeyGroup,

    -- ** Request lenses
    ldbkgMarker,
    ldbkgMaxItems,
    ldbkgKeyGroupId,

    -- * Destructuring the response
    ListDistributionsByKeyGroupResponse (..),
    mkListDistributionsByKeyGroupResponse,

    -- ** Response lenses
    ldbkgrsDistributionIdList,
    ldbkgrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDistributionsByKeyGroup' smart constructor.
data ListDistributionsByKeyGroup = ListDistributionsByKeyGroup'
  { marker ::
      Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Text,
    keyGroupId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDistributionsByKeyGroup' with the minimum fields required to make a request.
--
-- * 'keyGroupId' - The ID of the key group whose associated distribution IDs you are listing.
-- * 'marker' - Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
-- * 'maxItems' - The maximum number of distribution IDs that you want in the response.
mkListDistributionsByKeyGroup ::
  -- | 'keyGroupId'
  Lude.Text ->
  ListDistributionsByKeyGroup
mkListDistributionsByKeyGroup pKeyGroupId_ =
  ListDistributionsByKeyGroup'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      keyGroupId = pKeyGroupId_
    }

-- | Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbkgMarker :: Lens.Lens' ListDistributionsByKeyGroup (Lude.Maybe Lude.Text)
ldbkgMarker = Lens.lens (marker :: ListDistributionsByKeyGroup -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListDistributionsByKeyGroup)
{-# DEPRECATED ldbkgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of distribution IDs that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbkgMaxItems :: Lens.Lens' ListDistributionsByKeyGroup (Lude.Maybe Lude.Text)
ldbkgMaxItems = Lens.lens (maxItems :: ListDistributionsByKeyGroup -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListDistributionsByKeyGroup)
{-# DEPRECATED ldbkgMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The ID of the key group whose associated distribution IDs you are listing.
--
-- /Note:/ Consider using 'keyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbkgKeyGroupId :: Lens.Lens' ListDistributionsByKeyGroup Lude.Text
ldbkgKeyGroupId = Lens.lens (keyGroupId :: ListDistributionsByKeyGroup -> Lude.Text) (\s a -> s {keyGroupId = a} :: ListDistributionsByKeyGroup)
{-# DEPRECATED ldbkgKeyGroupId "Use generic-lens or generic-optics with 'keyGroupId' instead." #-}

instance Lude.AWSRequest ListDistributionsByKeyGroup where
  type
    Rs ListDistributionsByKeyGroup =
      ListDistributionsByKeyGroupResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListDistributionsByKeyGroupResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDistributionsByKeyGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDistributionsByKeyGroup where
  toPath ListDistributionsByKeyGroup' {..} =
    Lude.mconcat
      ["/2020-05-31/distributionsByKeyGroupId/", Lude.toBS keyGroupId]

instance Lude.ToQuery ListDistributionsByKeyGroup where
  toQuery ListDistributionsByKeyGroup' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | /See:/ 'mkListDistributionsByKeyGroupResponse' smart constructor.
data ListDistributionsByKeyGroupResponse = ListDistributionsByKeyGroupResponse'
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDistributionsByKeyGroupResponse' with the minimum fields required to make a request.
--
-- * 'distributionIdList' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkListDistributionsByKeyGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDistributionsByKeyGroupResponse
mkListDistributionsByKeyGroupResponse pResponseStatus_ =
  ListDistributionsByKeyGroupResponse'
    { distributionIdList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'distributionIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbkgrsDistributionIdList :: Lens.Lens' ListDistributionsByKeyGroupResponse (Lude.Maybe DistributionIdList)
ldbkgrsDistributionIdList = Lens.lens (distributionIdList :: ListDistributionsByKeyGroupResponse -> Lude.Maybe DistributionIdList) (\s a -> s {distributionIdList = a} :: ListDistributionsByKeyGroupResponse)
{-# DEPRECATED ldbkgrsDistributionIdList "Use generic-lens or generic-optics with 'distributionIdList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbkgrsResponseStatus :: Lens.Lens' ListDistributionsByKeyGroupResponse Lude.Int
ldbkgrsResponseStatus = Lens.lens (responseStatus :: ListDistributionsByKeyGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDistributionsByKeyGroupResponse)
{-# DEPRECATED ldbkgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
