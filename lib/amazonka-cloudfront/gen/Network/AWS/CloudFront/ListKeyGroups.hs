{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListKeyGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of key groups.
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListKeyGroups
  ( -- * Creating a request
    ListKeyGroups (..),
    mkListKeyGroups,

    -- ** Request lenses
    lkgMarker,
    lkgMaxItems,

    -- * Destructuring the response
    ListKeyGroupsResponse (..),
    mkListKeyGroupsResponse,

    -- ** Response lenses
    lkgrsKeyGroupList,
    lkgrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListKeyGroups' smart constructor.
data ListKeyGroups = ListKeyGroups'
  { marker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListKeyGroups' with the minimum fields required to make a request.
--
-- * 'marker' - Use this field when paginating results to indicate where to begin in your list of key groups. The response includes key groups in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
-- * 'maxItems' - The maximum number of key groups that you want in the response.
mkListKeyGroups ::
  ListKeyGroups
mkListKeyGroups =
  ListKeyGroups' {marker = Lude.Nothing, maxItems = Lude.Nothing}

-- | Use this field when paginating results to indicate where to begin in your list of key groups. The response includes key groups in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkgMarker :: Lens.Lens' ListKeyGroups (Lude.Maybe Lude.Text)
lkgMarker = Lens.lens (marker :: ListKeyGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListKeyGroups)
{-# DEPRECATED lkgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of key groups that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkgMaxItems :: Lens.Lens' ListKeyGroups (Lude.Maybe Lude.Text)
lkgMaxItems = Lens.lens (maxItems :: ListKeyGroups -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListKeyGroups)
{-# DEPRECATED lkgMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Lude.AWSRequest ListKeyGroups where
  type Rs ListKeyGroups = ListKeyGroupsResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListKeyGroupsResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListKeyGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListKeyGroups where
  toPath = Lude.const "/2020-05-31/key-group"

instance Lude.ToQuery ListKeyGroups where
  toQuery ListKeyGroups' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | /See:/ 'mkListKeyGroupsResponse' smart constructor.
data ListKeyGroupsResponse = ListKeyGroupsResponse'
  { keyGroupList ::
      Lude.Maybe KeyGroupList,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListKeyGroupsResponse' with the minimum fields required to make a request.
--
-- * 'keyGroupList' - A list of key groups.
-- * 'responseStatus' - The response status code.
mkListKeyGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListKeyGroupsResponse
mkListKeyGroupsResponse pResponseStatus_ =
  ListKeyGroupsResponse'
    { keyGroupList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of key groups.
--
-- /Note:/ Consider using 'keyGroupList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkgrsKeyGroupList :: Lens.Lens' ListKeyGroupsResponse (Lude.Maybe KeyGroupList)
lkgrsKeyGroupList = Lens.lens (keyGroupList :: ListKeyGroupsResponse -> Lude.Maybe KeyGroupList) (\s a -> s {keyGroupList = a} :: ListKeyGroupsResponse)
{-# DEPRECATED lkgrsKeyGroupList "Use generic-lens or generic-optics with 'keyGroupList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkgrsResponseStatus :: Lens.Lens' ListKeyGroupsResponse Lude.Int
lkgrsResponseStatus = Lens.lens (responseStatus :: ListKeyGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListKeyGroupsResponse)
{-# DEPRECATED lkgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
