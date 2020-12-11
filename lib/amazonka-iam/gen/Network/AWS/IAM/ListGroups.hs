{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IAM groups that have the specified path prefix.
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListGroups
  ( -- * Creating a request
    ListGroups (..),
    mkListGroups,

    -- ** Request lenses
    lgPathPrefix,
    lgMarker,
    lgMaxItems,

    -- * Destructuring the response
    ListGroupsResponse (..),
    mkListGroupsResponse,

    -- ** Response lenses
    lgrsMarker,
    lgrsIsTruncated,
    lgrsResponseStatus,
    lgrsGroups,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGroups' smart constructor.
data ListGroups = ListGroups'
  { pathPrefix :: Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroups' with the minimum fields required to make a request.
--
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
-- * 'pathPrefix' - The path prefix for filtering the results. For example, the prefix @/division_abc/subdivision_xyz/@ gets all groups whose path starts with @/division_abc/subdivision_xyz/@ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/), listing all groups. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
mkListGroups ::
  ListGroups
mkListGroups =
  ListGroups'
    { pathPrefix = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The path prefix for filtering the results. For example, the prefix @/division_abc/subdivision_xyz/@ gets all groups whose path starts with @/division_abc/subdivision_xyz/@ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/), listing all groups. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'pathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgPathPrefix :: Lens.Lens' ListGroups (Lude.Maybe Lude.Text)
lgPathPrefix = Lens.lens (pathPrefix :: ListGroups -> Lude.Maybe Lude.Text) (\s a -> s {pathPrefix = a} :: ListGroups)
{-# DEPRECATED lgPathPrefix "Use generic-lens or generic-optics with 'pathPrefix' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgMarker :: Lens.Lens' ListGroups (Lude.Maybe Lude.Text)
lgMarker = Lens.lens (marker :: ListGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListGroups)
{-# DEPRECATED lgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgMaxItems :: Lens.Lens' ListGroups (Lude.Maybe Lude.Natural)
lgMaxItems = Lens.lens (maxItems :: ListGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListGroups)
{-# DEPRECATED lgMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListGroups where
  page rq rs
    | Page.stop (rs Lens.^. lgrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lgrsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lgMarker Lens..~ rs Lens.^. lgrsMarker

instance Lude.AWSRequest ListGroups where
  type Rs ListGroups = ListGroupsResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListGroupsResult"
      ( \s h x ->
          ListGroupsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "Groups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders ListGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery ListGroups where
  toQuery ListGroups' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PathPrefix" Lude.=: pathPrefix,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | Contains the response to a successful 'ListGroups' request.
--
-- /See:/ 'mkListGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    isTruncated :: Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int,
    groups :: [Group]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroupsResponse' with the minimum fields required to make a request.
--
-- * 'groups' - A list of groups.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'responseStatus' - The response status code.
mkListGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGroupsResponse
mkListGroupsResponse pResponseStatus_ =
  ListGroupsResponse'
    { marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_,
      groups = Lude.mempty
    }

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsMarker :: Lens.Lens' ListGroupsResponse (Lude.Maybe Lude.Text)
lgrsMarker = Lens.lens (marker :: ListGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsIsTruncated :: Lens.Lens' ListGroupsResponse (Lude.Maybe Lude.Bool)
lgrsIsTruncated = Lens.lens (isTruncated :: ListGroupsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsResponseStatus :: Lens.Lens' ListGroupsResponse Lude.Int
lgrsResponseStatus = Lens.lens (responseStatus :: ListGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsGroups :: Lens.Lens' ListGroupsResponse [Group]
lgrsGroups = Lens.lens (groups :: ListGroupsResponse -> [Group]) (\s a -> s {groups = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}
