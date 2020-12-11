{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of IAM users that are in the specified IAM group. You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.GetGroup
  ( -- * Creating a request
    GetGroup (..),
    mkGetGroup,

    -- ** Request lenses
    ggMarker,
    ggMaxItems,
    ggGroupName,

    -- * Destructuring the response
    GetGroupResponse (..),
    mkGetGroupResponse,

    -- ** Response lenses
    ggrsMarker,
    ggrsIsTruncated,
    ggrsResponseStatus,
    ggrsGroup,
    ggrsUsers,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetGroup' smart constructor.
data GetGroup = GetGroup'
  { marker :: Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Natural,
    groupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroup' with the minimum fields required to make a request.
--
-- * 'groupName' - The name of the group.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
mkGetGroup ::
  -- | 'groupName'
  Lude.Text ->
  GetGroup
mkGetGroup pGroupName_ =
  GetGroup'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      groupName = pGroupName_
    }

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggMarker :: Lens.Lens' GetGroup (Lude.Maybe Lude.Text)
ggMarker = Lens.lens (marker :: GetGroup -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: GetGroup)
{-# DEPRECATED ggMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggMaxItems :: Lens.Lens' GetGroup (Lude.Maybe Lude.Natural)
ggMaxItems = Lens.lens (maxItems :: GetGroup -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: GetGroup)
{-# DEPRECATED ggMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The name of the group.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGroupName :: Lens.Lens' GetGroup Lude.Text
ggGroupName = Lens.lens (groupName :: GetGroup -> Lude.Text) (\s a -> s {groupName = a} :: GetGroup)
{-# DEPRECATED ggGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Page.AWSPager GetGroup where
  page rq rs
    | Page.stop (rs Lens.^. ggrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. ggrsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& ggMarker Lens..~ rs Lens.^. ggrsMarker

instance Lude.AWSRequest GetGroup where
  type Rs GetGroup = GetGroupResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetGroupResult"
      ( \s h x ->
          GetGroupResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "Group")
            Lude.<*> ( x Lude..@? "Users" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders GetGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery GetGroup where
  toQuery GetGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "GroupName" Lude.=: groupName
      ]

-- | Contains the response to a successful 'GetGroup' request.
--
-- /See:/ 'mkGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    isTruncated :: Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int,
    group :: Group,
    users :: [User]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupResponse' with the minimum fields required to make a request.
--
-- * 'group' - A structure that contains details about the group.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'responseStatus' - The response status code.
-- * 'users' - A list of users in the group.
mkGetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'group'
  Group ->
  GetGroupResponse
mkGetGroupResponse pResponseStatus_ pGroup_ =
  GetGroupResponse'
    { marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_,
      group = pGroup_,
      users = Lude.mempty
    }

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsMarker :: Lens.Lens' GetGroupResponse (Lude.Maybe Lude.Text)
ggrsMarker = Lens.lens (marker :: GetGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: GetGroupResponse)
{-# DEPRECATED ggrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsIsTruncated :: Lens.Lens' GetGroupResponse (Lude.Maybe Lude.Bool)
ggrsIsTruncated = Lens.lens (isTruncated :: GetGroupResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: GetGroupResponse)
{-# DEPRECATED ggrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsResponseStatus :: Lens.Lens' GetGroupResponse Lude.Int
ggrsResponseStatus = Lens.lens (responseStatus :: GetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGroupResponse)
{-# DEPRECATED ggrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A structure that contains details about the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsGroup :: Lens.Lens' GetGroupResponse Group
ggrsGroup = Lens.lens (group :: GetGroupResponse -> Group) (\s a -> s {group = a} :: GetGroupResponse)
{-# DEPRECATED ggrsGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | A list of users in the group.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsUsers :: Lens.Lens' GetGroupResponse [User]
ggrsUsers = Lens.lens (users :: GetGroupResponse -> [User]) (\s a -> s {users = a} :: GetGroupResponse)
{-# DEPRECATED ggrsUsers "Use generic-lens or generic-optics with 'users' instead." #-}
