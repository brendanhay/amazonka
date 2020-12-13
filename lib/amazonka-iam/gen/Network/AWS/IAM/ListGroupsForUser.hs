{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListGroupsForUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IAM groups that the specified IAM user belongs to.
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListGroupsForUser
  ( -- * Creating a request
    ListGroupsForUser (..),
    mkListGroupsForUser,

    -- ** Request lenses
    lgfuUserName,
    lgfuMarker,
    lgfuMaxItems,

    -- * Destructuring the response
    ListGroupsForUserResponse (..),
    mkListGroupsForUserResponse,

    -- ** Response lenses
    lgfursGroups,
    lgfursMarker,
    lgfursIsTruncated,
    lgfursResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGroupsForUser' smart constructor.
data ListGroupsForUser = ListGroupsForUser'
  { -- | The name of the user to list groups for.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Text,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Lude.Maybe Lude.Text,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroupsForUser' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the user to list groups for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
mkListGroupsForUser ::
  -- | 'userName'
  Lude.Text ->
  ListGroupsForUser
mkListGroupsForUser pUserName_ =
  ListGroupsForUser'
    { userName = pUserName_,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The name of the user to list groups for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfuUserName :: Lens.Lens' ListGroupsForUser Lude.Text
lgfuUserName = Lens.lens (userName :: ListGroupsForUser -> Lude.Text) (\s a -> s {userName = a} :: ListGroupsForUser)
{-# DEPRECATED lgfuUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfuMarker :: Lens.Lens' ListGroupsForUser (Lude.Maybe Lude.Text)
lgfuMarker = Lens.lens (marker :: ListGroupsForUser -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListGroupsForUser)
{-# DEPRECATED lgfuMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfuMaxItems :: Lens.Lens' ListGroupsForUser (Lude.Maybe Lude.Natural)
lgfuMaxItems = Lens.lens (maxItems :: ListGroupsForUser -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListGroupsForUser)
{-# DEPRECATED lgfuMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListGroupsForUser where
  page rq rs
    | Page.stop (rs Lens.^. lgfursIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lgfursMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lgfuMarker Lens..~ rs Lens.^. lgfursMarker

instance Lude.AWSRequest ListGroupsForUser where
  type Rs ListGroupsForUser = ListGroupsForUserResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListGroupsForUserResult"
      ( \s h x ->
          ListGroupsForUserResponse'
            Lude.<$> ( x Lude..@? "Groups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGroupsForUser where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListGroupsForUser where
  toPath = Lude.const "/"

instance Lude.ToQuery ListGroupsForUser where
  toQuery ListGroupsForUser' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListGroupsForUser" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | Contains the response to a successful 'ListGroupsForUser' request.
--
-- /See:/ 'mkListGroupsForUserResponse' smart constructor.
data ListGroupsForUserResponse = ListGroupsForUserResponse'
  { -- | A list of groups.
    groups :: [Group],
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroupsForUserResponse' with the minimum fields required to make a request.
--
-- * 'groups' - A list of groups.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'responseStatus' - The response status code.
mkListGroupsForUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGroupsForUserResponse
mkListGroupsForUserResponse pResponseStatus_ =
  ListGroupsForUserResponse'
    { groups = Lude.mempty,
      marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfursGroups :: Lens.Lens' ListGroupsForUserResponse [Group]
lgfursGroups = Lens.lens (groups :: ListGroupsForUserResponse -> [Group]) (\s a -> s {groups = a} :: ListGroupsForUserResponse)
{-# DEPRECATED lgfursGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfursMarker :: Lens.Lens' ListGroupsForUserResponse (Lude.Maybe Lude.Text)
lgfursMarker = Lens.lens (marker :: ListGroupsForUserResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListGroupsForUserResponse)
{-# DEPRECATED lgfursMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfursIsTruncated :: Lens.Lens' ListGroupsForUserResponse (Lude.Maybe Lude.Bool)
lgfursIsTruncated = Lens.lens (isTruncated :: ListGroupsForUserResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListGroupsForUserResponse)
{-# DEPRECATED lgfursIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfursResponseStatus :: Lens.Lens' ListGroupsForUserResponse Lude.Int
lgfursResponseStatus = Lens.lens (responseStatus :: ListGroupsForUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGroupsForUserResponse)
{-# DEPRECATED lgfursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
