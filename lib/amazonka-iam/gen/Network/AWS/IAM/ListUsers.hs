{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IAM users that have the specified path prefix. If no path prefix is specified, the operation returns all users in the AWS account. If there are none, the operation returns an empty list.
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListUsers
  ( -- * Creating a request
    ListUsers (..),
    mkListUsers,

    -- ** Request lenses
    luPathPrefix,
    luMarker,
    luMaxItems,

    -- * Destructuring the response
    ListUsersResponse (..),
    mkListUsersResponse,

    -- ** Response lenses
    lursMarker,
    lursIsTruncated,
    lursResponseStatus,
    lursUsers,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListUsers' smart constructor.
data ListUsers = ListUsers'
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

-- | Creates a value of 'ListUsers' with the minimum fields required to make a request.
--
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
-- * 'pathPrefix' - The path prefix for filtering the results. For example: @/division_abc/subdivision_xyz/@ , which would get all user names whose path starts with @/division_abc/subdivision_xyz/@ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/), listing all user names. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
mkListUsers ::
  ListUsers
mkListUsers =
  ListUsers'
    { pathPrefix = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The path prefix for filtering the results. For example: @/division_abc/subdivision_xyz/@ , which would get all user names whose path starts with @/division_abc/subdivision_xyz/@ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/), listing all user names. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'pathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luPathPrefix :: Lens.Lens' ListUsers (Lude.Maybe Lude.Text)
luPathPrefix = Lens.lens (pathPrefix :: ListUsers -> Lude.Maybe Lude.Text) (\s a -> s {pathPrefix = a} :: ListUsers)
{-# DEPRECATED luPathPrefix "Use generic-lens or generic-optics with 'pathPrefix' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luMarker :: Lens.Lens' ListUsers (Lude.Maybe Lude.Text)
luMarker = Lens.lens (marker :: ListUsers -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListUsers)
{-# DEPRECATED luMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luMaxItems :: Lens.Lens' ListUsers (Lude.Maybe Lude.Natural)
luMaxItems = Lens.lens (maxItems :: ListUsers -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListUsers)
{-# DEPRECATED luMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListUsers where
  page rq rs
    | Page.stop (rs Lens.^. lursIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lursMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& luMarker Lens..~ rs Lens.^. lursMarker

instance Lude.AWSRequest ListUsers where
  type Rs ListUsers = ListUsersResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListUsersResult"
      ( \s h x ->
          ListUsersResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "Users" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders ListUsers where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListUsers where
  toPath = Lude.const "/"

instance Lude.ToQuery ListUsers where
  toQuery ListUsers' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListUsers" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PathPrefix" Lude.=: pathPrefix,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | Contains the response to a successful 'ListUsers' request.
--
-- /See:/ 'mkListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    isTruncated :: Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int,
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

-- | Creates a value of 'ListUsersResponse' with the minimum fields required to make a request.
--
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'responseStatus' - The response status code.
-- * 'users' - A list of users.
mkListUsersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUsersResponse
mkListUsersResponse pResponseStatus_ =
  ListUsersResponse'
    { marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_,
      users = Lude.mempty
    }

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursMarker :: Lens.Lens' ListUsersResponse (Lude.Maybe Lude.Text)
lursMarker = Lens.lens (marker :: ListUsersResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListUsersResponse)
{-# DEPRECATED lursMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursIsTruncated :: Lens.Lens' ListUsersResponse (Lude.Maybe Lude.Bool)
lursIsTruncated = Lens.lens (isTruncated :: ListUsersResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListUsersResponse)
{-# DEPRECATED lursIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursResponseStatus :: Lens.Lens' ListUsersResponse Lude.Int
lursResponseStatus = Lens.lens (responseStatus :: ListUsersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListUsersResponse)
{-# DEPRECATED lursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of users.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursUsers :: Lens.Lens' ListUsersResponse [User]
lursUsers = Lens.lens (users :: ListUsersResponse -> [User]) (\s a -> s {users = a} :: ListUsersResponse)
{-# DEPRECATED lursUsers "Use generic-lens or generic-optics with 'users' instead." #-}
