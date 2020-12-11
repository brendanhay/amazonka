{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListUsersInGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the users in the specified group.
--
-- Calling this action requires developer credentials.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListUsersInGroup
  ( -- * Creating a request
    ListUsersInGroup (..),
    mkListUsersInGroup,

    -- ** Request lenses
    luigNextToken,
    luigLimit,
    luigUserPoolId,
    luigGroupName,

    -- * Destructuring the response
    ListUsersInGroupResponse (..),
    mkListUsersInGroupResponse,

    -- ** Response lenses
    luigrsUsers,
    luigrsNextToken,
    luigrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListUsersInGroup' smart constructor.
data ListUsersInGroup = ListUsersInGroup'
  { nextToken ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    userPoolId :: Lude.Text,
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

-- | Creates a value of 'ListUsersInGroup' with the minimum fields required to make a request.
--
-- * 'groupName' - The name of the group.
-- * 'limit' - The limit of the request to list users.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'userPoolId' - The user pool ID for the user pool.
mkListUsersInGroup ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'groupName'
  Lude.Text ->
  ListUsersInGroup
mkListUsersInGroup pUserPoolId_ pGroupName_ =
  ListUsersInGroup'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      userPoolId = pUserPoolId_,
      groupName = pGroupName_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luigNextToken :: Lens.Lens' ListUsersInGroup (Lude.Maybe Lude.Text)
luigNextToken = Lens.lens (nextToken :: ListUsersInGroup -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUsersInGroup)
{-# DEPRECATED luigNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The limit of the request to list users.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luigLimit :: Lens.Lens' ListUsersInGroup (Lude.Maybe Lude.Natural)
luigLimit = Lens.lens (limit :: ListUsersInGroup -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListUsersInGroup)
{-# DEPRECATED luigLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luigUserPoolId :: Lens.Lens' ListUsersInGroup Lude.Text
luigUserPoolId = Lens.lens (userPoolId :: ListUsersInGroup -> Lude.Text) (\s a -> s {userPoolId = a} :: ListUsersInGroup)
{-# DEPRECATED luigUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luigGroupName :: Lens.Lens' ListUsersInGroup Lude.Text
luigGroupName = Lens.lens (groupName :: ListUsersInGroup -> Lude.Text) (\s a -> s {groupName = a} :: ListUsersInGroup)
{-# DEPRECATED luigGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Page.AWSPager ListUsersInGroup where
  page rq rs
    | Page.stop (rs Lens.^. luigrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. luigrsUsers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& luigNextToken Lens..~ rs Lens.^. luigrsNextToken

instance Lude.AWSRequest ListUsersInGroup where
  type Rs ListUsersInGroup = ListUsersInGroupResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListUsersInGroupResponse'
            Lude.<$> (x Lude..?> "Users" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListUsersInGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.ListUsersInGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListUsersInGroup where
  toJSON ListUsersInGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("GroupName" Lude..= groupName)
          ]
      )

instance Lude.ToPath ListUsersInGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ListUsersInGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListUsersInGroupResponse' smart constructor.
data ListUsersInGroupResponse = ListUsersInGroupResponse'
  { users ::
      Lude.Maybe [UserType],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUsersInGroupResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
-- * 'users' - The users returned in the request to list users.
mkListUsersInGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUsersInGroupResponse
mkListUsersInGroupResponse pResponseStatus_ =
  ListUsersInGroupResponse'
    { users = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The users returned in the request to list users.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luigrsUsers :: Lens.Lens' ListUsersInGroupResponse (Lude.Maybe [UserType])
luigrsUsers = Lens.lens (users :: ListUsersInGroupResponse -> Lude.Maybe [UserType]) (\s a -> s {users = a} :: ListUsersInGroupResponse)
{-# DEPRECATED luigrsUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luigrsNextToken :: Lens.Lens' ListUsersInGroupResponse (Lude.Maybe Lude.Text)
luigrsNextToken = Lens.lens (nextToken :: ListUsersInGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUsersInGroupResponse)
{-# DEPRECATED luigrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luigrsResponseStatus :: Lens.Lens' ListUsersInGroupResponse Lude.Int
luigrsResponseStatus = Lens.lens (responseStatus :: ListUsersInGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListUsersInGroupResponse)
{-# DEPRECATED luigrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
