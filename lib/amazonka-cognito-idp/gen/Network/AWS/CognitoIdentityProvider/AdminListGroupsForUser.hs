{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminListGroupsForUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the groups that the user belongs to.
--
-- Calling this action requires developer credentials.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.AdminListGroupsForUser
  ( -- * Creating a request
    AdminListGroupsForUser (..),
    mkAdminListGroupsForUser,

    -- ** Request lenses
    algfuNextToken,
    algfuLimit,
    algfuUsername,
    algfuUserPoolId,

    -- * Destructuring the response
    AdminListGroupsForUserResponse (..),
    mkAdminListGroupsForUserResponse,

    -- ** Response lenses
    algfursGroups,
    algfursNextToken,
    algfursResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAdminListGroupsForUser' smart constructor.
data AdminListGroupsForUser = AdminListGroupsForUser'
  { nextToken ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    username :: Lude.Sensitive Lude.Text,
    userPoolId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminListGroupsForUser' with the minimum fields required to make a request.
--
-- * 'limit' - The limit of the request to list groups.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'userPoolId' - The user pool ID for the user pool.
-- * 'username' - The username for the user.
mkAdminListGroupsForUser ::
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  -- | 'userPoolId'
  Lude.Text ->
  AdminListGroupsForUser
mkAdminListGroupsForUser pUsername_ pUserPoolId_ =
  AdminListGroupsForUser'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      username = pUsername_,
      userPoolId = pUserPoolId_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
algfuNextToken :: Lens.Lens' AdminListGroupsForUser (Lude.Maybe Lude.Text)
algfuNextToken = Lens.lens (nextToken :: AdminListGroupsForUser -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: AdminListGroupsForUser)
{-# DEPRECATED algfuNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The limit of the request to list groups.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
algfuLimit :: Lens.Lens' AdminListGroupsForUser (Lude.Maybe Lude.Natural)
algfuLimit = Lens.lens (limit :: AdminListGroupsForUser -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: AdminListGroupsForUser)
{-# DEPRECATED algfuLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The username for the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
algfuUsername :: Lens.Lens' AdminListGroupsForUser (Lude.Sensitive Lude.Text)
algfuUsername = Lens.lens (username :: AdminListGroupsForUser -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminListGroupsForUser)
{-# DEPRECATED algfuUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
algfuUserPoolId :: Lens.Lens' AdminListGroupsForUser Lude.Text
algfuUserPoolId = Lens.lens (userPoolId :: AdminListGroupsForUser -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminListGroupsForUser)
{-# DEPRECATED algfuUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Page.AWSPager AdminListGroupsForUser where
  page rq rs
    | Page.stop (rs Lens.^. algfursNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. algfursGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& algfuNextToken Lens..~ rs Lens.^. algfursNextToken

instance Lude.AWSRequest AdminListGroupsForUser where
  type Rs AdminListGroupsForUser = AdminListGroupsForUserResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          AdminListGroupsForUserResponse'
            Lude.<$> (x Lude..?> "Groups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminListGroupsForUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminListGroupsForUser" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminListGroupsForUser where
  toJSON AdminListGroupsForUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("Username" Lude..= username),
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath AdminListGroupsForUser where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminListGroupsForUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAdminListGroupsForUserResponse' smart constructor.
data AdminListGroupsForUserResponse = AdminListGroupsForUserResponse'
  { groups ::
      Lude.Maybe [GroupType],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'AdminListGroupsForUserResponse' with the minimum fields required to make a request.
--
-- * 'groups' - The groups that the user belongs to.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
mkAdminListGroupsForUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminListGroupsForUserResponse
mkAdminListGroupsForUserResponse pResponseStatus_ =
  AdminListGroupsForUserResponse'
    { groups = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The groups that the user belongs to.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
algfursGroups :: Lens.Lens' AdminListGroupsForUserResponse (Lude.Maybe [GroupType])
algfursGroups = Lens.lens (groups :: AdminListGroupsForUserResponse -> Lude.Maybe [GroupType]) (\s a -> s {groups = a} :: AdminListGroupsForUserResponse)
{-# DEPRECATED algfursGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
algfursNextToken :: Lens.Lens' AdminListGroupsForUserResponse (Lude.Maybe Lude.Text)
algfursNextToken = Lens.lens (nextToken :: AdminListGroupsForUserResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: AdminListGroupsForUserResponse)
{-# DEPRECATED algfursNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
algfursResponseStatus :: Lens.Lens' AdminListGroupsForUserResponse Lude.Int
algfursResponseStatus = Lens.lens (responseStatus :: AdminListGroupsForUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminListGroupsForUserResponse)
{-# DEPRECATED algfursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
