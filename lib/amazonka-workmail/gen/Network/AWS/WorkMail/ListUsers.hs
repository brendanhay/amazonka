{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the organization's users.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListUsers
  ( -- * Creating a request
    ListUsers (..),
    mkListUsers,

    -- ** Request lenses
    luNextToken,
    luMaxResults,
    luOrganizationId,

    -- * Destructuring the response
    ListUsersResponse (..),
    mkListUsersResponse,

    -- ** Response lenses
    lursUsers,
    lursNextToken,
    lursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkListUsers' smart constructor.
data ListUsers = ListUsers'
  { nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    organizationId :: Lude.Text
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
-- * 'maxResults' - The maximum number of results to return in a single call.
-- * 'nextToken' - The token to use to retrieve the next page of results. The first call does not contain any tokens.
-- * 'organizationId' - The identifier for the organization under which the users exist.
mkListUsers ::
  -- | 'organizationId'
  Lude.Text ->
  ListUsers
mkListUsers pOrganizationId_ =
  ListUsers'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luNextToken :: Lens.Lens' ListUsers (Lude.Maybe Lude.Text)
luNextToken = Lens.lens (nextToken :: ListUsers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUsers)
{-# DEPRECATED luNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luMaxResults :: Lens.Lens' ListUsers (Lude.Maybe Lude.Natural)
luMaxResults = Lens.lens (maxResults :: ListUsers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListUsers)
{-# DEPRECATED luMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier for the organization under which the users exist.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luOrganizationId :: Lens.Lens' ListUsers Lude.Text
luOrganizationId = Lens.lens (organizationId :: ListUsers -> Lude.Text) (\s a -> s {organizationId = a} :: ListUsers)
{-# DEPRECATED luOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Page.AWSPager ListUsers where
  page rq rs
    | Page.stop (rs Lens.^. lursNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lursUsers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& luNextToken Lens..~ rs Lens.^. lursNextToken

instance Lude.AWSRequest ListUsers where
  type Rs ListUsers = ListUsersResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListUsersResponse'
            Lude.<$> (x Lude..?> "Users" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListUsers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.ListUsers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListUsers where
  toJSON ListUsers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath ListUsers where
  toPath = Lude.const "/"

instance Lude.ToQuery ListUsers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { users ::
      Lude.Maybe [User],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListUsersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is `null` when there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'users' - The overview of users for an organization.
mkListUsersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUsersResponse
mkListUsersResponse pResponseStatus_ =
  ListUsersResponse'
    { users = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The overview of users for an organization.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursUsers :: Lens.Lens' ListUsersResponse (Lude.Maybe [User])
lursUsers = Lens.lens (users :: ListUsersResponse -> Lude.Maybe [User]) (\s a -> s {users = a} :: ListUsersResponse)
{-# DEPRECATED lursUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The token to use to retrieve the next page of results. This value is `null` when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursNextToken :: Lens.Lens' ListUsersResponse (Lude.Maybe Lude.Text)
lursNextToken = Lens.lens (nextToken :: ListUsersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUsersResponse)
{-# DEPRECATED lursNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursResponseStatus :: Lens.Lens' ListUsersResponse Lude.Int
lursResponseStatus = Lens.lens (responseStatus :: ListUsersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListUsersResponse)
{-# DEPRECATED lursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
