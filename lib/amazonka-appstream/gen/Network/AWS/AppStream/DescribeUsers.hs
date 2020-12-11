{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified users in the user pool.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeUsers
  ( -- * Creating a request
    DescribeUsers (..),
    mkDescribeUsers,

    -- ** Request lenses
    duNextToken,
    duMaxResults,
    duAuthenticationType,

    -- * Destructuring the response
    DescribeUsersResponse (..),
    mkDescribeUsersResponse,

    -- ** Response lenses
    dursUsers,
    dursNextToken,
    dursResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int,
    authenticationType :: AuthenticationType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUsers' with the minimum fields required to make a request.
--
-- * 'authenticationType' - The authentication type for the users in the user pool to describe. You must specify USERPOOL.
-- * 'maxResults' - The maximum size of each page of results.
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
mkDescribeUsers ::
  -- | 'authenticationType'
  AuthenticationType ->
  DescribeUsers
mkDescribeUsers pAuthenticationType_ =
  DescribeUsers'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      authenticationType = pAuthenticationType_
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duNextToken :: Lens.Lens' DescribeUsers (Lude.Maybe Lude.Text)
duNextToken = Lens.lens (nextToken :: DescribeUsers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeUsers)
{-# DEPRECATED duNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum size of each page of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duMaxResults :: Lens.Lens' DescribeUsers (Lude.Maybe Lude.Int)
duMaxResults = Lens.lens (maxResults :: DescribeUsers -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeUsers)
{-# DEPRECATED duMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The authentication type for the users in the user pool to describe. You must specify USERPOOL.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duAuthenticationType :: Lens.Lens' DescribeUsers AuthenticationType
duAuthenticationType = Lens.lens (authenticationType :: DescribeUsers -> AuthenticationType) (\s a -> s {authenticationType = a} :: DescribeUsers)
{-# DEPRECATED duAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

instance Page.AWSPager DescribeUsers where
  page rq rs
    | Page.stop (rs Lens.^. dursNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dursUsers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& duNextToken Lens..~ rs Lens.^. dursNextToken

instance Lude.AWSRequest DescribeUsers where
  type Rs DescribeUsers = DescribeUsersResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUsersResponse'
            Lude.<$> (x Lude..?> "Users" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUsers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.DescribeUsers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeUsers where
  toJSON DescribeUsers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("AuthenticationType" Lude..= authenticationType)
          ]
      )

instance Lude.ToPath DescribeUsers where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUsers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeUsersResponse' smart constructor.
data DescribeUsersResponse = DescribeUsersResponse'
  { users ::
      Lude.Maybe [User],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUsersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
-- * 'responseStatus' - The response status code.
-- * 'users' - Information about users in the user pool.
mkDescribeUsersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUsersResponse
mkDescribeUsersResponse pResponseStatus_ =
  DescribeUsersResponse'
    { users = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about users in the user pool.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursUsers :: Lens.Lens' DescribeUsersResponse (Lude.Maybe [User])
dursUsers = Lens.lens (users :: DescribeUsersResponse -> Lude.Maybe [User]) (\s a -> s {users = a} :: DescribeUsersResponse)
{-# DEPRECATED dursUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursNextToken :: Lens.Lens' DescribeUsersResponse (Lude.Maybe Lude.Text)
dursNextToken = Lens.lens (nextToken :: DescribeUsersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeUsersResponse)
{-# DEPRECATED dursNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursResponseStatus :: Lens.Lens' DescribeUsersResponse Lude.Int
dursResponseStatus = Lens.lens (responseStatus :: DescribeUsersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUsersResponse)
{-# DEPRECATED dursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
