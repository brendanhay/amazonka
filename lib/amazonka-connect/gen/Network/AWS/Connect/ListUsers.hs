{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the users for the specified Amazon Connect instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListUsers
  ( -- * Creating a request
    ListUsers (..),
    mkListUsers,

    -- ** Request lenses
    luNextToken,
    luMaxResults,
    luInstanceId,

    -- * Destructuring the response
    ListUsersResponse (..),
    mkListUsersResponse,

    -- ** Response lenses
    lursNextToken,
    lursUserSummaryList,
    lursResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListUsers' smart constructor.
data ListUsers = ListUsers'
  { nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    instanceId :: Lude.Text
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
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'maxResults' - The maximimum number of results to return per page.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
mkListUsers ::
  -- | 'instanceId'
  Lude.Text ->
  ListUsers
mkListUsers pInstanceId_ =
  ListUsers'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luNextToken :: Lens.Lens' ListUsers (Lude.Maybe Lude.Text)
luNextToken = Lens.lens (nextToken :: ListUsers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUsers)
{-# DEPRECATED luNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luMaxResults :: Lens.Lens' ListUsers (Lude.Maybe Lude.Natural)
luMaxResults = Lens.lens (maxResults :: ListUsers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListUsers)
{-# DEPRECATED luMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luInstanceId :: Lens.Lens' ListUsers Lude.Text
luInstanceId = Lens.lens (instanceId :: ListUsers -> Lude.Text) (\s a -> s {instanceId = a} :: ListUsers)
{-# DEPRECATED luInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Page.AWSPager ListUsers where
  page rq rs
    | Page.stop (rs Lens.^. lursNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lursUserSummaryList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& luNextToken Lens..~ rs Lens.^. lursNextToken

instance Lude.AWSRequest ListUsers where
  type Rs ListUsers = ListUsersResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListUsersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "UserSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListUsers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListUsers where
  toPath ListUsers' {..} =
    Lude.mconcat ["/users-summary/", Lude.toBS instanceId]

instance Lude.ToQuery ListUsers where
  toQuery ListUsers' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    userSummaryList :: Lude.Maybe [UserSummary],
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
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'responseStatus' - The response status code.
-- * 'userSummaryList' - Information about the users.
mkListUsersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUsersResponse
mkListUsersResponse pResponseStatus_ =
  ListUsersResponse'
    { nextToken = Lude.Nothing,
      userSummaryList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursNextToken :: Lens.Lens' ListUsersResponse (Lude.Maybe Lude.Text)
lursNextToken = Lens.lens (nextToken :: ListUsersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUsersResponse)
{-# DEPRECATED lursNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the users.
--
-- /Note:/ Consider using 'userSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursUserSummaryList :: Lens.Lens' ListUsersResponse (Lude.Maybe [UserSummary])
lursUserSummaryList = Lens.lens (userSummaryList :: ListUsersResponse -> Lude.Maybe [UserSummary]) (\s a -> s {userSummaryList = a} :: ListUsersResponse)
{-# DEPRECATED lursUserSummaryList "Use generic-lens or generic-optics with 'userSummaryList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursResponseStatus :: Lens.Lens' ListUsersResponse Lude.Int
lursResponseStatus = Lens.lens (responseStatus :: ListUsersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListUsersResponse)
{-# DEPRECATED lursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
