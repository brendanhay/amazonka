{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.ListUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all ActiveMQ users.
module Network.AWS.MQ.ListUsers
  ( -- * Creating a request
    ListUsers (..),
    mkListUsers,

    -- ** Request lenses
    luNextToken,
    luBrokerId,
    luMaxResults,

    -- * Destructuring the response
    ListUsersResponse (..),
    mkListUsersResponse,

    -- ** Response lenses
    lursUsers,
    lursNextToken,
    lursBrokerId,
    lursMaxResults,
    lursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListUsers' smart constructor.
data ListUsers = ListUsers'
  { -- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Lude.Text,
    -- | The maximum number of ActiveMQ users that can be returned per page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUsers' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
-- * 'brokerId' - The unique ID that Amazon MQ generates for the broker.
-- * 'maxResults' - The maximum number of ActiveMQ users that can be returned per page (20 by default). This value must be an integer from 5 to 100.
mkListUsers ::
  -- | 'brokerId'
  Lude.Text ->
  ListUsers
mkListUsers pBrokerId_ =
  ListUsers'
    { nextToken = Lude.Nothing,
      brokerId = pBrokerId_,
      maxResults = Lude.Nothing
    }

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luNextToken :: Lens.Lens' ListUsers (Lude.Maybe Lude.Text)
luNextToken = Lens.lens (nextToken :: ListUsers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUsers)
{-# DEPRECATED luNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luBrokerId :: Lens.Lens' ListUsers Lude.Text
luBrokerId = Lens.lens (brokerId :: ListUsers -> Lude.Text) (\s a -> s {brokerId = a} :: ListUsers)
{-# DEPRECATED luBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | The maximum number of ActiveMQ users that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luMaxResults :: Lens.Lens' ListUsers (Lude.Maybe Lude.Natural)
luMaxResults = Lens.lens (maxResults :: ListUsers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListUsers)
{-# DEPRECATED luMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListUsers where
  type Rs ListUsers = ListUsersResponse
  request = Req.get mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListUsersResponse'
            Lude.<$> (x Lude..?> "users" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "brokerId")
            Lude.<*> (x Lude..?> "maxResults")
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
    Lude.mconcat ["/v1/brokers/", Lude.toBS brokerId, "/users"]

instance Lude.ToQuery ListUsers where
  toQuery ListUsers' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { -- | Required. The list of all ActiveMQ usernames for the specified broker.
    users :: Lude.Maybe [UserSummary],
    -- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Required. The unique ID that Amazon MQ generates for the broker.
    brokerId :: Lude.Maybe Lude.Text,
    -- | Required. The maximum number of ActiveMQ users that can be returned per page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUsersResponse' with the minimum fields required to make a request.
--
-- * 'users' - Required. The list of all ActiveMQ usernames for the specified broker.
-- * 'nextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
-- * 'brokerId' - Required. The unique ID that Amazon MQ generates for the broker.
-- * 'maxResults' - Required. The maximum number of ActiveMQ users that can be returned per page (20 by default). This value must be an integer from 5 to 100.
-- * 'responseStatus' - The response status code.
mkListUsersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUsersResponse
mkListUsersResponse pResponseStatus_ =
  ListUsersResponse'
    { users = Lude.Nothing,
      nextToken = Lude.Nothing,
      brokerId = Lude.Nothing,
      maxResults = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Required. The list of all ActiveMQ usernames for the specified broker.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursUsers :: Lens.Lens' ListUsersResponse (Lude.Maybe [UserSummary])
lursUsers = Lens.lens (users :: ListUsersResponse -> Lude.Maybe [UserSummary]) (\s a -> s {users = a} :: ListUsersResponse)
{-# DEPRECATED lursUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursNextToken :: Lens.Lens' ListUsersResponse (Lude.Maybe Lude.Text)
lursNextToken = Lens.lens (nextToken :: ListUsersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUsersResponse)
{-# DEPRECATED lursNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Required. The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursBrokerId :: Lens.Lens' ListUsersResponse (Lude.Maybe Lude.Text)
lursBrokerId = Lens.lens (brokerId :: ListUsersResponse -> Lude.Maybe Lude.Text) (\s a -> s {brokerId = a} :: ListUsersResponse)
{-# DEPRECATED lursBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | Required. The maximum number of ActiveMQ users that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursMaxResults :: Lens.Lens' ListUsersResponse (Lude.Maybe Lude.Natural)
lursMaxResults = Lens.lens (maxResults :: ListUsersResponse -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListUsersResponse)
{-# DEPRECATED lursMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursResponseStatus :: Lens.Lens' ListUsersResponse Lude.Int
lursResponseStatus = Lens.lens (responseStatus :: ListUsersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListUsersResponse)
{-# DEPRECATED lursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
