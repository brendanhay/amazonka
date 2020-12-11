{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DescribeUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an ActiveMQ user.
module Network.AWS.MQ.DescribeUser
  ( -- * Creating a request
    DescribeUser (..),
    mkDescribeUser,

    -- ** Request lenses
    duUsername,
    duBrokerId,

    -- * Destructuring the response
    DescribeUserResponse (..),
    mkDescribeUserResponse,

    -- ** Response lenses
    dursGroups,
    dursPending,
    dursConsoleAccess,
    dursUsername,
    dursBrokerId,
    dursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeUser' smart constructor.
data DescribeUser = DescribeUser'
  { username :: Lude.Text,
    brokerId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUser' with the minimum fields required to make a request.
--
-- * 'brokerId' - The unique ID that Amazon MQ generates for the broker.
-- * 'username' - The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
mkDescribeUser ::
  -- | 'username'
  Lude.Text ->
  -- | 'brokerId'
  Lude.Text ->
  DescribeUser
mkDescribeUser pUsername_ pBrokerId_ =
  DescribeUser' {username = pUsername_, brokerId = pBrokerId_}

-- | The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUsername :: Lens.Lens' DescribeUser Lude.Text
duUsername = Lens.lens (username :: DescribeUser -> Lude.Text) (\s a -> s {username = a} :: DescribeUser)
{-# DEPRECATED duUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duBrokerId :: Lens.Lens' DescribeUser Lude.Text
duBrokerId = Lens.lens (brokerId :: DescribeUser -> Lude.Text) (\s a -> s {brokerId = a} :: DescribeUser)
{-# DEPRECATED duBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

instance Lude.AWSRequest DescribeUser where
  type Rs DescribeUser = DescribeUserResponse
  request = Req.get mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Lude.<$> (x Lude..?> "groups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "pending")
            Lude.<*> (x Lude..?> "consoleAccess")
            Lude.<*> (x Lude..?> "username")
            Lude.<*> (x Lude..?> "brokerId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeUser where
  toPath DescribeUser' {..} =
    Lude.mconcat
      ["/v1/brokers/", Lude.toBS brokerId, "/users/", Lude.toBS username]

instance Lude.ToQuery DescribeUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { groups ::
      Lude.Maybe [Lude.Text],
    pending :: Lude.Maybe UserPendingChanges,
    consoleAccess :: Lude.Maybe Lude.Bool,
    username :: Lude.Maybe Lude.Text,
    brokerId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeUserResponse' with the minimum fields required to make a request.
--
-- * 'brokerId' - Required. The unique ID that Amazon MQ generates for the broker.
-- * 'consoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
-- * 'groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
-- * 'pending' - The status of the changes pending for the ActiveMQ user.
-- * 'responseStatus' - The response status code.
-- * 'username' - Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
mkDescribeUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUserResponse
mkDescribeUserResponse pResponseStatus_ =
  DescribeUserResponse'
    { groups = Lude.Nothing,
      pending = Lude.Nothing,
      consoleAccess = Lude.Nothing,
      username = Lude.Nothing,
      brokerId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursGroups :: Lens.Lens' DescribeUserResponse (Lude.Maybe [Lude.Text])
dursGroups = Lens.lens (groups :: DescribeUserResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {groups = a} :: DescribeUserResponse)
{-# DEPRECATED dursGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The status of the changes pending for the ActiveMQ user.
--
-- /Note:/ Consider using 'pending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursPending :: Lens.Lens' DescribeUserResponse (Lude.Maybe UserPendingChanges)
dursPending = Lens.lens (pending :: DescribeUserResponse -> Lude.Maybe UserPendingChanges) (\s a -> s {pending = a} :: DescribeUserResponse)
{-# DEPRECATED dursPending "Use generic-lens or generic-optics with 'pending' instead." #-}

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- /Note:/ Consider using 'consoleAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursConsoleAccess :: Lens.Lens' DescribeUserResponse (Lude.Maybe Lude.Bool)
dursConsoleAccess = Lens.lens (consoleAccess :: DescribeUserResponse -> Lude.Maybe Lude.Bool) (\s a -> s {consoleAccess = a} :: DescribeUserResponse)
{-# DEPRECATED dursConsoleAccess "Use generic-lens or generic-optics with 'consoleAccess' instead." #-}

-- | Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursUsername :: Lens.Lens' DescribeUserResponse (Lude.Maybe Lude.Text)
dursUsername = Lens.lens (username :: DescribeUserResponse -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: DescribeUserResponse)
{-# DEPRECATED dursUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Required. The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursBrokerId :: Lens.Lens' DescribeUserResponse (Lude.Maybe Lude.Text)
dursBrokerId = Lens.lens (brokerId :: DescribeUserResponse -> Lude.Maybe Lude.Text) (\s a -> s {brokerId = a} :: DescribeUserResponse)
{-# DEPRECATED dursBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursResponseStatus :: Lens.Lens' DescribeUserResponse Lude.Int
dursResponseStatus = Lens.lens (responseStatus :: DescribeUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUserResponse)
{-# DEPRECATED dursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
