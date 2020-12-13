{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.CreateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an ActiveMQ user.
module Network.AWS.MQ.CreateUser
  ( -- * Creating a request
    CreateUser (..),
    mkCreateUser,

    -- ** Request lenses
    cuGroups,
    cuConsoleAccess,
    cuUsername,
    cuPassword,
    cuBrokerId,

    -- * Destructuring the response
    CreateUserResponse (..),
    mkCreateUserResponse,

    -- ** Response lenses
    cursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Creates a new ActiveMQ user.
--
-- /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
    groups :: Lude.Maybe [Lude.Text],
    -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Lude.Maybe Lude.Bool,
    -- | The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
    username :: Lude.Text,
    -- | Required. The password of the user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
    password :: Lude.Maybe Lude.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUser' with the minimum fields required to make a request.
--
-- * 'groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
-- * 'consoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
-- * 'username' - The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
-- * 'password' - Required. The password of the user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
-- * 'brokerId' - The unique ID that Amazon MQ generates for the broker.
mkCreateUser ::
  -- | 'username'
  Lude.Text ->
  -- | 'brokerId'
  Lude.Text ->
  CreateUser
mkCreateUser pUsername_ pBrokerId_ =
  CreateUser'
    { groups = Lude.Nothing,
      consoleAccess = Lude.Nothing,
      username = pUsername_,
      password = Lude.Nothing,
      brokerId = pBrokerId_
    }

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuGroups :: Lens.Lens' CreateUser (Lude.Maybe [Lude.Text])
cuGroups = Lens.lens (groups :: CreateUser -> Lude.Maybe [Lude.Text]) (\s a -> s {groups = a} :: CreateUser)
{-# DEPRECATED cuGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- /Note:/ Consider using 'consoleAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuConsoleAccess :: Lens.Lens' CreateUser (Lude.Maybe Lude.Bool)
cuConsoleAccess = Lens.lens (consoleAccess :: CreateUser -> Lude.Maybe Lude.Bool) (\s a -> s {consoleAccess = a} :: CreateUser)
{-# DEPRECATED cuConsoleAccess "Use generic-lens or generic-optics with 'consoleAccess' instead." #-}

-- | The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUsername :: Lens.Lens' CreateUser Lude.Text
cuUsername = Lens.lens (username :: CreateUser -> Lude.Text) (\s a -> s {username = a} :: CreateUser)
{-# DEPRECATED cuUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Required. The password of the user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPassword :: Lens.Lens' CreateUser (Lude.Maybe Lude.Text)
cuPassword = Lens.lens (password :: CreateUser -> Lude.Maybe Lude.Text) (\s a -> s {password = a} :: CreateUser)
{-# DEPRECATED cuPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuBrokerId :: Lens.Lens' CreateUser Lude.Text
cuBrokerId = Lens.lens (brokerId :: CreateUser -> Lude.Text) (\s a -> s {brokerId = a} :: CreateUser)
{-# DEPRECATED cuBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

instance Lude.AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request = Req.postJSON mqService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateUserResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("groups" Lude..=) Lude.<$> groups,
            ("consoleAccess" Lude..=) Lude.<$> consoleAccess,
            ("password" Lude..=) Lude.<$> password
          ]
      )

instance Lude.ToPath CreateUser where
  toPath CreateUser' {..} =
    Lude.mconcat
      ["/v1/brokers/", Lude.toBS brokerId, "/users/", Lude.toBS username]

instance Lude.ToQuery CreateUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateUserResponse' smart constructor.
newtype CreateUserResponse = CreateUserResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserResponse
mkCreateUserResponse pResponseStatus_ =
  CreateUserResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursResponseStatus :: Lens.Lens' CreateUserResponse Lude.Int
cursResponseStatus = Lens.lens (responseStatus :: CreateUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserResponse)
{-# DEPRECATED cursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
