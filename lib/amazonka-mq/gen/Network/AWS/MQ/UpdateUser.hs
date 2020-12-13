{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.UpdateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the information for an ActiveMQ user.
module Network.AWS.MQ.UpdateUser
  ( -- * Creating a request
    UpdateUser (..),
    mkUpdateUser,

    -- ** Request lenses
    uuGroups,
    uuConsoleAccess,
    uuUsername,
    uuPassword,
    uuBrokerId,

    -- * Destructuring the response
    UpdateUserResponse (..),
    mkUpdateUserResponse,

    -- ** Response lenses
    uursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Updates the information for an ActiveMQ user.
--
-- /See:/ 'mkUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
    groups :: Lude.Maybe [Lude.Text],
    -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Lude.Maybe Lude.Bool,
    -- | Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
    username :: Lude.Text,
    -- | The password of the user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
    password :: Lude.Maybe Lude.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUser' with the minimum fields required to make a request.
--
-- * 'groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
-- * 'consoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
-- * 'username' - Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
-- * 'password' - The password of the user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
-- * 'brokerId' - The unique ID that Amazon MQ generates for the broker.
mkUpdateUser ::
  -- | 'username'
  Lude.Text ->
  -- | 'brokerId'
  Lude.Text ->
  UpdateUser
mkUpdateUser pUsername_ pBrokerId_ =
  UpdateUser'
    { groups = Lude.Nothing,
      consoleAccess = Lude.Nothing,
      username = pUsername_,
      password = Lude.Nothing,
      brokerId = pBrokerId_
    }

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuGroups :: Lens.Lens' UpdateUser (Lude.Maybe [Lude.Text])
uuGroups = Lens.lens (groups :: UpdateUser -> Lude.Maybe [Lude.Text]) (\s a -> s {groups = a} :: UpdateUser)
{-# DEPRECATED uuGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- /Note:/ Consider using 'consoleAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuConsoleAccess :: Lens.Lens' UpdateUser (Lude.Maybe Lude.Bool)
uuConsoleAccess = Lens.lens (consoleAccess :: UpdateUser -> Lude.Maybe Lude.Bool) (\s a -> s {consoleAccess = a} :: UpdateUser)
{-# DEPRECATED uuConsoleAccess "Use generic-lens or generic-optics with 'consoleAccess' instead." #-}

-- | Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuUsername :: Lens.Lens' UpdateUser Lude.Text
uuUsername = Lens.lens (username :: UpdateUser -> Lude.Text) (\s a -> s {username = a} :: UpdateUser)
{-# DEPRECATED uuUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The password of the user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuPassword :: Lens.Lens' UpdateUser (Lude.Maybe Lude.Text)
uuPassword = Lens.lens (password :: UpdateUser -> Lude.Maybe Lude.Text) (\s a -> s {password = a} :: UpdateUser)
{-# DEPRECATED uuPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuBrokerId :: Lens.Lens' UpdateUser Lude.Text
uuBrokerId = Lens.lens (brokerId :: UpdateUser -> Lude.Text) (\s a -> s {brokerId = a} :: UpdateUser)
{-# DEPRECATED uuBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

instance Lude.AWSRequest UpdateUser where
  type Rs UpdateUser = UpdateUserResponse
  request = Req.putJSON mqService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateUserResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUser where
  toJSON UpdateUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("groups" Lude..=) Lude.<$> groups,
            ("consoleAccess" Lude..=) Lude.<$> consoleAccess,
            ("password" Lude..=) Lude.<$> password
          ]
      )

instance Lude.ToPath UpdateUser where
  toPath UpdateUser' {..} =
    Lude.mconcat
      ["/v1/brokers/", Lude.toBS brokerId, "/users/", Lude.toBS username]

instance Lude.ToQuery UpdateUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateUserResponse' smart constructor.
newtype UpdateUserResponse = UpdateUserResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateUserResponse
mkUpdateUserResponse pResponseStatus_ =
  UpdateUserResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uursResponseStatus :: Lens.Lens' UpdateUserResponse Lude.Int
uursResponseStatus = Lens.lens (responseStatus :: UpdateUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateUserResponse)
{-# DEPRECATED uursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
