{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an ActiveMQ user.
module Network.AWS.MQ.DeleteUser
  ( -- * Creating a request
    DeleteUser (..),
    mkDeleteUser,

    -- ** Request lenses
    dUsername,
    dBrokerId,

    -- * Destructuring the response
    DeleteUserResponse (..),
    mkDeleteUserResponse,

    -- ** Response lenses
    dufrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
    username :: Lude.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- * 'username' - The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
-- * 'brokerId' - The unique ID that Amazon MQ generates for the broker.
mkDeleteUser ::
  -- | 'username'
  Lude.Text ->
  -- | 'brokerId'
  Lude.Text ->
  DeleteUser
mkDeleteUser pUsername_ pBrokerId_ =
  DeleteUser' {username = pUsername_, brokerId = pBrokerId_}

-- | The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUsername :: Lens.Lens' DeleteUser Lude.Text
dUsername = Lens.lens (username :: DeleteUser -> Lude.Text) (\s a -> s {username = a} :: DeleteUser)
{-# DEPRECATED dUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBrokerId :: Lens.Lens' DeleteUser Lude.Text
dBrokerId = Lens.lens (brokerId :: DeleteUser -> Lude.Text) (\s a -> s {brokerId = a} :: DeleteUser)
{-# DEPRECATED dBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

instance Lude.AWSRequest DeleteUser where
  type Rs DeleteUser = DeleteUserResponse
  request = Req.delete mqService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteUserResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteUser where
  toPath DeleteUser' {..} =
    Lude.mconcat
      ["/v1/brokers/", Lude.toBS brokerId, "/users/", Lude.toBS username]

instance Lude.ToQuery DeleteUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUserResponse' smart constructor.
newtype DeleteUserResponse = DeleteUserResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteUserResponse
mkDeleteUserResponse pResponseStatus_ =
  DeleteUserResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dufrsResponseStatus :: Lens.Lens' DeleteUserResponse Lude.Int
dufrsResponseStatus = Lens.lens (responseStatus :: DeleteUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteUserResponse)
{-# DEPRECATED dufrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
