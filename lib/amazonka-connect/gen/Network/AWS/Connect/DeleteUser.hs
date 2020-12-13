{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user account from the specified Amazon Connect instance.
--
-- For information about what happens to a user's data when their account is deleted, see <https://docs.aws.amazon.com/connect/latest/adminguide/delete-users.html Delete Users from Your Amazon Connect Instance> in the /Amazon Connect Administrator Guide/ .
module Network.AWS.Connect.DeleteUser
  ( -- * Creating a request
    DeleteUser (..),
    mkDeleteUser,

    -- ** Request lenses
    dufInstanceId,
    dufUserId,

    -- * Destructuring the response
    DeleteUserResponse (..),
    mkDeleteUserResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The identifier of the user.
    userId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'userId' - The identifier of the user.
mkDeleteUser ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  DeleteUser
mkDeleteUser pInstanceId_ pUserId_ =
  DeleteUser' {instanceId = pInstanceId_, userId = pUserId_}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dufInstanceId :: Lens.Lens' DeleteUser Lude.Text
dufInstanceId = Lens.lens (instanceId :: DeleteUser -> Lude.Text) (\s a -> s {instanceId = a} :: DeleteUser)
{-# DEPRECATED dufInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dufUserId :: Lens.Lens' DeleteUser Lude.Text
dufUserId = Lens.lens (userId :: DeleteUser -> Lude.Text) (\s a -> s {userId = a} :: DeleteUser)
{-# DEPRECATED dufUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.AWSRequest DeleteUser where
  type Rs DeleteUser = DeleteUserResponse
  request = Req.delete connectService
  response = Res.receiveNull DeleteUserResponse'

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
      ["/users/", Lude.toBS instanceId, "/", Lude.toBS userId]

instance Lude.ToQuery DeleteUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserResponse' with the minimum fields required to make a request.
mkDeleteUserResponse ::
  DeleteUserResponse
mkDeleteUserResponse = DeleteUserResponse'
