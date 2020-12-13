{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user from Amazon WorkMail and all subsequent systems. Before you can delete a user, the user state must be @DISABLED@ . Use the 'DescribeUser' action to confirm the user state.
--
-- Deleting a user is permanent and cannot be undone. WorkMail archives user mailboxes for 30 days before they are permanently removed.
module Network.AWS.WorkMail.DeleteUser
  ( -- * Creating a request
    DeleteUser (..),
    mkDeleteUser,

    -- ** Request lenses
    dUserId,
    dOrganizationId,

    -- * Destructuring the response
    DeleteUserResponse (..),
    mkDeleteUserResponse,

    -- ** Response lenses
    dufrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | The identifier of the user to be deleted.
    userId :: Lude.Text,
    -- | The organization that contains the user to be deleted.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- * 'userId' - The identifier of the user to be deleted.
-- * 'organizationId' - The organization that contains the user to be deleted.
mkDeleteUser ::
  -- | 'userId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  DeleteUser
mkDeleteUser pUserId_ pOrganizationId_ =
  DeleteUser' {userId = pUserId_, organizationId = pOrganizationId_}

-- | The identifier of the user to be deleted.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserId :: Lens.Lens' DeleteUser Lude.Text
dUserId = Lens.lens (userId :: DeleteUser -> Lude.Text) (\s a -> s {userId = a} :: DeleteUser)
{-# DEPRECATED dUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The organization that contains the user to be deleted.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOrganizationId :: Lens.Lens' DeleteUser Lude.Text
dOrganizationId = Lens.lens (organizationId :: DeleteUser -> Lude.Text) (\s a -> s {organizationId = a} :: DeleteUser)
{-# DEPRECATED dOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest DeleteUser where
  type Rs DeleteUser = DeleteUserResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteUserResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.DeleteUser" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteUser where
  toJSON DeleteUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserId" Lude..= userId),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath DeleteUser where
  toPath = Lude.const "/"

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
