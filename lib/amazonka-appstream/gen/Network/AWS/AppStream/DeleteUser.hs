{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user from the user pool.
module Network.AWS.AppStream.DeleteUser
  ( -- * Creating a request
    DeleteUser (..),
    mkDeleteUser,

    -- ** Request lenses
    delUserName,
    delAuthenticationType,

    -- * Destructuring the response
    DeleteUserResponse (..),
    mkDeleteUserResponse,

    -- ** Response lenses
    delrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { userName :: Lude.Sensitive Lude.Text,
    authenticationType :: AuthenticationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- * 'authenticationType' - The authentication type for the user. You must specify USERPOOL.
-- * 'userName' - The email address of the user.
mkDeleteUser ::
  -- | 'userName'
  Lude.Sensitive Lude.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  DeleteUser
mkDeleteUser pUserName_ pAuthenticationType_ =
  DeleteUser'
    { userName = pUserName_,
      authenticationType = pAuthenticationType_
    }

-- | The email address of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delUserName :: Lens.Lens' DeleteUser (Lude.Sensitive Lude.Text)
delUserName = Lens.lens (userName :: DeleteUser -> Lude.Sensitive Lude.Text) (\s a -> s {userName = a} :: DeleteUser)
{-# DEPRECATED delUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The authentication type for the user. You must specify USERPOOL.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delAuthenticationType :: Lens.Lens' DeleteUser AuthenticationType
delAuthenticationType = Lens.lens (authenticationType :: DeleteUser -> AuthenticationType) (\s a -> s {authenticationType = a} :: DeleteUser)
{-# DEPRECATED delAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

instance Lude.AWSRequest DeleteUser where
  type Rs DeleteUser = DeleteUserResponse
  request = Req.postJSON appStreamService
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
              Lude.=# ("PhotonAdminProxyService.DeleteUser" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteUser where
  toJSON DeleteUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserName" Lude..= userName),
            Lude.Just ("AuthenticationType" Lude..= authenticationType)
          ]
      )

instance Lude.ToPath DeleteUser where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUserResponse' smart constructor.
newtype DeleteUserResponse = DeleteUserResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
delrsResponseStatus :: Lens.Lens' DeleteUserResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeleteUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteUserResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
