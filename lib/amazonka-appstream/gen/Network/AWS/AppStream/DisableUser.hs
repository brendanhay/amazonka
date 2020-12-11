{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DisableUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified user in the user pool. Users can't sign in to AppStream 2.0 until they are re-enabled. This action does not delete the user.
module Network.AWS.AppStream.DisableUser
  ( -- * Creating a request
    DisableUser (..),
    mkDisableUser,

    -- ** Request lenses
    dUserName,
    dAuthenticationType,

    -- * Destructuring the response
    DisableUserResponse (..),
    mkDisableUserResponse,

    -- ** Response lenses
    disrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableUser' smart constructor.
data DisableUser = DisableUser'
  { userName ::
      Lude.Sensitive Lude.Text,
    authenticationType :: AuthenticationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableUser' with the minimum fields required to make a request.
--
-- * 'authenticationType' - The authentication type for the user. You must specify USERPOOL.
-- * 'userName' - The email address of the user.
mkDisableUser ::
  -- | 'userName'
  Lude.Sensitive Lude.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  DisableUser
mkDisableUser pUserName_ pAuthenticationType_ =
  DisableUser'
    { userName = pUserName_,
      authenticationType = pAuthenticationType_
    }

-- | The email address of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserName :: Lens.Lens' DisableUser (Lude.Sensitive Lude.Text)
dUserName = Lens.lens (userName :: DisableUser -> Lude.Sensitive Lude.Text) (\s a -> s {userName = a} :: DisableUser)
{-# DEPRECATED dUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The authentication type for the user. You must specify USERPOOL.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAuthenticationType :: Lens.Lens' DisableUser AuthenticationType
dAuthenticationType = Lens.lens (authenticationType :: DisableUser -> AuthenticationType) (\s a -> s {authenticationType = a} :: DisableUser)
{-# DEPRECATED dAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

instance Lude.AWSRequest DisableUser where
  type Rs DisableUser = DisableUserResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisableUserResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.DisableUser" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableUser where
  toJSON DisableUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserName" Lude..= userName),
            Lude.Just ("AuthenticationType" Lude..= authenticationType)
          ]
      )

instance Lude.ToPath DisableUser where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisableUserResponse' smart constructor.
newtype DisableUserResponse = DisableUserResponse'
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

-- | Creates a value of 'DisableUserResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisableUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableUserResponse
mkDisableUserResponse pResponseStatus_ =
  DisableUserResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrsResponseStatus :: Lens.Lens' DisableUserResponse Lude.Int
disrsResponseStatus = Lens.lens (responseStatus :: DisableUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableUserResponse)
{-# DEPRECATED disrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
