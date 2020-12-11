{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.EnableUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a user in the user pool. After being enabled, users can sign in to AppStream 2.0 and open applications from the stacks to which they are assigned.
module Network.AWS.AppStream.EnableUser
  ( -- * Creating a request
    EnableUser (..),
    mkEnableUser,

    -- ** Request lenses
    euUserName,
    euAuthenticationType,

    -- * Destructuring the response
    EnableUserResponse (..),
    mkEnableUserResponse,

    -- ** Response lenses
    eursResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableUser' smart constructor.
data EnableUser = EnableUser'
  { userName :: Lude.Sensitive Lude.Text,
    authenticationType :: AuthenticationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableUser' with the minimum fields required to make a request.
--
-- * 'authenticationType' - The authentication type for the user. You must specify USERPOOL.
-- * 'userName' - The email address of the user.
mkEnableUser ::
  -- | 'userName'
  Lude.Sensitive Lude.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  EnableUser
mkEnableUser pUserName_ pAuthenticationType_ =
  EnableUser'
    { userName = pUserName_,
      authenticationType = pAuthenticationType_
    }

-- | The email address of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
euUserName :: Lens.Lens' EnableUser (Lude.Sensitive Lude.Text)
euUserName = Lens.lens (userName :: EnableUser -> Lude.Sensitive Lude.Text) (\s a -> s {userName = a} :: EnableUser)
{-# DEPRECATED euUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The authentication type for the user. You must specify USERPOOL.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
euAuthenticationType :: Lens.Lens' EnableUser AuthenticationType
euAuthenticationType = Lens.lens (authenticationType :: EnableUser -> AuthenticationType) (\s a -> s {authenticationType = a} :: EnableUser)
{-# DEPRECATED euAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

instance Lude.AWSRequest EnableUser where
  type Rs EnableUser = EnableUserResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveEmpty
      ( \s h x ->
          EnableUserResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.EnableUser" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableUser where
  toJSON EnableUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserName" Lude..= userName),
            Lude.Just ("AuthenticationType" Lude..= authenticationType)
          ]
      )

instance Lude.ToPath EnableUser where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableUserResponse' smart constructor.
newtype EnableUserResponse = EnableUserResponse'
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

-- | Creates a value of 'EnableUserResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkEnableUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableUserResponse
mkEnableUserResponse pResponseStatus_ =
  EnableUserResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eursResponseStatus :: Lens.Lens' EnableUserResponse Lude.Int
eursResponseStatus = Lens.lens (responseStatus :: EnableUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableUserResponse)
{-# DEPRECATED eursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
