{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUserGlobalSignOut
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Signs out users from all devices, as an administrator. It also invalidates all refresh tokens issued to a user. The user's current access and Id tokens remain valid until their expiry. Access and Id tokens expire one hour after they are issued.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminUserGlobalSignOut
  ( -- * Creating a request
    AdminUserGlobalSignOut (..),
    mkAdminUserGlobalSignOut,

    -- ** Request lenses
    augsoUserPoolId,
    augsoUsername,

    -- * Destructuring the response
    AdminUserGlobalSignOutResponse (..),
    mkAdminUserGlobalSignOutResponse,

    -- ** Response lenses
    augsorsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to sign out of all devices, as an administrator.
--
-- /See:/ 'mkAdminUserGlobalSignOut' smart constructor.
data AdminUserGlobalSignOut = AdminUserGlobalSignOut'
  { userPoolId ::
      Lude.Text,
    username :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminUserGlobalSignOut' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID.
-- * 'username' - The user name.
mkAdminUserGlobalSignOut ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  AdminUserGlobalSignOut
mkAdminUserGlobalSignOut pUserPoolId_ pUsername_ =
  AdminUserGlobalSignOut'
    { userPoolId = pUserPoolId_,
      username = pUsername_
    }

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
augsoUserPoolId :: Lens.Lens' AdminUserGlobalSignOut Lude.Text
augsoUserPoolId = Lens.lens (userPoolId :: AdminUserGlobalSignOut -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminUserGlobalSignOut)
{-# DEPRECATED augsoUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
augsoUsername :: Lens.Lens' AdminUserGlobalSignOut (Lude.Sensitive Lude.Text)
augsoUsername = Lens.lens (username :: AdminUserGlobalSignOut -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminUserGlobalSignOut)
{-# DEPRECATED augsoUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Lude.AWSRequest AdminUserGlobalSignOut where
  type Rs AdminUserGlobalSignOut = AdminUserGlobalSignOutResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AdminUserGlobalSignOutResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminUserGlobalSignOut where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminUserGlobalSignOut" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminUserGlobalSignOut where
  toJSON AdminUserGlobalSignOut' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username)
          ]
      )

instance Lude.ToPath AdminUserGlobalSignOut where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminUserGlobalSignOut where
  toQuery = Lude.const Lude.mempty

-- | The global sign-out response, as an administrator.
--
-- /See:/ 'mkAdminUserGlobalSignOutResponse' smart constructor.
newtype AdminUserGlobalSignOutResponse = AdminUserGlobalSignOutResponse'
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

-- | Creates a value of 'AdminUserGlobalSignOutResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAdminUserGlobalSignOutResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminUserGlobalSignOutResponse
mkAdminUserGlobalSignOutResponse pResponseStatus_ =
  AdminUserGlobalSignOutResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
augsorsResponseStatus :: Lens.Lens' AdminUserGlobalSignOutResponse Lude.Int
augsorsResponseStatus = Lens.lens (responseStatus :: AdminUserGlobalSignOutResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminUserGlobalSignOutResponse)
{-# DEPRECATED augsorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
