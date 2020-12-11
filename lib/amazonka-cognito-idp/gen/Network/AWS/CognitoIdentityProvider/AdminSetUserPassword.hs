{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminSetUserPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified user's password in a user pool as an administrator. Works on any user.
--
-- The password can be temporary or permanent. If it is temporary, the user status will be placed into the @FORCE_CHANGE_PASSWORD@ state. When the user next tries to sign in, the InitiateAuth/AdminInitiateAuth response will contain the @NEW_PASSWORD_REQUIRED@ challenge. If the user does not sign in before it expires, the user will not be able to sign in and their password will need to be reset by an administrator.
-- Once the user has set a new password, or the password is permanent, the user status will be set to @Confirmed@ .
module Network.AWS.CognitoIdentityProvider.AdminSetUserPassword
  ( -- * Creating a request
    AdminSetUserPassword (..),
    mkAdminSetUserPassword,

    -- ** Request lenses
    asupPermanent,
    asupUserPoolId,
    asupUsername,
    asupPassword,

    -- * Destructuring the response
    AdminSetUserPasswordResponse (..),
    mkAdminSetUserPasswordResponse,

    -- ** Response lenses
    asuprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAdminSetUserPassword' smart constructor.
data AdminSetUserPassword = AdminSetUserPassword'
  { permanent ::
      Lude.Maybe Lude.Bool,
    userPoolId :: Lude.Text,
    username :: Lude.Sensitive Lude.Text,
    password :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminSetUserPassword' with the minimum fields required to make a request.
--
-- * 'password' - The password for the user.
-- * 'permanent' - @True@ if the password is permanent, @False@ if it is temporary.
-- * 'userPoolId' - The user pool ID for the user pool where you want to set the user's password.
-- * 'username' - The user name of the user whose password you wish to set.
mkAdminSetUserPassword ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  -- | 'password'
  Lude.Sensitive Lude.Text ->
  AdminSetUserPassword
mkAdminSetUserPassword pUserPoolId_ pUsername_ pPassword_ =
  AdminSetUserPassword'
    { permanent = Lude.Nothing,
      userPoolId = pUserPoolId_,
      username = pUsername_,
      password = pPassword_
    }

-- | @True@ if the password is permanent, @False@ if it is temporary.
--
-- /Note:/ Consider using 'permanent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asupPermanent :: Lens.Lens' AdminSetUserPassword (Lude.Maybe Lude.Bool)
asupPermanent = Lens.lens (permanent :: AdminSetUserPassword -> Lude.Maybe Lude.Bool) (\s a -> s {permanent = a} :: AdminSetUserPassword)
{-# DEPRECATED asupPermanent "Use generic-lens or generic-optics with 'permanent' instead." #-}

-- | The user pool ID for the user pool where you want to set the user's password.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asupUserPoolId :: Lens.Lens' AdminSetUserPassword Lude.Text
asupUserPoolId = Lens.lens (userPoolId :: AdminSetUserPassword -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminSetUserPassword)
{-# DEPRECATED asupUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name of the user whose password you wish to set.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asupUsername :: Lens.Lens' AdminSetUserPassword (Lude.Sensitive Lude.Text)
asupUsername = Lens.lens (username :: AdminSetUserPassword -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminSetUserPassword)
{-# DEPRECATED asupUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The password for the user.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asupPassword :: Lens.Lens' AdminSetUserPassword (Lude.Sensitive Lude.Text)
asupPassword = Lens.lens (password :: AdminSetUserPassword -> Lude.Sensitive Lude.Text) (\s a -> s {password = a} :: AdminSetUserPassword)
{-# DEPRECATED asupPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Lude.AWSRequest AdminSetUserPassword where
  type Rs AdminSetUserPassword = AdminSetUserPasswordResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AdminSetUserPasswordResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminSetUserPassword where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminSetUserPassword" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminSetUserPassword where
  toJSON AdminSetUserPassword' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Permanent" Lude..=) Lude.<$> permanent,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username),
            Lude.Just ("Password" Lude..= password)
          ]
      )

instance Lude.ToPath AdminSetUserPassword where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminSetUserPassword where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAdminSetUserPasswordResponse' smart constructor.
newtype AdminSetUserPasswordResponse = AdminSetUserPasswordResponse'
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

-- | Creates a value of 'AdminSetUserPasswordResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAdminSetUserPasswordResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminSetUserPasswordResponse
mkAdminSetUserPasswordResponse pResponseStatus_ =
  AdminSetUserPasswordResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asuprsResponseStatus :: Lens.Lens' AdminSetUserPasswordResponse Lude.Int
asuprsResponseStatus = Lens.lens (responseStatus :: AdminSetUserPasswordResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminSetUserPasswordResponse)
{-# DEPRECATED asuprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
