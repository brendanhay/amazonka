{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminEnableUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified user as an administrator. Works on any user.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminEnableUser
  ( -- * Creating a request
    AdminEnableUser (..),
    mkAdminEnableUser,

    -- ** Request lenses
    aeuUserPoolId,
    aeuUsername,

    -- * Destructuring the response
    AdminEnableUserResponse (..),
    mkAdminEnableUserResponse,

    -- ** Response lenses
    aeursResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request that enables the user as an administrator.
--
-- /See:/ 'mkAdminEnableUser' smart constructor.
data AdminEnableUser = AdminEnableUser'
  { -- | The user pool ID for the user pool where you want to enable the user.
    userPoolId :: Lude.Text,
    -- | The user name of the user you wish to enable.
    username :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminEnableUser' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID for the user pool where you want to enable the user.
-- * 'username' - The user name of the user you wish to enable.
mkAdminEnableUser ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  AdminEnableUser
mkAdminEnableUser pUserPoolId_ pUsername_ =
  AdminEnableUser'
    { userPoolId = pUserPoolId_,
      username = pUsername_
    }

-- | The user pool ID for the user pool where you want to enable the user.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeuUserPoolId :: Lens.Lens' AdminEnableUser Lude.Text
aeuUserPoolId = Lens.lens (userPoolId :: AdminEnableUser -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminEnableUser)
{-# DEPRECATED aeuUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name of the user you wish to enable.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeuUsername :: Lens.Lens' AdminEnableUser (Lude.Sensitive Lude.Text)
aeuUsername = Lens.lens (username :: AdminEnableUser -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminEnableUser)
{-# DEPRECATED aeuUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Lude.AWSRequest AdminEnableUser where
  type Rs AdminEnableUser = AdminEnableUserResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AdminEnableUserResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminEnableUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminEnableUser" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminEnableUser where
  toJSON AdminEnableUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username)
          ]
      )

instance Lude.ToPath AdminEnableUser where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminEnableUser where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server for the request to enable a user as an administrator.
--
-- /See:/ 'mkAdminEnableUserResponse' smart constructor.
newtype AdminEnableUserResponse = AdminEnableUserResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminEnableUserResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAdminEnableUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminEnableUserResponse
mkAdminEnableUserResponse pResponseStatus_ =
  AdminEnableUserResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeursResponseStatus :: Lens.Lens' AdminEnableUserResponse Lude.Int
aeursResponseStatus = Lens.lens (responseStatus :: AdminEnableUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminEnableUserResponse)
{-# DEPRECATED aeursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
