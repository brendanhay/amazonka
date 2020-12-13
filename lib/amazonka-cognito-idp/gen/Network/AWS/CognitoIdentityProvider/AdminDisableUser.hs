{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminDisableUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified user.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminDisableUser
  ( -- * Creating a request
    AdminDisableUser (..),
    mkAdminDisableUser,

    -- ** Request lenses
    aUserPoolId,
    aUsername,

    -- * Destructuring the response
    AdminDisableUserResponse (..),
    mkAdminDisableUserResponse,

    -- ** Response lenses
    adursResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to disable any user as an administrator.
--
-- /See:/ 'mkAdminDisableUser' smart constructor.
data AdminDisableUser = AdminDisableUser'
  { -- | The user pool ID for the user pool where you want to disable the user.
    userPoolId :: Lude.Text,
    -- | The user name of the user you wish to disable.
    username :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminDisableUser' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID for the user pool where you want to disable the user.
-- * 'username' - The user name of the user you wish to disable.
mkAdminDisableUser ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  AdminDisableUser
mkAdminDisableUser pUserPoolId_ pUsername_ =
  AdminDisableUser'
    { userPoolId = pUserPoolId_,
      username = pUsername_
    }

-- | The user pool ID for the user pool where you want to disable the user.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aUserPoolId :: Lens.Lens' AdminDisableUser Lude.Text
aUserPoolId = Lens.lens (userPoolId :: AdminDisableUser -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminDisableUser)
{-# DEPRECATED aUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name of the user you wish to disable.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aUsername :: Lens.Lens' AdminDisableUser (Lude.Sensitive Lude.Text)
aUsername = Lens.lens (username :: AdminDisableUser -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminDisableUser)
{-# DEPRECATED aUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Lude.AWSRequest AdminDisableUser where
  type Rs AdminDisableUser = AdminDisableUserResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AdminDisableUserResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminDisableUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminDisableUser" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminDisableUser where
  toJSON AdminDisableUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username)
          ]
      )

instance Lude.ToPath AdminDisableUser where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminDisableUser where
  toQuery = Lude.const Lude.mempty

-- | Represents the response received from the server to disable the user as an administrator.
--
-- /See:/ 'mkAdminDisableUserResponse' smart constructor.
newtype AdminDisableUserResponse = AdminDisableUserResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminDisableUserResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAdminDisableUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminDisableUserResponse
mkAdminDisableUserResponse pResponseStatus_ =
  AdminDisableUserResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adursResponseStatus :: Lens.Lens' AdminDisableUserResponse Lude.Int
adursResponseStatus = Lens.lens (responseStatus :: AdminDisableUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminDisableUserResponse)
{-# DEPRECATED adursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
