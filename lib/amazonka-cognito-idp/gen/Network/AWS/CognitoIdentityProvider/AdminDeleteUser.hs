{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminDeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user as an administrator. Works on any user.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminDeleteUser
  ( -- * Creating a request
    AdminDeleteUser (..),
    mkAdminDeleteUser,

    -- ** Request lenses
    aUserPoolId,
    aUsername,

    -- * Destructuring the response
    AdminDeleteUserResponse (..),
    mkAdminDeleteUserResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to delete a user as an administrator.
--
-- /See:/ 'mkAdminDeleteUser' smart constructor.
data AdminDeleteUser = AdminDeleteUser'
  { userPoolId :: Lude.Text,
    username :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminDeleteUser' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID for the user pool where you want to delete the user.
-- * 'username' - The user name of the user you wish to delete.
mkAdminDeleteUser ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  AdminDeleteUser
mkAdminDeleteUser pUserPoolId_ pUsername_ =
  AdminDeleteUser'
    { userPoolId = pUserPoolId_,
      username = pUsername_
    }

-- | The user pool ID for the user pool where you want to delete the user.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aUserPoolId :: Lens.Lens' AdminDeleteUser Lude.Text
aUserPoolId = Lens.lens (userPoolId :: AdminDeleteUser -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminDeleteUser)
{-# DEPRECATED aUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name of the user you wish to delete.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aUsername :: Lens.Lens' AdminDeleteUser (Lude.Sensitive Lude.Text)
aUsername = Lens.lens (username :: AdminDeleteUser -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminDeleteUser)
{-# DEPRECATED aUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Lude.AWSRequest AdminDeleteUser where
  type Rs AdminDeleteUser = AdminDeleteUserResponse
  request = Req.postJSON cognitoIdentityProviderService
  response = Res.receiveNull AdminDeleteUserResponse'

instance Lude.ToHeaders AdminDeleteUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminDeleteUser" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminDeleteUser where
  toJSON AdminDeleteUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username)
          ]
      )

instance Lude.ToPath AdminDeleteUser where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminDeleteUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAdminDeleteUserResponse' smart constructor.
data AdminDeleteUserResponse = AdminDeleteUserResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminDeleteUserResponse' with the minimum fields required to make a request.
mkAdminDeleteUserResponse ::
  AdminDeleteUserResponse
mkAdminDeleteUserResponse = AdminDeleteUserResponse'
