{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a user to delete himself or herself.
module Network.AWS.CognitoIdentityProvider.DeleteUser
  ( -- * Creating a request
    DeleteUser (..),
    mkDeleteUser,

    -- ** Request lenses
    duAccessToken,

    -- * Destructuring the response
    DeleteUserResponse (..),
    mkDeleteUserResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to delete a user.
--
-- /See:/ 'mkDeleteUser' smart constructor.
newtype DeleteUser = DeleteUser'
  { accessToken ::
      Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access token from a request to delete a user.
mkDeleteUser ::
  -- | 'accessToken'
  Lude.Sensitive Lude.Text ->
  DeleteUser
mkDeleteUser pAccessToken_ =
  DeleteUser' {accessToken = pAccessToken_}

-- | The access token from a request to delete a user.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duAccessToken :: Lens.Lens' DeleteUser (Lude.Sensitive Lude.Text)
duAccessToken = Lens.lens (accessToken :: DeleteUser -> Lude.Sensitive Lude.Text) (\s a -> s {accessToken = a} :: DeleteUser)
{-# DEPRECATED duAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

instance Lude.AWSRequest DeleteUser where
  type Rs DeleteUser = DeleteUserResponse
  request = Req.postJSON cognitoIdentityProviderService
  response = Res.receiveNull DeleteUserResponse'

instance Lude.ToHeaders DeleteUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.DeleteUser" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteUser where
  toJSON DeleteUser' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AccessToken" Lude..= accessToken)])

instance Lude.ToPath DeleteUser where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserResponse' with the minimum fields required to make a request.
mkDeleteUserResponse ::
  DeleteUserResponse
mkDeleteUserResponse = DeleteUserResponse'
