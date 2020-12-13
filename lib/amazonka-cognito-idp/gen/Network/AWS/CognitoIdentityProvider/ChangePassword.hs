{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ChangePassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the password for a specified user in a user pool.
module Network.AWS.CognitoIdentityProvider.ChangePassword
  ( -- * Creating a request
    ChangePassword (..),
    mkChangePassword,

    -- ** Request lenses
    cpAccessToken,
    cpProposedPassword,
    cpPreviousPassword,

    -- * Destructuring the response
    ChangePasswordResponse (..),
    mkChangePasswordResponse,

    -- ** Response lenses
    cprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to change a user password.
--
-- /See:/ 'mkChangePassword' smart constructor.
data ChangePassword = ChangePassword'
  { -- | The access token.
    accessToken :: Lude.Sensitive Lude.Text,
    -- | The new password.
    proposedPassword :: Lude.Sensitive Lude.Text,
    -- | The old password.
    previousPassword :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangePassword' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access token.
-- * 'proposedPassword' - The new password.
-- * 'previousPassword' - The old password.
mkChangePassword ::
  -- | 'accessToken'
  Lude.Sensitive Lude.Text ->
  -- | 'proposedPassword'
  Lude.Sensitive Lude.Text ->
  -- | 'previousPassword'
  Lude.Sensitive Lude.Text ->
  ChangePassword
mkChangePassword
  pAccessToken_
  pProposedPassword_
  pPreviousPassword_ =
    ChangePassword'
      { accessToken = pAccessToken_,
        proposedPassword = pProposedPassword_,
        previousPassword = pPreviousPassword_
      }

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpAccessToken :: Lens.Lens' ChangePassword (Lude.Sensitive Lude.Text)
cpAccessToken = Lens.lens (accessToken :: ChangePassword -> Lude.Sensitive Lude.Text) (\s a -> s {accessToken = a} :: ChangePassword)
{-# DEPRECATED cpAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | The new password.
--
-- /Note:/ Consider using 'proposedPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpProposedPassword :: Lens.Lens' ChangePassword (Lude.Sensitive Lude.Text)
cpProposedPassword = Lens.lens (proposedPassword :: ChangePassword -> Lude.Sensitive Lude.Text) (\s a -> s {proposedPassword = a} :: ChangePassword)
{-# DEPRECATED cpProposedPassword "Use generic-lens or generic-optics with 'proposedPassword' instead." #-}

-- | The old password.
--
-- /Note:/ Consider using 'previousPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPreviousPassword :: Lens.Lens' ChangePassword (Lude.Sensitive Lude.Text)
cpPreviousPassword = Lens.lens (previousPassword :: ChangePassword -> Lude.Sensitive Lude.Text) (\s a -> s {previousPassword = a} :: ChangePassword)
{-# DEPRECATED cpPreviousPassword "Use generic-lens or generic-optics with 'previousPassword' instead." #-}

instance Lude.AWSRequest ChangePassword where
  type Rs ChangePassword = ChangePasswordResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ChangePasswordResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ChangePassword where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.ChangePassword" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ChangePassword where
  toJSON ChangePassword' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccessToken" Lude..= accessToken),
            Lude.Just ("ProposedPassword" Lude..= proposedPassword),
            Lude.Just ("PreviousPassword" Lude..= previousPassword)
          ]
      )

instance Lude.ToPath ChangePassword where
  toPath = Lude.const "/"

instance Lude.ToQuery ChangePassword where
  toQuery = Lude.const Lude.mempty

-- | The response from the server to the change password request.
--
-- /See:/ 'mkChangePasswordResponse' smart constructor.
newtype ChangePasswordResponse = ChangePasswordResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangePasswordResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkChangePasswordResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ChangePasswordResponse
mkChangePasswordResponse pResponseStatus_ =
  ChangePasswordResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' ChangePasswordResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: ChangePasswordResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ChangePasswordResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
