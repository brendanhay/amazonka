{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AssociateSoftwareToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a unique generated shared secret key code for the user account. The request takes an access token or a session string, but not both.
module Network.AWS.CognitoIdentityProvider.AssociateSoftwareToken
  ( -- * Creating a request
    AssociateSoftwareToken (..),
    mkAssociateSoftwareToken,

    -- ** Request lenses
    astAccessToken,
    astSession,

    -- * Destructuring the response
    AssociateSoftwareTokenResponse (..),
    mkAssociateSoftwareTokenResponse,

    -- ** Response lenses
    astrsSecretCode,
    astrsSession,
    astrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateSoftwareToken' smart constructor.
data AssociateSoftwareToken = AssociateSoftwareToken'
  { -- | The access token.
    accessToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
    session :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateSoftwareToken' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access token.
-- * 'session' - The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
mkAssociateSoftwareToken ::
  AssociateSoftwareToken
mkAssociateSoftwareToken =
  AssociateSoftwareToken'
    { accessToken = Lude.Nothing,
      session = Lude.Nothing
    }

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astAccessToken :: Lens.Lens' AssociateSoftwareToken (Lude.Maybe (Lude.Sensitive Lude.Text))
astAccessToken = Lens.lens (accessToken :: AssociateSoftwareToken -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {accessToken = a} :: AssociateSoftwareToken)
{-# DEPRECATED astAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astSession :: Lens.Lens' AssociateSoftwareToken (Lude.Maybe Lude.Text)
astSession = Lens.lens (session :: AssociateSoftwareToken -> Lude.Maybe Lude.Text) (\s a -> s {session = a} :: AssociateSoftwareToken)
{-# DEPRECATED astSession "Use generic-lens or generic-optics with 'session' instead." #-}

instance Lude.AWSRequest AssociateSoftwareToken where
  type Rs AssociateSoftwareToken = AssociateSoftwareTokenResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          AssociateSoftwareTokenResponse'
            Lude.<$> (x Lude..?> "SecretCode")
            Lude.<*> (x Lude..?> "Session")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateSoftwareToken where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AssociateSoftwareToken" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateSoftwareToken where
  toJSON AssociateSoftwareToken' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AccessToken" Lude..=) Lude.<$> accessToken,
            ("Session" Lude..=) Lude.<$> session
          ]
      )

instance Lude.ToPath AssociateSoftwareToken where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateSoftwareToken where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateSoftwareTokenResponse' smart constructor.
data AssociateSoftwareTokenResponse = AssociateSoftwareTokenResponse'
  { -- | A unique generated shared secret code that is used in the TOTP algorithm to generate a one time code.
    secretCode :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
    session :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateSoftwareTokenResponse' with the minimum fields required to make a request.
--
-- * 'secretCode' - A unique generated shared secret code that is used in the TOTP algorithm to generate a one time code.
-- * 'session' - The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
-- * 'responseStatus' - The response status code.
mkAssociateSoftwareTokenResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateSoftwareTokenResponse
mkAssociateSoftwareTokenResponse pResponseStatus_ =
  AssociateSoftwareTokenResponse'
    { secretCode = Lude.Nothing,
      session = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique generated shared secret code that is used in the TOTP algorithm to generate a one time code.
--
-- /Note:/ Consider using 'secretCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astrsSecretCode :: Lens.Lens' AssociateSoftwareTokenResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
astrsSecretCode = Lens.lens (secretCode :: AssociateSoftwareTokenResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {secretCode = a} :: AssociateSoftwareTokenResponse)
{-# DEPRECATED astrsSecretCode "Use generic-lens or generic-optics with 'secretCode' instead." #-}

-- | The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astrsSession :: Lens.Lens' AssociateSoftwareTokenResponse (Lude.Maybe Lude.Text)
astrsSession = Lens.lens (session :: AssociateSoftwareTokenResponse -> Lude.Maybe Lude.Text) (\s a -> s {session = a} :: AssociateSoftwareTokenResponse)
{-# DEPRECATED astrsSession "Use generic-lens or generic-optics with 'session' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astrsResponseStatus :: Lens.Lens' AssociateSoftwareTokenResponse Lude.Int
astrsResponseStatus = Lens.lens (responseStatus :: AssociateSoftwareTokenResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateSoftwareTokenResponse)
{-# DEPRECATED astrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
