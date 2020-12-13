{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.VerifySoftwareToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API to register a user's entered TOTP code and mark the user's software token MFA status as "verified" if successful. The request takes an access token or a session string, but not both.
module Network.AWS.CognitoIdentityProvider.VerifySoftwareToken
  ( -- * Creating a request
    VerifySoftwareToken (..),
    mkVerifySoftwareToken,

    -- ** Request lenses
    vstAccessToken,
    vstFriendlyDeviceName,
    vstUserCode,
    vstSession,

    -- * Destructuring the response
    VerifySoftwareTokenResponse (..),
    mkVerifySoftwareTokenResponse,

    -- ** Response lenses
    vstrsStatus,
    vstrsSession,
    vstrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkVerifySoftwareToken' smart constructor.
data VerifySoftwareToken = VerifySoftwareToken'
  { -- | The access token.
    accessToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The friendly device name.
    friendlyDeviceName :: Lude.Maybe Lude.Text,
    -- | The one time password computed using the secret code returned by <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AssociateSoftwareToken.html AssociateSoftwareToken"> .
    userCode :: Lude.Text,
    -- | The session which should be passed both ways in challenge-response calls to the service.
    session :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VerifySoftwareToken' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access token.
-- * 'friendlyDeviceName' - The friendly device name.
-- * 'userCode' - The one time password computed using the secret code returned by <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AssociateSoftwareToken.html AssociateSoftwareToken"> .
-- * 'session' - The session which should be passed both ways in challenge-response calls to the service.
mkVerifySoftwareToken ::
  -- | 'userCode'
  Lude.Text ->
  VerifySoftwareToken
mkVerifySoftwareToken pUserCode_ =
  VerifySoftwareToken'
    { accessToken = Lude.Nothing,
      friendlyDeviceName = Lude.Nothing,
      userCode = pUserCode_,
      session = Lude.Nothing
    }

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vstAccessToken :: Lens.Lens' VerifySoftwareToken (Lude.Maybe (Lude.Sensitive Lude.Text))
vstAccessToken = Lens.lens (accessToken :: VerifySoftwareToken -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {accessToken = a} :: VerifySoftwareToken)
{-# DEPRECATED vstAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | The friendly device name.
--
-- /Note:/ Consider using 'friendlyDeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vstFriendlyDeviceName :: Lens.Lens' VerifySoftwareToken (Lude.Maybe Lude.Text)
vstFriendlyDeviceName = Lens.lens (friendlyDeviceName :: VerifySoftwareToken -> Lude.Maybe Lude.Text) (\s a -> s {friendlyDeviceName = a} :: VerifySoftwareToken)
{-# DEPRECATED vstFriendlyDeviceName "Use generic-lens or generic-optics with 'friendlyDeviceName' instead." #-}

-- | The one time password computed using the secret code returned by <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AssociateSoftwareToken.html AssociateSoftwareToken"> .
--
-- /Note:/ Consider using 'userCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vstUserCode :: Lens.Lens' VerifySoftwareToken Lude.Text
vstUserCode = Lens.lens (userCode :: VerifySoftwareToken -> Lude.Text) (\s a -> s {userCode = a} :: VerifySoftwareToken)
{-# DEPRECATED vstUserCode "Use generic-lens or generic-optics with 'userCode' instead." #-}

-- | The session which should be passed both ways in challenge-response calls to the service.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vstSession :: Lens.Lens' VerifySoftwareToken (Lude.Maybe Lude.Text)
vstSession = Lens.lens (session :: VerifySoftwareToken -> Lude.Maybe Lude.Text) (\s a -> s {session = a} :: VerifySoftwareToken)
{-# DEPRECATED vstSession "Use generic-lens or generic-optics with 'session' instead." #-}

instance Lude.AWSRequest VerifySoftwareToken where
  type Rs VerifySoftwareToken = VerifySoftwareTokenResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          VerifySoftwareTokenResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "Session")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders VerifySoftwareToken where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.VerifySoftwareToken" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON VerifySoftwareToken where
  toJSON VerifySoftwareToken' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AccessToken" Lude..=) Lude.<$> accessToken,
            ("FriendlyDeviceName" Lude..=) Lude.<$> friendlyDeviceName,
            Lude.Just ("UserCode" Lude..= userCode),
            ("Session" Lude..=) Lude.<$> session
          ]
      )

instance Lude.ToPath VerifySoftwareToken where
  toPath = Lude.const "/"

instance Lude.ToQuery VerifySoftwareToken where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkVerifySoftwareTokenResponse' smart constructor.
data VerifySoftwareTokenResponse = VerifySoftwareTokenResponse'
  { -- | The status of the verify software token.
    status :: Lude.Maybe VerifySoftwareTokenResponseType,
    -- | The session which should be passed both ways in challenge-response calls to the service.
    session :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VerifySoftwareTokenResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the verify software token.
-- * 'session' - The session which should be passed both ways in challenge-response calls to the service.
-- * 'responseStatus' - The response status code.
mkVerifySoftwareTokenResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  VerifySoftwareTokenResponse
mkVerifySoftwareTokenResponse pResponseStatus_ =
  VerifySoftwareTokenResponse'
    { status = Lude.Nothing,
      session = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the verify software token.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vstrsStatus :: Lens.Lens' VerifySoftwareTokenResponse (Lude.Maybe VerifySoftwareTokenResponseType)
vstrsStatus = Lens.lens (status :: VerifySoftwareTokenResponse -> Lude.Maybe VerifySoftwareTokenResponseType) (\s a -> s {status = a} :: VerifySoftwareTokenResponse)
{-# DEPRECATED vstrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The session which should be passed both ways in challenge-response calls to the service.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vstrsSession :: Lens.Lens' VerifySoftwareTokenResponse (Lude.Maybe Lude.Text)
vstrsSession = Lens.lens (session :: VerifySoftwareTokenResponse -> Lude.Maybe Lude.Text) (\s a -> s {session = a} :: VerifySoftwareTokenResponse)
{-# DEPRECATED vstrsSession "Use generic-lens or generic-optics with 'session' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vstrsResponseStatus :: Lens.Lens' VerifySoftwareTokenResponse Lude.Int
vstrsResponseStatus = Lens.lens (responseStatus :: VerifySoftwareTokenResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: VerifySoftwareTokenResponse)
{-# DEPRECATED vstrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
