{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers (or retrieves) a Cognito @IdentityId@ and an OpenID Connect token for a user authenticated by your backend authentication process. Supplying multiple logins will create an implicit linked account. You can only specify one developer provider as part of the @Logins@ map, which is linked to the identity pool. The developer provider is the "domain" by which Cognito will refer to your users.
--
-- You can use @GetOpenIdTokenForDeveloperIdentity@ to create a new identity and to link new logins (that is, user credentials issued by a public provider or developer provider) to an existing identity. When you want to create a new identity, the @IdentityId@ should be null. When you want to associate a new login with an existing authenticated/unauthenticated identity, you can do so by providing the existing @IdentityId@ . This API will create the identity in the specified @IdentityPoolId@ .
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
  ( -- * Creating a request
    GetOpenIdTokenForDeveloperIdentity (..),
    mkGetOpenIdTokenForDeveloperIdentity,

    -- ** Request lenses
    goitfdiTokenDuration,
    goitfdiIdentityId,
    goitfdiIdentityPoolId,
    goitfdiLogins,

    -- * Destructuring the response
    GetOpenIdTokenForDeveloperIdentityResponse (..),
    mkGetOpenIdTokenForDeveloperIdentityResponse,

    -- ** Response lenses
    goitfdirsToken,
    goitfdirsIdentityId,
    goitfdirsResponseStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the @GetOpenIdTokenForDeveloperIdentity@ action.
--
-- /See:/ 'mkGetOpenIdTokenForDeveloperIdentity' smart constructor.
data GetOpenIdTokenForDeveloperIdentity = GetOpenIdTokenForDeveloperIdentity'
  { tokenDuration ::
      Lude.Maybe
        Lude.Natural,
    identityId ::
      Lude.Maybe Lude.Text,
    identityPoolId ::
      Lude.Text,
    logins ::
      Lude.HashMap
        Lude.Text
        (Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOpenIdTokenForDeveloperIdentity' with the minimum fields required to make a request.
--
-- * 'identityId' - A unique identifier in the format REGION:GUID.
-- * 'identityPoolId' - An identity pool ID in the format REGION:GUID.
-- * 'logins' - A set of optional name-value pairs that map provider names to provider tokens. Each name-value pair represents a user from a public provider or developer provider. If the user is from a developer provider, the name-value pair will follow the syntax @"developer_provider_name": "developer_user_identifier"@ . The developer provider is the "domain" by which Cognito will refer to your users; you provided this domain while creating/updating the identity pool. The developer user identifier is an identifier from your backend that uniquely identifies a user. When you create an identity pool, you can specify the supported logins.
-- * 'tokenDuration' - The expiration time of the token, in seconds. You can specify a custom expiration time for the token so that you can cache it. If you don't provide an expiration time, the token is valid for 15 minutes. You can exchange the token with Amazon STS for temporary AWS credentials, which are valid for a maximum of one hour. The maximum token duration you can set is 24 hours. You should take care in setting the expiration time for a token, as there are significant security implications: an attacker could use a leaked token to access your AWS resources for the token's duration.
mkGetOpenIdTokenForDeveloperIdentity ::
  -- | 'identityPoolId'
  Lude.Text ->
  GetOpenIdTokenForDeveloperIdentity
mkGetOpenIdTokenForDeveloperIdentity pIdentityPoolId_ =
  GetOpenIdTokenForDeveloperIdentity'
    { tokenDuration = Lude.Nothing,
      identityId = Lude.Nothing,
      identityPoolId = pIdentityPoolId_,
      logins = Lude.mempty
    }

-- | The expiration time of the token, in seconds. You can specify a custom expiration time for the token so that you can cache it. If you don't provide an expiration time, the token is valid for 15 minutes. You can exchange the token with Amazon STS for temporary AWS credentials, which are valid for a maximum of one hour. The maximum token duration you can set is 24 hours. You should take care in setting the expiration time for a token, as there are significant security implications: an attacker could use a leaked token to access your AWS resources for the token's duration.
--
-- /Note:/ Consider using 'tokenDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdiTokenDuration :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Lude.Maybe Lude.Natural)
goitfdiTokenDuration = Lens.lens (tokenDuration :: GetOpenIdTokenForDeveloperIdentity -> Lude.Maybe Lude.Natural) (\s a -> s {tokenDuration = a} :: GetOpenIdTokenForDeveloperIdentity)
{-# DEPRECATED goitfdiTokenDuration "Use generic-lens or generic-optics with 'tokenDuration' instead." #-}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdiIdentityId :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Lude.Maybe Lude.Text)
goitfdiIdentityId = Lens.lens (identityId :: GetOpenIdTokenForDeveloperIdentity -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: GetOpenIdTokenForDeveloperIdentity)
{-# DEPRECATED goitfdiIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdiIdentityPoolId :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity Lude.Text
goitfdiIdentityPoolId = Lens.lens (identityPoolId :: GetOpenIdTokenForDeveloperIdentity -> Lude.Text) (\s a -> s {identityPoolId = a} :: GetOpenIdTokenForDeveloperIdentity)
{-# DEPRECATED goitfdiIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A set of optional name-value pairs that map provider names to provider tokens. Each name-value pair represents a user from a public provider or developer provider. If the user is from a developer provider, the name-value pair will follow the syntax @"developer_provider_name": "developer_user_identifier"@ . The developer provider is the "domain" by which Cognito will refer to your users; you provided this domain while creating/updating the identity pool. The developer user identifier is an identifier from your backend that uniquely identifies a user. When you create an identity pool, you can specify the supported logins.
--
-- /Note:/ Consider using 'logins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdiLogins :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Lude.HashMap Lude.Text (Lude.Text))
goitfdiLogins = Lens.lens (logins :: GetOpenIdTokenForDeveloperIdentity -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {logins = a} :: GetOpenIdTokenForDeveloperIdentity)
{-# DEPRECATED goitfdiLogins "Use generic-lens or generic-optics with 'logins' instead." #-}

instance Lude.AWSRequest GetOpenIdTokenForDeveloperIdentity where
  type
    Rs GetOpenIdTokenForDeveloperIdentity =
      GetOpenIdTokenForDeveloperIdentityResponse
  request = Req.postJSON cognitoIdentityService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetOpenIdTokenForDeveloperIdentityResponse'
            Lude.<$> (x Lude..?> "Token")
            Lude.<*> (x Lude..?> "IdentityId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOpenIdTokenForDeveloperIdentity where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityService.GetOpenIdTokenForDeveloperIdentity" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetOpenIdTokenForDeveloperIdentity where
  toJSON GetOpenIdTokenForDeveloperIdentity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TokenDuration" Lude..=) Lude.<$> tokenDuration,
            ("IdentityId" Lude..=) Lude.<$> identityId,
            Lude.Just ("IdentityPoolId" Lude..= identityPoolId),
            Lude.Just ("Logins" Lude..= logins)
          ]
      )

instance Lude.ToPath GetOpenIdTokenForDeveloperIdentity where
  toPath = Lude.const "/"

instance Lude.ToQuery GetOpenIdTokenForDeveloperIdentity where
  toQuery = Lude.const Lude.mempty

-- | Returned in response to a successful @GetOpenIdTokenForDeveloperIdentity@ request.
--
-- /See:/ 'mkGetOpenIdTokenForDeveloperIdentityResponse' smart constructor.
data GetOpenIdTokenForDeveloperIdentityResponse = GetOpenIdTokenForDeveloperIdentityResponse'
  { token ::
      Lude.Maybe
        Lude.Text,
    identityId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOpenIdTokenForDeveloperIdentityResponse' with the minimum fields required to make a request.
--
-- * 'identityId' - A unique identifier in the format REGION:GUID.
-- * 'responseStatus' - The response status code.
-- * 'token' - An OpenID token.
mkGetOpenIdTokenForDeveloperIdentityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOpenIdTokenForDeveloperIdentityResponse
mkGetOpenIdTokenForDeveloperIdentityResponse pResponseStatus_ =
  GetOpenIdTokenForDeveloperIdentityResponse'
    { token = Lude.Nothing,
      identityId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An OpenID token.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdirsToken :: Lens.Lens' GetOpenIdTokenForDeveloperIdentityResponse (Lude.Maybe Lude.Text)
goitfdirsToken = Lens.lens (token :: GetOpenIdTokenForDeveloperIdentityResponse -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: GetOpenIdTokenForDeveloperIdentityResponse)
{-# DEPRECATED goitfdirsToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdirsIdentityId :: Lens.Lens' GetOpenIdTokenForDeveloperIdentityResponse (Lude.Maybe Lude.Text)
goitfdirsIdentityId = Lens.lens (identityId :: GetOpenIdTokenForDeveloperIdentityResponse -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: GetOpenIdTokenForDeveloperIdentityResponse)
{-# DEPRECATED goitfdirsIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdirsResponseStatus :: Lens.Lens' GetOpenIdTokenForDeveloperIdentityResponse Lude.Int
goitfdirsResponseStatus = Lens.lens (responseStatus :: GetOpenIdTokenForDeveloperIdentityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOpenIdTokenForDeveloperIdentityResponse)
{-# DEPRECATED goitfdirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
