{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetOpenIdToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an OpenID token, using a known Cognito ID. This known Cognito ID is returned by 'GetId' . You can optionally add additional logins for the identity. Supplying multiple logins creates an implicit link.
--
-- The OpenId token is valid for 10 minutes.
-- This is a public API. You do not need any credentials to call this API.
module Network.AWS.CognitoIdentity.GetOpenIdToken
  ( -- * Creating a request
    GetOpenIdToken (..),
    mkGetOpenIdToken,

    -- ** Request lenses
    goitLogins,
    goitIdentityId,

    -- * Destructuring the response
    GetOpenIdTokenResponse (..),
    mkGetOpenIdTokenResponse,

    -- ** Response lenses
    goitrsToken,
    goitrsIdentityId,
    goitrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the GetOpenIdToken action.
--
-- /See:/ 'mkGetOpenIdToken' smart constructor.
data GetOpenIdToken = GetOpenIdToken'
  { -- | A set of optional name-value pairs that map provider names to provider tokens. When using graph.facebook.com and www.amazon.com, supply the access_token returned from the provider's authflow. For accounts.google.com, an Amazon Cognito user pool provider, or any other OpenId Connect provider, always include the @id_token@ .
    logins :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | A unique identifier in the format REGION:GUID.
    identityId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOpenIdToken' with the minimum fields required to make a request.
--
-- * 'logins' - A set of optional name-value pairs that map provider names to provider tokens. When using graph.facebook.com and www.amazon.com, supply the access_token returned from the provider's authflow. For accounts.google.com, an Amazon Cognito user pool provider, or any other OpenId Connect provider, always include the @id_token@ .
-- * 'identityId' - A unique identifier in the format REGION:GUID.
mkGetOpenIdToken ::
  -- | 'identityId'
  Lude.Text ->
  GetOpenIdToken
mkGetOpenIdToken pIdentityId_ =
  GetOpenIdToken' {logins = Lude.Nothing, identityId = pIdentityId_}

-- | A set of optional name-value pairs that map provider names to provider tokens. When using graph.facebook.com and www.amazon.com, supply the access_token returned from the provider's authflow. For accounts.google.com, an Amazon Cognito user pool provider, or any other OpenId Connect provider, always include the @id_token@ .
--
-- /Note:/ Consider using 'logins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitLogins :: Lens.Lens' GetOpenIdToken (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
goitLogins = Lens.lens (logins :: GetOpenIdToken -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {logins = a} :: GetOpenIdToken)
{-# DEPRECATED goitLogins "Use generic-lens or generic-optics with 'logins' instead." #-}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitIdentityId :: Lens.Lens' GetOpenIdToken Lude.Text
goitIdentityId = Lens.lens (identityId :: GetOpenIdToken -> Lude.Text) (\s a -> s {identityId = a} :: GetOpenIdToken)
{-# DEPRECATED goitIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

instance Lude.AWSRequest GetOpenIdToken where
  type Rs GetOpenIdToken = GetOpenIdTokenResponse
  request = Req.postJSON cognitoIdentityService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetOpenIdTokenResponse'
            Lude.<$> (x Lude..?> "Token")
            Lude.<*> (x Lude..?> "IdentityId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOpenIdToken where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSCognitoIdentityService.GetOpenIdToken" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetOpenIdToken where
  toJSON GetOpenIdToken' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Logins" Lude..=) Lude.<$> logins,
            Lude.Just ("IdentityId" Lude..= identityId)
          ]
      )

instance Lude.ToPath GetOpenIdToken where
  toPath = Lude.const "/"

instance Lude.ToQuery GetOpenIdToken where
  toQuery = Lude.const Lude.mempty

-- | Returned in response to a successful GetOpenIdToken request.
--
-- /See:/ 'mkGetOpenIdTokenResponse' smart constructor.
data GetOpenIdTokenResponse = GetOpenIdTokenResponse'
  { -- | An OpenID token, valid for 10 minutes.
    token :: Lude.Maybe Lude.Text,
    -- | A unique identifier in the format REGION:GUID. Note that the IdentityId returned may not match the one passed on input.
    identityId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOpenIdTokenResponse' with the minimum fields required to make a request.
--
-- * 'token' - An OpenID token, valid for 10 minutes.
-- * 'identityId' - A unique identifier in the format REGION:GUID. Note that the IdentityId returned may not match the one passed on input.
-- * 'responseStatus' - The response status code.
mkGetOpenIdTokenResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOpenIdTokenResponse
mkGetOpenIdTokenResponse pResponseStatus_ =
  GetOpenIdTokenResponse'
    { token = Lude.Nothing,
      identityId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An OpenID token, valid for 10 minutes.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitrsToken :: Lens.Lens' GetOpenIdTokenResponse (Lude.Maybe Lude.Text)
goitrsToken = Lens.lens (token :: GetOpenIdTokenResponse -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: GetOpenIdTokenResponse)
{-# DEPRECATED goitrsToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | A unique identifier in the format REGION:GUID. Note that the IdentityId returned may not match the one passed on input.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitrsIdentityId :: Lens.Lens' GetOpenIdTokenResponse (Lude.Maybe Lude.Text)
goitrsIdentityId = Lens.lens (identityId :: GetOpenIdTokenResponse -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: GetOpenIdTokenResponse)
{-# DEPRECATED goitrsIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitrsResponseStatus :: Lens.Lens' GetOpenIdTokenResponse Lude.Int
goitrsResponseStatus = Lens.lens (responseStatus :: GetOpenIdTokenResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOpenIdTokenResponse)
{-# DEPRECATED goitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
