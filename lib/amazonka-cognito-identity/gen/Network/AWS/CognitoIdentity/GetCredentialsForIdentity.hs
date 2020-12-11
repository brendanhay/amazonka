{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetCredentialsForIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns credentials for the provided identity ID. Any provided logins will be validated against supported login providers. If the token is for cognito-identity.amazonaws.com, it will be passed through to AWS Security Token Service with the appropriate role for the token.
--
-- This is a public API. You do not need any credentials to call this API.
module Network.AWS.CognitoIdentity.GetCredentialsForIdentity
  ( -- * Creating a request
    GetCredentialsForIdentity (..),
    mkGetCredentialsForIdentity,

    -- ** Request lenses
    gcfiCustomRoleARN,
    gcfiLogins,
    gcfiIdentityId,

    -- * Destructuring the response
    GetCredentialsForIdentityResponse (..),
    mkGetCredentialsForIdentityResponse,

    -- ** Response lenses
    gcfirsCredentials,
    gcfirsIdentityId,
    gcfirsResponseStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the @GetCredentialsForIdentity@ action.
--
-- /See:/ 'mkGetCredentialsForIdentity' smart constructor.
data GetCredentialsForIdentity = GetCredentialsForIdentity'
  { customRoleARN ::
      Lude.Maybe Lude.Text,
    logins ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    identityId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCredentialsForIdentity' with the minimum fields required to make a request.
--
-- * 'customRoleARN' - The Amazon Resource Name (ARN) of the role to be assumed when multiple roles were received in the token from the identity provider. For example, a SAML-based identity provider. This parameter is optional for identity providers that do not support role customization.
-- * 'identityId' - A unique identifier in the format REGION:GUID.
-- * 'logins' - A set of optional name-value pairs that map provider names to provider tokens. The name-value pair will follow the syntax "provider_name": "provider_user_identifier".
--
-- Logins should not be specified when trying to get credentials for an unauthenticated identity.
-- The Logins parameter is required when using identities associated with external identity providers such as FaceBook. For examples of @Logins@ maps, see the code examples in the <http://docs.aws.amazon.com/cognito/latest/developerguide/external-identity-providers.html External Identity Providers> section of the Amazon Cognito Developer Guide.
mkGetCredentialsForIdentity ::
  -- | 'identityId'
  Lude.Text ->
  GetCredentialsForIdentity
mkGetCredentialsForIdentity pIdentityId_ =
  GetCredentialsForIdentity'
    { customRoleARN = Lude.Nothing,
      logins = Lude.Nothing,
      identityId = pIdentityId_
    }

-- | The Amazon Resource Name (ARN) of the role to be assumed when multiple roles were received in the token from the identity provider. For example, a SAML-based identity provider. This parameter is optional for identity providers that do not support role customization.
--
-- /Note:/ Consider using 'customRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfiCustomRoleARN :: Lens.Lens' GetCredentialsForIdentity (Lude.Maybe Lude.Text)
gcfiCustomRoleARN = Lens.lens (customRoleARN :: GetCredentialsForIdentity -> Lude.Maybe Lude.Text) (\s a -> s {customRoleARN = a} :: GetCredentialsForIdentity)
{-# DEPRECATED gcfiCustomRoleARN "Use generic-lens or generic-optics with 'customRoleARN' instead." #-}

-- | A set of optional name-value pairs that map provider names to provider tokens. The name-value pair will follow the syntax "provider_name": "provider_user_identifier".
--
-- Logins should not be specified when trying to get credentials for an unauthenticated identity.
-- The Logins parameter is required when using identities associated with external identity providers such as FaceBook. For examples of @Logins@ maps, see the code examples in the <http://docs.aws.amazon.com/cognito/latest/developerguide/external-identity-providers.html External Identity Providers> section of the Amazon Cognito Developer Guide.
--
-- /Note:/ Consider using 'logins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfiLogins :: Lens.Lens' GetCredentialsForIdentity (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gcfiLogins = Lens.lens (logins :: GetCredentialsForIdentity -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {logins = a} :: GetCredentialsForIdentity)
{-# DEPRECATED gcfiLogins "Use generic-lens or generic-optics with 'logins' instead." #-}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfiIdentityId :: Lens.Lens' GetCredentialsForIdentity Lude.Text
gcfiIdentityId = Lens.lens (identityId :: GetCredentialsForIdentity -> Lude.Text) (\s a -> s {identityId = a} :: GetCredentialsForIdentity)
{-# DEPRECATED gcfiIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

instance Lude.AWSRequest GetCredentialsForIdentity where
  type
    Rs GetCredentialsForIdentity =
      GetCredentialsForIdentityResponse
  request = Req.postJSON cognitoIdentityService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCredentialsForIdentityResponse'
            Lude.<$> (x Lude..?> "Credentials")
            Lude.<*> (x Lude..?> "IdentityId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCredentialsForIdentity where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityService.GetCredentialsForIdentity" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCredentialsForIdentity where
  toJSON GetCredentialsForIdentity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CustomRoleArn" Lude..=) Lude.<$> customRoleARN,
            ("Logins" Lude..=) Lude.<$> logins,
            Lude.Just ("IdentityId" Lude..= identityId)
          ]
      )

instance Lude.ToPath GetCredentialsForIdentity where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCredentialsForIdentity where
  toQuery = Lude.const Lude.mempty

-- | Returned in response to a successful @GetCredentialsForIdentity@ operation.
--
-- /See:/ 'mkGetCredentialsForIdentityResponse' smart constructor.
data GetCredentialsForIdentityResponse = GetCredentialsForIdentityResponse'
  { credentials ::
      Lude.Maybe Credentials,
    identityId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetCredentialsForIdentityResponse' with the minimum fields required to make a request.
--
-- * 'credentials' - Credentials for the provided identity ID.
-- * 'identityId' - A unique identifier in the format REGION:GUID.
-- * 'responseStatus' - The response status code.
mkGetCredentialsForIdentityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCredentialsForIdentityResponse
mkGetCredentialsForIdentityResponse pResponseStatus_ =
  GetCredentialsForIdentityResponse'
    { credentials = Lude.Nothing,
      identityId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Credentials for the provided identity ID.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfirsCredentials :: Lens.Lens' GetCredentialsForIdentityResponse (Lude.Maybe Credentials)
gcfirsCredentials = Lens.lens (credentials :: GetCredentialsForIdentityResponse -> Lude.Maybe Credentials) (\s a -> s {credentials = a} :: GetCredentialsForIdentityResponse)
{-# DEPRECATED gcfirsCredentials "Use generic-lens or generic-optics with 'credentials' instead." #-}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfirsIdentityId :: Lens.Lens' GetCredentialsForIdentityResponse (Lude.Maybe Lude.Text)
gcfirsIdentityId = Lens.lens (identityId :: GetCredentialsForIdentityResponse -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: GetCredentialsForIdentityResponse)
{-# DEPRECATED gcfirsIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfirsResponseStatus :: Lens.Lens' GetCredentialsForIdentityResponse Lude.Int
gcfirsResponseStatus = Lens.lens (responseStatus :: GetCredentialsForIdentityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCredentialsForIdentityResponse)
{-# DEPRECATED gcfirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
