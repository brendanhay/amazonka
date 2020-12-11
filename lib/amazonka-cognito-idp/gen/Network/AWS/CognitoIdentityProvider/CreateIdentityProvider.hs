{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.CreateIdentityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an identity provider for a user pool.
module Network.AWS.CognitoIdentityProvider.CreateIdentityProvider
  ( -- * Creating a request
    CreateIdentityProvider (..),
    mkCreateIdentityProvider,

    -- ** Request lenses
    cipIdpIdentifiers,
    cipAttributeMapping,
    cipUserPoolId,
    cipProviderName,
    cipProviderType,
    cipProviderDetails,

    -- * Destructuring the response
    CreateIdentityProviderResponse (..),
    mkCreateIdentityProviderResponse,

    -- ** Response lenses
    ciprsResponseStatus,
    ciprsIdentityProvider,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateIdentityProvider' smart constructor.
data CreateIdentityProvider = CreateIdentityProvider'
  { idpIdentifiers ::
      Lude.Maybe [Lude.Text],
    attributeMapping ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    userPoolId :: Lude.Text,
    providerName :: Lude.Text,
    providerType :: IdentityProviderTypeType,
    providerDetails ::
      Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateIdentityProvider' with the minimum fields required to make a request.
--
-- * 'attributeMapping' - A mapping of identity provider attributes to standard and custom user pool attributes.
-- * 'idpIdentifiers' - A list of identity provider identifiers.
-- * 'providerDetails' - The identity provider details. The following list describes the provider detail keys for each identity provider type.
--
--
--     * For Google and Login with Amazon:
--
--     * client_id
--
--
--     * client_secret
--
--
--     * authorize_scopes
--
--
--
--
--     * For Facebook:
--
--     * client_id
--
--
--     * client_secret
--
--
--     * authorize_scopes
--
--
--     * api_version
--
--
--
--
--     * For Sign in with Apple:
--
--     * client_id
--
--
--     * team_id
--
--
--     * key_id
--
--
--     * private_key
--
--
--     * authorize_scopes
--
--
--
--
--     * For OIDC providers:
--
--     * client_id
--
--
--     * client_secret
--
--
--     * attributes_request_method
--
--
--     * oidc_issuer
--
--
--     * authorize_scopes
--
--
--     * authorize_url /if not available from discovery URL specified by oidc_issuer key/
--
--
--     * token_url /if not available from discovery URL specified by oidc_issuer key/
--
--
--     * attributes_url /if not available from discovery URL specified by oidc_issuer key/
--
--
--     * jwks_uri /if not available from discovery URL specified by oidc_issuer key/
--
--
--
--
--     * For SAML providers:
--
--     * MetadataFile OR MetadataURL
--
--
--     * IDPSignout /optional/
--
--
--
--
-- * 'providerName' - The identity provider name.
-- * 'providerType' - The identity provider type.
-- * 'userPoolId' - The user pool ID.
mkCreateIdentityProvider ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'providerName'
  Lude.Text ->
  -- | 'providerType'
  IdentityProviderTypeType ->
  CreateIdentityProvider
mkCreateIdentityProvider pUserPoolId_ pProviderName_ pProviderType_ =
  CreateIdentityProvider'
    { idpIdentifiers = Lude.Nothing,
      attributeMapping = Lude.Nothing,
      userPoolId = pUserPoolId_,
      providerName = pProviderName_,
      providerType = pProviderType_,
      providerDetails = Lude.mempty
    }

-- | A list of identity provider identifiers.
--
-- /Note:/ Consider using 'idpIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipIdpIdentifiers :: Lens.Lens' CreateIdentityProvider (Lude.Maybe [Lude.Text])
cipIdpIdentifiers = Lens.lens (idpIdentifiers :: CreateIdentityProvider -> Lude.Maybe [Lude.Text]) (\s a -> s {idpIdentifiers = a} :: CreateIdentityProvider)
{-# DEPRECATED cipIdpIdentifiers "Use generic-lens or generic-optics with 'idpIdentifiers' instead." #-}

-- | A mapping of identity provider attributes to standard and custom user pool attributes.
--
-- /Note:/ Consider using 'attributeMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipAttributeMapping :: Lens.Lens' CreateIdentityProvider (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cipAttributeMapping = Lens.lens (attributeMapping :: CreateIdentityProvider -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributeMapping = a} :: CreateIdentityProvider)
{-# DEPRECATED cipAttributeMapping "Use generic-lens or generic-optics with 'attributeMapping' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipUserPoolId :: Lens.Lens' CreateIdentityProvider Lude.Text
cipUserPoolId = Lens.lens (userPoolId :: CreateIdentityProvider -> Lude.Text) (\s a -> s {userPoolId = a} :: CreateIdentityProvider)
{-# DEPRECATED cipUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identity provider name.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipProviderName :: Lens.Lens' CreateIdentityProvider Lude.Text
cipProviderName = Lens.lens (providerName :: CreateIdentityProvider -> Lude.Text) (\s a -> s {providerName = a} :: CreateIdentityProvider)
{-# DEPRECATED cipProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | The identity provider type.
--
-- /Note:/ Consider using 'providerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipProviderType :: Lens.Lens' CreateIdentityProvider IdentityProviderTypeType
cipProviderType = Lens.lens (providerType :: CreateIdentityProvider -> IdentityProviderTypeType) (\s a -> s {providerType = a} :: CreateIdentityProvider)
{-# DEPRECATED cipProviderType "Use generic-lens or generic-optics with 'providerType' instead." #-}

-- | The identity provider details. The following list describes the provider detail keys for each identity provider type.
--
--
--     * For Google and Login with Amazon:
--
--     * client_id
--
--
--     * client_secret
--
--
--     * authorize_scopes
--
--
--
--
--     * For Facebook:
--
--     * client_id
--
--
--     * client_secret
--
--
--     * authorize_scopes
--
--
--     * api_version
--
--
--
--
--     * For Sign in with Apple:
--
--     * client_id
--
--
--     * team_id
--
--
--     * key_id
--
--
--     * private_key
--
--
--     * authorize_scopes
--
--
--
--
--     * For OIDC providers:
--
--     * client_id
--
--
--     * client_secret
--
--
--     * attributes_request_method
--
--
--     * oidc_issuer
--
--
--     * authorize_scopes
--
--
--     * authorize_url /if not available from discovery URL specified by oidc_issuer key/
--
--
--     * token_url /if not available from discovery URL specified by oidc_issuer key/
--
--
--     * attributes_url /if not available from discovery URL specified by oidc_issuer key/
--
--
--     * jwks_uri /if not available from discovery URL specified by oidc_issuer key/
--
--
--
--
--     * For SAML providers:
--
--     * MetadataFile OR MetadataURL
--
--
--     * IDPSignout /optional/
--
--
--
--
--
-- /Note:/ Consider using 'providerDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipProviderDetails :: Lens.Lens' CreateIdentityProvider (Lude.HashMap Lude.Text (Lude.Text))
cipProviderDetails = Lens.lens (providerDetails :: CreateIdentityProvider -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {providerDetails = a} :: CreateIdentityProvider)
{-# DEPRECATED cipProviderDetails "Use generic-lens or generic-optics with 'providerDetails' instead." #-}

instance Lude.AWSRequest CreateIdentityProvider where
  type Rs CreateIdentityProvider = CreateIdentityProviderResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateIdentityProviderResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "IdentityProvider")
      )

instance Lude.ToHeaders CreateIdentityProvider where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.CreateIdentityProvider" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateIdentityProvider where
  toJSON CreateIdentityProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IdpIdentifiers" Lude..=) Lude.<$> idpIdentifiers,
            ("AttributeMapping" Lude..=) Lude.<$> attributeMapping,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("ProviderName" Lude..= providerName),
            Lude.Just ("ProviderType" Lude..= providerType),
            Lude.Just ("ProviderDetails" Lude..= providerDetails)
          ]
      )

instance Lude.ToPath CreateIdentityProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateIdentityProvider where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateIdentityProviderResponse' smart constructor.
data CreateIdentityProviderResponse = CreateIdentityProviderResponse'
  { responseStatus ::
      Lude.Int,
    identityProvider ::
      IdentityProviderType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateIdentityProviderResponse' with the minimum fields required to make a request.
--
-- * 'identityProvider' - The newly created identity provider object.
-- * 'responseStatus' - The response status code.
mkCreateIdentityProviderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'identityProvider'
  IdentityProviderType ->
  CreateIdentityProviderResponse
mkCreateIdentityProviderResponse
  pResponseStatus_
  pIdentityProvider_ =
    CreateIdentityProviderResponse'
      { responseStatus =
          pResponseStatus_,
        identityProvider = pIdentityProvider_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciprsResponseStatus :: Lens.Lens' CreateIdentityProviderResponse Lude.Int
ciprsResponseStatus = Lens.lens (responseStatus :: CreateIdentityProviderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateIdentityProviderResponse)
{-# DEPRECATED ciprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The newly created identity provider object.
--
-- /Note:/ Consider using 'identityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciprsIdentityProvider :: Lens.Lens' CreateIdentityProviderResponse IdentityProviderType
ciprsIdentityProvider = Lens.lens (identityProvider :: CreateIdentityProviderResponse -> IdentityProviderType) (\s a -> s {identityProvider = a} :: CreateIdentityProviderResponse)
{-# DEPRECATED ciprsIdentityProvider "Use generic-lens or generic-optics with 'identityProvider' instead." #-}
