{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cipUserPoolId,
    cipProviderName,
    cipProviderType,
    cipProviderDetails,
    cipAttributeMapping,
    cipIdpIdentifiers,

    -- * Destructuring the response
    CreateIdentityProviderResponse (..),
    mkCreateIdentityProviderResponse,

    -- ** Response lenses
    ciprrsIdentityProvider,
    ciprrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateIdentityProvider' smart constructor.
data CreateIdentityProvider = CreateIdentityProvider'
  { -- | The user pool ID.
    userPoolId :: Types.UserPoolId,
    -- | The identity provider name.
    providerName :: Types.ProviderName,
    -- | The identity provider type.
    providerType :: Types.IdentityProviderTypeType,
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
    providerDetails :: Core.HashMap Types.StringType Types.StringType,
    -- | A mapping of identity provider attributes to standard and custom user pool attributes.
    attributeMapping :: Core.Maybe (Core.HashMap Types.AttributeMappingKeyType Types.StringType),
    -- | A list of identity provider identifiers.
    idpIdentifiers :: Core.Maybe [Types.IdpIdentifierType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateIdentityProvider' value with any optional fields omitted.
mkCreateIdentityProvider ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'providerName'
  Types.ProviderName ->
  -- | 'providerType'
  Types.IdentityProviderTypeType ->
  CreateIdentityProvider
mkCreateIdentityProvider userPoolId providerName providerType =
  CreateIdentityProvider'
    { userPoolId,
      providerName,
      providerType,
      providerDetails = Core.mempty,
      attributeMapping = Core.Nothing,
      idpIdentifiers = Core.Nothing
    }

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipUserPoolId :: Lens.Lens' CreateIdentityProvider Types.UserPoolId
cipUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED cipUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identity provider name.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipProviderName :: Lens.Lens' CreateIdentityProvider Types.ProviderName
cipProviderName = Lens.field @"providerName"
{-# DEPRECATED cipProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | The identity provider type.
--
-- /Note:/ Consider using 'providerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipProviderType :: Lens.Lens' CreateIdentityProvider Types.IdentityProviderTypeType
cipProviderType = Lens.field @"providerType"
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
cipProviderDetails :: Lens.Lens' CreateIdentityProvider (Core.HashMap Types.StringType Types.StringType)
cipProviderDetails = Lens.field @"providerDetails"
{-# DEPRECATED cipProviderDetails "Use generic-lens or generic-optics with 'providerDetails' instead." #-}

-- | A mapping of identity provider attributes to standard and custom user pool attributes.
--
-- /Note:/ Consider using 'attributeMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipAttributeMapping :: Lens.Lens' CreateIdentityProvider (Core.Maybe (Core.HashMap Types.AttributeMappingKeyType Types.StringType))
cipAttributeMapping = Lens.field @"attributeMapping"
{-# DEPRECATED cipAttributeMapping "Use generic-lens or generic-optics with 'attributeMapping' instead." #-}

-- | A list of identity provider identifiers.
--
-- /Note:/ Consider using 'idpIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipIdpIdentifiers :: Lens.Lens' CreateIdentityProvider (Core.Maybe [Types.IdpIdentifierType])
cipIdpIdentifiers = Lens.field @"idpIdentifiers"
{-# DEPRECATED cipIdpIdentifiers "Use generic-lens or generic-optics with 'idpIdentifiers' instead." #-}

instance Core.FromJSON CreateIdentityProvider where
  toJSON CreateIdentityProvider {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("ProviderName" Core..= providerName),
            Core.Just ("ProviderType" Core..= providerType),
            Core.Just ("ProviderDetails" Core..= providerDetails),
            ("AttributeMapping" Core..=) Core.<$> attributeMapping,
            ("IdpIdentifiers" Core..=) Core.<$> idpIdentifiers
          ]
      )

instance Core.AWSRequest CreateIdentityProvider where
  type Rs CreateIdentityProvider = CreateIdentityProviderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.CreateIdentityProvider"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIdentityProviderResponse'
            Core.<$> (x Core..: "IdentityProvider")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateIdentityProviderResponse' smart constructor.
data CreateIdentityProviderResponse = CreateIdentityProviderResponse'
  { -- | The newly created identity provider object.
    identityProvider :: Types.IdentityProviderType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateIdentityProviderResponse' value with any optional fields omitted.
mkCreateIdentityProviderResponse ::
  -- | 'identityProvider'
  Types.IdentityProviderType ->
  -- | 'responseStatus'
  Core.Int ->
  CreateIdentityProviderResponse
mkCreateIdentityProviderResponse identityProvider responseStatus =
  CreateIdentityProviderResponse' {identityProvider, responseStatus}

-- | The newly created identity provider object.
--
-- /Note:/ Consider using 'identityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciprrsIdentityProvider :: Lens.Lens' CreateIdentityProviderResponse Types.IdentityProviderType
ciprrsIdentityProvider = Lens.field @"identityProvider"
{-# DEPRECATED ciprrsIdentityProvider "Use generic-lens or generic-optics with 'identityProvider' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciprrsResponseStatus :: Lens.Lens' CreateIdentityProviderResponse Core.Int
ciprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ciprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
