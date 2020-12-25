{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.IdentityProviderType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.IdentityProviderType
  ( IdentityProviderType (..),

    -- * Smart constructor
    mkIdentityProviderType,

    -- * Lenses
    iptAttributeMapping,
    iptCreationDate,
    iptIdpIdentifiers,
    iptLastModifiedDate,
    iptProviderDetails,
    iptProviderName,
    iptProviderType,
    iptUserPoolId,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.AttributeMappingKeyType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.IdpIdentifierType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ProviderName as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.StringType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserPoolId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A container for information about an identity provider.
--
-- /See:/ 'mkIdentityProviderType' smart constructor.
data IdentityProviderType = IdentityProviderType'
  { -- | A mapping of identity provider attributes to standard and custom user pool attributes.
    attributeMapping :: Core.Maybe (Core.HashMap Types.AttributeMappingKeyType Types.StringType),
    -- | The date the identity provider was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | A list of identity provider identifiers.
    idpIdentifiers :: Core.Maybe [Types.IdpIdentifierType],
    -- | The date the identity provider was last modified.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
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
    --     * authorize_scopes
    --
    --
    --
    --
    --     * For SAML providers:
    --
    --     * MetadataFile OR MetadataURL
    --
    --
    --     * IDPSignOut /optional/
    providerDetails :: Core.Maybe (Core.HashMap Types.StringType Types.StringType),
    -- | The identity provider name.
    providerName :: Core.Maybe Types.ProviderName,
    -- | The identity provider type.
    providerType :: Core.Maybe Types.IdentityProviderTypeType,
    -- | The user pool ID.
    userPoolId :: Core.Maybe Types.UserPoolId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'IdentityProviderType' value with any optional fields omitted.
mkIdentityProviderType ::
  IdentityProviderType
mkIdentityProviderType =
  IdentityProviderType'
    { attributeMapping = Core.Nothing,
      creationDate = Core.Nothing,
      idpIdentifiers = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      providerDetails = Core.Nothing,
      providerName = Core.Nothing,
      providerType = Core.Nothing,
      userPoolId = Core.Nothing
    }

-- | A mapping of identity provider attributes to standard and custom user pool attributes.
--
-- /Note:/ Consider using 'attributeMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iptAttributeMapping :: Lens.Lens' IdentityProviderType (Core.Maybe (Core.HashMap Types.AttributeMappingKeyType Types.StringType))
iptAttributeMapping = Lens.field @"attributeMapping"
{-# DEPRECATED iptAttributeMapping "Use generic-lens or generic-optics with 'attributeMapping' instead." #-}

-- | The date the identity provider was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iptCreationDate :: Lens.Lens' IdentityProviderType (Core.Maybe Core.NominalDiffTime)
iptCreationDate = Lens.field @"creationDate"
{-# DEPRECATED iptCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | A list of identity provider identifiers.
--
-- /Note:/ Consider using 'idpIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iptIdpIdentifiers :: Lens.Lens' IdentityProviderType (Core.Maybe [Types.IdpIdentifierType])
iptIdpIdentifiers = Lens.field @"idpIdentifiers"
{-# DEPRECATED iptIdpIdentifiers "Use generic-lens or generic-optics with 'idpIdentifiers' instead." #-}

-- | The date the identity provider was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iptLastModifiedDate :: Lens.Lens' IdentityProviderType (Core.Maybe Core.NominalDiffTime)
iptLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED iptLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

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
--     * authorize_scopes
--
--
--
--
--     * For SAML providers:
--
--     * MetadataFile OR MetadataURL
--
--
--     * IDPSignOut /optional/
--
--
--
--
--
-- /Note:/ Consider using 'providerDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iptProviderDetails :: Lens.Lens' IdentityProviderType (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
iptProviderDetails = Lens.field @"providerDetails"
{-# DEPRECATED iptProviderDetails "Use generic-lens or generic-optics with 'providerDetails' instead." #-}

-- | The identity provider name.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iptProviderName :: Lens.Lens' IdentityProviderType (Core.Maybe Types.ProviderName)
iptProviderName = Lens.field @"providerName"
{-# DEPRECATED iptProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | The identity provider type.
--
-- /Note:/ Consider using 'providerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iptProviderType :: Lens.Lens' IdentityProviderType (Core.Maybe Types.IdentityProviderTypeType)
iptProviderType = Lens.field @"providerType"
{-# DEPRECATED iptProviderType "Use generic-lens or generic-optics with 'providerType' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iptUserPoolId :: Lens.Lens' IdentityProviderType (Core.Maybe Types.UserPoolId)
iptUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED iptUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Core.FromJSON IdentityProviderType where
  parseJSON =
    Core.withObject "IdentityProviderType" Core.$
      \x ->
        IdentityProviderType'
          Core.<$> (x Core..:? "AttributeMapping")
          Core.<*> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "IdpIdentifiers")
          Core.<*> (x Core..:? "LastModifiedDate")
          Core.<*> (x Core..:? "ProviderDetails")
          Core.<*> (x Core..:? "ProviderName")
          Core.<*> (x Core..:? "ProviderType")
          Core.<*> (x Core..:? "UserPoolId")
