{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.UpdateIdentityPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an identity pool.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.UpdateIdentityPool
  ( -- * Creating a request
    UpdateIdentityPool (..),
    mkUpdateIdentityPool,

    -- ** Request lenses
    uipIdentityPoolId,
    uipIdentityPoolName,
    uipAllowUnauthenticatedIdentities,
    uipAllowClassicFlow,
    uipCognitoIdentityProviders,
    uipDeveloperProviderName,
    uipIdentityPoolTags,
    uipOpenIdConnectProviderARNs,
    uipSamlProviderARNs,
    uipSupportedLoginProviders,

    -- * Destructuring the response
    Types.IdentityPool (..),
    Types.mkIdentityPool,

    -- ** Response lenses
    Types.ipIdentityPoolId,
    Types.ipIdentityPoolName,
    Types.ipAllowUnauthenticatedIdentities,
    Types.ipAllowClassicFlow,
    Types.ipCognitoIdentityProviders,
    Types.ipDeveloperProviderName,
    Types.ipIdentityPoolTags,
    Types.ipOpenIdConnectProviderARNs,
    Types.ipSamlProviderARNs,
    Types.ipSupportedLoginProviders,
  )
where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | An object representing an Amazon Cognito identity pool.
--
-- /See:/ 'mkUpdateIdentityPool' smart constructor.
data UpdateIdentityPool = UpdateIdentityPool'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Types.IdentityPoolId,
    -- | A string that you provide.
    identityPoolName :: Types.IdentityPoolName,
    -- | TRUE if the identity pool supports unauthenticated logins.
    allowUnauthenticatedIdentities :: Core.Bool,
    -- | Enables or disables the Basic (Classic) authentication flow. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow> in the /Amazon Cognito Developer Guide/ .
    allowClassicFlow :: Core.Maybe Core.Bool,
    -- | A list representing an Amazon Cognito user pool and its client ID.
    cognitoIdentityProviders :: Core.Maybe [Types.CognitoIdentityProvider],
    -- | The "domain" by which Cognito will refer to your users.
    developerProviderName :: Core.Maybe Types.DeveloperProviderName,
    -- | The tags that are assigned to the identity pool. A tag is a label that you can apply to identity pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
    identityPoolTags :: Core.Maybe (Core.HashMap Types.TagKeysType Types.TagValueType),
    -- | A list of OpendID Connect provider ARNs.
    openIdConnectProviderARNs :: Core.Maybe [Types.ARNString],
    -- | An array of Amazon Resource Names (ARNs) of the SAML provider for your identity pool.
    samlProviderARNs :: Core.Maybe [Types.ARNString],
    -- | Optional key:value pairs mapping provider names to provider app IDs.
    supportedLoginProviders :: Core.Maybe (Core.HashMap Types.IdentityProviderName Types.IdentityProviderId)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateIdentityPool' value with any optional fields omitted.
mkUpdateIdentityPool ::
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  -- | 'identityPoolName'
  Types.IdentityPoolName ->
  -- | 'allowUnauthenticatedIdentities'
  Core.Bool ->
  UpdateIdentityPool
mkUpdateIdentityPool
  identityPoolId
  identityPoolName
  allowUnauthenticatedIdentities =
    UpdateIdentityPool'
      { identityPoolId,
        identityPoolName,
        allowUnauthenticatedIdentities,
        allowClassicFlow = Core.Nothing,
        cognitoIdentityProviders = Core.Nothing,
        developerProviderName = Core.Nothing,
        identityPoolTags = Core.Nothing,
        openIdConnectProviderARNs = Core.Nothing,
        samlProviderARNs = Core.Nothing,
        supportedLoginProviders = Core.Nothing
      }

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipIdentityPoolId :: Lens.Lens' UpdateIdentityPool Types.IdentityPoolId
uipIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED uipIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A string that you provide.
--
-- /Note:/ Consider using 'identityPoolName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipIdentityPoolName :: Lens.Lens' UpdateIdentityPool Types.IdentityPoolName
uipIdentityPoolName = Lens.field @"identityPoolName"
{-# DEPRECATED uipIdentityPoolName "Use generic-lens or generic-optics with 'identityPoolName' instead." #-}

-- | TRUE if the identity pool supports unauthenticated logins.
--
-- /Note:/ Consider using 'allowUnauthenticatedIdentities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipAllowUnauthenticatedIdentities :: Lens.Lens' UpdateIdentityPool Core.Bool
uipAllowUnauthenticatedIdentities = Lens.field @"allowUnauthenticatedIdentities"
{-# DEPRECATED uipAllowUnauthenticatedIdentities "Use generic-lens or generic-optics with 'allowUnauthenticatedIdentities' instead." #-}

-- | Enables or disables the Basic (Classic) authentication flow. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'allowClassicFlow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipAllowClassicFlow :: Lens.Lens' UpdateIdentityPool (Core.Maybe Core.Bool)
uipAllowClassicFlow = Lens.field @"allowClassicFlow"
{-# DEPRECATED uipAllowClassicFlow "Use generic-lens or generic-optics with 'allowClassicFlow' instead." #-}

-- | A list representing an Amazon Cognito user pool and its client ID.
--
-- /Note:/ Consider using 'cognitoIdentityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipCognitoIdentityProviders :: Lens.Lens' UpdateIdentityPool (Core.Maybe [Types.CognitoIdentityProvider])
uipCognitoIdentityProviders = Lens.field @"cognitoIdentityProviders"
{-# DEPRECATED uipCognitoIdentityProviders "Use generic-lens or generic-optics with 'cognitoIdentityProviders' instead." #-}

-- | The "domain" by which Cognito will refer to your users.
--
-- /Note:/ Consider using 'developerProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipDeveloperProviderName :: Lens.Lens' UpdateIdentityPool (Core.Maybe Types.DeveloperProviderName)
uipDeveloperProviderName = Lens.field @"developerProviderName"
{-# DEPRECATED uipDeveloperProviderName "Use generic-lens or generic-optics with 'developerProviderName' instead." #-}

-- | The tags that are assigned to the identity pool. A tag is a label that you can apply to identity pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
--
-- /Note:/ Consider using 'identityPoolTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipIdentityPoolTags :: Lens.Lens' UpdateIdentityPool (Core.Maybe (Core.HashMap Types.TagKeysType Types.TagValueType))
uipIdentityPoolTags = Lens.field @"identityPoolTags"
{-# DEPRECATED uipIdentityPoolTags "Use generic-lens or generic-optics with 'identityPoolTags' instead." #-}

-- | A list of OpendID Connect provider ARNs.
--
-- /Note:/ Consider using 'openIdConnectProviderARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipOpenIdConnectProviderARNs :: Lens.Lens' UpdateIdentityPool (Core.Maybe [Types.ARNString])
uipOpenIdConnectProviderARNs = Lens.field @"openIdConnectProviderARNs"
{-# DEPRECATED uipOpenIdConnectProviderARNs "Use generic-lens or generic-optics with 'openIdConnectProviderARNs' instead." #-}

-- | An array of Amazon Resource Names (ARNs) of the SAML provider for your identity pool.
--
-- /Note:/ Consider using 'samlProviderARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipSamlProviderARNs :: Lens.Lens' UpdateIdentityPool (Core.Maybe [Types.ARNString])
uipSamlProviderARNs = Lens.field @"samlProviderARNs"
{-# DEPRECATED uipSamlProviderARNs "Use generic-lens or generic-optics with 'samlProviderARNs' instead." #-}

-- | Optional key:value pairs mapping provider names to provider app IDs.
--
-- /Note:/ Consider using 'supportedLoginProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipSupportedLoginProviders :: Lens.Lens' UpdateIdentityPool (Core.Maybe (Core.HashMap Types.IdentityProviderName Types.IdentityProviderId))
uipSupportedLoginProviders = Lens.field @"supportedLoginProviders"
{-# DEPRECATED uipSupportedLoginProviders "Use generic-lens or generic-optics with 'supportedLoginProviders' instead." #-}

instance Core.FromJSON UpdateIdentityPool where
  toJSON UpdateIdentityPool {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IdentityPoolId" Core..= identityPoolId),
            Core.Just ("IdentityPoolName" Core..= identityPoolName),
            Core.Just
              ( "AllowUnauthenticatedIdentities"
                  Core..= allowUnauthenticatedIdentities
              ),
            ("AllowClassicFlow" Core..=) Core.<$> allowClassicFlow,
            ("CognitoIdentityProviders" Core..=)
              Core.<$> cognitoIdentityProviders,
            ("DeveloperProviderName" Core..=) Core.<$> developerProviderName,
            ("IdentityPoolTags" Core..=) Core.<$> identityPoolTags,
            ("OpenIdConnectProviderARNs" Core..=)
              Core.<$> openIdConnectProviderARNs,
            ("SamlProviderARNs" Core..=) Core.<$> samlProviderARNs,
            ("SupportedLoginProviders" Core..=)
              Core.<$> supportedLoginProviders
          ]
      )

instance Core.AWSRequest UpdateIdentityPool where
  type Rs UpdateIdentityPool = Types.IdentityPool
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSCognitoIdentityService.UpdateIdentityPool")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
