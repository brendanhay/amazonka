{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.AuthenticateOidcActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.AuthenticateOidcActionConfig
  ( AuthenticateOidcActionConfig (..),

    -- * Smart constructor
    mkAuthenticateOidcActionConfig,

    -- * Lenses
    aoacIssuer,
    aoacAuthorizationEndpoint,
    aoacTokenEndpoint,
    aoacUserInfoEndpoint,
    aoacClientId,
    aoacAuthenticationRequestExtraParams,
    aoacClientSecret,
    aoacOnUnauthenticatedRequest,
    aoacScope,
    aoacSessionCookieName,
    aoacSessionTimeout,
    aoacUseExistingClientSecret,
  )
where

import qualified Network.AWS.ELBv2.Types.AuthenticateOidcActionAuthenticationRequestParamName as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateOidcActionAuthenticationRequestParamValue as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateOidcActionAuthorizationEndpoint as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateOidcActionClientId as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateOidcActionClientSecret as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateOidcActionConditionalBehaviorEnum as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateOidcActionIssuer as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateOidcActionScope as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateOidcActionSessionCookieName as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateOidcActionTokenEndpoint as Types
import qualified Network.AWS.ELBv2.Types.UserInfoEndpoint as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Request parameters when using an identity provider (IdP) that is compliant with OpenID Connect (OIDC) to authenticate users.
--
-- /See:/ 'mkAuthenticateOidcActionConfig' smart constructor.
data AuthenticateOidcActionConfig = AuthenticateOidcActionConfig'
  { -- | The OIDC issuer identifier of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
    issuer :: Types.AuthenticateOidcActionIssuer,
    -- | The authorization endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
    authorizationEndpoint :: Types.AuthenticateOidcActionAuthorizationEndpoint,
    -- | The token endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
    tokenEndpoint :: Types.AuthenticateOidcActionTokenEndpoint,
    -- | The user info endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
    userInfoEndpoint :: Types.UserInfoEndpoint,
    -- | The OAuth 2.0 client identifier.
    clientId :: Types.AuthenticateOidcActionClientId,
    -- | The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
    authenticationRequestExtraParams :: Core.Maybe (Core.HashMap Types.AuthenticateOidcActionAuthenticationRequestParamName Types.AuthenticateOidcActionAuthenticationRequestParamValue),
    -- | The OAuth 2.0 client secret. This parameter is required if you are creating a rule. If you are modifying a rule, you can omit this parameter if you set @UseExistingClientSecret@ to true.
    clientSecret :: Core.Maybe Types.AuthenticateOidcActionClientSecret,
    -- | The behavior if the user is not authenticated. The following are possible values:
    --
    --
    --     * deny- Return an HTTP 401 Unauthorized error.
    --
    --
    --     * allow- Allow the request to be forwarded to the target.
    --
    --
    --     * authenticate- Redirect the request to the IdP authorization endpoint. This is the default value.
    onUnauthenticatedRequest :: Core.Maybe Types.AuthenticateOidcActionConditionalBehaviorEnum,
    -- | The set of user claims to be requested from the IdP. The default is @openid@ .
    --
    -- To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
    scope :: Core.Maybe Types.AuthenticateOidcActionScope,
    -- | The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
    sessionCookieName :: Core.Maybe Types.AuthenticateOidcActionSessionCookieName,
    -- | The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
    sessionTimeout :: Core.Maybe Core.Integer,
    -- | Indicates whether to use the existing client secret when modifying a rule. If you are creating a rule, you can omit this parameter or set it to false.
    useExistingClientSecret :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthenticateOidcActionConfig' value with any optional fields omitted.
mkAuthenticateOidcActionConfig ::
  -- | 'issuer'
  Types.AuthenticateOidcActionIssuer ->
  -- | 'authorizationEndpoint'
  Types.AuthenticateOidcActionAuthorizationEndpoint ->
  -- | 'tokenEndpoint'
  Types.AuthenticateOidcActionTokenEndpoint ->
  -- | 'userInfoEndpoint'
  Types.UserInfoEndpoint ->
  -- | 'clientId'
  Types.AuthenticateOidcActionClientId ->
  AuthenticateOidcActionConfig
mkAuthenticateOidcActionConfig
  issuer
  authorizationEndpoint
  tokenEndpoint
  userInfoEndpoint
  clientId =
    AuthenticateOidcActionConfig'
      { issuer,
        authorizationEndpoint,
        tokenEndpoint,
        userInfoEndpoint,
        clientId,
        authenticationRequestExtraParams = Core.Nothing,
        clientSecret = Core.Nothing,
        onUnauthenticatedRequest = Core.Nothing,
        scope = Core.Nothing,
        sessionCookieName = Core.Nothing,
        sessionTimeout = Core.Nothing,
        useExistingClientSecret = Core.Nothing
      }

-- | The OIDC issuer identifier of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- /Note:/ Consider using 'issuer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacIssuer :: Lens.Lens' AuthenticateOidcActionConfig Types.AuthenticateOidcActionIssuer
aoacIssuer = Lens.field @"issuer"
{-# DEPRECATED aoacIssuer "Use generic-lens or generic-optics with 'issuer' instead." #-}

-- | The authorization endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- /Note:/ Consider using 'authorizationEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacAuthorizationEndpoint :: Lens.Lens' AuthenticateOidcActionConfig Types.AuthenticateOidcActionAuthorizationEndpoint
aoacAuthorizationEndpoint = Lens.field @"authorizationEndpoint"
{-# DEPRECATED aoacAuthorizationEndpoint "Use generic-lens or generic-optics with 'authorizationEndpoint' instead." #-}

-- | The token endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- /Note:/ Consider using 'tokenEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacTokenEndpoint :: Lens.Lens' AuthenticateOidcActionConfig Types.AuthenticateOidcActionTokenEndpoint
aoacTokenEndpoint = Lens.field @"tokenEndpoint"
{-# DEPRECATED aoacTokenEndpoint "Use generic-lens or generic-optics with 'tokenEndpoint' instead." #-}

-- | The user info endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- /Note:/ Consider using 'userInfoEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacUserInfoEndpoint :: Lens.Lens' AuthenticateOidcActionConfig Types.UserInfoEndpoint
aoacUserInfoEndpoint = Lens.field @"userInfoEndpoint"
{-# DEPRECATED aoacUserInfoEndpoint "Use generic-lens or generic-optics with 'userInfoEndpoint' instead." #-}

-- | The OAuth 2.0 client identifier.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacClientId :: Lens.Lens' AuthenticateOidcActionConfig Types.AuthenticateOidcActionClientId
aoacClientId = Lens.field @"clientId"
{-# DEPRECATED aoacClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
--
-- /Note:/ Consider using 'authenticationRequestExtraParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacAuthenticationRequestExtraParams :: Lens.Lens' AuthenticateOidcActionConfig (Core.Maybe (Core.HashMap Types.AuthenticateOidcActionAuthenticationRequestParamName Types.AuthenticateOidcActionAuthenticationRequestParamValue))
aoacAuthenticationRequestExtraParams = Lens.field @"authenticationRequestExtraParams"
{-# DEPRECATED aoacAuthenticationRequestExtraParams "Use generic-lens or generic-optics with 'authenticationRequestExtraParams' instead." #-}

-- | The OAuth 2.0 client secret. This parameter is required if you are creating a rule. If you are modifying a rule, you can omit this parameter if you set @UseExistingClientSecret@ to true.
--
-- /Note:/ Consider using 'clientSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacClientSecret :: Lens.Lens' AuthenticateOidcActionConfig (Core.Maybe Types.AuthenticateOidcActionClientSecret)
aoacClientSecret = Lens.field @"clientSecret"
{-# DEPRECATED aoacClientSecret "Use generic-lens or generic-optics with 'clientSecret' instead." #-}

-- | The behavior if the user is not authenticated. The following are possible values:
--
--
--     * deny- Return an HTTP 401 Unauthorized error.
--
--
--     * allow- Allow the request to be forwarded to the target.
--
--
--     * authenticate- Redirect the request to the IdP authorization endpoint. This is the default value.
--
--
--
-- /Note:/ Consider using 'onUnauthenticatedRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacOnUnauthenticatedRequest :: Lens.Lens' AuthenticateOidcActionConfig (Core.Maybe Types.AuthenticateOidcActionConditionalBehaviorEnum)
aoacOnUnauthenticatedRequest = Lens.field @"onUnauthenticatedRequest"
{-# DEPRECATED aoacOnUnauthenticatedRequest "Use generic-lens or generic-optics with 'onUnauthenticatedRequest' instead." #-}

-- | The set of user claims to be requested from the IdP. The default is @openid@ .
--
-- To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacScope :: Lens.Lens' AuthenticateOidcActionConfig (Core.Maybe Types.AuthenticateOidcActionScope)
aoacScope = Lens.field @"scope"
{-# DEPRECATED aoacScope "Use generic-lens or generic-optics with 'scope' instead." #-}

-- | The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
--
-- /Note:/ Consider using 'sessionCookieName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacSessionCookieName :: Lens.Lens' AuthenticateOidcActionConfig (Core.Maybe Types.AuthenticateOidcActionSessionCookieName)
aoacSessionCookieName = Lens.field @"sessionCookieName"
{-# DEPRECATED aoacSessionCookieName "Use generic-lens or generic-optics with 'sessionCookieName' instead." #-}

-- | The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
--
-- /Note:/ Consider using 'sessionTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacSessionTimeout :: Lens.Lens' AuthenticateOidcActionConfig (Core.Maybe Core.Integer)
aoacSessionTimeout = Lens.field @"sessionTimeout"
{-# DEPRECATED aoacSessionTimeout "Use generic-lens or generic-optics with 'sessionTimeout' instead." #-}

-- | Indicates whether to use the existing client secret when modifying a rule. If you are creating a rule, you can omit this parameter or set it to false.
--
-- /Note:/ Consider using 'useExistingClientSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacUseExistingClientSecret :: Lens.Lens' AuthenticateOidcActionConfig (Core.Maybe Core.Bool)
aoacUseExistingClientSecret = Lens.field @"useExistingClientSecret"
{-# DEPRECATED aoacUseExistingClientSecret "Use generic-lens or generic-optics with 'useExistingClientSecret' instead." #-}

instance Core.FromXML AuthenticateOidcActionConfig where
  parseXML x =
    AuthenticateOidcActionConfig'
      Core.<$> (x Core..@ "Issuer")
      Core.<*> (x Core..@ "AuthorizationEndpoint")
      Core.<*> (x Core..@ "TokenEndpoint")
      Core.<*> (x Core..@ "UserInfoEndpoint")
      Core.<*> (x Core..@ "ClientId")
      Core.<*> ( x Core..@? "AuthenticationRequestExtraParams"
                   Core..<@> Core.parseXMLMap "entry" "key" "value"
               )
      Core.<*> (x Core..@? "ClientSecret")
      Core.<*> (x Core..@? "OnUnauthenticatedRequest")
      Core.<*> (x Core..@? "Scope")
      Core.<*> (x Core..@? "SessionCookieName")
      Core.<*> (x Core..@? "SessionTimeout")
      Core.<*> (x Core..@? "UseExistingClientSecret")
