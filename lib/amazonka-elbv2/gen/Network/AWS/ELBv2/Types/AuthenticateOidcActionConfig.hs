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
    aoacClientId,
    aoacClientSecret,
    aoacUserInfoEndpoint,
    aoacUseExistingClientSecret,
    aoacAuthenticationRequestExtraParams,
    aoacScope,
    aoacOnUnauthenticatedRequest,
    aoacSessionCookieName,
    aoacSessionTimeout,
    aoacAuthorizationEndpoint,
    aoacTokenEndpoint,
    aoacIssuer,
  )
where

import Network.AWS.ELBv2.Types.AuthenticateOidcActionConditionalBehaviorEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Request parameters when using an identity provider (IdP) that is compliant with OpenID Connect (OIDC) to authenticate users.
--
-- /See:/ 'mkAuthenticateOidcActionConfig' smart constructor.
data AuthenticateOidcActionConfig = AuthenticateOidcActionConfig'
  { -- | The OAuth 2.0 client identifier.
    clientId :: Lude.Text,
    -- | The OAuth 2.0 client secret. This parameter is required if you are creating a rule. If you are modifying a rule, you can omit this parameter if you set @UseExistingClientSecret@ to true.
    clientSecret :: Lude.Maybe Lude.Text,
    -- | The user info endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
    userInfoEndpoint :: Lude.Text,
    -- | Indicates whether to use the existing client secret when modifying a rule. If you are creating a rule, you can omit this parameter or set it to false.
    useExistingClientSecret :: Lude.Maybe Lude.Bool,
    -- | The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
    authenticationRequestExtraParams :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The set of user claims to be requested from the IdP. The default is @openid@ .
    --
    -- To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
    scope :: Lude.Maybe Lude.Text,
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
    onUnauthenticatedRequest :: Lude.Maybe AuthenticateOidcActionConditionalBehaviorEnum,
    -- | The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
    sessionCookieName :: Lude.Maybe Lude.Text,
    -- | The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
    sessionTimeout :: Lude.Maybe Lude.Integer,
    -- | The authorization endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
    authorizationEndpoint :: Lude.Text,
    -- | The token endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
    tokenEndpoint :: Lude.Text,
    -- | The OIDC issuer identifier of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
    issuer :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthenticateOidcActionConfig' with the minimum fields required to make a request.
--
-- * 'clientId' - The OAuth 2.0 client identifier.
-- * 'clientSecret' - The OAuth 2.0 client secret. This parameter is required if you are creating a rule. If you are modifying a rule, you can omit this parameter if you set @UseExistingClientSecret@ to true.
-- * 'userInfoEndpoint' - The user info endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
-- * 'useExistingClientSecret' - Indicates whether to use the existing client secret when modifying a rule. If you are creating a rule, you can omit this parameter or set it to false.
-- * 'authenticationRequestExtraParams' - The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
-- * 'scope' - The set of user claims to be requested from the IdP. The default is @openid@ .
--
-- To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
-- * 'onUnauthenticatedRequest' - The behavior if the user is not authenticated. The following are possible values:
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
-- * 'sessionCookieName' - The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
-- * 'sessionTimeout' - The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
-- * 'authorizationEndpoint' - The authorization endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
-- * 'tokenEndpoint' - The token endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
-- * 'issuer' - The OIDC issuer identifier of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
mkAuthenticateOidcActionConfig ::
  -- | 'clientId'
  Lude.Text ->
  -- | 'userInfoEndpoint'
  Lude.Text ->
  -- | 'authorizationEndpoint'
  Lude.Text ->
  -- | 'tokenEndpoint'
  Lude.Text ->
  -- | 'issuer'
  Lude.Text ->
  AuthenticateOidcActionConfig
mkAuthenticateOidcActionConfig
  pClientId_
  pUserInfoEndpoint_
  pAuthorizationEndpoint_
  pTokenEndpoint_
  pIssuer_ =
    AuthenticateOidcActionConfig'
      { clientId = pClientId_,
        clientSecret = Lude.Nothing,
        userInfoEndpoint = pUserInfoEndpoint_,
        useExistingClientSecret = Lude.Nothing,
        authenticationRequestExtraParams = Lude.Nothing,
        scope = Lude.Nothing,
        onUnauthenticatedRequest = Lude.Nothing,
        sessionCookieName = Lude.Nothing,
        sessionTimeout = Lude.Nothing,
        authorizationEndpoint = pAuthorizationEndpoint_,
        tokenEndpoint = pTokenEndpoint_,
        issuer = pIssuer_
      }

-- | The OAuth 2.0 client identifier.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacClientId :: Lens.Lens' AuthenticateOidcActionConfig Lude.Text
aoacClientId = Lens.lens (clientId :: AuthenticateOidcActionConfig -> Lude.Text) (\s a -> s {clientId = a} :: AuthenticateOidcActionConfig)
{-# DEPRECATED aoacClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The OAuth 2.0 client secret. This parameter is required if you are creating a rule. If you are modifying a rule, you can omit this parameter if you set @UseExistingClientSecret@ to true.
--
-- /Note:/ Consider using 'clientSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacClientSecret :: Lens.Lens' AuthenticateOidcActionConfig (Lude.Maybe Lude.Text)
aoacClientSecret = Lens.lens (clientSecret :: AuthenticateOidcActionConfig -> Lude.Maybe Lude.Text) (\s a -> s {clientSecret = a} :: AuthenticateOidcActionConfig)
{-# DEPRECATED aoacClientSecret "Use generic-lens or generic-optics with 'clientSecret' instead." #-}

-- | The user info endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- /Note:/ Consider using 'userInfoEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacUserInfoEndpoint :: Lens.Lens' AuthenticateOidcActionConfig Lude.Text
aoacUserInfoEndpoint = Lens.lens (userInfoEndpoint :: AuthenticateOidcActionConfig -> Lude.Text) (\s a -> s {userInfoEndpoint = a} :: AuthenticateOidcActionConfig)
{-# DEPRECATED aoacUserInfoEndpoint "Use generic-lens or generic-optics with 'userInfoEndpoint' instead." #-}

-- | Indicates whether to use the existing client secret when modifying a rule. If you are creating a rule, you can omit this parameter or set it to false.
--
-- /Note:/ Consider using 'useExistingClientSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacUseExistingClientSecret :: Lens.Lens' AuthenticateOidcActionConfig (Lude.Maybe Lude.Bool)
aoacUseExistingClientSecret = Lens.lens (useExistingClientSecret :: AuthenticateOidcActionConfig -> Lude.Maybe Lude.Bool) (\s a -> s {useExistingClientSecret = a} :: AuthenticateOidcActionConfig)
{-# DEPRECATED aoacUseExistingClientSecret "Use generic-lens or generic-optics with 'useExistingClientSecret' instead." #-}

-- | The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
--
-- /Note:/ Consider using 'authenticationRequestExtraParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacAuthenticationRequestExtraParams :: Lens.Lens' AuthenticateOidcActionConfig (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
aoacAuthenticationRequestExtraParams = Lens.lens (authenticationRequestExtraParams :: AuthenticateOidcActionConfig -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {authenticationRequestExtraParams = a} :: AuthenticateOidcActionConfig)
{-# DEPRECATED aoacAuthenticationRequestExtraParams "Use generic-lens or generic-optics with 'authenticationRequestExtraParams' instead." #-}

-- | The set of user claims to be requested from the IdP. The default is @openid@ .
--
-- To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacScope :: Lens.Lens' AuthenticateOidcActionConfig (Lude.Maybe Lude.Text)
aoacScope = Lens.lens (scope :: AuthenticateOidcActionConfig -> Lude.Maybe Lude.Text) (\s a -> s {scope = a} :: AuthenticateOidcActionConfig)
{-# DEPRECATED aoacScope "Use generic-lens or generic-optics with 'scope' instead." #-}

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
aoacOnUnauthenticatedRequest :: Lens.Lens' AuthenticateOidcActionConfig (Lude.Maybe AuthenticateOidcActionConditionalBehaviorEnum)
aoacOnUnauthenticatedRequest = Lens.lens (onUnauthenticatedRequest :: AuthenticateOidcActionConfig -> Lude.Maybe AuthenticateOidcActionConditionalBehaviorEnum) (\s a -> s {onUnauthenticatedRequest = a} :: AuthenticateOidcActionConfig)
{-# DEPRECATED aoacOnUnauthenticatedRequest "Use generic-lens or generic-optics with 'onUnauthenticatedRequest' instead." #-}

-- | The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
--
-- /Note:/ Consider using 'sessionCookieName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacSessionCookieName :: Lens.Lens' AuthenticateOidcActionConfig (Lude.Maybe Lude.Text)
aoacSessionCookieName = Lens.lens (sessionCookieName :: AuthenticateOidcActionConfig -> Lude.Maybe Lude.Text) (\s a -> s {sessionCookieName = a} :: AuthenticateOidcActionConfig)
{-# DEPRECATED aoacSessionCookieName "Use generic-lens or generic-optics with 'sessionCookieName' instead." #-}

-- | The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
--
-- /Note:/ Consider using 'sessionTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacSessionTimeout :: Lens.Lens' AuthenticateOidcActionConfig (Lude.Maybe Lude.Integer)
aoacSessionTimeout = Lens.lens (sessionTimeout :: AuthenticateOidcActionConfig -> Lude.Maybe Lude.Integer) (\s a -> s {sessionTimeout = a} :: AuthenticateOidcActionConfig)
{-# DEPRECATED aoacSessionTimeout "Use generic-lens or generic-optics with 'sessionTimeout' instead." #-}

-- | The authorization endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- /Note:/ Consider using 'authorizationEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacAuthorizationEndpoint :: Lens.Lens' AuthenticateOidcActionConfig Lude.Text
aoacAuthorizationEndpoint = Lens.lens (authorizationEndpoint :: AuthenticateOidcActionConfig -> Lude.Text) (\s a -> s {authorizationEndpoint = a} :: AuthenticateOidcActionConfig)
{-# DEPRECATED aoacAuthorizationEndpoint "Use generic-lens or generic-optics with 'authorizationEndpoint' instead." #-}

-- | The token endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- /Note:/ Consider using 'tokenEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacTokenEndpoint :: Lens.Lens' AuthenticateOidcActionConfig Lude.Text
aoacTokenEndpoint = Lens.lens (tokenEndpoint :: AuthenticateOidcActionConfig -> Lude.Text) (\s a -> s {tokenEndpoint = a} :: AuthenticateOidcActionConfig)
{-# DEPRECATED aoacTokenEndpoint "Use generic-lens or generic-optics with 'tokenEndpoint' instead." #-}

-- | The OIDC issuer identifier of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- /Note:/ Consider using 'issuer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoacIssuer :: Lens.Lens' AuthenticateOidcActionConfig Lude.Text
aoacIssuer = Lens.lens (issuer :: AuthenticateOidcActionConfig -> Lude.Text) (\s a -> s {issuer = a} :: AuthenticateOidcActionConfig)
{-# DEPRECATED aoacIssuer "Use generic-lens or generic-optics with 'issuer' instead." #-}

instance Lude.FromXML AuthenticateOidcActionConfig where
  parseXML x =
    AuthenticateOidcActionConfig'
      Lude.<$> (x Lude..@ "ClientId")
      Lude.<*> (x Lude..@? "ClientSecret")
      Lude.<*> (x Lude..@ "UserInfoEndpoint")
      Lude.<*> (x Lude..@? "UseExistingClientSecret")
      Lude.<*> ( x Lude..@? "AuthenticationRequestExtraParams" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLMap "entry" "key" "value")
               )
      Lude.<*> (x Lude..@? "Scope")
      Lude.<*> (x Lude..@? "OnUnauthenticatedRequest")
      Lude.<*> (x Lude..@? "SessionCookieName")
      Lude.<*> (x Lude..@? "SessionTimeout")
      Lude.<*> (x Lude..@ "AuthorizationEndpoint")
      Lude.<*> (x Lude..@ "TokenEndpoint")
      Lude.<*> (x Lude..@ "Issuer")

instance Lude.ToQuery AuthenticateOidcActionConfig where
  toQuery AuthenticateOidcActionConfig' {..} =
    Lude.mconcat
      [ "ClientId" Lude.=: clientId,
        "ClientSecret" Lude.=: clientSecret,
        "UserInfoEndpoint" Lude.=: userInfoEndpoint,
        "UseExistingClientSecret" Lude.=: useExistingClientSecret,
        "AuthenticationRequestExtraParams"
          Lude.=: Lude.toQuery
            ( Lude.toQueryMap "entry" "key" "value"
                Lude.<$> authenticationRequestExtraParams
            ),
        "Scope" Lude.=: scope,
        "OnUnauthenticatedRequest" Lude.=: onUnauthenticatedRequest,
        "SessionCookieName" Lude.=: sessionCookieName,
        "SessionTimeout" Lude.=: sessionTimeout,
        "AuthorizationEndpoint" Lude.=: authorizationEndpoint,
        "TokenEndpoint" Lude.=: tokenEndpoint,
        "Issuer" Lude.=: issuer
      ]
