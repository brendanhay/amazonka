{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.AuthenticateOidcActionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.AuthenticateOidcActionConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types.AuthenticateOidcActionConditionalBehaviorEnum
import qualified Network.AWS.Lens as Lens

-- | Request parameters when using an identity provider (IdP) that is
-- compliant with OpenID Connect (OIDC) to authenticate users.
--
-- /See:/ 'newAuthenticateOidcActionConfig' smart constructor.
data AuthenticateOidcActionConfig = AuthenticateOidcActionConfig'
  { -- | Indicates whether to use the existing client secret when modifying a
    -- rule. If you are creating a rule, you can omit this parameter or set it
    -- to false.
    useExistingClientSecret :: Core.Maybe Core.Bool,
    -- | The OAuth 2.0 client secret. This parameter is required if you are
    -- creating a rule. If you are modifying a rule, you can omit this
    -- parameter if you set @UseExistingClientSecret@ to true.
    clientSecret :: Core.Maybe Core.Text,
    -- | The maximum duration of the authentication session, in seconds. The
    -- default is 604800 seconds (7 days).
    sessionTimeout :: Core.Maybe Core.Integer,
    -- | The set of user claims to be requested from the IdP. The default is
    -- @openid@.
    --
    -- To verify which scope values your IdP supports and how to separate
    -- multiple values, see the documentation for your IdP.
    scope :: Core.Maybe Core.Text,
    -- | The query parameters (up to 10) to include in the redirect request to
    -- the authorization endpoint.
    authenticationRequestExtraParams :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the cookie used to maintain session information. The default
    -- is AWSELBAuthSessionCookie.
    sessionCookieName :: Core.Maybe Core.Text,
    -- | The behavior if the user is not authenticated. The following are
    -- possible values:
    --
    -- -   deny@@ - Return an HTTP 401 Unauthorized error.
    --
    -- -   allow@@ - Allow the request to be forwarded to the target.
    --
    -- -   authenticate@@ - Redirect the request to the IdP authorization
    --     endpoint. This is the default value.
    onUnauthenticatedRequest :: Core.Maybe AuthenticateOidcActionConditionalBehaviorEnum,
    -- | The OIDC issuer identifier of the IdP. This must be a full URL,
    -- including the HTTPS protocol, the domain, and the path.
    issuer :: Core.Text,
    -- | The authorization endpoint of the IdP. This must be a full URL,
    -- including the HTTPS protocol, the domain, and the path.
    authorizationEndpoint :: Core.Text,
    -- | The token endpoint of the IdP. This must be a full URL, including the
    -- HTTPS protocol, the domain, and the path.
    tokenEndpoint :: Core.Text,
    -- | The user info endpoint of the IdP. This must be a full URL, including
    -- the HTTPS protocol, the domain, and the path.
    userInfoEndpoint :: Core.Text,
    -- | The OAuth 2.0 client identifier.
    clientId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AuthenticateOidcActionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useExistingClientSecret', 'authenticateOidcActionConfig_useExistingClientSecret' - Indicates whether to use the existing client secret when modifying a
-- rule. If you are creating a rule, you can omit this parameter or set it
-- to false.
--
-- 'clientSecret', 'authenticateOidcActionConfig_clientSecret' - The OAuth 2.0 client secret. This parameter is required if you are
-- creating a rule. If you are modifying a rule, you can omit this
-- parameter if you set @UseExistingClientSecret@ to true.
--
-- 'sessionTimeout', 'authenticateOidcActionConfig_sessionTimeout' - The maximum duration of the authentication session, in seconds. The
-- default is 604800 seconds (7 days).
--
-- 'scope', 'authenticateOidcActionConfig_scope' - The set of user claims to be requested from the IdP. The default is
-- @openid@.
--
-- To verify which scope values your IdP supports and how to separate
-- multiple values, see the documentation for your IdP.
--
-- 'authenticationRequestExtraParams', 'authenticateOidcActionConfig_authenticationRequestExtraParams' - The query parameters (up to 10) to include in the redirect request to
-- the authorization endpoint.
--
-- 'sessionCookieName', 'authenticateOidcActionConfig_sessionCookieName' - The name of the cookie used to maintain session information. The default
-- is AWSELBAuthSessionCookie.
--
-- 'onUnauthenticatedRequest', 'authenticateOidcActionConfig_onUnauthenticatedRequest' - The behavior if the user is not authenticated. The following are
-- possible values:
--
-- -   deny@@ - Return an HTTP 401 Unauthorized error.
--
-- -   allow@@ - Allow the request to be forwarded to the target.
--
-- -   authenticate@@ - Redirect the request to the IdP authorization
--     endpoint. This is the default value.
--
-- 'issuer', 'authenticateOidcActionConfig_issuer' - The OIDC issuer identifier of the IdP. This must be a full URL,
-- including the HTTPS protocol, the domain, and the path.
--
-- 'authorizationEndpoint', 'authenticateOidcActionConfig_authorizationEndpoint' - The authorization endpoint of the IdP. This must be a full URL,
-- including the HTTPS protocol, the domain, and the path.
--
-- 'tokenEndpoint', 'authenticateOidcActionConfig_tokenEndpoint' - The token endpoint of the IdP. This must be a full URL, including the
-- HTTPS protocol, the domain, and the path.
--
-- 'userInfoEndpoint', 'authenticateOidcActionConfig_userInfoEndpoint' - The user info endpoint of the IdP. This must be a full URL, including
-- the HTTPS protocol, the domain, and the path.
--
-- 'clientId', 'authenticateOidcActionConfig_clientId' - The OAuth 2.0 client identifier.
newAuthenticateOidcActionConfig ::
  -- | 'issuer'
  Core.Text ->
  -- | 'authorizationEndpoint'
  Core.Text ->
  -- | 'tokenEndpoint'
  Core.Text ->
  -- | 'userInfoEndpoint'
  Core.Text ->
  -- | 'clientId'
  Core.Text ->
  AuthenticateOidcActionConfig
newAuthenticateOidcActionConfig
  pIssuer_
  pAuthorizationEndpoint_
  pTokenEndpoint_
  pUserInfoEndpoint_
  pClientId_ =
    AuthenticateOidcActionConfig'
      { useExistingClientSecret =
          Core.Nothing,
        clientSecret = Core.Nothing,
        sessionTimeout = Core.Nothing,
        scope = Core.Nothing,
        authenticationRequestExtraParams =
          Core.Nothing,
        sessionCookieName = Core.Nothing,
        onUnauthenticatedRequest = Core.Nothing,
        issuer = pIssuer_,
        authorizationEndpoint =
          pAuthorizationEndpoint_,
        tokenEndpoint = pTokenEndpoint_,
        userInfoEndpoint = pUserInfoEndpoint_,
        clientId = pClientId_
      }

-- | Indicates whether to use the existing client secret when modifying a
-- rule. If you are creating a rule, you can omit this parameter or set it
-- to false.
authenticateOidcActionConfig_useExistingClientSecret :: Lens.Lens' AuthenticateOidcActionConfig (Core.Maybe Core.Bool)
authenticateOidcActionConfig_useExistingClientSecret = Lens.lens (\AuthenticateOidcActionConfig' {useExistingClientSecret} -> useExistingClientSecret) (\s@AuthenticateOidcActionConfig' {} a -> s {useExistingClientSecret = a} :: AuthenticateOidcActionConfig)

-- | The OAuth 2.0 client secret. This parameter is required if you are
-- creating a rule. If you are modifying a rule, you can omit this
-- parameter if you set @UseExistingClientSecret@ to true.
authenticateOidcActionConfig_clientSecret :: Lens.Lens' AuthenticateOidcActionConfig (Core.Maybe Core.Text)
authenticateOidcActionConfig_clientSecret = Lens.lens (\AuthenticateOidcActionConfig' {clientSecret} -> clientSecret) (\s@AuthenticateOidcActionConfig' {} a -> s {clientSecret = a} :: AuthenticateOidcActionConfig)

-- | The maximum duration of the authentication session, in seconds. The
-- default is 604800 seconds (7 days).
authenticateOidcActionConfig_sessionTimeout :: Lens.Lens' AuthenticateOidcActionConfig (Core.Maybe Core.Integer)
authenticateOidcActionConfig_sessionTimeout = Lens.lens (\AuthenticateOidcActionConfig' {sessionTimeout} -> sessionTimeout) (\s@AuthenticateOidcActionConfig' {} a -> s {sessionTimeout = a} :: AuthenticateOidcActionConfig)

-- | The set of user claims to be requested from the IdP. The default is
-- @openid@.
--
-- To verify which scope values your IdP supports and how to separate
-- multiple values, see the documentation for your IdP.
authenticateOidcActionConfig_scope :: Lens.Lens' AuthenticateOidcActionConfig (Core.Maybe Core.Text)
authenticateOidcActionConfig_scope = Lens.lens (\AuthenticateOidcActionConfig' {scope} -> scope) (\s@AuthenticateOidcActionConfig' {} a -> s {scope = a} :: AuthenticateOidcActionConfig)

-- | The query parameters (up to 10) to include in the redirect request to
-- the authorization endpoint.
authenticateOidcActionConfig_authenticationRequestExtraParams :: Lens.Lens' AuthenticateOidcActionConfig (Core.Maybe (Core.HashMap Core.Text Core.Text))
authenticateOidcActionConfig_authenticationRequestExtraParams = Lens.lens (\AuthenticateOidcActionConfig' {authenticationRequestExtraParams} -> authenticationRequestExtraParams) (\s@AuthenticateOidcActionConfig' {} a -> s {authenticationRequestExtraParams = a} :: AuthenticateOidcActionConfig) Core.. Lens.mapping Lens._Coerce

-- | The name of the cookie used to maintain session information. The default
-- is AWSELBAuthSessionCookie.
authenticateOidcActionConfig_sessionCookieName :: Lens.Lens' AuthenticateOidcActionConfig (Core.Maybe Core.Text)
authenticateOidcActionConfig_sessionCookieName = Lens.lens (\AuthenticateOidcActionConfig' {sessionCookieName} -> sessionCookieName) (\s@AuthenticateOidcActionConfig' {} a -> s {sessionCookieName = a} :: AuthenticateOidcActionConfig)

-- | The behavior if the user is not authenticated. The following are
-- possible values:
--
-- -   deny@@ - Return an HTTP 401 Unauthorized error.
--
-- -   allow@@ - Allow the request to be forwarded to the target.
--
-- -   authenticate@@ - Redirect the request to the IdP authorization
--     endpoint. This is the default value.
authenticateOidcActionConfig_onUnauthenticatedRequest :: Lens.Lens' AuthenticateOidcActionConfig (Core.Maybe AuthenticateOidcActionConditionalBehaviorEnum)
authenticateOidcActionConfig_onUnauthenticatedRequest = Lens.lens (\AuthenticateOidcActionConfig' {onUnauthenticatedRequest} -> onUnauthenticatedRequest) (\s@AuthenticateOidcActionConfig' {} a -> s {onUnauthenticatedRequest = a} :: AuthenticateOidcActionConfig)

-- | The OIDC issuer identifier of the IdP. This must be a full URL,
-- including the HTTPS protocol, the domain, and the path.
authenticateOidcActionConfig_issuer :: Lens.Lens' AuthenticateOidcActionConfig Core.Text
authenticateOidcActionConfig_issuer = Lens.lens (\AuthenticateOidcActionConfig' {issuer} -> issuer) (\s@AuthenticateOidcActionConfig' {} a -> s {issuer = a} :: AuthenticateOidcActionConfig)

-- | The authorization endpoint of the IdP. This must be a full URL,
-- including the HTTPS protocol, the domain, and the path.
authenticateOidcActionConfig_authorizationEndpoint :: Lens.Lens' AuthenticateOidcActionConfig Core.Text
authenticateOidcActionConfig_authorizationEndpoint = Lens.lens (\AuthenticateOidcActionConfig' {authorizationEndpoint} -> authorizationEndpoint) (\s@AuthenticateOidcActionConfig' {} a -> s {authorizationEndpoint = a} :: AuthenticateOidcActionConfig)

-- | The token endpoint of the IdP. This must be a full URL, including the
-- HTTPS protocol, the domain, and the path.
authenticateOidcActionConfig_tokenEndpoint :: Lens.Lens' AuthenticateOidcActionConfig Core.Text
authenticateOidcActionConfig_tokenEndpoint = Lens.lens (\AuthenticateOidcActionConfig' {tokenEndpoint} -> tokenEndpoint) (\s@AuthenticateOidcActionConfig' {} a -> s {tokenEndpoint = a} :: AuthenticateOidcActionConfig)

-- | The user info endpoint of the IdP. This must be a full URL, including
-- the HTTPS protocol, the domain, and the path.
authenticateOidcActionConfig_userInfoEndpoint :: Lens.Lens' AuthenticateOidcActionConfig Core.Text
authenticateOidcActionConfig_userInfoEndpoint = Lens.lens (\AuthenticateOidcActionConfig' {userInfoEndpoint} -> userInfoEndpoint) (\s@AuthenticateOidcActionConfig' {} a -> s {userInfoEndpoint = a} :: AuthenticateOidcActionConfig)

-- | The OAuth 2.0 client identifier.
authenticateOidcActionConfig_clientId :: Lens.Lens' AuthenticateOidcActionConfig Core.Text
authenticateOidcActionConfig_clientId = Lens.lens (\AuthenticateOidcActionConfig' {clientId} -> clientId) (\s@AuthenticateOidcActionConfig' {} a -> s {clientId = a} :: AuthenticateOidcActionConfig)

instance Core.FromXML AuthenticateOidcActionConfig where
  parseXML x =
    AuthenticateOidcActionConfig'
      Core.<$> (x Core..@? "UseExistingClientSecret")
      Core.<*> (x Core..@? "ClientSecret")
      Core.<*> (x Core..@? "SessionTimeout")
      Core.<*> (x Core..@? "Scope")
      Core.<*> ( x Core..@? "AuthenticationRequestExtraParams"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLMap "entry" "key" "value")
               )
      Core.<*> (x Core..@? "SessionCookieName")
      Core.<*> (x Core..@? "OnUnauthenticatedRequest")
      Core.<*> (x Core..@ "Issuer")
      Core.<*> (x Core..@ "AuthorizationEndpoint")
      Core.<*> (x Core..@ "TokenEndpoint")
      Core.<*> (x Core..@ "UserInfoEndpoint")
      Core.<*> (x Core..@ "ClientId")

instance Core.Hashable AuthenticateOidcActionConfig

instance Core.NFData AuthenticateOidcActionConfig

instance Core.ToQuery AuthenticateOidcActionConfig where
  toQuery AuthenticateOidcActionConfig' {..} =
    Core.mconcat
      [ "UseExistingClientSecret"
          Core.=: useExistingClientSecret,
        "ClientSecret" Core.=: clientSecret,
        "SessionTimeout" Core.=: sessionTimeout,
        "Scope" Core.=: scope,
        "AuthenticationRequestExtraParams"
          Core.=: Core.toQuery
            ( Core.toQueryMap "entry" "key" "value"
                Core.<$> authenticationRequestExtraParams
            ),
        "SessionCookieName" Core.=: sessionCookieName,
        "OnUnauthenticatedRequest"
          Core.=: onUnauthenticatedRequest,
        "Issuer" Core.=: issuer,
        "AuthorizationEndpoint"
          Core.=: authorizationEndpoint,
        "TokenEndpoint" Core.=: tokenEndpoint,
        "UserInfoEndpoint" Core.=: userInfoEndpoint,
        "ClientId" Core.=: clientId
      ]
