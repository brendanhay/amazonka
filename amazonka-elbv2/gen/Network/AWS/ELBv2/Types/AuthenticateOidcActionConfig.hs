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
import qualified Network.AWS.Prelude as Prelude

-- | Request parameters when using an identity provider (IdP) that is
-- compliant with OpenID Connect (OIDC) to authenticate users.
--
-- /See:/ 'newAuthenticateOidcActionConfig' smart constructor.
data AuthenticateOidcActionConfig = AuthenticateOidcActionConfig'
  { -- | Indicates whether to use the existing client secret when modifying a
    -- rule. If you are creating a rule, you can omit this parameter or set it
    -- to false.
    useExistingClientSecret :: Prelude.Maybe Prelude.Bool,
    -- | The OAuth 2.0 client secret. This parameter is required if you are
    -- creating a rule. If you are modifying a rule, you can omit this
    -- parameter if you set @UseExistingClientSecret@ to true.
    clientSecret :: Prelude.Maybe Prelude.Text,
    -- | The maximum duration of the authentication session, in seconds. The
    -- default is 604800 seconds (7 days).
    sessionTimeout :: Prelude.Maybe Prelude.Integer,
    -- | The set of user claims to be requested from the IdP. The default is
    -- @openid@.
    --
    -- To verify which scope values your IdP supports and how to separate
    -- multiple values, see the documentation for your IdP.
    scope :: Prelude.Maybe Prelude.Text,
    -- | The query parameters (up to 10) to include in the redirect request to
    -- the authorization endpoint.
    authenticationRequestExtraParams :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the cookie used to maintain session information. The default
    -- is AWSELBAuthSessionCookie.
    sessionCookieName :: Prelude.Maybe Prelude.Text,
    -- | The behavior if the user is not authenticated. The following are
    -- possible values:
    --
    -- -   deny@@ - Return an HTTP 401 Unauthorized error.
    --
    -- -   allow@@ - Allow the request to be forwarded to the target.
    --
    -- -   authenticate@@ - Redirect the request to the IdP authorization
    --     endpoint. This is the default value.
    onUnauthenticatedRequest :: Prelude.Maybe AuthenticateOidcActionConditionalBehaviorEnum,
    -- | The OIDC issuer identifier of the IdP. This must be a full URL,
    -- including the HTTPS protocol, the domain, and the path.
    issuer :: Prelude.Text,
    -- | The authorization endpoint of the IdP. This must be a full URL,
    -- including the HTTPS protocol, the domain, and the path.
    authorizationEndpoint :: Prelude.Text,
    -- | The token endpoint of the IdP. This must be a full URL, including the
    -- HTTPS protocol, the domain, and the path.
    tokenEndpoint :: Prelude.Text,
    -- | The user info endpoint of the IdP. This must be a full URL, including
    -- the HTTPS protocol, the domain, and the path.
    userInfoEndpoint :: Prelude.Text,
    -- | The OAuth 2.0 client identifier.
    clientId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'authorizationEndpoint'
  Prelude.Text ->
  -- | 'tokenEndpoint'
  Prelude.Text ->
  -- | 'userInfoEndpoint'
  Prelude.Text ->
  -- | 'clientId'
  Prelude.Text ->
  AuthenticateOidcActionConfig
newAuthenticateOidcActionConfig
  pIssuer_
  pAuthorizationEndpoint_
  pTokenEndpoint_
  pUserInfoEndpoint_
  pClientId_ =
    AuthenticateOidcActionConfig'
      { useExistingClientSecret =
          Prelude.Nothing,
        clientSecret = Prelude.Nothing,
        sessionTimeout = Prelude.Nothing,
        scope = Prelude.Nothing,
        authenticationRequestExtraParams =
          Prelude.Nothing,
        sessionCookieName = Prelude.Nothing,
        onUnauthenticatedRequest = Prelude.Nothing,
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
authenticateOidcActionConfig_useExistingClientSecret :: Lens.Lens' AuthenticateOidcActionConfig (Prelude.Maybe Prelude.Bool)
authenticateOidcActionConfig_useExistingClientSecret = Lens.lens (\AuthenticateOidcActionConfig' {useExistingClientSecret} -> useExistingClientSecret) (\s@AuthenticateOidcActionConfig' {} a -> s {useExistingClientSecret = a} :: AuthenticateOidcActionConfig)

-- | The OAuth 2.0 client secret. This parameter is required if you are
-- creating a rule. If you are modifying a rule, you can omit this
-- parameter if you set @UseExistingClientSecret@ to true.
authenticateOidcActionConfig_clientSecret :: Lens.Lens' AuthenticateOidcActionConfig (Prelude.Maybe Prelude.Text)
authenticateOidcActionConfig_clientSecret = Lens.lens (\AuthenticateOidcActionConfig' {clientSecret} -> clientSecret) (\s@AuthenticateOidcActionConfig' {} a -> s {clientSecret = a} :: AuthenticateOidcActionConfig)

-- | The maximum duration of the authentication session, in seconds. The
-- default is 604800 seconds (7 days).
authenticateOidcActionConfig_sessionTimeout :: Lens.Lens' AuthenticateOidcActionConfig (Prelude.Maybe Prelude.Integer)
authenticateOidcActionConfig_sessionTimeout = Lens.lens (\AuthenticateOidcActionConfig' {sessionTimeout} -> sessionTimeout) (\s@AuthenticateOidcActionConfig' {} a -> s {sessionTimeout = a} :: AuthenticateOidcActionConfig)

-- | The set of user claims to be requested from the IdP. The default is
-- @openid@.
--
-- To verify which scope values your IdP supports and how to separate
-- multiple values, see the documentation for your IdP.
authenticateOidcActionConfig_scope :: Lens.Lens' AuthenticateOidcActionConfig (Prelude.Maybe Prelude.Text)
authenticateOidcActionConfig_scope = Lens.lens (\AuthenticateOidcActionConfig' {scope} -> scope) (\s@AuthenticateOidcActionConfig' {} a -> s {scope = a} :: AuthenticateOidcActionConfig)

-- | The query parameters (up to 10) to include in the redirect request to
-- the authorization endpoint.
authenticateOidcActionConfig_authenticationRequestExtraParams :: Lens.Lens' AuthenticateOidcActionConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
authenticateOidcActionConfig_authenticationRequestExtraParams = Lens.lens (\AuthenticateOidcActionConfig' {authenticationRequestExtraParams} -> authenticationRequestExtraParams) (\s@AuthenticateOidcActionConfig' {} a -> s {authenticationRequestExtraParams = a} :: AuthenticateOidcActionConfig) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the cookie used to maintain session information. The default
-- is AWSELBAuthSessionCookie.
authenticateOidcActionConfig_sessionCookieName :: Lens.Lens' AuthenticateOidcActionConfig (Prelude.Maybe Prelude.Text)
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
authenticateOidcActionConfig_onUnauthenticatedRequest :: Lens.Lens' AuthenticateOidcActionConfig (Prelude.Maybe AuthenticateOidcActionConditionalBehaviorEnum)
authenticateOidcActionConfig_onUnauthenticatedRequest = Lens.lens (\AuthenticateOidcActionConfig' {onUnauthenticatedRequest} -> onUnauthenticatedRequest) (\s@AuthenticateOidcActionConfig' {} a -> s {onUnauthenticatedRequest = a} :: AuthenticateOidcActionConfig)

-- | The OIDC issuer identifier of the IdP. This must be a full URL,
-- including the HTTPS protocol, the domain, and the path.
authenticateOidcActionConfig_issuer :: Lens.Lens' AuthenticateOidcActionConfig Prelude.Text
authenticateOidcActionConfig_issuer = Lens.lens (\AuthenticateOidcActionConfig' {issuer} -> issuer) (\s@AuthenticateOidcActionConfig' {} a -> s {issuer = a} :: AuthenticateOidcActionConfig)

-- | The authorization endpoint of the IdP. This must be a full URL,
-- including the HTTPS protocol, the domain, and the path.
authenticateOidcActionConfig_authorizationEndpoint :: Lens.Lens' AuthenticateOidcActionConfig Prelude.Text
authenticateOidcActionConfig_authorizationEndpoint = Lens.lens (\AuthenticateOidcActionConfig' {authorizationEndpoint} -> authorizationEndpoint) (\s@AuthenticateOidcActionConfig' {} a -> s {authorizationEndpoint = a} :: AuthenticateOidcActionConfig)

-- | The token endpoint of the IdP. This must be a full URL, including the
-- HTTPS protocol, the domain, and the path.
authenticateOidcActionConfig_tokenEndpoint :: Lens.Lens' AuthenticateOidcActionConfig Prelude.Text
authenticateOidcActionConfig_tokenEndpoint = Lens.lens (\AuthenticateOidcActionConfig' {tokenEndpoint} -> tokenEndpoint) (\s@AuthenticateOidcActionConfig' {} a -> s {tokenEndpoint = a} :: AuthenticateOidcActionConfig)

-- | The user info endpoint of the IdP. This must be a full URL, including
-- the HTTPS protocol, the domain, and the path.
authenticateOidcActionConfig_userInfoEndpoint :: Lens.Lens' AuthenticateOidcActionConfig Prelude.Text
authenticateOidcActionConfig_userInfoEndpoint = Lens.lens (\AuthenticateOidcActionConfig' {userInfoEndpoint} -> userInfoEndpoint) (\s@AuthenticateOidcActionConfig' {} a -> s {userInfoEndpoint = a} :: AuthenticateOidcActionConfig)

-- | The OAuth 2.0 client identifier.
authenticateOidcActionConfig_clientId :: Lens.Lens' AuthenticateOidcActionConfig Prelude.Text
authenticateOidcActionConfig_clientId = Lens.lens (\AuthenticateOidcActionConfig' {clientId} -> clientId) (\s@AuthenticateOidcActionConfig' {} a -> s {clientId = a} :: AuthenticateOidcActionConfig)

instance Core.FromXML AuthenticateOidcActionConfig where
  parseXML x =
    AuthenticateOidcActionConfig'
      Prelude.<$> (x Core..@? "UseExistingClientSecret")
      Prelude.<*> (x Core..@? "ClientSecret")
      Prelude.<*> (x Core..@? "SessionTimeout")
      Prelude.<*> (x Core..@? "Scope")
      Prelude.<*> ( x Core..@? "AuthenticationRequestExtraParams"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLMap "entry" "key" "value")
                  )
      Prelude.<*> (x Core..@? "SessionCookieName")
      Prelude.<*> (x Core..@? "OnUnauthenticatedRequest")
      Prelude.<*> (x Core..@ "Issuer")
      Prelude.<*> (x Core..@ "AuthorizationEndpoint")
      Prelude.<*> (x Core..@ "TokenEndpoint")
      Prelude.<*> (x Core..@ "UserInfoEndpoint")
      Prelude.<*> (x Core..@ "ClientId")

instance
  Prelude.Hashable
    AuthenticateOidcActionConfig

instance Prelude.NFData AuthenticateOidcActionConfig

instance Core.ToQuery AuthenticateOidcActionConfig where
  toQuery AuthenticateOidcActionConfig' {..} =
    Prelude.mconcat
      [ "UseExistingClientSecret"
          Core.=: useExistingClientSecret,
        "ClientSecret" Core.=: clientSecret,
        "SessionTimeout" Core.=: sessionTimeout,
        "Scope" Core.=: scope,
        "AuthenticationRequestExtraParams"
          Core.=: Core.toQuery
            ( Core.toQueryMap "entry" "key" "value"
                Prelude.<$> authenticationRequestExtraParams
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
