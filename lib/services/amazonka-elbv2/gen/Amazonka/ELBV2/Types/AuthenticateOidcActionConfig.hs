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
-- Module      : Amazonka.ELBV2.Types.AuthenticateOidcActionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.AuthenticateOidcActionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types.AuthenticateOidcActionConditionalBehaviorEnum
import qualified Amazonka.Prelude as Prelude

-- | Request parameters when using an identity provider (IdP) that is
-- compliant with OpenID Connect (OIDC) to authenticate users.
--
-- /See:/ 'newAuthenticateOidcActionConfig' smart constructor.
data AuthenticateOidcActionConfig = AuthenticateOidcActionConfig'
  { -- | The query parameters (up to 10) to include in the redirect request to
    -- the authorization endpoint.
    authenticationRequestExtraParams :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The OAuth 2.0 client secret. This parameter is required if you are
    -- creating a rule. If you are modifying a rule, you can omit this
    -- parameter if you set @UseExistingClientSecret@ to true.
    clientSecret :: Prelude.Maybe Prelude.Text,
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
    -- | The set of user claims to be requested from the IdP. The default is
    -- @openid@.
    --
    -- To verify which scope values your IdP supports and how to separate
    -- multiple values, see the documentation for your IdP.
    scope :: Prelude.Maybe Prelude.Text,
    -- | The name of the cookie used to maintain session information. The default
    -- is AWSELBAuthSessionCookie.
    sessionCookieName :: Prelude.Maybe Prelude.Text,
    -- | The maximum duration of the authentication session, in seconds. The
    -- default is 604800 seconds (7 days).
    sessionTimeout :: Prelude.Maybe Prelude.Integer,
    -- | Indicates whether to use the existing client secret when modifying a
    -- rule. If you are creating a rule, you can omit this parameter or set it
    -- to false.
    useExistingClientSecret :: Prelude.Maybe Prelude.Bool,
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
-- 'authenticationRequestExtraParams', 'authenticateOidcActionConfig_authenticationRequestExtraParams' - The query parameters (up to 10) to include in the redirect request to
-- the authorization endpoint.
--
-- 'clientSecret', 'authenticateOidcActionConfig_clientSecret' - The OAuth 2.0 client secret. This parameter is required if you are
-- creating a rule. If you are modifying a rule, you can omit this
-- parameter if you set @UseExistingClientSecret@ to true.
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
-- 'scope', 'authenticateOidcActionConfig_scope' - The set of user claims to be requested from the IdP. The default is
-- @openid@.
--
-- To verify which scope values your IdP supports and how to separate
-- multiple values, see the documentation for your IdP.
--
-- 'sessionCookieName', 'authenticateOidcActionConfig_sessionCookieName' - The name of the cookie used to maintain session information. The default
-- is AWSELBAuthSessionCookie.
--
-- 'sessionTimeout', 'authenticateOidcActionConfig_sessionTimeout' - The maximum duration of the authentication session, in seconds. The
-- default is 604800 seconds (7 days).
--
-- 'useExistingClientSecret', 'authenticateOidcActionConfig_useExistingClientSecret' - Indicates whether to use the existing client secret when modifying a
-- rule. If you are creating a rule, you can omit this parameter or set it
-- to false.
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
      { authenticationRequestExtraParams =
          Prelude.Nothing,
        clientSecret = Prelude.Nothing,
        onUnauthenticatedRequest = Prelude.Nothing,
        scope = Prelude.Nothing,
        sessionCookieName = Prelude.Nothing,
        sessionTimeout = Prelude.Nothing,
        useExistingClientSecret = Prelude.Nothing,
        issuer = pIssuer_,
        authorizationEndpoint =
          pAuthorizationEndpoint_,
        tokenEndpoint = pTokenEndpoint_,
        userInfoEndpoint = pUserInfoEndpoint_,
        clientId = pClientId_
      }

-- | The query parameters (up to 10) to include in the redirect request to
-- the authorization endpoint.
authenticateOidcActionConfig_authenticationRequestExtraParams :: Lens.Lens' AuthenticateOidcActionConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
authenticateOidcActionConfig_authenticationRequestExtraParams = Lens.lens (\AuthenticateOidcActionConfig' {authenticationRequestExtraParams} -> authenticationRequestExtraParams) (\s@AuthenticateOidcActionConfig' {} a -> s {authenticationRequestExtraParams = a} :: AuthenticateOidcActionConfig) Prelude.. Lens.mapping Lens.coerced

-- | The OAuth 2.0 client secret. This parameter is required if you are
-- creating a rule. If you are modifying a rule, you can omit this
-- parameter if you set @UseExistingClientSecret@ to true.
authenticateOidcActionConfig_clientSecret :: Lens.Lens' AuthenticateOidcActionConfig (Prelude.Maybe Prelude.Text)
authenticateOidcActionConfig_clientSecret = Lens.lens (\AuthenticateOidcActionConfig' {clientSecret} -> clientSecret) (\s@AuthenticateOidcActionConfig' {} a -> s {clientSecret = a} :: AuthenticateOidcActionConfig)

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

-- | The set of user claims to be requested from the IdP. The default is
-- @openid@.
--
-- To verify which scope values your IdP supports and how to separate
-- multiple values, see the documentation for your IdP.
authenticateOidcActionConfig_scope :: Lens.Lens' AuthenticateOidcActionConfig (Prelude.Maybe Prelude.Text)
authenticateOidcActionConfig_scope = Lens.lens (\AuthenticateOidcActionConfig' {scope} -> scope) (\s@AuthenticateOidcActionConfig' {} a -> s {scope = a} :: AuthenticateOidcActionConfig)

-- | The name of the cookie used to maintain session information. The default
-- is AWSELBAuthSessionCookie.
authenticateOidcActionConfig_sessionCookieName :: Lens.Lens' AuthenticateOidcActionConfig (Prelude.Maybe Prelude.Text)
authenticateOidcActionConfig_sessionCookieName = Lens.lens (\AuthenticateOidcActionConfig' {sessionCookieName} -> sessionCookieName) (\s@AuthenticateOidcActionConfig' {} a -> s {sessionCookieName = a} :: AuthenticateOidcActionConfig)

-- | The maximum duration of the authentication session, in seconds. The
-- default is 604800 seconds (7 days).
authenticateOidcActionConfig_sessionTimeout :: Lens.Lens' AuthenticateOidcActionConfig (Prelude.Maybe Prelude.Integer)
authenticateOidcActionConfig_sessionTimeout = Lens.lens (\AuthenticateOidcActionConfig' {sessionTimeout} -> sessionTimeout) (\s@AuthenticateOidcActionConfig' {} a -> s {sessionTimeout = a} :: AuthenticateOidcActionConfig)

-- | Indicates whether to use the existing client secret when modifying a
-- rule. If you are creating a rule, you can omit this parameter or set it
-- to false.
authenticateOidcActionConfig_useExistingClientSecret :: Lens.Lens' AuthenticateOidcActionConfig (Prelude.Maybe Prelude.Bool)
authenticateOidcActionConfig_useExistingClientSecret = Lens.lens (\AuthenticateOidcActionConfig' {useExistingClientSecret} -> useExistingClientSecret) (\s@AuthenticateOidcActionConfig' {} a -> s {useExistingClientSecret = a} :: AuthenticateOidcActionConfig)

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

instance Data.FromXML AuthenticateOidcActionConfig where
  parseXML x =
    AuthenticateOidcActionConfig'
      Prelude.<$> ( x Data..@? "AuthenticationRequestExtraParams"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLMap "entry" "key" "value")
                  )
      Prelude.<*> (x Data..@? "ClientSecret")
      Prelude.<*> (x Data..@? "OnUnauthenticatedRequest")
      Prelude.<*> (x Data..@? "Scope")
      Prelude.<*> (x Data..@? "SessionCookieName")
      Prelude.<*> (x Data..@? "SessionTimeout")
      Prelude.<*> (x Data..@? "UseExistingClientSecret")
      Prelude.<*> (x Data..@ "Issuer")
      Prelude.<*> (x Data..@ "AuthorizationEndpoint")
      Prelude.<*> (x Data..@ "TokenEndpoint")
      Prelude.<*> (x Data..@ "UserInfoEndpoint")
      Prelude.<*> (x Data..@ "ClientId")

instance
  Prelude.Hashable
    AuthenticateOidcActionConfig
  where
  hashWithSalt _salt AuthenticateOidcActionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationRequestExtraParams
      `Prelude.hashWithSalt` clientSecret
      `Prelude.hashWithSalt` onUnauthenticatedRequest
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` sessionCookieName
      `Prelude.hashWithSalt` sessionTimeout
      `Prelude.hashWithSalt` useExistingClientSecret
      `Prelude.hashWithSalt` issuer
      `Prelude.hashWithSalt` authorizationEndpoint
      `Prelude.hashWithSalt` tokenEndpoint
      `Prelude.hashWithSalt` userInfoEndpoint
      `Prelude.hashWithSalt` clientId

instance Prelude.NFData AuthenticateOidcActionConfig where
  rnf AuthenticateOidcActionConfig' {..} =
    Prelude.rnf authenticationRequestExtraParams
      `Prelude.seq` Prelude.rnf clientSecret
      `Prelude.seq` Prelude.rnf onUnauthenticatedRequest
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf sessionCookieName
      `Prelude.seq` Prelude.rnf sessionTimeout
      `Prelude.seq` Prelude.rnf useExistingClientSecret
      `Prelude.seq` Prelude.rnf issuer
      `Prelude.seq` Prelude.rnf authorizationEndpoint
      `Prelude.seq` Prelude.rnf tokenEndpoint
      `Prelude.seq` Prelude.rnf userInfoEndpoint
      `Prelude.seq` Prelude.rnf clientId

instance Data.ToQuery AuthenticateOidcActionConfig where
  toQuery AuthenticateOidcActionConfig' {..} =
    Prelude.mconcat
      [ "AuthenticationRequestExtraParams"
          Data.=: Data.toQuery
            ( Data.toQueryMap "entry" "key" "value"
                Prelude.<$> authenticationRequestExtraParams
            ),
        "ClientSecret" Data.=: clientSecret,
        "OnUnauthenticatedRequest"
          Data.=: onUnauthenticatedRequest,
        "Scope" Data.=: scope,
        "SessionCookieName" Data.=: sessionCookieName,
        "SessionTimeout" Data.=: sessionTimeout,
        "UseExistingClientSecret"
          Data.=: useExistingClientSecret,
        "Issuer" Data.=: issuer,
        "AuthorizationEndpoint"
          Data.=: authorizationEndpoint,
        "TokenEndpoint" Data.=: tokenEndpoint,
        "UserInfoEndpoint" Data.=: userInfoEndpoint,
        "ClientId" Data.=: clientId
      ]
