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
-- Module      : Amazonka.ELBV2.Types.AuthenticateCognitoActionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.AuthenticateCognitoActionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types.AuthenticateCognitoActionConditionalBehaviorEnum
import qualified Amazonka.Prelude as Prelude

-- | Request parameters to use when integrating with Amazon Cognito to
-- authenticate users.
--
-- /See:/ 'newAuthenticateCognitoActionConfig' smart constructor.
data AuthenticateCognitoActionConfig = AuthenticateCognitoActionConfig'
  { -- | The query parameters (up to 10) to include in the redirect request to
    -- the authorization endpoint.
    authenticationRequestExtraParams :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The behavior if the user is not authenticated. The following are
    -- possible values:
    --
    -- -   deny - Return an HTTP 401 Unauthorized error.
    --
    -- -   allow - Allow the request to be forwarded to the target.
    --
    -- -   authenticate - Redirect the request to the IdP authorization
    --     endpoint. This is the default value.
    onUnauthenticatedRequest :: Prelude.Maybe AuthenticateCognitoActionConditionalBehaviorEnum,
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
    -- | The Amazon Resource Name (ARN) of the Amazon Cognito user pool.
    userPoolArn :: Prelude.Text,
    -- | The ID of the Amazon Cognito user pool client.
    userPoolClientId :: Prelude.Text,
    -- | The domain prefix or fully-qualified domain name of the Amazon Cognito
    -- user pool.
    userPoolDomain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthenticateCognitoActionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationRequestExtraParams', 'authenticateCognitoActionConfig_authenticationRequestExtraParams' - The query parameters (up to 10) to include in the redirect request to
-- the authorization endpoint.
--
-- 'onUnauthenticatedRequest', 'authenticateCognitoActionConfig_onUnauthenticatedRequest' - The behavior if the user is not authenticated. The following are
-- possible values:
--
-- -   deny - Return an HTTP 401 Unauthorized error.
--
-- -   allow - Allow the request to be forwarded to the target.
--
-- -   authenticate - Redirect the request to the IdP authorization
--     endpoint. This is the default value.
--
-- 'scope', 'authenticateCognitoActionConfig_scope' - The set of user claims to be requested from the IdP. The default is
-- @openid@.
--
-- To verify which scope values your IdP supports and how to separate
-- multiple values, see the documentation for your IdP.
--
-- 'sessionCookieName', 'authenticateCognitoActionConfig_sessionCookieName' - The name of the cookie used to maintain session information. The default
-- is AWSELBAuthSessionCookie.
--
-- 'sessionTimeout', 'authenticateCognitoActionConfig_sessionTimeout' - The maximum duration of the authentication session, in seconds. The
-- default is 604800 seconds (7 days).
--
-- 'userPoolArn', 'authenticateCognitoActionConfig_userPoolArn' - The Amazon Resource Name (ARN) of the Amazon Cognito user pool.
--
-- 'userPoolClientId', 'authenticateCognitoActionConfig_userPoolClientId' - The ID of the Amazon Cognito user pool client.
--
-- 'userPoolDomain', 'authenticateCognitoActionConfig_userPoolDomain' - The domain prefix or fully-qualified domain name of the Amazon Cognito
-- user pool.
newAuthenticateCognitoActionConfig ::
  -- | 'userPoolArn'
  Prelude.Text ->
  -- | 'userPoolClientId'
  Prelude.Text ->
  -- | 'userPoolDomain'
  Prelude.Text ->
  AuthenticateCognitoActionConfig
newAuthenticateCognitoActionConfig
  pUserPoolArn_
  pUserPoolClientId_
  pUserPoolDomain_ =
    AuthenticateCognitoActionConfig'
      { authenticationRequestExtraParams =
          Prelude.Nothing,
        onUnauthenticatedRequest = Prelude.Nothing,
        scope = Prelude.Nothing,
        sessionCookieName = Prelude.Nothing,
        sessionTimeout = Prelude.Nothing,
        userPoolArn = pUserPoolArn_,
        userPoolClientId = pUserPoolClientId_,
        userPoolDomain = pUserPoolDomain_
      }

-- | The query parameters (up to 10) to include in the redirect request to
-- the authorization endpoint.
authenticateCognitoActionConfig_authenticationRequestExtraParams :: Lens.Lens' AuthenticateCognitoActionConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
authenticateCognitoActionConfig_authenticationRequestExtraParams = Lens.lens (\AuthenticateCognitoActionConfig' {authenticationRequestExtraParams} -> authenticationRequestExtraParams) (\s@AuthenticateCognitoActionConfig' {} a -> s {authenticationRequestExtraParams = a} :: AuthenticateCognitoActionConfig) Prelude.. Lens.mapping Lens.coerced

-- | The behavior if the user is not authenticated. The following are
-- possible values:
--
-- -   deny - Return an HTTP 401 Unauthorized error.
--
-- -   allow - Allow the request to be forwarded to the target.
--
-- -   authenticate - Redirect the request to the IdP authorization
--     endpoint. This is the default value.
authenticateCognitoActionConfig_onUnauthenticatedRequest :: Lens.Lens' AuthenticateCognitoActionConfig (Prelude.Maybe AuthenticateCognitoActionConditionalBehaviorEnum)
authenticateCognitoActionConfig_onUnauthenticatedRequest = Lens.lens (\AuthenticateCognitoActionConfig' {onUnauthenticatedRequest} -> onUnauthenticatedRequest) (\s@AuthenticateCognitoActionConfig' {} a -> s {onUnauthenticatedRequest = a} :: AuthenticateCognitoActionConfig)

-- | The set of user claims to be requested from the IdP. The default is
-- @openid@.
--
-- To verify which scope values your IdP supports and how to separate
-- multiple values, see the documentation for your IdP.
authenticateCognitoActionConfig_scope :: Lens.Lens' AuthenticateCognitoActionConfig (Prelude.Maybe Prelude.Text)
authenticateCognitoActionConfig_scope = Lens.lens (\AuthenticateCognitoActionConfig' {scope} -> scope) (\s@AuthenticateCognitoActionConfig' {} a -> s {scope = a} :: AuthenticateCognitoActionConfig)

-- | The name of the cookie used to maintain session information. The default
-- is AWSELBAuthSessionCookie.
authenticateCognitoActionConfig_sessionCookieName :: Lens.Lens' AuthenticateCognitoActionConfig (Prelude.Maybe Prelude.Text)
authenticateCognitoActionConfig_sessionCookieName = Lens.lens (\AuthenticateCognitoActionConfig' {sessionCookieName} -> sessionCookieName) (\s@AuthenticateCognitoActionConfig' {} a -> s {sessionCookieName = a} :: AuthenticateCognitoActionConfig)

-- | The maximum duration of the authentication session, in seconds. The
-- default is 604800 seconds (7 days).
authenticateCognitoActionConfig_sessionTimeout :: Lens.Lens' AuthenticateCognitoActionConfig (Prelude.Maybe Prelude.Integer)
authenticateCognitoActionConfig_sessionTimeout = Lens.lens (\AuthenticateCognitoActionConfig' {sessionTimeout} -> sessionTimeout) (\s@AuthenticateCognitoActionConfig' {} a -> s {sessionTimeout = a} :: AuthenticateCognitoActionConfig)

-- | The Amazon Resource Name (ARN) of the Amazon Cognito user pool.
authenticateCognitoActionConfig_userPoolArn :: Lens.Lens' AuthenticateCognitoActionConfig Prelude.Text
authenticateCognitoActionConfig_userPoolArn = Lens.lens (\AuthenticateCognitoActionConfig' {userPoolArn} -> userPoolArn) (\s@AuthenticateCognitoActionConfig' {} a -> s {userPoolArn = a} :: AuthenticateCognitoActionConfig)

-- | The ID of the Amazon Cognito user pool client.
authenticateCognitoActionConfig_userPoolClientId :: Lens.Lens' AuthenticateCognitoActionConfig Prelude.Text
authenticateCognitoActionConfig_userPoolClientId = Lens.lens (\AuthenticateCognitoActionConfig' {userPoolClientId} -> userPoolClientId) (\s@AuthenticateCognitoActionConfig' {} a -> s {userPoolClientId = a} :: AuthenticateCognitoActionConfig)

-- | The domain prefix or fully-qualified domain name of the Amazon Cognito
-- user pool.
authenticateCognitoActionConfig_userPoolDomain :: Lens.Lens' AuthenticateCognitoActionConfig Prelude.Text
authenticateCognitoActionConfig_userPoolDomain = Lens.lens (\AuthenticateCognitoActionConfig' {userPoolDomain} -> userPoolDomain) (\s@AuthenticateCognitoActionConfig' {} a -> s {userPoolDomain = a} :: AuthenticateCognitoActionConfig)

instance Data.FromXML AuthenticateCognitoActionConfig where
  parseXML x =
    AuthenticateCognitoActionConfig'
      Prelude.<$> ( x
                      Data..@? "AuthenticationRequestExtraParams"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLMap "entry" "key" "value")
                  )
      Prelude.<*> (x Data..@? "OnUnauthenticatedRequest")
      Prelude.<*> (x Data..@? "Scope")
      Prelude.<*> (x Data..@? "SessionCookieName")
      Prelude.<*> (x Data..@? "SessionTimeout")
      Prelude.<*> (x Data..@ "UserPoolArn")
      Prelude.<*> (x Data..@ "UserPoolClientId")
      Prelude.<*> (x Data..@ "UserPoolDomain")

instance
  Prelude.Hashable
    AuthenticateCognitoActionConfig
  where
  hashWithSalt
    _salt
    AuthenticateCognitoActionConfig' {..} =
      _salt
        `Prelude.hashWithSalt` authenticationRequestExtraParams
        `Prelude.hashWithSalt` onUnauthenticatedRequest
        `Prelude.hashWithSalt` scope
        `Prelude.hashWithSalt` sessionCookieName
        `Prelude.hashWithSalt` sessionTimeout
        `Prelude.hashWithSalt` userPoolArn
        `Prelude.hashWithSalt` userPoolClientId
        `Prelude.hashWithSalt` userPoolDomain

instance
  Prelude.NFData
    AuthenticateCognitoActionConfig
  where
  rnf AuthenticateCognitoActionConfig' {..} =
    Prelude.rnf authenticationRequestExtraParams
      `Prelude.seq` Prelude.rnf onUnauthenticatedRequest
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf sessionCookieName
      `Prelude.seq` Prelude.rnf sessionTimeout
      `Prelude.seq` Prelude.rnf userPoolArn
      `Prelude.seq` Prelude.rnf userPoolClientId
      `Prelude.seq` Prelude.rnf userPoolDomain

instance Data.ToQuery AuthenticateCognitoActionConfig where
  toQuery AuthenticateCognitoActionConfig' {..} =
    Prelude.mconcat
      [ "AuthenticationRequestExtraParams"
          Data.=: Data.toQuery
            ( Data.toQueryMap "entry" "key" "value"
                Prelude.<$> authenticationRequestExtraParams
            ),
        "OnUnauthenticatedRequest"
          Data.=: onUnauthenticatedRequest,
        "Scope" Data.=: scope,
        "SessionCookieName" Data.=: sessionCookieName,
        "SessionTimeout" Data.=: sessionTimeout,
        "UserPoolArn" Data.=: userPoolArn,
        "UserPoolClientId" Data.=: userPoolClientId,
        "UserPoolDomain" Data.=: userPoolDomain
      ]
