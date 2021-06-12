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
-- Module      : Network.AWS.ELBv2.Types.AuthenticateCognitoActionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.AuthenticateCognitoActionConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types.AuthenticateCognitoActionConditionalBehaviorEnum
import qualified Network.AWS.Lens as Lens

-- | Request parameters to use when integrating with Amazon Cognito to
-- authenticate users.
--
-- /See:/ 'newAuthenticateCognitoActionConfig' smart constructor.
data AuthenticateCognitoActionConfig = AuthenticateCognitoActionConfig'
  { -- | The maximum duration of the authentication session, in seconds. The
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
    onUnauthenticatedRequest :: Core.Maybe AuthenticateCognitoActionConditionalBehaviorEnum,
    -- | The Amazon Resource Name (ARN) of the Amazon Cognito user pool.
    userPoolArn :: Core.Text,
    -- | The ID of the Amazon Cognito user pool client.
    userPoolClientId :: Core.Text,
    -- | The domain prefix or fully-qualified domain name of the Amazon Cognito
    -- user pool.
    userPoolDomain :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AuthenticateCognitoActionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionTimeout', 'authenticateCognitoActionConfig_sessionTimeout' - The maximum duration of the authentication session, in seconds. The
-- default is 604800 seconds (7 days).
--
-- 'scope', 'authenticateCognitoActionConfig_scope' - The set of user claims to be requested from the IdP. The default is
-- @openid@.
--
-- To verify which scope values your IdP supports and how to separate
-- multiple values, see the documentation for your IdP.
--
-- 'authenticationRequestExtraParams', 'authenticateCognitoActionConfig_authenticationRequestExtraParams' - The query parameters (up to 10) to include in the redirect request to
-- the authorization endpoint.
--
-- 'sessionCookieName', 'authenticateCognitoActionConfig_sessionCookieName' - The name of the cookie used to maintain session information. The default
-- is AWSELBAuthSessionCookie.
--
-- 'onUnauthenticatedRequest', 'authenticateCognitoActionConfig_onUnauthenticatedRequest' - The behavior if the user is not authenticated. The following are
-- possible values:
--
-- -   deny@@ - Return an HTTP 401 Unauthorized error.
--
-- -   allow@@ - Allow the request to be forwarded to the target.
--
-- -   authenticate@@ - Redirect the request to the IdP authorization
--     endpoint. This is the default value.
--
-- 'userPoolArn', 'authenticateCognitoActionConfig_userPoolArn' - The Amazon Resource Name (ARN) of the Amazon Cognito user pool.
--
-- 'userPoolClientId', 'authenticateCognitoActionConfig_userPoolClientId' - The ID of the Amazon Cognito user pool client.
--
-- 'userPoolDomain', 'authenticateCognitoActionConfig_userPoolDomain' - The domain prefix or fully-qualified domain name of the Amazon Cognito
-- user pool.
newAuthenticateCognitoActionConfig ::
  -- | 'userPoolArn'
  Core.Text ->
  -- | 'userPoolClientId'
  Core.Text ->
  -- | 'userPoolDomain'
  Core.Text ->
  AuthenticateCognitoActionConfig
newAuthenticateCognitoActionConfig
  pUserPoolArn_
  pUserPoolClientId_
  pUserPoolDomain_ =
    AuthenticateCognitoActionConfig'
      { sessionTimeout =
          Core.Nothing,
        scope = Core.Nothing,
        authenticationRequestExtraParams =
          Core.Nothing,
        sessionCookieName = Core.Nothing,
        onUnauthenticatedRequest = Core.Nothing,
        userPoolArn = pUserPoolArn_,
        userPoolClientId = pUserPoolClientId_,
        userPoolDomain = pUserPoolDomain_
      }

-- | The maximum duration of the authentication session, in seconds. The
-- default is 604800 seconds (7 days).
authenticateCognitoActionConfig_sessionTimeout :: Lens.Lens' AuthenticateCognitoActionConfig (Core.Maybe Core.Integer)
authenticateCognitoActionConfig_sessionTimeout = Lens.lens (\AuthenticateCognitoActionConfig' {sessionTimeout} -> sessionTimeout) (\s@AuthenticateCognitoActionConfig' {} a -> s {sessionTimeout = a} :: AuthenticateCognitoActionConfig)

-- | The set of user claims to be requested from the IdP. The default is
-- @openid@.
--
-- To verify which scope values your IdP supports and how to separate
-- multiple values, see the documentation for your IdP.
authenticateCognitoActionConfig_scope :: Lens.Lens' AuthenticateCognitoActionConfig (Core.Maybe Core.Text)
authenticateCognitoActionConfig_scope = Lens.lens (\AuthenticateCognitoActionConfig' {scope} -> scope) (\s@AuthenticateCognitoActionConfig' {} a -> s {scope = a} :: AuthenticateCognitoActionConfig)

-- | The query parameters (up to 10) to include in the redirect request to
-- the authorization endpoint.
authenticateCognitoActionConfig_authenticationRequestExtraParams :: Lens.Lens' AuthenticateCognitoActionConfig (Core.Maybe (Core.HashMap Core.Text Core.Text))
authenticateCognitoActionConfig_authenticationRequestExtraParams = Lens.lens (\AuthenticateCognitoActionConfig' {authenticationRequestExtraParams} -> authenticationRequestExtraParams) (\s@AuthenticateCognitoActionConfig' {} a -> s {authenticationRequestExtraParams = a} :: AuthenticateCognitoActionConfig) Core.. Lens.mapping Lens._Coerce

-- | The name of the cookie used to maintain session information. The default
-- is AWSELBAuthSessionCookie.
authenticateCognitoActionConfig_sessionCookieName :: Lens.Lens' AuthenticateCognitoActionConfig (Core.Maybe Core.Text)
authenticateCognitoActionConfig_sessionCookieName = Lens.lens (\AuthenticateCognitoActionConfig' {sessionCookieName} -> sessionCookieName) (\s@AuthenticateCognitoActionConfig' {} a -> s {sessionCookieName = a} :: AuthenticateCognitoActionConfig)

-- | The behavior if the user is not authenticated. The following are
-- possible values:
--
-- -   deny@@ - Return an HTTP 401 Unauthorized error.
--
-- -   allow@@ - Allow the request to be forwarded to the target.
--
-- -   authenticate@@ - Redirect the request to the IdP authorization
--     endpoint. This is the default value.
authenticateCognitoActionConfig_onUnauthenticatedRequest :: Lens.Lens' AuthenticateCognitoActionConfig (Core.Maybe AuthenticateCognitoActionConditionalBehaviorEnum)
authenticateCognitoActionConfig_onUnauthenticatedRequest = Lens.lens (\AuthenticateCognitoActionConfig' {onUnauthenticatedRequest} -> onUnauthenticatedRequest) (\s@AuthenticateCognitoActionConfig' {} a -> s {onUnauthenticatedRequest = a} :: AuthenticateCognitoActionConfig)

-- | The Amazon Resource Name (ARN) of the Amazon Cognito user pool.
authenticateCognitoActionConfig_userPoolArn :: Lens.Lens' AuthenticateCognitoActionConfig Core.Text
authenticateCognitoActionConfig_userPoolArn = Lens.lens (\AuthenticateCognitoActionConfig' {userPoolArn} -> userPoolArn) (\s@AuthenticateCognitoActionConfig' {} a -> s {userPoolArn = a} :: AuthenticateCognitoActionConfig)

-- | The ID of the Amazon Cognito user pool client.
authenticateCognitoActionConfig_userPoolClientId :: Lens.Lens' AuthenticateCognitoActionConfig Core.Text
authenticateCognitoActionConfig_userPoolClientId = Lens.lens (\AuthenticateCognitoActionConfig' {userPoolClientId} -> userPoolClientId) (\s@AuthenticateCognitoActionConfig' {} a -> s {userPoolClientId = a} :: AuthenticateCognitoActionConfig)

-- | The domain prefix or fully-qualified domain name of the Amazon Cognito
-- user pool.
authenticateCognitoActionConfig_userPoolDomain :: Lens.Lens' AuthenticateCognitoActionConfig Core.Text
authenticateCognitoActionConfig_userPoolDomain = Lens.lens (\AuthenticateCognitoActionConfig' {userPoolDomain} -> userPoolDomain) (\s@AuthenticateCognitoActionConfig' {} a -> s {userPoolDomain = a} :: AuthenticateCognitoActionConfig)

instance Core.FromXML AuthenticateCognitoActionConfig where
  parseXML x =
    AuthenticateCognitoActionConfig'
      Core.<$> (x Core..@? "SessionTimeout")
      Core.<*> (x Core..@? "Scope")
      Core.<*> ( x Core..@? "AuthenticationRequestExtraParams"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLMap "entry" "key" "value")
               )
      Core.<*> (x Core..@? "SessionCookieName")
      Core.<*> (x Core..@? "OnUnauthenticatedRequest")
      Core.<*> (x Core..@ "UserPoolArn")
      Core.<*> (x Core..@ "UserPoolClientId")
      Core.<*> (x Core..@ "UserPoolDomain")

instance
  Core.Hashable
    AuthenticateCognitoActionConfig

instance Core.NFData AuthenticateCognitoActionConfig

instance Core.ToQuery AuthenticateCognitoActionConfig where
  toQuery AuthenticateCognitoActionConfig' {..} =
    Core.mconcat
      [ "SessionTimeout" Core.=: sessionTimeout,
        "Scope" Core.=: scope,
        "AuthenticationRequestExtraParams"
          Core.=: Core.toQuery
            ( Core.toQueryMap "entry" "key" "value"
                Core.<$> authenticationRequestExtraParams
            ),
        "SessionCookieName" Core.=: sessionCookieName,
        "OnUnauthenticatedRequest"
          Core.=: onUnauthenticatedRequest,
        "UserPoolArn" Core.=: userPoolArn,
        "UserPoolClientId" Core.=: userPoolClientId,
        "UserPoolDomain" Core.=: userPoolDomain
      ]
