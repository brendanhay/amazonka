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
-- Module      : Amazonka.CognitoIdentityProvider.Types.UserPoolClientType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.UserPoolClientType where

import Amazonka.CognitoIdentityProvider.Types.AnalyticsConfigurationType
import Amazonka.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
import Amazonka.CognitoIdentityProvider.Types.OAuthFlowType
import Amazonka.CognitoIdentityProvider.Types.PreventUserExistenceErrorTypes
import Amazonka.CognitoIdentityProvider.Types.TokenValidityUnitsType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a user pool client.
--
-- /See:/ 'newUserPoolClientType' smart constructor.
data UserPoolClientType = UserPoolClientType'
  { -- | The access token time limit. After this limit expires, your user can\'t
    -- use their access token. To specify the time unit for
    -- @AccessTokenValidity@ as @seconds@, @minutes@, @hours@, or @days@, set a
    -- @TokenValidityUnits@ value in your API request.
    --
    -- For example, when you set @AccessTokenValidity@ to @10@ and
    -- @TokenValidityUnits@ to @hours@, your user can authorize access with
    -- their access token for 10 hours.
    --
    -- The default time unit for @AccessTokenValidity@ in an API request is
    -- hours. /Valid range/ is displayed below in seconds.
    --
    -- If you don\'t specify otherwise in the configuration of your app client,
    -- your access tokens are valid for one hour.
    accessTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | The allowed OAuth flows.
    --
    -- [code]
    --     Use a code grant flow, which provides an authorization code as the
    --     response. This code can be exchanged for access tokens with the
    --     @\/oauth2\/token@ endpoint.
    --
    -- [implicit]
    --     Issue the access token (and, optionally, ID token, based on scopes)
    --     directly to your user.
    --
    -- [client_credentials]
    --     Issue the access token from the @\/oauth2\/token@ endpoint directly
    --     to a non-person user using a combination of the client ID and client
    --     secret.
    allowedOAuthFlows :: Prelude.Maybe [OAuthFlowType],
    -- | Set to true if the client is allowed to follow the OAuth protocol when
    -- interacting with Amazon Cognito user pools.
    allowedOAuthFlowsUserPoolClient :: Prelude.Maybe Prelude.Bool,
    -- | The OAuth scopes that your app client supports. Possible values that
    -- OAuth provides are @phone@, @email@, @openid@, and @profile@. Possible
    -- values that Amazon Web Services provides are
    -- @aws.cognito.signin.user.admin@. Amazon Cognito also supports custom
    -- scopes that you create in Resource Servers.
    allowedOAuthScopes :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Pinpoint analytics configuration for the user pool client.
    --
    -- Amazon Cognito user pools only support sending events to Amazon Pinpoint
    -- projects in the US East (N. Virginia) us-east-1 Region, regardless of
    -- the Region where the user pool resides.
    analyticsConfiguration :: Prelude.Maybe AnalyticsConfigurationType,
    -- | Amazon Cognito creates a session token for each API request in an
    -- authentication flow. @AuthSessionValidity@ is the duration, in minutes,
    -- of that session token. Your user pool native user must respond to each
    -- authentication challenge before the session expires.
    authSessionValidity :: Prelude.Maybe Prelude.Natural,
    -- | A list of allowed redirect (callback) URLs for the IdPs.
    --
    -- A redirect URI must:
    --
    -- -   Be an absolute URI.
    --
    -- -   Be registered with the authorization server.
    --
    -- -   Not include a fragment component.
    --
    -- See
    -- <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint>.
    --
    -- Amazon Cognito requires HTTPS over HTTP except for http:\/\/localhost
    -- for testing purposes only.
    --
    -- App callback URLs such as myapp:\/\/example are also supported.
    callbackURLs :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the client associated with the user pool.
    clientId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The client name from the user pool request of the client type.
    clientName :: Prelude.Maybe Prelude.Text,
    -- | The client secret from the user pool request of the client type.
    clientSecret :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The date the user pool client was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The default redirect URI. Must be in the @CallbackURLs@ list.
    --
    -- A redirect URI must:
    --
    -- -   Be an absolute URI.
    --
    -- -   Be registered with the authorization server.
    --
    -- -   Not include a fragment component.
    --
    -- See
    -- <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint>.
    --
    -- Amazon Cognito requires HTTPS over HTTP except for http:\/\/localhost
    -- for testing purposes only.
    --
    -- App callback URLs such as myapp:\/\/example are also supported.
    defaultRedirectURI :: Prelude.Maybe Prelude.Text,
    -- | When @EnablePropagateAdditionalUserContextData@ is true, Amazon Cognito
    -- accepts an @IpAddress@ value that you send in the @UserContextData@
    -- parameter. The @UserContextData@ parameter sends information to Amazon
    -- Cognito advanced security for risk analysis. You can send
    -- @UserContextData@ when you sign in Amazon Cognito native users with the
    -- @InitiateAuth@ and @RespondToAuthChallenge@ API operations.
    --
    -- When @EnablePropagateAdditionalUserContextData@ is false, you can\'t
    -- send your user\'s source IP address to Amazon Cognito advanced security
    -- with unauthenticated API operations.
    -- @EnablePropagateAdditionalUserContextData@ doesn\'t affect whether you
    -- can send a source IP address in a @ContextData@ parameter with the
    -- authenticated API operations @AdminInitiateAuth@ and
    -- @AdminRespondToAuthChallenge@.
    --
    -- You can only activate @EnablePropagateAdditionalUserContextData@ in an
    -- app client that has a client secret. For more information about
    -- propagation of user context data, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-adaptive-authentication.html#user-pool-settings-adaptive-authentication-device-fingerprint Adding user device and session data to API requests>.
    enablePropagateAdditionalUserContextData :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether token revocation is activated for the user pool
    -- client. When you create a new user pool client, token revocation is
    -- activated by default. For more information about revoking tokens, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
    enableTokenRevocation :: Prelude.Maybe Prelude.Bool,
    -- | The authentication flows that you want your user pool client to support.
    -- For each app client in your user pool, you can sign in your users with
    -- any combination of one or more flows, including with a user name and
    -- Secure Remote Password (SRP), a user name and password, or a custom
    -- authentication process that you define with Lambda functions.
    --
    -- If you don\'t specify a value for @ExplicitAuthFlows@, your user client
    -- supports @ALLOW_REFRESH_TOKEN_AUTH@, @ALLOW_USER_SRP_AUTH@, and
    -- @ALLOW_CUSTOM_AUTH@.
    --
    -- Valid values include:
    --
    -- -   @ALLOW_ADMIN_USER_PASSWORD_AUTH@: Enable admin based user password
    --     authentication flow @ADMIN_USER_PASSWORD_AUTH@. This setting
    --     replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication
    --     flow, your app passes a user name and password to Amazon Cognito in
    --     the request, instead of using the Secure Remote Password (SRP)
    --     protocol to securely transmit the password.
    --
    -- -   @ALLOW_CUSTOM_AUTH@: Enable Lambda trigger based authentication.
    --
    -- -   @ALLOW_USER_PASSWORD_AUTH@: Enable user password-based
    --     authentication. In this flow, Amazon Cognito receives the password
    --     in the request instead of using the SRP protocol to verify
    --     passwords.
    --
    -- -   @ALLOW_USER_SRP_AUTH@: Enable SRP-based authentication.
    --
    -- -   @ALLOW_REFRESH_TOKEN_AUTH@: Enable authflow to refresh tokens.
    --
    -- In some environments, you will see the values @ADMIN_NO_SRP_AUTH@,
    -- @CUSTOM_AUTH_FLOW_ONLY@, or @USER_PASSWORD_AUTH@. You can\'t assign
    -- these legacy @ExplicitAuthFlows@ values to user pool clients at the same
    -- time as values that begin with @ALLOW_@, like @ALLOW_USER_SRP_AUTH@.
    explicitAuthFlows :: Prelude.Maybe [ExplicitAuthFlowsType],
    -- | The ID token time limit. After this limit expires, your user can\'t use
    -- their ID token. To specify the time unit for @IdTokenValidity@ as
    -- @seconds@, @minutes@, @hours@, or @days@, set a @TokenValidityUnits@
    -- value in your API request.
    --
    -- For example, when you set @IdTokenValidity@ as @10@ and
    -- @TokenValidityUnits@ as @hours@, your user can authenticate their
    -- session with their ID token for 10 hours.
    --
    -- The default time unit for @AccessTokenValidity@ in an API request is
    -- hours. /Valid range/ is displayed below in seconds.
    --
    -- If you don\'t specify otherwise in the configuration of your app client,
    -- your ID tokens are valid for one hour.
    idTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | The date the user pool client was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | A list of allowed logout URLs for the IdPs.
    logoutURLs :: Prelude.Maybe [Prelude.Text],
    -- | Errors and responses that you want Amazon Cognito APIs to return during
    -- authentication, account confirmation, and password recovery when the
    -- user doesn\'t exist in the user pool. When set to @ENABLED@ and the user
    -- doesn\'t exist, authentication returns an error indicating either the
    -- username or password was incorrect. Account confirmation and password
    -- recovery return a response indicating a code was sent to a simulated
    -- destination. When set to @LEGACY@, those APIs return a
    -- @UserNotFoundException@ exception if the user doesn\'t exist in the user
    -- pool.
    --
    -- Valid values include:
    --
    -- -   @ENABLED@ - This prevents user existence-related errors.
    --
    -- -   @LEGACY@ - This represents the old behavior of Amazon Cognito where
    --     user existence related errors aren\'t prevented.
    preventUserExistenceErrors :: Prelude.Maybe PreventUserExistenceErrorTypes,
    -- | The Read-only attributes.
    readAttributes :: Prelude.Maybe [Prelude.Text],
    -- | The refresh token time limit. After this limit expires, your user can\'t
    -- use their refresh token. To specify the time unit for
    -- @RefreshTokenValidity@ as @seconds@, @minutes@, @hours@, or @days@, set
    -- a @TokenValidityUnits@ value in your API request.
    --
    -- For example, when you set @RefreshTokenValidity@ as @10@ and
    -- @TokenValidityUnits@ as @days@, your user can refresh their session and
    -- retrieve new access and ID tokens for 10 days.
    --
    -- The default time unit for @RefreshTokenValidity@ in an API request is
    -- days. You can\'t set @RefreshTokenValidity@ to 0. If you do, Amazon
    -- Cognito overrides the value with the default value of 30 days. /Valid
    -- range/ is displayed below in seconds.
    --
    -- If you don\'t specify otherwise in the configuration of your app client,
    -- your refresh tokens are valid for 30 days.
    refreshTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | A list of provider names for the IdPs that this client supports. The
    -- following are supported: @COGNITO@, @Facebook@, @Google@,
    -- @SignInWithApple@, @LoginWithAmazon@, and the names of your own SAML and
    -- OIDC providers.
    supportedIdentityProviders :: Prelude.Maybe [Prelude.Text],
    -- | The time units used to specify the token validity times of each token
    -- type: ID, access, and refresh.
    tokenValidityUnits :: Prelude.Maybe TokenValidityUnitsType,
    -- | The user pool ID for the user pool client.
    userPoolId :: Prelude.Maybe Prelude.Text,
    -- | The writeable attributes.
    writeAttributes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserPoolClientType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessTokenValidity', 'userPoolClientType_accessTokenValidity' - The access token time limit. After this limit expires, your user can\'t
-- use their access token. To specify the time unit for
-- @AccessTokenValidity@ as @seconds@, @minutes@, @hours@, or @days@, set a
-- @TokenValidityUnits@ value in your API request.
--
-- For example, when you set @AccessTokenValidity@ to @10@ and
-- @TokenValidityUnits@ to @hours@, your user can authorize access with
-- their access token for 10 hours.
--
-- The default time unit for @AccessTokenValidity@ in an API request is
-- hours. /Valid range/ is displayed below in seconds.
--
-- If you don\'t specify otherwise in the configuration of your app client,
-- your access tokens are valid for one hour.
--
-- 'allowedOAuthFlows', 'userPoolClientType_allowedOAuthFlows' - The allowed OAuth flows.
--
-- [code]
--     Use a code grant flow, which provides an authorization code as the
--     response. This code can be exchanged for access tokens with the
--     @\/oauth2\/token@ endpoint.
--
-- [implicit]
--     Issue the access token (and, optionally, ID token, based on scopes)
--     directly to your user.
--
-- [client_credentials]
--     Issue the access token from the @\/oauth2\/token@ endpoint directly
--     to a non-person user using a combination of the client ID and client
--     secret.
--
-- 'allowedOAuthFlowsUserPoolClient', 'userPoolClientType_allowedOAuthFlowsUserPoolClient' - Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Amazon Cognito user pools.
--
-- 'allowedOAuthScopes', 'userPoolClientType_allowedOAuthScopes' - The OAuth scopes that your app client supports. Possible values that
-- OAuth provides are @phone@, @email@, @openid@, and @profile@. Possible
-- values that Amazon Web Services provides are
-- @aws.cognito.signin.user.admin@. Amazon Cognito also supports custom
-- scopes that you create in Resource Servers.
--
-- 'analyticsConfiguration', 'userPoolClientType_analyticsConfiguration' - The Amazon Pinpoint analytics configuration for the user pool client.
--
-- Amazon Cognito user pools only support sending events to Amazon Pinpoint
-- projects in the US East (N. Virginia) us-east-1 Region, regardless of
-- the Region where the user pool resides.
--
-- 'authSessionValidity', 'userPoolClientType_authSessionValidity' - Amazon Cognito creates a session token for each API request in an
-- authentication flow. @AuthSessionValidity@ is the duration, in minutes,
-- of that session token. Your user pool native user must respond to each
-- authentication challenge before the session expires.
--
-- 'callbackURLs', 'userPoolClientType_callbackURLs' - A list of allowed redirect (callback) URLs for the IdPs.
--
-- A redirect URI must:
--
-- -   Be an absolute URI.
--
-- -   Be registered with the authorization server.
--
-- -   Not include a fragment component.
--
-- See
-- <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint>.
--
-- Amazon Cognito requires HTTPS over HTTP except for http:\/\/localhost
-- for testing purposes only.
--
-- App callback URLs such as myapp:\/\/example are also supported.
--
-- 'clientId', 'userPoolClientType_clientId' - The ID of the client associated with the user pool.
--
-- 'clientName', 'userPoolClientType_clientName' - The client name from the user pool request of the client type.
--
-- 'clientSecret', 'userPoolClientType_clientSecret' - The client secret from the user pool request of the client type.
--
-- 'creationDate', 'userPoolClientType_creationDate' - The date the user pool client was created.
--
-- 'defaultRedirectURI', 'userPoolClientType_defaultRedirectURI' - The default redirect URI. Must be in the @CallbackURLs@ list.
--
-- A redirect URI must:
--
-- -   Be an absolute URI.
--
-- -   Be registered with the authorization server.
--
-- -   Not include a fragment component.
--
-- See
-- <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint>.
--
-- Amazon Cognito requires HTTPS over HTTP except for http:\/\/localhost
-- for testing purposes only.
--
-- App callback URLs such as myapp:\/\/example are also supported.
--
-- 'enablePropagateAdditionalUserContextData', 'userPoolClientType_enablePropagateAdditionalUserContextData' - When @EnablePropagateAdditionalUserContextData@ is true, Amazon Cognito
-- accepts an @IpAddress@ value that you send in the @UserContextData@
-- parameter. The @UserContextData@ parameter sends information to Amazon
-- Cognito advanced security for risk analysis. You can send
-- @UserContextData@ when you sign in Amazon Cognito native users with the
-- @InitiateAuth@ and @RespondToAuthChallenge@ API operations.
--
-- When @EnablePropagateAdditionalUserContextData@ is false, you can\'t
-- send your user\'s source IP address to Amazon Cognito advanced security
-- with unauthenticated API operations.
-- @EnablePropagateAdditionalUserContextData@ doesn\'t affect whether you
-- can send a source IP address in a @ContextData@ parameter with the
-- authenticated API operations @AdminInitiateAuth@ and
-- @AdminRespondToAuthChallenge@.
--
-- You can only activate @EnablePropagateAdditionalUserContextData@ in an
-- app client that has a client secret. For more information about
-- propagation of user context data, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-adaptive-authentication.html#user-pool-settings-adaptive-authentication-device-fingerprint Adding user device and session data to API requests>.
--
-- 'enableTokenRevocation', 'userPoolClientType_enableTokenRevocation' - Indicates whether token revocation is activated for the user pool
-- client. When you create a new user pool client, token revocation is
-- activated by default. For more information about revoking tokens, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
--
-- 'explicitAuthFlows', 'userPoolClientType_explicitAuthFlows' - The authentication flows that you want your user pool client to support.
-- For each app client in your user pool, you can sign in your users with
-- any combination of one or more flows, including with a user name and
-- Secure Remote Password (SRP), a user name and password, or a custom
-- authentication process that you define with Lambda functions.
--
-- If you don\'t specify a value for @ExplicitAuthFlows@, your user client
-- supports @ALLOW_REFRESH_TOKEN_AUTH@, @ALLOW_USER_SRP_AUTH@, and
-- @ALLOW_CUSTOM_AUTH@.
--
-- Valid values include:
--
-- -   @ALLOW_ADMIN_USER_PASSWORD_AUTH@: Enable admin based user password
--     authentication flow @ADMIN_USER_PASSWORD_AUTH@. This setting
--     replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication
--     flow, your app passes a user name and password to Amazon Cognito in
--     the request, instead of using the Secure Remote Password (SRP)
--     protocol to securely transmit the password.
--
-- -   @ALLOW_CUSTOM_AUTH@: Enable Lambda trigger based authentication.
--
-- -   @ALLOW_USER_PASSWORD_AUTH@: Enable user password-based
--     authentication. In this flow, Amazon Cognito receives the password
--     in the request instead of using the SRP protocol to verify
--     passwords.
--
-- -   @ALLOW_USER_SRP_AUTH@: Enable SRP-based authentication.
--
-- -   @ALLOW_REFRESH_TOKEN_AUTH@: Enable authflow to refresh tokens.
--
-- In some environments, you will see the values @ADMIN_NO_SRP_AUTH@,
-- @CUSTOM_AUTH_FLOW_ONLY@, or @USER_PASSWORD_AUTH@. You can\'t assign
-- these legacy @ExplicitAuthFlows@ values to user pool clients at the same
-- time as values that begin with @ALLOW_@, like @ALLOW_USER_SRP_AUTH@.
--
-- 'idTokenValidity', 'userPoolClientType_idTokenValidity' - The ID token time limit. After this limit expires, your user can\'t use
-- their ID token. To specify the time unit for @IdTokenValidity@ as
-- @seconds@, @minutes@, @hours@, or @days@, set a @TokenValidityUnits@
-- value in your API request.
--
-- For example, when you set @IdTokenValidity@ as @10@ and
-- @TokenValidityUnits@ as @hours@, your user can authenticate their
-- session with their ID token for 10 hours.
--
-- The default time unit for @AccessTokenValidity@ in an API request is
-- hours. /Valid range/ is displayed below in seconds.
--
-- If you don\'t specify otherwise in the configuration of your app client,
-- your ID tokens are valid for one hour.
--
-- 'lastModifiedDate', 'userPoolClientType_lastModifiedDate' - The date the user pool client was last modified.
--
-- 'logoutURLs', 'userPoolClientType_logoutURLs' - A list of allowed logout URLs for the IdPs.
--
-- 'preventUserExistenceErrors', 'userPoolClientType_preventUserExistenceErrors' - Errors and responses that you want Amazon Cognito APIs to return during
-- authentication, account confirmation, and password recovery when the
-- user doesn\'t exist in the user pool. When set to @ENABLED@ and the user
-- doesn\'t exist, authentication returns an error indicating either the
-- username or password was incorrect. Account confirmation and password
-- recovery return a response indicating a code was sent to a simulated
-- destination. When set to @LEGACY@, those APIs return a
-- @UserNotFoundException@ exception if the user doesn\'t exist in the user
-- pool.
--
-- Valid values include:
--
-- -   @ENABLED@ - This prevents user existence-related errors.
--
-- -   @LEGACY@ - This represents the old behavior of Amazon Cognito where
--     user existence related errors aren\'t prevented.
--
-- 'readAttributes', 'userPoolClientType_readAttributes' - The Read-only attributes.
--
-- 'refreshTokenValidity', 'userPoolClientType_refreshTokenValidity' - The refresh token time limit. After this limit expires, your user can\'t
-- use their refresh token. To specify the time unit for
-- @RefreshTokenValidity@ as @seconds@, @minutes@, @hours@, or @days@, set
-- a @TokenValidityUnits@ value in your API request.
--
-- For example, when you set @RefreshTokenValidity@ as @10@ and
-- @TokenValidityUnits@ as @days@, your user can refresh their session and
-- retrieve new access and ID tokens for 10 days.
--
-- The default time unit for @RefreshTokenValidity@ in an API request is
-- days. You can\'t set @RefreshTokenValidity@ to 0. If you do, Amazon
-- Cognito overrides the value with the default value of 30 days. /Valid
-- range/ is displayed below in seconds.
--
-- If you don\'t specify otherwise in the configuration of your app client,
-- your refresh tokens are valid for 30 days.
--
-- 'supportedIdentityProviders', 'userPoolClientType_supportedIdentityProviders' - A list of provider names for the IdPs that this client supports. The
-- following are supported: @COGNITO@, @Facebook@, @Google@,
-- @SignInWithApple@, @LoginWithAmazon@, and the names of your own SAML and
-- OIDC providers.
--
-- 'tokenValidityUnits', 'userPoolClientType_tokenValidityUnits' - The time units used to specify the token validity times of each token
-- type: ID, access, and refresh.
--
-- 'userPoolId', 'userPoolClientType_userPoolId' - The user pool ID for the user pool client.
--
-- 'writeAttributes', 'userPoolClientType_writeAttributes' - The writeable attributes.
newUserPoolClientType ::
  UserPoolClientType
newUserPoolClientType =
  UserPoolClientType'
    { accessTokenValidity =
        Prelude.Nothing,
      allowedOAuthFlows = Prelude.Nothing,
      allowedOAuthFlowsUserPoolClient = Prelude.Nothing,
      allowedOAuthScopes = Prelude.Nothing,
      analyticsConfiguration = Prelude.Nothing,
      authSessionValidity = Prelude.Nothing,
      callbackURLs = Prelude.Nothing,
      clientId = Prelude.Nothing,
      clientName = Prelude.Nothing,
      clientSecret = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      defaultRedirectURI = Prelude.Nothing,
      enablePropagateAdditionalUserContextData =
        Prelude.Nothing,
      enableTokenRevocation = Prelude.Nothing,
      explicitAuthFlows = Prelude.Nothing,
      idTokenValidity = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      logoutURLs = Prelude.Nothing,
      preventUserExistenceErrors = Prelude.Nothing,
      readAttributes = Prelude.Nothing,
      refreshTokenValidity = Prelude.Nothing,
      supportedIdentityProviders = Prelude.Nothing,
      tokenValidityUnits = Prelude.Nothing,
      userPoolId = Prelude.Nothing,
      writeAttributes = Prelude.Nothing
    }

-- | The access token time limit. After this limit expires, your user can\'t
-- use their access token. To specify the time unit for
-- @AccessTokenValidity@ as @seconds@, @minutes@, @hours@, or @days@, set a
-- @TokenValidityUnits@ value in your API request.
--
-- For example, when you set @AccessTokenValidity@ to @10@ and
-- @TokenValidityUnits@ to @hours@, your user can authorize access with
-- their access token for 10 hours.
--
-- The default time unit for @AccessTokenValidity@ in an API request is
-- hours. /Valid range/ is displayed below in seconds.
--
-- If you don\'t specify otherwise in the configuration of your app client,
-- your access tokens are valid for one hour.
userPoolClientType_accessTokenValidity :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Natural)
userPoolClientType_accessTokenValidity = Lens.lens (\UserPoolClientType' {accessTokenValidity} -> accessTokenValidity) (\s@UserPoolClientType' {} a -> s {accessTokenValidity = a} :: UserPoolClientType)

-- | The allowed OAuth flows.
--
-- [code]
--     Use a code grant flow, which provides an authorization code as the
--     response. This code can be exchanged for access tokens with the
--     @\/oauth2\/token@ endpoint.
--
-- [implicit]
--     Issue the access token (and, optionally, ID token, based on scopes)
--     directly to your user.
--
-- [client_credentials]
--     Issue the access token from the @\/oauth2\/token@ endpoint directly
--     to a non-person user using a combination of the client ID and client
--     secret.
userPoolClientType_allowedOAuthFlows :: Lens.Lens' UserPoolClientType (Prelude.Maybe [OAuthFlowType])
userPoolClientType_allowedOAuthFlows = Lens.lens (\UserPoolClientType' {allowedOAuthFlows} -> allowedOAuthFlows) (\s@UserPoolClientType' {} a -> s {allowedOAuthFlows = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens.coerced

-- | Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Amazon Cognito user pools.
userPoolClientType_allowedOAuthFlowsUserPoolClient :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Bool)
userPoolClientType_allowedOAuthFlowsUserPoolClient = Lens.lens (\UserPoolClientType' {allowedOAuthFlowsUserPoolClient} -> allowedOAuthFlowsUserPoolClient) (\s@UserPoolClientType' {} a -> s {allowedOAuthFlowsUserPoolClient = a} :: UserPoolClientType)

-- | The OAuth scopes that your app client supports. Possible values that
-- OAuth provides are @phone@, @email@, @openid@, and @profile@. Possible
-- values that Amazon Web Services provides are
-- @aws.cognito.signin.user.admin@. Amazon Cognito also supports custom
-- scopes that you create in Resource Servers.
userPoolClientType_allowedOAuthScopes :: Lens.Lens' UserPoolClientType (Prelude.Maybe [Prelude.Text])
userPoolClientType_allowedOAuthScopes = Lens.lens (\UserPoolClientType' {allowedOAuthScopes} -> allowedOAuthScopes) (\s@UserPoolClientType' {} a -> s {allowedOAuthScopes = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Pinpoint analytics configuration for the user pool client.
--
-- Amazon Cognito user pools only support sending events to Amazon Pinpoint
-- projects in the US East (N. Virginia) us-east-1 Region, regardless of
-- the Region where the user pool resides.
userPoolClientType_analyticsConfiguration :: Lens.Lens' UserPoolClientType (Prelude.Maybe AnalyticsConfigurationType)
userPoolClientType_analyticsConfiguration = Lens.lens (\UserPoolClientType' {analyticsConfiguration} -> analyticsConfiguration) (\s@UserPoolClientType' {} a -> s {analyticsConfiguration = a} :: UserPoolClientType)

-- | Amazon Cognito creates a session token for each API request in an
-- authentication flow. @AuthSessionValidity@ is the duration, in minutes,
-- of that session token. Your user pool native user must respond to each
-- authentication challenge before the session expires.
userPoolClientType_authSessionValidity :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Natural)
userPoolClientType_authSessionValidity = Lens.lens (\UserPoolClientType' {authSessionValidity} -> authSessionValidity) (\s@UserPoolClientType' {} a -> s {authSessionValidity = a} :: UserPoolClientType)

-- | A list of allowed redirect (callback) URLs for the IdPs.
--
-- A redirect URI must:
--
-- -   Be an absolute URI.
--
-- -   Be registered with the authorization server.
--
-- -   Not include a fragment component.
--
-- See
-- <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint>.
--
-- Amazon Cognito requires HTTPS over HTTP except for http:\/\/localhost
-- for testing purposes only.
--
-- App callback URLs such as myapp:\/\/example are also supported.
userPoolClientType_callbackURLs :: Lens.Lens' UserPoolClientType (Prelude.Maybe [Prelude.Text])
userPoolClientType_callbackURLs = Lens.lens (\UserPoolClientType' {callbackURLs} -> callbackURLs) (\s@UserPoolClientType' {} a -> s {callbackURLs = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the client associated with the user pool.
userPoolClientType_clientId :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Text)
userPoolClientType_clientId = Lens.lens (\UserPoolClientType' {clientId} -> clientId) (\s@UserPoolClientType' {} a -> s {clientId = a} :: UserPoolClientType) Prelude.. Lens.mapping Data._Sensitive

-- | The client name from the user pool request of the client type.
userPoolClientType_clientName :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Text)
userPoolClientType_clientName = Lens.lens (\UserPoolClientType' {clientName} -> clientName) (\s@UserPoolClientType' {} a -> s {clientName = a} :: UserPoolClientType)

-- | The client secret from the user pool request of the client type.
userPoolClientType_clientSecret :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Text)
userPoolClientType_clientSecret = Lens.lens (\UserPoolClientType' {clientSecret} -> clientSecret) (\s@UserPoolClientType' {} a -> s {clientSecret = a} :: UserPoolClientType) Prelude.. Lens.mapping Data._Sensitive

-- | The date the user pool client was created.
userPoolClientType_creationDate :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.UTCTime)
userPoolClientType_creationDate = Lens.lens (\UserPoolClientType' {creationDate} -> creationDate) (\s@UserPoolClientType' {} a -> s {creationDate = a} :: UserPoolClientType) Prelude.. Lens.mapping Data._Time

-- | The default redirect URI. Must be in the @CallbackURLs@ list.
--
-- A redirect URI must:
--
-- -   Be an absolute URI.
--
-- -   Be registered with the authorization server.
--
-- -   Not include a fragment component.
--
-- See
-- <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint>.
--
-- Amazon Cognito requires HTTPS over HTTP except for http:\/\/localhost
-- for testing purposes only.
--
-- App callback URLs such as myapp:\/\/example are also supported.
userPoolClientType_defaultRedirectURI :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Text)
userPoolClientType_defaultRedirectURI = Lens.lens (\UserPoolClientType' {defaultRedirectURI} -> defaultRedirectURI) (\s@UserPoolClientType' {} a -> s {defaultRedirectURI = a} :: UserPoolClientType)

-- | When @EnablePropagateAdditionalUserContextData@ is true, Amazon Cognito
-- accepts an @IpAddress@ value that you send in the @UserContextData@
-- parameter. The @UserContextData@ parameter sends information to Amazon
-- Cognito advanced security for risk analysis. You can send
-- @UserContextData@ when you sign in Amazon Cognito native users with the
-- @InitiateAuth@ and @RespondToAuthChallenge@ API operations.
--
-- When @EnablePropagateAdditionalUserContextData@ is false, you can\'t
-- send your user\'s source IP address to Amazon Cognito advanced security
-- with unauthenticated API operations.
-- @EnablePropagateAdditionalUserContextData@ doesn\'t affect whether you
-- can send a source IP address in a @ContextData@ parameter with the
-- authenticated API operations @AdminInitiateAuth@ and
-- @AdminRespondToAuthChallenge@.
--
-- You can only activate @EnablePropagateAdditionalUserContextData@ in an
-- app client that has a client secret. For more information about
-- propagation of user context data, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-adaptive-authentication.html#user-pool-settings-adaptive-authentication-device-fingerprint Adding user device and session data to API requests>.
userPoolClientType_enablePropagateAdditionalUserContextData :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Bool)
userPoolClientType_enablePropagateAdditionalUserContextData = Lens.lens (\UserPoolClientType' {enablePropagateAdditionalUserContextData} -> enablePropagateAdditionalUserContextData) (\s@UserPoolClientType' {} a -> s {enablePropagateAdditionalUserContextData = a} :: UserPoolClientType)

-- | Indicates whether token revocation is activated for the user pool
-- client. When you create a new user pool client, token revocation is
-- activated by default. For more information about revoking tokens, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
userPoolClientType_enableTokenRevocation :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Bool)
userPoolClientType_enableTokenRevocation = Lens.lens (\UserPoolClientType' {enableTokenRevocation} -> enableTokenRevocation) (\s@UserPoolClientType' {} a -> s {enableTokenRevocation = a} :: UserPoolClientType)

-- | The authentication flows that you want your user pool client to support.
-- For each app client in your user pool, you can sign in your users with
-- any combination of one or more flows, including with a user name and
-- Secure Remote Password (SRP), a user name and password, or a custom
-- authentication process that you define with Lambda functions.
--
-- If you don\'t specify a value for @ExplicitAuthFlows@, your user client
-- supports @ALLOW_REFRESH_TOKEN_AUTH@, @ALLOW_USER_SRP_AUTH@, and
-- @ALLOW_CUSTOM_AUTH@.
--
-- Valid values include:
--
-- -   @ALLOW_ADMIN_USER_PASSWORD_AUTH@: Enable admin based user password
--     authentication flow @ADMIN_USER_PASSWORD_AUTH@. This setting
--     replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication
--     flow, your app passes a user name and password to Amazon Cognito in
--     the request, instead of using the Secure Remote Password (SRP)
--     protocol to securely transmit the password.
--
-- -   @ALLOW_CUSTOM_AUTH@: Enable Lambda trigger based authentication.
--
-- -   @ALLOW_USER_PASSWORD_AUTH@: Enable user password-based
--     authentication. In this flow, Amazon Cognito receives the password
--     in the request instead of using the SRP protocol to verify
--     passwords.
--
-- -   @ALLOW_USER_SRP_AUTH@: Enable SRP-based authentication.
--
-- -   @ALLOW_REFRESH_TOKEN_AUTH@: Enable authflow to refresh tokens.
--
-- In some environments, you will see the values @ADMIN_NO_SRP_AUTH@,
-- @CUSTOM_AUTH_FLOW_ONLY@, or @USER_PASSWORD_AUTH@. You can\'t assign
-- these legacy @ExplicitAuthFlows@ values to user pool clients at the same
-- time as values that begin with @ALLOW_@, like @ALLOW_USER_SRP_AUTH@.
userPoolClientType_explicitAuthFlows :: Lens.Lens' UserPoolClientType (Prelude.Maybe [ExplicitAuthFlowsType])
userPoolClientType_explicitAuthFlows = Lens.lens (\UserPoolClientType' {explicitAuthFlows} -> explicitAuthFlows) (\s@UserPoolClientType' {} a -> s {explicitAuthFlows = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens.coerced

-- | The ID token time limit. After this limit expires, your user can\'t use
-- their ID token. To specify the time unit for @IdTokenValidity@ as
-- @seconds@, @minutes@, @hours@, or @days@, set a @TokenValidityUnits@
-- value in your API request.
--
-- For example, when you set @IdTokenValidity@ as @10@ and
-- @TokenValidityUnits@ as @hours@, your user can authenticate their
-- session with their ID token for 10 hours.
--
-- The default time unit for @AccessTokenValidity@ in an API request is
-- hours. /Valid range/ is displayed below in seconds.
--
-- If you don\'t specify otherwise in the configuration of your app client,
-- your ID tokens are valid for one hour.
userPoolClientType_idTokenValidity :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Natural)
userPoolClientType_idTokenValidity = Lens.lens (\UserPoolClientType' {idTokenValidity} -> idTokenValidity) (\s@UserPoolClientType' {} a -> s {idTokenValidity = a} :: UserPoolClientType)

-- | The date the user pool client was last modified.
userPoolClientType_lastModifiedDate :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.UTCTime)
userPoolClientType_lastModifiedDate = Lens.lens (\UserPoolClientType' {lastModifiedDate} -> lastModifiedDate) (\s@UserPoolClientType' {} a -> s {lastModifiedDate = a} :: UserPoolClientType) Prelude.. Lens.mapping Data._Time

-- | A list of allowed logout URLs for the IdPs.
userPoolClientType_logoutURLs :: Lens.Lens' UserPoolClientType (Prelude.Maybe [Prelude.Text])
userPoolClientType_logoutURLs = Lens.lens (\UserPoolClientType' {logoutURLs} -> logoutURLs) (\s@UserPoolClientType' {} a -> s {logoutURLs = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens.coerced

-- | Errors and responses that you want Amazon Cognito APIs to return during
-- authentication, account confirmation, and password recovery when the
-- user doesn\'t exist in the user pool. When set to @ENABLED@ and the user
-- doesn\'t exist, authentication returns an error indicating either the
-- username or password was incorrect. Account confirmation and password
-- recovery return a response indicating a code was sent to a simulated
-- destination. When set to @LEGACY@, those APIs return a
-- @UserNotFoundException@ exception if the user doesn\'t exist in the user
-- pool.
--
-- Valid values include:
--
-- -   @ENABLED@ - This prevents user existence-related errors.
--
-- -   @LEGACY@ - This represents the old behavior of Amazon Cognito where
--     user existence related errors aren\'t prevented.
userPoolClientType_preventUserExistenceErrors :: Lens.Lens' UserPoolClientType (Prelude.Maybe PreventUserExistenceErrorTypes)
userPoolClientType_preventUserExistenceErrors = Lens.lens (\UserPoolClientType' {preventUserExistenceErrors} -> preventUserExistenceErrors) (\s@UserPoolClientType' {} a -> s {preventUserExistenceErrors = a} :: UserPoolClientType)

-- | The Read-only attributes.
userPoolClientType_readAttributes :: Lens.Lens' UserPoolClientType (Prelude.Maybe [Prelude.Text])
userPoolClientType_readAttributes = Lens.lens (\UserPoolClientType' {readAttributes} -> readAttributes) (\s@UserPoolClientType' {} a -> s {readAttributes = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens.coerced

-- | The refresh token time limit. After this limit expires, your user can\'t
-- use their refresh token. To specify the time unit for
-- @RefreshTokenValidity@ as @seconds@, @minutes@, @hours@, or @days@, set
-- a @TokenValidityUnits@ value in your API request.
--
-- For example, when you set @RefreshTokenValidity@ as @10@ and
-- @TokenValidityUnits@ as @days@, your user can refresh their session and
-- retrieve new access and ID tokens for 10 days.
--
-- The default time unit for @RefreshTokenValidity@ in an API request is
-- days. You can\'t set @RefreshTokenValidity@ to 0. If you do, Amazon
-- Cognito overrides the value with the default value of 30 days. /Valid
-- range/ is displayed below in seconds.
--
-- If you don\'t specify otherwise in the configuration of your app client,
-- your refresh tokens are valid for 30 days.
userPoolClientType_refreshTokenValidity :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Natural)
userPoolClientType_refreshTokenValidity = Lens.lens (\UserPoolClientType' {refreshTokenValidity} -> refreshTokenValidity) (\s@UserPoolClientType' {} a -> s {refreshTokenValidity = a} :: UserPoolClientType)

-- | A list of provider names for the IdPs that this client supports. The
-- following are supported: @COGNITO@, @Facebook@, @Google@,
-- @SignInWithApple@, @LoginWithAmazon@, and the names of your own SAML and
-- OIDC providers.
userPoolClientType_supportedIdentityProviders :: Lens.Lens' UserPoolClientType (Prelude.Maybe [Prelude.Text])
userPoolClientType_supportedIdentityProviders = Lens.lens (\UserPoolClientType' {supportedIdentityProviders} -> supportedIdentityProviders) (\s@UserPoolClientType' {} a -> s {supportedIdentityProviders = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens.coerced

-- | The time units used to specify the token validity times of each token
-- type: ID, access, and refresh.
userPoolClientType_tokenValidityUnits :: Lens.Lens' UserPoolClientType (Prelude.Maybe TokenValidityUnitsType)
userPoolClientType_tokenValidityUnits = Lens.lens (\UserPoolClientType' {tokenValidityUnits} -> tokenValidityUnits) (\s@UserPoolClientType' {} a -> s {tokenValidityUnits = a} :: UserPoolClientType)

-- | The user pool ID for the user pool client.
userPoolClientType_userPoolId :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Text)
userPoolClientType_userPoolId = Lens.lens (\UserPoolClientType' {userPoolId} -> userPoolId) (\s@UserPoolClientType' {} a -> s {userPoolId = a} :: UserPoolClientType)

-- | The writeable attributes.
userPoolClientType_writeAttributes :: Lens.Lens' UserPoolClientType (Prelude.Maybe [Prelude.Text])
userPoolClientType_writeAttributes = Lens.lens (\UserPoolClientType' {writeAttributes} -> writeAttributes) (\s@UserPoolClientType' {} a -> s {writeAttributes = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON UserPoolClientType where
  parseJSON =
    Data.withObject
      "UserPoolClientType"
      ( \x ->
          UserPoolClientType'
            Prelude.<$> (x Data..:? "AccessTokenValidity")
            Prelude.<*> ( x Data..:? "AllowedOAuthFlows"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "AllowedOAuthFlowsUserPoolClient")
            Prelude.<*> ( x Data..:? "AllowedOAuthScopes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "AnalyticsConfiguration")
            Prelude.<*> (x Data..:? "AuthSessionValidity")
            Prelude.<*> (x Data..:? "CallbackURLs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ClientId")
            Prelude.<*> (x Data..:? "ClientName")
            Prelude.<*> (x Data..:? "ClientSecret")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "DefaultRedirectURI")
            Prelude.<*> ( x
                            Data..:? "EnablePropagateAdditionalUserContextData"
                        )
            Prelude.<*> (x Data..:? "EnableTokenRevocation")
            Prelude.<*> ( x Data..:? "ExplicitAuthFlows"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "IdTokenValidity")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "LogoutURLs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PreventUserExistenceErrors")
            Prelude.<*> (x Data..:? "ReadAttributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RefreshTokenValidity")
            Prelude.<*> ( x Data..:? "SupportedIdentityProviders"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "TokenValidityUnits")
            Prelude.<*> (x Data..:? "UserPoolId")
            Prelude.<*> ( x Data..:? "WriteAttributes"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable UserPoolClientType where
  hashWithSalt _salt UserPoolClientType' {..} =
    _salt `Prelude.hashWithSalt` accessTokenValidity
      `Prelude.hashWithSalt` allowedOAuthFlows
      `Prelude.hashWithSalt` allowedOAuthFlowsUserPoolClient
      `Prelude.hashWithSalt` allowedOAuthScopes
      `Prelude.hashWithSalt` analyticsConfiguration
      `Prelude.hashWithSalt` authSessionValidity
      `Prelude.hashWithSalt` callbackURLs
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` clientName
      `Prelude.hashWithSalt` clientSecret
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` defaultRedirectURI
      `Prelude.hashWithSalt` enablePropagateAdditionalUserContextData
      `Prelude.hashWithSalt` enableTokenRevocation
      `Prelude.hashWithSalt` explicitAuthFlows
      `Prelude.hashWithSalt` idTokenValidity
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` logoutURLs
      `Prelude.hashWithSalt` preventUserExistenceErrors
      `Prelude.hashWithSalt` readAttributes
      `Prelude.hashWithSalt` refreshTokenValidity
      `Prelude.hashWithSalt` supportedIdentityProviders
      `Prelude.hashWithSalt` tokenValidityUnits
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` writeAttributes

instance Prelude.NFData UserPoolClientType where
  rnf UserPoolClientType' {..} =
    Prelude.rnf accessTokenValidity
      `Prelude.seq` Prelude.rnf allowedOAuthFlows
      `Prelude.seq` Prelude.rnf allowedOAuthFlowsUserPoolClient
      `Prelude.seq` Prelude.rnf allowedOAuthScopes
      `Prelude.seq` Prelude.rnf analyticsConfiguration
      `Prelude.seq` Prelude.rnf authSessionValidity
      `Prelude.seq` Prelude.rnf callbackURLs
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf clientName
      `Prelude.seq` Prelude.rnf clientSecret
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf defaultRedirectURI
      `Prelude.seq` Prelude.rnf
        enablePropagateAdditionalUserContextData
      `Prelude.seq` Prelude.rnf enableTokenRevocation
      `Prelude.seq` Prelude.rnf explicitAuthFlows
      `Prelude.seq` Prelude.rnf idTokenValidity
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf logoutURLs
      `Prelude.seq` Prelude.rnf
        preventUserExistenceErrors
      `Prelude.seq` Prelude.rnf readAttributes
      `Prelude.seq` Prelude.rnf
        refreshTokenValidity
      `Prelude.seq` Prelude.rnf
        supportedIdentityProviders
      `Prelude.seq` Prelude.rnf
        tokenValidityUnits
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf
        writeAttributes
