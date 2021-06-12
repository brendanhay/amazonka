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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolClientType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolClientType where

import Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
import Network.AWS.CognitoIdentityProvider.Types.OAuthFlowType
import Network.AWS.CognitoIdentityProvider.Types.PreventUserExistenceErrorTypes
import Network.AWS.CognitoIdentityProvider.Types.TokenValidityUnitsType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a user pool client.
--
-- /See:/ 'newUserPoolClientType' smart constructor.
data UserPoolClientType = UserPoolClientType'
  { -- | The date the user pool client was last modified.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The client secret from the user pool request of the client type.
    clientSecret :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The time limit, in days, after which the refresh token is no longer
    -- valid and cannot be used.
    refreshTokenValidity :: Core.Maybe Core.Natural,
    -- | The ID of the client associated with the user pool.
    clientId :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The time limit, specified by tokenValidityUnits, defaulting to hours,
    -- after which the refresh token is no longer valid and cannot be used.
    idTokenValidity :: Core.Maybe Core.Natural,
    -- | The allowed OAuth scopes. Possible values provided by OAuth are:
    -- @phone@, @email@, @openid@, and @profile@. Possible values provided by
    -- AWS are: @aws.cognito.signin.user.admin@. Custom scopes created in
    -- Resource Servers are also supported.
    allowedOAuthScopes :: Core.Maybe [Core.Text],
    -- | The client name from the user pool request of the client type.
    clientName :: Core.Maybe Core.Text,
    -- | The Amazon Pinpoint analytics configuration for the user pool client.
    --
    -- Cognito User Pools only supports sending events to Amazon Pinpoint
    -- projects in the US East (N. Virginia) us-east-1 Region, regardless of
    -- the region in which the user pool resides.
    analyticsConfiguration :: Core.Maybe AnalyticsConfigurationType,
    -- | The user pool ID for the user pool client.
    userPoolId :: Core.Maybe Core.Text,
    -- | The Read-only attributes.
    readAttributes :: Core.Maybe [Core.Text],
    -- | The date the user pool client was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | A list of allowed logout URLs for the identity providers.
    logoutURLs :: Core.Maybe [Core.Text],
    -- | The writeable attributes.
    writeAttributes :: Core.Maybe [Core.Text],
    -- | A list of provider names for the identity providers that are supported
    -- on this client.
    supportedIdentityProviders :: Core.Maybe [Core.Text],
    -- | The authentication flows that are supported by the user pool clients.
    -- Flow names without the @ALLOW_@ prefix are deprecated in favor of new
    -- names with the @ALLOW_@ prefix. Note that values with @ALLOW_@ prefix
    -- cannot be used along with values without @ALLOW_@ prefix.
    --
    -- Valid values include:
    --
    -- -   @ALLOW_ADMIN_USER_PASSWORD_AUTH@: Enable admin based user password
    --     authentication flow @ADMIN_USER_PASSWORD_AUTH@. This setting
    --     replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication
    --     flow, Cognito receives the password in the request instead of using
    --     the SRP (Secure Remote Password protocol) protocol to verify
    --     passwords.
    --
    -- -   @ALLOW_CUSTOM_AUTH@: Enable Lambda trigger based authentication.
    --
    -- -   @ALLOW_USER_PASSWORD_AUTH@: Enable user password-based
    --     authentication. In this flow, Cognito receives the password in the
    --     request instead of using the SRP protocol to verify passwords.
    --
    -- -   @ALLOW_USER_SRP_AUTH@: Enable SRP based authentication.
    --
    -- -   @ALLOW_REFRESH_TOKEN_AUTH@: Enable authflow to refresh tokens.
    explicitAuthFlows :: Core.Maybe [ExplicitAuthFlowsType],
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
    defaultRedirectURI :: Core.Maybe Core.Text,
    -- | The time units used to specify the token validity times of their
    -- respective token.
    tokenValidityUnits :: Core.Maybe TokenValidityUnitsType,
    -- | A list of allowed redirect (callback) URLs for the identity providers.
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
    callbackURLs :: Core.Maybe [Core.Text],
    -- | The allowed OAuth flows.
    --
    -- Set to @code@ to initiate a code grant flow, which provides an
    -- authorization code as the response. This code can be exchanged for
    -- access tokens with the token endpoint.
    --
    -- Set to @implicit@ to specify that the client should get the access token
    -- (and, optionally, ID token, based on scopes) directly.
    --
    -- Set to @client_credentials@ to specify that the client should get the
    -- access token (and, optionally, ID token, based on scopes) from the token
    -- endpoint using a combination of client and client_secret.
    allowedOAuthFlows :: Core.Maybe [OAuthFlowType],
    -- | The time limit, specified by tokenValidityUnits, defaulting to hours,
    -- after which the access token is no longer valid and cannot be used.
    accessTokenValidity :: Core.Maybe Core.Natural,
    -- | Use this setting to choose which errors and responses are returned by
    -- Cognito APIs during authentication, account confirmation, and password
    -- recovery when the user does not exist in the user pool. When set to
    -- @ENABLED@ and the user does not exist, authentication returns an error
    -- indicating either the username or password was incorrect, and account
    -- confirmation and password recovery return a response indicating a code
    -- was sent to a simulated destination. When set to @LEGACY@, those APIs
    -- will return a @UserNotFoundException@ exception if the user does not
    -- exist in the user pool.
    --
    -- Valid values include:
    --
    -- -   @ENABLED@ - This prevents user existence-related errors.
    --
    -- -   @LEGACY@ - This represents the old behavior of Cognito where user
    --     existence related errors are not prevented.
    --
    -- After February 15th 2020, the value of @PreventUserExistenceErrors@ will
    -- default to @ENABLED@ for newly created user pool clients if no value is
    -- provided.
    preventUserExistenceErrors :: Core.Maybe PreventUserExistenceErrorTypes,
    -- | Set to true if the client is allowed to follow the OAuth protocol when
    -- interacting with Cognito user pools.
    allowedOAuthFlowsUserPoolClient :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserPoolClientType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'userPoolClientType_lastModifiedDate' - The date the user pool client was last modified.
--
-- 'clientSecret', 'userPoolClientType_clientSecret' - The client secret from the user pool request of the client type.
--
-- 'refreshTokenValidity', 'userPoolClientType_refreshTokenValidity' - The time limit, in days, after which the refresh token is no longer
-- valid and cannot be used.
--
-- 'clientId', 'userPoolClientType_clientId' - The ID of the client associated with the user pool.
--
-- 'idTokenValidity', 'userPoolClientType_idTokenValidity' - The time limit, specified by tokenValidityUnits, defaulting to hours,
-- after which the refresh token is no longer valid and cannot be used.
--
-- 'allowedOAuthScopes', 'userPoolClientType_allowedOAuthScopes' - The allowed OAuth scopes. Possible values provided by OAuth are:
-- @phone@, @email@, @openid@, and @profile@. Possible values provided by
-- AWS are: @aws.cognito.signin.user.admin@. Custom scopes created in
-- Resource Servers are also supported.
--
-- 'clientName', 'userPoolClientType_clientName' - The client name from the user pool request of the client type.
--
-- 'analyticsConfiguration', 'userPoolClientType_analyticsConfiguration' - The Amazon Pinpoint analytics configuration for the user pool client.
--
-- Cognito User Pools only supports sending events to Amazon Pinpoint
-- projects in the US East (N. Virginia) us-east-1 Region, regardless of
-- the region in which the user pool resides.
--
-- 'userPoolId', 'userPoolClientType_userPoolId' - The user pool ID for the user pool client.
--
-- 'readAttributes', 'userPoolClientType_readAttributes' - The Read-only attributes.
--
-- 'creationDate', 'userPoolClientType_creationDate' - The date the user pool client was created.
--
-- 'logoutURLs', 'userPoolClientType_logoutURLs' - A list of allowed logout URLs for the identity providers.
--
-- 'writeAttributes', 'userPoolClientType_writeAttributes' - The writeable attributes.
--
-- 'supportedIdentityProviders', 'userPoolClientType_supportedIdentityProviders' - A list of provider names for the identity providers that are supported
-- on this client.
--
-- 'explicitAuthFlows', 'userPoolClientType_explicitAuthFlows' - The authentication flows that are supported by the user pool clients.
-- Flow names without the @ALLOW_@ prefix are deprecated in favor of new
-- names with the @ALLOW_@ prefix. Note that values with @ALLOW_@ prefix
-- cannot be used along with values without @ALLOW_@ prefix.
--
-- Valid values include:
--
-- -   @ALLOW_ADMIN_USER_PASSWORD_AUTH@: Enable admin based user password
--     authentication flow @ADMIN_USER_PASSWORD_AUTH@. This setting
--     replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication
--     flow, Cognito receives the password in the request instead of using
--     the SRP (Secure Remote Password protocol) protocol to verify
--     passwords.
--
-- -   @ALLOW_CUSTOM_AUTH@: Enable Lambda trigger based authentication.
--
-- -   @ALLOW_USER_PASSWORD_AUTH@: Enable user password-based
--     authentication. In this flow, Cognito receives the password in the
--     request instead of using the SRP protocol to verify passwords.
--
-- -   @ALLOW_USER_SRP_AUTH@: Enable SRP based authentication.
--
-- -   @ALLOW_REFRESH_TOKEN_AUTH@: Enable authflow to refresh tokens.
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
-- 'tokenValidityUnits', 'userPoolClientType_tokenValidityUnits' - The time units used to specify the token validity times of their
-- respective token.
--
-- 'callbackURLs', 'userPoolClientType_callbackURLs' - A list of allowed redirect (callback) URLs for the identity providers.
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
-- 'allowedOAuthFlows', 'userPoolClientType_allowedOAuthFlows' - The allowed OAuth flows.
--
-- Set to @code@ to initiate a code grant flow, which provides an
-- authorization code as the response. This code can be exchanged for
-- access tokens with the token endpoint.
--
-- Set to @implicit@ to specify that the client should get the access token
-- (and, optionally, ID token, based on scopes) directly.
--
-- Set to @client_credentials@ to specify that the client should get the
-- access token (and, optionally, ID token, based on scopes) from the token
-- endpoint using a combination of client and client_secret.
--
-- 'accessTokenValidity', 'userPoolClientType_accessTokenValidity' - The time limit, specified by tokenValidityUnits, defaulting to hours,
-- after which the access token is no longer valid and cannot be used.
--
-- 'preventUserExistenceErrors', 'userPoolClientType_preventUserExistenceErrors' - Use this setting to choose which errors and responses are returned by
-- Cognito APIs during authentication, account confirmation, and password
-- recovery when the user does not exist in the user pool. When set to
-- @ENABLED@ and the user does not exist, authentication returns an error
-- indicating either the username or password was incorrect, and account
-- confirmation and password recovery return a response indicating a code
-- was sent to a simulated destination. When set to @LEGACY@, those APIs
-- will return a @UserNotFoundException@ exception if the user does not
-- exist in the user pool.
--
-- Valid values include:
--
-- -   @ENABLED@ - This prevents user existence-related errors.
--
-- -   @LEGACY@ - This represents the old behavior of Cognito where user
--     existence related errors are not prevented.
--
-- After February 15th 2020, the value of @PreventUserExistenceErrors@ will
-- default to @ENABLED@ for newly created user pool clients if no value is
-- provided.
--
-- 'allowedOAuthFlowsUserPoolClient', 'userPoolClientType_allowedOAuthFlowsUserPoolClient' - Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Cognito user pools.
newUserPoolClientType ::
  UserPoolClientType
newUserPoolClientType =
  UserPoolClientType'
    { lastModifiedDate =
        Core.Nothing,
      clientSecret = Core.Nothing,
      refreshTokenValidity = Core.Nothing,
      clientId = Core.Nothing,
      idTokenValidity = Core.Nothing,
      allowedOAuthScopes = Core.Nothing,
      clientName = Core.Nothing,
      analyticsConfiguration = Core.Nothing,
      userPoolId = Core.Nothing,
      readAttributes = Core.Nothing,
      creationDate = Core.Nothing,
      logoutURLs = Core.Nothing,
      writeAttributes = Core.Nothing,
      supportedIdentityProviders = Core.Nothing,
      explicitAuthFlows = Core.Nothing,
      defaultRedirectURI = Core.Nothing,
      tokenValidityUnits = Core.Nothing,
      callbackURLs = Core.Nothing,
      allowedOAuthFlows = Core.Nothing,
      accessTokenValidity = Core.Nothing,
      preventUserExistenceErrors = Core.Nothing,
      allowedOAuthFlowsUserPoolClient = Core.Nothing
    }

-- | The date the user pool client was last modified.
userPoolClientType_lastModifiedDate :: Lens.Lens' UserPoolClientType (Core.Maybe Core.UTCTime)
userPoolClientType_lastModifiedDate = Lens.lens (\UserPoolClientType' {lastModifiedDate} -> lastModifiedDate) (\s@UserPoolClientType' {} a -> s {lastModifiedDate = a} :: UserPoolClientType) Core.. Lens.mapping Core._Time

-- | The client secret from the user pool request of the client type.
userPoolClientType_clientSecret :: Lens.Lens' UserPoolClientType (Core.Maybe Core.Text)
userPoolClientType_clientSecret = Lens.lens (\UserPoolClientType' {clientSecret} -> clientSecret) (\s@UserPoolClientType' {} a -> s {clientSecret = a} :: UserPoolClientType) Core.. Lens.mapping Core._Sensitive

-- | The time limit, in days, after which the refresh token is no longer
-- valid and cannot be used.
userPoolClientType_refreshTokenValidity :: Lens.Lens' UserPoolClientType (Core.Maybe Core.Natural)
userPoolClientType_refreshTokenValidity = Lens.lens (\UserPoolClientType' {refreshTokenValidity} -> refreshTokenValidity) (\s@UserPoolClientType' {} a -> s {refreshTokenValidity = a} :: UserPoolClientType)

-- | The ID of the client associated with the user pool.
userPoolClientType_clientId :: Lens.Lens' UserPoolClientType (Core.Maybe Core.Text)
userPoolClientType_clientId = Lens.lens (\UserPoolClientType' {clientId} -> clientId) (\s@UserPoolClientType' {} a -> s {clientId = a} :: UserPoolClientType) Core.. Lens.mapping Core._Sensitive

-- | The time limit, specified by tokenValidityUnits, defaulting to hours,
-- after which the refresh token is no longer valid and cannot be used.
userPoolClientType_idTokenValidity :: Lens.Lens' UserPoolClientType (Core.Maybe Core.Natural)
userPoolClientType_idTokenValidity = Lens.lens (\UserPoolClientType' {idTokenValidity} -> idTokenValidity) (\s@UserPoolClientType' {} a -> s {idTokenValidity = a} :: UserPoolClientType)

-- | The allowed OAuth scopes. Possible values provided by OAuth are:
-- @phone@, @email@, @openid@, and @profile@. Possible values provided by
-- AWS are: @aws.cognito.signin.user.admin@. Custom scopes created in
-- Resource Servers are also supported.
userPoolClientType_allowedOAuthScopes :: Lens.Lens' UserPoolClientType (Core.Maybe [Core.Text])
userPoolClientType_allowedOAuthScopes = Lens.lens (\UserPoolClientType' {allowedOAuthScopes} -> allowedOAuthScopes) (\s@UserPoolClientType' {} a -> s {allowedOAuthScopes = a} :: UserPoolClientType) Core.. Lens.mapping Lens._Coerce

-- | The client name from the user pool request of the client type.
userPoolClientType_clientName :: Lens.Lens' UserPoolClientType (Core.Maybe Core.Text)
userPoolClientType_clientName = Lens.lens (\UserPoolClientType' {clientName} -> clientName) (\s@UserPoolClientType' {} a -> s {clientName = a} :: UserPoolClientType)

-- | The Amazon Pinpoint analytics configuration for the user pool client.
--
-- Cognito User Pools only supports sending events to Amazon Pinpoint
-- projects in the US East (N. Virginia) us-east-1 Region, regardless of
-- the region in which the user pool resides.
userPoolClientType_analyticsConfiguration :: Lens.Lens' UserPoolClientType (Core.Maybe AnalyticsConfigurationType)
userPoolClientType_analyticsConfiguration = Lens.lens (\UserPoolClientType' {analyticsConfiguration} -> analyticsConfiguration) (\s@UserPoolClientType' {} a -> s {analyticsConfiguration = a} :: UserPoolClientType)

-- | The user pool ID for the user pool client.
userPoolClientType_userPoolId :: Lens.Lens' UserPoolClientType (Core.Maybe Core.Text)
userPoolClientType_userPoolId = Lens.lens (\UserPoolClientType' {userPoolId} -> userPoolId) (\s@UserPoolClientType' {} a -> s {userPoolId = a} :: UserPoolClientType)

-- | The Read-only attributes.
userPoolClientType_readAttributes :: Lens.Lens' UserPoolClientType (Core.Maybe [Core.Text])
userPoolClientType_readAttributes = Lens.lens (\UserPoolClientType' {readAttributes} -> readAttributes) (\s@UserPoolClientType' {} a -> s {readAttributes = a} :: UserPoolClientType) Core.. Lens.mapping Lens._Coerce

-- | The date the user pool client was created.
userPoolClientType_creationDate :: Lens.Lens' UserPoolClientType (Core.Maybe Core.UTCTime)
userPoolClientType_creationDate = Lens.lens (\UserPoolClientType' {creationDate} -> creationDate) (\s@UserPoolClientType' {} a -> s {creationDate = a} :: UserPoolClientType) Core.. Lens.mapping Core._Time

-- | A list of allowed logout URLs for the identity providers.
userPoolClientType_logoutURLs :: Lens.Lens' UserPoolClientType (Core.Maybe [Core.Text])
userPoolClientType_logoutURLs = Lens.lens (\UserPoolClientType' {logoutURLs} -> logoutURLs) (\s@UserPoolClientType' {} a -> s {logoutURLs = a} :: UserPoolClientType) Core.. Lens.mapping Lens._Coerce

-- | The writeable attributes.
userPoolClientType_writeAttributes :: Lens.Lens' UserPoolClientType (Core.Maybe [Core.Text])
userPoolClientType_writeAttributes = Lens.lens (\UserPoolClientType' {writeAttributes} -> writeAttributes) (\s@UserPoolClientType' {} a -> s {writeAttributes = a} :: UserPoolClientType) Core.. Lens.mapping Lens._Coerce

-- | A list of provider names for the identity providers that are supported
-- on this client.
userPoolClientType_supportedIdentityProviders :: Lens.Lens' UserPoolClientType (Core.Maybe [Core.Text])
userPoolClientType_supportedIdentityProviders = Lens.lens (\UserPoolClientType' {supportedIdentityProviders} -> supportedIdentityProviders) (\s@UserPoolClientType' {} a -> s {supportedIdentityProviders = a} :: UserPoolClientType) Core.. Lens.mapping Lens._Coerce

-- | The authentication flows that are supported by the user pool clients.
-- Flow names without the @ALLOW_@ prefix are deprecated in favor of new
-- names with the @ALLOW_@ prefix. Note that values with @ALLOW_@ prefix
-- cannot be used along with values without @ALLOW_@ prefix.
--
-- Valid values include:
--
-- -   @ALLOW_ADMIN_USER_PASSWORD_AUTH@: Enable admin based user password
--     authentication flow @ADMIN_USER_PASSWORD_AUTH@. This setting
--     replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication
--     flow, Cognito receives the password in the request instead of using
--     the SRP (Secure Remote Password protocol) protocol to verify
--     passwords.
--
-- -   @ALLOW_CUSTOM_AUTH@: Enable Lambda trigger based authentication.
--
-- -   @ALLOW_USER_PASSWORD_AUTH@: Enable user password-based
--     authentication. In this flow, Cognito receives the password in the
--     request instead of using the SRP protocol to verify passwords.
--
-- -   @ALLOW_USER_SRP_AUTH@: Enable SRP based authentication.
--
-- -   @ALLOW_REFRESH_TOKEN_AUTH@: Enable authflow to refresh tokens.
userPoolClientType_explicitAuthFlows :: Lens.Lens' UserPoolClientType (Core.Maybe [ExplicitAuthFlowsType])
userPoolClientType_explicitAuthFlows = Lens.lens (\UserPoolClientType' {explicitAuthFlows} -> explicitAuthFlows) (\s@UserPoolClientType' {} a -> s {explicitAuthFlows = a} :: UserPoolClientType) Core.. Lens.mapping Lens._Coerce

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
userPoolClientType_defaultRedirectURI :: Lens.Lens' UserPoolClientType (Core.Maybe Core.Text)
userPoolClientType_defaultRedirectURI = Lens.lens (\UserPoolClientType' {defaultRedirectURI} -> defaultRedirectURI) (\s@UserPoolClientType' {} a -> s {defaultRedirectURI = a} :: UserPoolClientType)

-- | The time units used to specify the token validity times of their
-- respective token.
userPoolClientType_tokenValidityUnits :: Lens.Lens' UserPoolClientType (Core.Maybe TokenValidityUnitsType)
userPoolClientType_tokenValidityUnits = Lens.lens (\UserPoolClientType' {tokenValidityUnits} -> tokenValidityUnits) (\s@UserPoolClientType' {} a -> s {tokenValidityUnits = a} :: UserPoolClientType)

-- | A list of allowed redirect (callback) URLs for the identity providers.
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
userPoolClientType_callbackURLs :: Lens.Lens' UserPoolClientType (Core.Maybe [Core.Text])
userPoolClientType_callbackURLs = Lens.lens (\UserPoolClientType' {callbackURLs} -> callbackURLs) (\s@UserPoolClientType' {} a -> s {callbackURLs = a} :: UserPoolClientType) Core.. Lens.mapping Lens._Coerce

-- | The allowed OAuth flows.
--
-- Set to @code@ to initiate a code grant flow, which provides an
-- authorization code as the response. This code can be exchanged for
-- access tokens with the token endpoint.
--
-- Set to @implicit@ to specify that the client should get the access token
-- (and, optionally, ID token, based on scopes) directly.
--
-- Set to @client_credentials@ to specify that the client should get the
-- access token (and, optionally, ID token, based on scopes) from the token
-- endpoint using a combination of client and client_secret.
userPoolClientType_allowedOAuthFlows :: Lens.Lens' UserPoolClientType (Core.Maybe [OAuthFlowType])
userPoolClientType_allowedOAuthFlows = Lens.lens (\UserPoolClientType' {allowedOAuthFlows} -> allowedOAuthFlows) (\s@UserPoolClientType' {} a -> s {allowedOAuthFlows = a} :: UserPoolClientType) Core.. Lens.mapping Lens._Coerce

-- | The time limit, specified by tokenValidityUnits, defaulting to hours,
-- after which the access token is no longer valid and cannot be used.
userPoolClientType_accessTokenValidity :: Lens.Lens' UserPoolClientType (Core.Maybe Core.Natural)
userPoolClientType_accessTokenValidity = Lens.lens (\UserPoolClientType' {accessTokenValidity} -> accessTokenValidity) (\s@UserPoolClientType' {} a -> s {accessTokenValidity = a} :: UserPoolClientType)

-- | Use this setting to choose which errors and responses are returned by
-- Cognito APIs during authentication, account confirmation, and password
-- recovery when the user does not exist in the user pool. When set to
-- @ENABLED@ and the user does not exist, authentication returns an error
-- indicating either the username or password was incorrect, and account
-- confirmation and password recovery return a response indicating a code
-- was sent to a simulated destination. When set to @LEGACY@, those APIs
-- will return a @UserNotFoundException@ exception if the user does not
-- exist in the user pool.
--
-- Valid values include:
--
-- -   @ENABLED@ - This prevents user existence-related errors.
--
-- -   @LEGACY@ - This represents the old behavior of Cognito where user
--     existence related errors are not prevented.
--
-- After February 15th 2020, the value of @PreventUserExistenceErrors@ will
-- default to @ENABLED@ for newly created user pool clients if no value is
-- provided.
userPoolClientType_preventUserExistenceErrors :: Lens.Lens' UserPoolClientType (Core.Maybe PreventUserExistenceErrorTypes)
userPoolClientType_preventUserExistenceErrors = Lens.lens (\UserPoolClientType' {preventUserExistenceErrors} -> preventUserExistenceErrors) (\s@UserPoolClientType' {} a -> s {preventUserExistenceErrors = a} :: UserPoolClientType)

-- | Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Cognito user pools.
userPoolClientType_allowedOAuthFlowsUserPoolClient :: Lens.Lens' UserPoolClientType (Core.Maybe Core.Bool)
userPoolClientType_allowedOAuthFlowsUserPoolClient = Lens.lens (\UserPoolClientType' {allowedOAuthFlowsUserPoolClient} -> allowedOAuthFlowsUserPoolClient) (\s@UserPoolClientType' {} a -> s {allowedOAuthFlowsUserPoolClient = a} :: UserPoolClientType)

instance Core.FromJSON UserPoolClientType where
  parseJSON =
    Core.withObject
      "UserPoolClientType"
      ( \x ->
          UserPoolClientType'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "ClientSecret")
            Core.<*> (x Core..:? "RefreshTokenValidity")
            Core.<*> (x Core..:? "ClientId")
            Core.<*> (x Core..:? "IdTokenValidity")
            Core.<*> ( x Core..:? "AllowedOAuthScopes"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "ClientName")
            Core.<*> (x Core..:? "AnalyticsConfiguration")
            Core.<*> (x Core..:? "UserPoolId")
            Core.<*> (x Core..:? "ReadAttributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "LogoutURLs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "WriteAttributes" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "SupportedIdentityProviders"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "ExplicitAuthFlows" Core..!= Core.mempty)
            Core.<*> (x Core..:? "DefaultRedirectURI")
            Core.<*> (x Core..:? "TokenValidityUnits")
            Core.<*> (x Core..:? "CallbackURLs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AllowedOAuthFlows" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AccessTokenValidity")
            Core.<*> (x Core..:? "PreventUserExistenceErrors")
            Core.<*> (x Core..:? "AllowedOAuthFlowsUserPoolClient")
      )

instance Core.Hashable UserPoolClientType

instance Core.NFData UserPoolClientType
