{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified user pool app client with the specified
-- attributes. You can get a list of the current user pool app client
-- settings using
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_DescribeUserPoolClient.html DescribeUserPoolClient>.
--
-- If you don\'t provide a value for an attribute, it will be set to the
-- default value.
module Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
  ( -- * Creating a Request
    UpdateUserPoolClient (..),
    newUpdateUserPoolClient,

    -- * Request Lenses
    updateUserPoolClient_refreshTokenValidity,
    updateUserPoolClient_idTokenValidity,
    updateUserPoolClient_allowedOAuthScopes,
    updateUserPoolClient_clientName,
    updateUserPoolClient_analyticsConfiguration,
    updateUserPoolClient_readAttributes,
    updateUserPoolClient_logoutURLs,
    updateUserPoolClient_writeAttributes,
    updateUserPoolClient_supportedIdentityProviders,
    updateUserPoolClient_explicitAuthFlows,
    updateUserPoolClient_defaultRedirectURI,
    updateUserPoolClient_tokenValidityUnits,
    updateUserPoolClient_callbackURLs,
    updateUserPoolClient_allowedOAuthFlows,
    updateUserPoolClient_accessTokenValidity,
    updateUserPoolClient_preventUserExistenceErrors,
    updateUserPoolClient_allowedOAuthFlowsUserPoolClient,
    updateUserPoolClient_userPoolId,
    updateUserPoolClient_clientId,

    -- * Destructuring the Response
    UpdateUserPoolClientResponse (..),
    newUpdateUserPoolClientResponse,

    -- * Response Lenses
    updateUserPoolClientResponse_userPoolClient,
    updateUserPoolClientResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to update the user pool client.
--
-- /See:/ 'newUpdateUserPoolClient' smart constructor.
data UpdateUserPoolClient = UpdateUserPoolClient'
  { -- | The time limit, in days, after which the refresh token is no longer
    -- valid and cannot be used.
    refreshTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | The time limit, after which the ID token is no longer valid and cannot
    -- be used.
    idTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | The allowed OAuth scopes. Possible values provided by OAuth are:
    -- @phone@, @email@, @openid@, and @profile@. Possible values provided by
    -- AWS are: @aws.cognito.signin.user.admin@. Custom scopes created in
    -- Resource Servers are also supported.
    allowedOAuthScopes :: Prelude.Maybe [Prelude.Text],
    -- | The client name from the update user pool client request.
    clientName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Pinpoint analytics configuration for collecting metrics for
    -- this user pool.
    --
    -- In regions where Pinpoint is not available, Cognito User Pools only
    -- supports sending events to Amazon Pinpoint projects in us-east-1. In
    -- regions where Pinpoint is available, Cognito User Pools will support
    -- sending events to Amazon Pinpoint projects within that same region.
    analyticsConfiguration :: Prelude.Maybe AnalyticsConfigurationType,
    -- | The read-only attributes of the user pool.
    readAttributes :: Prelude.Maybe [Prelude.Text],
    -- | A list of allowed logout URLs for the identity providers.
    logoutURLs :: Prelude.Maybe [Prelude.Text],
    -- | The writeable attributes of the user pool.
    writeAttributes :: Prelude.Maybe [Prelude.Text],
    -- | A list of provider names for the identity providers that are supported
    -- on this client.
    supportedIdentityProviders :: Prelude.Maybe [Prelude.Text],
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
    explicitAuthFlows :: Prelude.Maybe [ExplicitAuthFlowsType],
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
    -- | The units in which the validity times are represented in. Default for
    -- RefreshToken is days, and default for ID and access tokens are hours.
    tokenValidityUnits :: Prelude.Maybe TokenValidityUnitsType,
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
    callbackURLs :: Prelude.Maybe [Prelude.Text],
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
    allowedOAuthFlows :: Prelude.Maybe [OAuthFlowType],
    -- | The time limit, after which the access token is no longer valid and
    -- cannot be used.
    accessTokenValidity :: Prelude.Maybe Prelude.Natural,
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
    preventUserExistenceErrors :: Prelude.Maybe PreventUserExistenceErrorTypes,
    -- | Set to true if the client is allowed to follow the OAuth protocol when
    -- interacting with Cognito user pools.
    allowedOAuthFlowsUserPoolClient :: Prelude.Maybe Prelude.Bool,
    -- | The user pool ID for the user pool where you want to update the user
    -- pool client.
    userPoolId :: Prelude.Text,
    -- | The ID of the client associated with the user pool.
    clientId :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserPoolClient' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'refreshTokenValidity', 'updateUserPoolClient_refreshTokenValidity' - The time limit, in days, after which the refresh token is no longer
-- valid and cannot be used.
--
-- 'idTokenValidity', 'updateUserPoolClient_idTokenValidity' - The time limit, after which the ID token is no longer valid and cannot
-- be used.
--
-- 'allowedOAuthScopes', 'updateUserPoolClient_allowedOAuthScopes' - The allowed OAuth scopes. Possible values provided by OAuth are:
-- @phone@, @email@, @openid@, and @profile@. Possible values provided by
-- AWS are: @aws.cognito.signin.user.admin@. Custom scopes created in
-- Resource Servers are also supported.
--
-- 'clientName', 'updateUserPoolClient_clientName' - The client name from the update user pool client request.
--
-- 'analyticsConfiguration', 'updateUserPoolClient_analyticsConfiguration' - The Amazon Pinpoint analytics configuration for collecting metrics for
-- this user pool.
--
-- In regions where Pinpoint is not available, Cognito User Pools only
-- supports sending events to Amazon Pinpoint projects in us-east-1. In
-- regions where Pinpoint is available, Cognito User Pools will support
-- sending events to Amazon Pinpoint projects within that same region.
--
-- 'readAttributes', 'updateUserPoolClient_readAttributes' - The read-only attributes of the user pool.
--
-- 'logoutURLs', 'updateUserPoolClient_logoutURLs' - A list of allowed logout URLs for the identity providers.
--
-- 'writeAttributes', 'updateUserPoolClient_writeAttributes' - The writeable attributes of the user pool.
--
-- 'supportedIdentityProviders', 'updateUserPoolClient_supportedIdentityProviders' - A list of provider names for the identity providers that are supported
-- on this client.
--
-- 'explicitAuthFlows', 'updateUserPoolClient_explicitAuthFlows' - The authentication flows that are supported by the user pool clients.
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
-- 'defaultRedirectURI', 'updateUserPoolClient_defaultRedirectURI' - The default redirect URI. Must be in the @CallbackURLs@ list.
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
-- 'tokenValidityUnits', 'updateUserPoolClient_tokenValidityUnits' - The units in which the validity times are represented in. Default for
-- RefreshToken is days, and default for ID and access tokens are hours.
--
-- 'callbackURLs', 'updateUserPoolClient_callbackURLs' - A list of allowed redirect (callback) URLs for the identity providers.
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
-- 'allowedOAuthFlows', 'updateUserPoolClient_allowedOAuthFlows' - The allowed OAuth flows.
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
-- 'accessTokenValidity', 'updateUserPoolClient_accessTokenValidity' - The time limit, after which the access token is no longer valid and
-- cannot be used.
--
-- 'preventUserExistenceErrors', 'updateUserPoolClient_preventUserExistenceErrors' - Use this setting to choose which errors and responses are returned by
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
-- 'allowedOAuthFlowsUserPoolClient', 'updateUserPoolClient_allowedOAuthFlowsUserPoolClient' - Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Cognito user pools.
--
-- 'userPoolId', 'updateUserPoolClient_userPoolId' - The user pool ID for the user pool where you want to update the user
-- pool client.
--
-- 'clientId', 'updateUserPoolClient_clientId' - The ID of the client associated with the user pool.
newUpdateUserPoolClient ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'clientId'
  Prelude.Text ->
  UpdateUserPoolClient
newUpdateUserPoolClient pUserPoolId_ pClientId_ =
  UpdateUserPoolClient'
    { refreshTokenValidity =
        Prelude.Nothing,
      idTokenValidity = Prelude.Nothing,
      allowedOAuthScopes = Prelude.Nothing,
      clientName = Prelude.Nothing,
      analyticsConfiguration = Prelude.Nothing,
      readAttributes = Prelude.Nothing,
      logoutURLs = Prelude.Nothing,
      writeAttributes = Prelude.Nothing,
      supportedIdentityProviders = Prelude.Nothing,
      explicitAuthFlows = Prelude.Nothing,
      defaultRedirectURI = Prelude.Nothing,
      tokenValidityUnits = Prelude.Nothing,
      callbackURLs = Prelude.Nothing,
      allowedOAuthFlows = Prelude.Nothing,
      accessTokenValidity = Prelude.Nothing,
      preventUserExistenceErrors = Prelude.Nothing,
      allowedOAuthFlowsUserPoolClient = Prelude.Nothing,
      userPoolId = pUserPoolId_,
      clientId = Core._Sensitive Lens.# pClientId_
    }

-- | The time limit, in days, after which the refresh token is no longer
-- valid and cannot be used.
updateUserPoolClient_refreshTokenValidity :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Natural)
updateUserPoolClient_refreshTokenValidity = Lens.lens (\UpdateUserPoolClient' {refreshTokenValidity} -> refreshTokenValidity) (\s@UpdateUserPoolClient' {} a -> s {refreshTokenValidity = a} :: UpdateUserPoolClient)

-- | The time limit, after which the ID token is no longer valid and cannot
-- be used.
updateUserPoolClient_idTokenValidity :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Natural)
updateUserPoolClient_idTokenValidity = Lens.lens (\UpdateUserPoolClient' {idTokenValidity} -> idTokenValidity) (\s@UpdateUserPoolClient' {} a -> s {idTokenValidity = a} :: UpdateUserPoolClient)

-- | The allowed OAuth scopes. Possible values provided by OAuth are:
-- @phone@, @email@, @openid@, and @profile@. Possible values provided by
-- AWS are: @aws.cognito.signin.user.admin@. Custom scopes created in
-- Resource Servers are also supported.
updateUserPoolClient_allowedOAuthScopes :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_allowedOAuthScopes = Lens.lens (\UpdateUserPoolClient' {allowedOAuthScopes} -> allowedOAuthScopes) (\s@UpdateUserPoolClient' {} a -> s {allowedOAuthScopes = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens._Coerce

-- | The client name from the update user pool client request.
updateUserPoolClient_clientName :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Text)
updateUserPoolClient_clientName = Lens.lens (\UpdateUserPoolClient' {clientName} -> clientName) (\s@UpdateUserPoolClient' {} a -> s {clientName = a} :: UpdateUserPoolClient)

-- | The Amazon Pinpoint analytics configuration for collecting metrics for
-- this user pool.
--
-- In regions where Pinpoint is not available, Cognito User Pools only
-- supports sending events to Amazon Pinpoint projects in us-east-1. In
-- regions where Pinpoint is available, Cognito User Pools will support
-- sending events to Amazon Pinpoint projects within that same region.
updateUserPoolClient_analyticsConfiguration :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe AnalyticsConfigurationType)
updateUserPoolClient_analyticsConfiguration = Lens.lens (\UpdateUserPoolClient' {analyticsConfiguration} -> analyticsConfiguration) (\s@UpdateUserPoolClient' {} a -> s {analyticsConfiguration = a} :: UpdateUserPoolClient)

-- | The read-only attributes of the user pool.
updateUserPoolClient_readAttributes :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_readAttributes = Lens.lens (\UpdateUserPoolClient' {readAttributes} -> readAttributes) (\s@UpdateUserPoolClient' {} a -> s {readAttributes = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens._Coerce

-- | A list of allowed logout URLs for the identity providers.
updateUserPoolClient_logoutURLs :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_logoutURLs = Lens.lens (\UpdateUserPoolClient' {logoutURLs} -> logoutURLs) (\s@UpdateUserPoolClient' {} a -> s {logoutURLs = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens._Coerce

-- | The writeable attributes of the user pool.
updateUserPoolClient_writeAttributes :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_writeAttributes = Lens.lens (\UpdateUserPoolClient' {writeAttributes} -> writeAttributes) (\s@UpdateUserPoolClient' {} a -> s {writeAttributes = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens._Coerce

-- | A list of provider names for the identity providers that are supported
-- on this client.
updateUserPoolClient_supportedIdentityProviders :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_supportedIdentityProviders = Lens.lens (\UpdateUserPoolClient' {supportedIdentityProviders} -> supportedIdentityProviders) (\s@UpdateUserPoolClient' {} a -> s {supportedIdentityProviders = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens._Coerce

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
updateUserPoolClient_explicitAuthFlows :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [ExplicitAuthFlowsType])
updateUserPoolClient_explicitAuthFlows = Lens.lens (\UpdateUserPoolClient' {explicitAuthFlows} -> explicitAuthFlows) (\s@UpdateUserPoolClient' {} a -> s {explicitAuthFlows = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens._Coerce

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
updateUserPoolClient_defaultRedirectURI :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Text)
updateUserPoolClient_defaultRedirectURI = Lens.lens (\UpdateUserPoolClient' {defaultRedirectURI} -> defaultRedirectURI) (\s@UpdateUserPoolClient' {} a -> s {defaultRedirectURI = a} :: UpdateUserPoolClient)

-- | The units in which the validity times are represented in. Default for
-- RefreshToken is days, and default for ID and access tokens are hours.
updateUserPoolClient_tokenValidityUnits :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe TokenValidityUnitsType)
updateUserPoolClient_tokenValidityUnits = Lens.lens (\UpdateUserPoolClient' {tokenValidityUnits} -> tokenValidityUnits) (\s@UpdateUserPoolClient' {} a -> s {tokenValidityUnits = a} :: UpdateUserPoolClient)

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
updateUserPoolClient_callbackURLs :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_callbackURLs = Lens.lens (\UpdateUserPoolClient' {callbackURLs} -> callbackURLs) (\s@UpdateUserPoolClient' {} a -> s {callbackURLs = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens._Coerce

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
updateUserPoolClient_allowedOAuthFlows :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [OAuthFlowType])
updateUserPoolClient_allowedOAuthFlows = Lens.lens (\UpdateUserPoolClient' {allowedOAuthFlows} -> allowedOAuthFlows) (\s@UpdateUserPoolClient' {} a -> s {allowedOAuthFlows = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens._Coerce

-- | The time limit, after which the access token is no longer valid and
-- cannot be used.
updateUserPoolClient_accessTokenValidity :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Natural)
updateUserPoolClient_accessTokenValidity = Lens.lens (\UpdateUserPoolClient' {accessTokenValidity} -> accessTokenValidity) (\s@UpdateUserPoolClient' {} a -> s {accessTokenValidity = a} :: UpdateUserPoolClient)

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
updateUserPoolClient_preventUserExistenceErrors :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe PreventUserExistenceErrorTypes)
updateUserPoolClient_preventUserExistenceErrors = Lens.lens (\UpdateUserPoolClient' {preventUserExistenceErrors} -> preventUserExistenceErrors) (\s@UpdateUserPoolClient' {} a -> s {preventUserExistenceErrors = a} :: UpdateUserPoolClient)

-- | Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Cognito user pools.
updateUserPoolClient_allowedOAuthFlowsUserPoolClient :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Bool)
updateUserPoolClient_allowedOAuthFlowsUserPoolClient = Lens.lens (\UpdateUserPoolClient' {allowedOAuthFlowsUserPoolClient} -> allowedOAuthFlowsUserPoolClient) (\s@UpdateUserPoolClient' {} a -> s {allowedOAuthFlowsUserPoolClient = a} :: UpdateUserPoolClient)

-- | The user pool ID for the user pool where you want to update the user
-- pool client.
updateUserPoolClient_userPoolId :: Lens.Lens' UpdateUserPoolClient Prelude.Text
updateUserPoolClient_userPoolId = Lens.lens (\UpdateUserPoolClient' {userPoolId} -> userPoolId) (\s@UpdateUserPoolClient' {} a -> s {userPoolId = a} :: UpdateUserPoolClient)

-- | The ID of the client associated with the user pool.
updateUserPoolClient_clientId :: Lens.Lens' UpdateUserPoolClient Prelude.Text
updateUserPoolClient_clientId = Lens.lens (\UpdateUserPoolClient' {clientId} -> clientId) (\s@UpdateUserPoolClient' {} a -> s {clientId = a} :: UpdateUserPoolClient) Prelude.. Core._Sensitive

instance Core.AWSRequest UpdateUserPoolClient where
  type
    AWSResponse UpdateUserPoolClient =
      UpdateUserPoolClientResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserPoolClientResponse'
            Prelude.<$> (x Core..?> "UserPoolClient")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUserPoolClient

instance Prelude.NFData UpdateUserPoolClient

instance Core.ToHeaders UpdateUserPoolClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.UpdateUserPoolClient" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateUserPoolClient where
  toJSON UpdateUserPoolClient' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RefreshTokenValidity" Core..=)
              Prelude.<$> refreshTokenValidity,
            ("IdTokenValidity" Core..=)
              Prelude.<$> idTokenValidity,
            ("AllowedOAuthScopes" Core..=)
              Prelude.<$> allowedOAuthScopes,
            ("ClientName" Core..=) Prelude.<$> clientName,
            ("AnalyticsConfiguration" Core..=)
              Prelude.<$> analyticsConfiguration,
            ("ReadAttributes" Core..=)
              Prelude.<$> readAttributes,
            ("LogoutURLs" Core..=) Prelude.<$> logoutURLs,
            ("WriteAttributes" Core..=)
              Prelude.<$> writeAttributes,
            ("SupportedIdentityProviders" Core..=)
              Prelude.<$> supportedIdentityProviders,
            ("ExplicitAuthFlows" Core..=)
              Prelude.<$> explicitAuthFlows,
            ("DefaultRedirectURI" Core..=)
              Prelude.<$> defaultRedirectURI,
            ("TokenValidityUnits" Core..=)
              Prelude.<$> tokenValidityUnits,
            ("CallbackURLs" Core..=) Prelude.<$> callbackURLs,
            ("AllowedOAuthFlows" Core..=)
              Prelude.<$> allowedOAuthFlows,
            ("AccessTokenValidity" Core..=)
              Prelude.<$> accessTokenValidity,
            ("PreventUserExistenceErrors" Core..=)
              Prelude.<$> preventUserExistenceErrors,
            ("AllowedOAuthFlowsUserPoolClient" Core..=)
              Prelude.<$> allowedOAuthFlowsUserPoolClient,
            Prelude.Just ("UserPoolId" Core..= userPoolId),
            Prelude.Just ("ClientId" Core..= clientId)
          ]
      )

instance Core.ToPath UpdateUserPoolClient where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateUserPoolClient where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to the request to update the
-- user pool client.
--
-- /See:/ 'newUpdateUserPoolClientResponse' smart constructor.
data UpdateUserPoolClientResponse = UpdateUserPoolClientResponse'
  { -- | The user pool client value from the response from the server when an
    -- update user pool client request is made.
    userPoolClient :: Prelude.Maybe UserPoolClientType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserPoolClientResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolClient', 'updateUserPoolClientResponse_userPoolClient' - The user pool client value from the response from the server when an
-- update user pool client request is made.
--
-- 'httpStatus', 'updateUserPoolClientResponse_httpStatus' - The response's http status code.
newUpdateUserPoolClientResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateUserPoolClientResponse
newUpdateUserPoolClientResponse pHttpStatus_ =
  UpdateUserPoolClientResponse'
    { userPoolClient =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user pool client value from the response from the server when an
-- update user pool client request is made.
updateUserPoolClientResponse_userPoolClient :: Lens.Lens' UpdateUserPoolClientResponse (Prelude.Maybe UserPoolClientType)
updateUserPoolClientResponse_userPoolClient = Lens.lens (\UpdateUserPoolClientResponse' {userPoolClient} -> userPoolClient) (\s@UpdateUserPoolClientResponse' {} a -> s {userPoolClient = a} :: UpdateUserPoolClientResponse)

-- | The response's http status code.
updateUserPoolClientResponse_httpStatus :: Lens.Lens' UpdateUserPoolClientResponse Prelude.Int
updateUserPoolClientResponse_httpStatus = Lens.lens (\UpdateUserPoolClientResponse' {httpStatus} -> httpStatus) (\s@UpdateUserPoolClientResponse' {} a -> s {httpStatus = a} :: UpdateUserPoolClientResponse)

instance Prelude.NFData UpdateUserPoolClientResponse
