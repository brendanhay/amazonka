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
-- Module      : Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the user pool client.
module Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
  ( -- * Creating a Request
    CreateUserPoolClient (..),
    newCreateUserPoolClient,

    -- * Request Lenses
    createUserPoolClient_refreshTokenValidity,
    createUserPoolClient_idTokenValidity,
    createUserPoolClient_allowedOAuthScopes,
    createUserPoolClient_analyticsConfiguration,
    createUserPoolClient_readAttributes,
    createUserPoolClient_logoutURLs,
    createUserPoolClient_writeAttributes,
    createUserPoolClient_supportedIdentityProviders,
    createUserPoolClient_explicitAuthFlows,
    createUserPoolClient_defaultRedirectURI,
    createUserPoolClient_tokenValidityUnits,
    createUserPoolClient_callbackURLs,
    createUserPoolClient_allowedOAuthFlows,
    createUserPoolClient_accessTokenValidity,
    createUserPoolClient_generateSecret,
    createUserPoolClient_preventUserExistenceErrors,
    createUserPoolClient_allowedOAuthFlowsUserPoolClient,
    createUserPoolClient_userPoolId,
    createUserPoolClient_clientName,

    -- * Destructuring the Response
    CreateUserPoolClientResponse (..),
    newCreateUserPoolClientResponse,

    -- * Response Lenses
    createUserPoolClientResponse_userPoolClient,
    createUserPoolClientResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to create a user pool client.
--
-- /See:/ 'newCreateUserPoolClient' smart constructor.
data CreateUserPoolClient = CreateUserPoolClient'
  { -- | The time limit, in days, after which the refresh token is no longer
    -- valid and cannot be used.
    refreshTokenValidity :: Core.Maybe Core.Natural,
    -- | The time limit, between 5 minutes and 1 day, after which the ID token is
    -- no longer valid and cannot be used. This value will be overridden if you
    -- have entered a value in TokenValidityUnits.
    idTokenValidity :: Core.Maybe Core.Natural,
    -- | The allowed OAuth scopes. Possible values provided by OAuth are:
    -- @phone@, @email@, @openid@, and @profile@. Possible values provided by
    -- AWS are: @aws.cognito.signin.user.admin@. Custom scopes created in
    -- Resource Servers are also supported.
    allowedOAuthScopes :: Core.Maybe [Core.Text],
    -- | The Amazon Pinpoint analytics configuration for collecting metrics for
    -- this user pool.
    --
    -- In regions where Pinpoint is not available, Cognito User Pools only
    -- supports sending events to Amazon Pinpoint projects in us-east-1. In
    -- regions where Pinpoint is available, Cognito User Pools will support
    -- sending events to Amazon Pinpoint projects within that same region.
    analyticsConfiguration :: Core.Maybe AnalyticsConfigurationType,
    -- | The read attributes.
    readAttributes :: Core.Maybe [Core.Text],
    -- | A list of allowed logout URLs for the identity providers.
    logoutURLs :: Core.Maybe [Core.Text],
    -- | The user pool attributes that the app client can write to.
    --
    -- If your app client allows users to sign in through an identity provider,
    -- this array must include all attributes that are mapped to identity
    -- provider attributes. Amazon Cognito updates mapped attributes when users
    -- sign in to your application through an identity provider. If your app
    -- client lacks write access to a mapped attribute, Amazon Cognito throws
    -- an error when it attempts to update the attribute. For more information,
    -- see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool>.
    writeAttributes :: Core.Maybe [Core.Text],
    -- | A list of provider names for the identity providers that are supported
    -- on this client. The following are supported: @COGNITO@, @Facebook@,
    -- @Google@ and @LoginWithAmazon@.
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
    -- | The units in which the validity times are represented in. Default for
    -- RefreshToken is days, and default for ID and access tokens are hours.
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
    -- | The time limit, between 5 minutes and 1 day, after which the access
    -- token is no longer valid and cannot be used. This value will be
    -- overridden if you have entered a value in TokenValidityUnits.
    accessTokenValidity :: Core.Maybe Core.Natural,
    -- | Boolean to specify whether you want to generate a secret for the user
    -- pool client being created.
    generateSecret :: Core.Maybe Core.Bool,
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
    allowedOAuthFlowsUserPoolClient :: Core.Maybe Core.Bool,
    -- | The user pool ID for the user pool where you want to create a user pool
    -- client.
    userPoolId :: Core.Text,
    -- | The client name for the user pool client you would like to create.
    clientName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUserPoolClient' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'refreshTokenValidity', 'createUserPoolClient_refreshTokenValidity' - The time limit, in days, after which the refresh token is no longer
-- valid and cannot be used.
--
-- 'idTokenValidity', 'createUserPoolClient_idTokenValidity' - The time limit, between 5 minutes and 1 day, after which the ID token is
-- no longer valid and cannot be used. This value will be overridden if you
-- have entered a value in TokenValidityUnits.
--
-- 'allowedOAuthScopes', 'createUserPoolClient_allowedOAuthScopes' - The allowed OAuth scopes. Possible values provided by OAuth are:
-- @phone@, @email@, @openid@, and @profile@. Possible values provided by
-- AWS are: @aws.cognito.signin.user.admin@. Custom scopes created in
-- Resource Servers are also supported.
--
-- 'analyticsConfiguration', 'createUserPoolClient_analyticsConfiguration' - The Amazon Pinpoint analytics configuration for collecting metrics for
-- this user pool.
--
-- In regions where Pinpoint is not available, Cognito User Pools only
-- supports sending events to Amazon Pinpoint projects in us-east-1. In
-- regions where Pinpoint is available, Cognito User Pools will support
-- sending events to Amazon Pinpoint projects within that same region.
--
-- 'readAttributes', 'createUserPoolClient_readAttributes' - The read attributes.
--
-- 'logoutURLs', 'createUserPoolClient_logoutURLs' - A list of allowed logout URLs for the identity providers.
--
-- 'writeAttributes', 'createUserPoolClient_writeAttributes' - The user pool attributes that the app client can write to.
--
-- If your app client allows users to sign in through an identity provider,
-- this array must include all attributes that are mapped to identity
-- provider attributes. Amazon Cognito updates mapped attributes when users
-- sign in to your application through an identity provider. If your app
-- client lacks write access to a mapped attribute, Amazon Cognito throws
-- an error when it attempts to update the attribute. For more information,
-- see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool>.
--
-- 'supportedIdentityProviders', 'createUserPoolClient_supportedIdentityProviders' - A list of provider names for the identity providers that are supported
-- on this client. The following are supported: @COGNITO@, @Facebook@,
-- @Google@ and @LoginWithAmazon@.
--
-- 'explicitAuthFlows', 'createUserPoolClient_explicitAuthFlows' - The authentication flows that are supported by the user pool clients.
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
-- 'defaultRedirectURI', 'createUserPoolClient_defaultRedirectURI' - The default redirect URI. Must be in the @CallbackURLs@ list.
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
-- 'tokenValidityUnits', 'createUserPoolClient_tokenValidityUnits' - The units in which the validity times are represented in. Default for
-- RefreshToken is days, and default for ID and access tokens are hours.
--
-- 'callbackURLs', 'createUserPoolClient_callbackURLs' - A list of allowed redirect (callback) URLs for the identity providers.
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
-- 'allowedOAuthFlows', 'createUserPoolClient_allowedOAuthFlows' - The allowed OAuth flows.
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
-- 'accessTokenValidity', 'createUserPoolClient_accessTokenValidity' - The time limit, between 5 minutes and 1 day, after which the access
-- token is no longer valid and cannot be used. This value will be
-- overridden if you have entered a value in TokenValidityUnits.
--
-- 'generateSecret', 'createUserPoolClient_generateSecret' - Boolean to specify whether you want to generate a secret for the user
-- pool client being created.
--
-- 'preventUserExistenceErrors', 'createUserPoolClient_preventUserExistenceErrors' - Use this setting to choose which errors and responses are returned by
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
-- 'allowedOAuthFlowsUserPoolClient', 'createUserPoolClient_allowedOAuthFlowsUserPoolClient' - Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Cognito user pools.
--
-- 'userPoolId', 'createUserPoolClient_userPoolId' - The user pool ID for the user pool where you want to create a user pool
-- client.
--
-- 'clientName', 'createUserPoolClient_clientName' - The client name for the user pool client you would like to create.
newCreateUserPoolClient ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'clientName'
  Core.Text ->
  CreateUserPoolClient
newCreateUserPoolClient pUserPoolId_ pClientName_ =
  CreateUserPoolClient'
    { refreshTokenValidity =
        Core.Nothing,
      idTokenValidity = Core.Nothing,
      allowedOAuthScopes = Core.Nothing,
      analyticsConfiguration = Core.Nothing,
      readAttributes = Core.Nothing,
      logoutURLs = Core.Nothing,
      writeAttributes = Core.Nothing,
      supportedIdentityProviders = Core.Nothing,
      explicitAuthFlows = Core.Nothing,
      defaultRedirectURI = Core.Nothing,
      tokenValidityUnits = Core.Nothing,
      callbackURLs = Core.Nothing,
      allowedOAuthFlows = Core.Nothing,
      accessTokenValidity = Core.Nothing,
      generateSecret = Core.Nothing,
      preventUserExistenceErrors = Core.Nothing,
      allowedOAuthFlowsUserPoolClient = Core.Nothing,
      userPoolId = pUserPoolId_,
      clientName = pClientName_
    }

-- | The time limit, in days, after which the refresh token is no longer
-- valid and cannot be used.
createUserPoolClient_refreshTokenValidity :: Lens.Lens' CreateUserPoolClient (Core.Maybe Core.Natural)
createUserPoolClient_refreshTokenValidity = Lens.lens (\CreateUserPoolClient' {refreshTokenValidity} -> refreshTokenValidity) (\s@CreateUserPoolClient' {} a -> s {refreshTokenValidity = a} :: CreateUserPoolClient)

-- | The time limit, between 5 minutes and 1 day, after which the ID token is
-- no longer valid and cannot be used. This value will be overridden if you
-- have entered a value in TokenValidityUnits.
createUserPoolClient_idTokenValidity :: Lens.Lens' CreateUserPoolClient (Core.Maybe Core.Natural)
createUserPoolClient_idTokenValidity = Lens.lens (\CreateUserPoolClient' {idTokenValidity} -> idTokenValidity) (\s@CreateUserPoolClient' {} a -> s {idTokenValidity = a} :: CreateUserPoolClient)

-- | The allowed OAuth scopes. Possible values provided by OAuth are:
-- @phone@, @email@, @openid@, and @profile@. Possible values provided by
-- AWS are: @aws.cognito.signin.user.admin@. Custom scopes created in
-- Resource Servers are also supported.
createUserPoolClient_allowedOAuthScopes :: Lens.Lens' CreateUserPoolClient (Core.Maybe [Core.Text])
createUserPoolClient_allowedOAuthScopes = Lens.lens (\CreateUserPoolClient' {allowedOAuthScopes} -> allowedOAuthScopes) (\s@CreateUserPoolClient' {} a -> s {allowedOAuthScopes = a} :: CreateUserPoolClient) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Pinpoint analytics configuration for collecting metrics for
-- this user pool.
--
-- In regions where Pinpoint is not available, Cognito User Pools only
-- supports sending events to Amazon Pinpoint projects in us-east-1. In
-- regions where Pinpoint is available, Cognito User Pools will support
-- sending events to Amazon Pinpoint projects within that same region.
createUserPoolClient_analyticsConfiguration :: Lens.Lens' CreateUserPoolClient (Core.Maybe AnalyticsConfigurationType)
createUserPoolClient_analyticsConfiguration = Lens.lens (\CreateUserPoolClient' {analyticsConfiguration} -> analyticsConfiguration) (\s@CreateUserPoolClient' {} a -> s {analyticsConfiguration = a} :: CreateUserPoolClient)

-- | The read attributes.
createUserPoolClient_readAttributes :: Lens.Lens' CreateUserPoolClient (Core.Maybe [Core.Text])
createUserPoolClient_readAttributes = Lens.lens (\CreateUserPoolClient' {readAttributes} -> readAttributes) (\s@CreateUserPoolClient' {} a -> s {readAttributes = a} :: CreateUserPoolClient) Core.. Lens.mapping Lens._Coerce

-- | A list of allowed logout URLs for the identity providers.
createUserPoolClient_logoutURLs :: Lens.Lens' CreateUserPoolClient (Core.Maybe [Core.Text])
createUserPoolClient_logoutURLs = Lens.lens (\CreateUserPoolClient' {logoutURLs} -> logoutURLs) (\s@CreateUserPoolClient' {} a -> s {logoutURLs = a} :: CreateUserPoolClient) Core.. Lens.mapping Lens._Coerce

-- | The user pool attributes that the app client can write to.
--
-- If your app client allows users to sign in through an identity provider,
-- this array must include all attributes that are mapped to identity
-- provider attributes. Amazon Cognito updates mapped attributes when users
-- sign in to your application through an identity provider. If your app
-- client lacks write access to a mapped attribute, Amazon Cognito throws
-- an error when it attempts to update the attribute. For more information,
-- see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool>.
createUserPoolClient_writeAttributes :: Lens.Lens' CreateUserPoolClient (Core.Maybe [Core.Text])
createUserPoolClient_writeAttributes = Lens.lens (\CreateUserPoolClient' {writeAttributes} -> writeAttributes) (\s@CreateUserPoolClient' {} a -> s {writeAttributes = a} :: CreateUserPoolClient) Core.. Lens.mapping Lens._Coerce

-- | A list of provider names for the identity providers that are supported
-- on this client. The following are supported: @COGNITO@, @Facebook@,
-- @Google@ and @LoginWithAmazon@.
createUserPoolClient_supportedIdentityProviders :: Lens.Lens' CreateUserPoolClient (Core.Maybe [Core.Text])
createUserPoolClient_supportedIdentityProviders = Lens.lens (\CreateUserPoolClient' {supportedIdentityProviders} -> supportedIdentityProviders) (\s@CreateUserPoolClient' {} a -> s {supportedIdentityProviders = a} :: CreateUserPoolClient) Core.. Lens.mapping Lens._Coerce

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
createUserPoolClient_explicitAuthFlows :: Lens.Lens' CreateUserPoolClient (Core.Maybe [ExplicitAuthFlowsType])
createUserPoolClient_explicitAuthFlows = Lens.lens (\CreateUserPoolClient' {explicitAuthFlows} -> explicitAuthFlows) (\s@CreateUserPoolClient' {} a -> s {explicitAuthFlows = a} :: CreateUserPoolClient) Core.. Lens.mapping Lens._Coerce

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
createUserPoolClient_defaultRedirectURI :: Lens.Lens' CreateUserPoolClient (Core.Maybe Core.Text)
createUserPoolClient_defaultRedirectURI = Lens.lens (\CreateUserPoolClient' {defaultRedirectURI} -> defaultRedirectURI) (\s@CreateUserPoolClient' {} a -> s {defaultRedirectURI = a} :: CreateUserPoolClient)

-- | The units in which the validity times are represented in. Default for
-- RefreshToken is days, and default for ID and access tokens are hours.
createUserPoolClient_tokenValidityUnits :: Lens.Lens' CreateUserPoolClient (Core.Maybe TokenValidityUnitsType)
createUserPoolClient_tokenValidityUnits = Lens.lens (\CreateUserPoolClient' {tokenValidityUnits} -> tokenValidityUnits) (\s@CreateUserPoolClient' {} a -> s {tokenValidityUnits = a} :: CreateUserPoolClient)

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
createUserPoolClient_callbackURLs :: Lens.Lens' CreateUserPoolClient (Core.Maybe [Core.Text])
createUserPoolClient_callbackURLs = Lens.lens (\CreateUserPoolClient' {callbackURLs} -> callbackURLs) (\s@CreateUserPoolClient' {} a -> s {callbackURLs = a} :: CreateUserPoolClient) Core.. Lens.mapping Lens._Coerce

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
createUserPoolClient_allowedOAuthFlows :: Lens.Lens' CreateUserPoolClient (Core.Maybe [OAuthFlowType])
createUserPoolClient_allowedOAuthFlows = Lens.lens (\CreateUserPoolClient' {allowedOAuthFlows} -> allowedOAuthFlows) (\s@CreateUserPoolClient' {} a -> s {allowedOAuthFlows = a} :: CreateUserPoolClient) Core.. Lens.mapping Lens._Coerce

-- | The time limit, between 5 minutes and 1 day, after which the access
-- token is no longer valid and cannot be used. This value will be
-- overridden if you have entered a value in TokenValidityUnits.
createUserPoolClient_accessTokenValidity :: Lens.Lens' CreateUserPoolClient (Core.Maybe Core.Natural)
createUserPoolClient_accessTokenValidity = Lens.lens (\CreateUserPoolClient' {accessTokenValidity} -> accessTokenValidity) (\s@CreateUserPoolClient' {} a -> s {accessTokenValidity = a} :: CreateUserPoolClient)

-- | Boolean to specify whether you want to generate a secret for the user
-- pool client being created.
createUserPoolClient_generateSecret :: Lens.Lens' CreateUserPoolClient (Core.Maybe Core.Bool)
createUserPoolClient_generateSecret = Lens.lens (\CreateUserPoolClient' {generateSecret} -> generateSecret) (\s@CreateUserPoolClient' {} a -> s {generateSecret = a} :: CreateUserPoolClient)

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
createUserPoolClient_preventUserExistenceErrors :: Lens.Lens' CreateUserPoolClient (Core.Maybe PreventUserExistenceErrorTypes)
createUserPoolClient_preventUserExistenceErrors = Lens.lens (\CreateUserPoolClient' {preventUserExistenceErrors} -> preventUserExistenceErrors) (\s@CreateUserPoolClient' {} a -> s {preventUserExistenceErrors = a} :: CreateUserPoolClient)

-- | Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Cognito user pools.
createUserPoolClient_allowedOAuthFlowsUserPoolClient :: Lens.Lens' CreateUserPoolClient (Core.Maybe Core.Bool)
createUserPoolClient_allowedOAuthFlowsUserPoolClient = Lens.lens (\CreateUserPoolClient' {allowedOAuthFlowsUserPoolClient} -> allowedOAuthFlowsUserPoolClient) (\s@CreateUserPoolClient' {} a -> s {allowedOAuthFlowsUserPoolClient = a} :: CreateUserPoolClient)

-- | The user pool ID for the user pool where you want to create a user pool
-- client.
createUserPoolClient_userPoolId :: Lens.Lens' CreateUserPoolClient Core.Text
createUserPoolClient_userPoolId = Lens.lens (\CreateUserPoolClient' {userPoolId} -> userPoolId) (\s@CreateUserPoolClient' {} a -> s {userPoolId = a} :: CreateUserPoolClient)

-- | The client name for the user pool client you would like to create.
createUserPoolClient_clientName :: Lens.Lens' CreateUserPoolClient Core.Text
createUserPoolClient_clientName = Lens.lens (\CreateUserPoolClient' {clientName} -> clientName) (\s@CreateUserPoolClient' {} a -> s {clientName = a} :: CreateUserPoolClient)

instance Core.AWSRequest CreateUserPoolClient where
  type
    AWSResponse CreateUserPoolClient =
      CreateUserPoolClientResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserPoolClientResponse'
            Core.<$> (x Core..?> "UserPoolClient")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateUserPoolClient

instance Core.NFData CreateUserPoolClient

instance Core.ToHeaders CreateUserPoolClient where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.CreateUserPoolClient" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateUserPoolClient where
  toJSON CreateUserPoolClient' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RefreshTokenValidity" Core..=)
              Core.<$> refreshTokenValidity,
            ("IdTokenValidity" Core..=) Core.<$> idTokenValidity,
            ("AllowedOAuthScopes" Core..=)
              Core.<$> allowedOAuthScopes,
            ("AnalyticsConfiguration" Core..=)
              Core.<$> analyticsConfiguration,
            ("ReadAttributes" Core..=) Core.<$> readAttributes,
            ("LogoutURLs" Core..=) Core.<$> logoutURLs,
            ("WriteAttributes" Core..=) Core.<$> writeAttributes,
            ("SupportedIdentityProviders" Core..=)
              Core.<$> supportedIdentityProviders,
            ("ExplicitAuthFlows" Core..=)
              Core.<$> explicitAuthFlows,
            ("DefaultRedirectURI" Core..=)
              Core.<$> defaultRedirectURI,
            ("TokenValidityUnits" Core..=)
              Core.<$> tokenValidityUnits,
            ("CallbackURLs" Core..=) Core.<$> callbackURLs,
            ("AllowedOAuthFlows" Core..=)
              Core.<$> allowedOAuthFlows,
            ("AccessTokenValidity" Core..=)
              Core.<$> accessTokenValidity,
            ("GenerateSecret" Core..=) Core.<$> generateSecret,
            ("PreventUserExistenceErrors" Core..=)
              Core.<$> preventUserExistenceErrors,
            ("AllowedOAuthFlowsUserPoolClient" Core..=)
              Core.<$> allowedOAuthFlowsUserPoolClient,
            Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("ClientName" Core..= clientName)
          ]
      )

instance Core.ToPath CreateUserPoolClient where
  toPath = Core.const "/"

instance Core.ToQuery CreateUserPoolClient where
  toQuery = Core.const Core.mempty

-- | Represents the response from the server to create a user pool client.
--
-- /See:/ 'newCreateUserPoolClientResponse' smart constructor.
data CreateUserPoolClientResponse = CreateUserPoolClientResponse'
  { -- | The user pool client that was just created.
    userPoolClient :: Core.Maybe UserPoolClientType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUserPoolClientResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolClient', 'createUserPoolClientResponse_userPoolClient' - The user pool client that was just created.
--
-- 'httpStatus', 'createUserPoolClientResponse_httpStatus' - The response's http status code.
newCreateUserPoolClientResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateUserPoolClientResponse
newCreateUserPoolClientResponse pHttpStatus_ =
  CreateUserPoolClientResponse'
    { userPoolClient =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user pool client that was just created.
createUserPoolClientResponse_userPoolClient :: Lens.Lens' CreateUserPoolClientResponse (Core.Maybe UserPoolClientType)
createUserPoolClientResponse_userPoolClient = Lens.lens (\CreateUserPoolClientResponse' {userPoolClient} -> userPoolClient) (\s@CreateUserPoolClientResponse' {} a -> s {userPoolClient = a} :: CreateUserPoolClientResponse)

-- | The response's http status code.
createUserPoolClientResponse_httpStatus :: Lens.Lens' CreateUserPoolClientResponse Core.Int
createUserPoolClientResponse_httpStatus = Lens.lens (\CreateUserPoolClientResponse' {httpStatus} -> httpStatus) (\s@CreateUserPoolClientResponse' {} a -> s {httpStatus = a} :: CreateUserPoolClientResponse)

instance Core.NFData CreateUserPoolClientResponse
