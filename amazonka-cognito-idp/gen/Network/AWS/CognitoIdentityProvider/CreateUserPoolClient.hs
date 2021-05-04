{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to create a user pool client.
--
-- /See:/ 'newCreateUserPoolClient' smart constructor.
data CreateUserPoolClient = CreateUserPoolClient'
  { -- | The time limit, in days, after which the refresh token is no longer
    -- valid and cannot be used.
    refreshTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | The time limit, between 5 minutes and 1 day, after which the ID token is
    -- no longer valid and cannot be used. This value will be overridden if you
    -- have entered a value in TokenValidityUnits.
    idTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | The allowed OAuth scopes. Possible values provided by OAuth are:
    -- @phone@, @email@, @openid@, and @profile@. Possible values provided by
    -- AWS are: @aws.cognito.signin.user.admin@. Custom scopes created in
    -- Resource Servers are also supported.
    allowedOAuthScopes :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Pinpoint analytics configuration for collecting metrics for
    -- this user pool.
    --
    -- In regions where Pinpoint is not available, Cognito User Pools only
    -- supports sending events to Amazon Pinpoint projects in us-east-1. In
    -- regions where Pinpoint is available, Cognito User Pools will support
    -- sending events to Amazon Pinpoint projects within that same region.
    analyticsConfiguration :: Prelude.Maybe AnalyticsConfigurationType,
    -- | The read attributes.
    readAttributes :: Prelude.Maybe [Prelude.Text],
    -- | A list of allowed logout URLs for the identity providers.
    logoutURLs :: Prelude.Maybe [Prelude.Text],
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
    writeAttributes :: Prelude.Maybe [Prelude.Text],
    -- | A list of provider names for the identity providers that are supported
    -- on this client. The following are supported: @COGNITO@, @Facebook@,
    -- @Google@ and @LoginWithAmazon@.
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
    -- | The time limit, between 5 minutes and 1 day, after which the access
    -- token is no longer valid and cannot be used. This value will be
    -- overridden if you have entered a value in TokenValidityUnits.
    accessTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | Boolean to specify whether you want to generate a secret for the user
    -- pool client being created.
    generateSecret :: Prelude.Maybe Prelude.Bool,
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
    -- | The user pool ID for the user pool where you want to create a user pool
    -- client.
    userPoolId :: Prelude.Text,
    -- | The client name for the user pool client you would like to create.
    clientName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'clientName'
  Prelude.Text ->
  CreateUserPoolClient
newCreateUserPoolClient pUserPoolId_ pClientName_ =
  CreateUserPoolClient'
    { refreshTokenValidity =
        Prelude.Nothing,
      idTokenValidity = Prelude.Nothing,
      allowedOAuthScopes = Prelude.Nothing,
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
      generateSecret = Prelude.Nothing,
      preventUserExistenceErrors = Prelude.Nothing,
      allowedOAuthFlowsUserPoolClient = Prelude.Nothing,
      userPoolId = pUserPoolId_,
      clientName = pClientName_
    }

-- | The time limit, in days, after which the refresh token is no longer
-- valid and cannot be used.
createUserPoolClient_refreshTokenValidity :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Natural)
createUserPoolClient_refreshTokenValidity = Lens.lens (\CreateUserPoolClient' {refreshTokenValidity} -> refreshTokenValidity) (\s@CreateUserPoolClient' {} a -> s {refreshTokenValidity = a} :: CreateUserPoolClient)

-- | The time limit, between 5 minutes and 1 day, after which the ID token is
-- no longer valid and cannot be used. This value will be overridden if you
-- have entered a value in TokenValidityUnits.
createUserPoolClient_idTokenValidity :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Natural)
createUserPoolClient_idTokenValidity = Lens.lens (\CreateUserPoolClient' {idTokenValidity} -> idTokenValidity) (\s@CreateUserPoolClient' {} a -> s {idTokenValidity = a} :: CreateUserPoolClient)

-- | The allowed OAuth scopes. Possible values provided by OAuth are:
-- @phone@, @email@, @openid@, and @profile@. Possible values provided by
-- AWS are: @aws.cognito.signin.user.admin@. Custom scopes created in
-- Resource Servers are also supported.
createUserPoolClient_allowedOAuthScopes :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_allowedOAuthScopes = Lens.lens (\CreateUserPoolClient' {allowedOAuthScopes} -> allowedOAuthScopes) (\s@CreateUserPoolClient' {} a -> s {allowedOAuthScopes = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Pinpoint analytics configuration for collecting metrics for
-- this user pool.
--
-- In regions where Pinpoint is not available, Cognito User Pools only
-- supports sending events to Amazon Pinpoint projects in us-east-1. In
-- regions where Pinpoint is available, Cognito User Pools will support
-- sending events to Amazon Pinpoint projects within that same region.
createUserPoolClient_analyticsConfiguration :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe AnalyticsConfigurationType)
createUserPoolClient_analyticsConfiguration = Lens.lens (\CreateUserPoolClient' {analyticsConfiguration} -> analyticsConfiguration) (\s@CreateUserPoolClient' {} a -> s {analyticsConfiguration = a} :: CreateUserPoolClient)

-- | The read attributes.
createUserPoolClient_readAttributes :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_readAttributes = Lens.lens (\CreateUserPoolClient' {readAttributes} -> readAttributes) (\s@CreateUserPoolClient' {} a -> s {readAttributes = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of allowed logout URLs for the identity providers.
createUserPoolClient_logoutURLs :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_logoutURLs = Lens.lens (\CreateUserPoolClient' {logoutURLs} -> logoutURLs) (\s@CreateUserPoolClient' {} a -> s {logoutURLs = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Prelude._Coerce

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
createUserPoolClient_writeAttributes :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_writeAttributes = Lens.lens (\CreateUserPoolClient' {writeAttributes} -> writeAttributes) (\s@CreateUserPoolClient' {} a -> s {writeAttributes = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of provider names for the identity providers that are supported
-- on this client. The following are supported: @COGNITO@, @Facebook@,
-- @Google@ and @LoginWithAmazon@.
createUserPoolClient_supportedIdentityProviders :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_supportedIdentityProviders = Lens.lens (\CreateUserPoolClient' {supportedIdentityProviders} -> supportedIdentityProviders) (\s@CreateUserPoolClient' {} a -> s {supportedIdentityProviders = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Prelude._Coerce

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
createUserPoolClient_explicitAuthFlows :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [ExplicitAuthFlowsType])
createUserPoolClient_explicitAuthFlows = Lens.lens (\CreateUserPoolClient' {explicitAuthFlows} -> explicitAuthFlows) (\s@CreateUserPoolClient' {} a -> s {explicitAuthFlows = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Prelude._Coerce

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
createUserPoolClient_defaultRedirectURI :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Text)
createUserPoolClient_defaultRedirectURI = Lens.lens (\CreateUserPoolClient' {defaultRedirectURI} -> defaultRedirectURI) (\s@CreateUserPoolClient' {} a -> s {defaultRedirectURI = a} :: CreateUserPoolClient)

-- | The units in which the validity times are represented in. Default for
-- RefreshToken is days, and default for ID and access tokens are hours.
createUserPoolClient_tokenValidityUnits :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe TokenValidityUnitsType)
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
createUserPoolClient_callbackURLs :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_callbackURLs = Lens.lens (\CreateUserPoolClient' {callbackURLs} -> callbackURLs) (\s@CreateUserPoolClient' {} a -> s {callbackURLs = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Prelude._Coerce

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
createUserPoolClient_allowedOAuthFlows :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [OAuthFlowType])
createUserPoolClient_allowedOAuthFlows = Lens.lens (\CreateUserPoolClient' {allowedOAuthFlows} -> allowedOAuthFlows) (\s@CreateUserPoolClient' {} a -> s {allowedOAuthFlows = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Prelude._Coerce

-- | The time limit, between 5 minutes and 1 day, after which the access
-- token is no longer valid and cannot be used. This value will be
-- overridden if you have entered a value in TokenValidityUnits.
createUserPoolClient_accessTokenValidity :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Natural)
createUserPoolClient_accessTokenValidity = Lens.lens (\CreateUserPoolClient' {accessTokenValidity} -> accessTokenValidity) (\s@CreateUserPoolClient' {} a -> s {accessTokenValidity = a} :: CreateUserPoolClient)

-- | Boolean to specify whether you want to generate a secret for the user
-- pool client being created.
createUserPoolClient_generateSecret :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Bool)
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
createUserPoolClient_preventUserExistenceErrors :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe PreventUserExistenceErrorTypes)
createUserPoolClient_preventUserExistenceErrors = Lens.lens (\CreateUserPoolClient' {preventUserExistenceErrors} -> preventUserExistenceErrors) (\s@CreateUserPoolClient' {} a -> s {preventUserExistenceErrors = a} :: CreateUserPoolClient)

-- | Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Cognito user pools.
createUserPoolClient_allowedOAuthFlowsUserPoolClient :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Bool)
createUserPoolClient_allowedOAuthFlowsUserPoolClient = Lens.lens (\CreateUserPoolClient' {allowedOAuthFlowsUserPoolClient} -> allowedOAuthFlowsUserPoolClient) (\s@CreateUserPoolClient' {} a -> s {allowedOAuthFlowsUserPoolClient = a} :: CreateUserPoolClient)

-- | The user pool ID for the user pool where you want to create a user pool
-- client.
createUserPoolClient_userPoolId :: Lens.Lens' CreateUserPoolClient Prelude.Text
createUserPoolClient_userPoolId = Lens.lens (\CreateUserPoolClient' {userPoolId} -> userPoolId) (\s@CreateUserPoolClient' {} a -> s {userPoolId = a} :: CreateUserPoolClient)

-- | The client name for the user pool client you would like to create.
createUserPoolClient_clientName :: Lens.Lens' CreateUserPoolClient Prelude.Text
createUserPoolClient_clientName = Lens.lens (\CreateUserPoolClient' {clientName} -> clientName) (\s@CreateUserPoolClient' {} a -> s {clientName = a} :: CreateUserPoolClient)

instance Prelude.AWSRequest CreateUserPoolClient where
  type
    Rs CreateUserPoolClient =
      CreateUserPoolClientResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserPoolClientResponse'
            Prelude.<$> (x Prelude..?> "UserPoolClient")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUserPoolClient

instance Prelude.NFData CreateUserPoolClient

instance Prelude.ToHeaders CreateUserPoolClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.CreateUserPoolClient" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateUserPoolClient where
  toJSON CreateUserPoolClient' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RefreshTokenValidity" Prelude..=)
              Prelude.<$> refreshTokenValidity,
            ("IdTokenValidity" Prelude..=)
              Prelude.<$> idTokenValidity,
            ("AllowedOAuthScopes" Prelude..=)
              Prelude.<$> allowedOAuthScopes,
            ("AnalyticsConfiguration" Prelude..=)
              Prelude.<$> analyticsConfiguration,
            ("ReadAttributes" Prelude..=)
              Prelude.<$> readAttributes,
            ("LogoutURLs" Prelude..=) Prelude.<$> logoutURLs,
            ("WriteAttributes" Prelude..=)
              Prelude.<$> writeAttributes,
            ("SupportedIdentityProviders" Prelude..=)
              Prelude.<$> supportedIdentityProviders,
            ("ExplicitAuthFlows" Prelude..=)
              Prelude.<$> explicitAuthFlows,
            ("DefaultRedirectURI" Prelude..=)
              Prelude.<$> defaultRedirectURI,
            ("TokenValidityUnits" Prelude..=)
              Prelude.<$> tokenValidityUnits,
            ("CallbackURLs" Prelude..=) Prelude.<$> callbackURLs,
            ("AllowedOAuthFlows" Prelude..=)
              Prelude.<$> allowedOAuthFlows,
            ("AccessTokenValidity" Prelude..=)
              Prelude.<$> accessTokenValidity,
            ("GenerateSecret" Prelude..=)
              Prelude.<$> generateSecret,
            ("PreventUserExistenceErrors" Prelude..=)
              Prelude.<$> preventUserExistenceErrors,
            ("AllowedOAuthFlowsUserPoolClient" Prelude..=)
              Prelude.<$> allowedOAuthFlowsUserPoolClient,
            Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just ("ClientName" Prelude..= clientName)
          ]
      )

instance Prelude.ToPath CreateUserPoolClient where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateUserPoolClient where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to create a user pool client.
--
-- /See:/ 'newCreateUserPoolClientResponse' smart constructor.
data CreateUserPoolClientResponse = CreateUserPoolClientResponse'
  { -- | The user pool client that was just created.
    userPoolClient :: Prelude.Maybe UserPoolClientType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateUserPoolClientResponse
newCreateUserPoolClientResponse pHttpStatus_ =
  CreateUserPoolClientResponse'
    { userPoolClient =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user pool client that was just created.
createUserPoolClientResponse_userPoolClient :: Lens.Lens' CreateUserPoolClientResponse (Prelude.Maybe UserPoolClientType)
createUserPoolClientResponse_userPoolClient = Lens.lens (\CreateUserPoolClientResponse' {userPoolClient} -> userPoolClient) (\s@CreateUserPoolClientResponse' {} a -> s {userPoolClient = a} :: CreateUserPoolClientResponse)

-- | The response's http status code.
createUserPoolClientResponse_httpStatus :: Lens.Lens' CreateUserPoolClientResponse Prelude.Int
createUserPoolClientResponse_httpStatus = Lens.lens (\CreateUserPoolClientResponse' {httpStatus} -> httpStatus) (\s@CreateUserPoolClientResponse' {} a -> s {httpStatus = a} :: CreateUserPoolClientResponse)

instance Prelude.NFData CreateUserPoolClientResponse
