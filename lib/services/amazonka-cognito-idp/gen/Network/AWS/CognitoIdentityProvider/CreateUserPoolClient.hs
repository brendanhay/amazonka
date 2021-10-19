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
--
-- When you create a new user pool client, token revocation is
-- automatically enabled. For more information about revoking tokens, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
module Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
  ( -- * Creating a Request
    CreateUserPoolClient (..),
    newCreateUserPoolClient,

    -- * Request Lenses
    createUserPoolClient_refreshTokenValidity,
    createUserPoolClient_explicitAuthFlows,
    createUserPoolClient_supportedIdentityProviders,
    createUserPoolClient_logoutURLs,
    createUserPoolClient_allowedOAuthFlowsUserPoolClient,
    createUserPoolClient_generateSecret,
    createUserPoolClient_idTokenValidity,
    createUserPoolClient_tokenValidityUnits,
    createUserPoolClient_defaultRedirectURI,
    createUserPoolClient_enableTokenRevocation,
    createUserPoolClient_writeAttributes,
    createUserPoolClient_preventUserExistenceErrors,
    createUserPoolClient_accessTokenValidity,
    createUserPoolClient_readAttributes,
    createUserPoolClient_allowedOAuthScopes,
    createUserPoolClient_allowedOAuthFlows,
    createUserPoolClient_analyticsConfiguration,
    createUserPoolClient_callbackURLs,
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
    -- | A list of provider names for the identity providers that are supported
    -- on this client. The following are supported: @COGNITO@, @Facebook@,
    -- @Google@ and @LoginWithAmazon@.
    supportedIdentityProviders :: Prelude.Maybe [Prelude.Text],
    -- | A list of allowed logout URLs for the identity providers.
    logoutURLs :: Prelude.Maybe [Prelude.Text],
    -- | Set to true if the client is allowed to follow the OAuth protocol when
    -- interacting with Cognito user pools.
    allowedOAuthFlowsUserPoolClient :: Prelude.Maybe Prelude.Bool,
    -- | Boolean to specify whether you want to generate a secret for the user
    -- pool client being created.
    generateSecret :: Prelude.Maybe Prelude.Bool,
    -- | The time limit, between 5 minutes and 1 day, after which the ID token is
    -- no longer valid and cannot be used. This value will be overridden if you
    -- have entered a value in TokenValidityUnits.
    idTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | The units in which the validity times are represented in. Default for
    -- RefreshToken is days, and default for ID and access tokens are hours.
    tokenValidityUnits :: Prelude.Maybe TokenValidityUnitsType,
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
    -- | Enables or disables token revocation. For more information about
    -- revoking tokens, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
    --
    -- If you don\'t include this parameter, token revocation is automatically
    -- enabled for the new user pool client.
    enableTokenRevocation :: Prelude.Maybe Prelude.Bool,
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
    -- | The time limit, between 5 minutes and 1 day, after which the access
    -- token is no longer valid and cannot be used. This value will be
    -- overridden if you have entered a value in TokenValidityUnits.
    accessTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | The read attributes.
    readAttributes :: Prelude.Maybe [Prelude.Text],
    -- | The allowed OAuth scopes. Possible values provided by OAuth are:
    -- @phone@, @email@, @openid@, and @profile@. Possible values provided by
    -- Amazon Web Services are: @aws.cognito.signin.user.admin@. Custom scopes
    -- created in Resource Servers are also supported.
    allowedOAuthScopes :: Prelude.Maybe [Prelude.Text],
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
    -- | The Amazon Pinpoint analytics configuration for collecting metrics for
    -- this user pool.
    --
    -- In regions where Pinpoint is not available, Cognito User Pools only
    -- supports sending events to Amazon Pinpoint projects in us-east-1. In
    -- regions where Pinpoint is available, Cognito User Pools will support
    -- sending events to Amazon Pinpoint projects within that same region.
    analyticsConfiguration :: Prelude.Maybe AnalyticsConfigurationType,
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
    -- | The user pool ID for the user pool where you want to create a user pool
    -- client.
    userPoolId :: Prelude.Text,
    -- | The client name for the user pool client you would like to create.
    clientName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'supportedIdentityProviders', 'createUserPoolClient_supportedIdentityProviders' - A list of provider names for the identity providers that are supported
-- on this client. The following are supported: @COGNITO@, @Facebook@,
-- @Google@ and @LoginWithAmazon@.
--
-- 'logoutURLs', 'createUserPoolClient_logoutURLs' - A list of allowed logout URLs for the identity providers.
--
-- 'allowedOAuthFlowsUserPoolClient', 'createUserPoolClient_allowedOAuthFlowsUserPoolClient' - Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Cognito user pools.
--
-- 'generateSecret', 'createUserPoolClient_generateSecret' - Boolean to specify whether you want to generate a secret for the user
-- pool client being created.
--
-- 'idTokenValidity', 'createUserPoolClient_idTokenValidity' - The time limit, between 5 minutes and 1 day, after which the ID token is
-- no longer valid and cannot be used. This value will be overridden if you
-- have entered a value in TokenValidityUnits.
--
-- 'tokenValidityUnits', 'createUserPoolClient_tokenValidityUnits' - The units in which the validity times are represented in. Default for
-- RefreshToken is days, and default for ID and access tokens are hours.
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
-- 'enableTokenRevocation', 'createUserPoolClient_enableTokenRevocation' - Enables or disables token revocation. For more information about
-- revoking tokens, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
--
-- If you don\'t include this parameter, token revocation is automatically
-- enabled for the new user pool client.
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
-- 'accessTokenValidity', 'createUserPoolClient_accessTokenValidity' - The time limit, between 5 minutes and 1 day, after which the access
-- token is no longer valid and cannot be used. This value will be
-- overridden if you have entered a value in TokenValidityUnits.
--
-- 'readAttributes', 'createUserPoolClient_readAttributes' - The read attributes.
--
-- 'allowedOAuthScopes', 'createUserPoolClient_allowedOAuthScopes' - The allowed OAuth scopes. Possible values provided by OAuth are:
-- @phone@, @email@, @openid@, and @profile@. Possible values provided by
-- Amazon Web Services are: @aws.cognito.signin.user.admin@. Custom scopes
-- created in Resource Servers are also supported.
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
-- 'analyticsConfiguration', 'createUserPoolClient_analyticsConfiguration' - The Amazon Pinpoint analytics configuration for collecting metrics for
-- this user pool.
--
-- In regions where Pinpoint is not available, Cognito User Pools only
-- supports sending events to Amazon Pinpoint projects in us-east-1. In
-- regions where Pinpoint is available, Cognito User Pools will support
-- sending events to Amazon Pinpoint projects within that same region.
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
      explicitAuthFlows = Prelude.Nothing,
      supportedIdentityProviders = Prelude.Nothing,
      logoutURLs = Prelude.Nothing,
      allowedOAuthFlowsUserPoolClient = Prelude.Nothing,
      generateSecret = Prelude.Nothing,
      idTokenValidity = Prelude.Nothing,
      tokenValidityUnits = Prelude.Nothing,
      defaultRedirectURI = Prelude.Nothing,
      enableTokenRevocation = Prelude.Nothing,
      writeAttributes = Prelude.Nothing,
      preventUserExistenceErrors = Prelude.Nothing,
      accessTokenValidity = Prelude.Nothing,
      readAttributes = Prelude.Nothing,
      allowedOAuthScopes = Prelude.Nothing,
      allowedOAuthFlows = Prelude.Nothing,
      analyticsConfiguration = Prelude.Nothing,
      callbackURLs = Prelude.Nothing,
      userPoolId = pUserPoolId_,
      clientName = pClientName_
    }

-- | The time limit, in days, after which the refresh token is no longer
-- valid and cannot be used.
createUserPoolClient_refreshTokenValidity :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Natural)
createUserPoolClient_refreshTokenValidity = Lens.lens (\CreateUserPoolClient' {refreshTokenValidity} -> refreshTokenValidity) (\s@CreateUserPoolClient' {} a -> s {refreshTokenValidity = a} :: CreateUserPoolClient)

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
createUserPoolClient_explicitAuthFlows = Lens.lens (\CreateUserPoolClient' {explicitAuthFlows} -> explicitAuthFlows) (\s@CreateUserPoolClient' {} a -> s {explicitAuthFlows = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | A list of provider names for the identity providers that are supported
-- on this client. The following are supported: @COGNITO@, @Facebook@,
-- @Google@ and @LoginWithAmazon@.
createUserPoolClient_supportedIdentityProviders :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_supportedIdentityProviders = Lens.lens (\CreateUserPoolClient' {supportedIdentityProviders} -> supportedIdentityProviders) (\s@CreateUserPoolClient' {} a -> s {supportedIdentityProviders = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | A list of allowed logout URLs for the identity providers.
createUserPoolClient_logoutURLs :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_logoutURLs = Lens.lens (\CreateUserPoolClient' {logoutURLs} -> logoutURLs) (\s@CreateUserPoolClient' {} a -> s {logoutURLs = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Cognito user pools.
createUserPoolClient_allowedOAuthFlowsUserPoolClient :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Bool)
createUserPoolClient_allowedOAuthFlowsUserPoolClient = Lens.lens (\CreateUserPoolClient' {allowedOAuthFlowsUserPoolClient} -> allowedOAuthFlowsUserPoolClient) (\s@CreateUserPoolClient' {} a -> s {allowedOAuthFlowsUserPoolClient = a} :: CreateUserPoolClient)

-- | Boolean to specify whether you want to generate a secret for the user
-- pool client being created.
createUserPoolClient_generateSecret :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Bool)
createUserPoolClient_generateSecret = Lens.lens (\CreateUserPoolClient' {generateSecret} -> generateSecret) (\s@CreateUserPoolClient' {} a -> s {generateSecret = a} :: CreateUserPoolClient)

-- | The time limit, between 5 minutes and 1 day, after which the ID token is
-- no longer valid and cannot be used. This value will be overridden if you
-- have entered a value in TokenValidityUnits.
createUserPoolClient_idTokenValidity :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Natural)
createUserPoolClient_idTokenValidity = Lens.lens (\CreateUserPoolClient' {idTokenValidity} -> idTokenValidity) (\s@CreateUserPoolClient' {} a -> s {idTokenValidity = a} :: CreateUserPoolClient)

-- | The units in which the validity times are represented in. Default for
-- RefreshToken is days, and default for ID and access tokens are hours.
createUserPoolClient_tokenValidityUnits :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe TokenValidityUnitsType)
createUserPoolClient_tokenValidityUnits = Lens.lens (\CreateUserPoolClient' {tokenValidityUnits} -> tokenValidityUnits) (\s@CreateUserPoolClient' {} a -> s {tokenValidityUnits = a} :: CreateUserPoolClient)

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

-- | Enables or disables token revocation. For more information about
-- revoking tokens, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
--
-- If you don\'t include this parameter, token revocation is automatically
-- enabled for the new user pool client.
createUserPoolClient_enableTokenRevocation :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Bool)
createUserPoolClient_enableTokenRevocation = Lens.lens (\CreateUserPoolClient' {enableTokenRevocation} -> enableTokenRevocation) (\s@CreateUserPoolClient' {} a -> s {enableTokenRevocation = a} :: CreateUserPoolClient)

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
createUserPoolClient_writeAttributes = Lens.lens (\CreateUserPoolClient' {writeAttributes} -> writeAttributes) (\s@CreateUserPoolClient' {} a -> s {writeAttributes = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

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

-- | The time limit, between 5 minutes and 1 day, after which the access
-- token is no longer valid and cannot be used. This value will be
-- overridden if you have entered a value in TokenValidityUnits.
createUserPoolClient_accessTokenValidity :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Natural)
createUserPoolClient_accessTokenValidity = Lens.lens (\CreateUserPoolClient' {accessTokenValidity} -> accessTokenValidity) (\s@CreateUserPoolClient' {} a -> s {accessTokenValidity = a} :: CreateUserPoolClient)

-- | The read attributes.
createUserPoolClient_readAttributes :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_readAttributes = Lens.lens (\CreateUserPoolClient' {readAttributes} -> readAttributes) (\s@CreateUserPoolClient' {} a -> s {readAttributes = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The allowed OAuth scopes. Possible values provided by OAuth are:
-- @phone@, @email@, @openid@, and @profile@. Possible values provided by
-- Amazon Web Services are: @aws.cognito.signin.user.admin@. Custom scopes
-- created in Resource Servers are also supported.
createUserPoolClient_allowedOAuthScopes :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_allowedOAuthScopes = Lens.lens (\CreateUserPoolClient' {allowedOAuthScopes} -> allowedOAuthScopes) (\s@CreateUserPoolClient' {} a -> s {allowedOAuthScopes = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

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
createUserPoolClient_allowedOAuthFlows = Lens.lens (\CreateUserPoolClient' {allowedOAuthFlows} -> allowedOAuthFlows) (\s@CreateUserPoolClient' {} a -> s {allowedOAuthFlows = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Pinpoint analytics configuration for collecting metrics for
-- this user pool.
--
-- In regions where Pinpoint is not available, Cognito User Pools only
-- supports sending events to Amazon Pinpoint projects in us-east-1. In
-- regions where Pinpoint is available, Cognito User Pools will support
-- sending events to Amazon Pinpoint projects within that same region.
createUserPoolClient_analyticsConfiguration :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe AnalyticsConfigurationType)
createUserPoolClient_analyticsConfiguration = Lens.lens (\CreateUserPoolClient' {analyticsConfiguration} -> analyticsConfiguration) (\s@CreateUserPoolClient' {} a -> s {analyticsConfiguration = a} :: CreateUserPoolClient)

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
createUserPoolClient_callbackURLs = Lens.lens (\CreateUserPoolClient' {callbackURLs} -> callbackURLs) (\s@CreateUserPoolClient' {} a -> s {callbackURLs = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The user pool ID for the user pool where you want to create a user pool
-- client.
createUserPoolClient_userPoolId :: Lens.Lens' CreateUserPoolClient Prelude.Text
createUserPoolClient_userPoolId = Lens.lens (\CreateUserPoolClient' {userPoolId} -> userPoolId) (\s@CreateUserPoolClient' {} a -> s {userPoolId = a} :: CreateUserPoolClient)

-- | The client name for the user pool client you would like to create.
createUserPoolClient_clientName :: Lens.Lens' CreateUserPoolClient Prelude.Text
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
            Prelude.<$> (x Core..?> "UserPoolClient")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUserPoolClient

instance Prelude.NFData CreateUserPoolClient

instance Core.ToHeaders CreateUserPoolClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.CreateUserPoolClient" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateUserPoolClient where
  toJSON CreateUserPoolClient' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RefreshTokenValidity" Core..=)
              Prelude.<$> refreshTokenValidity,
            ("ExplicitAuthFlows" Core..=)
              Prelude.<$> explicitAuthFlows,
            ("SupportedIdentityProviders" Core..=)
              Prelude.<$> supportedIdentityProviders,
            ("LogoutURLs" Core..=) Prelude.<$> logoutURLs,
            ("AllowedOAuthFlowsUserPoolClient" Core..=)
              Prelude.<$> allowedOAuthFlowsUserPoolClient,
            ("GenerateSecret" Core..=)
              Prelude.<$> generateSecret,
            ("IdTokenValidity" Core..=)
              Prelude.<$> idTokenValidity,
            ("TokenValidityUnits" Core..=)
              Prelude.<$> tokenValidityUnits,
            ("DefaultRedirectURI" Core..=)
              Prelude.<$> defaultRedirectURI,
            ("EnableTokenRevocation" Core..=)
              Prelude.<$> enableTokenRevocation,
            ("WriteAttributes" Core..=)
              Prelude.<$> writeAttributes,
            ("PreventUserExistenceErrors" Core..=)
              Prelude.<$> preventUserExistenceErrors,
            ("AccessTokenValidity" Core..=)
              Prelude.<$> accessTokenValidity,
            ("ReadAttributes" Core..=)
              Prelude.<$> readAttributes,
            ("AllowedOAuthScopes" Core..=)
              Prelude.<$> allowedOAuthScopes,
            ("AllowedOAuthFlows" Core..=)
              Prelude.<$> allowedOAuthFlows,
            ("AnalyticsConfiguration" Core..=)
              Prelude.<$> analyticsConfiguration,
            ("CallbackURLs" Core..=) Prelude.<$> callbackURLs,
            Prelude.Just ("UserPoolId" Core..= userPoolId),
            Prelude.Just ("ClientName" Core..= clientName)
          ]
      )

instance Core.ToPath CreateUserPoolClient where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateUserPoolClient where
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
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
