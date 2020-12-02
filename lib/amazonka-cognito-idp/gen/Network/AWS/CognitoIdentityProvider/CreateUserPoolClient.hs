{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the user pool client.
module Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
  ( -- * Creating a Request
    createUserPoolClient,
    CreateUserPoolClient,

    -- * Request Lenses
    cupcRefreshTokenValidity,
    cupcExplicitAuthFlows,
    cupcSupportedIdentityProviders,
    cupcLogoutURLs,
    cupcAllowedOAuthFlowsUserPoolClient,
    cupcGenerateSecret,
    cupcIdTokenValidity,
    cupcTokenValidityUnits,
    cupcDefaultRedirectURI,
    cupcWriteAttributes,
    cupcPreventUserExistenceErrors,
    cupcAccessTokenValidity,
    cupcReadAttributes,
    cupcAllowedOAuthScopes,
    cupcAllowedOAuthFlows,
    cupcAnalyticsConfiguration,
    cupcCallbackURLs,
    cupcUserPoolId,
    cupcClientName,

    -- * Destructuring the Response
    createUserPoolClientResponse,
    CreateUserPoolClientResponse,

    -- * Response Lenses
    cupcrsUserPoolClient,
    cupcrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to create a user pool client.
--
--
--
-- /See:/ 'createUserPoolClient' smart constructor.
data CreateUserPoolClient = CreateUserPoolClient'
  { _cupcRefreshTokenValidity ::
      !(Maybe Nat),
    _cupcExplicitAuthFlows ::
      !(Maybe [ExplicitAuthFlowsType]),
    _cupcSupportedIdentityProviders ::
      !(Maybe [Text]),
    _cupcLogoutURLs :: !(Maybe [Text]),
    _cupcAllowedOAuthFlowsUserPoolClient ::
      !(Maybe Bool),
    _cupcGenerateSecret :: !(Maybe Bool),
    _cupcIdTokenValidity :: !(Maybe Nat),
    _cupcTokenValidityUnits ::
      !(Maybe TokenValidityUnitsType),
    _cupcDefaultRedirectURI :: !(Maybe Text),
    _cupcWriteAttributes :: !(Maybe [Text]),
    _cupcPreventUserExistenceErrors ::
      !(Maybe PreventUserExistenceErrorTypes),
    _cupcAccessTokenValidity :: !(Maybe Nat),
    _cupcReadAttributes :: !(Maybe [Text]),
    _cupcAllowedOAuthScopes :: !(Maybe [Text]),
    _cupcAllowedOAuthFlows ::
      !(Maybe [OAuthFlowType]),
    _cupcAnalyticsConfiguration ::
      !(Maybe AnalyticsConfigurationType),
    _cupcCallbackURLs :: !(Maybe [Text]),
    _cupcUserPoolId :: !Text,
    _cupcClientName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateUserPoolClient' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupcRefreshTokenValidity' - The time limit, in days, after which the refresh token is no longer valid and cannot be used.
--
-- * 'cupcExplicitAuthFlows' - The authentication flows that are supported by the user pool clients. Flow names without the @ALLOW_@ prefix are deprecated in favor of new names with the @ALLOW_@ prefix. Note that values with @ALLOW_@ prefix cannot be used along with values without @ALLOW_@ prefix. Valid values include:     * @ALLOW_ADMIN_USER_PASSWORD_AUTH@ : Enable admin based user password authentication flow @ADMIN_USER_PASSWORD_AUTH@ . This setting replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication flow, Cognito receives the password in the request instead of using the SRP (Secure Remote Password protocol) protocol to verify passwords.     * @ALLOW_CUSTOM_AUTH@ : Enable Lambda trigger based authentication.     * @ALLOW_USER_PASSWORD_AUTH@ : Enable user password-based authentication. In this flow, Cognito receives the password in the request instead of using the SRP protocol to verify passwords.     * @ALLOW_USER_SRP_AUTH@ : Enable SRP based authentication.     * @ALLOW_REFRESH_TOKEN_AUTH@ : Enable authflow to refresh tokens.
--
-- * 'cupcSupportedIdentityProviders' - A list of provider names for the identity providers that are supported on this client. The following are supported: @COGNITO@ , @Facebook@ , @Google@ and @LoginWithAmazon@ .
--
-- * 'cupcLogoutURLs' - A list of allowed logout URLs for the identity providers.
--
-- * 'cupcAllowedOAuthFlowsUserPoolClient' - Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
--
-- * 'cupcGenerateSecret' - Boolean to specify whether you want to generate a secret for the user pool client being created.
--
-- * 'cupcIdTokenValidity' - The time limit, between 5 minutes and 1 day, after which the ID token is no longer valid and cannot be used. This value will be overridden if you have entered a value in TokenValidityUnits.
--
-- * 'cupcTokenValidityUnits' - The units in which the validity times are represented in. Default for RefreshToken is days, and default for ID and access tokens are hours.
--
-- * 'cupcDefaultRedirectURI' - The default redirect URI. Must be in the @CallbackURLs@ list. A redirect URI must:     * Be an absolute URI.     * Be registered with the authorization server.     * Not include a fragment component. See <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint> . Amazon Cognito requires HTTPS over HTTP except for http://localhost for testing purposes only. App callback URLs such as myapp://example are also supported.
--
-- * 'cupcWriteAttributes' - The user pool attributes that the app client can write to. If your app client allows users to sign in through an identity provider, this array must include all attributes that are mapped to identity provider attributes. Amazon Cognito updates mapped attributes when users sign in to your application through an identity provider. If your app client lacks write access to a mapped attribute, Amazon Cognito throws an error when it attempts to update the attribute. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool> .
--
-- * 'cupcPreventUserExistenceErrors' - Use this setting to choose which errors and responses are returned by Cognito APIs during authentication, account confirmation, and password recovery when the user does not exist in the user pool. When set to @ENABLED@ and the user does not exist, authentication returns an error indicating either the username or password was incorrect, and account confirmation and password recovery return a response indicating a code was sent to a simulated destination. When set to @LEGACY@ , those APIs will return a @UserNotFoundException@ exception if the user does not exist in the user pool. Valid values include:     * @ENABLED@ - This prevents user existence-related errors.     * @LEGACY@ - This represents the old behavior of Cognito where user existence related errors are not prevented.
--
-- * 'cupcAccessTokenValidity' - The time limit, between 5 minutes and 1 day, after which the access token is no longer valid and cannot be used. This value will be overridden if you have entered a value in TokenValidityUnits.
--
-- * 'cupcReadAttributes' - The read attributes.
--
-- * 'cupcAllowedOAuthScopes' - The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
--
-- * 'cupcAllowedOAuthFlows' - The allowed OAuth flows. Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint. Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly. Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
--
-- * 'cupcAnalyticsConfiguration' - The Amazon Pinpoint analytics configuration for collecting metrics for this user pool.
--
-- * 'cupcCallbackURLs' - A list of allowed redirect (callback) URLs for the identity providers. A redirect URI must:     * Be an absolute URI.     * Be registered with the authorization server.     * Not include a fragment component. See <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint> . Amazon Cognito requires HTTPS over HTTP except for http://localhost for testing purposes only. App callback URLs such as myapp://example are also supported.
--
-- * 'cupcUserPoolId' - The user pool ID for the user pool where you want to create a user pool client.
--
-- * 'cupcClientName' - The client name for the user pool client you would like to create.
createUserPoolClient ::
  -- | 'cupcUserPoolId'
  Text ->
  -- | 'cupcClientName'
  Text ->
  CreateUserPoolClient
createUserPoolClient pUserPoolId_ pClientName_ =
  CreateUserPoolClient'
    { _cupcRefreshTokenValidity = Nothing,
      _cupcExplicitAuthFlows = Nothing,
      _cupcSupportedIdentityProviders = Nothing,
      _cupcLogoutURLs = Nothing,
      _cupcAllowedOAuthFlowsUserPoolClient = Nothing,
      _cupcGenerateSecret = Nothing,
      _cupcIdTokenValidity = Nothing,
      _cupcTokenValidityUnits = Nothing,
      _cupcDefaultRedirectURI = Nothing,
      _cupcWriteAttributes = Nothing,
      _cupcPreventUserExistenceErrors = Nothing,
      _cupcAccessTokenValidity = Nothing,
      _cupcReadAttributes = Nothing,
      _cupcAllowedOAuthScopes = Nothing,
      _cupcAllowedOAuthFlows = Nothing,
      _cupcAnalyticsConfiguration = Nothing,
      _cupcCallbackURLs = Nothing,
      _cupcUserPoolId = pUserPoolId_,
      _cupcClientName = pClientName_
    }

-- | The time limit, in days, after which the refresh token is no longer valid and cannot be used.
cupcRefreshTokenValidity :: Lens' CreateUserPoolClient (Maybe Natural)
cupcRefreshTokenValidity = lens _cupcRefreshTokenValidity (\s a -> s {_cupcRefreshTokenValidity = a}) . mapping _Nat

-- | The authentication flows that are supported by the user pool clients. Flow names without the @ALLOW_@ prefix are deprecated in favor of new names with the @ALLOW_@ prefix. Note that values with @ALLOW_@ prefix cannot be used along with values without @ALLOW_@ prefix. Valid values include:     * @ALLOW_ADMIN_USER_PASSWORD_AUTH@ : Enable admin based user password authentication flow @ADMIN_USER_PASSWORD_AUTH@ . This setting replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication flow, Cognito receives the password in the request instead of using the SRP (Secure Remote Password protocol) protocol to verify passwords.     * @ALLOW_CUSTOM_AUTH@ : Enable Lambda trigger based authentication.     * @ALLOW_USER_PASSWORD_AUTH@ : Enable user password-based authentication. In this flow, Cognito receives the password in the request instead of using the SRP protocol to verify passwords.     * @ALLOW_USER_SRP_AUTH@ : Enable SRP based authentication.     * @ALLOW_REFRESH_TOKEN_AUTH@ : Enable authflow to refresh tokens.
cupcExplicitAuthFlows :: Lens' CreateUserPoolClient [ExplicitAuthFlowsType]
cupcExplicitAuthFlows = lens _cupcExplicitAuthFlows (\s a -> s {_cupcExplicitAuthFlows = a}) . _Default . _Coerce

-- | A list of provider names for the identity providers that are supported on this client. The following are supported: @COGNITO@ , @Facebook@ , @Google@ and @LoginWithAmazon@ .
cupcSupportedIdentityProviders :: Lens' CreateUserPoolClient [Text]
cupcSupportedIdentityProviders = lens _cupcSupportedIdentityProviders (\s a -> s {_cupcSupportedIdentityProviders = a}) . _Default . _Coerce

-- | A list of allowed logout URLs for the identity providers.
cupcLogoutURLs :: Lens' CreateUserPoolClient [Text]
cupcLogoutURLs = lens _cupcLogoutURLs (\s a -> s {_cupcLogoutURLs = a}) . _Default . _Coerce

-- | Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
cupcAllowedOAuthFlowsUserPoolClient :: Lens' CreateUserPoolClient (Maybe Bool)
cupcAllowedOAuthFlowsUserPoolClient = lens _cupcAllowedOAuthFlowsUserPoolClient (\s a -> s {_cupcAllowedOAuthFlowsUserPoolClient = a})

-- | Boolean to specify whether you want to generate a secret for the user pool client being created.
cupcGenerateSecret :: Lens' CreateUserPoolClient (Maybe Bool)
cupcGenerateSecret = lens _cupcGenerateSecret (\s a -> s {_cupcGenerateSecret = a})

-- | The time limit, between 5 minutes and 1 day, after which the ID token is no longer valid and cannot be used. This value will be overridden if you have entered a value in TokenValidityUnits.
cupcIdTokenValidity :: Lens' CreateUserPoolClient (Maybe Natural)
cupcIdTokenValidity = lens _cupcIdTokenValidity (\s a -> s {_cupcIdTokenValidity = a}) . mapping _Nat

-- | The units in which the validity times are represented in. Default for RefreshToken is days, and default for ID and access tokens are hours.
cupcTokenValidityUnits :: Lens' CreateUserPoolClient (Maybe TokenValidityUnitsType)
cupcTokenValidityUnits = lens _cupcTokenValidityUnits (\s a -> s {_cupcTokenValidityUnits = a})

-- | The default redirect URI. Must be in the @CallbackURLs@ list. A redirect URI must:     * Be an absolute URI.     * Be registered with the authorization server.     * Not include a fragment component. See <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint> . Amazon Cognito requires HTTPS over HTTP except for http://localhost for testing purposes only. App callback URLs such as myapp://example are also supported.
cupcDefaultRedirectURI :: Lens' CreateUserPoolClient (Maybe Text)
cupcDefaultRedirectURI = lens _cupcDefaultRedirectURI (\s a -> s {_cupcDefaultRedirectURI = a})

-- | The user pool attributes that the app client can write to. If your app client allows users to sign in through an identity provider, this array must include all attributes that are mapped to identity provider attributes. Amazon Cognito updates mapped attributes when users sign in to your application through an identity provider. If your app client lacks write access to a mapped attribute, Amazon Cognito throws an error when it attempts to update the attribute. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool> .
cupcWriteAttributes :: Lens' CreateUserPoolClient [Text]
cupcWriteAttributes = lens _cupcWriteAttributes (\s a -> s {_cupcWriteAttributes = a}) . _Default . _Coerce

-- | Use this setting to choose which errors and responses are returned by Cognito APIs during authentication, account confirmation, and password recovery when the user does not exist in the user pool. When set to @ENABLED@ and the user does not exist, authentication returns an error indicating either the username or password was incorrect, and account confirmation and password recovery return a response indicating a code was sent to a simulated destination. When set to @LEGACY@ , those APIs will return a @UserNotFoundException@ exception if the user does not exist in the user pool. Valid values include:     * @ENABLED@ - This prevents user existence-related errors.     * @LEGACY@ - This represents the old behavior of Cognito where user existence related errors are not prevented.
cupcPreventUserExistenceErrors :: Lens' CreateUserPoolClient (Maybe PreventUserExistenceErrorTypes)
cupcPreventUserExistenceErrors = lens _cupcPreventUserExistenceErrors (\s a -> s {_cupcPreventUserExistenceErrors = a})

-- | The time limit, between 5 minutes and 1 day, after which the access token is no longer valid and cannot be used. This value will be overridden if you have entered a value in TokenValidityUnits.
cupcAccessTokenValidity :: Lens' CreateUserPoolClient (Maybe Natural)
cupcAccessTokenValidity = lens _cupcAccessTokenValidity (\s a -> s {_cupcAccessTokenValidity = a}) . mapping _Nat

-- | The read attributes.
cupcReadAttributes :: Lens' CreateUserPoolClient [Text]
cupcReadAttributes = lens _cupcReadAttributes (\s a -> s {_cupcReadAttributes = a}) . _Default . _Coerce

-- | The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
cupcAllowedOAuthScopes :: Lens' CreateUserPoolClient [Text]
cupcAllowedOAuthScopes = lens _cupcAllowedOAuthScopes (\s a -> s {_cupcAllowedOAuthScopes = a}) . _Default . _Coerce

-- | The allowed OAuth flows. Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint. Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly. Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
cupcAllowedOAuthFlows :: Lens' CreateUserPoolClient [OAuthFlowType]
cupcAllowedOAuthFlows = lens _cupcAllowedOAuthFlows (\s a -> s {_cupcAllowedOAuthFlows = a}) . _Default . _Coerce

-- | The Amazon Pinpoint analytics configuration for collecting metrics for this user pool.
cupcAnalyticsConfiguration :: Lens' CreateUserPoolClient (Maybe AnalyticsConfigurationType)
cupcAnalyticsConfiguration = lens _cupcAnalyticsConfiguration (\s a -> s {_cupcAnalyticsConfiguration = a})

-- | A list of allowed redirect (callback) URLs for the identity providers. A redirect URI must:     * Be an absolute URI.     * Be registered with the authorization server.     * Not include a fragment component. See <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint> . Amazon Cognito requires HTTPS over HTTP except for http://localhost for testing purposes only. App callback URLs such as myapp://example are also supported.
cupcCallbackURLs :: Lens' CreateUserPoolClient [Text]
cupcCallbackURLs = lens _cupcCallbackURLs (\s a -> s {_cupcCallbackURLs = a}) . _Default . _Coerce

-- | The user pool ID for the user pool where you want to create a user pool client.
cupcUserPoolId :: Lens' CreateUserPoolClient Text
cupcUserPoolId = lens _cupcUserPoolId (\s a -> s {_cupcUserPoolId = a})

-- | The client name for the user pool client you would like to create.
cupcClientName :: Lens' CreateUserPoolClient Text
cupcClientName = lens _cupcClientName (\s a -> s {_cupcClientName = a})

instance AWSRequest CreateUserPoolClient where
  type Rs CreateUserPoolClient = CreateUserPoolClientResponse
  request = postJSON cognitoIdentityProvider
  response =
    receiveJSON
      ( \s h x ->
          CreateUserPoolClientResponse'
            <$> (x .?> "UserPoolClient") <*> (pure (fromEnum s))
      )

instance Hashable CreateUserPoolClient

instance NFData CreateUserPoolClient

instance ToHeaders CreateUserPoolClient where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSCognitoIdentityProviderService.CreateUserPoolClient" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateUserPoolClient where
  toJSON CreateUserPoolClient' {..} =
    object
      ( catMaybes
          [ ("RefreshTokenValidity" .=) <$> _cupcRefreshTokenValidity,
            ("ExplicitAuthFlows" .=) <$> _cupcExplicitAuthFlows,
            ("SupportedIdentityProviders" .=)
              <$> _cupcSupportedIdentityProviders,
            ("LogoutURLs" .=) <$> _cupcLogoutURLs,
            ("AllowedOAuthFlowsUserPoolClient" .=)
              <$> _cupcAllowedOAuthFlowsUserPoolClient,
            ("GenerateSecret" .=) <$> _cupcGenerateSecret,
            ("IdTokenValidity" .=) <$> _cupcIdTokenValidity,
            ("TokenValidityUnits" .=) <$> _cupcTokenValidityUnits,
            ("DefaultRedirectURI" .=) <$> _cupcDefaultRedirectURI,
            ("WriteAttributes" .=) <$> _cupcWriteAttributes,
            ("PreventUserExistenceErrors" .=)
              <$> _cupcPreventUserExistenceErrors,
            ("AccessTokenValidity" .=) <$> _cupcAccessTokenValidity,
            ("ReadAttributes" .=) <$> _cupcReadAttributes,
            ("AllowedOAuthScopes" .=) <$> _cupcAllowedOAuthScopes,
            ("AllowedOAuthFlows" .=) <$> _cupcAllowedOAuthFlows,
            ("AnalyticsConfiguration" .=) <$> _cupcAnalyticsConfiguration,
            ("CallbackURLs" .=) <$> _cupcCallbackURLs,
            Just ("UserPoolId" .= _cupcUserPoolId),
            Just ("ClientName" .= _cupcClientName)
          ]
      )

instance ToPath CreateUserPoolClient where
  toPath = const "/"

instance ToQuery CreateUserPoolClient where
  toQuery = const mempty

-- | Represents the response from the server to create a user pool client.
--
--
--
-- /See:/ 'createUserPoolClientResponse' smart constructor.
data CreateUserPoolClientResponse = CreateUserPoolClientResponse'
  { _cupcrsUserPoolClient ::
      !(Maybe UserPoolClientType),
    _cupcrsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateUserPoolClientResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupcrsUserPoolClient' - The user pool client that was just created.
--
-- * 'cupcrsResponseStatus' - -- | The response status code.
createUserPoolClientResponse ::
  -- | 'cupcrsResponseStatus'
  Int ->
  CreateUserPoolClientResponse
createUserPoolClientResponse pResponseStatus_ =
  CreateUserPoolClientResponse'
    { _cupcrsUserPoolClient = Nothing,
      _cupcrsResponseStatus = pResponseStatus_
    }

-- | The user pool client that was just created.
cupcrsUserPoolClient :: Lens' CreateUserPoolClientResponse (Maybe UserPoolClientType)
cupcrsUserPoolClient = lens _cupcrsUserPoolClient (\s a -> s {_cupcrsUserPoolClient = a})

-- | -- | The response status code.
cupcrsResponseStatus :: Lens' CreateUserPoolClientResponse Int
cupcrsResponseStatus = lens _cupcrsResponseStatus (\s a -> s {_cupcrsResponseStatus = a})

instance NFData CreateUserPoolClientResponse
