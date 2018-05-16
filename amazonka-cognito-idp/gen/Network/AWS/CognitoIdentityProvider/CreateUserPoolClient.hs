{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the user pool client.
--
--
module Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
    (
    -- * Creating a Request
      createUserPoolClient
    , CreateUserPoolClient
    -- * Request Lenses
    , cupcRefreshTokenValidity
    , cupcExplicitAuthFlows
    , cupcSupportedIdentityProviders
    , cupcLogoutURLs
    , cupcAllowedOAuthFlowsUserPoolClient
    , cupcGenerateSecret
    , cupcDefaultRedirectURI
    , cupcWriteAttributes
    , cupcReadAttributes
    , cupcAllowedOAuthScopes
    , cupcAllowedOAuthFlows
    , cupcAnalyticsConfiguration
    , cupcCallbackURLs
    , cupcUserPoolId
    , cupcClientName

    -- * Destructuring the Response
    , createUserPoolClientResponse
    , CreateUserPoolClientResponse
    -- * Response Lenses
    , cupcrsUserPoolClient
    , cupcrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
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
  { _cupcRefreshTokenValidity            :: !(Maybe Nat)
  , _cupcExplicitAuthFlows               :: !(Maybe [ExplicitAuthFlowsType])
  , _cupcSupportedIdentityProviders      :: !(Maybe [Text])
  , _cupcLogoutURLs                      :: !(Maybe [Text])
  , _cupcAllowedOAuthFlowsUserPoolClient :: !(Maybe Bool)
  , _cupcGenerateSecret                  :: !(Maybe Bool)
  , _cupcDefaultRedirectURI              :: !(Maybe Text)
  , _cupcWriteAttributes                 :: !(Maybe [Text])
  , _cupcReadAttributes                  :: !(Maybe [Text])
  , _cupcAllowedOAuthScopes              :: !(Maybe [Text])
  , _cupcAllowedOAuthFlows               :: !(Maybe [OAuthFlowType])
  , _cupcAnalyticsConfiguration          :: !(Maybe AnalyticsConfigurationType)
  , _cupcCallbackURLs                    :: !(Maybe [Text])
  , _cupcUserPoolId                      :: !Text
  , _cupcClientName                      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUserPoolClient' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupcRefreshTokenValidity' - The time limit, in days, after which the refresh token is no longer valid and cannot be used.
--
-- * 'cupcExplicitAuthFlows' - The explicit authentication flows.
--
-- * 'cupcSupportedIdentityProviders' - A list of provider names for the identity providers that are supported on this client.
--
-- * 'cupcLogoutURLs' - A list of allowed logout URLs for the identity providers.
--
-- * 'cupcAllowedOAuthFlowsUserPoolClient' - Set to @True@ if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
--
-- * 'cupcGenerateSecret' - Boolean to specify whether you want to generate a secret for the user pool client being created.
--
-- * 'cupcDefaultRedirectURI' - The default redirect URI. Must be in the @CallbackURLs@ list.
--
-- * 'cupcWriteAttributes' - The write attributes.
--
-- * 'cupcReadAttributes' - The read attributes.
--
-- * 'cupcAllowedOAuthScopes' - A list of allowed @OAuth@ scopes. Currently supported values are @"phone"@ , @"email"@ , @"openid"@ , and @"Cognito"@ .
--
-- * 'cupcAllowedOAuthFlows' - Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint. Set to @token@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
--
-- * 'cupcAnalyticsConfiguration' - The Amazon Pinpoint analytics configuration for collecting metrics for this user pool.
--
-- * 'cupcCallbackURLs' - A list of allowed callback URLs for the identity providers.
--
-- * 'cupcUserPoolId' - The user pool ID for the user pool where you want to create a user pool client.
--
-- * 'cupcClientName' - The client name for the user pool client you would like to create.
createUserPoolClient
    :: Text -- ^ 'cupcUserPoolId'
    -> Text -- ^ 'cupcClientName'
    -> CreateUserPoolClient
createUserPoolClient pUserPoolId_ pClientName_ =
  CreateUserPoolClient'
    { _cupcRefreshTokenValidity = Nothing
    , _cupcExplicitAuthFlows = Nothing
    , _cupcSupportedIdentityProviders = Nothing
    , _cupcLogoutURLs = Nothing
    , _cupcAllowedOAuthFlowsUserPoolClient = Nothing
    , _cupcGenerateSecret = Nothing
    , _cupcDefaultRedirectURI = Nothing
    , _cupcWriteAttributes = Nothing
    , _cupcReadAttributes = Nothing
    , _cupcAllowedOAuthScopes = Nothing
    , _cupcAllowedOAuthFlows = Nothing
    , _cupcAnalyticsConfiguration = Nothing
    , _cupcCallbackURLs = Nothing
    , _cupcUserPoolId = pUserPoolId_
    , _cupcClientName = pClientName_
    }


-- | The time limit, in days, after which the refresh token is no longer valid and cannot be used.
cupcRefreshTokenValidity :: Lens' CreateUserPoolClient (Maybe Natural)
cupcRefreshTokenValidity = lens _cupcRefreshTokenValidity (\ s a -> s{_cupcRefreshTokenValidity = a}) . mapping _Nat

-- | The explicit authentication flows.
cupcExplicitAuthFlows :: Lens' CreateUserPoolClient [ExplicitAuthFlowsType]
cupcExplicitAuthFlows = lens _cupcExplicitAuthFlows (\ s a -> s{_cupcExplicitAuthFlows = a}) . _Default . _Coerce

-- | A list of provider names for the identity providers that are supported on this client.
cupcSupportedIdentityProviders :: Lens' CreateUserPoolClient [Text]
cupcSupportedIdentityProviders = lens _cupcSupportedIdentityProviders (\ s a -> s{_cupcSupportedIdentityProviders = a}) . _Default . _Coerce

-- | A list of allowed logout URLs for the identity providers.
cupcLogoutURLs :: Lens' CreateUserPoolClient [Text]
cupcLogoutURLs = lens _cupcLogoutURLs (\ s a -> s{_cupcLogoutURLs = a}) . _Default . _Coerce

-- | Set to @True@ if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
cupcAllowedOAuthFlowsUserPoolClient :: Lens' CreateUserPoolClient (Maybe Bool)
cupcAllowedOAuthFlowsUserPoolClient = lens _cupcAllowedOAuthFlowsUserPoolClient (\ s a -> s{_cupcAllowedOAuthFlowsUserPoolClient = a})

-- | Boolean to specify whether you want to generate a secret for the user pool client being created.
cupcGenerateSecret :: Lens' CreateUserPoolClient (Maybe Bool)
cupcGenerateSecret = lens _cupcGenerateSecret (\ s a -> s{_cupcGenerateSecret = a})

-- | The default redirect URI. Must be in the @CallbackURLs@ list.
cupcDefaultRedirectURI :: Lens' CreateUserPoolClient (Maybe Text)
cupcDefaultRedirectURI = lens _cupcDefaultRedirectURI (\ s a -> s{_cupcDefaultRedirectURI = a})

-- | The write attributes.
cupcWriteAttributes :: Lens' CreateUserPoolClient [Text]
cupcWriteAttributes = lens _cupcWriteAttributes (\ s a -> s{_cupcWriteAttributes = a}) . _Default . _Coerce

-- | The read attributes.
cupcReadAttributes :: Lens' CreateUserPoolClient [Text]
cupcReadAttributes = lens _cupcReadAttributes (\ s a -> s{_cupcReadAttributes = a}) . _Default . _Coerce

-- | A list of allowed @OAuth@ scopes. Currently supported values are @"phone"@ , @"email"@ , @"openid"@ , and @"Cognito"@ .
cupcAllowedOAuthScopes :: Lens' CreateUserPoolClient [Text]
cupcAllowedOAuthScopes = lens _cupcAllowedOAuthScopes (\ s a -> s{_cupcAllowedOAuthScopes = a}) . _Default . _Coerce

-- | Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint. Set to @token@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
cupcAllowedOAuthFlows :: Lens' CreateUserPoolClient [OAuthFlowType]
cupcAllowedOAuthFlows = lens _cupcAllowedOAuthFlows (\ s a -> s{_cupcAllowedOAuthFlows = a}) . _Default . _Coerce

-- | The Amazon Pinpoint analytics configuration for collecting metrics for this user pool.
cupcAnalyticsConfiguration :: Lens' CreateUserPoolClient (Maybe AnalyticsConfigurationType)
cupcAnalyticsConfiguration = lens _cupcAnalyticsConfiguration (\ s a -> s{_cupcAnalyticsConfiguration = a})

-- | A list of allowed callback URLs for the identity providers.
cupcCallbackURLs :: Lens' CreateUserPoolClient [Text]
cupcCallbackURLs = lens _cupcCallbackURLs (\ s a -> s{_cupcCallbackURLs = a}) . _Default . _Coerce

-- | The user pool ID for the user pool where you want to create a user pool client.
cupcUserPoolId :: Lens' CreateUserPoolClient Text
cupcUserPoolId = lens _cupcUserPoolId (\ s a -> s{_cupcUserPoolId = a})

-- | The client name for the user pool client you would like to create.
cupcClientName :: Lens' CreateUserPoolClient Text
cupcClientName = lens _cupcClientName (\ s a -> s{_cupcClientName = a})

instance AWSRequest CreateUserPoolClient where
        type Rs CreateUserPoolClient =
             CreateUserPoolClientResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 CreateUserPoolClientResponse' <$>
                   (x .?> "UserPoolClient") <*> (pure (fromEnum s)))

instance Hashable CreateUserPoolClient where

instance NFData CreateUserPoolClient where

instance ToHeaders CreateUserPoolClient where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.CreateUserPoolClient"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateUserPoolClient where
        toJSON CreateUserPoolClient'{..}
          = object
              (catMaybes
                 [("RefreshTokenValidity" .=) <$>
                    _cupcRefreshTokenValidity,
                  ("ExplicitAuthFlows" .=) <$> _cupcExplicitAuthFlows,
                  ("SupportedIdentityProviders" .=) <$>
                    _cupcSupportedIdentityProviders,
                  ("LogoutURLs" .=) <$> _cupcLogoutURLs,
                  ("AllowedOAuthFlowsUserPoolClient" .=) <$>
                    _cupcAllowedOAuthFlowsUserPoolClient,
                  ("GenerateSecret" .=) <$> _cupcGenerateSecret,
                  ("DefaultRedirectURI" .=) <$>
                    _cupcDefaultRedirectURI,
                  ("WriteAttributes" .=) <$> _cupcWriteAttributes,
                  ("ReadAttributes" .=) <$> _cupcReadAttributes,
                  ("AllowedOAuthScopes" .=) <$>
                    _cupcAllowedOAuthScopes,
                  ("AllowedOAuthFlows" .=) <$> _cupcAllowedOAuthFlows,
                  ("AnalyticsConfiguration" .=) <$>
                    _cupcAnalyticsConfiguration,
                  ("CallbackURLs" .=) <$> _cupcCallbackURLs,
                  Just ("UserPoolId" .= _cupcUserPoolId),
                  Just ("ClientName" .= _cupcClientName)])

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
  { _cupcrsUserPoolClient :: !(Maybe UserPoolClientType)
  , _cupcrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUserPoolClientResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupcrsUserPoolClient' - The user pool client that was just created.
--
-- * 'cupcrsResponseStatus' - -- | The response status code.
createUserPoolClientResponse
    :: Int -- ^ 'cupcrsResponseStatus'
    -> CreateUserPoolClientResponse
createUserPoolClientResponse pResponseStatus_ =
  CreateUserPoolClientResponse'
    {_cupcrsUserPoolClient = Nothing, _cupcrsResponseStatus = pResponseStatus_}


-- | The user pool client that was just created.
cupcrsUserPoolClient :: Lens' CreateUserPoolClientResponse (Maybe UserPoolClientType)
cupcrsUserPoolClient = lens _cupcrsUserPoolClient (\ s a -> s{_cupcrsUserPoolClient = a})

-- | -- | The response status code.
cupcrsResponseStatus :: Lens' CreateUserPoolClientResponse Int
cupcrsResponseStatus = lens _cupcrsResponseStatus (\ s a -> s{_cupcrsResponseStatus = a})

instance NFData CreateUserPoolClientResponse where
