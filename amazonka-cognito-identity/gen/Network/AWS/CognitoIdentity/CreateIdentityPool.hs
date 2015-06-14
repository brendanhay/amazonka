{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoIdentity.CreateIdentityPool
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a new identity pool. The identity pool is a store of user
-- identity information that is specific to your AWS account. The limit on
-- identity pools is 60 per account. You must use AWS Developer credentials
-- to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_CreateIdentityPool.html>
module Network.AWS.CognitoIdentity.CreateIdentityPool
    (
    -- * Request
      CreateIdentityPool
    -- ** Request constructor
    , createIdentityPool
    -- ** Request lenses
    , cipSupportedLoginProviders
    , cipOpenIdConnectProviderARNs
    , cipIdentityPoolName
    , cipAllowUnauthenticatedIdentities
    , cipDeveloperProviderName

    -- * Response
    , IdentityPool
    -- ** Response constructor
    , identityPool
    -- ** Response lenses
    , ipSupportedLoginProviders
    , ipOpenIdConnectProviderARNs
    , ipIdentityPoolId
    , ipIdentityPoolName
    , ipAllowUnauthenticatedIdentities
    , ipDeveloperProviderName
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CognitoIdentity.Types

-- | /See:/ 'createIdentityPool' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cipSupportedLoginProviders'
--
-- * 'cipOpenIdConnectProviderARNs'
--
-- * 'cipIdentityPoolName'
--
-- * 'cipAllowUnauthenticatedIdentities'
--
-- * 'cipDeveloperProviderName'
data CreateIdentityPool = CreateIdentityPool'{_cipSupportedLoginProviders :: HashMap Text Text, _cipOpenIdConnectProviderARNs :: [Text], _cipIdentityPoolName :: Text, _cipAllowUnauthenticatedIdentities :: Bool, _cipDeveloperProviderName :: Text} deriving (Eq, Read, Show)

-- | 'CreateIdentityPool' smart constructor.
createIdentityPool :: Text -> Bool -> Text -> CreateIdentityPool
createIdentityPool pIdentityPoolName pAllowUnauthenticatedIdentities pDeveloperProviderName = CreateIdentityPool'{_cipSupportedLoginProviders = mempty, _cipOpenIdConnectProviderARNs = mempty, _cipIdentityPoolName = pIdentityPoolName, _cipAllowUnauthenticatedIdentities = pAllowUnauthenticatedIdentities, _cipDeveloperProviderName = pDeveloperProviderName};

-- | Optional key:value pairs mapping provider names to provider app IDs.
cipSupportedLoginProviders :: Lens' CreateIdentityPool (HashMap Text Text)
cipSupportedLoginProviders = lens _cipSupportedLoginProviders (\ s a -> s{_cipSupportedLoginProviders = a}) . _Coerce;

-- | A list of OpendID Connect provider ARNs.
cipOpenIdConnectProviderARNs :: Lens' CreateIdentityPool [Text]
cipOpenIdConnectProviderARNs = lens _cipOpenIdConnectProviderARNs (\ s a -> s{_cipOpenIdConnectProviderARNs = a});

-- | A string that you provide.
cipIdentityPoolName :: Lens' CreateIdentityPool Text
cipIdentityPoolName = lens _cipIdentityPoolName (\ s a -> s{_cipIdentityPoolName = a});

-- | TRUE if the identity pool supports unauthenticated logins.
cipAllowUnauthenticatedIdentities :: Lens' CreateIdentityPool Bool
cipAllowUnauthenticatedIdentities = lens _cipAllowUnauthenticatedIdentities (\ s a -> s{_cipAllowUnauthenticatedIdentities = a});

-- | The \"domain\" by which Cognito will refer to your users. This name acts
-- as a placeholder that allows your backend and the Cognito service to
-- communicate about the developer provider. For the
-- @DeveloperProviderName@, you can use letters as well as period (@.@),
-- underscore (@_@), and dash (@-@).
--
-- Once you have set a developer provider name, you cannot change it.
-- Please take care in setting this parameter.
cipDeveloperProviderName :: Lens' CreateIdentityPool Text
cipDeveloperProviderName = lens _cipDeveloperProviderName (\ s a -> s{_cipDeveloperProviderName = a});

instance AWSRequest CreateIdentityPool where
        type Sv CreateIdentityPool = CognitoIdentity
        type Rs CreateIdentityPool = IdentityPool
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders CreateIdentityPool where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.CreateIdentityPool" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateIdentityPool where
        toJSON CreateIdentityPool'{..}
          = object
              ["SupportedLoginProviders" .=
                 _cipSupportedLoginProviders,
               "OpenIdConnectProviderARNs" .=
                 _cipOpenIdConnectProviderARNs,
               "IdentityPoolName" .= _cipIdentityPoolName,
               "AllowUnauthenticatedIdentities" .=
                 _cipAllowUnauthenticatedIdentities,
               "DeveloperProviderName" .= _cipDeveloperProviderName]

instance ToPath CreateIdentityPool where
        toPath = const "/"

instance ToQuery CreateIdentityPool where
        toQuery = const mempty
