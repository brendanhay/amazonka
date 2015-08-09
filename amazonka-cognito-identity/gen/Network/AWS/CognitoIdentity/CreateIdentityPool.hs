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
-- Module      : Network.AWS.CognitoIdentity.CreateIdentityPool
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new identity pool. The identity pool is a store of user
-- identity information that is specific to your AWS account. The limit on
-- identity pools is 60 per account. You must use AWS Developer credentials
-- to call this API.
--
-- /See:/ <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_CreateIdentityPool.html AWS API Reference> for CreateIdentityPool.
module Network.AWS.CognitoIdentity.CreateIdentityPool
    (
    -- * Creating a Request
      createIdentityPool
    , CreateIdentityPool
    -- * Request Lenses
    , cipSupportedLoginProviders
    , cipDeveloperProviderName
    , cipOpenIdConnectProviderARNs
    , cipIdentityPoolName
    , cipAllowUnauthenticatedIdentities

    -- * Destructuring the Response
    , identityPool
    , IdentityPool
    -- * Response Lenses
    , ipSupportedLoginProviders
    , ipDeveloperProviderName
    , ipOpenIdConnectProviderARNs
    , ipIdentityPoolId
    , ipIdentityPoolName
    , ipAllowUnauthenticatedIdentities
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.CognitoIdentity.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the CreateIdentityPool action.
--
-- /See:/ 'createIdentityPool' smart constructor.
data CreateIdentityPool = CreateIdentityPool'
    { _cipSupportedLoginProviders        :: !(Maybe (Map Text Text))
    , _cipDeveloperProviderName          :: !(Maybe Text)
    , _cipOpenIdConnectProviderARNs      :: !(Maybe [Text])
    , _cipIdentityPoolName               :: !Text
    , _cipAllowUnauthenticatedIdentities :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateIdentityPool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cipSupportedLoginProviders'
--
-- * 'cipDeveloperProviderName'
--
-- * 'cipOpenIdConnectProviderARNs'
--
-- * 'cipIdentityPoolName'
--
-- * 'cipAllowUnauthenticatedIdentities'
createIdentityPool
    :: Text -- ^ 'cipIdentityPoolName'
    -> Bool -- ^ 'cipAllowUnauthenticatedIdentities'
    -> CreateIdentityPool
createIdentityPool pIdentityPoolName_ pAllowUnauthenticatedIdentities_ =
    CreateIdentityPool'
    { _cipSupportedLoginProviders = Nothing
    , _cipDeveloperProviderName = Nothing
    , _cipOpenIdConnectProviderARNs = Nothing
    , _cipIdentityPoolName = pIdentityPoolName_
    , _cipAllowUnauthenticatedIdentities = pAllowUnauthenticatedIdentities_
    }

-- | Optional key:value pairs mapping provider names to provider app IDs.
cipSupportedLoginProviders :: Lens' CreateIdentityPool (HashMap Text Text)
cipSupportedLoginProviders = lens _cipSupportedLoginProviders (\ s a -> s{_cipSupportedLoginProviders = a}) . _Default . _Map;

-- | The \"domain\" by which Cognito will refer to your users. This name acts
-- as a placeholder that allows your backend and the Cognito service to
-- communicate about the developer provider. For the
-- 'DeveloperProviderName', you can use letters as well as period ('.'),
-- underscore ('_'), and dash ('-').
--
-- Once you have set a developer provider name, you cannot change it.
-- Please take care in setting this parameter.
cipDeveloperProviderName :: Lens' CreateIdentityPool (Maybe Text)
cipDeveloperProviderName = lens _cipDeveloperProviderName (\ s a -> s{_cipDeveloperProviderName = a});

-- | A list of OpendID Connect provider ARNs.
cipOpenIdConnectProviderARNs :: Lens' CreateIdentityPool [Text]
cipOpenIdConnectProviderARNs = lens _cipOpenIdConnectProviderARNs (\ s a -> s{_cipOpenIdConnectProviderARNs = a}) . _Default . _Coerce;

-- | A string that you provide.
cipIdentityPoolName :: Lens' CreateIdentityPool Text
cipIdentityPoolName = lens _cipIdentityPoolName (\ s a -> s{_cipIdentityPoolName = a});

-- | TRUE if the identity pool supports unauthenticated logins.
cipAllowUnauthenticatedIdentities :: Lens' CreateIdentityPool Bool
cipAllowUnauthenticatedIdentities = lens _cipAllowUnauthenticatedIdentities (\ s a -> s{_cipAllowUnauthenticatedIdentities = a});

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
               "DeveloperProviderName" .= _cipDeveloperProviderName,
               "OpenIdConnectProviderARNs" .=
                 _cipOpenIdConnectProviderARNs,
               "IdentityPoolName" .= _cipIdentityPoolName,
               "AllowUnauthenticatedIdentities" .=
                 _cipAllowUnauthenticatedIdentities]

instance ToPath CreateIdentityPool where
        toPath = const "/"

instance ToQuery CreateIdentityPool where
        toQuery = const mempty
