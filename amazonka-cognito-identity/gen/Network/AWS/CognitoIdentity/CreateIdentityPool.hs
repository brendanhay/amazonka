{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.CreateIdentityPool
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new identity pool. The identity pool is a store of user
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
    , ciprqSupportedLoginProviders
    , ciprqDeveloperProviderName
    , ciprqOpenIdConnectProviderARNs
    , ciprqIdentityPoolName
    , ciprqAllowUnauthenticatedIdentities

    -- * Response
    , IdentityPool
    -- ** Response constructor
    , identityPool
    -- ** Response lenses
    , ipSupportedLoginProviders
    , ipDeveloperProviderName
    , ipOpenIdConnectProviderARNs
    , ipIdentityPoolId
    , ipIdentityPoolName
    , ipAllowUnauthenticatedIdentities
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the CreateIdentityPool action.
--
-- /See:/ 'createIdentityPool' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciprqSupportedLoginProviders'
--
-- * 'ciprqDeveloperProviderName'
--
-- * 'ciprqOpenIdConnectProviderARNs'
--
-- * 'ciprqIdentityPoolName'
--
-- * 'ciprqAllowUnauthenticatedIdentities'
data CreateIdentityPool = CreateIdentityPool'
    { _ciprqSupportedLoginProviders        :: !(Maybe (Map Text Text))
    , _ciprqDeveloperProviderName          :: !(Maybe Text)
    , _ciprqOpenIdConnectProviderARNs      :: !(Maybe [Text])
    , _ciprqIdentityPoolName               :: !Text
    , _ciprqAllowUnauthenticatedIdentities :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateIdentityPool' smart constructor.
createIdentityPool :: Text -> Bool -> CreateIdentityPool
createIdentityPool pIdentityPoolName pAllowUnauthenticatedIdentities =
    CreateIdentityPool'
    { _ciprqSupportedLoginProviders = Nothing
    , _ciprqDeveloperProviderName = Nothing
    , _ciprqOpenIdConnectProviderARNs = Nothing
    , _ciprqIdentityPoolName = pIdentityPoolName
    , _ciprqAllowUnauthenticatedIdentities = pAllowUnauthenticatedIdentities
    }

-- | Optional key:value pairs mapping provider names to provider app IDs.
ciprqSupportedLoginProviders :: Lens' CreateIdentityPool (HashMap Text Text)
ciprqSupportedLoginProviders = lens _ciprqSupportedLoginProviders (\ s a -> s{_ciprqSupportedLoginProviders = a}) . _Default . _Map;

-- | The \"domain\" by which Cognito will refer to your users. This name acts
-- as a placeholder that allows your backend and the Cognito service to
-- communicate about the developer provider. For the
-- @DeveloperProviderName@, you can use letters as well as period (@.@),
-- underscore (@_@), and dash (@-@).
--
-- Once you have set a developer provider name, you cannot change it.
-- Please take care in setting this parameter.
ciprqDeveloperProviderName :: Lens' CreateIdentityPool (Maybe Text)
ciprqDeveloperProviderName = lens _ciprqDeveloperProviderName (\ s a -> s{_ciprqDeveloperProviderName = a});

-- | A list of OpendID Connect provider ARNs.
ciprqOpenIdConnectProviderARNs :: Lens' CreateIdentityPool [Text]
ciprqOpenIdConnectProviderARNs = lens _ciprqOpenIdConnectProviderARNs (\ s a -> s{_ciprqOpenIdConnectProviderARNs = a}) . _Default;

-- | A string that you provide.
ciprqIdentityPoolName :: Lens' CreateIdentityPool Text
ciprqIdentityPoolName = lens _ciprqIdentityPoolName (\ s a -> s{_ciprqIdentityPoolName = a});

-- | TRUE if the identity pool supports unauthenticated logins.
ciprqAllowUnauthenticatedIdentities :: Lens' CreateIdentityPool Bool
ciprqAllowUnauthenticatedIdentities = lens _ciprqAllowUnauthenticatedIdentities (\ s a -> s{_ciprqAllowUnauthenticatedIdentities = a});

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
                 _ciprqSupportedLoginProviders,
               "DeveloperProviderName" .=
                 _ciprqDeveloperProviderName,
               "OpenIdConnectProviderARNs" .=
                 _ciprqOpenIdConnectProviderARNs,
               "IdentityPoolName" .= _ciprqIdentityPoolName,
               "AllowUnauthenticatedIdentities" .=
                 _ciprqAllowUnauthenticatedIdentities]

instance ToPath CreateIdentityPool where
        toPath = const "/"

instance ToQuery CreateIdentityPool where
        toQuery = const mempty
