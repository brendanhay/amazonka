{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.UpdateIdentityPool
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates a user pool.
--
-- You must use AWS Developer credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_UpdateIdentityPool.html>
module Network.AWS.CognitoIdentity.UpdateIdentityPool
    (
    -- * Request
      UpdateIdentityPool
    -- ** Request constructor
    , updateIdentityPool
    -- ** Request lenses
    , uipSupportedLoginProviders
    , uipDeveloperProviderName
    , uipOpenIdConnectProviderARNs
    , uipIdentityPoolId
    , uipIdentityPoolName
    , uipAllowUnauthenticatedIdentities

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

-- | An object representing a Cognito identity pool.
--
-- /See:/ 'updateIdentityPool' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uipSupportedLoginProviders'
--
-- * 'uipDeveloperProviderName'
--
-- * 'uipOpenIdConnectProviderARNs'
--
-- * 'uipIdentityPoolId'
--
-- * 'uipIdentityPoolName'
--
-- * 'uipAllowUnauthenticatedIdentities'
data UpdateIdentityPool = UpdateIdentityPool'
    { _uipSupportedLoginProviders        :: !(Maybe (Map Text Text))
    , _uipDeveloperProviderName          :: !(Maybe Text)
    , _uipOpenIdConnectProviderARNs      :: !(Maybe [Text])
    , _uipIdentityPoolId                 :: !Text
    , _uipIdentityPoolName               :: !Text
    , _uipAllowUnauthenticatedIdentities :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateIdentityPool' smart constructor.
updateIdentityPool :: Text -> Text -> Bool -> UpdateIdentityPool
updateIdentityPool pIdentityPoolId_ pIdentityPoolName_ pAllowUnauthenticatedIdentities_ =
    UpdateIdentityPool'
    { _uipSupportedLoginProviders = Nothing
    , _uipDeveloperProviderName = Nothing
    , _uipOpenIdConnectProviderARNs = Nothing
    , _uipIdentityPoolId = pIdentityPoolId_
    , _uipIdentityPoolName = pIdentityPoolName_
    , _uipAllowUnauthenticatedIdentities = pAllowUnauthenticatedIdentities_
    }

-- | Optional key:value pairs mapping provider names to provider app IDs.
uipSupportedLoginProviders :: Lens' UpdateIdentityPool (HashMap Text Text)
uipSupportedLoginProviders = lens _uipSupportedLoginProviders (\ s a -> s{_uipSupportedLoginProviders = a}) . _Default . _Map;

-- | The \"domain\" by which Cognito will refer to your users.
uipDeveloperProviderName :: Lens' UpdateIdentityPool (Maybe Text)
uipDeveloperProviderName = lens _uipDeveloperProviderName (\ s a -> s{_uipDeveloperProviderName = a});

-- | A list of OpendID Connect provider ARNs.
uipOpenIdConnectProviderARNs :: Lens' UpdateIdentityPool [Text]
uipOpenIdConnectProviderARNs = lens _uipOpenIdConnectProviderARNs (\ s a -> s{_uipOpenIdConnectProviderARNs = a}) . _Default;

-- | An identity pool ID in the format REGION:GUID.
uipIdentityPoolId :: Lens' UpdateIdentityPool Text
uipIdentityPoolId = lens _uipIdentityPoolId (\ s a -> s{_uipIdentityPoolId = a});

-- | A string that you provide.
uipIdentityPoolName :: Lens' UpdateIdentityPool Text
uipIdentityPoolName = lens _uipIdentityPoolName (\ s a -> s{_uipIdentityPoolName = a});

-- | TRUE if the identity pool supports unauthenticated logins.
uipAllowUnauthenticatedIdentities :: Lens' UpdateIdentityPool Bool
uipAllowUnauthenticatedIdentities = lens _uipAllowUnauthenticatedIdentities (\ s a -> s{_uipAllowUnauthenticatedIdentities = a});

instance AWSRequest UpdateIdentityPool where
        type Sv UpdateIdentityPool = CognitoIdentity
        type Rs UpdateIdentityPool = IdentityPool
        request = postJSON "UpdateIdentityPool"
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders UpdateIdentityPool where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.UpdateIdentityPool" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateIdentityPool where
        toJSON UpdateIdentityPool'{..}
          = object
              ["SupportedLoginProviders" .=
                 _uipSupportedLoginProviders,
               "DeveloperProviderName" .= _uipDeveloperProviderName,
               "OpenIdConnectProviderARNs" .=
                 _uipOpenIdConnectProviderARNs,
               "IdentityPoolId" .= _uipIdentityPoolId,
               "IdentityPoolName" .= _uipIdentityPoolName,
               "AllowUnauthenticatedIdentities" .=
                 _uipAllowUnauthenticatedIdentities]

instance ToPath UpdateIdentityPool where
        toPath = const "/"

instance ToQuery UpdateIdentityPool where
        toQuery = const mempty
