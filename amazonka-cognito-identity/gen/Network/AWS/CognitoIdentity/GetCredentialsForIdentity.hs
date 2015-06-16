{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoIdentity.GetCredentialsForIdentity
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

-- | Returns credentials for the the provided identity ID. Any provided
-- logins will be validated against supported login providers. If the token
-- is for cognito-identity.amazonaws.com, it will be passed through to AWS
-- Security Token Service with the appropriate role for the token.
--
-- This is a public API. You do not need any credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetCredentialsForIdentity.html>
module Network.AWS.CognitoIdentity.GetCredentialsForIdentity
    (
    -- * Request
      GetCredentialsForIdentity
    -- ** Request constructor
    , getCredentialsForIdentity
    -- ** Request lenses
    , gcfiLogins
    , gcfiIdentityId

    -- * Response
    , GetCredentialsForIdentityResponse
    -- ** Response constructor
    , getCredentialsForIdentityResponse
    -- ** Response lenses
    , gcfirCredentials
    , gcfirIdentityId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CognitoIdentity.Types

-- | /See:/ 'getCredentialsForIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfiLogins'
--
-- * 'gcfiIdentityId'
data GetCredentialsForIdentity = GetCredentialsForIdentity'{_gcfiLogins :: Maybe (Map Text Text), _gcfiIdentityId :: Text} deriving (Eq, Read, Show)

-- | 'GetCredentialsForIdentity' smart constructor.
getCredentialsForIdentity :: Text -> GetCredentialsForIdentity
getCredentialsForIdentity pIdentityId = GetCredentialsForIdentity'{_gcfiLogins = Nothing, _gcfiIdentityId = pIdentityId};

-- | A set of optional name-value pairs that map provider names to provider
-- tokens.
gcfiLogins :: Lens' GetCredentialsForIdentity (Map Text Text)
gcfiLogins = lens _gcfiLogins (\ s a -> s{_gcfiLogins = a}) . _Default . _Map;

-- | A unique identifier in the format REGION:GUID.
gcfiIdentityId :: Lens' GetCredentialsForIdentity Text
gcfiIdentityId = lens _gcfiIdentityId (\ s a -> s{_gcfiIdentityId = a});

instance AWSRequest GetCredentialsForIdentity where
        type Sv GetCredentialsForIdentity = CognitoIdentity
        type Rs GetCredentialsForIdentity =
             GetCredentialsForIdentityResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetCredentialsForIdentityResponse' <$>
                   (x .?> "Credentials") <*> (x .?> "IdentityId"))

instance ToHeaders GetCredentialsForIdentity where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.GetCredentialsForIdentity"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCredentialsForIdentity where
        toJSON GetCredentialsForIdentity'{..}
          = object
              ["Logins" .= _gcfiLogins,
               "IdentityId" .= _gcfiIdentityId]

instance ToPath GetCredentialsForIdentity where
        toPath = const "/"

instance ToQuery GetCredentialsForIdentity where
        toQuery = const mempty

-- | /See:/ 'getCredentialsForIdentityResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfirCredentials'
--
-- * 'gcfirIdentityId'
data GetCredentialsForIdentityResponse = GetCredentialsForIdentityResponse'{_gcfirCredentials :: Maybe Credentials, _gcfirIdentityId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'GetCredentialsForIdentityResponse' smart constructor.
getCredentialsForIdentityResponse :: GetCredentialsForIdentityResponse
getCredentialsForIdentityResponse = GetCredentialsForIdentityResponse'{_gcfirCredentials = Nothing, _gcfirIdentityId = Nothing};

-- | Credentials for the the provided identity ID.
gcfirCredentials :: Lens' GetCredentialsForIdentityResponse (Maybe Credentials)
gcfirCredentials = lens _gcfirCredentials (\ s a -> s{_gcfirCredentials = a});

-- | A unique identifier in the format REGION:GUID.
gcfirIdentityId :: Lens' GetCredentialsForIdentityResponse (Maybe Text)
gcfirIdentityId = lens _gcfirIdentityId (\ s a -> s{_gcfirIdentityId = a});
