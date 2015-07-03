{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoIdentity.GetOpenIdToken
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Gets an OpenID token, using a known Cognito ID. This known Cognito ID is
-- returned by GetId. You can optionally add additional logins for the
-- identity. Supplying multiple logins creates an implicit link.
--
-- The OpenId token is valid for 15 minutes.
--
-- This is a public API. You do not need any credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetOpenIdToken.html>
module Network.AWS.CognitoIdentity.GetOpenIdToken
    (
    -- * Request
      GetOpenIdToken
    -- ** Request constructor
    , getOpenIdToken
    -- ** Request lenses
    , goitLogins
    , goitIdentityId

    -- * Response
    , GetOpenIdTokenResponse
    -- ** Response constructor
    , getOpenIdTokenResponse
    -- ** Response lenses
    , goitrToken
    , goitrIdentityId
    , goitrStatus
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the GetOpenIdToken action.
--
-- /See:/ 'getOpenIdToken' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goitLogins'
--
-- * 'goitIdentityId'
data GetOpenIdToken = GetOpenIdToken'
    { _goitLogins     :: !(Maybe (Map Text Text))
    , _goitIdentityId :: !Text
    } deriving (Eq,Read,Show)

-- | 'GetOpenIdToken' smart constructor.
getOpenIdToken :: Text -> GetOpenIdToken
getOpenIdToken pIdentityId =
    GetOpenIdToken'
    { _goitLogins = Nothing
    , _goitIdentityId = pIdentityId
    }

-- | A set of optional name-value pairs that map provider names to provider
-- tokens. When using graph.facebook.com and www.amazon.com, supply the
-- access_token returned from the provider\'s authflow. For
-- accounts.google.com or any other OpenId Connect provider, always include
-- the id_token.
goitLogins :: Lens' GetOpenIdToken (HashMap Text Text)
goitLogins = lens _goitLogins (\ s a -> s{_goitLogins = a}) . _Default . _Map;

-- | A unique identifier in the format REGION:GUID.
goitIdentityId :: Lens' GetOpenIdToken Text
goitIdentityId = lens _goitIdentityId (\ s a -> s{_goitIdentityId = a});

instance AWSRequest GetOpenIdToken where
        type Sv GetOpenIdToken = CognitoIdentity
        type Rs GetOpenIdToken = GetOpenIdTokenResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetOpenIdTokenResponse' <$>
                   (x .?> "Token") <*> (x .?> "IdentityId") <*>
                     (pure (fromEnum s)))

instance ToHeaders GetOpenIdToken where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.GetOpenIdToken" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetOpenIdToken where
        toJSON GetOpenIdToken'{..}
          = object
              ["Logins" .= _goitLogins,
               "IdentityId" .= _goitIdentityId]

instance ToPath GetOpenIdToken where
        toPath = const "/"

instance ToQuery GetOpenIdToken where
        toQuery = const mempty

-- | Returned in response to a successful GetOpenIdToken request.
--
-- /See:/ 'getOpenIdTokenResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goitrToken'
--
-- * 'goitrIdentityId'
--
-- * 'goitrStatus'
data GetOpenIdTokenResponse = GetOpenIdTokenResponse'
    { _goitrToken      :: !(Maybe Text)
    , _goitrIdentityId :: !(Maybe Text)
    , _goitrStatus     :: !Int
    } deriving (Eq,Read,Show)

-- | 'GetOpenIdTokenResponse' smart constructor.
getOpenIdTokenResponse :: Int -> GetOpenIdTokenResponse
getOpenIdTokenResponse pStatus =
    GetOpenIdTokenResponse'
    { _goitrToken = Nothing
    , _goitrIdentityId = Nothing
    , _goitrStatus = pStatus
    }

-- | An OpenID token, valid for 15 minutes.
goitrToken :: Lens' GetOpenIdTokenResponse (Maybe Text)
goitrToken = lens _goitrToken (\ s a -> s{_goitrToken = a});

-- | A unique identifier in the format REGION:GUID. Note that the IdentityId
-- returned may not match the one passed on input.
goitrIdentityId :: Lens' GetOpenIdTokenResponse (Maybe Text)
goitrIdentityId = lens _goitrIdentityId (\ s a -> s{_goitrIdentityId = a});

-- | FIXME: Undocumented member.
goitrStatus :: Lens' GetOpenIdTokenResponse Int
goitrStatus = lens _goitrStatus (\ s a -> s{_goitrStatus = a});
