{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoIdentity.GetId
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

-- | Generates (or retrieves) a Cognito ID. Supplying multiple logins will
-- create an implicit linked account.
--
-- token+\";\"+tokenSecret.
--
-- This is a public API. You do not need any credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetId.html>
module Network.AWS.CognitoIdentity.GetId
    (
    -- * Request
      GetId
    -- ** Request constructor
    , getId
    -- ** Request lenses
    , giAccountId
    , giLogins
    , giIdentityPoolId

    -- * Response
    , GetIdResponse
    -- ** Response constructor
    , getIdResponse
    -- ** Response lenses
    , girIdentityId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CognitoIdentity.Types

-- | /See:/ 'getId' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giAccountId'
--
-- * 'giLogins'
--
-- * 'giIdentityPoolId'
data GetId = GetId'{_giAccountId :: Maybe Text, _giLogins :: Maybe (Map Text Text), _giIdentityPoolId :: Text} deriving (Eq, Read, Show)

-- | 'GetId' smart constructor.
getId :: Text -> GetId
getId pIdentityPoolId = GetId'{_giAccountId = Nothing, _giLogins = Nothing, _giIdentityPoolId = pIdentityPoolId};

-- | A standard AWS account ID (9+ digits).
giAccountId :: Lens' GetId (Maybe Text)
giAccountId = lens _giAccountId (\ s a -> s{_giAccountId = a});

-- | A set of optional name-value pairs that map provider names to provider
-- tokens.
--
-- The available provider names for @Logins@ are as follows:
--
-- -   Facebook: @graph.facebook.com@
-- -   Google: @accounts.google.com@
-- -   Amazon: @www.amazon.com@
-- -   Twitter: @www.twitter.com@
-- -   Digits: @www.digits.com@
giLogins :: Lens' GetId (HashMap Text Text)
giLogins = lens _giLogins (\ s a -> s{_giLogins = a}) . _Default . _Map;

-- | An identity pool ID in the format REGION:GUID.
giIdentityPoolId :: Lens' GetId Text
giIdentityPoolId = lens _giIdentityPoolId (\ s a -> s{_giIdentityPoolId = a});

instance AWSRequest GetId where
        type Sv GetId = CognitoIdentity
        type Rs GetId = GetIdResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x -> GetIdResponse' <$> (x .?> "IdentityId"))

instance ToHeaders GetId where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.GetId" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetId where
        toJSON GetId'{..}
          = object
              ["AccountId" .= _giAccountId, "Logins" .= _giLogins,
               "IdentityPoolId" .= _giIdentityPoolId]

instance ToPath GetId where
        toPath = const "/"

instance ToQuery GetId where
        toQuery = const mempty

-- | /See:/ 'getIdResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'girIdentityId'
newtype GetIdResponse = GetIdResponse'{_girIdentityId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'GetIdResponse' smart constructor.
getIdResponse :: GetIdResponse
getIdResponse = GetIdResponse'{_girIdentityId = Nothing};

-- | A unique identifier in the format REGION:GUID.
girIdentityId :: Lens' GetIdResponse (Maybe Text)
girIdentityId = lens _girIdentityId (\ s a -> s{_girIdentityId = a});
