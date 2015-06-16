{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoSync.GetCognitoEvents
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

-- | Gets the events and the corresponding Lambda functions associated with
-- an identity pool
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_GetCognitoEvents.html>
module Network.AWS.CognitoSync.GetCognitoEvents
    (
    -- * Request
      GetCognitoEvents
    -- ** Request constructor
    , getCognitoEvents
    -- ** Request lenses
    , gceIdentityPoolId

    -- * Response
    , GetCognitoEventsResponse
    -- ** Response constructor
    , getCognitoEventsResponse
    -- ** Response lenses
    , gcerEvents
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CognitoSync.Types

-- | /See:/ 'getCognitoEvents' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gceIdentityPoolId'
newtype GetCognitoEvents = GetCognitoEvents'{_gceIdentityPoolId :: Text} deriving (Eq, Read, Show)

-- | 'GetCognitoEvents' smart constructor.
getCognitoEvents :: Text -> GetCognitoEvents
getCognitoEvents pIdentityPoolId = GetCognitoEvents'{_gceIdentityPoolId = pIdentityPoolId};

-- | The Cognito Identity Pool ID for the request
gceIdentityPoolId :: Lens' GetCognitoEvents Text
gceIdentityPoolId = lens _gceIdentityPoolId (\ s a -> s{_gceIdentityPoolId = a});

instance AWSRequest GetCognitoEvents where
        type Sv GetCognitoEvents = CognitoSync
        type Rs GetCognitoEvents = GetCognitoEventsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 GetCognitoEventsResponse' <$>
                   (x .?> "Events" .!@ mempty))

instance ToHeaders GetCognitoEvents where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetCognitoEvents where
        toPath GetCognitoEvents'{..}
          = mconcat
              ["/identitypools/", toText _gceIdentityPoolId,
               "/events"]

instance ToQuery GetCognitoEvents where
        toQuery = const mempty

-- | /See:/ 'getCognitoEventsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcerEvents'
newtype GetCognitoEventsResponse = GetCognitoEventsResponse'{_gcerEvents :: Maybe (Map Text Text)} deriving (Eq, Read, Show)

-- | 'GetCognitoEventsResponse' smart constructor.
getCognitoEventsResponse :: GetCognitoEventsResponse
getCognitoEventsResponse = GetCognitoEventsResponse'{_gcerEvents = Nothing};

-- | The Cognito Events returned from the GetCognitoEvents request
gcerEvents :: Lens' GetCognitoEventsResponse (Map Text Text)
gcerEvents = lens _gcerEvents (\ s a -> s{_gcerEvents = a}) . _Default . _Map;
