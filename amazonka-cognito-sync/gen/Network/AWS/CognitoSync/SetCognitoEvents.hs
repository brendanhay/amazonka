{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoSync.SetCognitoEvents
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

-- | Sets the AWS Lambda function for a given event type for an identity
-- pool. This request only updates the key\/value pair specified. Other
-- key\/values pairs are not updated. To remove a key value pair, pass a
-- empty value for the particular key.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_SetCognitoEvents.html>
module Network.AWS.CognitoSync.SetCognitoEvents
    (
    -- * Request
      SetCognitoEvents
    -- ** Request constructor
    , setCognitoEvents
    -- ** Request lenses
    , sceIdentityPoolId
    , sceEvents

    -- * Response
    , SetCognitoEventsResponse
    -- ** Response constructor
    , setCognitoEventsResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CognitoSync.Types

-- | /See:/ 'setCognitoEvents' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sceIdentityPoolId'
--
-- * 'sceEvents'
data SetCognitoEvents = SetCognitoEvents'{_sceIdentityPoolId :: Text, _sceEvents :: Map Text Text} deriving (Eq, Read, Show)

-- | 'SetCognitoEvents' smart constructor.
setCognitoEvents :: Text -> SetCognitoEvents
setCognitoEvents pIdentityPoolId = SetCognitoEvents'{_sceIdentityPoolId = pIdentityPoolId, _sceEvents = mempty};

-- | The Cognito Identity Pool to use when configuring Cognito Events
sceIdentityPoolId :: Lens' SetCognitoEvents Text
sceIdentityPoolId = lens _sceIdentityPoolId (\ s a -> s{_sceIdentityPoolId = a});

-- | The events to configure
sceEvents :: Lens' SetCognitoEvents (Map Text Text)
sceEvents = lens _sceEvents (\ s a -> s{_sceEvents = a}) . _Map;

instance AWSRequest SetCognitoEvents where
        type Sv SetCognitoEvents = CognitoSync
        type Rs SetCognitoEvents = SetCognitoEventsResponse
        request = postJSON
        response = receiveNull SetCognitoEventsResponse'

instance ToHeaders SetCognitoEvents where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetCognitoEvents where
        toJSON SetCognitoEvents'{..}
          = object ["Events" .= _sceEvents]

instance ToPath SetCognitoEvents where
        toPath SetCognitoEvents'{..}
          = mconcat
              ["/identitypools/", toText _sceIdentityPoolId,
               "/events"]

instance ToQuery SetCognitoEvents where
        toQuery = const mempty

-- | /See:/ 'setCognitoEventsResponse' smart constructor.
data SetCognitoEventsResponse = SetCognitoEventsResponse' deriving (Eq, Read, Show)

-- | 'SetCognitoEventsResponse' smart constructor.
setCognitoEventsResponse :: SetCognitoEventsResponse
setCognitoEventsResponse = SetCognitoEventsResponse';
