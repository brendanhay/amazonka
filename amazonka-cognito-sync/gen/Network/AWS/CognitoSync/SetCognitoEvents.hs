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
-- Module      : Network.AWS.CognitoSync.SetCognitoEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the AWS Lambda function for a given event type for an identity pool. This request only updates the key/value pair specified. Other key/values pairs are not updated. To remove a key value pair, pass a empty value for the particular key.
--
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
--
module Network.AWS.CognitoSync.SetCognitoEvents
    (
    -- * Creating a Request
      setCognitoEvents
    , SetCognitoEvents
    -- * Request Lenses
    , sceIdentityPoolId
    , sceEvents

    -- * Destructuring the Response
    , setCognitoEventsResponse
    , SetCognitoEventsResponse
    ) where

import Network.AWS.CognitoSync.Types
import Network.AWS.CognitoSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to configure Cognito Events"
--
--
--
-- /See:/ 'setCognitoEvents' smart constructor.
data SetCognitoEvents = SetCognitoEvents'
  { _sceIdentityPoolId :: !Text
  , _sceEvents         :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetCognitoEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sceIdentityPoolId' - The Cognito Identity Pool to use when configuring Cognito Events
--
-- * 'sceEvents' - The events to configure
setCognitoEvents
    :: Text -- ^ 'sceIdentityPoolId'
    -> SetCognitoEvents
setCognitoEvents pIdentityPoolId_ =
  SetCognitoEvents' {_sceIdentityPoolId = pIdentityPoolId_, _sceEvents = mempty}


-- | The Cognito Identity Pool to use when configuring Cognito Events
sceIdentityPoolId :: Lens' SetCognitoEvents Text
sceIdentityPoolId = lens _sceIdentityPoolId (\ s a -> s{_sceIdentityPoolId = a})

-- | The events to configure
sceEvents :: Lens' SetCognitoEvents (HashMap Text Text)
sceEvents = lens _sceEvents (\ s a -> s{_sceEvents = a}) . _Map

instance AWSRequest SetCognitoEvents where
        type Rs SetCognitoEvents = SetCognitoEventsResponse
        request = postJSON cognitoSync
        response = receiveNull SetCognitoEventsResponse'

instance Hashable SetCognitoEvents where

instance NFData SetCognitoEvents where

instance ToHeaders SetCognitoEvents where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetCognitoEvents where
        toJSON SetCognitoEvents'{..}
          = object (catMaybes [Just ("Events" .= _sceEvents)])

instance ToPath SetCognitoEvents where
        toPath SetCognitoEvents'{..}
          = mconcat
              ["/identitypools/", toBS _sceIdentityPoolId,
               "/events"]

instance ToQuery SetCognitoEvents where
        toQuery = const mempty

-- | /See:/ 'setCognitoEventsResponse' smart constructor.
data SetCognitoEventsResponse =
  SetCognitoEventsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetCognitoEventsResponse' with the minimum fields required to make a request.
--
setCognitoEventsResponse
    :: SetCognitoEventsResponse
setCognitoEventsResponse = SetCognitoEventsResponse'


instance NFData SetCognitoEventsResponse where
