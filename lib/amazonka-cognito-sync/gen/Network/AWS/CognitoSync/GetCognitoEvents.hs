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
-- Module      : Network.AWS.CognitoSync.GetCognitoEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the events and the corresponding Lambda functions associated with an identity pool.
--
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
--
module Network.AWS.CognitoSync.GetCognitoEvents
    (
    -- * Creating a Request
      getCognitoEvents
    , GetCognitoEvents
    -- * Request Lenses
    , gceIdentityPoolId

    -- * Destructuring the Response
    , getCognitoEventsResponse
    , GetCognitoEventsResponse
    -- * Response Lenses
    , gcersEvents
    , gcersResponseStatus
    ) where

import Network.AWS.CognitoSync.Types
import Network.AWS.CognitoSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request for a list of the configured Cognito Events
--
--
--
-- /See:/ 'getCognitoEvents' smart constructor.
newtype GetCognitoEvents = GetCognitoEvents'
  { _gceIdentityPoolId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCognitoEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gceIdentityPoolId' - The Cognito Identity Pool ID for the request
getCognitoEvents
    :: Text -- ^ 'gceIdentityPoolId'
    -> GetCognitoEvents
getCognitoEvents pIdentityPoolId_ =
  GetCognitoEvents' {_gceIdentityPoolId = pIdentityPoolId_}


-- | The Cognito Identity Pool ID for the request
gceIdentityPoolId :: Lens' GetCognitoEvents Text
gceIdentityPoolId = lens _gceIdentityPoolId (\ s a -> s{_gceIdentityPoolId = a})

instance AWSRequest GetCognitoEvents where
        type Rs GetCognitoEvents = GetCognitoEventsResponse
        request = get cognitoSync
        response
          = receiveJSON
              (\ s h x ->
                 GetCognitoEventsResponse' <$>
                   (x .?> "Events" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable GetCognitoEvents where

instance NFData GetCognitoEvents where

instance ToHeaders GetCognitoEvents where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetCognitoEvents where
        toPath GetCognitoEvents'{..}
          = mconcat
              ["/identitypools/", toBS _gceIdentityPoolId,
               "/events"]

instance ToQuery GetCognitoEvents where
        toQuery = const mempty

-- | The response from the GetCognitoEvents request
--
--
--
-- /See:/ 'getCognitoEventsResponse' smart constructor.
data GetCognitoEventsResponse = GetCognitoEventsResponse'
  { _gcersEvents         :: !(Maybe (Map Text Text))
  , _gcersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCognitoEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcersEvents' - The Cognito Events returned from the GetCognitoEvents request
--
-- * 'gcersResponseStatus' - -- | The response status code.
getCognitoEventsResponse
    :: Int -- ^ 'gcersResponseStatus'
    -> GetCognitoEventsResponse
getCognitoEventsResponse pResponseStatus_ =
  GetCognitoEventsResponse'
    {_gcersEvents = Nothing, _gcersResponseStatus = pResponseStatus_}


-- | The Cognito Events returned from the GetCognitoEvents request
gcersEvents :: Lens' GetCognitoEventsResponse (HashMap Text Text)
gcersEvents = lens _gcersEvents (\ s a -> s{_gcersEvents = a}) . _Default . _Map

-- | -- | The response status code.
gcersResponseStatus :: Lens' GetCognitoEventsResponse Int
gcersResponseStatus = lens _gcersResponseStatus (\ s a -> s{_gcersResponseStatus = a})

instance NFData GetCognitoEventsResponse where
