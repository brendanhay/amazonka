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
-- Module      : Network.AWS.Pinpoint.GetEventStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the event stream for an app.
module Network.AWS.Pinpoint.GetEventStream
    (
    -- * Creating a Request
      getEventStream
    , GetEventStream
    -- * Request Lenses
    , gesApplicationId

    -- * Destructuring the Response
    , getEventStreamResponse
    , GetEventStreamResponse
    -- * Response Lenses
    , gesrsResponseStatus
    , gesrsEventStream
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | GetEventStreamRequest
--
-- /See:/ 'getEventStream' smart constructor.
newtype GetEventStream = GetEventStream'
  { _gesApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetEventStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gesApplicationId' - ApplicationId
getEventStream
    :: Text -- ^ 'gesApplicationId'
    -> GetEventStream
getEventStream pApplicationId_ =
  GetEventStream' {_gesApplicationId = pApplicationId_}


-- | ApplicationId
gesApplicationId :: Lens' GetEventStream Text
gesApplicationId = lens _gesApplicationId (\ s a -> s{_gesApplicationId = a})

instance AWSRequest GetEventStream where
        type Rs GetEventStream = GetEventStreamResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetEventStreamResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetEventStream where

instance NFData GetEventStream where

instance ToHeaders GetEventStream where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetEventStream where
        toPath GetEventStream'{..}
          = mconcat
              ["/v1/apps/", toBS _gesApplicationId, "/eventstream"]

instance ToQuery GetEventStream where
        toQuery = const mempty

-- | /See:/ 'getEventStreamResponse' smart constructor.
data GetEventStreamResponse = GetEventStreamResponse'
  { _gesrsResponseStatus :: !Int
  , _gesrsEventStream    :: !EventStream
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetEventStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gesrsResponseStatus' - -- | The response status code.
--
-- * 'gesrsEventStream' - Undocumented member.
getEventStreamResponse
    :: Int -- ^ 'gesrsResponseStatus'
    -> EventStream -- ^ 'gesrsEventStream'
    -> GetEventStreamResponse
getEventStreamResponse pResponseStatus_ pEventStream_ =
  GetEventStreamResponse'
    {_gesrsResponseStatus = pResponseStatus_, _gesrsEventStream = pEventStream_}


-- | -- | The response status code.
gesrsResponseStatus :: Lens' GetEventStreamResponse Int
gesrsResponseStatus = lens _gesrsResponseStatus (\ s a -> s{_gesrsResponseStatus = a})

-- | Undocumented member.
gesrsEventStream :: Lens' GetEventStreamResponse EventStream
gesrsEventStream = lens _gesrsEventStream (\ s a -> s{_gesrsEventStream = a})

instance NFData GetEventStreamResponse where
