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
-- Module      : Network.AWS.Pinpoint.PutEventStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use to create or update the event stream for an app.
module Network.AWS.Pinpoint.PutEventStream
    (
    -- * Creating a Request
      putEventStream
    , PutEventStream
    -- * Request Lenses
    , pesApplicationId
    , pesWriteEventStream

    -- * Destructuring the Response
    , putEventStreamResponse
    , PutEventStreamResponse
    -- * Response Lenses
    , pesrsResponseStatus
    , pesrsEventStream
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putEventStream' smart constructor.
data PutEventStream = PutEventStream'
  { _pesApplicationId    :: !Text
  , _pesWriteEventStream :: !WriteEventStream
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutEventStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pesApplicationId' - ApplicationId
--
-- * 'pesWriteEventStream' - EventStream to write.
putEventStream
    :: Text -- ^ 'pesApplicationId'
    -> WriteEventStream -- ^ 'pesWriteEventStream'
    -> PutEventStream
putEventStream pApplicationId_ pWriteEventStream_ =
  PutEventStream'
    { _pesApplicationId = pApplicationId_
    , _pesWriteEventStream = pWriteEventStream_
    }


-- | ApplicationId
pesApplicationId :: Lens' PutEventStream Text
pesApplicationId = lens _pesApplicationId (\ s a -> s{_pesApplicationId = a})

-- | EventStream to write.
pesWriteEventStream :: Lens' PutEventStream WriteEventStream
pesWriteEventStream = lens _pesWriteEventStream (\ s a -> s{_pesWriteEventStream = a})

instance AWSRequest PutEventStream where
        type Rs PutEventStream = PutEventStreamResponse
        request = postJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 PutEventStreamResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable PutEventStream where

instance NFData PutEventStream where

instance ToHeaders PutEventStream where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutEventStream where
        toJSON PutEventStream'{..}
          = object
              (catMaybes
                 [Just ("WriteEventStream" .= _pesWriteEventStream)])

instance ToPath PutEventStream where
        toPath PutEventStream'{..}
          = mconcat
              ["/v1/apps/", toBS _pesApplicationId, "/eventstream"]

instance ToQuery PutEventStream where
        toQuery = const mempty

-- | /See:/ 'putEventStreamResponse' smart constructor.
data PutEventStreamResponse = PutEventStreamResponse'
  { _pesrsResponseStatus :: !Int
  , _pesrsEventStream    :: !EventStream
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutEventStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pesrsResponseStatus' - -- | The response status code.
--
-- * 'pesrsEventStream' - Undocumented member.
putEventStreamResponse
    :: Int -- ^ 'pesrsResponseStatus'
    -> EventStream -- ^ 'pesrsEventStream'
    -> PutEventStreamResponse
putEventStreamResponse pResponseStatus_ pEventStream_ =
  PutEventStreamResponse'
    {_pesrsResponseStatus = pResponseStatus_, _pesrsEventStream = pEventStream_}


-- | -- | The response status code.
pesrsResponseStatus :: Lens' PutEventStreamResponse Int
pesrsResponseStatus = lens _pesrsResponseStatus (\ s a -> s{_pesrsResponseStatus = a})

-- | Undocumented member.
pesrsEventStream :: Lens' PutEventStreamResponse EventStream
pesrsEventStream = lens _pesrsEventStream (\ s a -> s{_pesrsEventStream = a})

instance NFData PutEventStreamResponse where
