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
-- Module      : Network.AWS.Pinpoint.DeleteEventStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the event stream for an app.
module Network.AWS.Pinpoint.DeleteEventStream
    (
    -- * Creating a Request
      deleteEventStream
    , DeleteEventStream
    -- * Request Lenses
    , desApplicationId

    -- * Destructuring the Response
    , deleteEventStreamResponse
    , DeleteEventStreamResponse
    -- * Response Lenses
    , desrsResponseStatus
    , desrsEventStream
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DeleteEventStream Request
--
-- /See:/ 'deleteEventStream' smart constructor.
newtype DeleteEventStream = DeleteEventStream'
  { _desApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEventStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desApplicationId' - ApplicationId
deleteEventStream
    :: Text -- ^ 'desApplicationId'
    -> DeleteEventStream
deleteEventStream pApplicationId_ =
  DeleteEventStream' {_desApplicationId = pApplicationId_}


-- | ApplicationId
desApplicationId :: Lens' DeleteEventStream Text
desApplicationId = lens _desApplicationId (\ s a -> s{_desApplicationId = a})

instance AWSRequest DeleteEventStream where
        type Rs DeleteEventStream = DeleteEventStreamResponse
        request = delete pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 DeleteEventStreamResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable DeleteEventStream where

instance NFData DeleteEventStream where

instance ToHeaders DeleteEventStream where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteEventStream where
        toPath DeleteEventStream'{..}
          = mconcat
              ["/v1/apps/", toBS _desApplicationId, "/eventstream"]

instance ToQuery DeleteEventStream where
        toQuery = const mempty

-- | /See:/ 'deleteEventStreamResponse' smart constructor.
data DeleteEventStreamResponse = DeleteEventStreamResponse'
  { _desrsResponseStatus :: !Int
  , _desrsEventStream    :: !EventStream
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEventStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsResponseStatus' - -- | The response status code.
--
-- * 'desrsEventStream' - Undocumented member.
deleteEventStreamResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> EventStream -- ^ 'desrsEventStream'
    -> DeleteEventStreamResponse
deleteEventStreamResponse pResponseStatus_ pEventStream_ =
  DeleteEventStreamResponse'
    {_desrsResponseStatus = pResponseStatus_, _desrsEventStream = pEventStream_}


-- | -- | The response status code.
desrsResponseStatus :: Lens' DeleteEventStreamResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

-- | Undocumented member.
desrsEventStream :: Lens' DeleteEventStreamResponse EventStream
desrsEventStream = lens _desrsEventStream (\ s a -> s{_desrsEventStream = a})

instance NFData DeleteEventStreamResponse where
