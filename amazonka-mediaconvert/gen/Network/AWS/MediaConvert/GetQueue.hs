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
-- Module      : Network.AWS.MediaConvert.GetQueue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the JSON for a specific queue.
module Network.AWS.MediaConvert.GetQueue
    (
    -- * Creating a Request
      getQueue
    , GetQueue
    -- * Request Lenses
    , gqName

    -- * Destructuring the Response
    , getQueueResponse
    , GetQueueResponse
    -- * Response Lenses
    , gqrsQueue
    , gqrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getQueue' smart constructor.
newtype GetQueue = GetQueue'
  { _gqName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqName' - The name of the queue.
getQueue
    :: Text -- ^ 'gqName'
    -> GetQueue
getQueue pName_ = GetQueue' {_gqName = pName_}


-- | The name of the queue.
gqName :: Lens' GetQueue Text
gqName = lens _gqName (\ s a -> s{_gqName = a})

instance AWSRequest GetQueue where
        type Rs GetQueue = GetQueueResponse
        request = get mediaConvert
        response
          = receiveJSON
              (\ s h x ->
                 GetQueueResponse' <$>
                   (x .?> "queue") <*> (pure (fromEnum s)))

instance Hashable GetQueue where

instance NFData GetQueue where

instance ToHeaders GetQueue where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetQueue where
        toPath GetQueue'{..}
          = mconcat ["/2017-08-29/queues/", toBS _gqName]

instance ToQuery GetQueue where
        toQuery = const mempty

-- | /See:/ 'getQueueResponse' smart constructor.
data GetQueueResponse = GetQueueResponse'
  { _gqrsQueue          :: !(Maybe Queue)
  , _gqrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQueueResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqrsQueue' - Undocumented member.
--
-- * 'gqrsResponseStatus' - -- | The response status code.
getQueueResponse
    :: Int -- ^ 'gqrsResponseStatus'
    -> GetQueueResponse
getQueueResponse pResponseStatus_ =
  GetQueueResponse'
    {_gqrsQueue = Nothing, _gqrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
gqrsQueue :: Lens' GetQueueResponse (Maybe Queue)
gqrsQueue = lens _gqrsQueue (\ s a -> s{_gqrsQueue = a})

-- | -- | The response status code.
gqrsResponseStatus :: Lens' GetQueueResponse Int
gqrsResponseStatus = lens _gqrsResponseStatus (\ s a -> s{_gqrsResponseStatus = a})

instance NFData GetQueueResponse where
