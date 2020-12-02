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
-- Module      : Network.AWS.MediaConvert.UpdateQueue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify one of your existing queues.
module Network.AWS.MediaConvert.UpdateQueue
    (
    -- * Creating a Request
      updateQueue
    , UpdateQueue
    -- * Request Lenses
    , uqStatus
    , uqDescription
    , uqName

    -- * Destructuring the Response
    , updateQueueResponse
    , UpdateQueueResponse
    -- * Response Lenses
    , uqrsQueue
    , uqrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateQueue' smart constructor.
data UpdateQueue = UpdateQueue'
  { _uqStatus      :: !(Maybe QueueStatus)
  , _uqDescription :: !(Maybe Text)
  , _uqName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uqStatus' - Undocumented member.
--
-- * 'uqDescription' - The new description for the queue, if you are changing it.
--
-- * 'uqName' - The name of the queue you are modifying.
updateQueue
    :: Text -- ^ 'uqName'
    -> UpdateQueue
updateQueue pName_ =
  UpdateQueue' {_uqStatus = Nothing, _uqDescription = Nothing, _uqName = pName_}


-- | Undocumented member.
uqStatus :: Lens' UpdateQueue (Maybe QueueStatus)
uqStatus = lens _uqStatus (\ s a -> s{_uqStatus = a})

-- | The new description for the queue, if you are changing it.
uqDescription :: Lens' UpdateQueue (Maybe Text)
uqDescription = lens _uqDescription (\ s a -> s{_uqDescription = a})

-- | The name of the queue you are modifying.
uqName :: Lens' UpdateQueue Text
uqName = lens _uqName (\ s a -> s{_uqName = a})

instance AWSRequest UpdateQueue where
        type Rs UpdateQueue = UpdateQueueResponse
        request = putJSON mediaConvert
        response
          = receiveJSON
              (\ s h x ->
                 UpdateQueueResponse' <$>
                   (x .?> "queue") <*> (pure (fromEnum s)))

instance Hashable UpdateQueue where

instance NFData UpdateQueue where

instance ToHeaders UpdateQueue where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateQueue where
        toJSON UpdateQueue'{..}
          = object
              (catMaybes
                 [("status" .=) <$> _uqStatus,
                  ("description" .=) <$> _uqDescription])

instance ToPath UpdateQueue where
        toPath UpdateQueue'{..}
          = mconcat ["/2017-08-29/queues/", toBS _uqName]

instance ToQuery UpdateQueue where
        toQuery = const mempty

-- | /See:/ 'updateQueueResponse' smart constructor.
data UpdateQueueResponse = UpdateQueueResponse'
  { _uqrsQueue          :: !(Maybe Queue)
  , _uqrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateQueueResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uqrsQueue' - Undocumented member.
--
-- * 'uqrsResponseStatus' - -- | The response status code.
updateQueueResponse
    :: Int -- ^ 'uqrsResponseStatus'
    -> UpdateQueueResponse
updateQueueResponse pResponseStatus_ =
  UpdateQueueResponse'
    {_uqrsQueue = Nothing, _uqrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
uqrsQueue :: Lens' UpdateQueueResponse (Maybe Queue)
uqrsQueue = lens _uqrsQueue (\ s a -> s{_uqrsQueue = a})

-- | -- | The response status code.
uqrsResponseStatus :: Lens' UpdateQueueResponse Int
uqrsResponseStatus = lens _uqrsResponseStatus (\ s a -> s{_uqrsResponseStatus = a})

instance NFData UpdateQueueResponse where
