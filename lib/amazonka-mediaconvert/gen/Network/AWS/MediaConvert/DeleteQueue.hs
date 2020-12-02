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
-- Module      : Network.AWS.MediaConvert.DeleteQueue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently delete a queue you have created.
module Network.AWS.MediaConvert.DeleteQueue
    (
    -- * Creating a Request
      deleteQueue
    , DeleteQueue
    -- * Request Lenses
    , dqName

    -- * Destructuring the Response
    , deleteQueueResponse
    , DeleteQueueResponse
    -- * Response Lenses
    , dqrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteQueue' smart constructor.
newtype DeleteQueue = DeleteQueue'
  { _dqName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqName' - The name of the queue to be deleted.
deleteQueue
    :: Text -- ^ 'dqName'
    -> DeleteQueue
deleteQueue pName_ = DeleteQueue' {_dqName = pName_}


-- | The name of the queue to be deleted.
dqName :: Lens' DeleteQueue Text
dqName = lens _dqName (\ s a -> s{_dqName = a})

instance AWSRequest DeleteQueue where
        type Rs DeleteQueue = DeleteQueueResponse
        request = delete mediaConvert
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteQueueResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteQueue where

instance NFData DeleteQueue where

instance ToHeaders DeleteQueue where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteQueue where
        toPath DeleteQueue'{..}
          = mconcat ["/2017-08-29/queues/", toBS _dqName]

instance ToQuery DeleteQueue where
        toQuery = const mempty

-- | /See:/ 'deleteQueueResponse' smart constructor.
newtype DeleteQueueResponse = DeleteQueueResponse'
  { _dqrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteQueueResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqrsResponseStatus' - -- | The response status code.
deleteQueueResponse
    :: Int -- ^ 'dqrsResponseStatus'
    -> DeleteQueueResponse
deleteQueueResponse pResponseStatus_ =
  DeleteQueueResponse' {_dqrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dqrsResponseStatus :: Lens' DeleteQueueResponse Int
dqrsResponseStatus = lens _dqrsResponseStatus (\ s a -> s{_dqrsResponseStatus = a})

instance NFData DeleteQueueResponse where
