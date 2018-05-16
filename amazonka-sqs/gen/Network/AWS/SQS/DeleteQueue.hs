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
-- Module      : Network.AWS.SQS.DeleteQueue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the queue specified by the @QueueUrl@ , regardless of the queue's contents. If the specified queue doesn't exist, Amazon SQS returns a successful response.
--
--
-- /Important:/ Be careful with the @DeleteQueue@ action: When you delete a queue, any messages in the queue are no longer available.
--
-- When you delete a queue, the deletion process takes up to 60 seconds. Requests you send involving that queue during the 60 seconds might succeed. For example, a @'SendMessage' @ request might succeed, but after 60 seconds the queue and the message you sent no longer exist.
--
-- When you delete a queue, you must wait at least 60 seconds before creating a queue with the same name.
--
module Network.AWS.SQS.DeleteQueue
    (
    -- * Creating a Request
      deleteQueue
    , DeleteQueue
    -- * Request Lenses
    , dqQueueURL

    -- * Destructuring the Response
    , deleteQueueResponse
    , DeleteQueueResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types
import Network.AWS.SQS.Types.Product

-- |
--
--
--
-- /See:/ 'deleteQueue' smart constructor.
newtype DeleteQueue = DeleteQueue'
  { _dqQueueURL :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqQueueURL' - The URL of the Amazon SQS queue to delete. Queue URLs are case-sensitive.
deleteQueue
    :: Text -- ^ 'dqQueueURL'
    -> DeleteQueue
deleteQueue pQueueURL_ = DeleteQueue' {_dqQueueURL = pQueueURL_}


-- | The URL of the Amazon SQS queue to delete. Queue URLs are case-sensitive.
dqQueueURL :: Lens' DeleteQueue Text
dqQueueURL = lens _dqQueueURL (\ s a -> s{_dqQueueURL = a})

instance AWSRequest DeleteQueue where
        type Rs DeleteQueue = DeleteQueueResponse
        request = postQuery sqs
        response = receiveNull DeleteQueueResponse'

instance Hashable DeleteQueue where

instance NFData DeleteQueue where

instance ToHeaders DeleteQueue where
        toHeaders = const mempty

instance ToPath DeleteQueue where
        toPath = const "/"

instance ToQuery DeleteQueue where
        toQuery DeleteQueue'{..}
          = mconcat
              ["Action" =: ("DeleteQueue" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueUrl" =: _dqQueueURL]

-- | /See:/ 'deleteQueueResponse' smart constructor.
data DeleteQueueResponse =
  DeleteQueueResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteQueueResponse' with the minimum fields required to make a request.
--
deleteQueueResponse
    :: DeleteQueueResponse
deleteQueueResponse = DeleteQueueResponse'


instance NFData DeleteQueueResponse where
