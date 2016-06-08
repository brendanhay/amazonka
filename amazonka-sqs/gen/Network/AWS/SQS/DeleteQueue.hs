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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the queue specified by the __queue URL__, regardless of whether the queue is empty. If the specified queue does not exist, Amazon SQS returns a successful response.
--
-- Use 'DeleteQueue' with care; once you delete your queue, any messages in the queue are no longer available.
--
-- When you delete a queue, the deletion process takes up to 60 seconds. Requests you send involving that queue during the 60 seconds might succeed. For example, a < SendMessage> request might succeed, but after the 60 seconds, the queue and that message you sent no longer exist. Also, when you delete a queue, you must wait at least 60 seconds before creating a queue with the same name.
--
-- We reserve the right to delete queues that have had no activity for more than 30 days. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSConcepts.html How Amazon SQS Queues Work> in the /Amazon SQS Developer Guide/.
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

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types
import           Network.AWS.SQS.Types.Product

-- | /See:/ 'deleteQueue' smart constructor.
newtype DeleteQueue = DeleteQueue'
    { _dqQueueURL :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqQueueURL'
deleteQueue
    :: Text -- ^ 'dqQueueURL'
    -> DeleteQueue
deleteQueue pQueueURL_ =
    DeleteQueue'
    { _dqQueueURL = pQueueURL_
    }

-- | The URL of the Amazon SQS queue to take action on.
dqQueueURL :: Lens' DeleteQueue Text
dqQueueURL = lens _dqQueueURL (\ s a -> s{_dqQueueURL = a});

instance AWSRequest DeleteQueue where
        type Rs DeleteQueue = DeleteQueueResponse
        request = postQuery sqs
        response = receiveNull DeleteQueueResponse'

instance Hashable DeleteQueue

instance NFData DeleteQueue

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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteQueueResponse' with the minimum fields required to make a request.
--
deleteQueueResponse
    :: DeleteQueueResponse
deleteQueueResponse = DeleteQueueResponse'

instance NFData DeleteQueueResponse
