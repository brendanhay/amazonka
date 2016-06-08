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
-- Module      : Network.AWS.SQS.ChangeMessageVisibility
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the visibility timeout of a specified message in a queue to a new value. The maximum allowed timeout value you can set the value to is 12 hours. This means you can\'t extend the timeout of a message in an existing queue to more than a total visibility timeout of 12 hours. (For more information visibility timeout, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/AboutVT.html Visibility Timeout> in the /Amazon SQS Developer Guide/.)
--
-- For example, let\'s say you have a message and its default message visibility timeout is 5 minutes. After 3 minutes, you call 'ChangeMessageVisiblity' with a timeout of 10 minutes. At that time, the timeout for the message would be extended by 10 minutes beyond the time of the ChangeMessageVisibility call. This results in a total visibility timeout of 13 minutes. You can continue to call ChangeMessageVisibility to extend the visibility timeout to a maximum of 12 hours. If you try to extend beyond 12 hours, the request will be rejected.
--
-- There is a 120,000 limit for the number of inflight messages per queue. Messages are inflight after they have been received from the queue by a consuming component, but have not yet been deleted from the queue. If you reach the 120,000 limit, you will receive an OverLimit error message from Amazon SQS. To help avoid reaching the limit, you should delete the messages from the queue after they have been processed. You can also increase the number of queues you use to process the messages.
--
-- If you attempt to set the 'VisibilityTimeout' to an amount more than the maximum time left, Amazon SQS returns an error. It will not automatically recalculate and increase the timeout to the maximum time remaining.
--
-- Unlike with a queue, when you change the visibility timeout for a specific message, that timeout value is applied immediately but is not saved in memory for that message. If you don\'t delete a message after it is received, the visibility timeout for the message the next time it is received reverts to the original timeout value, not the value you set with the 'ChangeMessageVisibility' action.
module Network.AWS.SQS.ChangeMessageVisibility
    (
    -- * Creating a Request
      changeMessageVisibility
    , ChangeMessageVisibility
    -- * Request Lenses
    , cmvQueueURL
    , cmvReceiptHandle
    , cmvVisibilityTimeout

    -- * Destructuring the Response
    , changeMessageVisibilityResponse
    , ChangeMessageVisibilityResponse
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types
import           Network.AWS.SQS.Types.Product

-- | /See:/ 'changeMessageVisibility' smart constructor.
data ChangeMessageVisibility = ChangeMessageVisibility'
    { _cmvQueueURL          :: !Text
    , _cmvReceiptHandle     :: !Text
    , _cmvVisibilityTimeout :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChangeMessageVisibility' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmvQueueURL'
--
-- * 'cmvReceiptHandle'
--
-- * 'cmvVisibilityTimeout'
changeMessageVisibility
    :: Text -- ^ 'cmvQueueURL'
    -> Text -- ^ 'cmvReceiptHandle'
    -> Int -- ^ 'cmvVisibilityTimeout'
    -> ChangeMessageVisibility
changeMessageVisibility pQueueURL_ pReceiptHandle_ pVisibilityTimeout_ =
    ChangeMessageVisibility'
    { _cmvQueueURL = pQueueURL_
    , _cmvReceiptHandle = pReceiptHandle_
    , _cmvVisibilityTimeout = pVisibilityTimeout_
    }

-- | The URL of the Amazon SQS queue to take action on.
cmvQueueURL :: Lens' ChangeMessageVisibility Text
cmvQueueURL = lens _cmvQueueURL (\ s a -> s{_cmvQueueURL = a});

-- | The receipt handle associated with the message whose visibility timeout should be changed. This parameter is returned by the < ReceiveMessage> action.
cmvReceiptHandle :: Lens' ChangeMessageVisibility Text
cmvReceiptHandle = lens _cmvReceiptHandle (\ s a -> s{_cmvReceiptHandle = a});

-- | The new value (in seconds - from 0 to 43200 - maximum 12 hours) for the message\'s visibility timeout.
cmvVisibilityTimeout :: Lens' ChangeMessageVisibility Int
cmvVisibilityTimeout = lens _cmvVisibilityTimeout (\ s a -> s{_cmvVisibilityTimeout = a});

instance AWSRequest ChangeMessageVisibility where
        type Rs ChangeMessageVisibility =
             ChangeMessageVisibilityResponse
        request = postQuery sqs
        response
          = receiveNull ChangeMessageVisibilityResponse'

instance Hashable ChangeMessageVisibility

instance NFData ChangeMessageVisibility

instance ToHeaders ChangeMessageVisibility where
        toHeaders = const mempty

instance ToPath ChangeMessageVisibility where
        toPath = const "/"

instance ToQuery ChangeMessageVisibility where
        toQuery ChangeMessageVisibility'{..}
          = mconcat
              ["Action" =:
                 ("ChangeMessageVisibility" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueUrl" =: _cmvQueueURL,
               "ReceiptHandle" =: _cmvReceiptHandle,
               "VisibilityTimeout" =: _cmvVisibilityTimeout]

-- | /See:/ 'changeMessageVisibilityResponse' smart constructor.
data ChangeMessageVisibilityResponse =
    ChangeMessageVisibilityResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChangeMessageVisibilityResponse' with the minimum fields required to make a request.
--
changeMessageVisibilityResponse
    :: ChangeMessageVisibilityResponse
changeMessageVisibilityResponse = ChangeMessageVisibilityResponse'

instance NFData ChangeMessageVisibilityResponse
