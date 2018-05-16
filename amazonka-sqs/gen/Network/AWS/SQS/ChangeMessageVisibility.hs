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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the visibility timeout of a specified message in a queue to a new value. The maximum allowed timeout value is 12 hours. Thus, you can't extend the timeout of a message in an existing queue to more than a total visibility timeout of 12 hours. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ .
--
--
-- For example, you have a message with a visibility timeout of 5 minutes. After 3 minutes, you call @ChangeMessageVisiblity@ with a timeout of 10 minutes. At that time, the timeout for the message is extended by 10 minutes beyond the time of the @ChangeMessageVisibility@ action. This results in a total visibility timeout of 13 minutes. You can continue to call the @ChangeMessageVisibility@ to extend the visibility timeout to a maximum of 12 hours. If you try to extend the visibility timeout beyond 12 hours, your request is rejected.
--
-- A message is considered to be /in flight/ after it's received from a queue by a consumer, but not yet deleted from the queue.
--
-- For standard queues, there can be a maximum of 120,000 inflight messages per queue. If you reach this limit, Amazon SQS returns the @OverLimit@ error message. To avoid reaching the limit, you should delete messages from the queue after they're processed. You can also increase the number of queues you use to process your messages.
--
-- For FIFO queues, there can be a maximum of 20,000 inflight messages per queue. If you reach this limit, Amazon SQS returns no error messages.
--
-- /Important:/ If you attempt to set the @VisibilityTimeout@ to a value greater than the maximum time left, Amazon SQS returns an error. Amazon SQS doesn't automatically recalculate and increase the timeout to the maximum remaining time.
--
-- Unlike with a queue, when you change the visibility timeout for a specific message the timeout value is applied immediately but isn't saved in memory for that message. If you don't delete a message after it is received, the visibility timeout for the message reverts to the original timeout value (not to the value you set using the @ChangeMessageVisibility@ action) the next time the message is received.
--
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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types
import Network.AWS.SQS.Types.Product

-- | /See:/ 'changeMessageVisibility' smart constructor.
data ChangeMessageVisibility = ChangeMessageVisibility'
  { _cmvQueueURL          :: !Text
  , _cmvReceiptHandle     :: !Text
  , _cmvVisibilityTimeout :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangeMessageVisibility' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmvQueueURL' - The URL of the Amazon SQS queue whose message's visibility is changed. Queue URLs are case-sensitive.
--
-- * 'cmvReceiptHandle' - The receipt handle associated with the message whose visibility timeout is changed. This parameter is returned by the @'ReceiveMessage' @ action.
--
-- * 'cmvVisibilityTimeout' - The new value for the message's visibility timeout (in seconds). Values values: @0@ to @43200@ . Maximum: 12 hours.
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


-- | The URL of the Amazon SQS queue whose message's visibility is changed. Queue URLs are case-sensitive.
cmvQueueURL :: Lens' ChangeMessageVisibility Text
cmvQueueURL = lens _cmvQueueURL (\ s a -> s{_cmvQueueURL = a})

-- | The receipt handle associated with the message whose visibility timeout is changed. This parameter is returned by the @'ReceiveMessage' @ action.
cmvReceiptHandle :: Lens' ChangeMessageVisibility Text
cmvReceiptHandle = lens _cmvReceiptHandle (\ s a -> s{_cmvReceiptHandle = a})

-- | The new value for the message's visibility timeout (in seconds). Values values: @0@ to @43200@ . Maximum: 12 hours.
cmvVisibilityTimeout :: Lens' ChangeMessageVisibility Int
cmvVisibilityTimeout = lens _cmvVisibilityTimeout (\ s a -> s{_cmvVisibilityTimeout = a})

instance AWSRequest ChangeMessageVisibility where
        type Rs ChangeMessageVisibility =
             ChangeMessageVisibilityResponse
        request = postQuery sqs
        response
          = receiveNull ChangeMessageVisibilityResponse'

instance Hashable ChangeMessageVisibility where

instance NFData ChangeMessageVisibility where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangeMessageVisibilityResponse' with the minimum fields required to make a request.
--
changeMessageVisibilityResponse
    :: ChangeMessageVisibilityResponse
changeMessageVisibilityResponse = ChangeMessageVisibilityResponse'


instance NFData ChangeMessageVisibilityResponse where
