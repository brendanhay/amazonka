{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.ChangeMessageVisibility
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the visibility timeout of a specified message in a queue to a new value. The default visibility timeout for a message is 30 seconds. The minimum is 0 seconds. The maximum is 12 hours. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ .
--
--
-- For example, you have a message with a visibility timeout of 5 minutes. After 3 minutes, you call @ChangeMessageVisibility@ with a timeout of 10 minutes. You can continue to call @ChangeMessageVisibility@ to extend the visibility timeout to the maximum allowed time. If you try to extend the visibility timeout beyond the maximum, your request is rejected.
--
-- An Amazon SQS message has three basic states:
--
--     * Sent to a queue by a producer.
--
--     * Received from the queue by a consumer.
--
--     * Deleted from the queue.
--
--
--
-- A message is considered to be /stored/ after it is sent to a queue by a producer, but not yet received from the queue by a consumer (that is, between states 1 and 2). There is no limit to the number of stored messages. A message is considered to be /in flight/ after it is received from a queue by a consumer, but not yet deleted from the queue (that is, between states 2 and 3). There is a limit to the number of inflight messages.
--
-- Limits that apply to inflight messages are unrelated to the /unlimited/ number of stored messages.
--
-- For most standard queues (depending on queue traffic and message backlog), there can be a maximum of approximately 120,000 inflight messages (received from a queue by a consumer, but not yet deleted from the queue). If you reach this limit, Amazon SQS returns the @OverLimit@ error message. To avoid reaching the limit, you should delete messages from the queue after they're processed. You can also increase the number of queues you use to process your messages. To request a limit increase, <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-sqs file a support request> .
--
-- For FIFO queues, there can be a maximum of 20,000 inflight messages (received from a queue by a consumer, but not yet deleted from the queue). If you reach this limit, Amazon SQS returns no error messages.
--
-- /Important:/ If you attempt to set the @VisibilityTimeout@ to a value greater than the maximum time left, Amazon SQS returns an error. Amazon SQS doesn't automatically recalculate and increase the timeout to the maximum remaining time.
--
-- Unlike with a queue, when you change the visibility timeout for a specific message the timeout value is applied immediately but isn't saved in memory for that message. If you don't delete a message after it is received, the visibility timeout for the message reverts to the original timeout value (not to the value you set using the @ChangeMessageVisibility@ action) the next time the message is received.
module Network.AWS.SQS.ChangeMessageVisibility
  ( -- * Creating a Request
    changeMessageVisibility,
    ChangeMessageVisibility,

    -- * Request Lenses
    cmvQueueURL,
    cmvReceiptHandle,
    cmvVisibilityTimeout,

    -- * Destructuring the Response
    changeMessageVisibilityResponse,
    ChangeMessageVisibilityResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types

-- | /See:/ 'changeMessageVisibility' smart constructor.
data ChangeMessageVisibility = ChangeMessageVisibility'
  { _cmvQueueURL ::
      !Text,
    _cmvReceiptHandle :: !Text,
    _cmvVisibilityTimeout :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChangeMessageVisibility' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmvQueueURL' - The URL of the Amazon SQS queue whose message's visibility is changed. Queue URLs and names are case-sensitive.
--
-- * 'cmvReceiptHandle' - The receipt handle associated with the message whose visibility timeout is changed. This parameter is returned by the @'ReceiveMessage' @ action.
--
-- * 'cmvVisibilityTimeout' - The new value for the message's visibility timeout (in seconds). Values range: @0@ to @43200@ . Maximum: 12 hours.
changeMessageVisibility ::
  -- | 'cmvQueueURL'
  Text ->
  -- | 'cmvReceiptHandle'
  Text ->
  -- | 'cmvVisibilityTimeout'
  Int ->
  ChangeMessageVisibility
changeMessageVisibility
  pQueueURL_
  pReceiptHandle_
  pVisibilityTimeout_ =
    ChangeMessageVisibility'
      { _cmvQueueURL = pQueueURL_,
        _cmvReceiptHandle = pReceiptHandle_,
        _cmvVisibilityTimeout = pVisibilityTimeout_
      }

-- | The URL of the Amazon SQS queue whose message's visibility is changed. Queue URLs and names are case-sensitive.
cmvQueueURL :: Lens' ChangeMessageVisibility Text
cmvQueueURL = lens _cmvQueueURL (\s a -> s {_cmvQueueURL = a})

-- | The receipt handle associated with the message whose visibility timeout is changed. This parameter is returned by the @'ReceiveMessage' @ action.
cmvReceiptHandle :: Lens' ChangeMessageVisibility Text
cmvReceiptHandle = lens _cmvReceiptHandle (\s a -> s {_cmvReceiptHandle = a})

-- | The new value for the message's visibility timeout (in seconds). Values range: @0@ to @43200@ . Maximum: 12 hours.
cmvVisibilityTimeout :: Lens' ChangeMessageVisibility Int
cmvVisibilityTimeout = lens _cmvVisibilityTimeout (\s a -> s {_cmvVisibilityTimeout = a})

instance AWSRequest ChangeMessageVisibility where
  type Rs ChangeMessageVisibility = ChangeMessageVisibilityResponse
  request = postQuery sqs
  response = receiveNull ChangeMessageVisibilityResponse'

instance Hashable ChangeMessageVisibility

instance NFData ChangeMessageVisibility

instance ToHeaders ChangeMessageVisibility where
  toHeaders = const mempty

instance ToPath ChangeMessageVisibility where
  toPath = const "/"

instance ToQuery ChangeMessageVisibility where
  toQuery ChangeMessageVisibility' {..} =
    mconcat
      [ "Action" =: ("ChangeMessageVisibility" :: ByteString),
        "Version" =: ("2012-11-05" :: ByteString),
        "QueueUrl" =: _cmvQueueURL,
        "ReceiptHandle" =: _cmvReceiptHandle,
        "VisibilityTimeout" =: _cmvVisibilityTimeout
      ]

-- | /See:/ 'changeMessageVisibilityResponse' smart constructor.
data ChangeMessageVisibilityResponse = ChangeMessageVisibilityResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChangeMessageVisibilityResponse' with the minimum fields required to make a request.
changeMessageVisibilityResponse ::
  ChangeMessageVisibilityResponse
changeMessageVisibilityResponse = ChangeMessageVisibilityResponse'

instance NFData ChangeMessageVisibilityResponse
