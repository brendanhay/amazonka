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
-- Module      : Network.AWS.SQS.SendMessageBatch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delivers up to ten messages to the specified queue. This is a batch version of @'SendMessage' .@ For a FIFO queue, multiple messages within a single batch are enqueued in the order they are sent.
--
--
-- The result of sending each message is reported individually in the response. Because the batch request can result in a combination of successful and unsuccessful actions, you should check for batch errors even when the call returns an HTTP status code of @200@ .
--
-- The maximum allowed individual message size and the maximum total payload size (the sum of the individual lengths of all of the batched messages) are both 256 KB (262,144 bytes).
--
-- /Important:/ A message can include only XML, JSON, and unformatted text. The following Unicode characters are allowed:
--
-- @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ | @#x10000@ to @#x10FFFF@
--
-- Any characters not included in this list will be rejected. For more information, see the <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters> .
--
-- If you don't specify the @DelaySeconds@ parameter for an entry, Amazon SQS uses the default value for the queue.
--
module Network.AWS.SQS.SendMessageBatch
    (
    -- * Creating a Request
      sendMessageBatch
    , SendMessageBatch
    -- * Request Lenses
    , smbQueueURL
    , smbEntries

    -- * Destructuring the Response
    , sendMessageBatchResponse
    , SendMessageBatchResponse
    -- * Response Lenses
    , smbrsResponseStatus
    , smbrsSuccessful
    , smbrsFailed
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
-- /See:/ 'sendMessageBatch' smart constructor.
data SendMessageBatch = SendMessageBatch'
  { _smbQueueURL :: !Text
  , _smbEntries  :: ![SendMessageBatchRequestEntry]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendMessageBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smbQueueURL' - The URL of the Amazon SQS queue to which batched messages are sent. Queue URLs are case-sensitive.
--
-- * 'smbEntries' - A list of @'SendMessageBatchRequestEntry' @ items.
sendMessageBatch
    :: Text -- ^ 'smbQueueURL'
    -> SendMessageBatch
sendMessageBatch pQueueURL_ =
  SendMessageBatch' {_smbQueueURL = pQueueURL_, _smbEntries = mempty}


-- | The URL of the Amazon SQS queue to which batched messages are sent. Queue URLs are case-sensitive.
smbQueueURL :: Lens' SendMessageBatch Text
smbQueueURL = lens _smbQueueURL (\ s a -> s{_smbQueueURL = a})

-- | A list of @'SendMessageBatchRequestEntry' @ items.
smbEntries :: Lens' SendMessageBatch [SendMessageBatchRequestEntry]
smbEntries = lens _smbEntries (\ s a -> s{_smbEntries = a}) . _Coerce

instance AWSRequest SendMessageBatch where
        type Rs SendMessageBatch = SendMessageBatchResponse
        request = postQuery sqs
        response
          = receiveXMLWrapper "SendMessageBatchResult"
              (\ s h x ->
                 SendMessageBatchResponse' <$>
                   (pure (fromEnum s)) <*>
                     (parseXMLList "SendMessageBatchResultEntry" x)
                     <*> (parseXMLList "BatchResultErrorEntry" x))

instance Hashable SendMessageBatch where

instance NFData SendMessageBatch where

instance ToHeaders SendMessageBatch where
        toHeaders = const mempty

instance ToPath SendMessageBatch where
        toPath = const "/"

instance ToQuery SendMessageBatch where
        toQuery SendMessageBatch'{..}
          = mconcat
              ["Action" =: ("SendMessageBatch" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueUrl" =: _smbQueueURL,
               toQueryList "SendMessageBatchRequestEntry"
                 _smbEntries]

-- | For each message in the batch, the response contains a @'SendMessageBatchResultEntry' @ tag if the message succeeds or a @'BatchResultErrorEntry' @ tag if the message fails.
--
--
--
-- /See:/ 'sendMessageBatchResponse' smart constructor.
data SendMessageBatchResponse = SendMessageBatchResponse'
  { _smbrsResponseStatus :: !Int
  , _smbrsSuccessful     :: ![SendMessageBatchResultEntry]
  , _smbrsFailed         :: ![BatchResultErrorEntry]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendMessageBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smbrsResponseStatus' - -- | The response status code.
--
-- * 'smbrsSuccessful' - A list of @'SendMessageBatchResultEntry' @ items.
--
-- * 'smbrsFailed' - A list of @'BatchResultErrorEntry' @ items with error details about each message that can't be enqueued.
sendMessageBatchResponse
    :: Int -- ^ 'smbrsResponseStatus'
    -> SendMessageBatchResponse
sendMessageBatchResponse pResponseStatus_ =
  SendMessageBatchResponse'
    { _smbrsResponseStatus = pResponseStatus_
    , _smbrsSuccessful = mempty
    , _smbrsFailed = mempty
    }


-- | -- | The response status code.
smbrsResponseStatus :: Lens' SendMessageBatchResponse Int
smbrsResponseStatus = lens _smbrsResponseStatus (\ s a -> s{_smbrsResponseStatus = a})

-- | A list of @'SendMessageBatchResultEntry' @ items.
smbrsSuccessful :: Lens' SendMessageBatchResponse [SendMessageBatchResultEntry]
smbrsSuccessful = lens _smbrsSuccessful (\ s a -> s{_smbrsSuccessful = a}) . _Coerce

-- | A list of @'BatchResultErrorEntry' @ items with error details about each message that can't be enqueued.
smbrsFailed :: Lens' SendMessageBatchResponse [BatchResultErrorEntry]
smbrsFailed = lens _smbrsFailed (\ s a -> s{_smbrsFailed = a}) . _Coerce

instance NFData SendMessageBatchResponse where
