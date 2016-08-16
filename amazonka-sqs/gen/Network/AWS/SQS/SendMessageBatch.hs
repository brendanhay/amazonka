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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delivers up to ten messages to the specified queue. This is a batch version of < SendMessage>. The result of the send action on each message is reported individually in the response. The maximum allowed individual message size is 256 KB (262,144 bytes).
--
-- The maximum total payload size (i.e., the sum of all a batch\'s individual message lengths) is also 256 KB (262,144 bytes).
--
-- If the 'DelaySeconds' parameter is not specified for an entry, the default for the queue is used.
--
-- The following list shows the characters (in Unicode) that are allowed in your message, according to the W3C XML specification. For more information, go to <http://www.faqs.org/rfcs/rfc1321.html>. If you send any characters that are not included in the list, your request will be rejected.
--
-- #x9 | #xA | #xD | [#x20 to #xD7FF] | [#xE000 to #xFFFD] | [#x10000 to #x10FFFF]
--
-- Because the batch request can result in a combination of successful and unsuccessful actions, you should check for batch errors even when the call returns an HTTP status code of 200.
--
-- Some API actions take lists of parameters. These lists are specified using the 'param.n' notation. Values of 'n' are integers starting from 1. For example, a parameter list with two elements looks like this:
--
-- '&amp;Attribute.1=this'
--
-- '&amp;Attribute.2=that'
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

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types
import           Network.AWS.SQS.Types.Product

-- |
--
-- /See:/ 'sendMessageBatch' smart constructor.
data SendMessageBatch = SendMessageBatch'
    { _smbQueueURL :: !Text
    , _smbEntries  :: ![SendMessageBatchRequestEntry]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SendMessageBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smbQueueURL'
--
-- * 'smbEntries'
sendMessageBatch
    :: Text -- ^ 'smbQueueURL'
    -> SendMessageBatch
sendMessageBatch pQueueURL_ =
    SendMessageBatch'
    { _smbQueueURL = pQueueURL_
    , _smbEntries = mempty
    }

-- | The URL of the Amazon SQS queue to take action on.
--
-- Queue URLs are case-sensitive.
smbQueueURL :: Lens' SendMessageBatch Text
smbQueueURL = lens _smbQueueURL (\ s a -> s{_smbQueueURL = a});

-- | A list of < SendMessageBatchRequestEntry> items.
smbEntries :: Lens' SendMessageBatch [SendMessageBatchRequestEntry]
smbEntries = lens _smbEntries (\ s a -> s{_smbEntries = a}) . _Coerce;

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

instance Hashable SendMessageBatch

instance NFData SendMessageBatch

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

-- | For each message in the batch, the response contains a < SendMessageBatchResultEntry> tag if the message succeeds or a < BatchResultErrorEntry> tag if the message fails.
--
-- /See:/ 'sendMessageBatchResponse' smart constructor.
data SendMessageBatchResponse = SendMessageBatchResponse'
    { _smbrsResponseStatus :: !Int
    , _smbrsSuccessful     :: ![SendMessageBatchResultEntry]
    , _smbrsFailed         :: ![BatchResultErrorEntry]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SendMessageBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smbrsResponseStatus'
--
-- * 'smbrsSuccessful'
--
-- * 'smbrsFailed'
sendMessageBatchResponse
    :: Int -- ^ 'smbrsResponseStatus'
    -> SendMessageBatchResponse
sendMessageBatchResponse pResponseStatus_ =
    SendMessageBatchResponse'
    { _smbrsResponseStatus = pResponseStatus_
    , _smbrsSuccessful = mempty
    , _smbrsFailed = mempty
    }

-- | The response status code.
smbrsResponseStatus :: Lens' SendMessageBatchResponse Int
smbrsResponseStatus = lens _smbrsResponseStatus (\ s a -> s{_smbrsResponseStatus = a});

-- | A list of < SendMessageBatchResultEntry> items.
smbrsSuccessful :: Lens' SendMessageBatchResponse [SendMessageBatchResultEntry]
smbrsSuccessful = lens _smbrsSuccessful (\ s a -> s{_smbrsSuccessful = a}) . _Coerce;

-- | A list of < BatchResultErrorEntry> items with the error detail about each message that could not be enqueued.
smbrsFailed :: Lens' SendMessageBatchResponse [BatchResultErrorEntry]
smbrsFailed = lens _smbrsFailed (\ s a -> s{_smbrsFailed = a}) . _Coerce;

instance NFData SendMessageBatchResponse
