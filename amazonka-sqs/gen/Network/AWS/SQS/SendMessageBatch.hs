{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SQS.SendMessageBatch
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Delivers up to ten messages to the specified queue. This is a batch
-- version of SendMessage. The result of the send action on each message is
-- reported individually in the response. The maximum allowed individual
-- message size is 256 KB (262,144 bytes).
--
-- The maximum total payload size (i.e., the sum of all a batch\'s
-- individual message lengths) is also 256 KB (262,144 bytes).
--
-- If the @DelaySeconds@ parameter is not specified for an entry, the
-- default for the queue is used.
--
-- The following list shows the characters (in Unicode) that are allowed in
-- your message, according to the W3C XML specification. For more
-- information, go to <http://www.faqs.org/rfcs/rfc1321.html>. If you send
-- any characters that are not included in the list, your request will be
-- rejected.
--
-- #x9 | #xA | #xD | [#x20 to #xD7FF] | [#xE000 to #xFFFD] | [#x10000 to
-- #x10FFFF]
--
-- Because the batch request can result in a combination of successful and
-- unsuccessful actions, you should check for batch errors even when the
-- call returns an HTTP status code of 200.
--
-- Some API actions take lists of parameters. These lists are specified
-- using the @param.n@ notation. Values of @n@ are integers starting from
-- 1. For example, a parameter list with two elements looks like this:
--
-- @&Attribute.1=this@
--
-- @&Attribute.2=that@
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessageBatch.html>
module Network.AWS.SQS.SendMessageBatch
    (
    -- * Request
      SendMessageBatch
    -- ** Request constructor
    , sendMessageBatch
    -- ** Request lenses
    , smbQueueURL
    , smbEntries

    -- * Response
    , SendMessageBatchResponse
    -- ** Response constructor
    , sendMessageBatchResponse
    -- ** Response lenses
    , smbrStatus
    , smbrSuccessful
    , smbrFailed
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types

-- | /See:/ 'sendMessageBatch' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smbQueueURL'
--
-- * 'smbEntries'
data SendMessageBatch = SendMessageBatch'
    { _smbQueueURL :: !Text
    , _smbEntries  :: ![SendMessageBatchRequestEntry]
    } deriving (Eq,Read,Show)

-- | 'SendMessageBatch' smart constructor.
sendMessageBatch :: Text -> SendMessageBatch
sendMessageBatch pQueueURL =
    SendMessageBatch'
    { _smbQueueURL = pQueueURL
    , _smbEntries = mempty
    }

-- | The URL of the Amazon SQS queue to take action on.
smbQueueURL :: Lens' SendMessageBatch Text
smbQueueURL = lens _smbQueueURL (\ s a -> s{_smbQueueURL = a});

-- | A list of SendMessageBatchRequestEntry items.
smbEntries :: Lens' SendMessageBatch [SendMessageBatchRequestEntry]
smbEntries = lens _smbEntries (\ s a -> s{_smbEntries = a});

instance AWSRequest SendMessageBatch where
        type Sv SendMessageBatch = SQS
        type Rs SendMessageBatch = SendMessageBatchResponse
        request = post
        response
          = receiveXMLWrapper "SendMessageBatchResult"
              (\ s h x ->
                 SendMessageBatchResponse' <$>
                   (pure s) <*>
                     (parseXMLList "SendMessageBatchResultEntry" x)
                     <*> (parseXMLList "BatchResultErrorEntry" x))

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

-- | For each message in the batch, the response contains a
-- SendMessageBatchResultEntry tag if the message succeeds or a
-- BatchResultErrorEntry tag if the message fails.
--
-- /See:/ 'sendMessageBatchResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smbrStatus'
--
-- * 'smbrSuccessful'
--
-- * 'smbrFailed'
data SendMessageBatchResponse = SendMessageBatchResponse'
    { _smbrStatus     :: !Status
    , _smbrSuccessful :: ![SendMessageBatchResultEntry]
    , _smbrFailed     :: ![BatchResultErrorEntry]
    } deriving (Eq,Show)

-- | 'SendMessageBatchResponse' smart constructor.
sendMessageBatchResponse :: Status -> SendMessageBatchResponse
sendMessageBatchResponse pStatus =
    SendMessageBatchResponse'
    { _smbrStatus = pStatus
    , _smbrSuccessful = mempty
    , _smbrFailed = mempty
    }

-- | FIXME: Undocumented member.
smbrStatus :: Lens' SendMessageBatchResponse Status
smbrStatus = lens _smbrStatus (\ s a -> s{_smbrStatus = a});

-- | A list of SendMessageBatchResultEntry items.
smbrSuccessful :: Lens' SendMessageBatchResponse [SendMessageBatchResultEntry]
smbrSuccessful = lens _smbrSuccessful (\ s a -> s{_smbrSuccessful = a});

-- | A list of BatchResultErrorEntry items with the error detail about each
-- message that could not be enqueued.
smbrFailed :: Lens' SendMessageBatchResponse [BatchResultErrorEntry]
smbrFailed = lens _smbrFailed (\ s a -> s{_smbrFailed = a});
