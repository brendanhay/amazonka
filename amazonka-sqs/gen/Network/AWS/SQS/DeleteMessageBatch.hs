{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SQS.DeleteMessageBatch
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

-- | Deletes up to ten messages from the specified queue. This is a batch
-- version of DeleteMessage. The result of the delete action on each
-- message is reported individually in the response.
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
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_DeleteMessageBatch.html>
module Network.AWS.SQS.DeleteMessageBatch
    (
    -- * Request
      DeleteMessageBatch
    -- ** Request constructor
    , deleteMessageBatch
    -- ** Request lenses
    , dmbQueueURL
    , dmbEntries

    -- * Response
    , DeleteMessageBatchResponse
    -- ** Response constructor
    , deleteMessageBatchResponse
    -- ** Response lenses
    , dmbrSuccessful
    , dmbrFailed
    , dmbrStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types

-- | /See:/ 'deleteMessageBatch' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmbQueueURL'
--
-- * 'dmbEntries'
data DeleteMessageBatch = DeleteMessageBatch'{_dmbQueueURL :: Text, _dmbEntries :: [DeleteMessageBatchRequestEntry]} deriving (Eq, Read, Show)

-- | 'DeleteMessageBatch' smart constructor.
deleteMessageBatch :: Text -> DeleteMessageBatch
deleteMessageBatch pQueueURL = DeleteMessageBatch'{_dmbQueueURL = pQueueURL, _dmbEntries = mempty};

-- | The URL of the Amazon SQS queue to take action on.
dmbQueueURL :: Lens' DeleteMessageBatch Text
dmbQueueURL = lens _dmbQueueURL (\ s a -> s{_dmbQueueURL = a});

-- | A list of receipt handles for the messages to be deleted.
dmbEntries :: Lens' DeleteMessageBatch [DeleteMessageBatchRequestEntry]
dmbEntries = lens _dmbEntries (\ s a -> s{_dmbEntries = a});

instance AWSRequest DeleteMessageBatch where
        type Sv DeleteMessageBatch = SQS
        type Rs DeleteMessageBatch =
             DeleteMessageBatchResponse
        request = post
        response
          = receiveXMLWrapper "DeleteMessageBatchResult"
              (\ s h x ->
                 DeleteMessageBatchResponse' <$>
                   (parseXMLList "DeleteMessageBatchResultEntry" x) <*>
                     (parseXMLList "BatchResultErrorEntry" x)
                     <*> (pure (fromEnum s)))

instance ToHeaders DeleteMessageBatch where
        toHeaders = const mempty

instance ToPath DeleteMessageBatch where
        toPath = const "/"

instance ToQuery DeleteMessageBatch where
        toQuery DeleteMessageBatch'{..}
          = mconcat
              ["Action" =: ("DeleteMessageBatch" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueUrl" =: _dmbQueueURL,
               toQueryList "DeleteMessageBatchRequestEntry"
                 _dmbEntries]

-- | For each message in the batch, the response contains a
-- DeleteMessageBatchResultEntry tag if the message is deleted or a
-- BatchResultErrorEntry tag if the message cannot be deleted.
--
-- /See:/ 'deleteMessageBatchResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmbrSuccessful'
--
-- * 'dmbrFailed'
--
-- * 'dmbrStatusCode'
data DeleteMessageBatchResponse = DeleteMessageBatchResponse'{_dmbrSuccessful :: [DeleteMessageBatchResultEntry], _dmbrFailed :: [BatchResultErrorEntry], _dmbrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DeleteMessageBatchResponse' smart constructor.
deleteMessageBatchResponse :: Int -> DeleteMessageBatchResponse
deleteMessageBatchResponse pStatusCode = DeleteMessageBatchResponse'{_dmbrSuccessful = mempty, _dmbrFailed = mempty, _dmbrStatusCode = pStatusCode};

-- | A list of DeleteMessageBatchResultEntry items.
dmbrSuccessful :: Lens' DeleteMessageBatchResponse [DeleteMessageBatchResultEntry]
dmbrSuccessful = lens _dmbrSuccessful (\ s a -> s{_dmbrSuccessful = a});

-- | A list of BatchResultErrorEntry items.
dmbrFailed :: Lens' DeleteMessageBatchResponse [BatchResultErrorEntry]
dmbrFailed = lens _dmbrFailed (\ s a -> s{_dmbrFailed = a});

-- | FIXME: Undocumented member.
dmbrStatusCode :: Lens' DeleteMessageBatchResponse Int
dmbrStatusCode = lens _dmbrStatusCode (\ s a -> s{_dmbrStatusCode = a});
