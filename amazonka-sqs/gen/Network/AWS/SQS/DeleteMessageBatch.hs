{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.DeleteMessageBatch
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes up to ten messages from the specified queue. This is a batch
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
-- /See:/ <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_DeleteMessageBatch.html AWS API Reference> for DeleteMessageBatch.
module Network.AWS.SQS.DeleteMessageBatch
    (
    -- * Creating a Request
      DeleteMessageBatch
    , deleteMessageBatch
    -- * Request Lenses
    , dmbQueueURL
    , dmbEntries

    -- * Destructuring the Response
    , DeleteMessageBatchResponse
    , deleteMessageBatchResponse
    -- * Response Lenses
    , dmbrsStatus
    , dmbrsSuccessful
    , dmbrsFailed
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types

-- | /See:/ 'deleteMessageBatch' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmbQueueURL'
--
-- * 'dmbEntries'
data DeleteMessageBatch = DeleteMessageBatch'
    { _dmbQueueURL :: !Text
    , _dmbEntries  :: ![DeleteMessageBatchRequestEntry]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteMessageBatch' smart constructor.
deleteMessageBatch :: Text -> DeleteMessageBatch
deleteMessageBatch pQueueURL_ =
    DeleteMessageBatch'
    { _dmbQueueURL = pQueueURL_
    , _dmbEntries = mempty
    }

-- | The URL of the Amazon SQS queue to take action on.
dmbQueueURL :: Lens' DeleteMessageBatch Text
dmbQueueURL = lens _dmbQueueURL (\ s a -> s{_dmbQueueURL = a});

-- | A list of receipt handles for the messages to be deleted.
dmbEntries :: Lens' DeleteMessageBatch [DeleteMessageBatchRequestEntry]
dmbEntries = lens _dmbEntries (\ s a -> s{_dmbEntries = a}) . _Coerce;

instance AWSRequest DeleteMessageBatch where
        type Sv DeleteMessageBatch = SQS
        type Rs DeleteMessageBatch =
             DeleteMessageBatchResponse
        request = postQuery
        response
          = receiveXMLWrapper "DeleteMessageBatchResult"
              (\ s h x ->
                 DeleteMessageBatchResponse' <$>
                   (pure (fromEnum s)) <*>
                     (parseXMLList "DeleteMessageBatchResultEntry" x)
                     <*> (parseXMLList "BatchResultErrorEntry" x))

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
-- * 'dmbrsStatus'
--
-- * 'dmbrsSuccessful'
--
-- * 'dmbrsFailed'
data DeleteMessageBatchResponse = DeleteMessageBatchResponse'
    { _dmbrsStatus     :: !Int
    , _dmbrsSuccessful :: ![DeleteMessageBatchResultEntry]
    , _dmbrsFailed     :: ![BatchResultErrorEntry]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteMessageBatchResponse' smart constructor.
deleteMessageBatchResponse :: Int -> DeleteMessageBatchResponse
deleteMessageBatchResponse pStatus_ =
    DeleteMessageBatchResponse'
    { _dmbrsStatus = pStatus_
    , _dmbrsSuccessful = mempty
    , _dmbrsFailed = mempty
    }

-- | Undocumented member.
dmbrsStatus :: Lens' DeleteMessageBatchResponse Int
dmbrsStatus = lens _dmbrsStatus (\ s a -> s{_dmbrsStatus = a});

-- | A list of DeleteMessageBatchResultEntry items.
dmbrsSuccessful :: Lens' DeleteMessageBatchResponse [DeleteMessageBatchResultEntry]
dmbrsSuccessful = lens _dmbrsSuccessful (\ s a -> s{_dmbrsSuccessful = a}) . _Coerce;

-- | A list of BatchResultErrorEntry items.
dmbrsFailed :: Lens' DeleteMessageBatchResponse [BatchResultErrorEntry]
dmbrsFailed = lens _dmbrsFailed (\ s a -> s{_dmbrsFailed = a}) . _Coerce;
