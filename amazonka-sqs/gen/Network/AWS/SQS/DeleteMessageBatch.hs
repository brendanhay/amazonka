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
-- Module      : Network.AWS.SQS.DeleteMessageBatch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes up to ten messages from the specified queue. This is a batch version of @'DeleteMessage' .@ The result of the action on each message is reported individually in the response.
--
--
-- /Important:/ Because the batch request can result in a combination of successful and unsuccessful actions, you should check for batch errors even when the call returns an HTTP status code of @200@ .
--
module Network.AWS.SQS.DeleteMessageBatch
    (
    -- * Creating a Request
      deleteMessageBatch
    , DeleteMessageBatch
    -- * Request Lenses
    , dmbQueueURL
    , dmbEntries

    -- * Destructuring the Response
    , deleteMessageBatchResponse
    , DeleteMessageBatchResponse
    -- * Response Lenses
    , dmbrsResponseStatus
    , dmbrsSuccessful
    , dmbrsFailed
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
-- /See:/ 'deleteMessageBatch' smart constructor.
data DeleteMessageBatch = DeleteMessageBatch'
  { _dmbQueueURL :: !Text
  , _dmbEntries  :: ![DeleteMessageBatchRequestEntry]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMessageBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmbQueueURL' - The URL of the Amazon SQS queue from which messages are deleted. Queue URLs are case-sensitive.
--
-- * 'dmbEntries' - A list of receipt handles for the messages to be deleted.
deleteMessageBatch
    :: Text -- ^ 'dmbQueueURL'
    -> DeleteMessageBatch
deleteMessageBatch pQueueURL_ =
  DeleteMessageBatch' {_dmbQueueURL = pQueueURL_, _dmbEntries = mempty}


-- | The URL of the Amazon SQS queue from which messages are deleted. Queue URLs are case-sensitive.
dmbQueueURL :: Lens' DeleteMessageBatch Text
dmbQueueURL = lens _dmbQueueURL (\ s a -> s{_dmbQueueURL = a})

-- | A list of receipt handles for the messages to be deleted.
dmbEntries :: Lens' DeleteMessageBatch [DeleteMessageBatchRequestEntry]
dmbEntries = lens _dmbEntries (\ s a -> s{_dmbEntries = a}) . _Coerce

instance AWSRequest DeleteMessageBatch where
        type Rs DeleteMessageBatch =
             DeleteMessageBatchResponse
        request = postQuery sqs
        response
          = receiveXMLWrapper "DeleteMessageBatchResult"
              (\ s h x ->
                 DeleteMessageBatchResponse' <$>
                   (pure (fromEnum s)) <*>
                     (parseXMLList "DeleteMessageBatchResultEntry" x)
                     <*> (parseXMLList "BatchResultErrorEntry" x))

instance Hashable DeleteMessageBatch where

instance NFData DeleteMessageBatch where

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

-- | For each message in the batch, the response contains a @'DeleteMessageBatchResultEntry' @ tag if the message is deleted or a @'BatchResultErrorEntry' @ tag if the message can't be deleted.
--
--
--
-- /See:/ 'deleteMessageBatchResponse' smart constructor.
data DeleteMessageBatchResponse = DeleteMessageBatchResponse'
  { _dmbrsResponseStatus :: !Int
  , _dmbrsSuccessful     :: ![DeleteMessageBatchResultEntry]
  , _dmbrsFailed         :: ![BatchResultErrorEntry]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMessageBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmbrsResponseStatus' - -- | The response status code.
--
-- * 'dmbrsSuccessful' - A list of @'DeleteMessageBatchResultEntry' @ items.
--
-- * 'dmbrsFailed' - A list of @'BatchResultErrorEntry' @ items.
deleteMessageBatchResponse
    :: Int -- ^ 'dmbrsResponseStatus'
    -> DeleteMessageBatchResponse
deleteMessageBatchResponse pResponseStatus_ =
  DeleteMessageBatchResponse'
    { _dmbrsResponseStatus = pResponseStatus_
    , _dmbrsSuccessful = mempty
    , _dmbrsFailed = mempty
    }


-- | -- | The response status code.
dmbrsResponseStatus :: Lens' DeleteMessageBatchResponse Int
dmbrsResponseStatus = lens _dmbrsResponseStatus (\ s a -> s{_dmbrsResponseStatus = a})

-- | A list of @'DeleteMessageBatchResultEntry' @ items.
dmbrsSuccessful :: Lens' DeleteMessageBatchResponse [DeleteMessageBatchResultEntry]
dmbrsSuccessful = lens _dmbrsSuccessful (\ s a -> s{_dmbrsSuccessful = a}) . _Coerce

-- | A list of @'BatchResultErrorEntry' @ items.
dmbrsFailed :: Lens' DeleteMessageBatchResponse [BatchResultErrorEntry]
dmbrsFailed = lens _dmbrsFailed (\ s a -> s{_dmbrsFailed = a}) . _Coerce

instance NFData DeleteMessageBatchResponse where
