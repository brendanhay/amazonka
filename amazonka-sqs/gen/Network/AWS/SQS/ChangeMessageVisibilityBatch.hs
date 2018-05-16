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
-- Module      : Network.AWS.SQS.ChangeMessageVisibilityBatch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the visibility timeout of multiple messages. This is a batch version of @'ChangeMessageVisibility' .@ The result of the action on each message is reported individually in the response. You can send up to 10 @'ChangeMessageVisibility' @ requests with each @ChangeMessageVisibilityBatch@ action.
--
--
-- /Important:/ Because the batch request can result in a combination of successful and unsuccessful actions, you should check for batch errors even when the call returns an HTTP status code of @200@ .
--
module Network.AWS.SQS.ChangeMessageVisibilityBatch
    (
    -- * Creating a Request
      changeMessageVisibilityBatch
    , ChangeMessageVisibilityBatch
    -- * Request Lenses
    , cmvbQueueURL
    , cmvbEntries

    -- * Destructuring the Response
    , changeMessageVisibilityBatchResponse
    , ChangeMessageVisibilityBatchResponse
    -- * Response Lenses
    , cmvbrsResponseStatus
    , cmvbrsSuccessful
    , cmvbrsFailed
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
-- /See:/ 'changeMessageVisibilityBatch' smart constructor.
data ChangeMessageVisibilityBatch = ChangeMessageVisibilityBatch'
  { _cmvbQueueURL :: !Text
  , _cmvbEntries  :: ![ChangeMessageVisibilityBatchRequestEntry]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangeMessageVisibilityBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmvbQueueURL' - The URL of the Amazon SQS queue whose messages' visibility is changed. Queue URLs are case-sensitive.
--
-- * 'cmvbEntries' - A list of receipt handles of the messages for which the visibility timeout must be changed.
changeMessageVisibilityBatch
    :: Text -- ^ 'cmvbQueueURL'
    -> ChangeMessageVisibilityBatch
changeMessageVisibilityBatch pQueueURL_ =
  ChangeMessageVisibilityBatch'
    {_cmvbQueueURL = pQueueURL_, _cmvbEntries = mempty}


-- | The URL of the Amazon SQS queue whose messages' visibility is changed. Queue URLs are case-sensitive.
cmvbQueueURL :: Lens' ChangeMessageVisibilityBatch Text
cmvbQueueURL = lens _cmvbQueueURL (\ s a -> s{_cmvbQueueURL = a})

-- | A list of receipt handles of the messages for which the visibility timeout must be changed.
cmvbEntries :: Lens' ChangeMessageVisibilityBatch [ChangeMessageVisibilityBatchRequestEntry]
cmvbEntries = lens _cmvbEntries (\ s a -> s{_cmvbEntries = a}) . _Coerce

instance AWSRequest ChangeMessageVisibilityBatch
         where
        type Rs ChangeMessageVisibilityBatch =
             ChangeMessageVisibilityBatchResponse
        request = postQuery sqs
        response
          = receiveXMLWrapper
              "ChangeMessageVisibilityBatchResult"
              (\ s h x ->
                 ChangeMessageVisibilityBatchResponse' <$>
                   (pure (fromEnum s)) <*>
                     (parseXMLList
                        "ChangeMessageVisibilityBatchResultEntry"
                        x)
                     <*> (parseXMLList "BatchResultErrorEntry" x))

instance Hashable ChangeMessageVisibilityBatch where

instance NFData ChangeMessageVisibilityBatch where

instance ToHeaders ChangeMessageVisibilityBatch where
        toHeaders = const mempty

instance ToPath ChangeMessageVisibilityBatch where
        toPath = const "/"

instance ToQuery ChangeMessageVisibilityBatch where
        toQuery ChangeMessageVisibilityBatch'{..}
          = mconcat
              ["Action" =:
                 ("ChangeMessageVisibilityBatch" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueUrl" =: _cmvbQueueURL,
               toQueryList
                 "ChangeMessageVisibilityBatchRequestEntry"
                 _cmvbEntries]

-- | For each message in the batch, the response contains a @'ChangeMessageVisibilityBatchResultEntry' @ tag if the message succeeds or a @'BatchResultErrorEntry' @ tag if the message fails.
--
--
--
-- /See:/ 'changeMessageVisibilityBatchResponse' smart constructor.
data ChangeMessageVisibilityBatchResponse = ChangeMessageVisibilityBatchResponse'
  { _cmvbrsResponseStatus :: !Int
  , _cmvbrsSuccessful     :: ![ChangeMessageVisibilityBatchResultEntry]
  , _cmvbrsFailed         :: ![BatchResultErrorEntry]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangeMessageVisibilityBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmvbrsResponseStatus' - -- | The response status code.
--
-- * 'cmvbrsSuccessful' - A list of @'ChangeMessageVisibilityBatchResultEntry' @ items.
--
-- * 'cmvbrsFailed' - A list of @'BatchResultErrorEntry' @ items.
changeMessageVisibilityBatchResponse
    :: Int -- ^ 'cmvbrsResponseStatus'
    -> ChangeMessageVisibilityBatchResponse
changeMessageVisibilityBatchResponse pResponseStatus_ =
  ChangeMessageVisibilityBatchResponse'
    { _cmvbrsResponseStatus = pResponseStatus_
    , _cmvbrsSuccessful = mempty
    , _cmvbrsFailed = mempty
    }


-- | -- | The response status code.
cmvbrsResponseStatus :: Lens' ChangeMessageVisibilityBatchResponse Int
cmvbrsResponseStatus = lens _cmvbrsResponseStatus (\ s a -> s{_cmvbrsResponseStatus = a})

-- | A list of @'ChangeMessageVisibilityBatchResultEntry' @ items.
cmvbrsSuccessful :: Lens' ChangeMessageVisibilityBatchResponse [ChangeMessageVisibilityBatchResultEntry]
cmvbrsSuccessful = lens _cmvbrsSuccessful (\ s a -> s{_cmvbrsSuccessful = a}) . _Coerce

-- | A list of @'BatchResultErrorEntry' @ items.
cmvbrsFailed :: Lens' ChangeMessageVisibilityBatchResponse [BatchResultErrorEntry]
cmvbrsFailed = lens _cmvbrsFailed (\ s a -> s{_cmvbrsFailed = a}) . _Coerce

instance NFData ChangeMessageVisibilityBatchResponse
         where
