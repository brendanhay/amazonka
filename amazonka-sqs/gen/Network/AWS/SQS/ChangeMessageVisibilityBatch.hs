{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SQS.ChangeMessageVisibilityBatch
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

-- | Changes the visibility timeout of multiple messages. This is a batch
-- version of ChangeMessageVisibility. The result of the action on each
-- message is reported individually in the response. You can send up to 10
-- ChangeMessageVisibility requests with each
-- @ChangeMessageVisibilityBatch@ action.
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
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ChangeMessageVisibilityBatch.html>
module Network.AWS.SQS.ChangeMessageVisibilityBatch
    (
    -- * Request
      ChangeMessageVisibilityBatch
    -- ** Request constructor
    , changeMessageVisibilityBatch
    -- ** Request lenses
    , cmvbQueueURL
    , cmvbEntries

    -- * Response
    , ChangeMessageVisibilityBatchResponse
    -- ** Response constructor
    , changeMessageVisibilityBatchResponse
    -- ** Response lenses
    , cmvbrSuccessful
    , cmvbrFailed
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types

-- | /See:/ 'changeMessageVisibilityBatch' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmvbQueueURL'
--
-- * 'cmvbEntries'
data ChangeMessageVisibilityBatch = ChangeMessageVisibilityBatch'{_cmvbQueueURL :: Text, _cmvbEntries :: [ChangeMessageVisibilityBatchRequestEntry]} deriving (Eq, Read, Show)

-- | 'ChangeMessageVisibilityBatch' smart constructor.
changeMessageVisibilityBatch :: Text -> ChangeMessageVisibilityBatch
changeMessageVisibilityBatch pQueueURL = ChangeMessageVisibilityBatch'{_cmvbQueueURL = pQueueURL, _cmvbEntries = mempty};

-- | The URL of the Amazon SQS queue to take action on.
cmvbQueueURL :: Lens' ChangeMessageVisibilityBatch Text
cmvbQueueURL = lens _cmvbQueueURL (\ s a -> s{_cmvbQueueURL = a});

-- | A list of receipt handles of the messages for which the visibility
-- timeout must be changed.
cmvbEntries :: Lens' ChangeMessageVisibilityBatch [ChangeMessageVisibilityBatchRequestEntry]
cmvbEntries = lens _cmvbEntries (\ s a -> s{_cmvbEntries = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest ChangeMessageVisibilityBatch
         where
        type Sv ChangeMessageVisibilityBatch = SQS
        type Rs ChangeMessageVisibilityBatch =
             ChangeMessageVisibilityBatchResponse
        request = post
        response
          = receiveXMLWrapper
              "ChangeMessageVisibilityBatchResult"
              (\ s h x ->
                 ChangeMessageVisibilityBatchResponse' <$>
                   (parseXMLList
                      "ChangeMessageVisibilityBatchResultEntry"
                      x)
                     <*> (parseXMLList "BatchResultErrorEntry" x))

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

-- | /See:/ 'changeMessageVisibilityBatchResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmvbrSuccessful'
--
-- * 'cmvbrFailed'
data ChangeMessageVisibilityBatchResponse = ChangeMessageVisibilityBatchResponse'{_cmvbrSuccessful :: [ChangeMessageVisibilityBatchResultEntry], _cmvbrFailed :: [BatchResultErrorEntry]} deriving (Eq, Read, Show)

-- | 'ChangeMessageVisibilityBatchResponse' smart constructor.
changeMessageVisibilityBatchResponse :: ChangeMessageVisibilityBatchResponse
changeMessageVisibilityBatchResponse = ChangeMessageVisibilityBatchResponse'{_cmvbrSuccessful = mempty, _cmvbrFailed = mempty};

-- | A list of ChangeMessageVisibilityBatchResultEntry items.
cmvbrSuccessful :: Lens' ChangeMessageVisibilityBatchResponse [ChangeMessageVisibilityBatchResultEntry]
cmvbrSuccessful = lens _cmvbrSuccessful (\ s a -> s{_cmvbrSuccessful = a});

-- | A list of BatchResultErrorEntry items.
cmvbrFailed :: Lens' ChangeMessageVisibilityBatchResponse [BatchResultErrorEntry]
cmvbrFailed = lens _cmvbrFailed (\ s a -> s{_cmvbrFailed = a});
