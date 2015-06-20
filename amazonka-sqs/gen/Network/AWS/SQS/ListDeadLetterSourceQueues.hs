{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SQS.ListDeadLetterSourceQueues
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

-- | Returns a list of your queues that have the RedrivePolicy queue
-- attribute configured with a dead letter queue.
--
-- For more information about using dead letter queues, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSDeadLetterQueue.html Using Amazon SQS Dead Letter Queues>.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ListDeadLetterSourceQueues.html>
module Network.AWS.SQS.ListDeadLetterSourceQueues
    (
    -- * Request
      ListDeadLetterSourceQueues
    -- ** Request constructor
    , listDeadLetterSourceQueues
    -- ** Request lenses
    , ldlsqQueueURL

    -- * Response
    , ListDeadLetterSourceQueuesResponse
    -- ** Response constructor
    , listDeadLetterSourceQueuesResponse
    -- ** Response lenses
    , ldlsqrQueueURLs
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types

-- | /See:/ 'listDeadLetterSourceQueues' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldlsqQueueURL'
newtype ListDeadLetterSourceQueues = ListDeadLetterSourceQueues'{_ldlsqQueueURL :: Text} deriving (Eq, Read, Show)

-- | 'ListDeadLetterSourceQueues' smart constructor.
listDeadLetterSourceQueues :: Text -> ListDeadLetterSourceQueues
listDeadLetterSourceQueues pQueueURL = ListDeadLetterSourceQueues'{_ldlsqQueueURL = pQueueURL};

-- | The queue URL of a dead letter queue.
ldlsqQueueURL :: Lens' ListDeadLetterSourceQueues Text
ldlsqQueueURL = lens _ldlsqQueueURL (\ s a -> s{_ldlsqQueueURL = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest ListDeadLetterSourceQueues where
        type Sv ListDeadLetterSourceQueues = SQS
        type Rs ListDeadLetterSourceQueues =
             ListDeadLetterSourceQueuesResponse
        request = post
        response
          = receiveXMLWrapper
              "ListDeadLetterSourceQueuesResult"
              (\ s h x ->
                 ListDeadLetterSourceQueuesResponse' <$>
                   (parseXMLList "QueueUrl" x))

instance ToHeaders ListDeadLetterSourceQueues where
        toHeaders = const mempty

instance ToPath ListDeadLetterSourceQueues where
        toPath = const "/"

instance ToQuery ListDeadLetterSourceQueues where
        toQuery ListDeadLetterSourceQueues'{..}
          = mconcat
              ["Action" =:
                 ("ListDeadLetterSourceQueues" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueUrl" =: _ldlsqQueueURL]

-- | /See:/ 'listDeadLetterSourceQueuesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldlsqrQueueURLs'
newtype ListDeadLetterSourceQueuesResponse = ListDeadLetterSourceQueuesResponse'{_ldlsqrQueueURLs :: [Text]} deriving (Eq, Read, Show)

-- | 'ListDeadLetterSourceQueuesResponse' smart constructor.
listDeadLetterSourceQueuesResponse :: ListDeadLetterSourceQueuesResponse
listDeadLetterSourceQueuesResponse = ListDeadLetterSourceQueuesResponse'{_ldlsqrQueueURLs = mempty};

-- | A list of source queue URLs that have the RedrivePolicy queue attribute
-- configured with a dead letter queue.
ldlsqrQueueURLs :: Lens' ListDeadLetterSourceQueuesResponse [Text]
ldlsqrQueueURLs = lens _ldlsqrQueueURLs (\ s a -> s{_ldlsqrQueueURLs = a});
