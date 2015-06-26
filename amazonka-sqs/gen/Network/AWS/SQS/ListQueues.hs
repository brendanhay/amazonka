{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SQS.ListQueues
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

-- | Returns a list of your queues. The maximum number of queues that can be
-- returned is 1000. If you specify a value for the optional
-- @QueueNamePrefix@ parameter, only queues with a name beginning with the
-- specified value are returned.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ListQueues.html>
module Network.AWS.SQS.ListQueues
    (
    -- * Request
      ListQueues
    -- ** Request constructor
    , listQueues
    -- ** Request lenses
    , lqQueueNamePrefix

    -- * Response
    , ListQueuesResponse
    -- ** Response constructor
    , listQueuesResponse
    -- ** Response lenses
    , lqrQueueURLs
    , lqrStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types

-- | /See:/ 'listQueues' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lqQueueNamePrefix'
newtype ListQueues = ListQueues'{_lqQueueNamePrefix :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListQueues' smart constructor.
listQueues :: ListQueues
listQueues = ListQueues'{_lqQueueNamePrefix = Nothing};

-- | A string to use for filtering the list results. Only those queues whose
-- name begins with the specified string are returned.
lqQueueNamePrefix :: Lens' ListQueues (Maybe Text)
lqQueueNamePrefix = lens _lqQueueNamePrefix (\ s a -> s{_lqQueueNamePrefix = a});

instance AWSRequest ListQueues where
        type Sv ListQueues = SQS
        type Rs ListQueues = ListQueuesResponse
        request = post
        response
          = receiveXMLWrapper "ListQueuesResult"
              (\ s h x ->
                 ListQueuesResponse' <$>
                   (may (parseXMLList "QueueUrl") x) <*>
                     (pure (fromEnum s)))

instance ToHeaders ListQueues where
        toHeaders = const mempty

instance ToPath ListQueues where
        toPath = const "/"

instance ToQuery ListQueues where
        toQuery ListQueues'{..}
          = mconcat
              ["Action" =: ("ListQueues" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueNamePrefix" =: _lqQueueNamePrefix]

-- | A list of your queues.
--
-- /See:/ 'listQueuesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lqrQueueURLs'
--
-- * 'lqrStatusCode'
data ListQueuesResponse = ListQueuesResponse'{_lqrQueueURLs :: Maybe [Text], _lqrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'ListQueuesResponse' smart constructor.
listQueuesResponse :: Int -> ListQueuesResponse
listQueuesResponse pStatusCode = ListQueuesResponse'{_lqrQueueURLs = Nothing, _lqrStatusCode = pStatusCode};

-- | A list of queue URLs, up to 1000 entries.
lqrQueueURLs :: Lens' ListQueuesResponse [Text]
lqrQueueURLs = lens _lqrQueueURLs (\ s a -> s{_lqrQueueURLs = a}) . _Default;

-- | FIXME: Undocumented member.
lqrStatusCode :: Lens' ListQueuesResponse Int
lqrStatusCode = lens _lqrStatusCode (\ s a -> s{_lqrStatusCode = a});
