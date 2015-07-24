{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.ListQueues
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of your queues. The maximum number of queues that can be
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
    , lqrsQueueURLs
    , lqrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types

-- | /See:/ 'listQueues' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lqQueueNamePrefix'
newtype ListQueues = ListQueues'
    { _lqQueueNamePrefix :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListQueues' smart constructor.
listQueues :: ListQueues
listQueues =
    ListQueues'
    { _lqQueueNamePrefix = Nothing
    }

-- | A string to use for filtering the list results. Only those queues whose
-- name begins with the specified string are returned.
lqQueueNamePrefix :: Lens' ListQueues (Maybe Text)
lqQueueNamePrefix = lens _lqQueueNamePrefix (\ s a -> s{_lqQueueNamePrefix = a});

instance AWSRequest ListQueues where
        type Sv ListQueues = SQS
        type Rs ListQueues = ListQueuesResponse
        request = postQuery
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
-- * 'lqrsQueueURLs'
--
-- * 'lqrsStatus'
data ListQueuesResponse = ListQueuesResponse'
    { _lqrsQueueURLs :: !(Maybe [Text])
    , _lqrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListQueuesResponse' smart constructor.
listQueuesResponse :: Int -> ListQueuesResponse
listQueuesResponse pStatus_ =
    ListQueuesResponse'
    { _lqrsQueueURLs = Nothing
    , _lqrsStatus = pStatus_
    }

-- | A list of queue URLs, up to 1000 entries.
lqrsQueueURLs :: Lens' ListQueuesResponse [Text]
lqrsQueueURLs = lens _lqrsQueueURLs (\ s a -> s{_lqrsQueueURLs = a}) . _Default;

-- | FIXME: Undocumented member.
lqrsStatus :: Lens' ListQueuesResponse Int
lqrsStatus = lens _lqrsStatus (\ s a -> s{_lqrsStatus = a});
