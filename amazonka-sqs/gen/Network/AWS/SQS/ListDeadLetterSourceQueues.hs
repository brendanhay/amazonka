{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.ListDeadLetterSourceQueues
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of your queues that have the RedrivePolicy queue
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
    , ldlsqrsStatus
    , ldlsqrsQueueURLs
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types

-- | /See:/ 'listDeadLetterSourceQueues' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldlsqQueueURL'
newtype ListDeadLetterSourceQueues = ListDeadLetterSourceQueues'
    { _ldlsqQueueURL :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDeadLetterSourceQueues' smart constructor.
listDeadLetterSourceQueues :: Text -> ListDeadLetterSourceQueues
listDeadLetterSourceQueues pQueueURL_ =
    ListDeadLetterSourceQueues'
    { _ldlsqQueueURL = pQueueURL_
    }

-- | The queue URL of a dead letter queue.
ldlsqQueueURL :: Lens' ListDeadLetterSourceQueues Text
ldlsqQueueURL = lens _ldlsqQueueURL (\ s a -> s{_ldlsqQueueURL = a});

instance AWSRequest ListDeadLetterSourceQueues where
        type Sv ListDeadLetterSourceQueues = SQS
        type Rs ListDeadLetterSourceQueues =
             ListDeadLetterSourceQueuesResponse
        request = post "ListDeadLetterSourceQueues"
        response
          = receiveXMLWrapper
              "ListDeadLetterSourceQueuesResult"
              (\ s h x ->
                 ListDeadLetterSourceQueuesResponse' <$>
                   (pure (fromEnum s)) <*> (parseXMLList "QueueUrl" x))

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

-- | A list of your dead letter source queues.
--
-- /See:/ 'listDeadLetterSourceQueuesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldlsqrsStatus'
--
-- * 'ldlsqrsQueueURLs'
data ListDeadLetterSourceQueuesResponse = ListDeadLetterSourceQueuesResponse'
    { _ldlsqrsStatus    :: !Int
    , _ldlsqrsQueueURLs :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDeadLetterSourceQueuesResponse' smart constructor.
listDeadLetterSourceQueuesResponse :: Int -> ListDeadLetterSourceQueuesResponse
listDeadLetterSourceQueuesResponse pStatus_ =
    ListDeadLetterSourceQueuesResponse'
    { _ldlsqrsStatus = pStatus_
    , _ldlsqrsQueueURLs = mempty
    }

-- | FIXME: Undocumented member.
ldlsqrsStatus :: Lens' ListDeadLetterSourceQueuesResponse Int
ldlsqrsStatus = lens _ldlsqrsStatus (\ s a -> s{_ldlsqrsStatus = a});

-- | A list of source queue URLs that have the RedrivePolicy queue attribute
-- configured with a dead letter queue.
ldlsqrsQueueURLs :: Lens' ListDeadLetterSourceQueuesResponse [Text]
ldlsqrsQueueURLs = lens _ldlsqrsQueueURLs (\ s a -> s{_ldlsqrsQueueURLs = a});
