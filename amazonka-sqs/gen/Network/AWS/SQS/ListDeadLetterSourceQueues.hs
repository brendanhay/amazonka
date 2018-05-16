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
-- Module      : Network.AWS.SQS.ListDeadLetterSourceQueues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of your queues that have the @RedrivePolicy@ queue attribute configured with a dead-letter queue.
--
--
-- For more information about using dead-letter queues, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues> in the /Amazon Simple Queue Service Developer Guide/ .
--
module Network.AWS.SQS.ListDeadLetterSourceQueues
    (
    -- * Creating a Request
      listDeadLetterSourceQueues
    , ListDeadLetterSourceQueues
    -- * Request Lenses
    , ldlsqQueueURL

    -- * Destructuring the Response
    , listDeadLetterSourceQueuesResponse
    , ListDeadLetterSourceQueuesResponse
    -- * Response Lenses
    , ldlsqrsResponseStatus
    , ldlsqrsQueueURLs
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
-- /See:/ 'listDeadLetterSourceQueues' smart constructor.
newtype ListDeadLetterSourceQueues = ListDeadLetterSourceQueues'
  { _ldlsqQueueURL :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeadLetterSourceQueues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldlsqQueueURL' - The URL of a dead-letter queue. Queue URLs are case-sensitive.
listDeadLetterSourceQueues
    :: Text -- ^ 'ldlsqQueueURL'
    -> ListDeadLetterSourceQueues
listDeadLetterSourceQueues pQueueURL_ =
  ListDeadLetterSourceQueues' {_ldlsqQueueURL = pQueueURL_}


-- | The URL of a dead-letter queue. Queue URLs are case-sensitive.
ldlsqQueueURL :: Lens' ListDeadLetterSourceQueues Text
ldlsqQueueURL = lens _ldlsqQueueURL (\ s a -> s{_ldlsqQueueURL = a})

instance AWSRequest ListDeadLetterSourceQueues where
        type Rs ListDeadLetterSourceQueues =
             ListDeadLetterSourceQueuesResponse
        request = postQuery sqs
        response
          = receiveXMLWrapper
              "ListDeadLetterSourceQueuesResult"
              (\ s h x ->
                 ListDeadLetterSourceQueuesResponse' <$>
                   (pure (fromEnum s)) <*> (parseXMLList "QueueUrl" x))

instance Hashable ListDeadLetterSourceQueues where

instance NFData ListDeadLetterSourceQueues where

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
--
--
-- /See:/ 'listDeadLetterSourceQueuesResponse' smart constructor.
data ListDeadLetterSourceQueuesResponse = ListDeadLetterSourceQueuesResponse'
  { _ldlsqrsResponseStatus :: !Int
  , _ldlsqrsQueueURLs      :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeadLetterSourceQueuesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldlsqrsResponseStatus' - -- | The response status code.
--
-- * 'ldlsqrsQueueURLs' - A list of source queue URLs that have the @RedrivePolicy@ queue attribute configured with a dead-letter queue.
listDeadLetterSourceQueuesResponse
    :: Int -- ^ 'ldlsqrsResponseStatus'
    -> ListDeadLetterSourceQueuesResponse
listDeadLetterSourceQueuesResponse pResponseStatus_ =
  ListDeadLetterSourceQueuesResponse'
    {_ldlsqrsResponseStatus = pResponseStatus_, _ldlsqrsQueueURLs = mempty}


-- | -- | The response status code.
ldlsqrsResponseStatus :: Lens' ListDeadLetterSourceQueuesResponse Int
ldlsqrsResponseStatus = lens _ldlsqrsResponseStatus (\ s a -> s{_ldlsqrsResponseStatus = a})

-- | A list of source queue URLs that have the @RedrivePolicy@ queue attribute configured with a dead-letter queue.
ldlsqrsQueueURLs :: Lens' ListDeadLetterSourceQueuesResponse [Text]
ldlsqrsQueueURLs = lens _ldlsqrsQueueURLs (\ s a -> s{_ldlsqrsQueueURLs = a}) . _Coerce

instance NFData ListDeadLetterSourceQueuesResponse
         where
