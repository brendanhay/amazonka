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
-- Module      : Network.AWS.SQS.ListQueues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of your queues. The maximum number of queues that can be returned is 1,000. If you specify a value for the optional @QueueNamePrefix@ parameter, only queues with a name that begins with the specified value are returned.
--
--
module Network.AWS.SQS.ListQueues
    (
    -- * Creating a Request
      listQueues
    , ListQueues
    -- * Request Lenses
    , lqQueueNamePrefix

    -- * Destructuring the Response
    , listQueuesResponse
    , ListQueuesResponse
    -- * Response Lenses
    , lqrsQueueURLs
    , lqrsResponseStatus
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
-- /See:/ 'listQueues' smart constructor.
newtype ListQueues = ListQueues'
  { _lqQueueNamePrefix :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListQueues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqQueueNamePrefix' - A string to use for filtering the list results. Only those queues whose name begins with the specified string are returned. Queue names are case-sensitive.
listQueues
    :: ListQueues
listQueues = ListQueues' {_lqQueueNamePrefix = Nothing}


-- | A string to use for filtering the list results. Only those queues whose name begins with the specified string are returned. Queue names are case-sensitive.
lqQueueNamePrefix :: Lens' ListQueues (Maybe Text)
lqQueueNamePrefix = lens _lqQueueNamePrefix (\ s a -> s{_lqQueueNamePrefix = a})

instance AWSRequest ListQueues where
        type Rs ListQueues = ListQueuesResponse
        request = postQuery sqs
        response
          = receiveXMLWrapper "ListQueuesResult"
              (\ s h x ->
                 ListQueuesResponse' <$>
                   (may (parseXMLList "QueueUrl") x) <*>
                     (pure (fromEnum s)))

instance Hashable ListQueues where

instance NFData ListQueues where

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
--
--
-- /See:/ 'listQueuesResponse' smart constructor.
data ListQueuesResponse = ListQueuesResponse'
  { _lqrsQueueURLs      :: !(Maybe [Text])
  , _lqrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListQueuesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqrsQueueURLs' - A list of queue URLs, up to 1,000 entries.
--
-- * 'lqrsResponseStatus' - -- | The response status code.
listQueuesResponse
    :: Int -- ^ 'lqrsResponseStatus'
    -> ListQueuesResponse
listQueuesResponse pResponseStatus_ =
  ListQueuesResponse'
    {_lqrsQueueURLs = Nothing, _lqrsResponseStatus = pResponseStatus_}


-- | A list of queue URLs, up to 1,000 entries.
lqrsQueueURLs :: Lens' ListQueuesResponse [Text]
lqrsQueueURLs = lens _lqrsQueueURLs (\ s a -> s{_lqrsQueueURLs = a}) . _Default . _Coerce

-- | -- | The response status code.
lqrsResponseStatus :: Lens' ListQueuesResponse Int
lqrsResponseStatus = lens _lqrsResponseStatus (\ s a -> s{_lqrsResponseStatus = a})

instance NFData ListQueuesResponse where
