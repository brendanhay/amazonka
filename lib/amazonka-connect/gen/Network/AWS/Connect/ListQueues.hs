{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the queues for the specified Amazon Connect instance.
--
--
-- For more information about queues, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-queues-standard-and-agent.html Queues: Standard and Agent> in the /Amazon Connect Administrator Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListQueues
  ( -- * Creating a Request
    listQueues,
    ListQueues,

    -- * Request Lenses
    lqNextToken,
    lqQueueTypes,
    lqMaxResults,
    lqInstanceId,

    -- * Destructuring the Response
    listQueuesResponse,
    ListQueuesResponse,

    -- * Response Lenses
    lqrsNextToken,
    lqrsQueueSummaryList,
    lqrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listQueues' smart constructor.
data ListQueues = ListQueues'
  { _lqNextToken :: !(Maybe Text),
    _lqQueueTypes :: !(Maybe [QueueType]),
    _lqMaxResults :: !(Maybe Nat),
    _lqInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListQueues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'lqQueueTypes' - The type of queue.
--
-- * 'lqMaxResults' - The maximimum number of results to return per page.
--
-- * 'lqInstanceId' - The identifier of the Amazon Connect instance.
listQueues ::
  -- | 'lqInstanceId'
  Text ->
  ListQueues
listQueues pInstanceId_ =
  ListQueues'
    { _lqNextToken = Nothing,
      _lqQueueTypes = Nothing,
      _lqMaxResults = Nothing,
      _lqInstanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
lqNextToken :: Lens' ListQueues (Maybe Text)
lqNextToken = lens _lqNextToken (\s a -> s {_lqNextToken = a})

-- | The type of queue.
lqQueueTypes :: Lens' ListQueues [QueueType]
lqQueueTypes = lens _lqQueueTypes (\s a -> s {_lqQueueTypes = a}) . _Default . _Coerce

-- | The maximimum number of results to return per page.
lqMaxResults :: Lens' ListQueues (Maybe Natural)
lqMaxResults = lens _lqMaxResults (\s a -> s {_lqMaxResults = a}) . mapping _Nat

-- | The identifier of the Amazon Connect instance.
lqInstanceId :: Lens' ListQueues Text
lqInstanceId = lens _lqInstanceId (\s a -> s {_lqInstanceId = a})

instance AWSPager ListQueues where
  page rq rs
    | stop (rs ^. lqrsNextToken) = Nothing
    | stop (rs ^. lqrsQueueSummaryList) = Nothing
    | otherwise = Just $ rq & lqNextToken .~ rs ^. lqrsNextToken

instance AWSRequest ListQueues where
  type Rs ListQueues = ListQueuesResponse
  request = get connect
  response =
    receiveJSON
      ( \s h x ->
          ListQueuesResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "QueueSummaryList" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListQueues

instance NFData ListQueues

instance ToHeaders ListQueues where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListQueues where
  toPath ListQueues' {..} =
    mconcat ["/queues-summary/", toBS _lqInstanceId]

instance ToQuery ListQueues where
  toQuery ListQueues' {..} =
    mconcat
      [ "nextToken" =: _lqNextToken,
        "queueTypes" =: toQuery (toQueryList "member" <$> _lqQueueTypes),
        "maxResults" =: _lqMaxResults
      ]

-- | /See:/ 'listQueuesResponse' smart constructor.
data ListQueuesResponse = ListQueuesResponse'
  { _lqrsNextToken ::
      !(Maybe Text),
    _lqrsQueueSummaryList :: !(Maybe [QueueSummary]),
    _lqrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListQueuesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqrsNextToken' - If there are additional results, this is the token for the next set of results.
--
-- * 'lqrsQueueSummaryList' - Information about the queues.
--
-- * 'lqrsResponseStatus' - -- | The response status code.
listQueuesResponse ::
  -- | 'lqrsResponseStatus'
  Int ->
  ListQueuesResponse
listQueuesResponse pResponseStatus_ =
  ListQueuesResponse'
    { _lqrsNextToken = Nothing,
      _lqrsQueueSummaryList = Nothing,
      _lqrsResponseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
lqrsNextToken :: Lens' ListQueuesResponse (Maybe Text)
lqrsNextToken = lens _lqrsNextToken (\s a -> s {_lqrsNextToken = a})

-- | Information about the queues.
lqrsQueueSummaryList :: Lens' ListQueuesResponse [QueueSummary]
lqrsQueueSummaryList = lens _lqrsQueueSummaryList (\s a -> s {_lqrsQueueSummaryList = a}) . _Default . _Coerce

-- | -- | The response status code.
lqrsResponseStatus :: Lens' ListQueuesResponse Int
lqrsResponseStatus = lens _lqrsResponseStatus (\s a -> s {_lqrsResponseStatus = a})

instance NFData ListQueuesResponse
