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
-- Module      : Network.AWS.SQS.ListQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of your queues in the current region. The response includes a maximum of 1,000 results. If you specify a value for the optional @QueueNamePrefix@ parameter, only queues with a name that begins with the specified value are returned.
--
--
-- The @listQueues@ methods supports pagination. Set parameter @MaxResults@ in the request to specify the maximum number of results to be returned in the response. If you do not set @MaxResults@ , the response includes a maximum of 1,000 results. If you set @MaxResults@ and there are additional results to display, the response includes a value for @NextToken@ . Use @NextToken@ as a parameter in your next request to @listQueues@ to receive the next page of results.
--
--
-- This operation returns paginated results.
module Network.AWS.SQS.ListQueues
  ( -- * Creating a Request
    listQueues,
    ListQueues,

    -- * Request Lenses
    lqQueueNamePrefix,
    lqNextToken,
    lqMaxResults,

    -- * Destructuring the Response
    listQueuesResponse,
    ListQueuesResponse,

    -- * Response Lenses
    lqrsQueueURLs,
    lqrsNextToken,
    lqrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types

-- |
--
--
--
-- /See:/ 'listQueues' smart constructor.
data ListQueues = ListQueues'
  { _lqQueueNamePrefix :: !(Maybe Text),
    _lqNextToken :: !(Maybe Text),
    _lqMaxResults :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListQueues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqQueueNamePrefix' - A string to use for filtering the list results. Only those queues whose name begins with the specified string are returned. Queue URLs and names are case-sensitive.
--
-- * 'lqNextToken' - Pagination token to request the next set of results.
--
-- * 'lqMaxResults' - Maximum number of results to include in the response. Value range is 1 to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in the response.
listQueues ::
  ListQueues
listQueues =
  ListQueues'
    { _lqQueueNamePrefix = Nothing,
      _lqNextToken = Nothing,
      _lqMaxResults = Nothing
    }

-- | A string to use for filtering the list results. Only those queues whose name begins with the specified string are returned. Queue URLs and names are case-sensitive.
lqQueueNamePrefix :: Lens' ListQueues (Maybe Text)
lqQueueNamePrefix = lens _lqQueueNamePrefix (\s a -> s {_lqQueueNamePrefix = a})

-- | Pagination token to request the next set of results.
lqNextToken :: Lens' ListQueues (Maybe Text)
lqNextToken = lens _lqNextToken (\s a -> s {_lqNextToken = a})

-- | Maximum number of results to include in the response. Value range is 1 to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in the response.
lqMaxResults :: Lens' ListQueues (Maybe Int)
lqMaxResults = lens _lqMaxResults (\s a -> s {_lqMaxResults = a})

instance AWSPager ListQueues where
  page rq rs
    | stop (rs ^. lqrsNextToken) = Nothing
    | stop (rs ^. lqrsQueueURLs) = Nothing
    | otherwise = Just $ rq & lqNextToken .~ rs ^. lqrsNextToken

instance AWSRequest ListQueues where
  type Rs ListQueues = ListQueuesResponse
  request = postQuery sqs
  response =
    receiveXMLWrapper
      "ListQueuesResult"
      ( \s h x ->
          ListQueuesResponse'
            <$> (may (parseXMLList "QueueUrl") x)
            <*> (x .@? "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListQueues

instance NFData ListQueues

instance ToHeaders ListQueues where
  toHeaders = const mempty

instance ToPath ListQueues where
  toPath = const "/"

instance ToQuery ListQueues where
  toQuery ListQueues' {..} =
    mconcat
      [ "Action" =: ("ListQueues" :: ByteString),
        "Version" =: ("2012-11-05" :: ByteString),
        "QueueNamePrefix" =: _lqQueueNamePrefix,
        "NextToken" =: _lqNextToken,
        "MaxResults" =: _lqMaxResults
      ]

-- | A list of your queues.
--
--
--
-- /See:/ 'listQueuesResponse' smart constructor.
data ListQueuesResponse = ListQueuesResponse'
  { _lqrsQueueURLs ::
      !(Maybe [Text]),
    _lqrsNextToken :: !(Maybe Text),
    _lqrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListQueuesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqrsQueueURLs' - A list of queue URLs, up to 1,000 entries, or the value of MaxResults that you sent in the request.
--
-- * 'lqrsNextToken' - Pagination token to include in the next request. Token value is @null@ if there are no additional results to request, or if you did not set @MaxResults@ in the request.
--
-- * 'lqrsResponseStatus' - -- | The response status code.
listQueuesResponse ::
  -- | 'lqrsResponseStatus'
  Int ->
  ListQueuesResponse
listQueuesResponse pResponseStatus_ =
  ListQueuesResponse'
    { _lqrsQueueURLs = Nothing,
      _lqrsNextToken = Nothing,
      _lqrsResponseStatus = pResponseStatus_
    }

-- | A list of queue URLs, up to 1,000 entries, or the value of MaxResults that you sent in the request.
lqrsQueueURLs :: Lens' ListQueuesResponse [Text]
lqrsQueueURLs = lens _lqrsQueueURLs (\s a -> s {_lqrsQueueURLs = a}) . _Default . _Coerce

-- | Pagination token to include in the next request. Token value is @null@ if there are no additional results to request, or if you did not set @MaxResults@ in the request.
lqrsNextToken :: Lens' ListQueuesResponse (Maybe Text)
lqrsNextToken = lens _lqrsNextToken (\s a -> s {_lqrsNextToken = a})

-- | -- | The response status code.
lqrsResponseStatus :: Lens' ListQueuesResponse Int
lqrsResponseStatus = lens _lqrsResponseStatus (\s a -> s {_lqrsResponseStatus = a})

instance NFData ListQueuesResponse
