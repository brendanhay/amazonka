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
-- Module      : Network.AWS.SQS.ListDeadLetterSourceQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of your queues that have the @RedrivePolicy@ queue attribute configured with a dead-letter queue.
--
--
-- The @ListDeadLetterSourceQueues@ methods supports pagination. Set parameter @MaxResults@ in the request to specify the maximum number of results to be returned in the response. If you do not set @MaxResults@ , the response includes a maximum of 1,000 results. If you set @MaxResults@ and there are additional results to display, the response includes a value for @NextToken@ . Use @NextToken@ as a parameter in your next request to @ListDeadLetterSourceQueues@ to receive the next page of results.
--
-- For more information about using dead-letter queues, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues> in the /Amazon Simple Queue Service Developer Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.SQS.ListDeadLetterSourceQueues
  ( -- * Creating a Request
    listDeadLetterSourceQueues,
    ListDeadLetterSourceQueues,

    -- * Request Lenses
    ldlsqNextToken,
    ldlsqMaxResults,
    ldlsqQueueURL,

    -- * Destructuring the Response
    listDeadLetterSourceQueuesResponse,
    ListDeadLetterSourceQueuesResponse,

    -- * Response Lenses
    ldlsqrsNextToken,
    ldlsqrsResponseStatus,
    ldlsqrsQueueURLs,
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
-- /See:/ 'listDeadLetterSourceQueues' smart constructor.
data ListDeadLetterSourceQueues = ListDeadLetterSourceQueues'
  { _ldlsqNextToken ::
      !(Maybe Text),
    _ldlsqMaxResults :: !(Maybe Int),
    _ldlsqQueueURL :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDeadLetterSourceQueues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldlsqNextToken' - Pagination token to request the next set of results.
--
-- * 'ldlsqMaxResults' - Maximum number of results to include in the response. Value range is 1 to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in the response.
--
-- * 'ldlsqQueueURL' - The URL of a dead-letter queue. Queue URLs and names are case-sensitive.
listDeadLetterSourceQueues ::
  -- | 'ldlsqQueueURL'
  Text ->
  ListDeadLetterSourceQueues
listDeadLetterSourceQueues pQueueURL_ =
  ListDeadLetterSourceQueues'
    { _ldlsqNextToken = Nothing,
      _ldlsqMaxResults = Nothing,
      _ldlsqQueueURL = pQueueURL_
    }

-- | Pagination token to request the next set of results.
ldlsqNextToken :: Lens' ListDeadLetterSourceQueues (Maybe Text)
ldlsqNextToken = lens _ldlsqNextToken (\s a -> s {_ldlsqNextToken = a})

-- | Maximum number of results to include in the response. Value range is 1 to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in the response.
ldlsqMaxResults :: Lens' ListDeadLetterSourceQueues (Maybe Int)
ldlsqMaxResults = lens _ldlsqMaxResults (\s a -> s {_ldlsqMaxResults = a})

-- | The URL of a dead-letter queue. Queue URLs and names are case-sensitive.
ldlsqQueueURL :: Lens' ListDeadLetterSourceQueues Text
ldlsqQueueURL = lens _ldlsqQueueURL (\s a -> s {_ldlsqQueueURL = a})

instance AWSPager ListDeadLetterSourceQueues where
  page rq rs
    | stop (rs ^. ldlsqrsNextToken) = Nothing
    | stop (rs ^. ldlsqrsQueueURLs) = Nothing
    | otherwise = Just $ rq & ldlsqNextToken .~ rs ^. ldlsqrsNextToken

instance AWSRequest ListDeadLetterSourceQueues where
  type
    Rs ListDeadLetterSourceQueues =
      ListDeadLetterSourceQueuesResponse
  request = postQuery sqs
  response =
    receiveXMLWrapper
      "ListDeadLetterSourceQueuesResult"
      ( \s h x ->
          ListDeadLetterSourceQueuesResponse'
            <$> (x .@? "NextToken")
            <*> (pure (fromEnum s))
            <*> (parseXMLList "QueueUrl" x)
      )

instance Hashable ListDeadLetterSourceQueues

instance NFData ListDeadLetterSourceQueues

instance ToHeaders ListDeadLetterSourceQueues where
  toHeaders = const mempty

instance ToPath ListDeadLetterSourceQueues where
  toPath = const "/"

instance ToQuery ListDeadLetterSourceQueues where
  toQuery ListDeadLetterSourceQueues' {..} =
    mconcat
      [ "Action" =: ("ListDeadLetterSourceQueues" :: ByteString),
        "Version" =: ("2012-11-05" :: ByteString),
        "NextToken" =: _ldlsqNextToken,
        "MaxResults" =: _ldlsqMaxResults,
        "QueueUrl" =: _ldlsqQueueURL
      ]

-- | A list of your dead letter source queues.
--
--
--
-- /See:/ 'listDeadLetterSourceQueuesResponse' smart constructor.
data ListDeadLetterSourceQueuesResponse = ListDeadLetterSourceQueuesResponse'
  { _ldlsqrsNextToken ::
      !(Maybe Text),
    _ldlsqrsResponseStatus ::
      !Int,
    _ldlsqrsQueueURLs ::
      ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDeadLetterSourceQueuesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldlsqrsNextToken' - Pagination token to include in the next request. Token value is @null@ if there are no additional results to request, or if you did not set @MaxResults@ in the request.
--
-- * 'ldlsqrsResponseStatus' - -- | The response status code.
--
-- * 'ldlsqrsQueueURLs' - A list of source queue URLs that have the @RedrivePolicy@ queue attribute configured with a dead-letter queue.
listDeadLetterSourceQueuesResponse ::
  -- | 'ldlsqrsResponseStatus'
  Int ->
  ListDeadLetterSourceQueuesResponse
listDeadLetterSourceQueuesResponse pResponseStatus_ =
  ListDeadLetterSourceQueuesResponse'
    { _ldlsqrsNextToken = Nothing,
      _ldlsqrsResponseStatus = pResponseStatus_,
      _ldlsqrsQueueURLs = mempty
    }

-- | Pagination token to include in the next request. Token value is @null@ if there are no additional results to request, or if you did not set @MaxResults@ in the request.
ldlsqrsNextToken :: Lens' ListDeadLetterSourceQueuesResponse (Maybe Text)
ldlsqrsNextToken = lens _ldlsqrsNextToken (\s a -> s {_ldlsqrsNextToken = a})

-- | -- | The response status code.
ldlsqrsResponseStatus :: Lens' ListDeadLetterSourceQueuesResponse Int
ldlsqrsResponseStatus = lens _ldlsqrsResponseStatus (\s a -> s {_ldlsqrsResponseStatus = a})

-- | A list of source queue URLs that have the @RedrivePolicy@ queue attribute configured with a dead-letter queue.
ldlsqrsQueueURLs :: Lens' ListDeadLetterSourceQueuesResponse [Text]
ldlsqrsQueueURLs = lens _ldlsqrsQueueURLs (\s a -> s {_ldlsqrsQueueURLs = a}) . _Coerce

instance NFData ListDeadLetterSourceQueuesResponse
