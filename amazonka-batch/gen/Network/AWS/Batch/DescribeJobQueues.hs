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
-- Module      : Network.AWS.Batch.DescribeJobQueues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your job queues.
--
--
module Network.AWS.Batch.DescribeJobQueues
    (
    -- * Creating a Request
      describeJobQueues
    , DescribeJobQueues
    -- * Request Lenses
    , djqNextToken
    , djqJobQueues
    , djqMaxResults

    -- * Destructuring the Response
    , describeJobQueuesResponse
    , DescribeJobQueuesResponse
    -- * Response Lenses
    , djqsrsNextToken
    , djqsrsJobQueues
    , djqsrsResponseStatus
    ) where

import Network.AWS.Batch.Types
import Network.AWS.Batch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeJobQueues' smart constructor.
data DescribeJobQueues = DescribeJobQueues'
  { _djqNextToken  :: !(Maybe Text)
  , _djqJobQueues  :: !(Maybe [Text])
  , _djqMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJobQueues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djqNextToken' - The @nextToken@ value returned from a previous paginated @DescribeJobQueues@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
--
-- * 'djqJobQueues' - A list of up to 100 queue names or full queue Amazon Resource Name (ARN) entries.
--
-- * 'djqMaxResults' - The maximum number of results returned by @DescribeJobQueues@ in paginated output. When this parameter is used, @DescribeJobQueues@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeJobQueues@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeJobQueues@ returns up to 100 results and a @nextToken@ value if applicable.
describeJobQueues
    :: DescribeJobQueues
describeJobQueues =
  DescribeJobQueues'
    {_djqNextToken = Nothing, _djqJobQueues = Nothing, _djqMaxResults = Nothing}


-- | The @nextToken@ value returned from a previous paginated @DescribeJobQueues@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
djqNextToken :: Lens' DescribeJobQueues (Maybe Text)
djqNextToken = lens _djqNextToken (\ s a -> s{_djqNextToken = a})

-- | A list of up to 100 queue names or full queue Amazon Resource Name (ARN) entries.
djqJobQueues :: Lens' DescribeJobQueues [Text]
djqJobQueues = lens _djqJobQueues (\ s a -> s{_djqJobQueues = a}) . _Default . _Coerce

-- | The maximum number of results returned by @DescribeJobQueues@ in paginated output. When this parameter is used, @DescribeJobQueues@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeJobQueues@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeJobQueues@ returns up to 100 results and a @nextToken@ value if applicable.
djqMaxResults :: Lens' DescribeJobQueues (Maybe Int)
djqMaxResults = lens _djqMaxResults (\ s a -> s{_djqMaxResults = a})

instance AWSRequest DescribeJobQueues where
        type Rs DescribeJobQueues = DescribeJobQueuesResponse
        request = postJSON batch
        response
          = receiveJSON
              (\ s h x ->
                 DescribeJobQueuesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "jobQueues" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeJobQueues where

instance NFData DescribeJobQueues where

instance ToHeaders DescribeJobQueues where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeJobQueues where
        toJSON DescribeJobQueues'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _djqNextToken,
                  ("jobQueues" .=) <$> _djqJobQueues,
                  ("maxResults" .=) <$> _djqMaxResults])

instance ToPath DescribeJobQueues where
        toPath = const "/v1/describejobqueues"

instance ToQuery DescribeJobQueues where
        toQuery = const mempty

-- | /See:/ 'describeJobQueuesResponse' smart constructor.
data DescribeJobQueuesResponse = DescribeJobQueuesResponse'
  { _djqsrsNextToken      :: !(Maybe Text)
  , _djqsrsJobQueues      :: !(Maybe [JobQueueDetail])
  , _djqsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJobQueuesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djqsrsNextToken' - The @nextToken@ value to include in a future @DescribeJobQueues@ request. When the results of a @DescribeJobQueues@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'djqsrsJobQueues' - The list of job queues.
--
-- * 'djqsrsResponseStatus' - -- | The response status code.
describeJobQueuesResponse
    :: Int -- ^ 'djqsrsResponseStatus'
    -> DescribeJobQueuesResponse
describeJobQueuesResponse pResponseStatus_ =
  DescribeJobQueuesResponse'
    { _djqsrsNextToken = Nothing
    , _djqsrsJobQueues = Nothing
    , _djqsrsResponseStatus = pResponseStatus_
    }


-- | The @nextToken@ value to include in a future @DescribeJobQueues@ request. When the results of a @DescribeJobQueues@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
djqsrsNextToken :: Lens' DescribeJobQueuesResponse (Maybe Text)
djqsrsNextToken = lens _djqsrsNextToken (\ s a -> s{_djqsrsNextToken = a})

-- | The list of job queues.
djqsrsJobQueues :: Lens' DescribeJobQueuesResponse [JobQueueDetail]
djqsrsJobQueues = lens _djqsrsJobQueues (\ s a -> s{_djqsrsJobQueues = a}) . _Default . _Coerce

-- | -- | The response status code.
djqsrsResponseStatus :: Lens' DescribeJobQueuesResponse Int
djqsrsResponseStatus = lens _djqsrsResponseStatus (\ s a -> s{_djqsrsResponseStatus = a})

instance NFData DescribeJobQueuesResponse where
