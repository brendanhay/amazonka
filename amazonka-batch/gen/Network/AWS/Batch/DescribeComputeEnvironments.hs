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
-- Module      : Network.AWS.Batch.DescribeComputeEnvironments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your compute environments.
--
--
-- If you are using an unmanaged compute environment, you can use the @DescribeComputeEnvironment@ operation to determine the @ecsClusterArn@ that you should launch your Amazon ECS container instances into.
--
module Network.AWS.Batch.DescribeComputeEnvironments
    (
    -- * Creating a Request
      describeComputeEnvironments
    , DescribeComputeEnvironments
    -- * Request Lenses
    , dceComputeEnvironments
    , dceNextToken
    , dceMaxResults

    -- * Destructuring the Response
    , describeComputeEnvironmentsResponse
    , DescribeComputeEnvironmentsResponse
    -- * Response Lenses
    , drsComputeEnvironments
    , drsNextToken
    , drsResponseStatus
    ) where

import Network.AWS.Batch.Types
import Network.AWS.Batch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeComputeEnvironments' smart constructor.
data DescribeComputeEnvironments = DescribeComputeEnvironments'
  { _dceComputeEnvironments :: !(Maybe [Text])
  , _dceNextToken           :: !(Maybe Text)
  , _dceMaxResults          :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeComputeEnvironments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dceComputeEnvironments' - A list of up to 100 compute environment names or full Amazon Resource Name (ARN) entries.
--
-- * 'dceNextToken' - The @nextToken@ value returned from a previous paginated @DescribeComputeEnvironments@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
--
-- * 'dceMaxResults' - The maximum number of cluster results returned by @DescribeComputeEnvironments@ in paginated output. When this parameter is used, @DescribeComputeEnvironments@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeComputeEnvironments@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeComputeEnvironments@ returns up to 100 results and a @nextToken@ value if applicable.
describeComputeEnvironments
    :: DescribeComputeEnvironments
describeComputeEnvironments =
  DescribeComputeEnvironments'
    { _dceComputeEnvironments = Nothing
    , _dceNextToken = Nothing
    , _dceMaxResults = Nothing
    }


-- | A list of up to 100 compute environment names or full Amazon Resource Name (ARN) entries.
dceComputeEnvironments :: Lens' DescribeComputeEnvironments [Text]
dceComputeEnvironments = lens _dceComputeEnvironments (\ s a -> s{_dceComputeEnvironments = a}) . _Default . _Coerce

-- | The @nextToken@ value returned from a previous paginated @DescribeComputeEnvironments@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
dceNextToken :: Lens' DescribeComputeEnvironments (Maybe Text)
dceNextToken = lens _dceNextToken (\ s a -> s{_dceNextToken = a})

-- | The maximum number of cluster results returned by @DescribeComputeEnvironments@ in paginated output. When this parameter is used, @DescribeComputeEnvironments@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeComputeEnvironments@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeComputeEnvironments@ returns up to 100 results and a @nextToken@ value if applicable.
dceMaxResults :: Lens' DescribeComputeEnvironments (Maybe Int)
dceMaxResults = lens _dceMaxResults (\ s a -> s{_dceMaxResults = a})

instance AWSRequest DescribeComputeEnvironments where
        type Rs DescribeComputeEnvironments =
             DescribeComputeEnvironmentsResponse
        request = postJSON batch
        response
          = receiveJSON
              (\ s h x ->
                 DescribeComputeEnvironmentsResponse' <$>
                   (x .?> "computeEnvironments" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeComputeEnvironments where

instance NFData DescribeComputeEnvironments where

instance ToHeaders DescribeComputeEnvironments where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeComputeEnvironments where
        toJSON DescribeComputeEnvironments'{..}
          = object
              (catMaybes
                 [("computeEnvironments" .=) <$>
                    _dceComputeEnvironments,
                  ("nextToken" .=) <$> _dceNextToken,
                  ("maxResults" .=) <$> _dceMaxResults])

instance ToPath DescribeComputeEnvironments where
        toPath = const "/v1/describecomputeenvironments"

instance ToQuery DescribeComputeEnvironments where
        toQuery = const mempty

-- | /See:/ 'describeComputeEnvironmentsResponse' smart constructor.
data DescribeComputeEnvironmentsResponse = DescribeComputeEnvironmentsResponse'
  { _drsComputeEnvironments :: !(Maybe [ComputeEnvironmentDetail])
  , _drsNextToken           :: !(Maybe Text)
  , _drsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeComputeEnvironmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsComputeEnvironments' - The list of compute environments.
--
-- * 'drsNextToken' - The @nextToken@ value to include in a future @DescribeComputeEnvironments@ request. When the results of a @DescribeJobDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeComputeEnvironmentsResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeComputeEnvironmentsResponse
describeComputeEnvironmentsResponse pResponseStatus_ =
  DescribeComputeEnvironmentsResponse'
    { _drsComputeEnvironments = Nothing
    , _drsNextToken = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | The list of compute environments.
drsComputeEnvironments :: Lens' DescribeComputeEnvironmentsResponse [ComputeEnvironmentDetail]
drsComputeEnvironments = lens _drsComputeEnvironments (\ s a -> s{_drsComputeEnvironments = a}) . _Default . _Coerce

-- | The @nextToken@ value to include in a future @DescribeComputeEnvironments@ request. When the results of a @DescribeJobDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
drsNextToken :: Lens' DescribeComputeEnvironmentsResponse (Maybe Text)
drsNextToken = lens _drsNextToken (\ s a -> s{_drsNextToken = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeComputeEnvironmentsResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeComputeEnvironmentsResponse
         where
