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
-- Module      : Network.AWS.EC2.DescribeElasticGpus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Elastic GPUs associated with your instances. For more information about Elastic GPUs, see <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-gpus.html Amazon EC2 Elastic GPUs> .
--
--
module Network.AWS.EC2.DescribeElasticGpus
    (
    -- * Creating a Request
      describeElasticGpus
    , DescribeElasticGpus
    -- * Request Lenses
    , degFilters
    , degNextToken
    , degDryRun
    , degMaxResults
    , degElasticGpuIds

    -- * Destructuring the Response
    , describeElasticGpusResponse
    , DescribeElasticGpusResponse
    -- * Response Lenses
    , degrsElasticGpuSet
    , degrsNextToken
    , degrsMaxResults
    , degrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeElasticGpus' smart constructor.
data DescribeElasticGpus = DescribeElasticGpus'
  { _degFilters       :: !(Maybe [Filter])
  , _degNextToken     :: !(Maybe Text)
  , _degDryRun        :: !(Maybe Bool)
  , _degMaxResults    :: !(Maybe Int)
  , _degElasticGpuIds :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeElasticGpus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'degFilters' - One or more filters.     * @availability-zone@ - The Availability Zone in which the Elastic GPU resides.     * @elastic-gpu-health@ - The status of the Elastic GPU (@OK@ | @IMPAIRED@ ).     * @elastic-gpu-state@ - The state of the Elastic GPU (@ATTACHED@ ).     * @elastic-gpu-type@ - The type of Elastic GPU; for example, @eg1.medium@ .     * @instance-id@ - The ID of the instance to which the Elastic GPU is associated.
--
-- * 'degNextToken' - The token to request the next page of results.
--
-- * 'degDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'degMaxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000.
--
-- * 'degElasticGpuIds' - One or more Elastic GPU IDs.
describeElasticGpus
    :: DescribeElasticGpus
describeElasticGpus =
  DescribeElasticGpus'
    { _degFilters = Nothing
    , _degNextToken = Nothing
    , _degDryRun = Nothing
    , _degMaxResults = Nothing
    , _degElasticGpuIds = Nothing
    }


-- | One or more filters.     * @availability-zone@ - The Availability Zone in which the Elastic GPU resides.     * @elastic-gpu-health@ - The status of the Elastic GPU (@OK@ | @IMPAIRED@ ).     * @elastic-gpu-state@ - The state of the Elastic GPU (@ATTACHED@ ).     * @elastic-gpu-type@ - The type of Elastic GPU; for example, @eg1.medium@ .     * @instance-id@ - The ID of the instance to which the Elastic GPU is associated.
degFilters :: Lens' DescribeElasticGpus [Filter]
degFilters = lens _degFilters (\ s a -> s{_degFilters = a}) . _Default . _Coerce

-- | The token to request the next page of results.
degNextToken :: Lens' DescribeElasticGpus (Maybe Text)
degNextToken = lens _degNextToken (\ s a -> s{_degNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
degDryRun :: Lens' DescribeElasticGpus (Maybe Bool)
degDryRun = lens _degDryRun (\ s a -> s{_degDryRun = a})

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000.
degMaxResults :: Lens' DescribeElasticGpus (Maybe Int)
degMaxResults = lens _degMaxResults (\ s a -> s{_degMaxResults = a})

-- | One or more Elastic GPU IDs.
degElasticGpuIds :: Lens' DescribeElasticGpus [Text]
degElasticGpuIds = lens _degElasticGpuIds (\ s a -> s{_degElasticGpuIds = a}) . _Default . _Coerce

instance AWSRequest DescribeElasticGpus where
        type Rs DescribeElasticGpus =
             DescribeElasticGpusResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeElasticGpusResponse' <$>
                   (x .@? "elasticGpuSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (x .@? "maxResults")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeElasticGpus where

instance NFData DescribeElasticGpus where

instance ToHeaders DescribeElasticGpus where
        toHeaders = const mempty

instance ToPath DescribeElasticGpus where
        toPath = const "/"

instance ToQuery DescribeElasticGpus where
        toQuery DescribeElasticGpus'{..}
          = mconcat
              ["Action" =: ("DescribeElasticGpus" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _degFilters),
               "NextToken" =: _degNextToken, "DryRun" =: _degDryRun,
               "MaxResults" =: _degMaxResults,
               toQuery
                 (toQueryList "ElasticGpuId" <$> _degElasticGpuIds)]

-- | /See:/ 'describeElasticGpusResponse' smart constructor.
data DescribeElasticGpusResponse = DescribeElasticGpusResponse'
  { _degrsElasticGpuSet  :: !(Maybe [ElasticGpus])
  , _degrsNextToken      :: !(Maybe Text)
  , _degrsMaxResults     :: !(Maybe Int)
  , _degrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeElasticGpusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'degrsElasticGpuSet' - Information about the Elastic GPUs.
--
-- * 'degrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'degrsMaxResults' - The total number of items to return. If the total number of items available is more than the value specified in max-items then a Next-Token will be provided in the output that you can use to resume pagination.
--
-- * 'degrsResponseStatus' - -- | The response status code.
describeElasticGpusResponse
    :: Int -- ^ 'degrsResponseStatus'
    -> DescribeElasticGpusResponse
describeElasticGpusResponse pResponseStatus_ =
  DescribeElasticGpusResponse'
    { _degrsElasticGpuSet = Nothing
    , _degrsNextToken = Nothing
    , _degrsMaxResults = Nothing
    , _degrsResponseStatus = pResponseStatus_
    }


-- | Information about the Elastic GPUs.
degrsElasticGpuSet :: Lens' DescribeElasticGpusResponse [ElasticGpus]
degrsElasticGpuSet = lens _degrsElasticGpuSet (\ s a -> s{_degrsElasticGpuSet = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
degrsNextToken :: Lens' DescribeElasticGpusResponse (Maybe Text)
degrsNextToken = lens _degrsNextToken (\ s a -> s{_degrsNextToken = a})

-- | The total number of items to return. If the total number of items available is more than the value specified in max-items then a Next-Token will be provided in the output that you can use to resume pagination.
degrsMaxResults :: Lens' DescribeElasticGpusResponse (Maybe Int)
degrsMaxResults = lens _degrsMaxResults (\ s a -> s{_degrsMaxResults = a})

-- | -- | The response status code.
degrsResponseStatus :: Lens' DescribeElasticGpusResponse Int
degrsResponseStatus = lens _degrsResponseStatus (\ s a -> s{_degrsResponseStatus = a})

instance NFData DescribeElasticGpusResponse where
