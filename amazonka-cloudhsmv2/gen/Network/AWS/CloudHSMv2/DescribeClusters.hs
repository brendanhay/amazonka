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
-- Module      : Network.AWS.CloudHSMv2.DescribeClusters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about AWS CloudHSM clusters.
--
--
-- This is a paginated operation, which means that each response might contain only a subset of all the clusters. When the response contains only a subset of clusters, it includes a @NextToken@ value. Use this value in a subsequent @DescribeClusters@ request to get more clusters. When you receive a response with no @NextToken@ (or an empty or null value), that means there are no more clusters to get.
--
--
-- This operation returns paginated results.
module Network.AWS.CloudHSMv2.DescribeClusters
    (
    -- * Creating a Request
      describeClusters
    , DescribeClusters
    -- * Request Lenses
    , dcFilters
    , dcNextToken
    , dcMaxResults

    -- * Destructuring the Response
    , describeClustersResponse
    , DescribeClustersResponse
    -- * Response Lenses
    , dcrsNextToken
    , dcrsClusters
    , dcrsResponseStatus
    ) where

import Network.AWS.CloudHSMv2.Types
import Network.AWS.CloudHSMv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { _dcFilters    :: !(Maybe (Map Text [Text]))
  , _dcNextToken  :: !(Maybe Text)
  , _dcMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClusters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcFilters' - One or more filters to limit the items returned in the response. Use the @clusterIds@ filter to return only the specified clusters. Specify clusters by their cluster identifier (ID). Use the @vpcIds@ filter to return only the clusters in the specified virtual private clouds (VPCs). Specify VPCs by their VPC identifier (ID). Use the @states@ filter to return only clusters that match the specified state.
--
-- * 'dcNextToken' - The @NextToken@ value that you received in the previous response. Use this value to get more clusters.
--
-- * 'dcMaxResults' - The maximum number of clusters to return in the response. When there are more clusters than the number you specify, the response contains a @NextToken@ value.
describeClusters
    :: DescribeClusters
describeClusters =
  DescribeClusters'
    {_dcFilters = Nothing, _dcNextToken = Nothing, _dcMaxResults = Nothing}


-- | One or more filters to limit the items returned in the response. Use the @clusterIds@ filter to return only the specified clusters. Specify clusters by their cluster identifier (ID). Use the @vpcIds@ filter to return only the clusters in the specified virtual private clouds (VPCs). Specify VPCs by their VPC identifier (ID). Use the @states@ filter to return only clusters that match the specified state.
dcFilters :: Lens' DescribeClusters (HashMap Text [Text])
dcFilters = lens _dcFilters (\ s a -> s{_dcFilters = a}) . _Default . _Map

-- | The @NextToken@ value that you received in the previous response. Use this value to get more clusters.
dcNextToken :: Lens' DescribeClusters (Maybe Text)
dcNextToken = lens _dcNextToken (\ s a -> s{_dcNextToken = a})

-- | The maximum number of clusters to return in the response. When there are more clusters than the number you specify, the response contains a @NextToken@ value.
dcMaxResults :: Lens' DescribeClusters (Maybe Natural)
dcMaxResults = lens _dcMaxResults (\ s a -> s{_dcMaxResults = a}) . mapping _Nat

instance AWSPager DescribeClusters where
        page rq rs
          | stop (rs ^. dcrsNextToken) = Nothing
          | stop (rs ^. dcrsClusters) = Nothing
          | otherwise =
            Just $ rq & dcNextToken .~ rs ^. dcrsNextToken

instance AWSRequest DescribeClusters where
        type Rs DescribeClusters = DescribeClustersResponse
        request = postJSON cloudHSMv2
        response
          = receiveJSON
              (\ s h x ->
                 DescribeClustersResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Clusters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeClusters where

instance NFData DescribeClusters where

instance ToHeaders DescribeClusters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("BaldrApiService.DescribeClusters" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeClusters where
        toJSON DescribeClusters'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dcFilters,
                  ("NextToken" .=) <$> _dcNextToken,
                  ("MaxResults" .=) <$> _dcMaxResults])

instance ToPath DescribeClusters where
        toPath = const "/"

instance ToQuery DescribeClusters where
        toQuery = const mempty

-- | /See:/ 'describeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { _dcrsNextToken      :: !(Maybe Text)
  , _dcrsClusters       :: !(Maybe [Cluster])
  , _dcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClustersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsNextToken' - An opaque string that indicates that the response contains only a subset of clusters. Use this value in a subsequent @DescribeClusters@ request to get more clusters.
--
-- * 'dcrsClusters' - A list of clusters.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeClustersResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DescribeClustersResponse
describeClustersResponse pResponseStatus_ =
  DescribeClustersResponse'
    { _dcrsNextToken = Nothing
    , _dcrsClusters = Nothing
    , _dcrsResponseStatus = pResponseStatus_
    }


-- | An opaque string that indicates that the response contains only a subset of clusters. Use this value in a subsequent @DescribeClusters@ request to get more clusters.
dcrsNextToken :: Lens' DescribeClustersResponse (Maybe Text)
dcrsNextToken = lens _dcrsNextToken (\ s a -> s{_dcrsNextToken = a})

-- | A list of clusters.
dcrsClusters :: Lens' DescribeClustersResponse [Cluster]
dcrsClusters = lens _dcrsClusters (\ s a -> s{_dcrsClusters = a}) . _Default . _Coerce

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeClustersResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DescribeClustersResponse where
