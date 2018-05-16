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
-- Module      : Network.AWS.EC2.DescribeVPCEndpointConnections
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the VPC endpoint connections to your VPC endpoint services, including any endpoints that are pending your acceptance.
--
--
module Network.AWS.EC2.DescribeVPCEndpointConnections
    (
    -- * Creating a Request
      describeVPCEndpointConnections
    , DescribeVPCEndpointConnections
    -- * Request Lenses
    , dvecFilters
    , dvecNextToken
    , dvecDryRun
    , dvecMaxResults

    -- * Destructuring the Response
    , describeVPCEndpointConnectionsResponse
    , DescribeVPCEndpointConnectionsResponse
    -- * Response Lenses
    , dvecrsVPCEndpointConnections
    , dvecrsNextToken
    , dvecrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVPCEndpointConnections' smart constructor.
data DescribeVPCEndpointConnections = DescribeVPCEndpointConnections'
  { _dvecFilters    :: !(Maybe [Filter])
  , _dvecNextToken  :: !(Maybe Text)
  , _dvecDryRun     :: !(Maybe Bool)
  , _dvecMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCEndpointConnections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvecFilters' - One or more filters.     * @service-id@ - The ID of the service.     * @vpc-endpoint-owner@ - The AWS account number of the owner of the endpoint.     * @vpc-endpoint-state@ - The state of the endpoint (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ | @deleted@ | @rejected@ | @failed@ ).     * @vpc-endpoint-id@ - The ID of the endpoint.
--
-- * 'dvecNextToken' - The token to retrieve the next page of results.
--
-- * 'dvecDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvecMaxResults' - The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned.
describeVPCEndpointConnections
    :: DescribeVPCEndpointConnections
describeVPCEndpointConnections =
  DescribeVPCEndpointConnections'
    { _dvecFilters = Nothing
    , _dvecNextToken = Nothing
    , _dvecDryRun = Nothing
    , _dvecMaxResults = Nothing
    }


-- | One or more filters.     * @service-id@ - The ID of the service.     * @vpc-endpoint-owner@ - The AWS account number of the owner of the endpoint.     * @vpc-endpoint-state@ - The state of the endpoint (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ | @deleted@ | @rejected@ | @failed@ ).     * @vpc-endpoint-id@ - The ID of the endpoint.
dvecFilters :: Lens' DescribeVPCEndpointConnections [Filter]
dvecFilters = lens _dvecFilters (\ s a -> s{_dvecFilters = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
dvecNextToken :: Lens' DescribeVPCEndpointConnections (Maybe Text)
dvecNextToken = lens _dvecNextToken (\ s a -> s{_dvecNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvecDryRun :: Lens' DescribeVPCEndpointConnections (Maybe Bool)
dvecDryRun = lens _dvecDryRun (\ s a -> s{_dvecDryRun = a})

-- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned.
dvecMaxResults :: Lens' DescribeVPCEndpointConnections (Maybe Int)
dvecMaxResults = lens _dvecMaxResults (\ s a -> s{_dvecMaxResults = a})

instance AWSRequest DescribeVPCEndpointConnections
         where
        type Rs DescribeVPCEndpointConnections =
             DescribeVPCEndpointConnectionsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCEndpointConnectionsResponse' <$>
                   (x .@? "vpcEndpointConnectionSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeVPCEndpointConnections
         where

instance NFData DescribeVPCEndpointConnections where

instance ToHeaders DescribeVPCEndpointConnections
         where
        toHeaders = const mempty

instance ToPath DescribeVPCEndpointConnections where
        toPath = const "/"

instance ToQuery DescribeVPCEndpointConnections where
        toQuery DescribeVPCEndpointConnections'{..}
          = mconcat
              ["Action" =:
                 ("DescribeVpcEndpointConnections" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvecFilters),
               "NextToken" =: _dvecNextToken,
               "DryRun" =: _dvecDryRun,
               "MaxResults" =: _dvecMaxResults]

-- | /See:/ 'describeVPCEndpointConnectionsResponse' smart constructor.
data DescribeVPCEndpointConnectionsResponse = DescribeVPCEndpointConnectionsResponse'
  { _dvecrsVPCEndpointConnections :: !(Maybe [VPCEndpointConnection])
  , _dvecrsNextToken              :: !(Maybe Text)
  , _dvecrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCEndpointConnectionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvecrsVPCEndpointConnections' - Information about one or more VPC endpoint connections.
--
-- * 'dvecrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dvecrsResponseStatus' - -- | The response status code.
describeVPCEndpointConnectionsResponse
    :: Int -- ^ 'dvecrsResponseStatus'
    -> DescribeVPCEndpointConnectionsResponse
describeVPCEndpointConnectionsResponse pResponseStatus_ =
  DescribeVPCEndpointConnectionsResponse'
    { _dvecrsVPCEndpointConnections = Nothing
    , _dvecrsNextToken = Nothing
    , _dvecrsResponseStatus = pResponseStatus_
    }


-- | Information about one or more VPC endpoint connections.
dvecrsVPCEndpointConnections :: Lens' DescribeVPCEndpointConnectionsResponse [VPCEndpointConnection]
dvecrsVPCEndpointConnections = lens _dvecrsVPCEndpointConnections (\ s a -> s{_dvecrsVPCEndpointConnections = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dvecrsNextToken :: Lens' DescribeVPCEndpointConnectionsResponse (Maybe Text)
dvecrsNextToken = lens _dvecrsNextToken (\ s a -> s{_dvecrsNextToken = a})

-- | -- | The response status code.
dvecrsResponseStatus :: Lens' DescribeVPCEndpointConnectionsResponse Int
dvecrsResponseStatus = lens _dvecrsResponseStatus (\ s a -> s{_dvecrsResponseStatus = a})

instance NFData
           DescribeVPCEndpointConnectionsResponse
         where
