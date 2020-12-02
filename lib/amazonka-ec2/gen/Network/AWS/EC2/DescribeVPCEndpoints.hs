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
-- Module      : Network.AWS.EC2.DescribeVPCEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPC endpoints.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVPCEndpoints
  ( -- * Creating a Request
    describeVPCEndpoints,
    DescribeVPCEndpoints,

    -- * Request Lenses
    dvpceFilters,
    dvpceNextToken,
    dvpceVPCEndpointIds,
    dvpceDryRun,
    dvpceMaxResults,

    -- * Destructuring the Response
    describeVPCEndpointsResponse,
    DescribeVPCEndpointsResponse,

    -- * Response Lenses
    dvpcersNextToken,
    dvpcersVPCEndpoints,
    dvpcersResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeVpcEndpoints.
--
--
--
-- /See:/ 'describeVPCEndpoints' smart constructor.
data DescribeVPCEndpoints = DescribeVPCEndpoints'
  { _dvpceFilters ::
      !(Maybe [Filter]),
    _dvpceNextToken :: !(Maybe Text),
    _dvpceVPCEndpointIds :: !(Maybe [Text]),
    _dvpceDryRun :: !(Maybe Bool),
    _dvpceMaxResults :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeVPCEndpoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpceFilters' - One or more filters.     * @service-name@ - The name of the service.     * @vpc-id@ - The ID of the VPC in which the endpoint resides.     * @vpc-endpoint-id@ - The ID of the endpoint.     * @vpc-endpoint-state@ - The state of the endpoint (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ | @deleted@ | @rejected@ | @failed@ ).     * @vpc-endpoint-type@ - The type of VPC endpoint (@Interface@ | @Gateway@ | @GatewayLoadBalancer@ ).     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
-- * 'dvpceNextToken' - The token for the next set of items to return. (You received this token from a prior call.)
--
-- * 'dvpceVPCEndpointIds' - One or more endpoint IDs.
--
-- * 'dvpceDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvpceMaxResults' - The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results. Constraint: If the value is greater than 1,000, we return only 1,000 items.
describeVPCEndpoints ::
  DescribeVPCEndpoints
describeVPCEndpoints =
  DescribeVPCEndpoints'
    { _dvpceFilters = Nothing,
      _dvpceNextToken = Nothing,
      _dvpceVPCEndpointIds = Nothing,
      _dvpceDryRun = Nothing,
      _dvpceMaxResults = Nothing
    }

-- | One or more filters.     * @service-name@ - The name of the service.     * @vpc-id@ - The ID of the VPC in which the endpoint resides.     * @vpc-endpoint-id@ - The ID of the endpoint.     * @vpc-endpoint-state@ - The state of the endpoint (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ | @deleted@ | @rejected@ | @failed@ ).     * @vpc-endpoint-type@ - The type of VPC endpoint (@Interface@ | @Gateway@ | @GatewayLoadBalancer@ ).     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
dvpceFilters :: Lens' DescribeVPCEndpoints [Filter]
dvpceFilters = lens _dvpceFilters (\s a -> s {_dvpceFilters = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a prior call.)
dvpceNextToken :: Lens' DescribeVPCEndpoints (Maybe Text)
dvpceNextToken = lens _dvpceNextToken (\s a -> s {_dvpceNextToken = a})

-- | One or more endpoint IDs.
dvpceVPCEndpointIds :: Lens' DescribeVPCEndpoints [Text]
dvpceVPCEndpointIds = lens _dvpceVPCEndpointIds (\s a -> s {_dvpceVPCEndpointIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvpceDryRun :: Lens' DescribeVPCEndpoints (Maybe Bool)
dvpceDryRun = lens _dvpceDryRun (\s a -> s {_dvpceDryRun = a})

-- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results. Constraint: If the value is greater than 1,000, we return only 1,000 items.
dvpceMaxResults :: Lens' DescribeVPCEndpoints (Maybe Int)
dvpceMaxResults = lens _dvpceMaxResults (\s a -> s {_dvpceMaxResults = a})

instance AWSPager DescribeVPCEndpoints where
  page rq rs
    | stop (rs ^. dvpcersNextToken) = Nothing
    | stop (rs ^. dvpcersVPCEndpoints) = Nothing
    | otherwise = Just $ rq & dvpceNextToken .~ rs ^. dvpcersNextToken

instance AWSRequest DescribeVPCEndpoints where
  type Rs DescribeVPCEndpoints = DescribeVPCEndpointsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeVPCEndpointsResponse'
            <$> (x .@? "nextToken")
            <*> (x .@? "vpcEndpointSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeVPCEndpoints

instance NFData DescribeVPCEndpoints

instance ToHeaders DescribeVPCEndpoints where
  toHeaders = const mempty

instance ToPath DescribeVPCEndpoints where
  toPath = const "/"

instance ToQuery DescribeVPCEndpoints where
  toQuery DescribeVPCEndpoints' {..} =
    mconcat
      [ "Action" =: ("DescribeVpcEndpoints" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dvpceFilters),
        "NextToken" =: _dvpceNextToken,
        toQuery (toQueryList "VpcEndpointId" <$> _dvpceVPCEndpointIds),
        "DryRun" =: _dvpceDryRun,
        "MaxResults" =: _dvpceMaxResults
      ]

-- | Contains the output of DescribeVpcEndpoints.
--
--
--
-- /See:/ 'describeVPCEndpointsResponse' smart constructor.
data DescribeVPCEndpointsResponse = DescribeVPCEndpointsResponse'
  { _dvpcersNextToken ::
      !(Maybe Text),
    _dvpcersVPCEndpoints ::
      !(Maybe [VPCEndpoint]),
    _dvpcersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeVPCEndpointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcersNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dvpcersVPCEndpoints' - Information about the endpoints.
--
-- * 'dvpcersResponseStatus' - -- | The response status code.
describeVPCEndpointsResponse ::
  -- | 'dvpcersResponseStatus'
  Int ->
  DescribeVPCEndpointsResponse
describeVPCEndpointsResponse pResponseStatus_ =
  DescribeVPCEndpointsResponse'
    { _dvpcersNextToken = Nothing,
      _dvpcersVPCEndpoints = Nothing,
      _dvpcersResponseStatus = pResponseStatus_
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dvpcersNextToken :: Lens' DescribeVPCEndpointsResponse (Maybe Text)
dvpcersNextToken = lens _dvpcersNextToken (\s a -> s {_dvpcersNextToken = a})

-- | Information about the endpoints.
dvpcersVPCEndpoints :: Lens' DescribeVPCEndpointsResponse [VPCEndpoint]
dvpcersVPCEndpoints = lens _dvpcersVPCEndpoints (\s a -> s {_dvpcersVPCEndpoints = a}) . _Default . _Coerce

-- | -- | The response status code.
dvpcersResponseStatus :: Lens' DescribeVPCEndpointsResponse Int
dvpcersResponseStatus = lens _dvpcersResponseStatus (\s a -> s {_dvpcersResponseStatus = a})

instance NFData DescribeVPCEndpointsResponse
