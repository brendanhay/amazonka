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
-- Module      : Network.AWS.EC2.DescribeRouteTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your route tables.
--
--
-- Each subnet in your VPC must be associated with a route table. If a subnet is not explicitly associated with any route table, it is implicitly associated with the main route table. This command does not return the subnet ID for implicit associations.
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeRouteTables
  ( -- * Creating a Request
    describeRouteTables,
    DescribeRouteTables,

    -- * Request Lenses
    drtsFilters,
    drtsNextToken,
    drtsDryRun,
    drtsMaxResults,
    drtsRouteTableIds,

    -- * Destructuring the Response
    describeRouteTablesResponse,
    DescribeRouteTablesResponse,

    -- * Response Lenses
    drtrsNextToken,
    drtrsRouteTables,
    drtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRouteTables' smart constructor.
data DescribeRouteTables = DescribeRouteTables'
  { _drtsFilters ::
      !(Maybe [Filter]),
    _drtsNextToken :: !(Maybe Text),
    _drtsDryRun :: !(Maybe Bool),
    _drtsMaxResults :: !(Maybe Nat),
    _drtsRouteTableIds :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeRouteTables' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drtsFilters' - One or more filters.     * @association.route-table-association-id@ - The ID of an association ID for the route table.     * @association.route-table-id@ - The ID of the route table involved in the association.     * @association.subnet-id@ - The ID of the subnet involved in the association.     * @association.main@ - Indicates whether the route table is the main route table for the VPC (@true@ | @false@ ). Route tables that do not have an association ID are not returned in the response.     * @owner-id@ - The ID of the AWS account that owns the route table.     * @route-table-id@ - The ID of the route table.     * @route.destination-cidr-block@ - The IPv4 CIDR range specified in a route in the table.     * @route.destination-ipv6-cidr-block@ - The IPv6 CIDR range specified in a route in the route table.     * @route.destination-prefix-list-id@ - The ID (prefix) of the AWS service specified in a route in the table.     * @route.egress-only-internet-gateway-id@ - The ID of an egress-only Internet gateway specified in a route in the route table.     * @route.gateway-id@ - The ID of a gateway specified in a route in the table.     * @route.instance-id@ - The ID of an instance specified in a route in the table.     * @route.nat-gateway-id@ - The ID of a NAT gateway.     * @route.transit-gateway-id@ - The ID of a transit gateway.     * @route.origin@ - Describes how the route was created. @CreateRouteTable@ indicates that the route was automatically created when the route table was created; @CreateRoute@ indicates that the route was manually added to the route table; @EnableVgwRoutePropagation@ indicates that the route was propagated by route propagation.     * @route.state@ - The state of a route in the route table (@active@ | @blackhole@ ). The blackhole state indicates that the route's target isn't available (for example, the specified gateway isn't attached to the VPC, the specified NAT instance has been terminated, and so on).     * @route.vpc-peering-connection-id@ - The ID of a VPC peering connection specified in a route in the table.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @vpc-id@ - The ID of the VPC for the route table.
--
-- * 'drtsNextToken' - The token for the next page of results.
--
-- * 'drtsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'drtsMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- * 'drtsRouteTableIds' - One or more route table IDs. Default: Describes all your route tables.
describeRouteTables ::
  DescribeRouteTables
describeRouteTables =
  DescribeRouteTables'
    { _drtsFilters = Nothing,
      _drtsNextToken = Nothing,
      _drtsDryRun = Nothing,
      _drtsMaxResults = Nothing,
      _drtsRouteTableIds = Nothing
    }

-- | One or more filters.     * @association.route-table-association-id@ - The ID of an association ID for the route table.     * @association.route-table-id@ - The ID of the route table involved in the association.     * @association.subnet-id@ - The ID of the subnet involved in the association.     * @association.main@ - Indicates whether the route table is the main route table for the VPC (@true@ | @false@ ). Route tables that do not have an association ID are not returned in the response.     * @owner-id@ - The ID of the AWS account that owns the route table.     * @route-table-id@ - The ID of the route table.     * @route.destination-cidr-block@ - The IPv4 CIDR range specified in a route in the table.     * @route.destination-ipv6-cidr-block@ - The IPv6 CIDR range specified in a route in the route table.     * @route.destination-prefix-list-id@ - The ID (prefix) of the AWS service specified in a route in the table.     * @route.egress-only-internet-gateway-id@ - The ID of an egress-only Internet gateway specified in a route in the route table.     * @route.gateway-id@ - The ID of a gateway specified in a route in the table.     * @route.instance-id@ - The ID of an instance specified in a route in the table.     * @route.nat-gateway-id@ - The ID of a NAT gateway.     * @route.transit-gateway-id@ - The ID of a transit gateway.     * @route.origin@ - Describes how the route was created. @CreateRouteTable@ indicates that the route was automatically created when the route table was created; @CreateRoute@ indicates that the route was manually added to the route table; @EnableVgwRoutePropagation@ indicates that the route was propagated by route propagation.     * @route.state@ - The state of a route in the route table (@active@ | @blackhole@ ). The blackhole state indicates that the route's target isn't available (for example, the specified gateway isn't attached to the VPC, the specified NAT instance has been terminated, and so on).     * @route.vpc-peering-connection-id@ - The ID of a VPC peering connection specified in a route in the table.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @vpc-id@ - The ID of the VPC for the route table.
drtsFilters :: Lens' DescribeRouteTables [Filter]
drtsFilters = lens _drtsFilters (\s a -> s {_drtsFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
drtsNextToken :: Lens' DescribeRouteTables (Maybe Text)
drtsNextToken = lens _drtsNextToken (\s a -> s {_drtsNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
drtsDryRun :: Lens' DescribeRouteTables (Maybe Bool)
drtsDryRun = lens _drtsDryRun (\s a -> s {_drtsDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
drtsMaxResults :: Lens' DescribeRouteTables (Maybe Natural)
drtsMaxResults = lens _drtsMaxResults (\s a -> s {_drtsMaxResults = a}) . mapping _Nat

-- | One or more route table IDs. Default: Describes all your route tables.
drtsRouteTableIds :: Lens' DescribeRouteTables [Text]
drtsRouteTableIds = lens _drtsRouteTableIds (\s a -> s {_drtsRouteTableIds = a}) . _Default . _Coerce

instance AWSPager DescribeRouteTables where
  page rq rs
    | stop (rs ^. drtrsNextToken) = Nothing
    | stop (rs ^. drtrsRouteTables) = Nothing
    | otherwise = Just $ rq & drtsNextToken .~ rs ^. drtrsNextToken

instance AWSRequest DescribeRouteTables where
  type Rs DescribeRouteTables = DescribeRouteTablesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeRouteTablesResponse'
            <$> (x .@? "nextToken")
            <*> (x .@? "routeTableSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeRouteTables

instance NFData DescribeRouteTables

instance ToHeaders DescribeRouteTables where
  toHeaders = const mempty

instance ToPath DescribeRouteTables where
  toPath = const "/"

instance ToQuery DescribeRouteTables where
  toQuery DescribeRouteTables' {..} =
    mconcat
      [ "Action" =: ("DescribeRouteTables" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _drtsFilters),
        "NextToken" =: _drtsNextToken,
        "DryRun" =: _drtsDryRun,
        "MaxResults" =: _drtsMaxResults,
        toQuery (toQueryList "RouteTableId" <$> _drtsRouteTableIds)
      ]

-- | Contains the output of DescribeRouteTables.
--
--
--
-- /See:/ 'describeRouteTablesResponse' smart constructor.
data DescribeRouteTablesResponse = DescribeRouteTablesResponse'
  { _drtrsNextToken ::
      !(Maybe Text),
    _drtrsRouteTables ::
      !(Maybe [RouteTable]),
    _drtrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeRouteTablesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drtrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'drtrsRouteTables' - Information about one or more route tables.
--
-- * 'drtrsResponseStatus' - -- | The response status code.
describeRouteTablesResponse ::
  -- | 'drtrsResponseStatus'
  Int ->
  DescribeRouteTablesResponse
describeRouteTablesResponse pResponseStatus_ =
  DescribeRouteTablesResponse'
    { _drtrsNextToken = Nothing,
      _drtrsRouteTables = Nothing,
      _drtrsResponseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
drtrsNextToken :: Lens' DescribeRouteTablesResponse (Maybe Text)
drtrsNextToken = lens _drtrsNextToken (\s a -> s {_drtrsNextToken = a})

-- | Information about one or more route tables.
drtrsRouteTables :: Lens' DescribeRouteTablesResponse [RouteTable]
drtrsRouteTables = lens _drtrsRouteTables (\s a -> s {_drtrsRouteTables = a}) . _Default . _Coerce

-- | -- | The response status code.
drtrsResponseStatus :: Lens' DescribeRouteTablesResponse Int
drtrsResponseStatus = lens _drtrsResponseStatus (\s a -> s {_drtrsResponseStatus = a})

instance NFData DescribeRouteTablesResponse
