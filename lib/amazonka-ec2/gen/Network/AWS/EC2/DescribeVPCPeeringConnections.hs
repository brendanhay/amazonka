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
-- Module      : Network.AWS.EC2.DescribeVPCPeeringConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPC peering connections.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVPCPeeringConnections
  ( -- * Creating a Request
    describeVPCPeeringConnections,
    DescribeVPCPeeringConnections,

    -- * Request Lenses
    dvpcpcFilters,
    dvpcpcNextToken,
    dvpcpcVPCPeeringConnectionIds,
    dvpcpcDryRun,
    dvpcpcMaxResults,

    -- * Destructuring the Response
    describeVPCPeeringConnectionsResponse,
    DescribeVPCPeeringConnectionsResponse,

    -- * Response Lenses
    dvpcpcrsNextToken,
    dvpcpcrsVPCPeeringConnections,
    dvpcpcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVPCPeeringConnections' smart constructor.
data DescribeVPCPeeringConnections = DescribeVPCPeeringConnections'
  { _dvpcpcFilters ::
      !(Maybe [Filter]),
    _dvpcpcNextToken ::
      !(Maybe Text),
    _dvpcpcVPCPeeringConnectionIds ::
      !(Maybe [Text]),
    _dvpcpcDryRun :: !(Maybe Bool),
    _dvpcpcMaxResults ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeVPCPeeringConnections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcpcFilters' - One or more filters.     * @accepter-vpc-info.cidr-block@ - The IPv4 CIDR block of the accepter VPC.     * @accepter-vpc-info.owner-id@ - The AWS account ID of the owner of the accepter VPC.     * @accepter-vpc-info.vpc-id@ - The ID of the accepter VPC.     * @expiration-time@ - The expiration date and time for the VPC peering connection.     * @requester-vpc-info.cidr-block@ - The IPv4 CIDR block of the requester's VPC.     * @requester-vpc-info.owner-id@ - The AWS account ID of the owner of the requester VPC.     * @requester-vpc-info.vpc-id@ - The ID of the requester VPC.     * @status-code@ - The status of the VPC peering connection (@pending-acceptance@ | @failed@ | @expired@ | @provisioning@ | @active@ | @deleting@ | @deleted@ | @rejected@ ).     * @status-message@ - A message that provides more information about the status of the VPC peering connection, if applicable.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @vpc-peering-connection-id@ - The ID of the VPC peering connection.
--
-- * 'dvpcpcNextToken' - The token for the next page of results.
--
-- * 'dvpcpcVPCPeeringConnectionIds' - One or more VPC peering connection IDs. Default: Describes all your VPC peering connections.
--
-- * 'dvpcpcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvpcpcMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeVPCPeeringConnections ::
  DescribeVPCPeeringConnections
describeVPCPeeringConnections =
  DescribeVPCPeeringConnections'
    { _dvpcpcFilters = Nothing,
      _dvpcpcNextToken = Nothing,
      _dvpcpcVPCPeeringConnectionIds = Nothing,
      _dvpcpcDryRun = Nothing,
      _dvpcpcMaxResults = Nothing
    }

-- | One or more filters.     * @accepter-vpc-info.cidr-block@ - The IPv4 CIDR block of the accepter VPC.     * @accepter-vpc-info.owner-id@ - The AWS account ID of the owner of the accepter VPC.     * @accepter-vpc-info.vpc-id@ - The ID of the accepter VPC.     * @expiration-time@ - The expiration date and time for the VPC peering connection.     * @requester-vpc-info.cidr-block@ - The IPv4 CIDR block of the requester's VPC.     * @requester-vpc-info.owner-id@ - The AWS account ID of the owner of the requester VPC.     * @requester-vpc-info.vpc-id@ - The ID of the requester VPC.     * @status-code@ - The status of the VPC peering connection (@pending-acceptance@ | @failed@ | @expired@ | @provisioning@ | @active@ | @deleting@ | @deleted@ | @rejected@ ).     * @status-message@ - A message that provides more information about the status of the VPC peering connection, if applicable.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @vpc-peering-connection-id@ - The ID of the VPC peering connection.
dvpcpcFilters :: Lens' DescribeVPCPeeringConnections [Filter]
dvpcpcFilters = lens _dvpcpcFilters (\s a -> s {_dvpcpcFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dvpcpcNextToken :: Lens' DescribeVPCPeeringConnections (Maybe Text)
dvpcpcNextToken = lens _dvpcpcNextToken (\s a -> s {_dvpcpcNextToken = a})

-- | One or more VPC peering connection IDs. Default: Describes all your VPC peering connections.
dvpcpcVPCPeeringConnectionIds :: Lens' DescribeVPCPeeringConnections [Text]
dvpcpcVPCPeeringConnectionIds = lens _dvpcpcVPCPeeringConnectionIds (\s a -> s {_dvpcpcVPCPeeringConnectionIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvpcpcDryRun :: Lens' DescribeVPCPeeringConnections (Maybe Bool)
dvpcpcDryRun = lens _dvpcpcDryRun (\s a -> s {_dvpcpcDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dvpcpcMaxResults :: Lens' DescribeVPCPeeringConnections (Maybe Natural)
dvpcpcMaxResults = lens _dvpcpcMaxResults (\s a -> s {_dvpcpcMaxResults = a}) . mapping _Nat

instance AWSPager DescribeVPCPeeringConnections where
  page rq rs
    | stop (rs ^. dvpcpcrsNextToken) = Nothing
    | stop (rs ^. dvpcpcrsVPCPeeringConnections) = Nothing
    | otherwise =
      Just $ rq & dvpcpcNextToken .~ rs ^. dvpcpcrsNextToken

instance AWSRequest DescribeVPCPeeringConnections where
  type
    Rs DescribeVPCPeeringConnections =
      DescribeVPCPeeringConnectionsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeVPCPeeringConnectionsResponse'
            <$> (x .@? "nextToken")
            <*> ( x .@? "vpcPeeringConnectionSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeVPCPeeringConnections

instance NFData DescribeVPCPeeringConnections

instance ToHeaders DescribeVPCPeeringConnections where
  toHeaders = const mempty

instance ToPath DescribeVPCPeeringConnections where
  toPath = const "/"

instance ToQuery DescribeVPCPeeringConnections where
  toQuery DescribeVPCPeeringConnections' {..} =
    mconcat
      [ "Action" =: ("DescribeVpcPeeringConnections" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dvpcpcFilters),
        "NextToken" =: _dvpcpcNextToken,
        toQuery
          ( toQueryList "VpcPeeringConnectionId"
              <$> _dvpcpcVPCPeeringConnectionIds
          ),
        "DryRun" =: _dvpcpcDryRun,
        "MaxResults" =: _dvpcpcMaxResults
      ]

-- | /See:/ 'describeVPCPeeringConnectionsResponse' smart constructor.
data DescribeVPCPeeringConnectionsResponse = DescribeVPCPeeringConnectionsResponse'
  { _dvpcpcrsNextToken ::
      !(Maybe Text),
    _dvpcpcrsVPCPeeringConnections ::
      !( Maybe
           [VPCPeeringConnection]
       ),
    _dvpcpcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeVPCPeeringConnectionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcpcrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dvpcpcrsVPCPeeringConnections' - Information about the VPC peering connections.
--
-- * 'dvpcpcrsResponseStatus' - -- | The response status code.
describeVPCPeeringConnectionsResponse ::
  -- | 'dvpcpcrsResponseStatus'
  Int ->
  DescribeVPCPeeringConnectionsResponse
describeVPCPeeringConnectionsResponse pResponseStatus_ =
  DescribeVPCPeeringConnectionsResponse'
    { _dvpcpcrsNextToken =
        Nothing,
      _dvpcpcrsVPCPeeringConnections = Nothing,
      _dvpcpcrsResponseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dvpcpcrsNextToken :: Lens' DescribeVPCPeeringConnectionsResponse (Maybe Text)
dvpcpcrsNextToken = lens _dvpcpcrsNextToken (\s a -> s {_dvpcpcrsNextToken = a})

-- | Information about the VPC peering connections.
dvpcpcrsVPCPeeringConnections :: Lens' DescribeVPCPeeringConnectionsResponse [VPCPeeringConnection]
dvpcpcrsVPCPeeringConnections = lens _dvpcpcrsVPCPeeringConnections (\s a -> s {_dvpcpcrsVPCPeeringConnections = a}) . _Default . _Coerce

-- | -- | The response status code.
dvpcpcrsResponseStatus :: Lens' DescribeVPCPeeringConnectionsResponse Int
dvpcpcrsResponseStatus = lens _dvpcpcrsResponseStatus (\s a -> s {_dvpcpcrsResponseStatus = a})

instance NFData DescribeVPCPeeringConnectionsResponse
