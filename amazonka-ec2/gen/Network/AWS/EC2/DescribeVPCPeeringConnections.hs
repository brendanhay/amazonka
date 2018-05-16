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
-- Module      : Network.AWS.EC2.DescribeVPCPeeringConnections
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPC peering connections.
--
--
module Network.AWS.EC2.DescribeVPCPeeringConnections
    (
    -- * Creating a Request
      describeVPCPeeringConnections
    , DescribeVPCPeeringConnections
    -- * Request Lenses
    , dvpcpcFilters
    , dvpcpcVPCPeeringConnectionIds
    , dvpcpcDryRun

    -- * Destructuring the Response
    , describeVPCPeeringConnectionsResponse
    , DescribeVPCPeeringConnectionsResponse
    -- * Response Lenses
    , dvpcpcrsVPCPeeringConnections
    , dvpcpcrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeVpcPeeringConnections.
--
--
--
-- /See:/ 'describeVPCPeeringConnections' smart constructor.
data DescribeVPCPeeringConnections = DescribeVPCPeeringConnections'
  { _dvpcpcFilters                 :: !(Maybe [Filter])
  , _dvpcpcVPCPeeringConnectionIds :: !(Maybe [Text])
  , _dvpcpcDryRun                  :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCPeeringConnections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcpcFilters' - One or more filters.     * @accepter-vpc-info.cidr-block@ - The IPv4 CIDR block of the accepter VPC.     * @accepter-vpc-info.owner-id@ - The AWS account ID of the owner of the accepter VPC.     * @accepter-vpc-info.vpc-id@ - The ID of the accepter VPC.     * @expiration-time@ - The expiration date and time for the VPC peering connection.     * @requester-vpc-info.cidr-block@ - The IPv4 CIDR block of the requester's VPC.     * @requester-vpc-info.owner-id@ - The AWS account ID of the owner of the requester VPC.     * @requester-vpc-info.vpc-id@ - The ID of the requester VPC.     * @status-code@ - The status of the VPC peering connection (@pending-acceptance@ | @failed@ | @expired@ | @provisioning@ | @active@ | @deleting@ | @deleted@ | @rejected@ ).     * @status-message@ - A message that provides more information about the status of the VPC peering connection, if applicable.     * @tag@ :/key/ =/value/ - The key/value combination of a tag assigned to the resource. Specify the key of the tag in the filter name and the value of the tag in the filter value. For example, for the tag Purpose=X, specify @tag:Purpose@ for the filter name and @X@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. This filter is independent of the @tag-value@ filter. For example, if you use both the filter "tag-key=Purpose" and the filter "tag-value=X", you get any resources assigned both the tag key Purpose (regardless of what the tag's value is), and the tag value X (regardless of what the tag's key is). If you want to list only resources where Purpose is X, see the @tag@ :/key/ =/value/ filter.     * @tag-value@ - The value of a tag assigned to the resource. This filter is independent of the @tag-key@ filter.     * @vpc-peering-connection-id@ - The ID of the VPC peering connection.
--
-- * 'dvpcpcVPCPeeringConnectionIds' - One or more VPC peering connection IDs. Default: Describes all your VPC peering connections.
--
-- * 'dvpcpcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describeVPCPeeringConnections
    :: DescribeVPCPeeringConnections
describeVPCPeeringConnections =
  DescribeVPCPeeringConnections'
    { _dvpcpcFilters = Nothing
    , _dvpcpcVPCPeeringConnectionIds = Nothing
    , _dvpcpcDryRun = Nothing
    }


-- | One or more filters.     * @accepter-vpc-info.cidr-block@ - The IPv4 CIDR block of the accepter VPC.     * @accepter-vpc-info.owner-id@ - The AWS account ID of the owner of the accepter VPC.     * @accepter-vpc-info.vpc-id@ - The ID of the accepter VPC.     * @expiration-time@ - The expiration date and time for the VPC peering connection.     * @requester-vpc-info.cidr-block@ - The IPv4 CIDR block of the requester's VPC.     * @requester-vpc-info.owner-id@ - The AWS account ID of the owner of the requester VPC.     * @requester-vpc-info.vpc-id@ - The ID of the requester VPC.     * @status-code@ - The status of the VPC peering connection (@pending-acceptance@ | @failed@ | @expired@ | @provisioning@ | @active@ | @deleting@ | @deleted@ | @rejected@ ).     * @status-message@ - A message that provides more information about the status of the VPC peering connection, if applicable.     * @tag@ :/key/ =/value/ - The key/value combination of a tag assigned to the resource. Specify the key of the tag in the filter name and the value of the tag in the filter value. For example, for the tag Purpose=X, specify @tag:Purpose@ for the filter name and @X@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. This filter is independent of the @tag-value@ filter. For example, if you use both the filter "tag-key=Purpose" and the filter "tag-value=X", you get any resources assigned both the tag key Purpose (regardless of what the tag's value is), and the tag value X (regardless of what the tag's key is). If you want to list only resources where Purpose is X, see the @tag@ :/key/ =/value/ filter.     * @tag-value@ - The value of a tag assigned to the resource. This filter is independent of the @tag-key@ filter.     * @vpc-peering-connection-id@ - The ID of the VPC peering connection.
dvpcpcFilters :: Lens' DescribeVPCPeeringConnections [Filter]
dvpcpcFilters = lens _dvpcpcFilters (\ s a -> s{_dvpcpcFilters = a}) . _Default . _Coerce

-- | One or more VPC peering connection IDs. Default: Describes all your VPC peering connections.
dvpcpcVPCPeeringConnectionIds :: Lens' DescribeVPCPeeringConnections [Text]
dvpcpcVPCPeeringConnectionIds = lens _dvpcpcVPCPeeringConnectionIds (\ s a -> s{_dvpcpcVPCPeeringConnectionIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvpcpcDryRun :: Lens' DescribeVPCPeeringConnections (Maybe Bool)
dvpcpcDryRun = lens _dvpcpcDryRun (\ s a -> s{_dvpcpcDryRun = a})

instance AWSRequest DescribeVPCPeeringConnections
         where
        type Rs DescribeVPCPeeringConnections =
             DescribeVPCPeeringConnectionsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCPeeringConnectionsResponse' <$>
                   (x .@? "vpcPeeringConnectionSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeVPCPeeringConnections where

instance NFData DescribeVPCPeeringConnections where

instance ToHeaders DescribeVPCPeeringConnections
         where
        toHeaders = const mempty

instance ToPath DescribeVPCPeeringConnections where
        toPath = const "/"

instance ToQuery DescribeVPCPeeringConnections where
        toQuery DescribeVPCPeeringConnections'{..}
          = mconcat
              ["Action" =:
                 ("DescribeVpcPeeringConnections" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvpcpcFilters),
               toQuery
                 (toQueryList "VpcPeeringConnectionId" <$>
                    _dvpcpcVPCPeeringConnectionIds),
               "DryRun" =: _dvpcpcDryRun]

-- | Contains the output of DescribeVpcPeeringConnections.
--
--
--
-- /See:/ 'describeVPCPeeringConnectionsResponse' smart constructor.
data DescribeVPCPeeringConnectionsResponse = DescribeVPCPeeringConnectionsResponse'
  { _dvpcpcrsVPCPeeringConnections :: !(Maybe [VPCPeeringConnection])
  , _dvpcpcrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCPeeringConnectionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcpcrsVPCPeeringConnections' - Information about the VPC peering connections.
--
-- * 'dvpcpcrsResponseStatus' - -- | The response status code.
describeVPCPeeringConnectionsResponse
    :: Int -- ^ 'dvpcpcrsResponseStatus'
    -> DescribeVPCPeeringConnectionsResponse
describeVPCPeeringConnectionsResponse pResponseStatus_ =
  DescribeVPCPeeringConnectionsResponse'
    { _dvpcpcrsVPCPeeringConnections = Nothing
    , _dvpcpcrsResponseStatus = pResponseStatus_
    }


-- | Information about the VPC peering connections.
dvpcpcrsVPCPeeringConnections :: Lens' DescribeVPCPeeringConnectionsResponse [VPCPeeringConnection]
dvpcpcrsVPCPeeringConnections = lens _dvpcpcrsVPCPeeringConnections (\ s a -> s{_dvpcpcrsVPCPeeringConnections = a}) . _Default . _Coerce

-- | -- | The response status code.
dvpcpcrsResponseStatus :: Lens' DescribeVPCPeeringConnectionsResponse Int
dvpcpcrsResponseStatus = lens _dvpcpcrsResponseStatus (\ s a -> s{_dvpcpcrsResponseStatus = a})

instance NFData DescribeVPCPeeringConnectionsResponse
         where
