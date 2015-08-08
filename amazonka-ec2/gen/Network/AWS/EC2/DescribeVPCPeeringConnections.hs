{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCPeeringConnections
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPC peering connections.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVPCPeeringConnections.html AWS API Reference> for DescribeVPCPeeringConnections.
module Network.AWS.EC2.DescribeVPCPeeringConnections
    (
    -- * Creating a Request
      DescribeVPCPeeringConnections
    , describeVPCPeeringConnections
    -- * Request Lenses
    , dvpcpcFilters
    , dvpcpcVPCPeeringConnectionIds
    , dvpcpcDryRun

    -- * Destructuring the Response
    , DescribeVPCPeeringConnectionsResponse
    , describeVPCPeeringConnectionsResponse
    -- * Response Lenses
    , dvpcpcrsVPCPeeringConnections
    , dvpcpcrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVPCPeeringConnections' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpcpcFilters'
--
-- * 'dvpcpcVPCPeeringConnectionIds'
--
-- * 'dvpcpcDryRun'
data DescribeVPCPeeringConnections = DescribeVPCPeeringConnections'
    { _dvpcpcFilters                 :: !(Maybe [Filter])
    , _dvpcpcVPCPeeringConnectionIds :: !(Maybe [Text])
    , _dvpcpcDryRun                  :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVPCPeeringConnections' smart constructor.
describeVPCPeeringConnections :: DescribeVPCPeeringConnections
describeVPCPeeringConnections =
    DescribeVPCPeeringConnections'
    { _dvpcpcFilters = Nothing
    , _dvpcpcVPCPeeringConnectionIds = Nothing
    , _dvpcpcDryRun = Nothing
    }

-- | One or more filters.
--
-- -   @accepter-vpc-info.cidr-block@ - The CIDR block of the peer VPC.
--
-- -   @accepter-vpc-info.owner-id@ - The AWS account ID of the owner of
--     the peer VPC.
--
-- -   @accepter-vpc-info.vpc-id@ - The ID of the peer VPC.
--
-- -   @expiration-time@ - The expiration date and time for the VPC peering
--     connection.
--
-- -   @requester-vpc-info.cidr-block@ - The CIDR block of the requester\'s
--     VPC.
--
-- -   @requester-vpc-info.owner-id@ - The AWS account ID of the owner of
--     the requester VPC.
--
-- -   @requester-vpc-info.vpc-id@ - The ID of the requester VPC.
--
-- -   @status-code@ - The status of the VPC peering connection
--     (@pending-acceptance@ | @failed@ | @expired@ | @provisioning@ |
--     @active@ | @deleted@ | @rejected@).
--
-- -   @status-message@ - A message that provides more information about
--     the status of the VPC peering connection, if applicable.
--
-- -   @tag@:/key/=/value/ - The key\/value combination of a tag assigned
--     to the resource.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. This filter
--     is independent of the @tag-value@ filter. For example, if you use
--     both the filter \"tag-key=Purpose\" and the filter \"tag-value=X\",
--     you get any resources assigned both the tag key Purpose (regardless
--     of what the tag\'s value is), and the tag value X (regardless of
--     what the tag\'s key is). If you want to list only resources where
--     Purpose is X, see the @tag@:/key/=/value/ filter.
--
-- -   @tag-value@ - The value of a tag assigned to the resource. This
--     filter is independent of the @tag-key@ filter.
--
-- -   @vpc-peering-connection-id@ - The ID of the VPC peering connection.
--
dvpcpcFilters :: Lens' DescribeVPCPeeringConnections [Filter]
dvpcpcFilters = lens _dvpcpcFilters (\ s a -> s{_dvpcpcFilters = a}) . _Default . _Coerce;

-- | One or more VPC peering connection IDs.
--
-- Default: Describes all your VPC peering connections.
dvpcpcVPCPeeringConnectionIds :: Lens' DescribeVPCPeeringConnections [Text]
dvpcpcVPCPeeringConnectionIds = lens _dvpcpcVPCPeeringConnectionIds (\ s a -> s{_dvpcpcVPCPeeringConnectionIds = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvpcpcDryRun :: Lens' DescribeVPCPeeringConnections (Maybe Bool)
dvpcpcDryRun = lens _dvpcpcDryRun (\ s a -> s{_dvpcpcDryRun = a});

instance AWSRequest DescribeVPCPeeringConnections
         where
        type Sv DescribeVPCPeeringConnections = EC2
        type Rs DescribeVPCPeeringConnections =
             DescribeVPCPeeringConnectionsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCPeeringConnectionsResponse' <$>
                   (x .@? "vpcPeeringConnectionSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

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
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvpcpcFilters),
               toQuery
                 (toQueryList "item" <$>
                    _dvpcpcVPCPeeringConnectionIds),
               "DryRun" =: _dvpcpcDryRun]

-- | /See:/ 'describeVPCPeeringConnectionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpcpcrsVPCPeeringConnections'
--
-- * 'dvpcpcrsStatus'
data DescribeVPCPeeringConnectionsResponse = DescribeVPCPeeringConnectionsResponse'
    { _dvpcpcrsVPCPeeringConnections :: !(Maybe [VPCPeeringConnection])
    , _dvpcpcrsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVPCPeeringConnectionsResponse' smart constructor.
describeVPCPeeringConnectionsResponse :: Int -> DescribeVPCPeeringConnectionsResponse
describeVPCPeeringConnectionsResponse pStatus_ =
    DescribeVPCPeeringConnectionsResponse'
    { _dvpcpcrsVPCPeeringConnections = Nothing
    , _dvpcpcrsStatus = pStatus_
    }

-- | Information about the VPC peering connections.
dvpcpcrsVPCPeeringConnections :: Lens' DescribeVPCPeeringConnectionsResponse [VPCPeeringConnection]
dvpcpcrsVPCPeeringConnections = lens _dvpcpcrsVPCPeeringConnections (\ s a -> s{_dvpcpcrsVPCPeeringConnections = a}) . _Default . _Coerce;

-- | Undocumented member.
dvpcpcrsStatus :: Lens' DescribeVPCPeeringConnectionsResponse Int
dvpcpcrsStatus = lens _dvpcpcrsStatus (\ s a -> s{_dvpcpcrsStatus = a});
