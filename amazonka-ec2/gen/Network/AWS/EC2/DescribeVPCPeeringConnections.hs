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
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPC peering connections.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVPCPeeringConnections.html>
module Network.AWS.EC2.DescribeVPCPeeringConnections
    (
    -- * Request
      DescribeVPCPeeringConnections
    -- ** Request constructor
    , describeVPCPeeringConnections
    -- ** Request lenses
    , dvpcpcrqFilters
    , dvpcpcrqVPCPeeringConnectionIds
    , dvpcpcrqDryRun

    -- * Response
    , DescribeVPCPeeringConnectionsResponse
    -- ** Response constructor
    , describeVPCPeeringConnectionsResponse
    -- ** Response lenses
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
-- * 'dvpcpcrqFilters'
--
-- * 'dvpcpcrqVPCPeeringConnectionIds'
--
-- * 'dvpcpcrqDryRun'
data DescribeVPCPeeringConnections = DescribeVPCPeeringConnections'
    { _dvpcpcrqFilters                 :: !(Maybe [Filter])
    , _dvpcpcrqVPCPeeringConnectionIds :: !(Maybe [Text])
    , _dvpcpcrqDryRun                  :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVPCPeeringConnections' smart constructor.
describeVPCPeeringConnections :: DescribeVPCPeeringConnections
describeVPCPeeringConnections =
    DescribeVPCPeeringConnections'
    { _dvpcpcrqFilters = Nothing
    , _dvpcpcrqVPCPeeringConnectionIds = Nothing
    , _dvpcpcrqDryRun = Nothing
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
dvpcpcrqFilters :: Lens' DescribeVPCPeeringConnections [Filter]
dvpcpcrqFilters = lens _dvpcpcrqFilters (\ s a -> s{_dvpcpcrqFilters = a}) . _Default;

-- | One or more VPC peering connection IDs.
--
-- Default: Describes all your VPC peering connections.
dvpcpcrqVPCPeeringConnectionIds :: Lens' DescribeVPCPeeringConnections [Text]
dvpcpcrqVPCPeeringConnectionIds = lens _dvpcpcrqVPCPeeringConnectionIds (\ s a -> s{_dvpcpcrqVPCPeeringConnectionIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvpcpcrqDryRun :: Lens' DescribeVPCPeeringConnections (Maybe Bool)
dvpcpcrqDryRun = lens _dvpcpcrqDryRun (\ s a -> s{_dvpcpcrqDryRun = a});

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
                 ("DescribeVPCPeeringConnections" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvpcpcrqFilters),
               toQuery
                 (toQueryList "item" <$>
                    _dvpcpcrqVPCPeeringConnectionIds),
               "DryRun" =: _dvpcpcrqDryRun]

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
describeVPCPeeringConnectionsResponse pStatus =
    DescribeVPCPeeringConnectionsResponse'
    { _dvpcpcrsVPCPeeringConnections = Nothing
    , _dvpcpcrsStatus = pStatus
    }

-- | Information about the VPC peering connections.
dvpcpcrsVPCPeeringConnections :: Lens' DescribeVPCPeeringConnectionsResponse [VPCPeeringConnection]
dvpcpcrsVPCPeeringConnections = lens _dvpcpcrsVPCPeeringConnections (\ s a -> s{_dvpcpcrsVPCPeeringConnections = a}) . _Default;

-- | FIXME: Undocumented member.
dvpcpcrsStatus :: Lens' DescribeVPCPeeringConnectionsResponse Int
dvpcpcrsStatus = lens _dvpcpcrsStatus (\ s a -> s{_dvpcpcrsStatus = a});
