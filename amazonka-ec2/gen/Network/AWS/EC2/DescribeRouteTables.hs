{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeRouteTables
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your route tables.
--
-- Each subnet in your VPC must be associated with a route table. If a
-- subnet is not explicitly associated with any route table, it is
-- implicitly associated with the main route table. This command does not
-- return the subnet ID for implicit associations.
--
-- For more information about route tables, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html Route Tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeRouteTables.html>
module Network.AWS.EC2.DescribeRouteTables
    (
    -- * Request
      DescribeRouteTables
    -- ** Request constructor
    , describeRouteTables
    -- ** Request lenses
    , drtsFilters
    , drtsDryRun
    , drtsRouteTableIds

    -- * Response
    , DescribeRouteTablesResponse
    -- ** Response constructor
    , describeRouteTablesResponse
    -- ** Response lenses
    , drtrRouteTables
    , drtrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeRouteTables' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drtsFilters'
--
-- * 'drtsDryRun'
--
-- * 'drtsRouteTableIds'
data DescribeRouteTables = DescribeRouteTables'
    { _drtsFilters       :: !(Maybe [Filter])
    , _drtsDryRun        :: !(Maybe Bool)
    , _drtsRouteTableIds :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeRouteTables' smart constructor.
describeRouteTables :: DescribeRouteTables
describeRouteTables =
    DescribeRouteTables'
    { _drtsFilters = Nothing
    , _drtsDryRun = Nothing
    , _drtsRouteTableIds = Nothing
    }

-- | One or more filters.
--
-- -   @association.route-table-association-id@ - The ID of an association
--     ID for the route table.
--
-- -   @association.route-table-id@ - The ID of the route table involved in
--     the association.
--
-- -   @association.subnet-id@ - The ID of the subnet involved in the
--     association.
--
-- -   @association.main@ - Indicates whether the route table is the main
--     route table for the VPC.
--
-- -   @route-table-id@ - The ID of the route table.
--
-- -   @route.destination-cidr-block@ - The CIDR range specified in a route
--     in the table.
--
-- -   @route.destination-prefix-list-id@ - The ID (prefix) of the AWS
--     service specified in a route in the table.
--
-- -   @route.gateway-id@ - The ID of a gateway specified in a route in the
--     table.
--
-- -   @route.instance-id@ - The ID of an instance specified in a route in
--     the table.
--
-- -   @route.origin@ - Describes how the route was created.
--     @CreateRouteTable@ indicates that the route was automatically
--     created when the route table was created; @CreateRoute@ indicates
--     that the route was manually added to the route table;
--     @EnableVgwRoutePropagation@ indicates that the route was propagated
--     by route propagation.
--
-- -   @route.state@ - The state of a route in the route table (@active@ |
--     @blackhole@). The blackhole state indicates that the route\'s target
--     isn\'t available (for example, the specified gateway isn\'t attached
--     to the VPC, the specified NAT instance has been terminated, and so
--     on).
--
-- -   @route.vpc-peering-connection-id@ - The ID of a VPC peering
--     connection specified in a route in the table.
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
-- -   @vpc-id@ - The ID of the VPC for the route table.
--
drtsFilters :: Lens' DescribeRouteTables [Filter]
drtsFilters = lens _drtsFilters (\ s a -> s{_drtsFilters = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
drtsDryRun :: Lens' DescribeRouteTables (Maybe Bool)
drtsDryRun = lens _drtsDryRun (\ s a -> s{_drtsDryRun = a});

-- | One or more route table IDs.
--
-- Default: Describes all your route tables.
drtsRouteTableIds :: Lens' DescribeRouteTables [Text]
drtsRouteTableIds = lens _drtsRouteTableIds (\ s a -> s{_drtsRouteTableIds = a}) . _Default;

instance AWSRequest DescribeRouteTables where
        type Sv DescribeRouteTables = EC2
        type Rs DescribeRouteTables =
             DescribeRouteTablesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeRouteTablesResponse' <$>
                   (x .@? "routeTableSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeRouteTables where
        toHeaders = const mempty

instance ToPath DescribeRouteTables where
        toPath = const "/"

instance ToQuery DescribeRouteTables where
        toQuery DescribeRouteTables'{..}
          = mconcat
              ["Action" =: ("DescribeRouteTables" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _drtsFilters),
               "DryRun" =: _drtsDryRun,
               toQuery (toQueryList "item" <$> _drtsRouteTableIds)]

-- | /See:/ 'describeRouteTablesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drtrRouteTables'
--
-- * 'drtrStatus'
data DescribeRouteTablesResponse = DescribeRouteTablesResponse'
    { _drtrRouteTables :: !(Maybe [RouteTable])
    , _drtrStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeRouteTablesResponse' smart constructor.
describeRouteTablesResponse :: Int -> DescribeRouteTablesResponse
describeRouteTablesResponse pStatus =
    DescribeRouteTablesResponse'
    { _drtrRouteTables = Nothing
    , _drtrStatus = pStatus
    }

-- | Information about one or more route tables.
drtrRouteTables :: Lens' DescribeRouteTablesResponse [RouteTable]
drtrRouteTables = lens _drtrRouteTables (\ s a -> s{_drtrRouteTables = a}) . _Default;

-- | FIXME: Undocumented member.
drtrStatus :: Lens' DescribeRouteTablesResponse Int
drtrStatus = lens _drtrStatus (\ s a -> s{_drtrStatus = a});
