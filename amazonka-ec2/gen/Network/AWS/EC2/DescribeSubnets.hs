{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSubnets
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your subnets.
--
-- For more information about subnets, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Subnets.html Your VPC and Subnets>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSubnets.html AWS API Reference> for DescribeSubnets.
module Network.AWS.EC2.DescribeSubnets
    (
    -- * Creating a Request
      DescribeSubnets
    , describeSubnets
    -- * Request Lenses
    , dsSubnetIds
    , dsFilters
    , dsDryRun

    -- * Destructuring the Response
    , DescribeSubnetsResponse
    , describeSubnetsResponse
    -- * Response Lenses
    , dsrsSubnets
    , dsrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeSubnets' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsSubnetIds'
--
-- * 'dsFilters'
--
-- * 'dsDryRun'
data DescribeSubnets = DescribeSubnets'
    { _dsSubnetIds :: !(Maybe [Text])
    , _dsFilters   :: !(Maybe [Filter])
    , _dsDryRun    :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSubnets' smart constructor.
describeSubnets :: DescribeSubnets
describeSubnets =
    DescribeSubnets'
    { _dsSubnetIds = Nothing
    , _dsFilters = Nothing
    , _dsDryRun = Nothing
    }

-- | One or more subnet IDs.
--
-- Default: Describes all your subnets.
dsSubnetIds :: Lens' DescribeSubnets [Text]
dsSubnetIds = lens _dsSubnetIds (\ s a -> s{_dsSubnetIds = a}) . _Default . _Coerce;

-- | One or more filters.
--
-- -   @availabilityZone@ - The Availability Zone for the subnet. You can
--     also use @availability-zone@ as the filter name.
--
-- -   @available-ip-address-count@ - The number of IP addresses in the
--     subnet that are available.
--
-- -   @cidrBlock@ - The CIDR block of the subnet. The CIDR block you
--     specify must exactly match the subnet\'s CIDR block for information
--     to be returned for the subnet. You can also use @cidr@ or
--     @cidr-block@ as the filter names.
--
-- -   @defaultForAz@ - Indicates whether this is the default subnet for
--     the Availability Zone. You can also use @default-for-az@ as the
--     filter name.
--
-- -   @state@ - The state of the subnet (@pending@ | @available@).
--
-- -   @subnet-id@ - The ID of the subnet.
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
-- -   @vpc-id@ - The ID of the VPC for the subnet.
--
dsFilters :: Lens' DescribeSubnets [Filter]
dsFilters = lens _dsFilters (\ s a -> s{_dsFilters = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsDryRun :: Lens' DescribeSubnets (Maybe Bool)
dsDryRun = lens _dsDryRun (\ s a -> s{_dsDryRun = a});

instance AWSRequest DescribeSubnets where
        type Sv DescribeSubnets = EC2
        type Rs DescribeSubnets = DescribeSubnetsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeSubnetsResponse' <$>
                   (x .@? "subnetSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeSubnets where
        toHeaders = const mempty

instance ToPath DescribeSubnets where
        toPath = const "/"

instance ToQuery DescribeSubnets where
        toQuery DescribeSubnets'{..}
          = mconcat
              ["Action" =: ("DescribeSubnets" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "SubnetId" <$> _dsSubnetIds),
               toQuery (toQueryList "Filter" <$> _dsFilters),
               "DryRun" =: _dsDryRun]

-- | /See:/ 'describeSubnetsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrsSubnets'
--
-- * 'dsrsStatus'
data DescribeSubnetsResponse = DescribeSubnetsResponse'
    { _dsrsSubnets :: !(Maybe [Subnet])
    , _dsrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSubnetsResponse' smart constructor.
describeSubnetsResponse :: Int -> DescribeSubnetsResponse
describeSubnetsResponse pStatus_ =
    DescribeSubnetsResponse'
    { _dsrsSubnets = Nothing
    , _dsrsStatus = pStatus_
    }

-- | Information about one or more subnets.
dsrsSubnets :: Lens' DescribeSubnetsResponse [Subnet]
dsrsSubnets = lens _dsrsSubnets (\ s a -> s{_dsrsSubnets = a}) . _Default . _Coerce;

-- | Undocumented member.
dsrsStatus :: Lens' DescribeSubnetsResponse Int
dsrsStatus = lens _dsrsStatus (\ s a -> s{_dsrsStatus = a});
