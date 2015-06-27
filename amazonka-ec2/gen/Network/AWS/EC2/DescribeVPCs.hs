{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeVPCs
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more of your VPCs.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVPCs.html>
module Network.AWS.EC2.DescribeVPCs
    (
    -- * Request
      DescribeVPCs
    -- ** Request constructor
    , describeVPCs
    -- ** Request lenses
    , dv1Filters
    , dv1VPCIds
    , dv1DryRun

    -- * Response
    , DescribeVPCsResponse
    -- ** Response constructor
    , describeVPCsResponse
    -- ** Response lenses
    , dvrVPCs
    , dvrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVPCs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dv1Filters'
--
-- * 'dv1VPCIds'
--
-- * 'dv1DryRun'
data DescribeVPCs = DescribeVPCs'
    { _dv1Filters :: !(Maybe [Filter])
    , _dv1VPCIds  :: !(Maybe [Text])
    , _dv1DryRun  :: !(Maybe Bool)
    } deriving (Eq,Read,Show)

-- | 'DescribeVPCs' smart constructor.
describeVPCs :: DescribeVPCs
describeVPCs =
    DescribeVPCs'
    { _dv1Filters = Nothing
    , _dv1VPCIds = Nothing
    , _dv1DryRun = Nothing
    }

-- | One or more filters.
--
-- -   @cidr@ - The CIDR block of the VPC. The CIDR block you specify must
--     exactly match the VPC\'s CIDR block for information to be returned
--     for the VPC. Must contain the slash followed by one or two digits
--     (for example, @\/28@).
--
-- -   @dhcp-options-id@ - The ID of a set of DHCP options.
--
-- -   @isDefault@ - Indicates whether the VPC is the default VPC.
--
-- -   @state@ - The state of the VPC (@pending@ | @available@).
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
-- -   @vpc-id@ - The ID of the VPC.
--
dv1Filters :: Lens' DescribeVPCs [Filter]
dv1Filters = lens _dv1Filters (\ s a -> s{_dv1Filters = a}) . _Default;

-- | One or more VPC IDs.
--
-- Default: Describes all your VPCs.
dv1VPCIds :: Lens' DescribeVPCs [Text]
dv1VPCIds = lens _dv1VPCIds (\ s a -> s{_dv1VPCIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dv1DryRun :: Lens' DescribeVPCs (Maybe Bool)
dv1DryRun = lens _dv1DryRun (\ s a -> s{_dv1DryRun = a});

instance AWSRequest DescribeVPCs where
        type Sv DescribeVPCs = EC2
        type Rs DescribeVPCs = DescribeVPCsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCsResponse' <$>
                   (may (parseXMLList "item") x) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeVPCs where
        toHeaders = const mempty

instance ToPath DescribeVPCs where
        toPath = const "/"

instance ToQuery DescribeVPCs where
        toQuery DescribeVPCs'{..}
          = mconcat
              ["Action" =: ("DescribeVPCs" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dv1Filters),
               toQuery (toQueryList "VpcId" <$> _dv1VPCIds),
               "DryRun" =: _dv1DryRun]

-- | /See:/ 'describeVPCsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvrVPCs'
--
-- * 'dvrStatus'
data DescribeVPCsResponse = DescribeVPCsResponse'
    { _dvrVPCs   :: !(Maybe [VPC])
    , _dvrStatus :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeVPCsResponse' smart constructor.
describeVPCsResponse :: Int -> DescribeVPCsResponse
describeVPCsResponse pStatus =
    DescribeVPCsResponse'
    { _dvrVPCs = Nothing
    , _dvrStatus = pStatus
    }

-- | Information about one or more VPCs.
dvrVPCs :: Lens' DescribeVPCsResponse [VPC]
dvrVPCs = lens _dvrVPCs (\ s a -> s{_dvrVPCs = a}) . _Default;

-- | FIXME: Undocumented member.
dvrStatus :: Lens' DescribeVPCsResponse Int
dvrStatus = lens _dvrStatus (\ s a -> s{_dvrStatus = a});
