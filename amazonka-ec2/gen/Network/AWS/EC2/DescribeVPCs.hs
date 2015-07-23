{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPCs.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVPCs.html>
module Network.AWS.EC2.DescribeVPCs
    (
    -- * Request
      DescribeVPCs
    -- ** Request constructor
    , describeVPCs
    -- ** Request lenses
    , dvsrqFilters
    , dvsrqVPCIds
    , dvsrqDryRun

    -- * Response
    , DescribeVPCsResponse
    -- ** Response constructor
    , describeVPCsResponse
    -- ** Response lenses
    , dvrsVPCs
    , dvrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVPCs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvsrqFilters'
--
-- * 'dvsrqVPCIds'
--
-- * 'dvsrqDryRun'
data DescribeVPCs = DescribeVPCs'
    { _dvsrqFilters :: !(Maybe [Filter])
    , _dvsrqVPCIds  :: !(Maybe [Text])
    , _dvsrqDryRun  :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVPCs' smart constructor.
describeVPCs :: DescribeVPCs
describeVPCs =
    DescribeVPCs'
    { _dvsrqFilters = Nothing
    , _dvsrqVPCIds = Nothing
    , _dvsrqDryRun = Nothing
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
dvsrqFilters :: Lens' DescribeVPCs [Filter]
dvsrqFilters = lens _dvsrqFilters (\ s a -> s{_dvsrqFilters = a}) . _Default;

-- | One or more VPC IDs.
--
-- Default: Describes all your VPCs.
dvsrqVPCIds :: Lens' DescribeVPCs [Text]
dvsrqVPCIds = lens _dvsrqVPCIds (\ s a -> s{_dvsrqVPCIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvsrqDryRun :: Lens' DescribeVPCs (Maybe Bool)
dvsrqDryRun = lens _dvsrqDryRun (\ s a -> s{_dvsrqDryRun = a});

instance AWSRequest DescribeVPCs where
        type Sv DescribeVPCs = EC2
        type Rs DescribeVPCs = DescribeVPCsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCsResponse' <$>
                   (x .@? "vpcSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeVPCs where
        toHeaders = const mempty

instance ToPath DescribeVPCs where
        toPath = const "/"

instance ToQuery DescribeVPCs where
        toQuery DescribeVPCs'{..}
          = mconcat
              ["Action" =: ("DescribeVPCs" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvsrqFilters),
               toQuery (toQueryList "VpcId" <$> _dvsrqVPCIds),
               "DryRun" =: _dvsrqDryRun]

-- | /See:/ 'describeVPCsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvrsVPCs'
--
-- * 'dvrsStatus'
data DescribeVPCsResponse = DescribeVPCsResponse'
    { _dvrsVPCs   :: !(Maybe [VPC])
    , _dvrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVPCsResponse' smart constructor.
describeVPCsResponse :: Int -> DescribeVPCsResponse
describeVPCsResponse pStatus_ =
    DescribeVPCsResponse'
    { _dvrsVPCs = Nothing
    , _dvrsStatus = pStatus_
    }

-- | Information about one or more VPCs.
dvrsVPCs :: Lens' DescribeVPCsResponse [VPC]
dvrsVPCs = lens _dvrsVPCs (\ s a -> s{_dvrsVPCs = a}) . _Default;

-- | FIXME: Undocumented member.
dvrsStatus :: Lens' DescribeVPCsResponse Int
dvrsStatus = lens _dvrsStatus (\ s a -> s{_dvrsStatus = a});
