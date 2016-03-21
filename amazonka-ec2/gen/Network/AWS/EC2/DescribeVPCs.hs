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
-- Module      : Network.AWS.EC2.DescribeVPCs
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPCs.
module Network.AWS.EC2.DescribeVPCs
    (
    -- * Creating a Request
      describeVPCs
    , DescribeVPCs
    -- * Request Lenses
    , dvsFilters
    , dvsVPCIds
    , dvsDryRun

    -- * Destructuring the Response
    , describeVPCsResponse
    , DescribeVPCsResponse
    -- * Response Lenses
    , dvrsVPCs
    , dvrsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVPCs' smart constructor.
data DescribeVPCs = DescribeVPCs'
    { _dvsFilters :: !(Maybe [Filter])
    , _dvsVPCIds  :: !(Maybe [Text])
    , _dvsDryRun  :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeVPCs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvsFilters'
--
-- * 'dvsVPCIds'
--
-- * 'dvsDryRun'
describeVPCs
    :: DescribeVPCs
describeVPCs =
    DescribeVPCs'
    { _dvsFilters = Nothing
    , _dvsVPCIds = Nothing
    , _dvsDryRun = Nothing
    }

-- | One or more filters.
--
-- -   'cidr' - The CIDR block of the VPC. The CIDR block you specify must
--     exactly match the VPC\'s CIDR block for information to be returned
--     for the VPC. Must contain the slash followed by one or two digits
--     (for example, '\/28').
--
-- -   'dhcp-options-id' - The ID of a set of DHCP options.
--
-- -   'isDefault' - Indicates whether the VPC is the default VPC.
--
-- -   'state' - The state of the VPC ('pending' | 'available').
--
-- -   'tag':/key/=/value/ - The key\/value combination of a tag assigned
--     to the resource.
--
-- -   'tag-key' - The key of a tag assigned to the resource. This filter
--     is independent of the 'tag-value' filter. For example, if you use
--     both the filter \"tag-key=Purpose\" and the filter \"tag-value=X\",
--     you get any resources assigned both the tag key Purpose (regardless
--     of what the tag\'s value is), and the tag value X (regardless of
--     what the tag\'s key is). If you want to list only resources where
--     Purpose is X, see the 'tag':/key/=/value/ filter.
--
-- -   'tag-value' - The value of a tag assigned to the resource. This
--     filter is independent of the 'tag-key' filter.
--
-- -   'vpc-id' - The ID of the VPC.
--
dvsFilters :: Lens' DescribeVPCs [Filter]
dvsFilters = lens _dvsFilters (\ s a -> s{_dvsFilters = a}) . _Default . _Coerce;

-- | One or more VPC IDs.
--
-- Default: Describes all your VPCs.
dvsVPCIds :: Lens' DescribeVPCs [Text]
dvsVPCIds = lens _dvsVPCIds (\ s a -> s{_dvsVPCIds = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
dvsDryRun :: Lens' DescribeVPCs (Maybe Bool)
dvsDryRun = lens _dvsDryRun (\ s a -> s{_dvsDryRun = a});

instance AWSRequest DescribeVPCs where
        type Rs DescribeVPCs = DescribeVPCsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCsResponse' <$>
                   (x .@? "vpcSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeVPCs

instance ToHeaders DescribeVPCs where
        toHeaders = const mempty

instance ToPath DescribeVPCs where
        toPath = const "/"

instance ToQuery DescribeVPCs where
        toQuery DescribeVPCs'{..}
          = mconcat
              ["Action" =: ("DescribeVpcs" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvsFilters),
               toQuery (toQueryList "VpcId" <$> _dvsVPCIds),
               "DryRun" =: _dvsDryRun]

-- | /See:/ 'describeVPCsResponse' smart constructor.
data DescribeVPCsResponse = DescribeVPCsResponse'
    { _dvrsVPCs           :: !(Maybe [VPC])
    , _dvrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeVPCsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvrsVPCs'
--
-- * 'dvrsResponseStatus'
describeVPCsResponse
    :: Int -- ^ 'dvrsResponseStatus'
    -> DescribeVPCsResponse
describeVPCsResponse pResponseStatus_ =
    DescribeVPCsResponse'
    { _dvrsVPCs = Nothing
    , _dvrsResponseStatus = pResponseStatus_
    }

-- | Information about one or more VPCs.
dvrsVPCs :: Lens' DescribeVPCsResponse [VPC]
dvrsVPCs = lens _dvrsVPCs (\ s a -> s{_dvrsVPCs = a}) . _Default . _Coerce;

-- | The response status code.
dvrsResponseStatus :: Lens' DescribeVPCsResponse Int
dvrsResponseStatus = lens _dvrsResponseStatus (\ s a -> s{_dvrsResponseStatus = a});
