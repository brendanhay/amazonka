{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCClassicLink
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ClassicLink status of one or more VPCs.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVPCClassicLink.html AWS API Reference> for DescribeVPCClassicLink.
module Network.AWS.EC2.DescribeVPCClassicLink
    (
    -- * Creating a Request
      DescribeVPCClassicLink
    , describeVPCClassicLink
    -- * Request Lenses
    , dvclFilters
    , dvclVPCIds
    , dvclDryRun

    -- * Destructuring the Response
    , DescribeVPCClassicLinkResponse
    , describeVPCClassicLinkResponse
    -- * Response Lenses
    , dvclrsVPCs
    , dvclrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVPCClassicLink' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvclFilters'
--
-- * 'dvclVPCIds'
--
-- * 'dvclDryRun'
data DescribeVPCClassicLink = DescribeVPCClassicLink'
    { _dvclFilters :: !(Maybe [Filter])
    , _dvclVPCIds  :: !(Maybe [Text])
    , _dvclDryRun  :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVPCClassicLink' smart constructor.
describeVPCClassicLink :: DescribeVPCClassicLink
describeVPCClassicLink =
    DescribeVPCClassicLink'
    { _dvclFilters = Nothing
    , _dvclVPCIds = Nothing
    , _dvclDryRun = Nothing
    }

-- | One or more filters.
--
-- -   @is-classic-link-enabled@ - Whether the VPC is enabled for
--     ClassicLink (@true@ | @false@).
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
dvclFilters :: Lens' DescribeVPCClassicLink [Filter]
dvclFilters = lens _dvclFilters (\ s a -> s{_dvclFilters = a}) . _Default . _Coerce;

-- | One or more VPCs for which you want to describe the ClassicLink status.
dvclVPCIds :: Lens' DescribeVPCClassicLink [Text]
dvclVPCIds = lens _dvclVPCIds (\ s a -> s{_dvclVPCIds = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvclDryRun :: Lens' DescribeVPCClassicLink (Maybe Bool)
dvclDryRun = lens _dvclDryRun (\ s a -> s{_dvclDryRun = a});

instance AWSRequest DescribeVPCClassicLink where
        type Sv DescribeVPCClassicLink = EC2
        type Rs DescribeVPCClassicLink =
             DescribeVPCClassicLinkResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCClassicLinkResponse' <$>
                   (x .@? "vpcSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeVPCClassicLink where
        toHeaders = const mempty

instance ToPath DescribeVPCClassicLink where
        toPath = const "/"

instance ToQuery DescribeVPCClassicLink where
        toQuery DescribeVPCClassicLink'{..}
          = mconcat
              ["Action" =:
                 ("DescribeVpcClassicLink" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvclFilters),
               toQuery (toQueryList "VpcId" <$> _dvclVPCIds),
               "DryRun" =: _dvclDryRun]

-- | /See:/ 'describeVPCClassicLinkResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvclrsVPCs'
--
-- * 'dvclrsStatus'
data DescribeVPCClassicLinkResponse = DescribeVPCClassicLinkResponse'
    { _dvclrsVPCs   :: !(Maybe [VPCClassicLink])
    , _dvclrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVPCClassicLinkResponse' smart constructor.
describeVPCClassicLinkResponse :: Int -> DescribeVPCClassicLinkResponse
describeVPCClassicLinkResponse pStatus_ =
    DescribeVPCClassicLinkResponse'
    { _dvclrsVPCs = Nothing
    , _dvclrsStatus = pStatus_
    }

-- | The ClassicLink status of one or more VPCs.
dvclrsVPCs :: Lens' DescribeVPCClassicLinkResponse [VPCClassicLink]
dvclrsVPCs = lens _dvclrsVPCs (\ s a -> s{_dvclrsVPCs = a}) . _Default . _Coerce;

-- | Undocumented member.
dvclrsStatus :: Lens' DescribeVPCClassicLinkResponse Int
dvclrsStatus = lens _dvclrsStatus (\ s a -> s{_dvclrsStatus = a});
