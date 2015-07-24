{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeInternetGateways
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your Internet gateways.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInternetGateways.html>
module Network.AWS.EC2.DescribeInternetGateways
    (
    -- * Request
      DescribeInternetGateways
    -- ** Request constructor
    , describeInternetGateways
    -- ** Request lenses
    , dFilters
    , dInternetGatewayIds
    , dDryRun

    -- * Response
    , DescribeInternetGatewaysResponse
    -- ** Response constructor
    , describeInternetGatewaysResponse
    -- ** Response lenses
    , digrsInternetGateways
    , digrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeInternetGateways' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dFilters'
--
-- * 'dInternetGatewayIds'
--
-- * 'dDryRun'
data DescribeInternetGateways = DescribeInternetGateways'
    { _dFilters            :: !(Maybe [Filter])
    , _dInternetGatewayIds :: !(Maybe [Text])
    , _dDryRun             :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeInternetGateways' smart constructor.
describeInternetGateways :: DescribeInternetGateways
describeInternetGateways =
    DescribeInternetGateways'
    { _dFilters = Nothing
    , _dInternetGatewayIds = Nothing
    , _dDryRun = Nothing
    }

-- | One or more filters.
--
-- -   @attachment.state@ - The current state of the attachment between the
--     gateway and the VPC (@available@). Present only if a VPC is
--     attached.
--
-- -   @attachment.vpc-id@ - The ID of an attached VPC.
--
-- -   @internet-gateway-id@ - The ID of the Internet gateway.
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
dFilters :: Lens' DescribeInternetGateways [Filter]
dFilters = lens _dFilters (\ s a -> s{_dFilters = a}) . _Default;

-- | One or more Internet gateway IDs.
--
-- Default: Describes all your Internet gateways.
dInternetGatewayIds :: Lens' DescribeInternetGateways [Text]
dInternetGatewayIds = lens _dInternetGatewayIds (\ s a -> s{_dInternetGatewayIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dDryRun :: Lens' DescribeInternetGateways (Maybe Bool)
dDryRun = lens _dDryRun (\ s a -> s{_dDryRun = a});

instance AWSRequest DescribeInternetGateways where
        type Sv DescribeInternetGateways = EC2
        type Rs DescribeInternetGateways =
             DescribeInternetGatewaysResponse
        request = post "DescribeInternetGateways"
        response
          = receiveXML
              (\ s h x ->
                 DescribeInternetGatewaysResponse' <$>
                   (x .@? "internetGatewaySet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeInternetGateways where
        toHeaders = const mempty

instance ToPath DescribeInternetGateways where
        toPath = const "/"

instance ToQuery DescribeInternetGateways where
        toQuery DescribeInternetGateways'{..}
          = mconcat
              ["Action" =:
                 ("DescribeInternetGateways" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dFilters),
               toQuery
                 (toQueryList "item" <$> _dInternetGatewayIds),
               "DryRun" =: _dDryRun]

-- | /See:/ 'describeInternetGatewaysResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'digrsInternetGateways'
--
-- * 'digrsStatus'
data DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse'
    { _digrsInternetGateways :: !(Maybe [InternetGateway])
    , _digrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeInternetGatewaysResponse' smart constructor.
describeInternetGatewaysResponse :: Int -> DescribeInternetGatewaysResponse
describeInternetGatewaysResponse pStatus_ =
    DescribeInternetGatewaysResponse'
    { _digrsInternetGateways = Nothing
    , _digrsStatus = pStatus_
    }

-- | Information about one or more Internet gateways.
digrsInternetGateways :: Lens' DescribeInternetGatewaysResponse [InternetGateway]
digrsInternetGateways = lens _digrsInternetGateways (\ s a -> s{_digrsInternetGateways = a}) . _Default;

-- | FIXME: Undocumented member.
digrsStatus :: Lens' DescribeInternetGatewaysResponse Int
digrsStatus = lens _digrsStatus (\ s a -> s{_digrsStatus = a});
