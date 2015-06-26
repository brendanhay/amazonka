{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DescribeVPNGateways
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

-- | Describes one or more of your virtual private gateways.
--
-- For more information about virtual private gateways, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding an IPsec Hardware VPN to Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVPNGateways.html>
module Network.AWS.EC2.DescribeVPNGateways
    (
    -- * Request
      DescribeVPNGateways
    -- ** Request constructor
    , describeVPNGateways
    -- ** Request lenses
    , dvpngFilters
    , dvpngDryRun
    , dvpngVPNGatewayIds

    -- * Response
    , DescribeVPNGatewaysResponse
    -- ** Response constructor
    , describeVPNGatewaysResponse
    -- ** Response lenses
    , dvgrVPNGateways
    , dvgrStatusCode
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVPNGateways' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpngFilters'
--
-- * 'dvpngDryRun'
--
-- * 'dvpngVPNGatewayIds'
data DescribeVPNGateways = DescribeVPNGateways'{_dvpngFilters :: Maybe [Filter], _dvpngDryRun :: Maybe Bool, _dvpngVPNGatewayIds :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'DescribeVPNGateways' smart constructor.
describeVPNGateways :: DescribeVPNGateways
describeVPNGateways = DescribeVPNGateways'{_dvpngFilters = Nothing, _dvpngDryRun = Nothing, _dvpngVPNGatewayIds = Nothing};

-- | One or more filters.
--
-- -   @attachment.state@ - The current state of the attachment between the
--     gateway and the VPC (@attaching@ | @attached@ | @detaching@ |
--     @detached@).
--
-- -   @attachment.vpc-id@ - The ID of an attached VPC.
--
-- -   @availability-zone@ - The Availability Zone for the virtual private
--     gateway.
--
-- -   @state@ - The state of the virtual private gateway (@pending@ |
--     @available@ | @deleting@ | @deleted@).
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
-- -   @type@ - The type of virtual private gateway. Currently the only
--     supported type is @ipsec.1@.
--
-- -   @vpn-gateway-id@ - The ID of the virtual private gateway.
--
dvpngFilters :: Lens' DescribeVPNGateways [Filter]
dvpngFilters = lens _dvpngFilters (\ s a -> s{_dvpngFilters = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvpngDryRun :: Lens' DescribeVPNGateways (Maybe Bool)
dvpngDryRun = lens _dvpngDryRun (\ s a -> s{_dvpngDryRun = a});

-- | One or more virtual private gateway IDs.
--
-- Default: Describes all your virtual private gateways.
dvpngVPNGatewayIds :: Lens' DescribeVPNGateways [Text]
dvpngVPNGatewayIds = lens _dvpngVPNGatewayIds (\ s a -> s{_dvpngVPNGatewayIds = a}) . _Default;

instance AWSRequest DescribeVPNGateways where
        type Sv DescribeVPNGateways = EC2
        type Rs DescribeVPNGateways =
             DescribeVPNGatewaysResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPNGatewaysResponse' <$>
                   (may (parseXMLList "item") x) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeVPNGateways where
        toHeaders = const mempty

instance ToPath DescribeVPNGateways where
        toPath = const "/"

instance ToQuery DescribeVPNGateways where
        toQuery DescribeVPNGateways'{..}
          = mconcat
              ["Action" =: ("DescribeVPNGateways" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvpngFilters),
               "DryRun" =: _dvpngDryRun,
               toQuery
                 (toQueryList "VpnGatewayId" <$> _dvpngVPNGatewayIds)]

-- | /See:/ 'describeVPNGatewaysResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvgrVPNGateways'
--
-- * 'dvgrStatusCode'
data DescribeVPNGatewaysResponse = DescribeVPNGatewaysResponse'{_dvgrVPNGateways :: Maybe [VPNGateway], _dvgrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DescribeVPNGatewaysResponse' smart constructor.
describeVPNGatewaysResponse :: Int -> DescribeVPNGatewaysResponse
describeVPNGatewaysResponse pStatusCode = DescribeVPNGatewaysResponse'{_dvgrVPNGateways = Nothing, _dvgrStatusCode = pStatusCode};

-- | Information about one or more virtual private gateways.
dvgrVPNGateways :: Lens' DescribeVPNGatewaysResponse [VPNGateway]
dvgrVPNGateways = lens _dvgrVPNGateways (\ s a -> s{_dvgrVPNGateways = a}) . _Default;

-- | FIXME: Undocumented member.
dvgrStatusCode :: Lens' DescribeVPNGatewaysResponse Int
dvgrStatusCode = lens _dvgrStatusCode (\ s a -> s{_dvgrStatusCode = a});
