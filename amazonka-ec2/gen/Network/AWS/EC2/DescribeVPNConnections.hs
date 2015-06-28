{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeVPNConnections
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

-- | Describes one or more of your VPN connections.
--
-- For more information about VPN connections, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding a Hardware Virtual Private Gateway to Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVPNConnections.html>
module Network.AWS.EC2.DescribeVPNConnections
    (
    -- * Request
      DescribeVPNConnections
    -- ** Request constructor
    , describeVPNConnections
    -- ** Request lenses
    , dvpncFilters
    , dvpncVPNConnectionIds
    , dvpncDryRun

    -- * Response
    , DescribeVPNConnectionsResponse
    -- ** Response constructor
    , describeVPNConnectionsResponse
    -- ** Response lenses
    , dvcrVPNConnections
    , dvcrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVPNConnections' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpncFilters'
--
-- * 'dvpncVPNConnectionIds'
--
-- * 'dvpncDryRun'
data DescribeVPNConnections = DescribeVPNConnections'
    { _dvpncFilters          :: !(Maybe [Filter])
    , _dvpncVPNConnectionIds :: !(Maybe [Text])
    , _dvpncDryRun           :: !(Maybe Bool)
    } deriving (Eq,Read,Show)

-- | 'DescribeVPNConnections' smart constructor.
describeVPNConnections :: DescribeVPNConnections
describeVPNConnections =
    DescribeVPNConnections'
    { _dvpncFilters = Nothing
    , _dvpncVPNConnectionIds = Nothing
    , _dvpncDryRun = Nothing
    }

-- | One or more filters.
--
-- -   @customer-gateway-configuration@ - The configuration information for
--     the customer gateway.
--
-- -   @customer-gateway-id@ - The ID of a customer gateway associated with
--     the VPN connection.
--
-- -   @state@ - The state of the VPN connection (@pending@ | @available@ |
--     @deleting@ | @deleted@).
--
-- -   @option.static-routes-only@ - Indicates whether the connection has
--     static routes only. Used for devices that do not support Border
--     Gateway Protocol (BGP).
--
-- -   @route.destination-cidr-block@ - The destination CIDR block. This
--     corresponds to the subnet used in a customer data center.
--
-- -   @bgp-asn@ - The BGP Autonomous System Number (ASN) associated with a
--     BGP device.
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
-- -   @type@ - The type of VPN connection. Currently the only supported
--     type is @ipsec.1@.
--
-- -   @vpn-connection-id@ - The ID of the VPN connection.
--
-- -   @vpn-gateway-id@ - The ID of a virtual private gateway associated
--     with the VPN connection.
--
dvpncFilters :: Lens' DescribeVPNConnections [Filter]
dvpncFilters = lens _dvpncFilters (\ s a -> s{_dvpncFilters = a}) . _Default;

-- | One or more VPN connection IDs.
--
-- Default: Describes your VPN connections.
dvpncVPNConnectionIds :: Lens' DescribeVPNConnections [Text]
dvpncVPNConnectionIds = lens _dvpncVPNConnectionIds (\ s a -> s{_dvpncVPNConnectionIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvpncDryRun :: Lens' DescribeVPNConnections (Maybe Bool)
dvpncDryRun = lens _dvpncDryRun (\ s a -> s{_dvpncDryRun = a});

instance AWSRequest DescribeVPNConnections where
        type Sv DescribeVPNConnections = EC2
        type Rs DescribeVPNConnections =
             DescribeVPNConnectionsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPNConnectionsResponse' <$>
                   (may (parseXMLList "item") x) <*> (pure s))

instance ToHeaders DescribeVPNConnections where
        toHeaders = const mempty

instance ToPath DescribeVPNConnections where
        toPath = const "/"

instance ToQuery DescribeVPNConnections where
        toQuery DescribeVPNConnections'{..}
          = mconcat
              ["Action" =:
                 ("DescribeVPNConnections" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvpncFilters),
               toQuery
                 (toQueryList "VpnConnectionId" <$>
                    _dvpncVPNConnectionIds),
               "DryRun" =: _dvpncDryRun]

-- | /See:/ 'describeVPNConnectionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvcrVPNConnections'
--
-- * 'dvcrStatus'
data DescribeVPNConnectionsResponse = DescribeVPNConnectionsResponse'
    { _dvcrVPNConnections :: !(Maybe [VPNConnection])
    , _dvcrStatus         :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeVPNConnectionsResponse' smart constructor.
describeVPNConnectionsResponse :: Status -> DescribeVPNConnectionsResponse
describeVPNConnectionsResponse pStatus =
    DescribeVPNConnectionsResponse'
    { _dvcrVPNConnections = Nothing
    , _dvcrStatus = pStatus
    }

-- | Information about one or more VPN connections.
dvcrVPNConnections :: Lens' DescribeVPNConnectionsResponse [VPNConnection]
dvcrVPNConnections = lens _dvcrVPNConnections (\ s a -> s{_dvcrVPNConnections = a}) . _Default;

-- | FIXME: Undocumented member.
dvcrStatus :: Lens' DescribeVPNConnectionsResponse Status
dvcrStatus = lens _dvcrStatus (\ s a -> s{_dvcrStatus = a});
