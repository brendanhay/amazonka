{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPNConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPN connections.
--
--
-- For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.DescribeVPNConnections
  ( -- * Creating a Request
    describeVPNConnections,
    DescribeVPNConnections,

    -- * Request Lenses
    dvpncFilters,
    dvpncVPNConnectionIds,
    dvpncDryRun,

    -- * Destructuring the Response
    describeVPNConnectionsResponse,
    DescribeVPNConnectionsResponse,

    -- * Response Lenses
    dvcrsVPNConnections,
    dvcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeVpnConnections.
--
--
--
-- /See:/ 'describeVPNConnections' smart constructor.
data DescribeVPNConnections = DescribeVPNConnections'
  { _dvpncFilters ::
      !(Maybe [Filter]),
    _dvpncVPNConnectionIds :: !(Maybe [Text]),
    _dvpncDryRun :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeVPNConnections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpncFilters' - One or more filters.     * @customer-gateway-configuration@ - The configuration information for the customer gateway.     * @customer-gateway-id@ - The ID of a customer gateway associated with the VPN connection.     * @state@ - The state of the VPN connection (@pending@ | @available@ | @deleting@ | @deleted@ ).     * @option.static-routes-only@ - Indicates whether the connection has static routes only. Used for devices that do not support Border Gateway Protocol (BGP).     * @route.destination-cidr-block@ - The destination CIDR block. This corresponds to the subnet used in a customer data center.     * @bgp-asn@ - The BGP Autonomous System Number (ASN) associated with a BGP device.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @type@ - The type of VPN connection. Currently the only supported type is @ipsec.1@ .     * @vpn-connection-id@ - The ID of the VPN connection.     * @vpn-gateway-id@ - The ID of a virtual private gateway associated with the VPN connection.     * @transit-gateway-id@ - The ID of a transit gateway associated with the VPN connection.
--
-- * 'dvpncVPNConnectionIds' - One or more VPN connection IDs. Default: Describes your VPN connections.
--
-- * 'dvpncDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describeVPNConnections ::
  DescribeVPNConnections
describeVPNConnections =
  DescribeVPNConnections'
    { _dvpncFilters = Nothing,
      _dvpncVPNConnectionIds = Nothing,
      _dvpncDryRun = Nothing
    }

-- | One or more filters.     * @customer-gateway-configuration@ - The configuration information for the customer gateway.     * @customer-gateway-id@ - The ID of a customer gateway associated with the VPN connection.     * @state@ - The state of the VPN connection (@pending@ | @available@ | @deleting@ | @deleted@ ).     * @option.static-routes-only@ - Indicates whether the connection has static routes only. Used for devices that do not support Border Gateway Protocol (BGP).     * @route.destination-cidr-block@ - The destination CIDR block. This corresponds to the subnet used in a customer data center.     * @bgp-asn@ - The BGP Autonomous System Number (ASN) associated with a BGP device.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @type@ - The type of VPN connection. Currently the only supported type is @ipsec.1@ .     * @vpn-connection-id@ - The ID of the VPN connection.     * @vpn-gateway-id@ - The ID of a virtual private gateway associated with the VPN connection.     * @transit-gateway-id@ - The ID of a transit gateway associated with the VPN connection.
dvpncFilters :: Lens' DescribeVPNConnections [Filter]
dvpncFilters = lens _dvpncFilters (\s a -> s {_dvpncFilters = a}) . _Default . _Coerce

-- | One or more VPN connection IDs. Default: Describes your VPN connections.
dvpncVPNConnectionIds :: Lens' DescribeVPNConnections [Text]
dvpncVPNConnectionIds = lens _dvpncVPNConnectionIds (\s a -> s {_dvpncVPNConnectionIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvpncDryRun :: Lens' DescribeVPNConnections (Maybe Bool)
dvpncDryRun = lens _dvpncDryRun (\s a -> s {_dvpncDryRun = a})

instance AWSRequest DescribeVPNConnections where
  type Rs DescribeVPNConnections = DescribeVPNConnectionsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeVPNConnectionsResponse'
            <$> (x .@? "vpnConnectionSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeVPNConnections

instance NFData DescribeVPNConnections

instance ToHeaders DescribeVPNConnections where
  toHeaders = const mempty

instance ToPath DescribeVPNConnections where
  toPath = const "/"

instance ToQuery DescribeVPNConnections where
  toQuery DescribeVPNConnections' {..} =
    mconcat
      [ "Action" =: ("DescribeVpnConnections" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dvpncFilters),
        toQuery (toQueryList "VpnConnectionId" <$> _dvpncVPNConnectionIds),
        "DryRun" =: _dvpncDryRun
      ]

-- | Contains the output of DescribeVpnConnections.
--
--
--
-- /See:/ 'describeVPNConnectionsResponse' smart constructor.
data DescribeVPNConnectionsResponse = DescribeVPNConnectionsResponse'
  { _dvcrsVPNConnections ::
      !(Maybe [VPNConnection]),
    _dvcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeVPNConnectionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvcrsVPNConnections' - Information about one or more VPN connections.
--
-- * 'dvcrsResponseStatus' - -- | The response status code.
describeVPNConnectionsResponse ::
  -- | 'dvcrsResponseStatus'
  Int ->
  DescribeVPNConnectionsResponse
describeVPNConnectionsResponse pResponseStatus_ =
  DescribeVPNConnectionsResponse'
    { _dvcrsVPNConnections = Nothing,
      _dvcrsResponseStatus = pResponseStatus_
    }

-- | Information about one or more VPN connections.
dvcrsVPNConnections :: Lens' DescribeVPNConnectionsResponse [VPNConnection]
dvcrsVPNConnections = lens _dvcrsVPNConnections (\s a -> s {_dvcrsVPNConnections = a}) . _Default . _Coerce

-- | -- | The response status code.
dvcrsResponseStatus :: Lens' DescribeVPNConnectionsResponse Int
dvcrsResponseStatus = lens _dvcrsResponseStatus (\s a -> s {_dvcrsResponseStatus = a})

instance NFData DescribeVPNConnectionsResponse
