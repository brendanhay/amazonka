{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.DescribeVpnConnections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPN connections.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html Amazon Web Services Site-to-Site VPN>
-- in the /Amazon Web Services Site-to-Site VPN User Guide/.
module Amazonka.EC2.DescribeVpnConnections
  ( -- * Creating a Request
    DescribeVpnConnections (..),
    newDescribeVpnConnections,

    -- * Request Lenses
    describeVpnConnections_dryRun,
    describeVpnConnections_filters,
    describeVpnConnections_vpnConnectionIds,

    -- * Destructuring the Response
    DescribeVpnConnectionsResponse (..),
    newDescribeVpnConnectionsResponse,

    -- * Response Lenses
    describeVpnConnectionsResponse_vpnConnections,
    describeVpnConnectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeVpnConnections.
--
-- /See:/ 'newDescribeVpnConnections' smart constructor.
data DescribeVpnConnections = DescribeVpnConnections'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    --
    -- -   @type@ - The type of VPN connection. Currently the only supported
    --     type is @ipsec.1@.
    --
    -- -   @vpn-connection-id@ - The ID of the VPN connection.
    --
    -- -   @vpn-gateway-id@ - The ID of a virtual private gateway associated
    --     with the VPN connection.
    --
    -- -   @transit-gateway-id@ - The ID of a transit gateway associated with
    --     the VPN connection.
    filters :: Prelude.Maybe [Filter],
    -- | One or more VPN connection IDs.
    --
    -- Default: Describes your VPN connections.
    vpnConnectionIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpnConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVpnConnections_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeVpnConnections_filters' - One or more filters.
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
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @type@ - The type of VPN connection. Currently the only supported
--     type is @ipsec.1@.
--
-- -   @vpn-connection-id@ - The ID of the VPN connection.
--
-- -   @vpn-gateway-id@ - The ID of a virtual private gateway associated
--     with the VPN connection.
--
-- -   @transit-gateway-id@ - The ID of a transit gateway associated with
--     the VPN connection.
--
-- 'vpnConnectionIds', 'describeVpnConnections_vpnConnectionIds' - One or more VPN connection IDs.
--
-- Default: Describes your VPN connections.
newDescribeVpnConnections ::
  DescribeVpnConnections
newDescribeVpnConnections =
  DescribeVpnConnections'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      vpnConnectionIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpnConnections_dryRun :: Lens.Lens' DescribeVpnConnections (Prelude.Maybe Prelude.Bool)
describeVpnConnections_dryRun = Lens.lens (\DescribeVpnConnections' {dryRun} -> dryRun) (\s@DescribeVpnConnections' {} a -> s {dryRun = a} :: DescribeVpnConnections)

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
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @type@ - The type of VPN connection. Currently the only supported
--     type is @ipsec.1@.
--
-- -   @vpn-connection-id@ - The ID of the VPN connection.
--
-- -   @vpn-gateway-id@ - The ID of a virtual private gateway associated
--     with the VPN connection.
--
-- -   @transit-gateway-id@ - The ID of a transit gateway associated with
--     the VPN connection.
describeVpnConnections_filters :: Lens.Lens' DescribeVpnConnections (Prelude.Maybe [Filter])
describeVpnConnections_filters = Lens.lens (\DescribeVpnConnections' {filters} -> filters) (\s@DescribeVpnConnections' {} a -> s {filters = a} :: DescribeVpnConnections) Prelude.. Lens.mapping Lens.coerced

-- | One or more VPN connection IDs.
--
-- Default: Describes your VPN connections.
describeVpnConnections_vpnConnectionIds :: Lens.Lens' DescribeVpnConnections (Prelude.Maybe [Prelude.Text])
describeVpnConnections_vpnConnectionIds = Lens.lens (\DescribeVpnConnections' {vpnConnectionIds} -> vpnConnectionIds) (\s@DescribeVpnConnections' {} a -> s {vpnConnectionIds = a} :: DescribeVpnConnections) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeVpnConnections where
  type
    AWSResponse DescribeVpnConnections =
      DescribeVpnConnectionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpnConnectionsResponse'
            Prelude.<$> ( x Data..@? "vpnConnectionSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeVpnConnections where
  hashWithSalt _salt DescribeVpnConnections' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` vpnConnectionIds

instance Prelude.NFData DescribeVpnConnections where
  rnf DescribeVpnConnections' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf vpnConnectionIds

instance Data.ToHeaders DescribeVpnConnections where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeVpnConnections where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeVpnConnections where
  toQuery DescribeVpnConnections' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeVpnConnections" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          ( Data.toQueryList "VpnConnectionId"
              Prelude.<$> vpnConnectionIds
          )
      ]

-- | Contains the output of DescribeVpnConnections.
--
-- /See:/ 'newDescribeVpnConnectionsResponse' smart constructor.
data DescribeVpnConnectionsResponse = DescribeVpnConnectionsResponse'
  { -- | Information about one or more VPN connections.
    vpnConnections :: Prelude.Maybe [VpnConnection],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpnConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnConnections', 'describeVpnConnectionsResponse_vpnConnections' - Information about one or more VPN connections.
--
-- 'httpStatus', 'describeVpnConnectionsResponse_httpStatus' - The response's http status code.
newDescribeVpnConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVpnConnectionsResponse
newDescribeVpnConnectionsResponse pHttpStatus_ =
  DescribeVpnConnectionsResponse'
    { vpnConnections =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about one or more VPN connections.
describeVpnConnectionsResponse_vpnConnections :: Lens.Lens' DescribeVpnConnectionsResponse (Prelude.Maybe [VpnConnection])
describeVpnConnectionsResponse_vpnConnections = Lens.lens (\DescribeVpnConnectionsResponse' {vpnConnections} -> vpnConnections) (\s@DescribeVpnConnectionsResponse' {} a -> s {vpnConnections = a} :: DescribeVpnConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeVpnConnectionsResponse_httpStatus :: Lens.Lens' DescribeVpnConnectionsResponse Prelude.Int
describeVpnConnectionsResponse_httpStatus = Lens.lens (\DescribeVpnConnectionsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpnConnectionsResponse' {} a -> s {httpStatus = a} :: DescribeVpnConnectionsResponse)

instance
  Prelude.NFData
    DescribeVpnConnectionsResponse
  where
  rnf DescribeVpnConnectionsResponse' {..} =
    Prelude.rnf vpnConnections
      `Prelude.seq` Prelude.rnf httpStatus
