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
-- Module      : Network.AWS.EC2.DescribeVpnGateways
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your virtual private gateways.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN>
-- in the /AWS Site-to-Site VPN User Guide/.
module Network.AWS.EC2.DescribeVpnGateways
  ( -- * Creating a Request
    DescribeVpnGateways (..),
    newDescribeVpnGateways,

    -- * Request Lenses
    describeVpnGateways_dryRun,
    describeVpnGateways_vpnGatewayIds,
    describeVpnGateways_filters,

    -- * Destructuring the Response
    DescribeVpnGatewaysResponse (..),
    newDescribeVpnGatewaysResponse,

    -- * Response Lenses
    describeVpnGatewaysResponse_vpnGateways,
    describeVpnGatewaysResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeVpnGateways.
--
-- /See:/ 'newDescribeVpnGateways' smart constructor.
data DescribeVpnGateways = DescribeVpnGateways'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more virtual private gateway IDs.
    --
    -- Default: Describes all your virtual private gateways.
    vpnGatewayIds :: Core.Maybe [Core.Text],
    -- | One or more filters.
    --
    -- -   @amazon-side-asn@ - The Autonomous System Number (ASN) for the
    --     Amazon side of the gateway.
    --
    -- -   @attachment.state@ - The current state of the attachment between the
    --     gateway and the VPC (@attaching@ | @attached@ | @detaching@ |
    --     @detached@).
    --
    -- -   @attachment.vpc-id@ - The ID of an attached VPC.
    --
    -- -   @availability-zone@ - The Availability Zone for the virtual private
    --     gateway (if applicable).
    --
    -- -   @state@ - The state of the virtual private gateway (@pending@ |
    --     @available@ | @deleting@ | @deleted@).
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
    -- -   @type@ - The type of virtual private gateway. Currently the only
    --     supported type is @ipsec.1@.
    --
    -- -   @vpn-gateway-id@ - The ID of the virtual private gateway.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpnGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVpnGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpnGatewayIds', 'describeVpnGateways_vpnGatewayIds' - One or more virtual private gateway IDs.
--
-- Default: Describes all your virtual private gateways.
--
-- 'filters', 'describeVpnGateways_filters' - One or more filters.
--
-- -   @amazon-side-asn@ - The Autonomous System Number (ASN) for the
--     Amazon side of the gateway.
--
-- -   @attachment.state@ - The current state of the attachment between the
--     gateway and the VPC (@attaching@ | @attached@ | @detaching@ |
--     @detached@).
--
-- -   @attachment.vpc-id@ - The ID of an attached VPC.
--
-- -   @availability-zone@ - The Availability Zone for the virtual private
--     gateway (if applicable).
--
-- -   @state@ - The state of the virtual private gateway (@pending@ |
--     @available@ | @deleting@ | @deleted@).
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
-- -   @type@ - The type of virtual private gateway. Currently the only
--     supported type is @ipsec.1@.
--
-- -   @vpn-gateway-id@ - The ID of the virtual private gateway.
newDescribeVpnGateways ::
  DescribeVpnGateways
newDescribeVpnGateways =
  DescribeVpnGateways'
    { dryRun = Core.Nothing,
      vpnGatewayIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpnGateways_dryRun :: Lens.Lens' DescribeVpnGateways (Core.Maybe Core.Bool)
describeVpnGateways_dryRun = Lens.lens (\DescribeVpnGateways' {dryRun} -> dryRun) (\s@DescribeVpnGateways' {} a -> s {dryRun = a} :: DescribeVpnGateways)

-- | One or more virtual private gateway IDs.
--
-- Default: Describes all your virtual private gateways.
describeVpnGateways_vpnGatewayIds :: Lens.Lens' DescribeVpnGateways (Core.Maybe [Core.Text])
describeVpnGateways_vpnGatewayIds = Lens.lens (\DescribeVpnGateways' {vpnGatewayIds} -> vpnGatewayIds) (\s@DescribeVpnGateways' {} a -> s {vpnGatewayIds = a} :: DescribeVpnGateways) Core.. Lens.mapping Lens._Coerce

-- | One or more filters.
--
-- -   @amazon-side-asn@ - The Autonomous System Number (ASN) for the
--     Amazon side of the gateway.
--
-- -   @attachment.state@ - The current state of the attachment between the
--     gateway and the VPC (@attaching@ | @attached@ | @detaching@ |
--     @detached@).
--
-- -   @attachment.vpc-id@ - The ID of an attached VPC.
--
-- -   @availability-zone@ - The Availability Zone for the virtual private
--     gateway (if applicable).
--
-- -   @state@ - The state of the virtual private gateway (@pending@ |
--     @available@ | @deleting@ | @deleted@).
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
-- -   @type@ - The type of virtual private gateway. Currently the only
--     supported type is @ipsec.1@.
--
-- -   @vpn-gateway-id@ - The ID of the virtual private gateway.
describeVpnGateways_filters :: Lens.Lens' DescribeVpnGateways (Core.Maybe [Filter])
describeVpnGateways_filters = Lens.lens (\DescribeVpnGateways' {filters} -> filters) (\s@DescribeVpnGateways' {} a -> s {filters = a} :: DescribeVpnGateways) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeVpnGateways where
  type
    AWSResponse DescribeVpnGateways =
      DescribeVpnGatewaysResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpnGatewaysResponse'
            Core.<$> ( x Core..@? "vpnGatewaySet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeVpnGateways

instance Core.NFData DescribeVpnGateways

instance Core.ToHeaders DescribeVpnGateways where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeVpnGateways where
  toPath = Core.const "/"

instance Core.ToQuery DescribeVpnGateways where
  toQuery DescribeVpnGateways' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeVpnGateways" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "VpnGatewayId"
              Core.<$> vpnGatewayIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | Contains the output of DescribeVpnGateways.
--
-- /See:/ 'newDescribeVpnGatewaysResponse' smart constructor.
data DescribeVpnGatewaysResponse = DescribeVpnGatewaysResponse'
  { -- | Information about one or more virtual private gateways.
    vpnGateways :: Core.Maybe [VpnGateway],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpnGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnGateways', 'describeVpnGatewaysResponse_vpnGateways' - Information about one or more virtual private gateways.
--
-- 'httpStatus', 'describeVpnGatewaysResponse_httpStatus' - The response's http status code.
newDescribeVpnGatewaysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVpnGatewaysResponse
newDescribeVpnGatewaysResponse pHttpStatus_ =
  DescribeVpnGatewaysResponse'
    { vpnGateways =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about one or more virtual private gateways.
describeVpnGatewaysResponse_vpnGateways :: Lens.Lens' DescribeVpnGatewaysResponse (Core.Maybe [VpnGateway])
describeVpnGatewaysResponse_vpnGateways = Lens.lens (\DescribeVpnGatewaysResponse' {vpnGateways} -> vpnGateways) (\s@DescribeVpnGatewaysResponse' {} a -> s {vpnGateways = a} :: DescribeVpnGatewaysResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVpnGatewaysResponse_httpStatus :: Lens.Lens' DescribeVpnGatewaysResponse Core.Int
describeVpnGatewaysResponse_httpStatus = Lens.lens (\DescribeVpnGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeVpnGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeVpnGatewaysResponse)

instance Core.NFData DescribeVpnGatewaysResponse
