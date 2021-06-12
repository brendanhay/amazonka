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
-- Module      : Network.AWS.EC2.DescribeCustomerGateways
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPN customer gateways.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN>
-- in the /AWS Site-to-Site VPN User Guide/.
module Network.AWS.EC2.DescribeCustomerGateways
  ( -- * Creating a Request
    DescribeCustomerGateways (..),
    newDescribeCustomerGateways,

    -- * Request Lenses
    describeCustomerGateways_dryRun,
    describeCustomerGateways_customerGatewayIds,
    describeCustomerGateways_filters,

    -- * Destructuring the Response
    DescribeCustomerGatewaysResponse (..),
    newDescribeCustomerGatewaysResponse,

    -- * Response Lenses
    describeCustomerGatewaysResponse_customerGateways,
    describeCustomerGatewaysResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeCustomerGateways.
--
-- /See:/ 'newDescribeCustomerGateways' smart constructor.
data DescribeCustomerGateways = DescribeCustomerGateways'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more customer gateway IDs.
    --
    -- Default: Describes all your customer gateways.
    customerGatewayIds :: Core.Maybe [Core.Text],
    -- | One or more filters.
    --
    -- -   @bgp-asn@ - The customer gateway\'s Border Gateway Protocol (BGP)
    --     Autonomous System Number (ASN).
    --
    -- -   @customer-gateway-id@ - The ID of the customer gateway.
    --
    -- -   @ip-address@ - The IP address of the customer gateway\'s
    --     Internet-routable external interface.
    --
    -- -   @state@ - The state of the customer gateway (@pending@ | @available@
    --     | @deleting@ | @deleted@).
    --
    -- -   @type@ - The type of customer gateway. Currently, the only supported
    --     type is @ipsec.1@.
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
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCustomerGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeCustomerGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'customerGatewayIds', 'describeCustomerGateways_customerGatewayIds' - One or more customer gateway IDs.
--
-- Default: Describes all your customer gateways.
--
-- 'filters', 'describeCustomerGateways_filters' - One or more filters.
--
-- -   @bgp-asn@ - The customer gateway\'s Border Gateway Protocol (BGP)
--     Autonomous System Number (ASN).
--
-- -   @customer-gateway-id@ - The ID of the customer gateway.
--
-- -   @ip-address@ - The IP address of the customer gateway\'s
--     Internet-routable external interface.
--
-- -   @state@ - The state of the customer gateway (@pending@ | @available@
--     | @deleting@ | @deleted@).
--
-- -   @type@ - The type of customer gateway. Currently, the only supported
--     type is @ipsec.1@.
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
newDescribeCustomerGateways ::
  DescribeCustomerGateways
newDescribeCustomerGateways =
  DescribeCustomerGateways'
    { dryRun = Core.Nothing,
      customerGatewayIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeCustomerGateways_dryRun :: Lens.Lens' DescribeCustomerGateways (Core.Maybe Core.Bool)
describeCustomerGateways_dryRun = Lens.lens (\DescribeCustomerGateways' {dryRun} -> dryRun) (\s@DescribeCustomerGateways' {} a -> s {dryRun = a} :: DescribeCustomerGateways)

-- | One or more customer gateway IDs.
--
-- Default: Describes all your customer gateways.
describeCustomerGateways_customerGatewayIds :: Lens.Lens' DescribeCustomerGateways (Core.Maybe [Core.Text])
describeCustomerGateways_customerGatewayIds = Lens.lens (\DescribeCustomerGateways' {customerGatewayIds} -> customerGatewayIds) (\s@DescribeCustomerGateways' {} a -> s {customerGatewayIds = a} :: DescribeCustomerGateways) Core.. Lens.mapping Lens._Coerce

-- | One or more filters.
--
-- -   @bgp-asn@ - The customer gateway\'s Border Gateway Protocol (BGP)
--     Autonomous System Number (ASN).
--
-- -   @customer-gateway-id@ - The ID of the customer gateway.
--
-- -   @ip-address@ - The IP address of the customer gateway\'s
--     Internet-routable external interface.
--
-- -   @state@ - The state of the customer gateway (@pending@ | @available@
--     | @deleting@ | @deleted@).
--
-- -   @type@ - The type of customer gateway. Currently, the only supported
--     type is @ipsec.1@.
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
describeCustomerGateways_filters :: Lens.Lens' DescribeCustomerGateways (Core.Maybe [Filter])
describeCustomerGateways_filters = Lens.lens (\DescribeCustomerGateways' {filters} -> filters) (\s@DescribeCustomerGateways' {} a -> s {filters = a} :: DescribeCustomerGateways) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeCustomerGateways where
  type
    AWSResponse DescribeCustomerGateways =
      DescribeCustomerGatewaysResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeCustomerGatewaysResponse'
            Core.<$> ( x Core..@? "customerGatewaySet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCustomerGateways

instance Core.NFData DescribeCustomerGateways

instance Core.ToHeaders DescribeCustomerGateways where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeCustomerGateways where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCustomerGateways where
  toQuery DescribeCustomerGateways' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeCustomerGateways" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "CustomerGatewayId"
              Core.<$> customerGatewayIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | Contains the output of DescribeCustomerGateways.
--
-- /See:/ 'newDescribeCustomerGatewaysResponse' smart constructor.
data DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse'
  { -- | Information about one or more customer gateways.
    customerGateways :: Core.Maybe [CustomerGateway],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCustomerGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerGateways', 'describeCustomerGatewaysResponse_customerGateways' - Information about one or more customer gateways.
--
-- 'httpStatus', 'describeCustomerGatewaysResponse_httpStatus' - The response's http status code.
newDescribeCustomerGatewaysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCustomerGatewaysResponse
newDescribeCustomerGatewaysResponse pHttpStatus_ =
  DescribeCustomerGatewaysResponse'
    { customerGateways =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about one or more customer gateways.
describeCustomerGatewaysResponse_customerGateways :: Lens.Lens' DescribeCustomerGatewaysResponse (Core.Maybe [CustomerGateway])
describeCustomerGatewaysResponse_customerGateways = Lens.lens (\DescribeCustomerGatewaysResponse' {customerGateways} -> customerGateways) (\s@DescribeCustomerGatewaysResponse' {} a -> s {customerGateways = a} :: DescribeCustomerGatewaysResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeCustomerGatewaysResponse_httpStatus :: Lens.Lens' DescribeCustomerGatewaysResponse Core.Int
describeCustomerGatewaysResponse_httpStatus = Lens.lens (\DescribeCustomerGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomerGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeCustomerGatewaysResponse)

instance Core.NFData DescribeCustomerGatewaysResponse
