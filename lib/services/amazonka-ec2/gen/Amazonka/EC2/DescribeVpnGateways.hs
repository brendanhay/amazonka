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
-- Module      : Amazonka.EC2.DescribeVpnGateways
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your virtual private gateways.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html Amazon Web Services Site-to-Site VPN>
-- in the /Amazon Web Services Site-to-Site VPN User Guide/.
module Amazonka.EC2.DescribeVpnGateways
  ( -- * Creating a Request
    DescribeVpnGateways (..),
    newDescribeVpnGateways,

    -- * Request Lenses
    describeVpnGateways_dryRun,
    describeVpnGateways_filters,
    describeVpnGateways_vpnGatewayIds,

    -- * Destructuring the Response
    DescribeVpnGatewaysResponse (..),
    newDescribeVpnGatewaysResponse,

    -- * Response Lenses
    describeVpnGatewaysResponse_vpnGateways,
    describeVpnGatewaysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeVpnGateways.
--
-- /See:/ 'newDescribeVpnGateways' smart constructor.
data DescribeVpnGateways = DescribeVpnGateways'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
    filters :: Prelude.Maybe [Filter],
    -- | One or more virtual private gateway IDs.
    --
    -- Default: Describes all your virtual private gateways.
    vpnGatewayIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
--
-- 'vpnGatewayIds', 'describeVpnGateways_vpnGatewayIds' - One or more virtual private gateway IDs.
--
-- Default: Describes all your virtual private gateways.
newDescribeVpnGateways ::
  DescribeVpnGateways
newDescribeVpnGateways =
  DescribeVpnGateways'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      vpnGatewayIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpnGateways_dryRun :: Lens.Lens' DescribeVpnGateways (Prelude.Maybe Prelude.Bool)
describeVpnGateways_dryRun = Lens.lens (\DescribeVpnGateways' {dryRun} -> dryRun) (\s@DescribeVpnGateways' {} a -> s {dryRun = a} :: DescribeVpnGateways)

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
describeVpnGateways_filters :: Lens.Lens' DescribeVpnGateways (Prelude.Maybe [Filter])
describeVpnGateways_filters = Lens.lens (\DescribeVpnGateways' {filters} -> filters) (\s@DescribeVpnGateways' {} a -> s {filters = a} :: DescribeVpnGateways) Prelude.. Lens.mapping Lens.coerced

-- | One or more virtual private gateway IDs.
--
-- Default: Describes all your virtual private gateways.
describeVpnGateways_vpnGatewayIds :: Lens.Lens' DescribeVpnGateways (Prelude.Maybe [Prelude.Text])
describeVpnGateways_vpnGatewayIds = Lens.lens (\DescribeVpnGateways' {vpnGatewayIds} -> vpnGatewayIds) (\s@DescribeVpnGateways' {} a -> s {vpnGatewayIds = a} :: DescribeVpnGateways) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeVpnGateways where
  type
    AWSResponse DescribeVpnGateways =
      DescribeVpnGatewaysResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpnGatewaysResponse'
            Prelude.<$> ( x
                            Data..@? "vpnGatewaySet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeVpnGateways where
  hashWithSalt _salt DescribeVpnGateways' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` vpnGatewayIds

instance Prelude.NFData DescribeVpnGateways where
  rnf DescribeVpnGateways' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf vpnGatewayIds

instance Data.ToHeaders DescribeVpnGateways where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeVpnGateways where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeVpnGateways where
  toQuery DescribeVpnGateways' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeVpnGateways" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          ( Data.toQueryList "VpnGatewayId"
              Prelude.<$> vpnGatewayIds
          )
      ]

-- | Contains the output of DescribeVpnGateways.
--
-- /See:/ 'newDescribeVpnGatewaysResponse' smart constructor.
data DescribeVpnGatewaysResponse = DescribeVpnGatewaysResponse'
  { -- | Information about one or more virtual private gateways.
    vpnGateways :: Prelude.Maybe [VpnGateway],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeVpnGatewaysResponse
newDescribeVpnGatewaysResponse pHttpStatus_ =
  DescribeVpnGatewaysResponse'
    { vpnGateways =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about one or more virtual private gateways.
describeVpnGatewaysResponse_vpnGateways :: Lens.Lens' DescribeVpnGatewaysResponse (Prelude.Maybe [VpnGateway])
describeVpnGatewaysResponse_vpnGateways = Lens.lens (\DescribeVpnGatewaysResponse' {vpnGateways} -> vpnGateways) (\s@DescribeVpnGatewaysResponse' {} a -> s {vpnGateways = a} :: DescribeVpnGatewaysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeVpnGatewaysResponse_httpStatus :: Lens.Lens' DescribeVpnGatewaysResponse Prelude.Int
describeVpnGatewaysResponse_httpStatus = Lens.lens (\DescribeVpnGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeVpnGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeVpnGatewaysResponse)

instance Prelude.NFData DescribeVpnGatewaysResponse where
  rnf DescribeVpnGatewaysResponse' {..} =
    Prelude.rnf vpnGateways
      `Prelude.seq` Prelude.rnf httpStatus
