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
-- Module      : Amazonka.EC2.DescribeCustomerGateways
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPN customer gateways.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html Amazon Web Services Site-to-Site VPN>
-- in the /Amazon Web Services Site-to-Site VPN User Guide/.
module Amazonka.EC2.DescribeCustomerGateways
  ( -- * Creating a Request
    DescribeCustomerGateways (..),
    newDescribeCustomerGateways,

    -- * Request Lenses
    describeCustomerGateways_customerGatewayIds,
    describeCustomerGateways_dryRun,
    describeCustomerGateways_filters,

    -- * Destructuring the Response
    DescribeCustomerGatewaysResponse (..),
    newDescribeCustomerGatewaysResponse,

    -- * Response Lenses
    describeCustomerGatewaysResponse_customerGateways,
    describeCustomerGatewaysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeCustomerGateways.
--
-- /See:/ 'newDescribeCustomerGateways' smart constructor.
data DescribeCustomerGateways = DescribeCustomerGateways'
  { -- | One or more customer gateway IDs.
    --
    -- Default: Describes all your customer gateways.
    customerGatewayIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @bgp-asn@ - The customer gateway\'s Border Gateway Protocol (BGP)
    --     Autonomous System Number (ASN).
    --
    -- -   @customer-gateway-id@ - The ID of the customer gateway.
    --
    -- -   @ip-address@ - The IP address of the customer gateway device\'s
    --     external interface.
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
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomerGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerGatewayIds', 'describeCustomerGateways_customerGatewayIds' - One or more customer gateway IDs.
--
-- Default: Describes all your customer gateways.
--
-- 'dryRun', 'describeCustomerGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeCustomerGateways_filters' - One or more filters.
--
-- -   @bgp-asn@ - The customer gateway\'s Border Gateway Protocol (BGP)
--     Autonomous System Number (ASN).
--
-- -   @customer-gateway-id@ - The ID of the customer gateway.
--
-- -   @ip-address@ - The IP address of the customer gateway device\'s
--     external interface.
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
    { customerGatewayIds =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | One or more customer gateway IDs.
--
-- Default: Describes all your customer gateways.
describeCustomerGateways_customerGatewayIds :: Lens.Lens' DescribeCustomerGateways (Prelude.Maybe [Prelude.Text])
describeCustomerGateways_customerGatewayIds = Lens.lens (\DescribeCustomerGateways' {customerGatewayIds} -> customerGatewayIds) (\s@DescribeCustomerGateways' {} a -> s {customerGatewayIds = a} :: DescribeCustomerGateways) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeCustomerGateways_dryRun :: Lens.Lens' DescribeCustomerGateways (Prelude.Maybe Prelude.Bool)
describeCustomerGateways_dryRun = Lens.lens (\DescribeCustomerGateways' {dryRun} -> dryRun) (\s@DescribeCustomerGateways' {} a -> s {dryRun = a} :: DescribeCustomerGateways)

-- | One or more filters.
--
-- -   @bgp-asn@ - The customer gateway\'s Border Gateway Protocol (BGP)
--     Autonomous System Number (ASN).
--
-- -   @customer-gateway-id@ - The ID of the customer gateway.
--
-- -   @ip-address@ - The IP address of the customer gateway device\'s
--     external interface.
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
describeCustomerGateways_filters :: Lens.Lens' DescribeCustomerGateways (Prelude.Maybe [Filter])
describeCustomerGateways_filters = Lens.lens (\DescribeCustomerGateways' {filters} -> filters) (\s@DescribeCustomerGateways' {} a -> s {filters = a} :: DescribeCustomerGateways) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeCustomerGateways where
  type
    AWSResponse DescribeCustomerGateways =
      DescribeCustomerGatewaysResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeCustomerGatewaysResponse'
            Prelude.<$> ( x Data..@? "customerGatewaySet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCustomerGateways where
  hashWithSalt _salt DescribeCustomerGateways' {..} =
    _salt `Prelude.hashWithSalt` customerGatewayIds
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters

instance Prelude.NFData DescribeCustomerGateways where
  rnf DescribeCustomerGateways' {..} =
    Prelude.rnf customerGatewayIds
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters

instance Data.ToHeaders DescribeCustomerGateways where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeCustomerGateways where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCustomerGateways where
  toQuery DescribeCustomerGateways' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeCustomerGateways" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "CustomerGatewayId"
              Prelude.<$> customerGatewayIds
          ),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | Contains the output of DescribeCustomerGateways.
--
-- /See:/ 'newDescribeCustomerGatewaysResponse' smart constructor.
data DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse'
  { -- | Information about one or more customer gateways.
    customerGateways :: Prelude.Maybe [CustomerGateway],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeCustomerGatewaysResponse
newDescribeCustomerGatewaysResponse pHttpStatus_ =
  DescribeCustomerGatewaysResponse'
    { customerGateways =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about one or more customer gateways.
describeCustomerGatewaysResponse_customerGateways :: Lens.Lens' DescribeCustomerGatewaysResponse (Prelude.Maybe [CustomerGateway])
describeCustomerGatewaysResponse_customerGateways = Lens.lens (\DescribeCustomerGatewaysResponse' {customerGateways} -> customerGateways) (\s@DescribeCustomerGatewaysResponse' {} a -> s {customerGateways = a} :: DescribeCustomerGatewaysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeCustomerGatewaysResponse_httpStatus :: Lens.Lens' DescribeCustomerGatewaysResponse Prelude.Int
describeCustomerGatewaysResponse_httpStatus = Lens.lens (\DescribeCustomerGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomerGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeCustomerGatewaysResponse)

instance
  Prelude.NFData
    DescribeCustomerGatewaysResponse
  where
  rnf DescribeCustomerGatewaysResponse' {..} =
    Prelude.rnf customerGateways
      `Prelude.seq` Prelude.rnf httpStatus
