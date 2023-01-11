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
-- Module      : Amazonka.EC2.DescribeTransitGateways
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more transit gateways. By default, all transit gateways
-- are described. Alternatively, you can filter the results.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeTransitGateways
  ( -- * Creating a Request
    DescribeTransitGateways (..),
    newDescribeTransitGateways,

    -- * Request Lenses
    describeTransitGateways_dryRun,
    describeTransitGateways_filters,
    describeTransitGateways_maxResults,
    describeTransitGateways_nextToken,
    describeTransitGateways_transitGatewayIds,

    -- * Destructuring the Response
    DescribeTransitGatewaysResponse (..),
    newDescribeTransitGatewaysResponse,

    -- * Response Lenses
    describeTransitGatewaysResponse_nextToken,
    describeTransitGatewaysResponse_transitGateways,
    describeTransitGatewaysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTransitGateways' smart constructor.
data DescribeTransitGateways = DescribeTransitGateways'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters. The possible values are:
    --
    -- -   @options.propagation-default-route-table-id@ - The ID of the default
    --     propagation route table.
    --
    -- -   @options.amazon-side-asn@ - The private ASN for the Amazon side of a
    --     BGP session.
    --
    -- -   @options.association-default-route-table-id@ - The ID of the default
    --     association route table.
    --
    -- -   @options.auto-accept-shared-attachments@ - Indicates whether there
    --     is automatic acceptance of attachment requests (@enable@ |
    --     @disable@).
    --
    -- -   @options.default-route-table-association@ - Indicates whether
    --     resource attachments are automatically associated with the default
    --     association route table (@enable@ | @disable@).
    --
    -- -   @options.default-route-table-propagation@ - Indicates whether
    --     resource attachments automatically propagate routes to the default
    --     propagation route table (@enable@ | @disable@).
    --
    -- -   @options.dns-support@ - Indicates whether DNS support is enabled
    --     (@enable@ | @disable@).
    --
    -- -   @options.vpn-ecmp-support@ - Indicates whether Equal Cost Multipath
    --     Protocol support is enabled (@enable@ | @disable@).
    --
    -- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
    --     transit gateway.
    --
    -- -   @state@ - The state of the transit gateway (@available@ | @deleted@
    --     | @deleting@ | @modifying@ | @pending@).
    --
    -- -   @transit-gateway-id@ - The ID of the transit gateway.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the transit gateways.
    transitGatewayIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeTransitGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeTransitGateways_filters' - One or more filters. The possible values are:
--
-- -   @options.propagation-default-route-table-id@ - The ID of the default
--     propagation route table.
--
-- -   @options.amazon-side-asn@ - The private ASN for the Amazon side of a
--     BGP session.
--
-- -   @options.association-default-route-table-id@ - The ID of the default
--     association route table.
--
-- -   @options.auto-accept-shared-attachments@ - Indicates whether there
--     is automatic acceptance of attachment requests (@enable@ |
--     @disable@).
--
-- -   @options.default-route-table-association@ - Indicates whether
--     resource attachments are automatically associated with the default
--     association route table (@enable@ | @disable@).
--
-- -   @options.default-route-table-propagation@ - Indicates whether
--     resource attachments automatically propagate routes to the default
--     propagation route table (@enable@ | @disable@).
--
-- -   @options.dns-support@ - Indicates whether DNS support is enabled
--     (@enable@ | @disable@).
--
-- -   @options.vpn-ecmp-support@ - Indicates whether Equal Cost Multipath
--     Protocol support is enabled (@enable@ | @disable@).
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     transit gateway.
--
-- -   @state@ - The state of the transit gateway (@available@ | @deleted@
--     | @deleting@ | @modifying@ | @pending@).
--
-- -   @transit-gateway-id@ - The ID of the transit gateway.
--
-- 'maxResults', 'describeTransitGateways_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeTransitGateways_nextToken' - The token for the next page of results.
--
-- 'transitGatewayIds', 'describeTransitGateways_transitGatewayIds' - The IDs of the transit gateways.
newDescribeTransitGateways ::
  DescribeTransitGateways
newDescribeTransitGateways =
  DescribeTransitGateways'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      transitGatewayIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTransitGateways_dryRun :: Lens.Lens' DescribeTransitGateways (Prelude.Maybe Prelude.Bool)
describeTransitGateways_dryRun = Lens.lens (\DescribeTransitGateways' {dryRun} -> dryRun) (\s@DescribeTransitGateways' {} a -> s {dryRun = a} :: DescribeTransitGateways)

-- | One or more filters. The possible values are:
--
-- -   @options.propagation-default-route-table-id@ - The ID of the default
--     propagation route table.
--
-- -   @options.amazon-side-asn@ - The private ASN for the Amazon side of a
--     BGP session.
--
-- -   @options.association-default-route-table-id@ - The ID of the default
--     association route table.
--
-- -   @options.auto-accept-shared-attachments@ - Indicates whether there
--     is automatic acceptance of attachment requests (@enable@ |
--     @disable@).
--
-- -   @options.default-route-table-association@ - Indicates whether
--     resource attachments are automatically associated with the default
--     association route table (@enable@ | @disable@).
--
-- -   @options.default-route-table-propagation@ - Indicates whether
--     resource attachments automatically propagate routes to the default
--     propagation route table (@enable@ | @disable@).
--
-- -   @options.dns-support@ - Indicates whether DNS support is enabled
--     (@enable@ | @disable@).
--
-- -   @options.vpn-ecmp-support@ - Indicates whether Equal Cost Multipath
--     Protocol support is enabled (@enable@ | @disable@).
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     transit gateway.
--
-- -   @state@ - The state of the transit gateway (@available@ | @deleted@
--     | @deleting@ | @modifying@ | @pending@).
--
-- -   @transit-gateway-id@ - The ID of the transit gateway.
describeTransitGateways_filters :: Lens.Lens' DescribeTransitGateways (Prelude.Maybe [Filter])
describeTransitGateways_filters = Lens.lens (\DescribeTransitGateways' {filters} -> filters) (\s@DescribeTransitGateways' {} a -> s {filters = a} :: DescribeTransitGateways) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTransitGateways_maxResults :: Lens.Lens' DescribeTransitGateways (Prelude.Maybe Prelude.Natural)
describeTransitGateways_maxResults = Lens.lens (\DescribeTransitGateways' {maxResults} -> maxResults) (\s@DescribeTransitGateways' {} a -> s {maxResults = a} :: DescribeTransitGateways)

-- | The token for the next page of results.
describeTransitGateways_nextToken :: Lens.Lens' DescribeTransitGateways (Prelude.Maybe Prelude.Text)
describeTransitGateways_nextToken = Lens.lens (\DescribeTransitGateways' {nextToken} -> nextToken) (\s@DescribeTransitGateways' {} a -> s {nextToken = a} :: DescribeTransitGateways)

-- | The IDs of the transit gateways.
describeTransitGateways_transitGatewayIds :: Lens.Lens' DescribeTransitGateways (Prelude.Maybe [Prelude.Text])
describeTransitGateways_transitGatewayIds = Lens.lens (\DescribeTransitGateways' {transitGatewayIds} -> transitGatewayIds) (\s@DescribeTransitGateways' {} a -> s {transitGatewayIds = a} :: DescribeTransitGateways) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeTransitGateways where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewaysResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewaysResponse_transitGateways
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTransitGateways_nextToken
          Lens..~ rs
          Lens.^? describeTransitGatewaysResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeTransitGateways where
  type
    AWSResponse DescribeTransitGateways =
      DescribeTransitGatewaysResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewaysResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "transitGatewaySet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTransitGateways where
  hashWithSalt _salt DescribeTransitGateways' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` transitGatewayIds

instance Prelude.NFData DescribeTransitGateways where
  rnf DescribeTransitGateways' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf transitGatewayIds

instance Data.ToHeaders DescribeTransitGateways where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeTransitGateways where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTransitGateways where
  toQuery DescribeTransitGateways' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeTransitGateways" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "TransitGatewayIds"
              Prelude.<$> transitGatewayIds
          )
      ]

-- | /See:/ 'newDescribeTransitGatewaysResponse' smart constructor.
data DescribeTransitGatewaysResponse = DescribeTransitGatewaysResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the transit gateways.
    transitGateways :: Prelude.Maybe [TransitGateway],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewaysResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'transitGateways', 'describeTransitGatewaysResponse_transitGateways' - Information about the transit gateways.
--
-- 'httpStatus', 'describeTransitGatewaysResponse_httpStatus' - The response's http status code.
newDescribeTransitGatewaysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTransitGatewaysResponse
newDescribeTransitGatewaysResponse pHttpStatus_ =
  DescribeTransitGatewaysResponse'
    { nextToken =
        Prelude.Nothing,
      transitGateways = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeTransitGatewaysResponse_nextToken :: Lens.Lens' DescribeTransitGatewaysResponse (Prelude.Maybe Prelude.Text)
describeTransitGatewaysResponse_nextToken = Lens.lens (\DescribeTransitGatewaysResponse' {nextToken} -> nextToken) (\s@DescribeTransitGatewaysResponse' {} a -> s {nextToken = a} :: DescribeTransitGatewaysResponse)

-- | Information about the transit gateways.
describeTransitGatewaysResponse_transitGateways :: Lens.Lens' DescribeTransitGatewaysResponse (Prelude.Maybe [TransitGateway])
describeTransitGatewaysResponse_transitGateways = Lens.lens (\DescribeTransitGatewaysResponse' {transitGateways} -> transitGateways) (\s@DescribeTransitGatewaysResponse' {} a -> s {transitGateways = a} :: DescribeTransitGatewaysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTransitGatewaysResponse_httpStatus :: Lens.Lens' DescribeTransitGatewaysResponse Prelude.Int
describeTransitGatewaysResponse_httpStatus = Lens.lens (\DescribeTransitGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewaysResponse)

instance
  Prelude.NFData
    DescribeTransitGatewaysResponse
  where
  rnf DescribeTransitGatewaysResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf transitGateways
      `Prelude.seq` Prelude.rnf httpStatus
