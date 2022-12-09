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
-- Module      : Amazonka.EC2.DescribeNatGateways
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your NAT gateways.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeNatGateways
  ( -- * Creating a Request
    DescribeNatGateways (..),
    newDescribeNatGateways,

    -- * Request Lenses
    describeNatGateways_dryRun,
    describeNatGateways_filter,
    describeNatGateways_maxResults,
    describeNatGateways_natGatewayIds,
    describeNatGateways_nextToken,

    -- * Destructuring the Response
    DescribeNatGatewaysResponse (..),
    newDescribeNatGatewaysResponse,

    -- * Response Lenses
    describeNatGatewaysResponse_natGateways,
    describeNatGatewaysResponse_nextToken,
    describeNatGatewaysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeNatGateways' smart constructor.
data DescribeNatGateways = DescribeNatGateways'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @nat-gateway-id@ - The ID of the NAT gateway.
    --
    -- -   @state@ - The state of the NAT gateway (@pending@ | @failed@ |
    --     @available@ | @deleting@ | @deleted@).
    --
    -- -   @subnet-id@ - The ID of the subnet in which the NAT gateway resides.
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
    -- -   @vpc-id@ - The ID of the VPC in which the NAT gateway resides.
    filter' :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | One or more NAT gateway IDs.
    natGatewayIds :: Prelude.Maybe [Prelude.Text],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNatGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeNatGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filter'', 'describeNatGateways_filter' - One or more filters.
--
-- -   @nat-gateway-id@ - The ID of the NAT gateway.
--
-- -   @state@ - The state of the NAT gateway (@pending@ | @failed@ |
--     @available@ | @deleting@ | @deleted@).
--
-- -   @subnet-id@ - The ID of the subnet in which the NAT gateway resides.
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
-- -   @vpc-id@ - The ID of the VPC in which the NAT gateway resides.
--
-- 'maxResults', 'describeNatGateways_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'natGatewayIds', 'describeNatGateways_natGatewayIds' - One or more NAT gateway IDs.
--
-- 'nextToken', 'describeNatGateways_nextToken' - The token for the next page of results.
newDescribeNatGateways ::
  DescribeNatGateways
newDescribeNatGateways =
  DescribeNatGateways'
    { dryRun = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      natGatewayIds = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeNatGateways_dryRun :: Lens.Lens' DescribeNatGateways (Prelude.Maybe Prelude.Bool)
describeNatGateways_dryRun = Lens.lens (\DescribeNatGateways' {dryRun} -> dryRun) (\s@DescribeNatGateways' {} a -> s {dryRun = a} :: DescribeNatGateways)

-- | One or more filters.
--
-- -   @nat-gateway-id@ - The ID of the NAT gateway.
--
-- -   @state@ - The state of the NAT gateway (@pending@ | @failed@ |
--     @available@ | @deleting@ | @deleted@).
--
-- -   @subnet-id@ - The ID of the subnet in which the NAT gateway resides.
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
-- -   @vpc-id@ - The ID of the VPC in which the NAT gateway resides.
describeNatGateways_filter :: Lens.Lens' DescribeNatGateways (Prelude.Maybe [Filter])
describeNatGateways_filter = Lens.lens (\DescribeNatGateways' {filter'} -> filter') (\s@DescribeNatGateways' {} a -> s {filter' = a} :: DescribeNatGateways) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeNatGateways_maxResults :: Lens.Lens' DescribeNatGateways (Prelude.Maybe Prelude.Natural)
describeNatGateways_maxResults = Lens.lens (\DescribeNatGateways' {maxResults} -> maxResults) (\s@DescribeNatGateways' {} a -> s {maxResults = a} :: DescribeNatGateways)

-- | One or more NAT gateway IDs.
describeNatGateways_natGatewayIds :: Lens.Lens' DescribeNatGateways (Prelude.Maybe [Prelude.Text])
describeNatGateways_natGatewayIds = Lens.lens (\DescribeNatGateways' {natGatewayIds} -> natGatewayIds) (\s@DescribeNatGateways' {} a -> s {natGatewayIds = a} :: DescribeNatGateways) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
describeNatGateways_nextToken :: Lens.Lens' DescribeNatGateways (Prelude.Maybe Prelude.Text)
describeNatGateways_nextToken = Lens.lens (\DescribeNatGateways' {nextToken} -> nextToken) (\s@DescribeNatGateways' {} a -> s {nextToken = a} :: DescribeNatGateways)

instance Core.AWSPager DescribeNatGateways where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNatGatewaysResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeNatGatewaysResponse_natGateways
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeNatGateways_nextToken
          Lens..~ rs
          Lens.^? describeNatGatewaysResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeNatGateways where
  type
    AWSResponse DescribeNatGateways =
      DescribeNatGatewaysResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNatGatewaysResponse'
            Prelude.<$> ( x Data..@? "natGatewaySet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeNatGateways where
  hashWithSalt _salt DescribeNatGateways' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` natGatewayIds
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeNatGateways where
  rnf DescribeNatGateways' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf natGatewayIds
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeNatGateways where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeNatGateways where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeNatGateways where
  toQuery DescribeNatGateways' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeNatGateways" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filter'),
        "MaxResults" Data.=: maxResults,
        Data.toQuery
          ( Data.toQueryList "NatGatewayId"
              Prelude.<$> natGatewayIds
          ),
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeNatGatewaysResponse' smart constructor.
data DescribeNatGatewaysResponse = DescribeNatGatewaysResponse'
  { -- | Information about the NAT gateways.
    natGateways :: Prelude.Maybe [NatGateway],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNatGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'natGateways', 'describeNatGatewaysResponse_natGateways' - Information about the NAT gateways.
--
-- 'nextToken', 'describeNatGatewaysResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeNatGatewaysResponse_httpStatus' - The response's http status code.
newDescribeNatGatewaysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNatGatewaysResponse
newDescribeNatGatewaysResponse pHttpStatus_ =
  DescribeNatGatewaysResponse'
    { natGateways =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the NAT gateways.
describeNatGatewaysResponse_natGateways :: Lens.Lens' DescribeNatGatewaysResponse (Prelude.Maybe [NatGateway])
describeNatGatewaysResponse_natGateways = Lens.lens (\DescribeNatGatewaysResponse' {natGateways} -> natGateways) (\s@DescribeNatGatewaysResponse' {} a -> s {natGateways = a} :: DescribeNatGatewaysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeNatGatewaysResponse_nextToken :: Lens.Lens' DescribeNatGatewaysResponse (Prelude.Maybe Prelude.Text)
describeNatGatewaysResponse_nextToken = Lens.lens (\DescribeNatGatewaysResponse' {nextToken} -> nextToken) (\s@DescribeNatGatewaysResponse' {} a -> s {nextToken = a} :: DescribeNatGatewaysResponse)

-- | The response's http status code.
describeNatGatewaysResponse_httpStatus :: Lens.Lens' DescribeNatGatewaysResponse Prelude.Int
describeNatGatewaysResponse_httpStatus = Lens.lens (\DescribeNatGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeNatGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeNatGatewaysResponse)

instance Prelude.NFData DescribeNatGatewaysResponse where
  rnf DescribeNatGatewaysResponse' {..} =
    Prelude.rnf natGateways
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
