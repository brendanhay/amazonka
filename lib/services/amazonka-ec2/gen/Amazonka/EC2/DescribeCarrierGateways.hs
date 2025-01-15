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
-- Module      : Amazonka.EC2.DescribeCarrierGateways
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your carrier gateways.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeCarrierGateways
  ( -- * Creating a Request
    DescribeCarrierGateways (..),
    newDescribeCarrierGateways,

    -- * Request Lenses
    describeCarrierGateways_carrierGatewayIds,
    describeCarrierGateways_dryRun,
    describeCarrierGateways_filters,
    describeCarrierGateways_maxResults,
    describeCarrierGateways_nextToken,

    -- * Destructuring the Response
    DescribeCarrierGatewaysResponse (..),
    newDescribeCarrierGatewaysResponse,

    -- * Response Lenses
    describeCarrierGatewaysResponse_carrierGateways,
    describeCarrierGatewaysResponse_nextToken,
    describeCarrierGatewaysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCarrierGateways' smart constructor.
data DescribeCarrierGateways = DescribeCarrierGateways'
  { -- | One or more carrier gateway IDs.
    carrierGatewayIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @carrier-gateway-id@ - The ID of the carrier gateway.
    --
    -- -   @state@ - The state of the carrier gateway (@pending@ | @failed@ |
    --     @available@ | @deleting@ | @deleted@).
    --
    -- -   @owner-id@ - The Amazon Web Services account ID of the owner of the
    --     carrier gateway.
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
    -- -   @vpc-id@ - The ID of the VPC associated with the carrier gateway.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCarrierGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'carrierGatewayIds', 'describeCarrierGateways_carrierGatewayIds' - One or more carrier gateway IDs.
--
-- 'dryRun', 'describeCarrierGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeCarrierGateways_filters' - One or more filters.
--
-- -   @carrier-gateway-id@ - The ID of the carrier gateway.
--
-- -   @state@ - The state of the carrier gateway (@pending@ | @failed@ |
--     @available@ | @deleting@ | @deleted@).
--
-- -   @owner-id@ - The Amazon Web Services account ID of the owner of the
--     carrier gateway.
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
-- -   @vpc-id@ - The ID of the VPC associated with the carrier gateway.
--
-- 'maxResults', 'describeCarrierGateways_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeCarrierGateways_nextToken' - The token for the next page of results.
newDescribeCarrierGateways ::
  DescribeCarrierGateways
newDescribeCarrierGateways =
  DescribeCarrierGateways'
    { carrierGatewayIds =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | One or more carrier gateway IDs.
describeCarrierGateways_carrierGatewayIds :: Lens.Lens' DescribeCarrierGateways (Prelude.Maybe [Prelude.Text])
describeCarrierGateways_carrierGatewayIds = Lens.lens (\DescribeCarrierGateways' {carrierGatewayIds} -> carrierGatewayIds) (\s@DescribeCarrierGateways' {} a -> s {carrierGatewayIds = a} :: DescribeCarrierGateways) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeCarrierGateways_dryRun :: Lens.Lens' DescribeCarrierGateways (Prelude.Maybe Prelude.Bool)
describeCarrierGateways_dryRun = Lens.lens (\DescribeCarrierGateways' {dryRun} -> dryRun) (\s@DescribeCarrierGateways' {} a -> s {dryRun = a} :: DescribeCarrierGateways)

-- | One or more filters.
--
-- -   @carrier-gateway-id@ - The ID of the carrier gateway.
--
-- -   @state@ - The state of the carrier gateway (@pending@ | @failed@ |
--     @available@ | @deleting@ | @deleted@).
--
-- -   @owner-id@ - The Amazon Web Services account ID of the owner of the
--     carrier gateway.
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
-- -   @vpc-id@ - The ID of the VPC associated with the carrier gateway.
describeCarrierGateways_filters :: Lens.Lens' DescribeCarrierGateways (Prelude.Maybe [Filter])
describeCarrierGateways_filters = Lens.lens (\DescribeCarrierGateways' {filters} -> filters) (\s@DescribeCarrierGateways' {} a -> s {filters = a} :: DescribeCarrierGateways) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeCarrierGateways_maxResults :: Lens.Lens' DescribeCarrierGateways (Prelude.Maybe Prelude.Natural)
describeCarrierGateways_maxResults = Lens.lens (\DescribeCarrierGateways' {maxResults} -> maxResults) (\s@DescribeCarrierGateways' {} a -> s {maxResults = a} :: DescribeCarrierGateways)

-- | The token for the next page of results.
describeCarrierGateways_nextToken :: Lens.Lens' DescribeCarrierGateways (Prelude.Maybe Prelude.Text)
describeCarrierGateways_nextToken = Lens.lens (\DescribeCarrierGateways' {nextToken} -> nextToken) (\s@DescribeCarrierGateways' {} a -> s {nextToken = a} :: DescribeCarrierGateways)

instance Core.AWSPager DescribeCarrierGateways where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCarrierGatewaysResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCarrierGatewaysResponse_carrierGateways
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeCarrierGateways_nextToken
              Lens..~ rs
              Lens.^? describeCarrierGatewaysResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeCarrierGateways where
  type
    AWSResponse DescribeCarrierGateways =
      DescribeCarrierGatewaysResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeCarrierGatewaysResponse'
            Prelude.<$> ( x
                            Data..@? "carrierGatewaySet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCarrierGateways where
  hashWithSalt _salt DescribeCarrierGateways' {..} =
    _salt
      `Prelude.hashWithSalt` carrierGatewayIds
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeCarrierGateways where
  rnf DescribeCarrierGateways' {..} =
    Prelude.rnf carrierGatewayIds `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf filters `Prelude.seq`
          Prelude.rnf maxResults `Prelude.seq`
            Prelude.rnf nextToken

instance Data.ToHeaders DescribeCarrierGateways where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeCarrierGateways where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCarrierGateways where
  toQuery DescribeCarrierGateways' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeCarrierGateways" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "CarrierGatewayId"
              Prelude.<$> carrierGatewayIds
          ),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeCarrierGatewaysResponse' smart constructor.
data DescribeCarrierGatewaysResponse = DescribeCarrierGatewaysResponse'
  { -- | Information about the carrier gateway.
    carrierGateways :: Prelude.Maybe [CarrierGateway],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCarrierGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'carrierGateways', 'describeCarrierGatewaysResponse_carrierGateways' - Information about the carrier gateway.
--
-- 'nextToken', 'describeCarrierGatewaysResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeCarrierGatewaysResponse_httpStatus' - The response's http status code.
newDescribeCarrierGatewaysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCarrierGatewaysResponse
newDescribeCarrierGatewaysResponse pHttpStatus_ =
  DescribeCarrierGatewaysResponse'
    { carrierGateways =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the carrier gateway.
describeCarrierGatewaysResponse_carrierGateways :: Lens.Lens' DescribeCarrierGatewaysResponse (Prelude.Maybe [CarrierGateway])
describeCarrierGatewaysResponse_carrierGateways = Lens.lens (\DescribeCarrierGatewaysResponse' {carrierGateways} -> carrierGateways) (\s@DescribeCarrierGatewaysResponse' {} a -> s {carrierGateways = a} :: DescribeCarrierGatewaysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeCarrierGatewaysResponse_nextToken :: Lens.Lens' DescribeCarrierGatewaysResponse (Prelude.Maybe Prelude.Text)
describeCarrierGatewaysResponse_nextToken = Lens.lens (\DescribeCarrierGatewaysResponse' {nextToken} -> nextToken) (\s@DescribeCarrierGatewaysResponse' {} a -> s {nextToken = a} :: DescribeCarrierGatewaysResponse)

-- | The response's http status code.
describeCarrierGatewaysResponse_httpStatus :: Lens.Lens' DescribeCarrierGatewaysResponse Prelude.Int
describeCarrierGatewaysResponse_httpStatus = Lens.lens (\DescribeCarrierGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeCarrierGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeCarrierGatewaysResponse)

instance
  Prelude.NFData
    DescribeCarrierGatewaysResponse
  where
  rnf DescribeCarrierGatewaysResponse' {..} =
    Prelude.rnf carrierGateways `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
