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
-- Module      : Network.AWS.EC2.DescribeCarrierGateways
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your carrier gateways.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeCarrierGateways
  ( -- * Creating a Request
    DescribeCarrierGateways (..),
    newDescribeCarrierGateways,

    -- * Request Lenses
    describeCarrierGateways_filters,
    describeCarrierGateways_nextToken,
    describeCarrierGateways_carrierGatewayIds,
    describeCarrierGateways_dryRun,
    describeCarrierGateways_maxResults,

    -- * Destructuring the Response
    DescribeCarrierGatewaysResponse (..),
    newDescribeCarrierGatewaysResponse,

    -- * Response Lenses
    describeCarrierGatewaysResponse_nextToken,
    describeCarrierGatewaysResponse_carrierGateways,
    describeCarrierGatewaysResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCarrierGateways' smart constructor.
data DescribeCarrierGateways = DescribeCarrierGateways'
  { -- | One or more filters.
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
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more carrier gateway IDs.
    carrierGatewayIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'describeCarrierGateways_nextToken' - The token for the next page of results.
--
-- 'carrierGatewayIds', 'describeCarrierGateways_carrierGatewayIds' - One or more carrier gateway IDs.
--
-- 'dryRun', 'describeCarrierGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeCarrierGateways_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newDescribeCarrierGateways ::
  DescribeCarrierGateways
newDescribeCarrierGateways =
  DescribeCarrierGateways'
    { filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      carrierGatewayIds = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

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

-- | The token for the next page of results.
describeCarrierGateways_nextToken :: Lens.Lens' DescribeCarrierGateways (Prelude.Maybe Prelude.Text)
describeCarrierGateways_nextToken = Lens.lens (\DescribeCarrierGateways' {nextToken} -> nextToken) (\s@DescribeCarrierGateways' {} a -> s {nextToken = a} :: DescribeCarrierGateways)

-- | One or more carrier gateway IDs.
describeCarrierGateways_carrierGatewayIds :: Lens.Lens' DescribeCarrierGateways (Prelude.Maybe [Prelude.Text])
describeCarrierGateways_carrierGatewayIds = Lens.lens (\DescribeCarrierGateways' {carrierGatewayIds} -> carrierGatewayIds) (\s@DescribeCarrierGateways' {} a -> s {carrierGatewayIds = a} :: DescribeCarrierGateways) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeCarrierGateways_dryRun :: Lens.Lens' DescribeCarrierGateways (Prelude.Maybe Prelude.Bool)
describeCarrierGateways_dryRun = Lens.lens (\DescribeCarrierGateways' {dryRun} -> dryRun) (\s@DescribeCarrierGateways' {} a -> s {dryRun = a} :: DescribeCarrierGateways)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeCarrierGateways_maxResults :: Lens.Lens' DescribeCarrierGateways (Prelude.Maybe Prelude.Natural)
describeCarrierGateways_maxResults = Lens.lens (\DescribeCarrierGateways' {maxResults} -> maxResults) (\s@DescribeCarrierGateways' {} a -> s {maxResults = a} :: DescribeCarrierGateways)

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeCarrierGatewaysResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "carrierGatewaySet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCarrierGateways

instance Prelude.NFData DescribeCarrierGateways

instance Core.ToHeaders DescribeCarrierGateways where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeCarrierGateways where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCarrierGateways where
  toQuery DescribeCarrierGateways' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeCarrierGateways" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          ( Core.toQueryList "CarrierGatewayId"
              Prelude.<$> carrierGatewayIds
          ),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeCarrierGatewaysResponse' smart constructor.
data DescribeCarrierGatewaysResponse = DescribeCarrierGatewaysResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the carrier gateway.
    carrierGateways :: Prelude.Maybe [CarrierGateway],
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
-- 'nextToken', 'describeCarrierGatewaysResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'carrierGateways', 'describeCarrierGatewaysResponse_carrierGateways' - Information about the carrier gateway.
--
-- 'httpStatus', 'describeCarrierGatewaysResponse_httpStatus' - The response's http status code.
newDescribeCarrierGatewaysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCarrierGatewaysResponse
newDescribeCarrierGatewaysResponse pHttpStatus_ =
  DescribeCarrierGatewaysResponse'
    { nextToken =
        Prelude.Nothing,
      carrierGateways = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeCarrierGatewaysResponse_nextToken :: Lens.Lens' DescribeCarrierGatewaysResponse (Prelude.Maybe Prelude.Text)
describeCarrierGatewaysResponse_nextToken = Lens.lens (\DescribeCarrierGatewaysResponse' {nextToken} -> nextToken) (\s@DescribeCarrierGatewaysResponse' {} a -> s {nextToken = a} :: DescribeCarrierGatewaysResponse)

-- | Information about the carrier gateway.
describeCarrierGatewaysResponse_carrierGateways :: Lens.Lens' DescribeCarrierGatewaysResponse (Prelude.Maybe [CarrierGateway])
describeCarrierGatewaysResponse_carrierGateways = Lens.lens (\DescribeCarrierGatewaysResponse' {carrierGateways} -> carrierGateways) (\s@DescribeCarrierGatewaysResponse' {} a -> s {carrierGateways = a} :: DescribeCarrierGatewaysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeCarrierGatewaysResponse_httpStatus :: Lens.Lens' DescribeCarrierGatewaysResponse Prelude.Int
describeCarrierGatewaysResponse_httpStatus = Lens.lens (\DescribeCarrierGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeCarrierGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeCarrierGatewaysResponse)

instance
  Prelude.NFData
    DescribeCarrierGatewaysResponse
