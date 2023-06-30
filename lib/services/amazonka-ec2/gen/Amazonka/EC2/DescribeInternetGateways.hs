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
-- Module      : Amazonka.EC2.DescribeInternetGateways
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your internet gateways.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeInternetGateways
  ( -- * Creating a Request
    DescribeInternetGateways (..),
    newDescribeInternetGateways,

    -- * Request Lenses
    describeInternetGateways_dryRun,
    describeInternetGateways_filters,
    describeInternetGateways_internetGatewayIds,
    describeInternetGateways_maxResults,
    describeInternetGateways_nextToken,

    -- * Destructuring the Response
    DescribeInternetGatewaysResponse (..),
    newDescribeInternetGatewaysResponse,

    -- * Response Lenses
    describeInternetGatewaysResponse_internetGateways,
    describeInternetGatewaysResponse_nextToken,
    describeInternetGatewaysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInternetGateways' smart constructor.
data DescribeInternetGateways = DescribeInternetGateways'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @attachment.state@ - The current state of the attachment between the
    --     gateway and the VPC (@available@). Present only if a VPC is
    --     attached.
    --
    -- -   @attachment.vpc-id@ - The ID of an attached VPC.
    --
    -- -   @internet-gateway-id@ - The ID of the Internet gateway.
    --
    -- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
    --     internet gateway.
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
    filters :: Prelude.Maybe [Filter],
    -- | One or more internet gateway IDs.
    --
    -- Default: Describes all your internet gateways.
    internetGatewayIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInternetGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeInternetGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeInternetGateways_filters' - One or more filters.
--
-- -   @attachment.state@ - The current state of the attachment between the
--     gateway and the VPC (@available@). Present only if a VPC is
--     attached.
--
-- -   @attachment.vpc-id@ - The ID of an attached VPC.
--
-- -   @internet-gateway-id@ - The ID of the Internet gateway.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     internet gateway.
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
-- 'internetGatewayIds', 'describeInternetGateways_internetGatewayIds' - One or more internet gateway IDs.
--
-- Default: Describes all your internet gateways.
--
-- 'maxResults', 'describeInternetGateways_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeInternetGateways_nextToken' - The token for the next page of results.
newDescribeInternetGateways ::
  DescribeInternetGateways
newDescribeInternetGateways =
  DescribeInternetGateways'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      internetGatewayIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeInternetGateways_dryRun :: Lens.Lens' DescribeInternetGateways (Prelude.Maybe Prelude.Bool)
describeInternetGateways_dryRun = Lens.lens (\DescribeInternetGateways' {dryRun} -> dryRun) (\s@DescribeInternetGateways' {} a -> s {dryRun = a} :: DescribeInternetGateways)

-- | One or more filters.
--
-- -   @attachment.state@ - The current state of the attachment between the
--     gateway and the VPC (@available@). Present only if a VPC is
--     attached.
--
-- -   @attachment.vpc-id@ - The ID of an attached VPC.
--
-- -   @internet-gateway-id@ - The ID of the Internet gateway.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     internet gateway.
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
describeInternetGateways_filters :: Lens.Lens' DescribeInternetGateways (Prelude.Maybe [Filter])
describeInternetGateways_filters = Lens.lens (\DescribeInternetGateways' {filters} -> filters) (\s@DescribeInternetGateways' {} a -> s {filters = a} :: DescribeInternetGateways) Prelude.. Lens.mapping Lens.coerced

-- | One or more internet gateway IDs.
--
-- Default: Describes all your internet gateways.
describeInternetGateways_internetGatewayIds :: Lens.Lens' DescribeInternetGateways (Prelude.Maybe [Prelude.Text])
describeInternetGateways_internetGatewayIds = Lens.lens (\DescribeInternetGateways' {internetGatewayIds} -> internetGatewayIds) (\s@DescribeInternetGateways' {} a -> s {internetGatewayIds = a} :: DescribeInternetGateways) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeInternetGateways_maxResults :: Lens.Lens' DescribeInternetGateways (Prelude.Maybe Prelude.Natural)
describeInternetGateways_maxResults = Lens.lens (\DescribeInternetGateways' {maxResults} -> maxResults) (\s@DescribeInternetGateways' {} a -> s {maxResults = a} :: DescribeInternetGateways)

-- | The token for the next page of results.
describeInternetGateways_nextToken :: Lens.Lens' DescribeInternetGateways (Prelude.Maybe Prelude.Text)
describeInternetGateways_nextToken = Lens.lens (\DescribeInternetGateways' {nextToken} -> nextToken) (\s@DescribeInternetGateways' {} a -> s {nextToken = a} :: DescribeInternetGateways)

instance Core.AWSPager DescribeInternetGateways where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInternetGatewaysResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInternetGatewaysResponse_internetGateways
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeInternetGateways_nextToken
          Lens..~ rs
          Lens.^? describeInternetGatewaysResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeInternetGateways where
  type
    AWSResponse DescribeInternetGateways =
      DescribeInternetGatewaysResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInternetGatewaysResponse'
            Prelude.<$> ( x
                            Data..@? "internetGatewaySet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInternetGateways where
  hashWithSalt _salt DescribeInternetGateways' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` internetGatewayIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeInternetGateways where
  rnf DescribeInternetGateways' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf internetGatewayIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeInternetGateways where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeInternetGateways where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeInternetGateways where
  toQuery DescribeInternetGateways' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeInternetGateways" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          ( Data.toQueryList "InternetGatewayId"
              Prelude.<$> internetGatewayIds
          ),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeInternetGatewaysResponse' smart constructor.
data DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse'
  { -- | Information about one or more internet gateways.
    internetGateways :: Prelude.Maybe [InternetGateway],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInternetGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'internetGateways', 'describeInternetGatewaysResponse_internetGateways' - Information about one or more internet gateways.
--
-- 'nextToken', 'describeInternetGatewaysResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeInternetGatewaysResponse_httpStatus' - The response's http status code.
newDescribeInternetGatewaysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInternetGatewaysResponse
newDescribeInternetGatewaysResponse pHttpStatus_ =
  DescribeInternetGatewaysResponse'
    { internetGateways =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about one or more internet gateways.
describeInternetGatewaysResponse_internetGateways :: Lens.Lens' DescribeInternetGatewaysResponse (Prelude.Maybe [InternetGateway])
describeInternetGatewaysResponse_internetGateways = Lens.lens (\DescribeInternetGatewaysResponse' {internetGateways} -> internetGateways) (\s@DescribeInternetGatewaysResponse' {} a -> s {internetGateways = a} :: DescribeInternetGatewaysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeInternetGatewaysResponse_nextToken :: Lens.Lens' DescribeInternetGatewaysResponse (Prelude.Maybe Prelude.Text)
describeInternetGatewaysResponse_nextToken = Lens.lens (\DescribeInternetGatewaysResponse' {nextToken} -> nextToken) (\s@DescribeInternetGatewaysResponse' {} a -> s {nextToken = a} :: DescribeInternetGatewaysResponse)

-- | The response's http status code.
describeInternetGatewaysResponse_httpStatus :: Lens.Lens' DescribeInternetGatewaysResponse Prelude.Int
describeInternetGatewaysResponse_httpStatus = Lens.lens (\DescribeInternetGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeInternetGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeInternetGatewaysResponse)

instance
  Prelude.NFData
    DescribeInternetGatewaysResponse
  where
  rnf DescribeInternetGatewaysResponse' {..} =
    Prelude.rnf internetGateways
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
