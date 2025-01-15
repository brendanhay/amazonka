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
-- Module      : Amazonka.EC2.DescribeEgressOnlyInternetGateways
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your egress-only internet gateways.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeEgressOnlyInternetGateways
  ( -- * Creating a Request
    DescribeEgressOnlyInternetGateways (..),
    newDescribeEgressOnlyInternetGateways,

    -- * Request Lenses
    describeEgressOnlyInternetGateways_dryRun,
    describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds,
    describeEgressOnlyInternetGateways_filters,
    describeEgressOnlyInternetGateways_maxResults,
    describeEgressOnlyInternetGateways_nextToken,

    -- * Destructuring the Response
    DescribeEgressOnlyInternetGatewaysResponse (..),
    newDescribeEgressOnlyInternetGatewaysResponse,

    -- * Response Lenses
    describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways,
    describeEgressOnlyInternetGatewaysResponse_nextToken,
    describeEgressOnlyInternetGatewaysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEgressOnlyInternetGateways' smart constructor.
data DescribeEgressOnlyInternetGateways = DescribeEgressOnlyInternetGateways'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more egress-only internet gateway IDs.
    egressOnlyInternetGatewayIds :: Prelude.Maybe [Prelude.Text],
    -- | One or more filters.
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
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEgressOnlyInternetGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeEgressOnlyInternetGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'egressOnlyInternetGatewayIds', 'describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds' - One or more egress-only internet gateway IDs.
--
-- 'filters', 'describeEgressOnlyInternetGateways_filters' - One or more filters.
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
-- 'maxResults', 'describeEgressOnlyInternetGateways_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeEgressOnlyInternetGateways_nextToken' - The token for the next page of results.
newDescribeEgressOnlyInternetGateways ::
  DescribeEgressOnlyInternetGateways
newDescribeEgressOnlyInternetGateways =
  DescribeEgressOnlyInternetGateways'
    { dryRun =
        Prelude.Nothing,
      egressOnlyInternetGatewayIds =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeEgressOnlyInternetGateways_dryRun :: Lens.Lens' DescribeEgressOnlyInternetGateways (Prelude.Maybe Prelude.Bool)
describeEgressOnlyInternetGateways_dryRun = Lens.lens (\DescribeEgressOnlyInternetGateways' {dryRun} -> dryRun) (\s@DescribeEgressOnlyInternetGateways' {} a -> s {dryRun = a} :: DescribeEgressOnlyInternetGateways)

-- | One or more egress-only internet gateway IDs.
describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds :: Lens.Lens' DescribeEgressOnlyInternetGateways (Prelude.Maybe [Prelude.Text])
describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds = Lens.lens (\DescribeEgressOnlyInternetGateways' {egressOnlyInternetGatewayIds} -> egressOnlyInternetGatewayIds) (\s@DescribeEgressOnlyInternetGateways' {} a -> s {egressOnlyInternetGatewayIds = a} :: DescribeEgressOnlyInternetGateways) Prelude.. Lens.mapping Lens.coerced

-- | One or more filters.
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
describeEgressOnlyInternetGateways_filters :: Lens.Lens' DescribeEgressOnlyInternetGateways (Prelude.Maybe [Filter])
describeEgressOnlyInternetGateways_filters = Lens.lens (\DescribeEgressOnlyInternetGateways' {filters} -> filters) (\s@DescribeEgressOnlyInternetGateways' {} a -> s {filters = a} :: DescribeEgressOnlyInternetGateways) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeEgressOnlyInternetGateways_maxResults :: Lens.Lens' DescribeEgressOnlyInternetGateways (Prelude.Maybe Prelude.Natural)
describeEgressOnlyInternetGateways_maxResults = Lens.lens (\DescribeEgressOnlyInternetGateways' {maxResults} -> maxResults) (\s@DescribeEgressOnlyInternetGateways' {} a -> s {maxResults = a} :: DescribeEgressOnlyInternetGateways)

-- | The token for the next page of results.
describeEgressOnlyInternetGateways_nextToken :: Lens.Lens' DescribeEgressOnlyInternetGateways (Prelude.Maybe Prelude.Text)
describeEgressOnlyInternetGateways_nextToken = Lens.lens (\DescribeEgressOnlyInternetGateways' {nextToken} -> nextToken) (\s@DescribeEgressOnlyInternetGateways' {} a -> s {nextToken = a} :: DescribeEgressOnlyInternetGateways)

instance
  Core.AWSPager
    DescribeEgressOnlyInternetGateways
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEgressOnlyInternetGatewaysResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeEgressOnlyInternetGateways_nextToken
              Lens..~ rs
              Lens.^? describeEgressOnlyInternetGatewaysResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeEgressOnlyInternetGateways
  where
  type
    AWSResponse DescribeEgressOnlyInternetGateways =
      DescribeEgressOnlyInternetGatewaysResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeEgressOnlyInternetGatewaysResponse'
            Prelude.<$> ( x
                            Data..@? "egressOnlyInternetGatewaySet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEgressOnlyInternetGateways
  where
  hashWithSalt
    _salt
    DescribeEgressOnlyInternetGateways' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` egressOnlyInternetGatewayIds
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeEgressOnlyInternetGateways
  where
  rnf DescribeEgressOnlyInternetGateways' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf egressOnlyInternetGatewayIds `Prelude.seq`
        Prelude.rnf filters `Prelude.seq`
          Prelude.rnf maxResults `Prelude.seq`
            Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeEgressOnlyInternetGateways
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeEgressOnlyInternetGateways
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeEgressOnlyInternetGateways
  where
  toQuery DescribeEgressOnlyInternetGateways' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeEgressOnlyInternetGateways" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "EgressOnlyInternetGatewayId"
              Prelude.<$> egressOnlyInternetGatewayIds
          ),
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeEgressOnlyInternetGatewaysResponse' smart constructor.
data DescribeEgressOnlyInternetGatewaysResponse = DescribeEgressOnlyInternetGatewaysResponse'
  { -- | Information about the egress-only internet gateways.
    egressOnlyInternetGateways :: Prelude.Maybe [EgressOnlyInternetGateway],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEgressOnlyInternetGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressOnlyInternetGateways', 'describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways' - Information about the egress-only internet gateways.
--
-- 'nextToken', 'describeEgressOnlyInternetGatewaysResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeEgressOnlyInternetGatewaysResponse_httpStatus' - The response's http status code.
newDescribeEgressOnlyInternetGatewaysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEgressOnlyInternetGatewaysResponse
newDescribeEgressOnlyInternetGatewaysResponse
  pHttpStatus_ =
    DescribeEgressOnlyInternetGatewaysResponse'
      { egressOnlyInternetGateways =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the egress-only internet gateways.
describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways :: Lens.Lens' DescribeEgressOnlyInternetGatewaysResponse (Prelude.Maybe [EgressOnlyInternetGateway])
describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways = Lens.lens (\DescribeEgressOnlyInternetGatewaysResponse' {egressOnlyInternetGateways} -> egressOnlyInternetGateways) (\s@DescribeEgressOnlyInternetGatewaysResponse' {} a -> s {egressOnlyInternetGateways = a} :: DescribeEgressOnlyInternetGatewaysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeEgressOnlyInternetGatewaysResponse_nextToken :: Lens.Lens' DescribeEgressOnlyInternetGatewaysResponse (Prelude.Maybe Prelude.Text)
describeEgressOnlyInternetGatewaysResponse_nextToken = Lens.lens (\DescribeEgressOnlyInternetGatewaysResponse' {nextToken} -> nextToken) (\s@DescribeEgressOnlyInternetGatewaysResponse' {} a -> s {nextToken = a} :: DescribeEgressOnlyInternetGatewaysResponse)

-- | The response's http status code.
describeEgressOnlyInternetGatewaysResponse_httpStatus :: Lens.Lens' DescribeEgressOnlyInternetGatewaysResponse Prelude.Int
describeEgressOnlyInternetGatewaysResponse_httpStatus = Lens.lens (\DescribeEgressOnlyInternetGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeEgressOnlyInternetGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeEgressOnlyInternetGatewaysResponse)

instance
  Prelude.NFData
    DescribeEgressOnlyInternetGatewaysResponse
  where
  rnf DescribeEgressOnlyInternetGatewaysResponse' {..} =
    Prelude.rnf egressOnlyInternetGateways `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
