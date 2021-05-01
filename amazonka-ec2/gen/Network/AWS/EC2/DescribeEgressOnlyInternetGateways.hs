{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DescribeEgressOnlyInternetGateways
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your egress-only internet gateways.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeEgressOnlyInternetGateways
  ( -- * Creating a Request
    DescribeEgressOnlyInternetGateways (..),
    newDescribeEgressOnlyInternetGateways,

    -- * Request Lenses
    describeEgressOnlyInternetGateways_nextToken,
    describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds,
    describeEgressOnlyInternetGateways_dryRun,
    describeEgressOnlyInternetGateways_maxResults,
    describeEgressOnlyInternetGateways_filters,

    -- * Destructuring the Response
    DescribeEgressOnlyInternetGatewaysResponse (..),
    newDescribeEgressOnlyInternetGatewaysResponse,

    -- * Response Lenses
    describeEgressOnlyInternetGatewaysResponse_nextToken,
    describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways,
    describeEgressOnlyInternetGatewaysResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEgressOnlyInternetGateways' smart constructor.
data DescribeEgressOnlyInternetGateways = DescribeEgressOnlyInternetGateways'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more egress-only internet gateway IDs.
    egressOnlyInternetGatewayIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeEgressOnlyInternetGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEgressOnlyInternetGateways_nextToken' - The token for the next page of results.
--
-- 'egressOnlyInternetGatewayIds', 'describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds' - One or more egress-only internet gateway IDs.
--
-- 'dryRun', 'describeEgressOnlyInternetGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeEgressOnlyInternetGateways_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
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
newDescribeEgressOnlyInternetGateways ::
  DescribeEgressOnlyInternetGateways
newDescribeEgressOnlyInternetGateways =
  DescribeEgressOnlyInternetGateways'
    { nextToken =
        Prelude.Nothing,
      egressOnlyInternetGatewayIds =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token for the next page of results.
describeEgressOnlyInternetGateways_nextToken :: Lens.Lens' DescribeEgressOnlyInternetGateways (Prelude.Maybe Prelude.Text)
describeEgressOnlyInternetGateways_nextToken = Lens.lens (\DescribeEgressOnlyInternetGateways' {nextToken} -> nextToken) (\s@DescribeEgressOnlyInternetGateways' {} a -> s {nextToken = a} :: DescribeEgressOnlyInternetGateways)

-- | One or more egress-only internet gateway IDs.
describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds :: Lens.Lens' DescribeEgressOnlyInternetGateways (Prelude.Maybe [Prelude.Text])
describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds = Lens.lens (\DescribeEgressOnlyInternetGateways' {egressOnlyInternetGatewayIds} -> egressOnlyInternetGatewayIds) (\s@DescribeEgressOnlyInternetGateways' {} a -> s {egressOnlyInternetGatewayIds = a} :: DescribeEgressOnlyInternetGateways) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeEgressOnlyInternetGateways_dryRun :: Lens.Lens' DescribeEgressOnlyInternetGateways (Prelude.Maybe Prelude.Bool)
describeEgressOnlyInternetGateways_dryRun = Lens.lens (\DescribeEgressOnlyInternetGateways' {dryRun} -> dryRun) (\s@DescribeEgressOnlyInternetGateways' {} a -> s {dryRun = a} :: DescribeEgressOnlyInternetGateways)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeEgressOnlyInternetGateways_maxResults :: Lens.Lens' DescribeEgressOnlyInternetGateways (Prelude.Maybe Prelude.Natural)
describeEgressOnlyInternetGateways_maxResults = Lens.lens (\DescribeEgressOnlyInternetGateways' {maxResults} -> maxResults) (\s@DescribeEgressOnlyInternetGateways' {} a -> s {maxResults = a} :: DescribeEgressOnlyInternetGateways)

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
describeEgressOnlyInternetGateways_filters = Lens.lens (\DescribeEgressOnlyInternetGateways' {filters} -> filters) (\s@DescribeEgressOnlyInternetGateways' {} a -> s {filters = a} :: DescribeEgressOnlyInternetGateways) Prelude.. Lens.mapping Prelude._Coerce

instance
  Pager.AWSPager
    DescribeEgressOnlyInternetGateways
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeEgressOnlyInternetGatewaysResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeEgressOnlyInternetGateways_nextToken
          Lens..~ rs
          Lens.^? describeEgressOnlyInternetGatewaysResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeEgressOnlyInternetGateways
  where
  type
    Rs DescribeEgressOnlyInternetGateways =
      DescribeEgressOnlyInternetGatewaysResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeEgressOnlyInternetGatewaysResponse'
            Prelude.<$> (x Prelude..@? "nextToken")
              Prelude.<*> ( x Prelude..@? "egressOnlyInternetGatewaySet"
                              Prelude..!@ Prelude.mempty
                              Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEgressOnlyInternetGateways

instance
  Prelude.NFData
    DescribeEgressOnlyInternetGateways

instance
  Prelude.ToHeaders
    DescribeEgressOnlyInternetGateways
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DescribeEgressOnlyInternetGateways
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeEgressOnlyInternetGateways
  where
  toQuery DescribeEgressOnlyInternetGateways' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DescribeEgressOnlyInternetGateways" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        Prelude.toQuery
          ( Prelude.toQueryList "EgressOnlyInternetGatewayId"
              Prelude.<$> egressOnlyInternetGatewayIds
          ),
        "DryRun" Prelude.=: dryRun,
        "MaxResults" Prelude.=: maxResults,
        Prelude.toQuery
          (Prelude.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeEgressOnlyInternetGatewaysResponse' smart constructor.
data DescribeEgressOnlyInternetGatewaysResponse = DescribeEgressOnlyInternetGatewaysResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the egress-only internet gateways.
    egressOnlyInternetGateways :: Prelude.Maybe [EgressOnlyInternetGateway],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeEgressOnlyInternetGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEgressOnlyInternetGatewaysResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'egressOnlyInternetGateways', 'describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways' - Information about the egress-only internet gateways.
--
-- 'httpStatus', 'describeEgressOnlyInternetGatewaysResponse_httpStatus' - The response's http status code.
newDescribeEgressOnlyInternetGatewaysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEgressOnlyInternetGatewaysResponse
newDescribeEgressOnlyInternetGatewaysResponse
  pHttpStatus_ =
    DescribeEgressOnlyInternetGatewaysResponse'
      { nextToken =
          Prelude.Nothing,
        egressOnlyInternetGateways =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeEgressOnlyInternetGatewaysResponse_nextToken :: Lens.Lens' DescribeEgressOnlyInternetGatewaysResponse (Prelude.Maybe Prelude.Text)
describeEgressOnlyInternetGatewaysResponse_nextToken = Lens.lens (\DescribeEgressOnlyInternetGatewaysResponse' {nextToken} -> nextToken) (\s@DescribeEgressOnlyInternetGatewaysResponse' {} a -> s {nextToken = a} :: DescribeEgressOnlyInternetGatewaysResponse)

-- | Information about the egress-only internet gateways.
describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways :: Lens.Lens' DescribeEgressOnlyInternetGatewaysResponse (Prelude.Maybe [EgressOnlyInternetGateway])
describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways = Lens.lens (\DescribeEgressOnlyInternetGatewaysResponse' {egressOnlyInternetGateways} -> egressOnlyInternetGateways) (\s@DescribeEgressOnlyInternetGatewaysResponse' {} a -> s {egressOnlyInternetGateways = a} :: DescribeEgressOnlyInternetGatewaysResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeEgressOnlyInternetGatewaysResponse_httpStatus :: Lens.Lens' DescribeEgressOnlyInternetGatewaysResponse Prelude.Int
describeEgressOnlyInternetGatewaysResponse_httpStatus = Lens.lens (\DescribeEgressOnlyInternetGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeEgressOnlyInternetGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeEgressOnlyInternetGatewaysResponse)

instance
  Prelude.NFData
    DescribeEgressOnlyInternetGatewaysResponse
