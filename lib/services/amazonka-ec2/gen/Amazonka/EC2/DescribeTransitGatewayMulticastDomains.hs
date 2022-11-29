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
-- Module      : Amazonka.EC2.DescribeTransitGatewayMulticastDomains
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more transit gateway multicast domains.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeTransitGatewayMulticastDomains
  ( -- * Creating a Request
    DescribeTransitGatewayMulticastDomains (..),
    newDescribeTransitGatewayMulticastDomains,

    -- * Request Lenses
    describeTransitGatewayMulticastDomains_nextToken,
    describeTransitGatewayMulticastDomains_filters,
    describeTransitGatewayMulticastDomains_dryRun,
    describeTransitGatewayMulticastDomains_maxResults,
    describeTransitGatewayMulticastDomains_transitGatewayMulticastDomainIds,

    -- * Destructuring the Response
    DescribeTransitGatewayMulticastDomainsResponse (..),
    newDescribeTransitGatewayMulticastDomainsResponse,

    -- * Response Lenses
    describeTransitGatewayMulticastDomainsResponse_nextToken,
    describeTransitGatewayMulticastDomainsResponse_transitGatewayMulticastDomains,
    describeTransitGatewayMulticastDomainsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTransitGatewayMulticastDomains' smart constructor.
data DescribeTransitGatewayMulticastDomains = DescribeTransitGatewayMulticastDomains'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more filters. The possible values are:
    --
    -- -   @state@ - The state of the transit gateway multicast domain. Valid
    --     values are @pending@ | @available@ | @deleting@ | @deleted@.
    --
    -- -   @transit-gateway-id@ - The ID of the transit gateway.
    --
    -- -   @transit-gateway-multicast-domain-id@ - The ID of the transit
    --     gateway multicast domain.
    filters :: Prelude.Maybe [Filter],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayMulticastDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewayMulticastDomains_nextToken' - The token for the next page of results.
--
-- 'filters', 'describeTransitGatewayMulticastDomains_filters' - One or more filters. The possible values are:
--
-- -   @state@ - The state of the transit gateway multicast domain. Valid
--     values are @pending@ | @available@ | @deleting@ | @deleted@.
--
-- -   @transit-gateway-id@ - The ID of the transit gateway.
--
-- -   @transit-gateway-multicast-domain-id@ - The ID of the transit
--     gateway multicast domain.
--
-- 'dryRun', 'describeTransitGatewayMulticastDomains_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeTransitGatewayMulticastDomains_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'transitGatewayMulticastDomainIds', 'describeTransitGatewayMulticastDomains_transitGatewayMulticastDomainIds' - The ID of the transit gateway multicast domain.
newDescribeTransitGatewayMulticastDomains ::
  DescribeTransitGatewayMulticastDomains
newDescribeTransitGatewayMulticastDomains =
  DescribeTransitGatewayMulticastDomains'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      transitGatewayMulticastDomainIds =
        Prelude.Nothing
    }

-- | The token for the next page of results.
describeTransitGatewayMulticastDomains_nextToken :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Prelude.Maybe Prelude.Text)
describeTransitGatewayMulticastDomains_nextToken = Lens.lens (\DescribeTransitGatewayMulticastDomains' {nextToken} -> nextToken) (\s@DescribeTransitGatewayMulticastDomains' {} a -> s {nextToken = a} :: DescribeTransitGatewayMulticastDomains)

-- | One or more filters. The possible values are:
--
-- -   @state@ - The state of the transit gateway multicast domain. Valid
--     values are @pending@ | @available@ | @deleting@ | @deleted@.
--
-- -   @transit-gateway-id@ - The ID of the transit gateway.
--
-- -   @transit-gateway-multicast-domain-id@ - The ID of the transit
--     gateway multicast domain.
describeTransitGatewayMulticastDomains_filters :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Prelude.Maybe [Filter])
describeTransitGatewayMulticastDomains_filters = Lens.lens (\DescribeTransitGatewayMulticastDomains' {filters} -> filters) (\s@DescribeTransitGatewayMulticastDomains' {} a -> s {filters = a} :: DescribeTransitGatewayMulticastDomains) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTransitGatewayMulticastDomains_dryRun :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Prelude.Maybe Prelude.Bool)
describeTransitGatewayMulticastDomains_dryRun = Lens.lens (\DescribeTransitGatewayMulticastDomains' {dryRun} -> dryRun) (\s@DescribeTransitGatewayMulticastDomains' {} a -> s {dryRun = a} :: DescribeTransitGatewayMulticastDomains)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTransitGatewayMulticastDomains_maxResults :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Prelude.Maybe Prelude.Natural)
describeTransitGatewayMulticastDomains_maxResults = Lens.lens (\DescribeTransitGatewayMulticastDomains' {maxResults} -> maxResults) (\s@DescribeTransitGatewayMulticastDomains' {} a -> s {maxResults = a} :: DescribeTransitGatewayMulticastDomains)

-- | The ID of the transit gateway multicast domain.
describeTransitGatewayMulticastDomains_transitGatewayMulticastDomainIds :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Prelude.Maybe [Prelude.Text])
describeTransitGatewayMulticastDomains_transitGatewayMulticastDomainIds = Lens.lens (\DescribeTransitGatewayMulticastDomains' {transitGatewayMulticastDomainIds} -> transitGatewayMulticastDomainIds) (\s@DescribeTransitGatewayMulticastDomains' {} a -> s {transitGatewayMulticastDomainIds = a} :: DescribeTransitGatewayMulticastDomains) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSPager
    DescribeTransitGatewayMulticastDomains
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayMulticastDomainsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayMulticastDomainsResponse_transitGatewayMulticastDomains
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTransitGatewayMulticastDomains_nextToken
          Lens..~ rs
            Lens.^? describeTransitGatewayMulticastDomainsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeTransitGatewayMulticastDomains
  where
  type
    AWSResponse
      DescribeTransitGatewayMulticastDomains =
      DescribeTransitGatewayMulticastDomainsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayMulticastDomainsResponse'
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "transitGatewayMulticastDomains"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTransitGatewayMulticastDomains
  where
  hashWithSalt
    _salt
    DescribeTransitGatewayMulticastDomains' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` transitGatewayMulticastDomainIds

instance
  Prelude.NFData
    DescribeTransitGatewayMulticastDomains
  where
  rnf DescribeTransitGatewayMulticastDomains' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf transitGatewayMulticastDomainIds

instance
  Core.ToHeaders
    DescribeTransitGatewayMulticastDomains
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeTransitGatewayMulticastDomains
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeTransitGatewayMulticastDomains
  where
  toQuery DescribeTransitGatewayMulticastDomains' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeTransitGatewayMulticastDomains" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "TransitGatewayMulticastDomainIds"
              Prelude.<$> transitGatewayMulticastDomainIds
          )
      ]

-- | /See:/ 'newDescribeTransitGatewayMulticastDomainsResponse' smart constructor.
data DescribeTransitGatewayMulticastDomainsResponse = DescribeTransitGatewayMulticastDomainsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the transit gateway multicast domains.
    transitGatewayMulticastDomains :: Prelude.Maybe [TransitGatewayMulticastDomain],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayMulticastDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewayMulticastDomainsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'transitGatewayMulticastDomains', 'describeTransitGatewayMulticastDomainsResponse_transitGatewayMulticastDomains' - Information about the transit gateway multicast domains.
--
-- 'httpStatus', 'describeTransitGatewayMulticastDomainsResponse_httpStatus' - The response's http status code.
newDescribeTransitGatewayMulticastDomainsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTransitGatewayMulticastDomainsResponse
newDescribeTransitGatewayMulticastDomainsResponse
  pHttpStatus_ =
    DescribeTransitGatewayMulticastDomainsResponse'
      { nextToken =
          Prelude.Nothing,
        transitGatewayMulticastDomains =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeTransitGatewayMulticastDomainsResponse_nextToken :: Lens.Lens' DescribeTransitGatewayMulticastDomainsResponse (Prelude.Maybe Prelude.Text)
describeTransitGatewayMulticastDomainsResponse_nextToken = Lens.lens (\DescribeTransitGatewayMulticastDomainsResponse' {nextToken} -> nextToken) (\s@DescribeTransitGatewayMulticastDomainsResponse' {} a -> s {nextToken = a} :: DescribeTransitGatewayMulticastDomainsResponse)

-- | Information about the transit gateway multicast domains.
describeTransitGatewayMulticastDomainsResponse_transitGatewayMulticastDomains :: Lens.Lens' DescribeTransitGatewayMulticastDomainsResponse (Prelude.Maybe [TransitGatewayMulticastDomain])
describeTransitGatewayMulticastDomainsResponse_transitGatewayMulticastDomains = Lens.lens (\DescribeTransitGatewayMulticastDomainsResponse' {transitGatewayMulticastDomains} -> transitGatewayMulticastDomains) (\s@DescribeTransitGatewayMulticastDomainsResponse' {} a -> s {transitGatewayMulticastDomains = a} :: DescribeTransitGatewayMulticastDomainsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTransitGatewayMulticastDomainsResponse_httpStatus :: Lens.Lens' DescribeTransitGatewayMulticastDomainsResponse Prelude.Int
describeTransitGatewayMulticastDomainsResponse_httpStatus = Lens.lens (\DescribeTransitGatewayMulticastDomainsResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewayMulticastDomainsResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewayMulticastDomainsResponse)

instance
  Prelude.NFData
    DescribeTransitGatewayMulticastDomainsResponse
  where
  rnf
    DescribeTransitGatewayMulticastDomainsResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf transitGatewayMulticastDomains
        `Prelude.seq` Prelude.rnf httpStatus
