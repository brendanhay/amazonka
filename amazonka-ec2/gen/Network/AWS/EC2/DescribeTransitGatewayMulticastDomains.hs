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
-- Module      : Network.AWS.EC2.DescribeTransitGatewayMulticastDomains
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more transit gateway multicast domains.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayMulticastDomains
  ( -- * Creating a Request
    DescribeTransitGatewayMulticastDomains (..),
    newDescribeTransitGatewayMulticastDomains,

    -- * Request Lenses
    describeTransitGatewayMulticastDomains_nextToken,
    describeTransitGatewayMulticastDomains_dryRun,
    describeTransitGatewayMulticastDomains_maxResults,
    describeTransitGatewayMulticastDomains_filters,
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTransitGatewayMulticastDomains' smart constructor.
data DescribeTransitGatewayMulticastDomains = DescribeTransitGatewayMulticastDomains'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'dryRun', 'describeTransitGatewayMulticastDomains_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeTransitGatewayMulticastDomains_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
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
-- 'transitGatewayMulticastDomainIds', 'describeTransitGatewayMulticastDomains_transitGatewayMulticastDomainIds' - The ID of the transit gateway multicast domain.
newDescribeTransitGatewayMulticastDomains ::
  DescribeTransitGatewayMulticastDomains
newDescribeTransitGatewayMulticastDomains =
  DescribeTransitGatewayMulticastDomains'
    { nextToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing,
      transitGatewayMulticastDomainIds =
        Prelude.Nothing
    }

-- | The token for the next page of results.
describeTransitGatewayMulticastDomains_nextToken :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Prelude.Maybe Prelude.Text)
describeTransitGatewayMulticastDomains_nextToken = Lens.lens (\DescribeTransitGatewayMulticastDomains' {nextToken} -> nextToken) (\s@DescribeTransitGatewayMulticastDomains' {} a -> s {nextToken = a} :: DescribeTransitGatewayMulticastDomains)

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
describeTransitGatewayMulticastDomains_filters = Lens.lens (\DescribeTransitGatewayMulticastDomains' {filters} -> filters) (\s@DescribeTransitGatewayMulticastDomains' {} a -> s {filters = a} :: DescribeTransitGatewayMulticastDomains) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the transit gateway multicast domain.
describeTransitGatewayMulticastDomains_transitGatewayMulticastDomainIds :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Prelude.Maybe [Prelude.Text])
describeTransitGatewayMulticastDomains_transitGatewayMulticastDomainIds = Lens.lens (\DescribeTransitGatewayMulticastDomains' {transitGatewayMulticastDomainIds} -> transitGatewayMulticastDomainIds) (\s@DescribeTransitGatewayMulticastDomains' {} a -> s {transitGatewayMulticastDomainIds = a} :: DescribeTransitGatewayMulticastDomains) Prelude.. Lens.mapping Prelude._Coerce

instance
  Pager.AWSPager
    DescribeTransitGatewayMulticastDomains
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeTransitGatewayMulticastDomainsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeTransitGatewayMulticastDomainsResponse_transitGatewayMulticastDomains
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeTransitGatewayMulticastDomains_nextToken
          Lens..~ rs
            Lens.^? describeTransitGatewayMulticastDomainsResponse_nextToken
              Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeTransitGatewayMulticastDomains
  where
  type
    Rs DescribeTransitGatewayMulticastDomains =
      DescribeTransitGatewayMulticastDomainsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayMulticastDomainsResponse'
            Prelude.<$> (x Prelude..@? "nextToken")
              Prelude.<*> ( x Prelude..@? "transitGatewayMulticastDomains"
                              Prelude..!@ Prelude.mempty
                              Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTransitGatewayMulticastDomains

instance
  Prelude.NFData
    DescribeTransitGatewayMulticastDomains

instance
  Prelude.ToHeaders
    DescribeTransitGatewayMulticastDomains
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DescribeTransitGatewayMulticastDomains
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeTransitGatewayMulticastDomains
  where
  toQuery DescribeTransitGatewayMulticastDomains' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DescribeTransitGatewayMulticastDomains" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "DryRun" Prelude.=: dryRun,
        "MaxResults" Prelude.=: maxResults,
        Prelude.toQuery
          (Prelude.toQueryList "Filter" Prelude.<$> filters),
        Prelude.toQuery
          ( Prelude.toQueryList
              "TransitGatewayMulticastDomainIds"
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeTransitGatewayMulticastDomainsResponse_transitGatewayMulticastDomains = Lens.lens (\DescribeTransitGatewayMulticastDomainsResponse' {transitGatewayMulticastDomains} -> transitGatewayMulticastDomains) (\s@DescribeTransitGatewayMulticastDomainsResponse' {} a -> s {transitGatewayMulticastDomains = a} :: DescribeTransitGatewayMulticastDomainsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeTransitGatewayMulticastDomainsResponse_httpStatus :: Lens.Lens' DescribeTransitGatewayMulticastDomainsResponse Prelude.Int
describeTransitGatewayMulticastDomainsResponse_httpStatus = Lens.lens (\DescribeTransitGatewayMulticastDomainsResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewayMulticastDomainsResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewayMulticastDomainsResponse)

instance
  Prelude.NFData
    DescribeTransitGatewayMulticastDomainsResponse
