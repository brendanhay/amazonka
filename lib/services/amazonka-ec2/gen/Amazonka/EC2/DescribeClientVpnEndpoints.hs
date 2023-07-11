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
-- Module      : Amazonka.EC2.DescribeClientVpnEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Client VPN endpoints in the account.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeClientVpnEndpoints
  ( -- * Creating a Request
    DescribeClientVpnEndpoints (..),
    newDescribeClientVpnEndpoints,

    -- * Request Lenses
    describeClientVpnEndpoints_clientVpnEndpointIds,
    describeClientVpnEndpoints_dryRun,
    describeClientVpnEndpoints_filters,
    describeClientVpnEndpoints_maxResults,
    describeClientVpnEndpoints_nextToken,

    -- * Destructuring the Response
    DescribeClientVpnEndpointsResponse (..),
    newDescribeClientVpnEndpointsResponse,

    -- * Response Lenses
    describeClientVpnEndpointsResponse_clientVpnEndpoints,
    describeClientVpnEndpointsResponse_nextToken,
    describeClientVpnEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeClientVpnEndpoints' smart constructor.
data DescribeClientVpnEndpoints = DescribeClientVpnEndpoints'
  { -- | The ID of the Client VPN endpoint.
    clientVpnEndpointIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters. Filter names and values are case-sensitive.
    --
    -- -   @endpoint-id@ - The ID of the Client VPN endpoint.
    --
    -- -   @transport-protocol@ - The transport protocol (@tcp@ | @udp@).
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the nextToken value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClientVpnEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientVpnEndpointIds', 'describeClientVpnEndpoints_clientVpnEndpointIds' - The ID of the Client VPN endpoint.
--
-- 'dryRun', 'describeClientVpnEndpoints_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeClientVpnEndpoints_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- -   @endpoint-id@ - The ID of the Client VPN endpoint.
--
-- -   @transport-protocol@ - The transport protocol (@tcp@ | @udp@).
--
-- 'maxResults', 'describeClientVpnEndpoints_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
--
-- 'nextToken', 'describeClientVpnEndpoints_nextToken' - The token to retrieve the next page of results.
newDescribeClientVpnEndpoints ::
  DescribeClientVpnEndpoints
newDescribeClientVpnEndpoints =
  DescribeClientVpnEndpoints'
    { clientVpnEndpointIds =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The ID of the Client VPN endpoint.
describeClientVpnEndpoints_clientVpnEndpointIds :: Lens.Lens' DescribeClientVpnEndpoints (Prelude.Maybe [Prelude.Text])
describeClientVpnEndpoints_clientVpnEndpointIds = Lens.lens (\DescribeClientVpnEndpoints' {clientVpnEndpointIds} -> clientVpnEndpointIds) (\s@DescribeClientVpnEndpoints' {} a -> s {clientVpnEndpointIds = a} :: DescribeClientVpnEndpoints) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeClientVpnEndpoints_dryRun :: Lens.Lens' DescribeClientVpnEndpoints (Prelude.Maybe Prelude.Bool)
describeClientVpnEndpoints_dryRun = Lens.lens (\DescribeClientVpnEndpoints' {dryRun} -> dryRun) (\s@DescribeClientVpnEndpoints' {} a -> s {dryRun = a} :: DescribeClientVpnEndpoints)

-- | One or more filters. Filter names and values are case-sensitive.
--
-- -   @endpoint-id@ - The ID of the Client VPN endpoint.
--
-- -   @transport-protocol@ - The transport protocol (@tcp@ | @udp@).
describeClientVpnEndpoints_filters :: Lens.Lens' DescribeClientVpnEndpoints (Prelude.Maybe [Filter])
describeClientVpnEndpoints_filters = Lens.lens (\DescribeClientVpnEndpoints' {filters} -> filters) (\s@DescribeClientVpnEndpoints' {} a -> s {filters = a} :: DescribeClientVpnEndpoints) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
describeClientVpnEndpoints_maxResults :: Lens.Lens' DescribeClientVpnEndpoints (Prelude.Maybe Prelude.Natural)
describeClientVpnEndpoints_maxResults = Lens.lens (\DescribeClientVpnEndpoints' {maxResults} -> maxResults) (\s@DescribeClientVpnEndpoints' {} a -> s {maxResults = a} :: DescribeClientVpnEndpoints)

-- | The token to retrieve the next page of results.
describeClientVpnEndpoints_nextToken :: Lens.Lens' DescribeClientVpnEndpoints (Prelude.Maybe Prelude.Text)
describeClientVpnEndpoints_nextToken = Lens.lens (\DescribeClientVpnEndpoints' {nextToken} -> nextToken) (\s@DescribeClientVpnEndpoints' {} a -> s {nextToken = a} :: DescribeClientVpnEndpoints)

instance Core.AWSPager DescribeClientVpnEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClientVpnEndpointsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClientVpnEndpointsResponse_clientVpnEndpoints
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeClientVpnEndpoints_nextToken
          Lens..~ rs
          Lens.^? describeClientVpnEndpointsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeClientVpnEndpoints where
  type
    AWSResponse DescribeClientVpnEndpoints =
      DescribeClientVpnEndpointsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeClientVpnEndpointsResponse'
            Prelude.<$> ( x
                            Data..@? "clientVpnEndpoint"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClientVpnEndpoints where
  hashWithSalt _salt DescribeClientVpnEndpoints' {..} =
    _salt
      `Prelude.hashWithSalt` clientVpnEndpointIds
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeClientVpnEndpoints where
  rnf DescribeClientVpnEndpoints' {..} =
    Prelude.rnf clientVpnEndpointIds
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeClientVpnEndpoints where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeClientVpnEndpoints where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeClientVpnEndpoints where
  toQuery DescribeClientVpnEndpoints' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeClientVpnEndpoints" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "ClientVpnEndpointId"
              Prelude.<$> clientVpnEndpointIds
          ),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeClientVpnEndpointsResponse' smart constructor.
data DescribeClientVpnEndpointsResponse = DescribeClientVpnEndpointsResponse'
  { -- | Information about the Client VPN endpoints.
    clientVpnEndpoints :: Prelude.Maybe [ClientVpnEndpoint],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClientVpnEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientVpnEndpoints', 'describeClientVpnEndpointsResponse_clientVpnEndpoints' - Information about the Client VPN endpoints.
--
-- 'nextToken', 'describeClientVpnEndpointsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeClientVpnEndpointsResponse_httpStatus' - The response's http status code.
newDescribeClientVpnEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClientVpnEndpointsResponse
newDescribeClientVpnEndpointsResponse pHttpStatus_ =
  DescribeClientVpnEndpointsResponse'
    { clientVpnEndpoints =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Client VPN endpoints.
describeClientVpnEndpointsResponse_clientVpnEndpoints :: Lens.Lens' DescribeClientVpnEndpointsResponse (Prelude.Maybe [ClientVpnEndpoint])
describeClientVpnEndpointsResponse_clientVpnEndpoints = Lens.lens (\DescribeClientVpnEndpointsResponse' {clientVpnEndpoints} -> clientVpnEndpoints) (\s@DescribeClientVpnEndpointsResponse' {} a -> s {clientVpnEndpoints = a} :: DescribeClientVpnEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeClientVpnEndpointsResponse_nextToken :: Lens.Lens' DescribeClientVpnEndpointsResponse (Prelude.Maybe Prelude.Text)
describeClientVpnEndpointsResponse_nextToken = Lens.lens (\DescribeClientVpnEndpointsResponse' {nextToken} -> nextToken) (\s@DescribeClientVpnEndpointsResponse' {} a -> s {nextToken = a} :: DescribeClientVpnEndpointsResponse)

-- | The response's http status code.
describeClientVpnEndpointsResponse_httpStatus :: Lens.Lens' DescribeClientVpnEndpointsResponse Prelude.Int
describeClientVpnEndpointsResponse_httpStatus = Lens.lens (\DescribeClientVpnEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeClientVpnEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeClientVpnEndpointsResponse)

instance
  Prelude.NFData
    DescribeClientVpnEndpointsResponse
  where
  rnf DescribeClientVpnEndpointsResponse' {..} =
    Prelude.rnf clientVpnEndpoints
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
