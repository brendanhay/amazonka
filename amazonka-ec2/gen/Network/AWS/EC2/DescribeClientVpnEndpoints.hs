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
-- Module      : Network.AWS.EC2.DescribeClientVpnEndpoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Client VPN endpoints in the account.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVpnEndpoints
  ( -- * Creating a Request
    DescribeClientVpnEndpoints (..),
    newDescribeClientVpnEndpoints,

    -- * Request Lenses
    describeClientVpnEndpoints_nextToken,
    describeClientVpnEndpoints_dryRun,
    describeClientVpnEndpoints_maxResults,
    describeClientVpnEndpoints_clientVpnEndpointIds,
    describeClientVpnEndpoints_filters,

    -- * Destructuring the Response
    DescribeClientVpnEndpointsResponse (..),
    newDescribeClientVpnEndpointsResponse,

    -- * Response Lenses
    describeClientVpnEndpointsResponse_nextToken,
    describeClientVpnEndpointsResponse_clientVpnEndpoints,
    describeClientVpnEndpointsResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeClientVpnEndpoints' smart constructor.
data DescribeClientVpnEndpoints = DescribeClientVpnEndpoints'
  { -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the nextToken value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointIds :: Prelude.Maybe [Prelude.Text],
    -- | One or more filters. Filter names and values are case-sensitive.
    --
    -- -   @endpoint-id@ - The ID of the Client VPN endpoint.
    --
    -- -   @transport-protocol@ - The transport protocol (@tcp@ | @udp@).
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeClientVpnEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeClientVpnEndpoints_nextToken' - The token to retrieve the next page of results.
--
-- 'dryRun', 'describeClientVpnEndpoints_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeClientVpnEndpoints_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
--
-- 'clientVpnEndpointIds', 'describeClientVpnEndpoints_clientVpnEndpointIds' - The ID of the Client VPN endpoint.
--
-- 'filters', 'describeClientVpnEndpoints_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- -   @endpoint-id@ - The ID of the Client VPN endpoint.
--
-- -   @transport-protocol@ - The transport protocol (@tcp@ | @udp@).
newDescribeClientVpnEndpoints ::
  DescribeClientVpnEndpoints
newDescribeClientVpnEndpoints =
  DescribeClientVpnEndpoints'
    { nextToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      clientVpnEndpointIds = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token to retrieve the next page of results.
describeClientVpnEndpoints_nextToken :: Lens.Lens' DescribeClientVpnEndpoints (Prelude.Maybe Prelude.Text)
describeClientVpnEndpoints_nextToken = Lens.lens (\DescribeClientVpnEndpoints' {nextToken} -> nextToken) (\s@DescribeClientVpnEndpoints' {} a -> s {nextToken = a} :: DescribeClientVpnEndpoints)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeClientVpnEndpoints_dryRun :: Lens.Lens' DescribeClientVpnEndpoints (Prelude.Maybe Prelude.Bool)
describeClientVpnEndpoints_dryRun = Lens.lens (\DescribeClientVpnEndpoints' {dryRun} -> dryRun) (\s@DescribeClientVpnEndpoints' {} a -> s {dryRun = a} :: DescribeClientVpnEndpoints)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
describeClientVpnEndpoints_maxResults :: Lens.Lens' DescribeClientVpnEndpoints (Prelude.Maybe Prelude.Natural)
describeClientVpnEndpoints_maxResults = Lens.lens (\DescribeClientVpnEndpoints' {maxResults} -> maxResults) (\s@DescribeClientVpnEndpoints' {} a -> s {maxResults = a} :: DescribeClientVpnEndpoints)

-- | The ID of the Client VPN endpoint.
describeClientVpnEndpoints_clientVpnEndpointIds :: Lens.Lens' DescribeClientVpnEndpoints (Prelude.Maybe [Prelude.Text])
describeClientVpnEndpoints_clientVpnEndpointIds = Lens.lens (\DescribeClientVpnEndpoints' {clientVpnEndpointIds} -> clientVpnEndpointIds) (\s@DescribeClientVpnEndpoints' {} a -> s {clientVpnEndpointIds = a} :: DescribeClientVpnEndpoints) Prelude.. Lens.mapping Prelude._Coerce

-- | One or more filters. Filter names and values are case-sensitive.
--
-- -   @endpoint-id@ - The ID of the Client VPN endpoint.
--
-- -   @transport-protocol@ - The transport protocol (@tcp@ | @udp@).
describeClientVpnEndpoints_filters :: Lens.Lens' DescribeClientVpnEndpoints (Prelude.Maybe [Filter])
describeClientVpnEndpoints_filters = Lens.lens (\DescribeClientVpnEndpoints' {filters} -> filters) (\s@DescribeClientVpnEndpoints' {} a -> s {filters = a} :: DescribeClientVpnEndpoints) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager DescribeClientVpnEndpoints where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeClientVpnEndpointsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeClientVpnEndpointsResponse_clientVpnEndpoints
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeClientVpnEndpoints_nextToken
          Lens..~ rs
          Lens.^? describeClientVpnEndpointsResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeClientVpnEndpoints
  where
  type
    Rs DescribeClientVpnEndpoints =
      DescribeClientVpnEndpointsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeClientVpnEndpointsResponse'
            Prelude.<$> (x Prelude..@? "nextToken")
            Prelude.<*> ( x Prelude..@? "clientVpnEndpoint"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClientVpnEndpoints

instance Prelude.NFData DescribeClientVpnEndpoints

instance Prelude.ToHeaders DescribeClientVpnEndpoints where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeClientVpnEndpoints where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeClientVpnEndpoints where
  toQuery DescribeClientVpnEndpoints' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeClientVpnEndpoints" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "DryRun" Prelude.=: dryRun,
        "MaxResults" Prelude.=: maxResults,
        Prelude.toQuery
          ( Prelude.toQueryList "ClientVpnEndpointId"
              Prelude.<$> clientVpnEndpointIds
          ),
        Prelude.toQuery
          (Prelude.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeClientVpnEndpointsResponse' smart constructor.
data DescribeClientVpnEndpointsResponse = DescribeClientVpnEndpointsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the Client VPN endpoints.
    clientVpnEndpoints :: Prelude.Maybe [ClientVpnEndpoint],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeClientVpnEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeClientVpnEndpointsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'clientVpnEndpoints', 'describeClientVpnEndpointsResponse_clientVpnEndpoints' - Information about the Client VPN endpoints.
--
-- 'httpStatus', 'describeClientVpnEndpointsResponse_httpStatus' - The response's http status code.
newDescribeClientVpnEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClientVpnEndpointsResponse
newDescribeClientVpnEndpointsResponse pHttpStatus_ =
  DescribeClientVpnEndpointsResponse'
    { nextToken =
        Prelude.Nothing,
      clientVpnEndpoints = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeClientVpnEndpointsResponse_nextToken :: Lens.Lens' DescribeClientVpnEndpointsResponse (Prelude.Maybe Prelude.Text)
describeClientVpnEndpointsResponse_nextToken = Lens.lens (\DescribeClientVpnEndpointsResponse' {nextToken} -> nextToken) (\s@DescribeClientVpnEndpointsResponse' {} a -> s {nextToken = a} :: DescribeClientVpnEndpointsResponse)

-- | Information about the Client VPN endpoints.
describeClientVpnEndpointsResponse_clientVpnEndpoints :: Lens.Lens' DescribeClientVpnEndpointsResponse (Prelude.Maybe [ClientVpnEndpoint])
describeClientVpnEndpointsResponse_clientVpnEndpoints = Lens.lens (\DescribeClientVpnEndpointsResponse' {clientVpnEndpoints} -> clientVpnEndpoints) (\s@DescribeClientVpnEndpointsResponse' {} a -> s {clientVpnEndpoints = a} :: DescribeClientVpnEndpointsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeClientVpnEndpointsResponse_httpStatus :: Lens.Lens' DescribeClientVpnEndpointsResponse Prelude.Int
describeClientVpnEndpointsResponse_httpStatus = Lens.lens (\DescribeClientVpnEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeClientVpnEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeClientVpnEndpointsResponse)

instance
  Prelude.NFData
    DescribeClientVpnEndpointsResponse
