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
-- Module      : Amazonka.EC2.DescribeVerifiedAccessEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe Amazon Web Services Verified Access endpoints.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeVerifiedAccessEndpoints
  ( -- * Creating a Request
    DescribeVerifiedAccessEndpoints (..),
    newDescribeVerifiedAccessEndpoints,

    -- * Request Lenses
    describeVerifiedAccessEndpoints_dryRun,
    describeVerifiedAccessEndpoints_filters,
    describeVerifiedAccessEndpoints_maxResults,
    describeVerifiedAccessEndpoints_nextToken,
    describeVerifiedAccessEndpoints_verifiedAccessEndpointIds,
    describeVerifiedAccessEndpoints_verifiedAccessGroupId,
    describeVerifiedAccessEndpoints_verifiedAccessInstanceId,

    -- * Destructuring the Response
    DescribeVerifiedAccessEndpointsResponse (..),
    newDescribeVerifiedAccessEndpointsResponse,

    -- * Response Lenses
    describeVerifiedAccessEndpointsResponse_nextToken,
    describeVerifiedAccessEndpointsResponse_verifiedAccessEndpoints,
    describeVerifiedAccessEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVerifiedAccessEndpoints' smart constructor.
data DescribeVerifiedAccessEndpoints = DescribeVerifiedAccessEndpoints'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters. Filter names and values are case-sensitive.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services Verified Access endpoint.
    verifiedAccessEndpointIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the Amazon Web Services Verified Access group.
    verifiedAccessGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstanceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVerifiedAccessEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVerifiedAccessEndpoints_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeVerifiedAccessEndpoints_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- 'maxResults', 'describeVerifiedAccessEndpoints_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeVerifiedAccessEndpoints_nextToken' - The token for the next page of results.
--
-- 'verifiedAccessEndpointIds', 'describeVerifiedAccessEndpoints_verifiedAccessEndpointIds' - The ID of the Amazon Web Services Verified Access endpoint.
--
-- 'verifiedAccessGroupId', 'describeVerifiedAccessEndpoints_verifiedAccessGroupId' - The ID of the Amazon Web Services Verified Access group.
--
-- 'verifiedAccessInstanceId', 'describeVerifiedAccessEndpoints_verifiedAccessInstanceId' - The ID of the Amazon Web Services Verified Access instance.
newDescribeVerifiedAccessEndpoints ::
  DescribeVerifiedAccessEndpoints
newDescribeVerifiedAccessEndpoints =
  DescribeVerifiedAccessEndpoints'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      verifiedAccessEndpointIds =
        Prelude.Nothing,
      verifiedAccessGroupId = Prelude.Nothing,
      verifiedAccessInstanceId = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVerifiedAccessEndpoints_dryRun :: Lens.Lens' DescribeVerifiedAccessEndpoints (Prelude.Maybe Prelude.Bool)
describeVerifiedAccessEndpoints_dryRun = Lens.lens (\DescribeVerifiedAccessEndpoints' {dryRun} -> dryRun) (\s@DescribeVerifiedAccessEndpoints' {} a -> s {dryRun = a} :: DescribeVerifiedAccessEndpoints)

-- | One or more filters. Filter names and values are case-sensitive.
describeVerifiedAccessEndpoints_filters :: Lens.Lens' DescribeVerifiedAccessEndpoints (Prelude.Maybe [Filter])
describeVerifiedAccessEndpoints_filters = Lens.lens (\DescribeVerifiedAccessEndpoints' {filters} -> filters) (\s@DescribeVerifiedAccessEndpoints' {} a -> s {filters = a} :: DescribeVerifiedAccessEndpoints) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeVerifiedAccessEndpoints_maxResults :: Lens.Lens' DescribeVerifiedAccessEndpoints (Prelude.Maybe Prelude.Natural)
describeVerifiedAccessEndpoints_maxResults = Lens.lens (\DescribeVerifiedAccessEndpoints' {maxResults} -> maxResults) (\s@DescribeVerifiedAccessEndpoints' {} a -> s {maxResults = a} :: DescribeVerifiedAccessEndpoints)

-- | The token for the next page of results.
describeVerifiedAccessEndpoints_nextToken :: Lens.Lens' DescribeVerifiedAccessEndpoints (Prelude.Maybe Prelude.Text)
describeVerifiedAccessEndpoints_nextToken = Lens.lens (\DescribeVerifiedAccessEndpoints' {nextToken} -> nextToken) (\s@DescribeVerifiedAccessEndpoints' {} a -> s {nextToken = a} :: DescribeVerifiedAccessEndpoints)

-- | The ID of the Amazon Web Services Verified Access endpoint.
describeVerifiedAccessEndpoints_verifiedAccessEndpointIds :: Lens.Lens' DescribeVerifiedAccessEndpoints (Prelude.Maybe [Prelude.Text])
describeVerifiedAccessEndpoints_verifiedAccessEndpointIds = Lens.lens (\DescribeVerifiedAccessEndpoints' {verifiedAccessEndpointIds} -> verifiedAccessEndpointIds) (\s@DescribeVerifiedAccessEndpoints' {} a -> s {verifiedAccessEndpointIds = a} :: DescribeVerifiedAccessEndpoints) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services Verified Access group.
describeVerifiedAccessEndpoints_verifiedAccessGroupId :: Lens.Lens' DescribeVerifiedAccessEndpoints (Prelude.Maybe Prelude.Text)
describeVerifiedAccessEndpoints_verifiedAccessGroupId = Lens.lens (\DescribeVerifiedAccessEndpoints' {verifiedAccessGroupId} -> verifiedAccessGroupId) (\s@DescribeVerifiedAccessEndpoints' {} a -> s {verifiedAccessGroupId = a} :: DescribeVerifiedAccessEndpoints)

-- | The ID of the Amazon Web Services Verified Access instance.
describeVerifiedAccessEndpoints_verifiedAccessInstanceId :: Lens.Lens' DescribeVerifiedAccessEndpoints (Prelude.Maybe Prelude.Text)
describeVerifiedAccessEndpoints_verifiedAccessInstanceId = Lens.lens (\DescribeVerifiedAccessEndpoints' {verifiedAccessInstanceId} -> verifiedAccessInstanceId) (\s@DescribeVerifiedAccessEndpoints' {} a -> s {verifiedAccessInstanceId = a} :: DescribeVerifiedAccessEndpoints)

instance
  Core.AWSPager
    DescribeVerifiedAccessEndpoints
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVerifiedAccessEndpointsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVerifiedAccessEndpointsResponse_verifiedAccessEndpoints
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeVerifiedAccessEndpoints_nextToken
              Lens..~ rs
              Lens.^? describeVerifiedAccessEndpointsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeVerifiedAccessEndpoints
  where
  type
    AWSResponse DescribeVerifiedAccessEndpoints =
      DescribeVerifiedAccessEndpointsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVerifiedAccessEndpointsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "verifiedAccessEndpointSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeVerifiedAccessEndpoints
  where
  hashWithSalt
    _salt
    DescribeVerifiedAccessEndpoints' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` verifiedAccessEndpointIds
        `Prelude.hashWithSalt` verifiedAccessGroupId
        `Prelude.hashWithSalt` verifiedAccessInstanceId

instance
  Prelude.NFData
    DescribeVerifiedAccessEndpoints
  where
  rnf DescribeVerifiedAccessEndpoints' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf filters `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf verifiedAccessEndpointIds `Prelude.seq`
              Prelude.rnf verifiedAccessGroupId `Prelude.seq`
                Prelude.rnf verifiedAccessInstanceId

instance
  Data.ToHeaders
    DescribeVerifiedAccessEndpoints
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeVerifiedAccessEndpoints where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeVerifiedAccessEndpoints where
  toQuery DescribeVerifiedAccessEndpoints' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeVerifiedAccessEndpoints" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "VerifiedAccessEndpointId"
              Prelude.<$> verifiedAccessEndpointIds
          ),
        "VerifiedAccessGroupId"
          Data.=: verifiedAccessGroupId,
        "VerifiedAccessInstanceId"
          Data.=: verifiedAccessInstanceId
      ]

-- | /See:/ 'newDescribeVerifiedAccessEndpointsResponse' smart constructor.
data DescribeVerifiedAccessEndpointsResponse = DescribeVerifiedAccessEndpointsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services Verified Access endpoint.
    verifiedAccessEndpoints :: Prelude.Maybe [VerifiedAccessEndpoint],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVerifiedAccessEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVerifiedAccessEndpointsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'verifiedAccessEndpoints', 'describeVerifiedAccessEndpointsResponse_verifiedAccessEndpoints' - The ID of the Amazon Web Services Verified Access endpoint.
--
-- 'httpStatus', 'describeVerifiedAccessEndpointsResponse_httpStatus' - The response's http status code.
newDescribeVerifiedAccessEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVerifiedAccessEndpointsResponse
newDescribeVerifiedAccessEndpointsResponse
  pHttpStatus_ =
    DescribeVerifiedAccessEndpointsResponse'
      { nextToken =
          Prelude.Nothing,
        verifiedAccessEndpoints =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVerifiedAccessEndpointsResponse_nextToken :: Lens.Lens' DescribeVerifiedAccessEndpointsResponse (Prelude.Maybe Prelude.Text)
describeVerifiedAccessEndpointsResponse_nextToken = Lens.lens (\DescribeVerifiedAccessEndpointsResponse' {nextToken} -> nextToken) (\s@DescribeVerifiedAccessEndpointsResponse' {} a -> s {nextToken = a} :: DescribeVerifiedAccessEndpointsResponse)

-- | The ID of the Amazon Web Services Verified Access endpoint.
describeVerifiedAccessEndpointsResponse_verifiedAccessEndpoints :: Lens.Lens' DescribeVerifiedAccessEndpointsResponse (Prelude.Maybe [VerifiedAccessEndpoint])
describeVerifiedAccessEndpointsResponse_verifiedAccessEndpoints = Lens.lens (\DescribeVerifiedAccessEndpointsResponse' {verifiedAccessEndpoints} -> verifiedAccessEndpoints) (\s@DescribeVerifiedAccessEndpointsResponse' {} a -> s {verifiedAccessEndpoints = a} :: DescribeVerifiedAccessEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeVerifiedAccessEndpointsResponse_httpStatus :: Lens.Lens' DescribeVerifiedAccessEndpointsResponse Prelude.Int
describeVerifiedAccessEndpointsResponse_httpStatus = Lens.lens (\DescribeVerifiedAccessEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeVerifiedAccessEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeVerifiedAccessEndpointsResponse)

instance
  Prelude.NFData
    DescribeVerifiedAccessEndpointsResponse
  where
  rnf DescribeVerifiedAccessEndpointsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf verifiedAccessEndpoints `Prelude.seq`
        Prelude.rnf httpStatus
