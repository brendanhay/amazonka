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
-- Module      : Network.AWS.EC2.DescribeNetworkInsightsPaths
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your paths.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeNetworkInsightsPaths
  ( -- * Creating a Request
    DescribeNetworkInsightsPaths (..),
    newDescribeNetworkInsightsPaths,

    -- * Request Lenses
    describeNetworkInsightsPaths_nextToken,
    describeNetworkInsightsPaths_dryRun,
    describeNetworkInsightsPaths_maxResults,
    describeNetworkInsightsPaths_networkInsightsPathIds,
    describeNetworkInsightsPaths_filters,

    -- * Destructuring the Response
    DescribeNetworkInsightsPathsResponse (..),
    newDescribeNetworkInsightsPathsResponse,

    -- * Response Lenses
    describeNetworkInsightsPathsResponse_nextToken,
    describeNetworkInsightsPathsResponse_networkInsightsPaths,
    describeNetworkInsightsPathsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeNetworkInsightsPaths' smart constructor.
data DescribeNetworkInsightsPaths = DescribeNetworkInsightsPaths'
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
    -- | The IDs of the paths.
    networkInsightsPathIds :: Prelude.Maybe [Prelude.Text],
    -- | The filters. The following are possible values:
    --
    -- -   Destination - The ID of the resource.
    --
    -- -   DestinationPort - The destination port.
    --
    -- -   Name - The path name.
    --
    -- -   Protocol - The protocol.
    --
    -- -   Source - The ID of the resource.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInsightsPaths' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeNetworkInsightsPaths_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeNetworkInsightsPaths_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeNetworkInsightsPaths_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'networkInsightsPathIds', 'describeNetworkInsightsPaths_networkInsightsPathIds' - The IDs of the paths.
--
-- 'filters', 'describeNetworkInsightsPaths_filters' - The filters. The following are possible values:
--
-- -   Destination - The ID of the resource.
--
-- -   DestinationPort - The destination port.
--
-- -   Name - The path name.
--
-- -   Protocol - The protocol.
--
-- -   Source - The ID of the resource.
newDescribeNetworkInsightsPaths ::
  DescribeNetworkInsightsPaths
newDescribeNetworkInsightsPaths =
  DescribeNetworkInsightsPaths'
    { nextToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      networkInsightsPathIds = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token for the next page of results.
describeNetworkInsightsPaths_nextToken :: Lens.Lens' DescribeNetworkInsightsPaths (Prelude.Maybe Prelude.Text)
describeNetworkInsightsPaths_nextToken = Lens.lens (\DescribeNetworkInsightsPaths' {nextToken} -> nextToken) (\s@DescribeNetworkInsightsPaths' {} a -> s {nextToken = a} :: DescribeNetworkInsightsPaths)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeNetworkInsightsPaths_dryRun :: Lens.Lens' DescribeNetworkInsightsPaths (Prelude.Maybe Prelude.Bool)
describeNetworkInsightsPaths_dryRun = Lens.lens (\DescribeNetworkInsightsPaths' {dryRun} -> dryRun) (\s@DescribeNetworkInsightsPaths' {} a -> s {dryRun = a} :: DescribeNetworkInsightsPaths)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeNetworkInsightsPaths_maxResults :: Lens.Lens' DescribeNetworkInsightsPaths (Prelude.Maybe Prelude.Natural)
describeNetworkInsightsPaths_maxResults = Lens.lens (\DescribeNetworkInsightsPaths' {maxResults} -> maxResults) (\s@DescribeNetworkInsightsPaths' {} a -> s {maxResults = a} :: DescribeNetworkInsightsPaths)

-- | The IDs of the paths.
describeNetworkInsightsPaths_networkInsightsPathIds :: Lens.Lens' DescribeNetworkInsightsPaths (Prelude.Maybe [Prelude.Text])
describeNetworkInsightsPaths_networkInsightsPathIds = Lens.lens (\DescribeNetworkInsightsPaths' {networkInsightsPathIds} -> networkInsightsPathIds) (\s@DescribeNetworkInsightsPaths' {} a -> s {networkInsightsPathIds = a} :: DescribeNetworkInsightsPaths) Prelude.. Lens.mapping Lens._Coerce

-- | The filters. The following are possible values:
--
-- -   Destination - The ID of the resource.
--
-- -   DestinationPort - The destination port.
--
-- -   Name - The path name.
--
-- -   Protocol - The protocol.
--
-- -   Source - The ID of the resource.
describeNetworkInsightsPaths_filters :: Lens.Lens' DescribeNetworkInsightsPaths (Prelude.Maybe [Filter])
describeNetworkInsightsPaths_filters = Lens.lens (\DescribeNetworkInsightsPaths' {filters} -> filters) (\s@DescribeNetworkInsightsPaths' {} a -> s {filters = a} :: DescribeNetworkInsightsPaths) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeNetworkInsightsPaths where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNetworkInsightsPathsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeNetworkInsightsPathsResponse_networkInsightsPaths
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeNetworkInsightsPaths_nextToken
          Lens..~ rs
          Lens.^? describeNetworkInsightsPathsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeNetworkInsightsPaths where
  type
    AWSResponse DescribeNetworkInsightsPaths =
      DescribeNetworkInsightsPathsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNetworkInsightsPathsResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "networkInsightsPathSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeNetworkInsightsPaths

instance Prelude.NFData DescribeNetworkInsightsPaths

instance Core.ToHeaders DescribeNetworkInsightsPaths where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeNetworkInsightsPaths where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeNetworkInsightsPaths where
  toQuery DescribeNetworkInsightsPaths' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeNetworkInsightsPaths" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "NetworkInsightsPathId"
              Prelude.<$> networkInsightsPathIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeNetworkInsightsPathsResponse' smart constructor.
data DescribeNetworkInsightsPathsResponse = DescribeNetworkInsightsPathsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the paths.
    networkInsightsPaths :: Prelude.Maybe [NetworkInsightsPath],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInsightsPathsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeNetworkInsightsPathsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'networkInsightsPaths', 'describeNetworkInsightsPathsResponse_networkInsightsPaths' - Information about the paths.
--
-- 'httpStatus', 'describeNetworkInsightsPathsResponse_httpStatus' - The response's http status code.
newDescribeNetworkInsightsPathsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNetworkInsightsPathsResponse
newDescribeNetworkInsightsPathsResponse pHttpStatus_ =
  DescribeNetworkInsightsPathsResponse'
    { nextToken =
        Prelude.Nothing,
      networkInsightsPaths =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeNetworkInsightsPathsResponse_nextToken :: Lens.Lens' DescribeNetworkInsightsPathsResponse (Prelude.Maybe Prelude.Text)
describeNetworkInsightsPathsResponse_nextToken = Lens.lens (\DescribeNetworkInsightsPathsResponse' {nextToken} -> nextToken) (\s@DescribeNetworkInsightsPathsResponse' {} a -> s {nextToken = a} :: DescribeNetworkInsightsPathsResponse)

-- | Information about the paths.
describeNetworkInsightsPathsResponse_networkInsightsPaths :: Lens.Lens' DescribeNetworkInsightsPathsResponse (Prelude.Maybe [NetworkInsightsPath])
describeNetworkInsightsPathsResponse_networkInsightsPaths = Lens.lens (\DescribeNetworkInsightsPathsResponse' {networkInsightsPaths} -> networkInsightsPaths) (\s@DescribeNetworkInsightsPathsResponse' {} a -> s {networkInsightsPaths = a} :: DescribeNetworkInsightsPathsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeNetworkInsightsPathsResponse_httpStatus :: Lens.Lens' DescribeNetworkInsightsPathsResponse Prelude.Int
describeNetworkInsightsPathsResponse_httpStatus = Lens.lens (\DescribeNetworkInsightsPathsResponse' {httpStatus} -> httpStatus) (\s@DescribeNetworkInsightsPathsResponse' {} a -> s {httpStatus = a} :: DescribeNetworkInsightsPathsResponse)

instance
  Prelude.NFData
    DescribeNetworkInsightsPathsResponse
