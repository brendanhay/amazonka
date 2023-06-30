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
-- Module      : Amazonka.EC2.DescribeNetworkInsightsPaths
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your paths.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeNetworkInsightsPaths
  ( -- * Creating a Request
    DescribeNetworkInsightsPaths (..),
    newDescribeNetworkInsightsPaths,

    -- * Request Lenses
    describeNetworkInsightsPaths_dryRun,
    describeNetworkInsightsPaths_filters,
    describeNetworkInsightsPaths_maxResults,
    describeNetworkInsightsPaths_networkInsightsPathIds,
    describeNetworkInsightsPaths_nextToken,

    -- * Destructuring the Response
    DescribeNetworkInsightsPathsResponse (..),
    newDescribeNetworkInsightsPathsResponse,

    -- * Response Lenses
    describeNetworkInsightsPathsResponse_networkInsightsPaths,
    describeNetworkInsightsPathsResponse_nextToken,
    describeNetworkInsightsPathsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeNetworkInsightsPaths' smart constructor.
data DescribeNetworkInsightsPaths = DescribeNetworkInsightsPaths'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The filters. The following are the possible values:
    --
    -- -   destination - The ID of the resource.
    --
    -- -   destination-port - The destination port.
    --
    -- -   protocol - The protocol.
    --
    -- -   source - The ID of the resource.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The IDs of the paths.
    networkInsightsPathIds :: Prelude.Maybe [Prelude.Text],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'dryRun', 'describeNetworkInsightsPaths_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeNetworkInsightsPaths_filters' - The filters. The following are the possible values:
--
-- -   destination - The ID of the resource.
--
-- -   destination-port - The destination port.
--
-- -   protocol - The protocol.
--
-- -   source - The ID of the resource.
--
-- 'maxResults', 'describeNetworkInsightsPaths_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'networkInsightsPathIds', 'describeNetworkInsightsPaths_networkInsightsPathIds' - The IDs of the paths.
--
-- 'nextToken', 'describeNetworkInsightsPaths_nextToken' - The token for the next page of results.
newDescribeNetworkInsightsPaths ::
  DescribeNetworkInsightsPaths
newDescribeNetworkInsightsPaths =
  DescribeNetworkInsightsPaths'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      networkInsightsPathIds = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeNetworkInsightsPaths_dryRun :: Lens.Lens' DescribeNetworkInsightsPaths (Prelude.Maybe Prelude.Bool)
describeNetworkInsightsPaths_dryRun = Lens.lens (\DescribeNetworkInsightsPaths' {dryRun} -> dryRun) (\s@DescribeNetworkInsightsPaths' {} a -> s {dryRun = a} :: DescribeNetworkInsightsPaths)

-- | The filters. The following are the possible values:
--
-- -   destination - The ID of the resource.
--
-- -   destination-port - The destination port.
--
-- -   protocol - The protocol.
--
-- -   source - The ID of the resource.
describeNetworkInsightsPaths_filters :: Lens.Lens' DescribeNetworkInsightsPaths (Prelude.Maybe [Filter])
describeNetworkInsightsPaths_filters = Lens.lens (\DescribeNetworkInsightsPaths' {filters} -> filters) (\s@DescribeNetworkInsightsPaths' {} a -> s {filters = a} :: DescribeNetworkInsightsPaths) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeNetworkInsightsPaths_maxResults :: Lens.Lens' DescribeNetworkInsightsPaths (Prelude.Maybe Prelude.Natural)
describeNetworkInsightsPaths_maxResults = Lens.lens (\DescribeNetworkInsightsPaths' {maxResults} -> maxResults) (\s@DescribeNetworkInsightsPaths' {} a -> s {maxResults = a} :: DescribeNetworkInsightsPaths)

-- | The IDs of the paths.
describeNetworkInsightsPaths_networkInsightsPathIds :: Lens.Lens' DescribeNetworkInsightsPaths (Prelude.Maybe [Prelude.Text])
describeNetworkInsightsPaths_networkInsightsPathIds = Lens.lens (\DescribeNetworkInsightsPaths' {networkInsightsPathIds} -> networkInsightsPathIds) (\s@DescribeNetworkInsightsPaths' {} a -> s {networkInsightsPathIds = a} :: DescribeNetworkInsightsPaths) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
describeNetworkInsightsPaths_nextToken :: Lens.Lens' DescribeNetworkInsightsPaths (Prelude.Maybe Prelude.Text)
describeNetworkInsightsPaths_nextToken = Lens.lens (\DescribeNetworkInsightsPaths' {nextToken} -> nextToken) (\s@DescribeNetworkInsightsPaths' {} a -> s {nextToken = a} :: DescribeNetworkInsightsPaths)

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeNetworkInsightsPaths_nextToken
          Lens..~ rs
          Lens.^? describeNetworkInsightsPathsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeNetworkInsightsPaths where
  type
    AWSResponse DescribeNetworkInsightsPaths =
      DescribeNetworkInsightsPathsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNetworkInsightsPathsResponse'
            Prelude.<$> ( x
                            Data..@? "networkInsightsPathSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeNetworkInsightsPaths
  where
  hashWithSalt _salt DescribeNetworkInsightsPaths' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` networkInsightsPathIds
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeNetworkInsightsPaths where
  rnf DescribeNetworkInsightsPaths' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf networkInsightsPathIds
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeNetworkInsightsPaths where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeNetworkInsightsPaths where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeNetworkInsightsPaths where
  toQuery DescribeNetworkInsightsPaths' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeNetworkInsightsPaths" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        Data.toQuery
          ( Data.toQueryList "NetworkInsightsPathId"
              Prelude.<$> networkInsightsPathIds
          ),
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeNetworkInsightsPathsResponse' smart constructor.
data DescribeNetworkInsightsPathsResponse = DescribeNetworkInsightsPathsResponse'
  { -- | Information about the paths.
    networkInsightsPaths :: Prelude.Maybe [NetworkInsightsPath],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'networkInsightsPaths', 'describeNetworkInsightsPathsResponse_networkInsightsPaths' - Information about the paths.
--
-- 'nextToken', 'describeNetworkInsightsPathsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeNetworkInsightsPathsResponse_httpStatus' - The response's http status code.
newDescribeNetworkInsightsPathsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNetworkInsightsPathsResponse
newDescribeNetworkInsightsPathsResponse pHttpStatus_ =
  DescribeNetworkInsightsPathsResponse'
    { networkInsightsPaths =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the paths.
describeNetworkInsightsPathsResponse_networkInsightsPaths :: Lens.Lens' DescribeNetworkInsightsPathsResponse (Prelude.Maybe [NetworkInsightsPath])
describeNetworkInsightsPathsResponse_networkInsightsPaths = Lens.lens (\DescribeNetworkInsightsPathsResponse' {networkInsightsPaths} -> networkInsightsPaths) (\s@DescribeNetworkInsightsPathsResponse' {} a -> s {networkInsightsPaths = a} :: DescribeNetworkInsightsPathsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeNetworkInsightsPathsResponse_nextToken :: Lens.Lens' DescribeNetworkInsightsPathsResponse (Prelude.Maybe Prelude.Text)
describeNetworkInsightsPathsResponse_nextToken = Lens.lens (\DescribeNetworkInsightsPathsResponse' {nextToken} -> nextToken) (\s@DescribeNetworkInsightsPathsResponse' {} a -> s {nextToken = a} :: DescribeNetworkInsightsPathsResponse)

-- | The response's http status code.
describeNetworkInsightsPathsResponse_httpStatus :: Lens.Lens' DescribeNetworkInsightsPathsResponse Prelude.Int
describeNetworkInsightsPathsResponse_httpStatus = Lens.lens (\DescribeNetworkInsightsPathsResponse' {httpStatus} -> httpStatus) (\s@DescribeNetworkInsightsPathsResponse' {} a -> s {httpStatus = a} :: DescribeNetworkInsightsPathsResponse)

instance
  Prelude.NFData
    DescribeNetworkInsightsPathsResponse
  where
  rnf DescribeNetworkInsightsPathsResponse' {..} =
    Prelude.rnf networkInsightsPaths
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
