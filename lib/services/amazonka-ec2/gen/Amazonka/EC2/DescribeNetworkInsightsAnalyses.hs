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
-- Module      : Amazonka.EC2.DescribeNetworkInsightsAnalyses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your network insights analyses.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeNetworkInsightsAnalyses
  ( -- * Creating a Request
    DescribeNetworkInsightsAnalyses (..),
    newDescribeNetworkInsightsAnalyses,

    -- * Request Lenses
    describeNetworkInsightsAnalyses_analysisEndTime,
    describeNetworkInsightsAnalyses_analysisStartTime,
    describeNetworkInsightsAnalyses_dryRun,
    describeNetworkInsightsAnalyses_filters,
    describeNetworkInsightsAnalyses_maxResults,
    describeNetworkInsightsAnalyses_networkInsightsAnalysisIds,
    describeNetworkInsightsAnalyses_networkInsightsPathId,
    describeNetworkInsightsAnalyses_nextToken,

    -- * Destructuring the Response
    DescribeNetworkInsightsAnalysesResponse (..),
    newDescribeNetworkInsightsAnalysesResponse,

    -- * Response Lenses
    describeNetworkInsightsAnalysesResponse_networkInsightsAnalyses,
    describeNetworkInsightsAnalysesResponse_nextToken,
    describeNetworkInsightsAnalysesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeNetworkInsightsAnalyses' smart constructor.
data DescribeNetworkInsightsAnalyses = DescribeNetworkInsightsAnalyses'
  { -- | The time when the network insights analyses ended.
    analysisEndTime :: Prelude.Maybe Data.ISO8601,
    -- | The time when the network insights analyses started.
    analysisStartTime :: Prelude.Maybe Data.ISO8601,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The filters. The following are the possible values:
    --
    -- -   path-found - A Boolean value that indicates whether a feasible path
    --     is found.
    --
    -- -   status - The status of the analysis (running | succeeded | failed).
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the network insights analyses. You must specify either
    -- analysis IDs or a path ID.
    networkInsightsAnalysisIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the path. You must specify either a path ID or analysis IDs.
    networkInsightsPathId :: Prelude.Maybe Prelude.Text,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInsightsAnalyses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisEndTime', 'describeNetworkInsightsAnalyses_analysisEndTime' - The time when the network insights analyses ended.
--
-- 'analysisStartTime', 'describeNetworkInsightsAnalyses_analysisStartTime' - The time when the network insights analyses started.
--
-- 'dryRun', 'describeNetworkInsightsAnalyses_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeNetworkInsightsAnalyses_filters' - The filters. The following are the possible values:
--
-- -   path-found - A Boolean value that indicates whether a feasible path
--     is found.
--
-- -   status - The status of the analysis (running | succeeded | failed).
--
-- 'maxResults', 'describeNetworkInsightsAnalyses_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'networkInsightsAnalysisIds', 'describeNetworkInsightsAnalyses_networkInsightsAnalysisIds' - The ID of the network insights analyses. You must specify either
-- analysis IDs or a path ID.
--
-- 'networkInsightsPathId', 'describeNetworkInsightsAnalyses_networkInsightsPathId' - The ID of the path. You must specify either a path ID or analysis IDs.
--
-- 'nextToken', 'describeNetworkInsightsAnalyses_nextToken' - The token for the next page of results.
newDescribeNetworkInsightsAnalyses ::
  DescribeNetworkInsightsAnalyses
newDescribeNetworkInsightsAnalyses =
  DescribeNetworkInsightsAnalyses'
    { analysisEndTime =
        Prelude.Nothing,
      analysisStartTime = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      networkInsightsAnalysisIds =
        Prelude.Nothing,
      networkInsightsPathId = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The time when the network insights analyses ended.
describeNetworkInsightsAnalyses_analysisEndTime :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe Prelude.UTCTime)
describeNetworkInsightsAnalyses_analysisEndTime = Lens.lens (\DescribeNetworkInsightsAnalyses' {analysisEndTime} -> analysisEndTime) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {analysisEndTime = a} :: DescribeNetworkInsightsAnalyses) Prelude.. Lens.mapping Data._Time

-- | The time when the network insights analyses started.
describeNetworkInsightsAnalyses_analysisStartTime :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe Prelude.UTCTime)
describeNetworkInsightsAnalyses_analysisStartTime = Lens.lens (\DescribeNetworkInsightsAnalyses' {analysisStartTime} -> analysisStartTime) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {analysisStartTime = a} :: DescribeNetworkInsightsAnalyses) Prelude.. Lens.mapping Data._Time

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeNetworkInsightsAnalyses_dryRun :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe Prelude.Bool)
describeNetworkInsightsAnalyses_dryRun = Lens.lens (\DescribeNetworkInsightsAnalyses' {dryRun} -> dryRun) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {dryRun = a} :: DescribeNetworkInsightsAnalyses)

-- | The filters. The following are the possible values:
--
-- -   path-found - A Boolean value that indicates whether a feasible path
--     is found.
--
-- -   status - The status of the analysis (running | succeeded | failed).
describeNetworkInsightsAnalyses_filters :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe [Filter])
describeNetworkInsightsAnalyses_filters = Lens.lens (\DescribeNetworkInsightsAnalyses' {filters} -> filters) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {filters = a} :: DescribeNetworkInsightsAnalyses) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeNetworkInsightsAnalyses_maxResults :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe Prelude.Natural)
describeNetworkInsightsAnalyses_maxResults = Lens.lens (\DescribeNetworkInsightsAnalyses' {maxResults} -> maxResults) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {maxResults = a} :: DescribeNetworkInsightsAnalyses)

-- | The ID of the network insights analyses. You must specify either
-- analysis IDs or a path ID.
describeNetworkInsightsAnalyses_networkInsightsAnalysisIds :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe [Prelude.Text])
describeNetworkInsightsAnalyses_networkInsightsAnalysisIds = Lens.lens (\DescribeNetworkInsightsAnalyses' {networkInsightsAnalysisIds} -> networkInsightsAnalysisIds) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {networkInsightsAnalysisIds = a} :: DescribeNetworkInsightsAnalyses) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the path. You must specify either a path ID or analysis IDs.
describeNetworkInsightsAnalyses_networkInsightsPathId :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe Prelude.Text)
describeNetworkInsightsAnalyses_networkInsightsPathId = Lens.lens (\DescribeNetworkInsightsAnalyses' {networkInsightsPathId} -> networkInsightsPathId) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {networkInsightsPathId = a} :: DescribeNetworkInsightsAnalyses)

-- | The token for the next page of results.
describeNetworkInsightsAnalyses_nextToken :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe Prelude.Text)
describeNetworkInsightsAnalyses_nextToken = Lens.lens (\DescribeNetworkInsightsAnalyses' {nextToken} -> nextToken) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {nextToken = a} :: DescribeNetworkInsightsAnalyses)

instance
  Core.AWSPager
    DescribeNetworkInsightsAnalyses
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNetworkInsightsAnalysesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeNetworkInsightsAnalysesResponse_networkInsightsAnalyses
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeNetworkInsightsAnalyses_nextToken
          Lens..~ rs
          Lens.^? describeNetworkInsightsAnalysesResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeNetworkInsightsAnalyses
  where
  type
    AWSResponse DescribeNetworkInsightsAnalyses =
      DescribeNetworkInsightsAnalysesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNetworkInsightsAnalysesResponse'
            Prelude.<$> ( x
                            Data..@? "networkInsightsAnalysisSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeNetworkInsightsAnalyses
  where
  hashWithSalt
    _salt
    DescribeNetworkInsightsAnalyses' {..} =
      _salt
        `Prelude.hashWithSalt` analysisEndTime
        `Prelude.hashWithSalt` analysisStartTime
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` networkInsightsAnalysisIds
        `Prelude.hashWithSalt` networkInsightsPathId
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeNetworkInsightsAnalyses
  where
  rnf DescribeNetworkInsightsAnalyses' {..} =
    Prelude.rnf analysisEndTime
      `Prelude.seq` Prelude.rnf analysisStartTime
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf networkInsightsAnalysisIds
      `Prelude.seq` Prelude.rnf networkInsightsPathId
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeNetworkInsightsAnalyses
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeNetworkInsightsAnalyses where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeNetworkInsightsAnalyses where
  toQuery DescribeNetworkInsightsAnalyses' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeNetworkInsightsAnalyses" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AnalysisEndTime" Data.=: analysisEndTime,
        "AnalysisStartTime" Data.=: analysisStartTime,
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        Data.toQuery
          ( Data.toQueryList "NetworkInsightsAnalysisId"
              Prelude.<$> networkInsightsAnalysisIds
          ),
        "NetworkInsightsPathId"
          Data.=: networkInsightsPathId,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeNetworkInsightsAnalysesResponse' smart constructor.
data DescribeNetworkInsightsAnalysesResponse = DescribeNetworkInsightsAnalysesResponse'
  { -- | Information about the network insights analyses.
    networkInsightsAnalyses :: Prelude.Maybe [NetworkInsightsAnalysis],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInsightsAnalysesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInsightsAnalyses', 'describeNetworkInsightsAnalysesResponse_networkInsightsAnalyses' - Information about the network insights analyses.
--
-- 'nextToken', 'describeNetworkInsightsAnalysesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeNetworkInsightsAnalysesResponse_httpStatus' - The response's http status code.
newDescribeNetworkInsightsAnalysesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNetworkInsightsAnalysesResponse
newDescribeNetworkInsightsAnalysesResponse
  pHttpStatus_ =
    DescribeNetworkInsightsAnalysesResponse'
      { networkInsightsAnalyses =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the network insights analyses.
describeNetworkInsightsAnalysesResponse_networkInsightsAnalyses :: Lens.Lens' DescribeNetworkInsightsAnalysesResponse (Prelude.Maybe [NetworkInsightsAnalysis])
describeNetworkInsightsAnalysesResponse_networkInsightsAnalyses = Lens.lens (\DescribeNetworkInsightsAnalysesResponse' {networkInsightsAnalyses} -> networkInsightsAnalyses) (\s@DescribeNetworkInsightsAnalysesResponse' {} a -> s {networkInsightsAnalyses = a} :: DescribeNetworkInsightsAnalysesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeNetworkInsightsAnalysesResponse_nextToken :: Lens.Lens' DescribeNetworkInsightsAnalysesResponse (Prelude.Maybe Prelude.Text)
describeNetworkInsightsAnalysesResponse_nextToken = Lens.lens (\DescribeNetworkInsightsAnalysesResponse' {nextToken} -> nextToken) (\s@DescribeNetworkInsightsAnalysesResponse' {} a -> s {nextToken = a} :: DescribeNetworkInsightsAnalysesResponse)

-- | The response's http status code.
describeNetworkInsightsAnalysesResponse_httpStatus :: Lens.Lens' DescribeNetworkInsightsAnalysesResponse Prelude.Int
describeNetworkInsightsAnalysesResponse_httpStatus = Lens.lens (\DescribeNetworkInsightsAnalysesResponse' {httpStatus} -> httpStatus) (\s@DescribeNetworkInsightsAnalysesResponse' {} a -> s {httpStatus = a} :: DescribeNetworkInsightsAnalysesResponse)

instance
  Prelude.NFData
    DescribeNetworkInsightsAnalysesResponse
  where
  rnf DescribeNetworkInsightsAnalysesResponse' {..} =
    Prelude.rnf networkInsightsAnalyses
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
