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
-- Module      : Network.AWS.EC2.DescribeNetworkInsightsAnalyses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your network insights analyses.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeNetworkInsightsAnalyses
  ( -- * Creating a Request
    DescribeNetworkInsightsAnalyses (..),
    newDescribeNetworkInsightsAnalyses,

    -- * Request Lenses
    describeNetworkInsightsAnalyses_analysisStartTime,
    describeNetworkInsightsAnalyses_networkInsightsAnalysisIds,
    describeNetworkInsightsAnalyses_nextToken,
    describeNetworkInsightsAnalyses_dryRun,
    describeNetworkInsightsAnalyses_maxResults,
    describeNetworkInsightsAnalyses_filters,
    describeNetworkInsightsAnalyses_networkInsightsPathId,
    describeNetworkInsightsAnalyses_analysisEndTime,

    -- * Destructuring the Response
    DescribeNetworkInsightsAnalysesResponse (..),
    newDescribeNetworkInsightsAnalysesResponse,

    -- * Response Lenses
    describeNetworkInsightsAnalysesResponse_nextToken,
    describeNetworkInsightsAnalysesResponse_networkInsightsAnalyses,
    describeNetworkInsightsAnalysesResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeNetworkInsightsAnalyses' smart constructor.
data DescribeNetworkInsightsAnalyses = DescribeNetworkInsightsAnalyses'
  { -- | The time when the network insights analyses started.
    analysisStartTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The ID of the network insights analyses. You must specify either
    -- analysis IDs or a path ID.
    networkInsightsAnalysisIds :: Prelude.Maybe [Prelude.Text],
    -- | The token for the next page of results.
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
    -- | The filters. The following are possible values:
    --
    -- -   PathFound - A Boolean value that indicates whether a feasible path
    --     is found.
    --
    -- -   Status - The status of the analysis (running | succeeded | failed).
    filters :: Prelude.Maybe [Filter],
    -- | The ID of the path. You must specify either a path ID or analysis IDs.
    networkInsightsPathId :: Prelude.Maybe Prelude.Text,
    -- | The time when the network insights analyses ended.
    analysisEndTime :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInsightsAnalyses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisStartTime', 'describeNetworkInsightsAnalyses_analysisStartTime' - The time when the network insights analyses started.
--
-- 'networkInsightsAnalysisIds', 'describeNetworkInsightsAnalyses_networkInsightsAnalysisIds' - The ID of the network insights analyses. You must specify either
-- analysis IDs or a path ID.
--
-- 'nextToken', 'describeNetworkInsightsAnalyses_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeNetworkInsightsAnalyses_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeNetworkInsightsAnalyses_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'describeNetworkInsightsAnalyses_filters' - The filters. The following are possible values:
--
-- -   PathFound - A Boolean value that indicates whether a feasible path
--     is found.
--
-- -   Status - The status of the analysis (running | succeeded | failed).
--
-- 'networkInsightsPathId', 'describeNetworkInsightsAnalyses_networkInsightsPathId' - The ID of the path. You must specify either a path ID or analysis IDs.
--
-- 'analysisEndTime', 'describeNetworkInsightsAnalyses_analysisEndTime' - The time when the network insights analyses ended.
newDescribeNetworkInsightsAnalyses ::
  DescribeNetworkInsightsAnalyses
newDescribeNetworkInsightsAnalyses =
  DescribeNetworkInsightsAnalyses'
    { analysisStartTime =
        Prelude.Nothing,
      networkInsightsAnalysisIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing,
      networkInsightsPathId = Prelude.Nothing,
      analysisEndTime = Prelude.Nothing
    }

-- | The time when the network insights analyses started.
describeNetworkInsightsAnalyses_analysisStartTime :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe Prelude.UTCTime)
describeNetworkInsightsAnalyses_analysisStartTime = Lens.lens (\DescribeNetworkInsightsAnalyses' {analysisStartTime} -> analysisStartTime) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {analysisStartTime = a} :: DescribeNetworkInsightsAnalyses) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the network insights analyses. You must specify either
-- analysis IDs or a path ID.
describeNetworkInsightsAnalyses_networkInsightsAnalysisIds :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe [Prelude.Text])
describeNetworkInsightsAnalyses_networkInsightsAnalysisIds = Lens.lens (\DescribeNetworkInsightsAnalyses' {networkInsightsAnalysisIds} -> networkInsightsAnalysisIds) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {networkInsightsAnalysisIds = a} :: DescribeNetworkInsightsAnalyses) Prelude.. Lens.mapping Prelude._Coerce

-- | The token for the next page of results.
describeNetworkInsightsAnalyses_nextToken :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe Prelude.Text)
describeNetworkInsightsAnalyses_nextToken = Lens.lens (\DescribeNetworkInsightsAnalyses' {nextToken} -> nextToken) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {nextToken = a} :: DescribeNetworkInsightsAnalyses)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeNetworkInsightsAnalyses_dryRun :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe Prelude.Bool)
describeNetworkInsightsAnalyses_dryRun = Lens.lens (\DescribeNetworkInsightsAnalyses' {dryRun} -> dryRun) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {dryRun = a} :: DescribeNetworkInsightsAnalyses)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeNetworkInsightsAnalyses_maxResults :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe Prelude.Natural)
describeNetworkInsightsAnalyses_maxResults = Lens.lens (\DescribeNetworkInsightsAnalyses' {maxResults} -> maxResults) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {maxResults = a} :: DescribeNetworkInsightsAnalyses)

-- | The filters. The following are possible values:
--
-- -   PathFound - A Boolean value that indicates whether a feasible path
--     is found.
--
-- -   Status - The status of the analysis (running | succeeded | failed).
describeNetworkInsightsAnalyses_filters :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe [Filter])
describeNetworkInsightsAnalyses_filters = Lens.lens (\DescribeNetworkInsightsAnalyses' {filters} -> filters) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {filters = a} :: DescribeNetworkInsightsAnalyses) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the path. You must specify either a path ID or analysis IDs.
describeNetworkInsightsAnalyses_networkInsightsPathId :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe Prelude.Text)
describeNetworkInsightsAnalyses_networkInsightsPathId = Lens.lens (\DescribeNetworkInsightsAnalyses' {networkInsightsPathId} -> networkInsightsPathId) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {networkInsightsPathId = a} :: DescribeNetworkInsightsAnalyses)

-- | The time when the network insights analyses ended.
describeNetworkInsightsAnalyses_analysisEndTime :: Lens.Lens' DescribeNetworkInsightsAnalyses (Prelude.Maybe Prelude.UTCTime)
describeNetworkInsightsAnalyses_analysisEndTime = Lens.lens (\DescribeNetworkInsightsAnalyses' {analysisEndTime} -> analysisEndTime) (\s@DescribeNetworkInsightsAnalyses' {} a -> s {analysisEndTime = a} :: DescribeNetworkInsightsAnalyses) Prelude.. Lens.mapping Prelude._Time

instance
  Pager.AWSPager
    DescribeNetworkInsightsAnalyses
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeNetworkInsightsAnalysesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeNetworkInsightsAnalysesResponse_networkInsightsAnalyses
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeNetworkInsightsAnalyses_nextToken
          Lens..~ rs
          Lens.^? describeNetworkInsightsAnalysesResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeNetworkInsightsAnalyses
  where
  type
    Rs DescribeNetworkInsightsAnalyses =
      DescribeNetworkInsightsAnalysesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNetworkInsightsAnalysesResponse'
            Prelude.<$> (x Prelude..@? "nextToken")
            Prelude.<*> ( x Prelude..@? "networkInsightsAnalysisSet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeNetworkInsightsAnalyses

instance
  Prelude.NFData
    DescribeNetworkInsightsAnalyses

instance
  Prelude.ToHeaders
    DescribeNetworkInsightsAnalyses
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DescribeNetworkInsightsAnalyses
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeNetworkInsightsAnalyses
  where
  toQuery DescribeNetworkInsightsAnalyses' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DescribeNetworkInsightsAnalyses" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "AnalysisStartTime" Prelude.=: analysisStartTime,
        Prelude.toQuery
          ( Prelude.toQueryList "NetworkInsightsAnalysisId"
              Prelude.<$> networkInsightsAnalysisIds
          ),
        "NextToken" Prelude.=: nextToken,
        "DryRun" Prelude.=: dryRun,
        "MaxResults" Prelude.=: maxResults,
        Prelude.toQuery
          (Prelude.toQueryList "Filter" Prelude.<$> filters),
        "NetworkInsightsPathId"
          Prelude.=: networkInsightsPathId,
        "AnalysisEndTime" Prelude.=: analysisEndTime
      ]

-- | /See:/ 'newDescribeNetworkInsightsAnalysesResponse' smart constructor.
data DescribeNetworkInsightsAnalysesResponse = DescribeNetworkInsightsAnalysesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the network insights analyses.
    networkInsightsAnalyses :: Prelude.Maybe [NetworkInsightsAnalysis],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInsightsAnalysesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeNetworkInsightsAnalysesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'networkInsightsAnalyses', 'describeNetworkInsightsAnalysesResponse_networkInsightsAnalyses' - Information about the network insights analyses.
--
-- 'httpStatus', 'describeNetworkInsightsAnalysesResponse_httpStatus' - The response's http status code.
newDescribeNetworkInsightsAnalysesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNetworkInsightsAnalysesResponse
newDescribeNetworkInsightsAnalysesResponse
  pHttpStatus_ =
    DescribeNetworkInsightsAnalysesResponse'
      { nextToken =
          Prelude.Nothing,
        networkInsightsAnalyses =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeNetworkInsightsAnalysesResponse_nextToken :: Lens.Lens' DescribeNetworkInsightsAnalysesResponse (Prelude.Maybe Prelude.Text)
describeNetworkInsightsAnalysesResponse_nextToken = Lens.lens (\DescribeNetworkInsightsAnalysesResponse' {nextToken} -> nextToken) (\s@DescribeNetworkInsightsAnalysesResponse' {} a -> s {nextToken = a} :: DescribeNetworkInsightsAnalysesResponse)

-- | Information about the network insights analyses.
describeNetworkInsightsAnalysesResponse_networkInsightsAnalyses :: Lens.Lens' DescribeNetworkInsightsAnalysesResponse (Prelude.Maybe [NetworkInsightsAnalysis])
describeNetworkInsightsAnalysesResponse_networkInsightsAnalyses = Lens.lens (\DescribeNetworkInsightsAnalysesResponse' {networkInsightsAnalyses} -> networkInsightsAnalyses) (\s@DescribeNetworkInsightsAnalysesResponse' {} a -> s {networkInsightsAnalyses = a} :: DescribeNetworkInsightsAnalysesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeNetworkInsightsAnalysesResponse_httpStatus :: Lens.Lens' DescribeNetworkInsightsAnalysesResponse Prelude.Int
describeNetworkInsightsAnalysesResponse_httpStatus = Lens.lens (\DescribeNetworkInsightsAnalysesResponse' {httpStatus} -> httpStatus) (\s@DescribeNetworkInsightsAnalysesResponse' {} a -> s {httpStatus = a} :: DescribeNetworkInsightsAnalysesResponse)

instance
  Prelude.NFData
    DescribeNetworkInsightsAnalysesResponse
