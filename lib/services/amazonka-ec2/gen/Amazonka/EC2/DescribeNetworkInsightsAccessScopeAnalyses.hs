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
-- Module      : Amazonka.EC2.DescribeNetworkInsightsAccessScopeAnalyses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Network Access Scope analyses.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeNetworkInsightsAccessScopeAnalyses
  ( -- * Creating a Request
    DescribeNetworkInsightsAccessScopeAnalyses (..),
    newDescribeNetworkInsightsAccessScopeAnalyses,

    -- * Request Lenses
    describeNetworkInsightsAccessScopeAnalyses_analysisStartTimeBegin,
    describeNetworkInsightsAccessScopeAnalyses_analysisStartTimeEnd,
    describeNetworkInsightsAccessScopeAnalyses_dryRun,
    describeNetworkInsightsAccessScopeAnalyses_filters,
    describeNetworkInsightsAccessScopeAnalyses_maxResults,
    describeNetworkInsightsAccessScopeAnalyses_networkInsightsAccessScopeAnalysisIds,
    describeNetworkInsightsAccessScopeAnalyses_networkInsightsAccessScopeId,
    describeNetworkInsightsAccessScopeAnalyses_nextToken,

    -- * Destructuring the Response
    DescribeNetworkInsightsAccessScopeAnalysesResponse (..),
    newDescribeNetworkInsightsAccessScopeAnalysesResponse,

    -- * Response Lenses
    describeNetworkInsightsAccessScopeAnalysesResponse_networkInsightsAccessScopeAnalyses,
    describeNetworkInsightsAccessScopeAnalysesResponse_nextToken,
    describeNetworkInsightsAccessScopeAnalysesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeNetworkInsightsAccessScopeAnalyses' smart constructor.
data DescribeNetworkInsightsAccessScopeAnalyses = DescribeNetworkInsightsAccessScopeAnalyses'
  { -- | Filters the results based on the start time. The analysis must have
    -- started on or after this time.
    analysisStartTimeBegin :: Prelude.Maybe Data.ISO8601,
    -- | Filters the results based on the start time. The analysis must have
    -- started on or before this time.
    analysisStartTimeEnd :: Prelude.Maybe Data.ISO8601,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | There are no supported filters.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The IDs of the Network Access Scope analyses.
    networkInsightsAccessScopeAnalysisIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the Network Access Scope.
    networkInsightsAccessScopeId :: Prelude.Maybe Prelude.Text,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInsightsAccessScopeAnalyses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisStartTimeBegin', 'describeNetworkInsightsAccessScopeAnalyses_analysisStartTimeBegin' - Filters the results based on the start time. The analysis must have
-- started on or after this time.
--
-- 'analysisStartTimeEnd', 'describeNetworkInsightsAccessScopeAnalyses_analysisStartTimeEnd' - Filters the results based on the start time. The analysis must have
-- started on or before this time.
--
-- 'dryRun', 'describeNetworkInsightsAccessScopeAnalyses_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeNetworkInsightsAccessScopeAnalyses_filters' - There are no supported filters.
--
-- 'maxResults', 'describeNetworkInsightsAccessScopeAnalyses_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'networkInsightsAccessScopeAnalysisIds', 'describeNetworkInsightsAccessScopeAnalyses_networkInsightsAccessScopeAnalysisIds' - The IDs of the Network Access Scope analyses.
--
-- 'networkInsightsAccessScopeId', 'describeNetworkInsightsAccessScopeAnalyses_networkInsightsAccessScopeId' - The ID of the Network Access Scope.
--
-- 'nextToken', 'describeNetworkInsightsAccessScopeAnalyses_nextToken' - The token for the next page of results.
newDescribeNetworkInsightsAccessScopeAnalyses ::
  DescribeNetworkInsightsAccessScopeAnalyses
newDescribeNetworkInsightsAccessScopeAnalyses =
  DescribeNetworkInsightsAccessScopeAnalyses'
    { analysisStartTimeBegin =
        Prelude.Nothing,
      analysisStartTimeEnd =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      networkInsightsAccessScopeAnalysisIds =
        Prelude.Nothing,
      networkInsightsAccessScopeId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters the results based on the start time. The analysis must have
-- started on or after this time.
describeNetworkInsightsAccessScopeAnalyses_analysisStartTimeBegin :: Lens.Lens' DescribeNetworkInsightsAccessScopeAnalyses (Prelude.Maybe Prelude.UTCTime)
describeNetworkInsightsAccessScopeAnalyses_analysisStartTimeBegin = Lens.lens (\DescribeNetworkInsightsAccessScopeAnalyses' {analysisStartTimeBegin} -> analysisStartTimeBegin) (\s@DescribeNetworkInsightsAccessScopeAnalyses' {} a -> s {analysisStartTimeBegin = a} :: DescribeNetworkInsightsAccessScopeAnalyses) Prelude.. Lens.mapping Data._Time

-- | Filters the results based on the start time. The analysis must have
-- started on or before this time.
describeNetworkInsightsAccessScopeAnalyses_analysisStartTimeEnd :: Lens.Lens' DescribeNetworkInsightsAccessScopeAnalyses (Prelude.Maybe Prelude.UTCTime)
describeNetworkInsightsAccessScopeAnalyses_analysisStartTimeEnd = Lens.lens (\DescribeNetworkInsightsAccessScopeAnalyses' {analysisStartTimeEnd} -> analysisStartTimeEnd) (\s@DescribeNetworkInsightsAccessScopeAnalyses' {} a -> s {analysisStartTimeEnd = a} :: DescribeNetworkInsightsAccessScopeAnalyses) Prelude.. Lens.mapping Data._Time

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeNetworkInsightsAccessScopeAnalyses_dryRun :: Lens.Lens' DescribeNetworkInsightsAccessScopeAnalyses (Prelude.Maybe Prelude.Bool)
describeNetworkInsightsAccessScopeAnalyses_dryRun = Lens.lens (\DescribeNetworkInsightsAccessScopeAnalyses' {dryRun} -> dryRun) (\s@DescribeNetworkInsightsAccessScopeAnalyses' {} a -> s {dryRun = a} :: DescribeNetworkInsightsAccessScopeAnalyses)

-- | There are no supported filters.
describeNetworkInsightsAccessScopeAnalyses_filters :: Lens.Lens' DescribeNetworkInsightsAccessScopeAnalyses (Prelude.Maybe [Filter])
describeNetworkInsightsAccessScopeAnalyses_filters = Lens.lens (\DescribeNetworkInsightsAccessScopeAnalyses' {filters} -> filters) (\s@DescribeNetworkInsightsAccessScopeAnalyses' {} a -> s {filters = a} :: DescribeNetworkInsightsAccessScopeAnalyses) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeNetworkInsightsAccessScopeAnalyses_maxResults :: Lens.Lens' DescribeNetworkInsightsAccessScopeAnalyses (Prelude.Maybe Prelude.Natural)
describeNetworkInsightsAccessScopeAnalyses_maxResults = Lens.lens (\DescribeNetworkInsightsAccessScopeAnalyses' {maxResults} -> maxResults) (\s@DescribeNetworkInsightsAccessScopeAnalyses' {} a -> s {maxResults = a} :: DescribeNetworkInsightsAccessScopeAnalyses)

-- | The IDs of the Network Access Scope analyses.
describeNetworkInsightsAccessScopeAnalyses_networkInsightsAccessScopeAnalysisIds :: Lens.Lens' DescribeNetworkInsightsAccessScopeAnalyses (Prelude.Maybe [Prelude.Text])
describeNetworkInsightsAccessScopeAnalyses_networkInsightsAccessScopeAnalysisIds = Lens.lens (\DescribeNetworkInsightsAccessScopeAnalyses' {networkInsightsAccessScopeAnalysisIds} -> networkInsightsAccessScopeAnalysisIds) (\s@DescribeNetworkInsightsAccessScopeAnalyses' {} a -> s {networkInsightsAccessScopeAnalysisIds = a} :: DescribeNetworkInsightsAccessScopeAnalyses) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Network Access Scope.
describeNetworkInsightsAccessScopeAnalyses_networkInsightsAccessScopeId :: Lens.Lens' DescribeNetworkInsightsAccessScopeAnalyses (Prelude.Maybe Prelude.Text)
describeNetworkInsightsAccessScopeAnalyses_networkInsightsAccessScopeId = Lens.lens (\DescribeNetworkInsightsAccessScopeAnalyses' {networkInsightsAccessScopeId} -> networkInsightsAccessScopeId) (\s@DescribeNetworkInsightsAccessScopeAnalyses' {} a -> s {networkInsightsAccessScopeId = a} :: DescribeNetworkInsightsAccessScopeAnalyses)

-- | The token for the next page of results.
describeNetworkInsightsAccessScopeAnalyses_nextToken :: Lens.Lens' DescribeNetworkInsightsAccessScopeAnalyses (Prelude.Maybe Prelude.Text)
describeNetworkInsightsAccessScopeAnalyses_nextToken = Lens.lens (\DescribeNetworkInsightsAccessScopeAnalyses' {nextToken} -> nextToken) (\s@DescribeNetworkInsightsAccessScopeAnalyses' {} a -> s {nextToken = a} :: DescribeNetworkInsightsAccessScopeAnalyses)

instance
  Core.AWSPager
    DescribeNetworkInsightsAccessScopeAnalyses
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNetworkInsightsAccessScopeAnalysesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeNetworkInsightsAccessScopeAnalysesResponse_networkInsightsAccessScopeAnalyses
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeNetworkInsightsAccessScopeAnalyses_nextToken
              Lens..~ rs
              Lens.^? describeNetworkInsightsAccessScopeAnalysesResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeNetworkInsightsAccessScopeAnalyses
  where
  type
    AWSResponse
      DescribeNetworkInsightsAccessScopeAnalyses =
      DescribeNetworkInsightsAccessScopeAnalysesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNetworkInsightsAccessScopeAnalysesResponse'
            Prelude.<$> ( x
                            Data..@? "networkInsightsAccessScopeAnalysisSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeNetworkInsightsAccessScopeAnalyses
  where
  hashWithSalt
    _salt
    DescribeNetworkInsightsAccessScopeAnalyses' {..} =
      _salt
        `Prelude.hashWithSalt` analysisStartTimeBegin
        `Prelude.hashWithSalt` analysisStartTimeEnd
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` networkInsightsAccessScopeAnalysisIds
        `Prelude.hashWithSalt` networkInsightsAccessScopeId
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeNetworkInsightsAccessScopeAnalyses
  where
  rnf DescribeNetworkInsightsAccessScopeAnalyses' {..} =
    Prelude.rnf analysisStartTimeBegin `Prelude.seq`
      Prelude.rnf analysisStartTimeEnd `Prelude.seq`
        Prelude.rnf dryRun `Prelude.seq`
          Prelude.rnf filters `Prelude.seq`
            Prelude.rnf maxResults `Prelude.seq`
              Prelude.rnf networkInsightsAccessScopeAnalysisIds `Prelude.seq`
                Prelude.rnf networkInsightsAccessScopeId `Prelude.seq`
                  Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeNetworkInsightsAccessScopeAnalyses
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeNetworkInsightsAccessScopeAnalyses
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeNetworkInsightsAccessScopeAnalyses
  where
  toQuery
    DescribeNetworkInsightsAccessScopeAnalyses' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "DescribeNetworkInsightsAccessScopeAnalyses" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "AnalysisStartTimeBegin"
            Data.=: analysisStartTimeBegin,
          "AnalysisStartTimeEnd" Data.=: analysisStartTimeEnd,
          "DryRun" Data.=: dryRun,
          Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
          "MaxResults" Data.=: maxResults,
          Data.toQuery
            ( Data.toQueryList
                "NetworkInsightsAccessScopeAnalysisId"
                Prelude.<$> networkInsightsAccessScopeAnalysisIds
            ),
          "NetworkInsightsAccessScopeId"
            Data.=: networkInsightsAccessScopeId,
          "NextToken" Data.=: nextToken
        ]

-- | /See:/ 'newDescribeNetworkInsightsAccessScopeAnalysesResponse' smart constructor.
data DescribeNetworkInsightsAccessScopeAnalysesResponse = DescribeNetworkInsightsAccessScopeAnalysesResponse'
  { -- | The Network Access Scope analyses.
    networkInsightsAccessScopeAnalyses :: Prelude.Maybe [NetworkInsightsAccessScopeAnalysis],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInsightsAccessScopeAnalysesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInsightsAccessScopeAnalyses', 'describeNetworkInsightsAccessScopeAnalysesResponse_networkInsightsAccessScopeAnalyses' - The Network Access Scope analyses.
--
-- 'nextToken', 'describeNetworkInsightsAccessScopeAnalysesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeNetworkInsightsAccessScopeAnalysesResponse_httpStatus' - The response's http status code.
newDescribeNetworkInsightsAccessScopeAnalysesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNetworkInsightsAccessScopeAnalysesResponse
newDescribeNetworkInsightsAccessScopeAnalysesResponse
  pHttpStatus_ =
    DescribeNetworkInsightsAccessScopeAnalysesResponse'
      { networkInsightsAccessScopeAnalyses =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The Network Access Scope analyses.
describeNetworkInsightsAccessScopeAnalysesResponse_networkInsightsAccessScopeAnalyses :: Lens.Lens' DescribeNetworkInsightsAccessScopeAnalysesResponse (Prelude.Maybe [NetworkInsightsAccessScopeAnalysis])
describeNetworkInsightsAccessScopeAnalysesResponse_networkInsightsAccessScopeAnalyses = Lens.lens (\DescribeNetworkInsightsAccessScopeAnalysesResponse' {networkInsightsAccessScopeAnalyses} -> networkInsightsAccessScopeAnalyses) (\s@DescribeNetworkInsightsAccessScopeAnalysesResponse' {} a -> s {networkInsightsAccessScopeAnalyses = a} :: DescribeNetworkInsightsAccessScopeAnalysesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeNetworkInsightsAccessScopeAnalysesResponse_nextToken :: Lens.Lens' DescribeNetworkInsightsAccessScopeAnalysesResponse (Prelude.Maybe Prelude.Text)
describeNetworkInsightsAccessScopeAnalysesResponse_nextToken = Lens.lens (\DescribeNetworkInsightsAccessScopeAnalysesResponse' {nextToken} -> nextToken) (\s@DescribeNetworkInsightsAccessScopeAnalysesResponse' {} a -> s {nextToken = a} :: DescribeNetworkInsightsAccessScopeAnalysesResponse)

-- | The response's http status code.
describeNetworkInsightsAccessScopeAnalysesResponse_httpStatus :: Lens.Lens' DescribeNetworkInsightsAccessScopeAnalysesResponse Prelude.Int
describeNetworkInsightsAccessScopeAnalysesResponse_httpStatus = Lens.lens (\DescribeNetworkInsightsAccessScopeAnalysesResponse' {httpStatus} -> httpStatus) (\s@DescribeNetworkInsightsAccessScopeAnalysesResponse' {} a -> s {httpStatus = a} :: DescribeNetworkInsightsAccessScopeAnalysesResponse)

instance
  Prelude.NFData
    DescribeNetworkInsightsAccessScopeAnalysesResponse
  where
  rnf
    DescribeNetworkInsightsAccessScopeAnalysesResponse' {..} =
      Prelude.rnf networkInsightsAccessScopeAnalyses `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf httpStatus
