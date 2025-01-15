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
-- Module      : Amazonka.EC2.GetAwsNetworkPerformanceData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets network performance data.
--
-- This operation returns paginated results.
module Amazonka.EC2.GetAwsNetworkPerformanceData
  ( -- * Creating a Request
    GetAwsNetworkPerformanceData (..),
    newGetAwsNetworkPerformanceData,

    -- * Request Lenses
    getAwsNetworkPerformanceData_dataQueries,
    getAwsNetworkPerformanceData_dryRun,
    getAwsNetworkPerformanceData_endTime,
    getAwsNetworkPerformanceData_maxResults,
    getAwsNetworkPerformanceData_nextToken,
    getAwsNetworkPerformanceData_startTime,

    -- * Destructuring the Response
    GetAwsNetworkPerformanceDataResponse (..),
    newGetAwsNetworkPerformanceDataResponse,

    -- * Response Lenses
    getAwsNetworkPerformanceDataResponse_dataResponses,
    getAwsNetworkPerformanceDataResponse_nextToken,
    getAwsNetworkPerformanceDataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAwsNetworkPerformanceData' smart constructor.
data GetAwsNetworkPerformanceData = GetAwsNetworkPerformanceData'
  { -- | A list of network performance data queries.
    dataQueries :: Prelude.Maybe [DataQuery],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ending time for the performance data request. The end time must be
    -- formatted as @yyyy-mm-ddThh:mm:ss@. For example,
    -- @2022-06-12T12:00:00.000Z@.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The starting time for the performance data request. The starting time
    -- must be formatted as @yyyy-mm-ddThh:mm:ss@. For example,
    -- @2022-06-10T12:00:00.000Z@.
    startTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAwsNetworkPerformanceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataQueries', 'getAwsNetworkPerformanceData_dataQueries' - A list of network performance data queries.
--
-- 'dryRun', 'getAwsNetworkPerformanceData_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'endTime', 'getAwsNetworkPerformanceData_endTime' - The ending time for the performance data request. The end time must be
-- formatted as @yyyy-mm-ddThh:mm:ss@. For example,
-- @2022-06-12T12:00:00.000Z@.
--
-- 'maxResults', 'getAwsNetworkPerformanceData_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'getAwsNetworkPerformanceData_nextToken' - The token for the next page of results.
--
-- 'startTime', 'getAwsNetworkPerformanceData_startTime' - The starting time for the performance data request. The starting time
-- must be formatted as @yyyy-mm-ddThh:mm:ss@. For example,
-- @2022-06-10T12:00:00.000Z@.
newGetAwsNetworkPerformanceData ::
  GetAwsNetworkPerformanceData
newGetAwsNetworkPerformanceData =
  GetAwsNetworkPerformanceData'
    { dataQueries =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      endTime = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | A list of network performance data queries.
getAwsNetworkPerformanceData_dataQueries :: Lens.Lens' GetAwsNetworkPerformanceData (Prelude.Maybe [DataQuery])
getAwsNetworkPerformanceData_dataQueries = Lens.lens (\GetAwsNetworkPerformanceData' {dataQueries} -> dataQueries) (\s@GetAwsNetworkPerformanceData' {} a -> s {dataQueries = a} :: GetAwsNetworkPerformanceData) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getAwsNetworkPerformanceData_dryRun :: Lens.Lens' GetAwsNetworkPerformanceData (Prelude.Maybe Prelude.Bool)
getAwsNetworkPerformanceData_dryRun = Lens.lens (\GetAwsNetworkPerformanceData' {dryRun} -> dryRun) (\s@GetAwsNetworkPerformanceData' {} a -> s {dryRun = a} :: GetAwsNetworkPerformanceData)

-- | The ending time for the performance data request. The end time must be
-- formatted as @yyyy-mm-ddThh:mm:ss@. For example,
-- @2022-06-12T12:00:00.000Z@.
getAwsNetworkPerformanceData_endTime :: Lens.Lens' GetAwsNetworkPerformanceData (Prelude.Maybe Prelude.UTCTime)
getAwsNetworkPerformanceData_endTime = Lens.lens (\GetAwsNetworkPerformanceData' {endTime} -> endTime) (\s@GetAwsNetworkPerformanceData' {} a -> s {endTime = a} :: GetAwsNetworkPerformanceData) Prelude.. Lens.mapping Data._Time

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getAwsNetworkPerformanceData_maxResults :: Lens.Lens' GetAwsNetworkPerformanceData (Prelude.Maybe Prelude.Int)
getAwsNetworkPerformanceData_maxResults = Lens.lens (\GetAwsNetworkPerformanceData' {maxResults} -> maxResults) (\s@GetAwsNetworkPerformanceData' {} a -> s {maxResults = a} :: GetAwsNetworkPerformanceData)

-- | The token for the next page of results.
getAwsNetworkPerformanceData_nextToken :: Lens.Lens' GetAwsNetworkPerformanceData (Prelude.Maybe Prelude.Text)
getAwsNetworkPerformanceData_nextToken = Lens.lens (\GetAwsNetworkPerformanceData' {nextToken} -> nextToken) (\s@GetAwsNetworkPerformanceData' {} a -> s {nextToken = a} :: GetAwsNetworkPerformanceData)

-- | The starting time for the performance data request. The starting time
-- must be formatted as @yyyy-mm-ddThh:mm:ss@. For example,
-- @2022-06-10T12:00:00.000Z@.
getAwsNetworkPerformanceData_startTime :: Lens.Lens' GetAwsNetworkPerformanceData (Prelude.Maybe Prelude.UTCTime)
getAwsNetworkPerformanceData_startTime = Lens.lens (\GetAwsNetworkPerformanceData' {startTime} -> startTime) (\s@GetAwsNetworkPerformanceData' {} a -> s {startTime = a} :: GetAwsNetworkPerformanceData) Prelude.. Lens.mapping Data._Time

instance Core.AWSPager GetAwsNetworkPerformanceData where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getAwsNetworkPerformanceDataResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getAwsNetworkPerformanceDataResponse_dataResponses
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getAwsNetworkPerformanceData_nextToken
              Lens..~ rs
              Lens.^? getAwsNetworkPerformanceDataResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetAwsNetworkPerformanceData where
  type
    AWSResponse GetAwsNetworkPerformanceData =
      GetAwsNetworkPerformanceDataResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetAwsNetworkPerformanceDataResponse'
            Prelude.<$> ( x Data..@? "dataResponseSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetAwsNetworkPerformanceData
  where
  hashWithSalt _salt GetAwsNetworkPerformanceData' {..} =
    _salt
      `Prelude.hashWithSalt` dataQueries
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData GetAwsNetworkPerformanceData where
  rnf GetAwsNetworkPerformanceData' {..} =
    Prelude.rnf dataQueries `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf endTime `Prelude.seq`
          Prelude.rnf maxResults `Prelude.seq`
            Prelude.rnf nextToken `Prelude.seq`
              Prelude.rnf startTime

instance Data.ToHeaders GetAwsNetworkPerformanceData where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetAwsNetworkPerformanceData where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAwsNetworkPerformanceData where
  toQuery GetAwsNetworkPerformanceData' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetAwsNetworkPerformanceData" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "DataQuery"
              Prelude.<$> dataQueries
          ),
        "DryRun" Data.=: dryRun,
        "EndTime" Data.=: endTime,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "StartTime" Data.=: startTime
      ]

-- | /See:/ 'newGetAwsNetworkPerformanceDataResponse' smart constructor.
data GetAwsNetworkPerformanceDataResponse = GetAwsNetworkPerformanceDataResponse'
  { -- | The list of data responses.
    dataResponses :: Prelude.Maybe [DataResponse],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAwsNetworkPerformanceDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataResponses', 'getAwsNetworkPerformanceDataResponse_dataResponses' - The list of data responses.
--
-- 'nextToken', 'getAwsNetworkPerformanceDataResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'getAwsNetworkPerformanceDataResponse_httpStatus' - The response's http status code.
newGetAwsNetworkPerformanceDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAwsNetworkPerformanceDataResponse
newGetAwsNetworkPerformanceDataResponse pHttpStatus_ =
  GetAwsNetworkPerformanceDataResponse'
    { dataResponses =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of data responses.
getAwsNetworkPerformanceDataResponse_dataResponses :: Lens.Lens' GetAwsNetworkPerformanceDataResponse (Prelude.Maybe [DataResponse])
getAwsNetworkPerformanceDataResponse_dataResponses = Lens.lens (\GetAwsNetworkPerformanceDataResponse' {dataResponses} -> dataResponses) (\s@GetAwsNetworkPerformanceDataResponse' {} a -> s {dataResponses = a} :: GetAwsNetworkPerformanceDataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getAwsNetworkPerformanceDataResponse_nextToken :: Lens.Lens' GetAwsNetworkPerformanceDataResponse (Prelude.Maybe Prelude.Text)
getAwsNetworkPerformanceDataResponse_nextToken = Lens.lens (\GetAwsNetworkPerformanceDataResponse' {nextToken} -> nextToken) (\s@GetAwsNetworkPerformanceDataResponse' {} a -> s {nextToken = a} :: GetAwsNetworkPerformanceDataResponse)

-- | The response's http status code.
getAwsNetworkPerformanceDataResponse_httpStatus :: Lens.Lens' GetAwsNetworkPerformanceDataResponse Prelude.Int
getAwsNetworkPerformanceDataResponse_httpStatus = Lens.lens (\GetAwsNetworkPerformanceDataResponse' {httpStatus} -> httpStatus) (\s@GetAwsNetworkPerformanceDataResponse' {} a -> s {httpStatus = a} :: GetAwsNetworkPerformanceDataResponse)

instance
  Prelude.NFData
    GetAwsNetworkPerformanceDataResponse
  where
  rnf GetAwsNetworkPerformanceDataResponse' {..} =
    Prelude.rnf dataResponses `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
