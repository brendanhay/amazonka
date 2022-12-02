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
-- Module      : Amazonka.Pi.GetResourceMetrics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve Performance Insights metrics for a set of data sources over a
-- time period. You can provide specific dimension groups and dimensions,
-- and provide aggregation and filtering criteria for each group.
--
-- Each response element returns a maximum of 500 bytes. For larger
-- elements, such as SQL statements, only the first 500 bytes are returned.
module Amazonka.Pi.GetResourceMetrics
  ( -- * Creating a Request
    GetResourceMetrics (..),
    newGetResourceMetrics,

    -- * Request Lenses
    getResourceMetrics_nextToken,
    getResourceMetrics_maxResults,
    getResourceMetrics_periodInSeconds,
    getResourceMetrics_serviceType,
    getResourceMetrics_identifier,
    getResourceMetrics_metricQueries,
    getResourceMetrics_startTime,
    getResourceMetrics_endTime,

    -- * Destructuring the Response
    GetResourceMetricsResponse (..),
    newGetResourceMetricsResponse,

    -- * Response Lenses
    getResourceMetricsResponse_nextToken,
    getResourceMetricsResponse_alignedEndTime,
    getResourceMetricsResponse_identifier,
    getResourceMetricsResponse_metricList,
    getResourceMetricsResponse_alignedStartTime,
    getResourceMetricsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pi.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourceMetrics' smart constructor.
data GetResourceMetrics = GetResourceMetrics'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- token, up to the value specified by @MaxRecords@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return in the response. If more items
    -- exist than the specified @MaxRecords@ value, a pagination token is
    -- included in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The granularity, in seconds, of the data points returned from
    -- Performance Insights. A period can be as short as one second, or as long
    -- as one day (86400 seconds). Valid values are:
    --
    -- -   @1@ (one second)
    --
    -- -   @60@ (one minute)
    --
    -- -   @300@ (five minutes)
    --
    -- -   @3600@ (one hour)
    --
    -- -   @86400@ (twenty-four hours)
    --
    -- If you don\'t specify @PeriodInSeconds@, then Performance Insights will
    -- choose a value for you, with a goal of returning roughly 100-200 data
    -- points in the response.
    periodInSeconds :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services service for which Performance Insights returns
    -- metrics. Valid values are as follows:
    --
    -- -   @RDS@
    --
    -- -   @DOCDB@
    serviceType :: ServiceType,
    -- | An immutable identifier for a data source that is unique for an Amazon
    -- Web Services Region. Performance Insights gathers metrics from this data
    -- source. In the console, the identifier is shown as /ResourceID/. When
    -- you call @DescribeDBInstances@, the identifier is returned as
    -- @DbiResourceId@.
    --
    -- To use a DB instance as a data source, specify its @DbiResourceId@
    -- value. For example, specify @db-ABCDEFGHIJKLMNOPQRSTU1VW2X@.
    identifier :: Prelude.Text,
    -- | An array of one or more queries to perform. Each query must specify a
    -- Performance Insights metric, and can optionally specify aggregation and
    -- filtering criteria.
    metricQueries :: Prelude.NonEmpty MetricQuery,
    -- | The date and time specifying the beginning of the requested time series
    -- query range. You can\'t specify a @StartTime@ that is earlier than 7
    -- days ago. By default, Performance Insights has 7 days of retention, but
    -- you can extend this range up to 2 years. The value specified is
    -- /inclusive/. Thus, the command returns data points equal to or greater
    -- than @StartTime@.
    --
    -- The value for @StartTime@ must be earlier than the value for @EndTime@.
    startTime :: Data.POSIX,
    -- | The date and time specifying the end of the requested time series query
    -- range. The value specified is /exclusive/. Thus, the command returns
    -- data points less than (but not equal to) @EndTime@.
    --
    -- The value for @EndTime@ must be later than the value for @StartTime@.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getResourceMetrics_nextToken' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- token, up to the value specified by @MaxRecords@.
--
-- 'maxResults', 'getResourceMetrics_maxResults' - The maximum number of items to return in the response. If more items
-- exist than the specified @MaxRecords@ value, a pagination token is
-- included in the response so that the remaining results can be retrieved.
--
-- 'periodInSeconds', 'getResourceMetrics_periodInSeconds' - The granularity, in seconds, of the data points returned from
-- Performance Insights. A period can be as short as one second, or as long
-- as one day (86400 seconds). Valid values are:
--
-- -   @1@ (one second)
--
-- -   @60@ (one minute)
--
-- -   @300@ (five minutes)
--
-- -   @3600@ (one hour)
--
-- -   @86400@ (twenty-four hours)
--
-- If you don\'t specify @PeriodInSeconds@, then Performance Insights will
-- choose a value for you, with a goal of returning roughly 100-200 data
-- points in the response.
--
-- 'serviceType', 'getResourceMetrics_serviceType' - The Amazon Web Services service for which Performance Insights returns
-- metrics. Valid values are as follows:
--
-- -   @RDS@
--
-- -   @DOCDB@
--
-- 'identifier', 'getResourceMetrics_identifier' - An immutable identifier for a data source that is unique for an Amazon
-- Web Services Region. Performance Insights gathers metrics from this data
-- source. In the console, the identifier is shown as /ResourceID/. When
-- you call @DescribeDBInstances@, the identifier is returned as
-- @DbiResourceId@.
--
-- To use a DB instance as a data source, specify its @DbiResourceId@
-- value. For example, specify @db-ABCDEFGHIJKLMNOPQRSTU1VW2X@.
--
-- 'metricQueries', 'getResourceMetrics_metricQueries' - An array of one or more queries to perform. Each query must specify a
-- Performance Insights metric, and can optionally specify aggregation and
-- filtering criteria.
--
-- 'startTime', 'getResourceMetrics_startTime' - The date and time specifying the beginning of the requested time series
-- query range. You can\'t specify a @StartTime@ that is earlier than 7
-- days ago. By default, Performance Insights has 7 days of retention, but
-- you can extend this range up to 2 years. The value specified is
-- /inclusive/. Thus, the command returns data points equal to or greater
-- than @StartTime@.
--
-- The value for @StartTime@ must be earlier than the value for @EndTime@.
--
-- 'endTime', 'getResourceMetrics_endTime' - The date and time specifying the end of the requested time series query
-- range. The value specified is /exclusive/. Thus, the command returns
-- data points less than (but not equal to) @EndTime@.
--
-- The value for @EndTime@ must be later than the value for @StartTime@.
newGetResourceMetrics ::
  -- | 'serviceType'
  ServiceType ->
  -- | 'identifier'
  Prelude.Text ->
  -- | 'metricQueries'
  Prelude.NonEmpty MetricQuery ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  GetResourceMetrics
newGetResourceMetrics
  pServiceType_
  pIdentifier_
  pMetricQueries_
  pStartTime_
  pEndTime_ =
    GetResourceMetrics'
      { nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        periodInSeconds = Prelude.Nothing,
        serviceType = pServiceType_,
        identifier = pIdentifier_,
        metricQueries = Lens.coerced Lens.# pMetricQueries_,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_
      }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- token, up to the value specified by @MaxRecords@.
getResourceMetrics_nextToken :: Lens.Lens' GetResourceMetrics (Prelude.Maybe Prelude.Text)
getResourceMetrics_nextToken = Lens.lens (\GetResourceMetrics' {nextToken} -> nextToken) (\s@GetResourceMetrics' {} a -> s {nextToken = a} :: GetResourceMetrics)

-- | The maximum number of items to return in the response. If more items
-- exist than the specified @MaxRecords@ value, a pagination token is
-- included in the response so that the remaining results can be retrieved.
getResourceMetrics_maxResults :: Lens.Lens' GetResourceMetrics (Prelude.Maybe Prelude.Natural)
getResourceMetrics_maxResults = Lens.lens (\GetResourceMetrics' {maxResults} -> maxResults) (\s@GetResourceMetrics' {} a -> s {maxResults = a} :: GetResourceMetrics)

-- | The granularity, in seconds, of the data points returned from
-- Performance Insights. A period can be as short as one second, or as long
-- as one day (86400 seconds). Valid values are:
--
-- -   @1@ (one second)
--
-- -   @60@ (one minute)
--
-- -   @300@ (five minutes)
--
-- -   @3600@ (one hour)
--
-- -   @86400@ (twenty-four hours)
--
-- If you don\'t specify @PeriodInSeconds@, then Performance Insights will
-- choose a value for you, with a goal of returning roughly 100-200 data
-- points in the response.
getResourceMetrics_periodInSeconds :: Lens.Lens' GetResourceMetrics (Prelude.Maybe Prelude.Int)
getResourceMetrics_periodInSeconds = Lens.lens (\GetResourceMetrics' {periodInSeconds} -> periodInSeconds) (\s@GetResourceMetrics' {} a -> s {periodInSeconds = a} :: GetResourceMetrics)

-- | The Amazon Web Services service for which Performance Insights returns
-- metrics. Valid values are as follows:
--
-- -   @RDS@
--
-- -   @DOCDB@
getResourceMetrics_serviceType :: Lens.Lens' GetResourceMetrics ServiceType
getResourceMetrics_serviceType = Lens.lens (\GetResourceMetrics' {serviceType} -> serviceType) (\s@GetResourceMetrics' {} a -> s {serviceType = a} :: GetResourceMetrics)

-- | An immutable identifier for a data source that is unique for an Amazon
-- Web Services Region. Performance Insights gathers metrics from this data
-- source. In the console, the identifier is shown as /ResourceID/. When
-- you call @DescribeDBInstances@, the identifier is returned as
-- @DbiResourceId@.
--
-- To use a DB instance as a data source, specify its @DbiResourceId@
-- value. For example, specify @db-ABCDEFGHIJKLMNOPQRSTU1VW2X@.
getResourceMetrics_identifier :: Lens.Lens' GetResourceMetrics Prelude.Text
getResourceMetrics_identifier = Lens.lens (\GetResourceMetrics' {identifier} -> identifier) (\s@GetResourceMetrics' {} a -> s {identifier = a} :: GetResourceMetrics)

-- | An array of one or more queries to perform. Each query must specify a
-- Performance Insights metric, and can optionally specify aggregation and
-- filtering criteria.
getResourceMetrics_metricQueries :: Lens.Lens' GetResourceMetrics (Prelude.NonEmpty MetricQuery)
getResourceMetrics_metricQueries = Lens.lens (\GetResourceMetrics' {metricQueries} -> metricQueries) (\s@GetResourceMetrics' {} a -> s {metricQueries = a} :: GetResourceMetrics) Prelude.. Lens.coerced

-- | The date and time specifying the beginning of the requested time series
-- query range. You can\'t specify a @StartTime@ that is earlier than 7
-- days ago. By default, Performance Insights has 7 days of retention, but
-- you can extend this range up to 2 years. The value specified is
-- /inclusive/. Thus, the command returns data points equal to or greater
-- than @StartTime@.
--
-- The value for @StartTime@ must be earlier than the value for @EndTime@.
getResourceMetrics_startTime :: Lens.Lens' GetResourceMetrics Prelude.UTCTime
getResourceMetrics_startTime = Lens.lens (\GetResourceMetrics' {startTime} -> startTime) (\s@GetResourceMetrics' {} a -> s {startTime = a} :: GetResourceMetrics) Prelude.. Data._Time

-- | The date and time specifying the end of the requested time series query
-- range. The value specified is /exclusive/. Thus, the command returns
-- data points less than (but not equal to) @EndTime@.
--
-- The value for @EndTime@ must be later than the value for @StartTime@.
getResourceMetrics_endTime :: Lens.Lens' GetResourceMetrics Prelude.UTCTime
getResourceMetrics_endTime = Lens.lens (\GetResourceMetrics' {endTime} -> endTime) (\s@GetResourceMetrics' {} a -> s {endTime = a} :: GetResourceMetrics) Prelude.. Data._Time

instance Core.AWSRequest GetResourceMetrics where
  type
    AWSResponse GetResourceMetrics =
      GetResourceMetricsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceMetricsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "AlignedEndTime")
            Prelude.<*> (x Data..?> "Identifier")
            Prelude.<*> (x Data..?> "MetricList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "AlignedStartTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourceMetrics where
  hashWithSalt _salt GetResourceMetrics' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` periodInSeconds
      `Prelude.hashWithSalt` serviceType
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` metricQueries
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData GetResourceMetrics where
  rnf GetResourceMetrics' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf periodInSeconds
      `Prelude.seq` Prelude.rnf serviceType
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf metricQueries
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Data.ToHeaders GetResourceMetrics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PerformanceInsightsv20180227.GetResourceMetrics" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResourceMetrics where
  toJSON GetResourceMetrics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("PeriodInSeconds" Data..=)
              Prelude.<$> periodInSeconds,
            Prelude.Just ("ServiceType" Data..= serviceType),
            Prelude.Just ("Identifier" Data..= identifier),
            Prelude.Just ("MetricQueries" Data..= metricQueries),
            Prelude.Just ("StartTime" Data..= startTime),
            Prelude.Just ("EndTime" Data..= endTime)
          ]
      )

instance Data.ToPath GetResourceMetrics where
  toPath = Prelude.const "/"

instance Data.ToQuery GetResourceMetrics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceMetricsResponse' smart constructor.
data GetResourceMetricsResponse = GetResourceMetricsResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- token, up to the value specified by @MaxRecords@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The end time for the returned metrics, after alignment to a granular
    -- boundary (as specified by @PeriodInSeconds@). @AlignedEndTime@ will be
    -- greater than or equal to the value of the user-specified @Endtime@.
    alignedEndTime :: Prelude.Maybe Data.POSIX,
    -- | An immutable identifier for a data source that is unique for an Amazon
    -- Web Services Region. Performance Insights gathers metrics from this data
    -- source. In the console, the identifier is shown as /ResourceID/. When
    -- you call @DescribeDBInstances@, the identifier is returned as
    -- @DbiResourceId@.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | An array of metric results, where each array element contains all of the
    -- data points for a particular dimension.
    metricList :: Prelude.Maybe [MetricKeyDataPoints],
    -- | The start time for the returned metrics, after alignment to a granular
    -- boundary (as specified by @PeriodInSeconds@). @AlignedStartTime@ will be
    -- less than or equal to the value of the user-specified @StartTime@.
    alignedStartTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getResourceMetricsResponse_nextToken' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- token, up to the value specified by @MaxRecords@.
--
-- 'alignedEndTime', 'getResourceMetricsResponse_alignedEndTime' - The end time for the returned metrics, after alignment to a granular
-- boundary (as specified by @PeriodInSeconds@). @AlignedEndTime@ will be
-- greater than or equal to the value of the user-specified @Endtime@.
--
-- 'identifier', 'getResourceMetricsResponse_identifier' - An immutable identifier for a data source that is unique for an Amazon
-- Web Services Region. Performance Insights gathers metrics from this data
-- source. In the console, the identifier is shown as /ResourceID/. When
-- you call @DescribeDBInstances@, the identifier is returned as
-- @DbiResourceId@.
--
-- 'metricList', 'getResourceMetricsResponse_metricList' - An array of metric results, where each array element contains all of the
-- data points for a particular dimension.
--
-- 'alignedStartTime', 'getResourceMetricsResponse_alignedStartTime' - The start time for the returned metrics, after alignment to a granular
-- boundary (as specified by @PeriodInSeconds@). @AlignedStartTime@ will be
-- less than or equal to the value of the user-specified @StartTime@.
--
-- 'httpStatus', 'getResourceMetricsResponse_httpStatus' - The response's http status code.
newGetResourceMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceMetricsResponse
newGetResourceMetricsResponse pHttpStatus_ =
  GetResourceMetricsResponse'
    { nextToken =
        Prelude.Nothing,
      alignedEndTime = Prelude.Nothing,
      identifier = Prelude.Nothing,
      metricList = Prelude.Nothing,
      alignedStartTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- token, up to the value specified by @MaxRecords@.
getResourceMetricsResponse_nextToken :: Lens.Lens' GetResourceMetricsResponse (Prelude.Maybe Prelude.Text)
getResourceMetricsResponse_nextToken = Lens.lens (\GetResourceMetricsResponse' {nextToken} -> nextToken) (\s@GetResourceMetricsResponse' {} a -> s {nextToken = a} :: GetResourceMetricsResponse)

-- | The end time for the returned metrics, after alignment to a granular
-- boundary (as specified by @PeriodInSeconds@). @AlignedEndTime@ will be
-- greater than or equal to the value of the user-specified @Endtime@.
getResourceMetricsResponse_alignedEndTime :: Lens.Lens' GetResourceMetricsResponse (Prelude.Maybe Prelude.UTCTime)
getResourceMetricsResponse_alignedEndTime = Lens.lens (\GetResourceMetricsResponse' {alignedEndTime} -> alignedEndTime) (\s@GetResourceMetricsResponse' {} a -> s {alignedEndTime = a} :: GetResourceMetricsResponse) Prelude.. Lens.mapping Data._Time

-- | An immutable identifier for a data source that is unique for an Amazon
-- Web Services Region. Performance Insights gathers metrics from this data
-- source. In the console, the identifier is shown as /ResourceID/. When
-- you call @DescribeDBInstances@, the identifier is returned as
-- @DbiResourceId@.
getResourceMetricsResponse_identifier :: Lens.Lens' GetResourceMetricsResponse (Prelude.Maybe Prelude.Text)
getResourceMetricsResponse_identifier = Lens.lens (\GetResourceMetricsResponse' {identifier} -> identifier) (\s@GetResourceMetricsResponse' {} a -> s {identifier = a} :: GetResourceMetricsResponse)

-- | An array of metric results, where each array element contains all of the
-- data points for a particular dimension.
getResourceMetricsResponse_metricList :: Lens.Lens' GetResourceMetricsResponse (Prelude.Maybe [MetricKeyDataPoints])
getResourceMetricsResponse_metricList = Lens.lens (\GetResourceMetricsResponse' {metricList} -> metricList) (\s@GetResourceMetricsResponse' {} a -> s {metricList = a} :: GetResourceMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The start time for the returned metrics, after alignment to a granular
-- boundary (as specified by @PeriodInSeconds@). @AlignedStartTime@ will be
-- less than or equal to the value of the user-specified @StartTime@.
getResourceMetricsResponse_alignedStartTime :: Lens.Lens' GetResourceMetricsResponse (Prelude.Maybe Prelude.UTCTime)
getResourceMetricsResponse_alignedStartTime = Lens.lens (\GetResourceMetricsResponse' {alignedStartTime} -> alignedStartTime) (\s@GetResourceMetricsResponse' {} a -> s {alignedStartTime = a} :: GetResourceMetricsResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getResourceMetricsResponse_httpStatus :: Lens.Lens' GetResourceMetricsResponse Prelude.Int
getResourceMetricsResponse_httpStatus = Lens.lens (\GetResourceMetricsResponse' {httpStatus} -> httpStatus) (\s@GetResourceMetricsResponse' {} a -> s {httpStatus = a} :: GetResourceMetricsResponse)

instance Prelude.NFData GetResourceMetricsResponse where
  rnf GetResourceMetricsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf alignedEndTime
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf metricList
      `Prelude.seq` Prelude.rnf alignedStartTime
      `Prelude.seq` Prelude.rnf httpStatus
