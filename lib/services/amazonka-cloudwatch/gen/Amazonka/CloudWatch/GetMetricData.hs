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
-- Module      : Amazonka.CloudWatch.GetMetricData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use the @GetMetricData@ API to retrieve CloudWatch metric
-- values. The operation can also include a CloudWatch Metrics Insights
-- query, and one or more metric math functions.
--
-- A @GetMetricData@ operation that does not include a query can retrieve
-- as many as 500 different metrics in a single request, with a total of as
-- many as 100,800 data points. You can also optionally perform metric math
-- expressions on the values of the returned statistics, to create new time
-- series that represent new insights into your data. For example, using
-- Lambda metrics, you could divide the Errors metric by the Invocations
-- metric to get an error rate time series. For more information about
-- metric math expressions, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/using-metric-math.html#metric-math-syntax Metric Math Syntax and Functions>
-- in the /Amazon CloudWatch User Guide/.
--
-- If you include a Metrics Insights query, each @GetMetricData@ operation
-- can include only one query. But the same @GetMetricData@ operation can
-- also retrieve other metrics. Metrics Insights queries can query only the
-- most recent three hours of metric data. For more information about
-- Metrics Insights, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/query_with_cloudwatch-metrics-insights.html Query your metrics with CloudWatch Metrics Insights>.
--
-- Calls to the @GetMetricData@ API have a different pricing structure than
-- calls to @GetMetricStatistics@. For more information about pricing, see
-- <https://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing>.
--
-- Amazon CloudWatch retains metric data as follows:
--
-- -   Data points with a period of less than 60 seconds are available for
--     3 hours. These data points are high-resolution metrics and are
--     available only for custom metrics that have been defined with a
--     @StorageResolution@ of 1.
--
-- -   Data points with a period of 60 seconds (1-minute) are available for
--     15 days.
--
-- -   Data points with a period of 300 seconds (5-minute) are available
--     for 63 days.
--
-- -   Data points with a period of 3600 seconds (1 hour) are available for
--     455 days (15 months).
--
-- Data points that are initially published with a shorter period are
-- aggregated together for long-term storage. For example, if you collect
-- data using a period of 1 minute, the data remains available for 15 days
-- with 1-minute resolution. After 15 days, this data is still available,
-- but is aggregated and retrievable only with a resolution of 5 minutes.
-- After 63 days, the data is further aggregated and is available with a
-- resolution of 1 hour.
--
-- If you omit @Unit@ in your request, all data that was collected with any
-- unit is returned, along with the corresponding units that were specified
-- when the data was reported to CloudWatch. If you specify a unit, the
-- operation returns only data that was collected with that unit specified.
-- If you specify a unit that does not match the data collected, the
-- results of the operation are null. CloudWatch does not perform unit
-- conversions.
--
-- __Using Metrics Insights queries with metric math__
--
-- You can\'t mix a Metric Insights query and metric math syntax in the
-- same expression, but you can reference results from a Metrics Insights
-- query within other Metric math expressions. A Metrics Insights query
-- without a __GROUP BY__ clause returns a single time-series (TS), and can
-- be used as input for a metric math expression that expects a single time
-- series. A Metrics Insights query with a __GROUP BY__ clause returns an
-- array of time-series (TS[]), and can be used as input for a metric math
-- expression that expects an array of time series.
--
-- This operation returns paginated results.
module Amazonka.CloudWatch.GetMetricData
  ( -- * Creating a Request
    GetMetricData (..),
    newGetMetricData,

    -- * Request Lenses
    getMetricData_nextToken,
    getMetricData_labelOptions,
    getMetricData_scanBy,
    getMetricData_maxDatapoints,
    getMetricData_metricDataQueries,
    getMetricData_startTime,
    getMetricData_endTime,

    -- * Destructuring the Response
    GetMetricDataResponse (..),
    newGetMetricDataResponse,

    -- * Response Lenses
    getMetricDataResponse_nextToken,
    getMetricDataResponse_messages,
    getMetricDataResponse_metricDataResults,
    getMetricDataResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMetricData' smart constructor.
data GetMetricData = GetMetricData'
  { -- | Include this value, if it was returned by the previous @GetMetricData@
    -- operation, to get the next set of data points.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This structure includes the @Timezone@ parameter, which you can use to
    -- specify your time zone so that the labels of returned data display the
    -- correct time for your time zone.
    labelOptions :: Prelude.Maybe LabelOptions,
    -- | The order in which data points should be returned. @TimestampDescending@
    -- returns the newest data first and paginates when the @MaxDatapoints@
    -- limit is reached. @TimestampAscending@ returns the oldest data first and
    -- paginates when the @MaxDatapoints@ limit is reached.
    scanBy :: Prelude.Maybe ScanBy,
    -- | The maximum number of data points the request should return before
    -- paginating. If you omit this, the default of 100,800 is used.
    maxDatapoints :: Prelude.Maybe Prelude.Int,
    -- | The metric queries to be returned. A single @GetMetricData@ call can
    -- include as many as 500 @MetricDataQuery@ structures. Each of these
    -- structures can specify either a metric to retrieve, a Metrics Insights
    -- query, or a math expression to perform on retrieved data.
    metricDataQueries :: [MetricDataQuery],
    -- | The time stamp indicating the earliest data to be returned.
    --
    -- The value specified is inclusive; results include data points with the
    -- specified time stamp.
    --
    -- CloudWatch rounds the specified time stamp as follows:
    --
    -- -   Start time less than 15 days ago - Round down to the nearest whole
    --     minute. For example, 12:32:34 is rounded down to 12:32:00.
    --
    -- -   Start time between 15 and 63 days ago - Round down to the nearest
    --     5-minute clock interval. For example, 12:32:34 is rounded down to
    --     12:30:00.
    --
    -- -   Start time greater than 63 days ago - Round down to the nearest
    --     1-hour clock interval. For example, 12:32:34 is rounded down to
    --     12:00:00.
    --
    -- If you set @Period@ to 5, 10, or 30, the start time of your request is
    -- rounded down to the nearest time that corresponds to even 5-, 10-, or
    -- 30-second divisions of a minute. For example, if you make a query at
    -- (HH:mm:ss) 01:05:23 for the previous 10-second period, the start time of
    -- your request is rounded down and you receive data from 01:05:10 to
    -- 01:05:20. If you make a query at 15:07:17 for the previous 5 minutes of
    -- data, using a period of 5 seconds, you receive data timestamped between
    -- 15:02:15 and 15:07:15.
    --
    -- For better performance, specify @StartTime@ and @EndTime@ values that
    -- align with the value of the metric\'s @Period@ and sync up with the
    -- beginning and end of an hour. For example, if the @Period@ of a metric
    -- is 5 minutes, specifying 12:05 or 12:30 as @StartTime@ can get a faster
    -- response from CloudWatch than setting 12:07 or 12:29 as the @StartTime@.
    startTime :: Data.ISO8601,
    -- | The time stamp indicating the latest data to be returned.
    --
    -- The value specified is exclusive; results include data points up to the
    -- specified time stamp.
    --
    -- For better performance, specify @StartTime@ and @EndTime@ values that
    -- align with the value of the metric\'s @Period@ and sync up with the
    -- beginning and end of an hour. For example, if the @Period@ of a metric
    -- is 5 minutes, specifying 12:05 or 12:30 as @EndTime@ can get a faster
    -- response from CloudWatch than setting 12:07 or 12:29 as the @EndTime@.
    endTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMetricData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getMetricData_nextToken' - Include this value, if it was returned by the previous @GetMetricData@
-- operation, to get the next set of data points.
--
-- 'labelOptions', 'getMetricData_labelOptions' - This structure includes the @Timezone@ parameter, which you can use to
-- specify your time zone so that the labels of returned data display the
-- correct time for your time zone.
--
-- 'scanBy', 'getMetricData_scanBy' - The order in which data points should be returned. @TimestampDescending@
-- returns the newest data first and paginates when the @MaxDatapoints@
-- limit is reached. @TimestampAscending@ returns the oldest data first and
-- paginates when the @MaxDatapoints@ limit is reached.
--
-- 'maxDatapoints', 'getMetricData_maxDatapoints' - The maximum number of data points the request should return before
-- paginating. If you omit this, the default of 100,800 is used.
--
-- 'metricDataQueries', 'getMetricData_metricDataQueries' - The metric queries to be returned. A single @GetMetricData@ call can
-- include as many as 500 @MetricDataQuery@ structures. Each of these
-- structures can specify either a metric to retrieve, a Metrics Insights
-- query, or a math expression to perform on retrieved data.
--
-- 'startTime', 'getMetricData_startTime' - The time stamp indicating the earliest data to be returned.
--
-- The value specified is inclusive; results include data points with the
-- specified time stamp.
--
-- CloudWatch rounds the specified time stamp as follows:
--
-- -   Start time less than 15 days ago - Round down to the nearest whole
--     minute. For example, 12:32:34 is rounded down to 12:32:00.
--
-- -   Start time between 15 and 63 days ago - Round down to the nearest
--     5-minute clock interval. For example, 12:32:34 is rounded down to
--     12:30:00.
--
-- -   Start time greater than 63 days ago - Round down to the nearest
--     1-hour clock interval. For example, 12:32:34 is rounded down to
--     12:00:00.
--
-- If you set @Period@ to 5, 10, or 30, the start time of your request is
-- rounded down to the nearest time that corresponds to even 5-, 10-, or
-- 30-second divisions of a minute. For example, if you make a query at
-- (HH:mm:ss) 01:05:23 for the previous 10-second period, the start time of
-- your request is rounded down and you receive data from 01:05:10 to
-- 01:05:20. If you make a query at 15:07:17 for the previous 5 minutes of
-- data, using a period of 5 seconds, you receive data timestamped between
-- 15:02:15 and 15:07:15.
--
-- For better performance, specify @StartTime@ and @EndTime@ values that
-- align with the value of the metric\'s @Period@ and sync up with the
-- beginning and end of an hour. For example, if the @Period@ of a metric
-- is 5 minutes, specifying 12:05 or 12:30 as @StartTime@ can get a faster
-- response from CloudWatch than setting 12:07 or 12:29 as the @StartTime@.
--
-- 'endTime', 'getMetricData_endTime' - The time stamp indicating the latest data to be returned.
--
-- The value specified is exclusive; results include data points up to the
-- specified time stamp.
--
-- For better performance, specify @StartTime@ and @EndTime@ values that
-- align with the value of the metric\'s @Period@ and sync up with the
-- beginning and end of an hour. For example, if the @Period@ of a metric
-- is 5 minutes, specifying 12:05 or 12:30 as @EndTime@ can get a faster
-- response from CloudWatch than setting 12:07 or 12:29 as the @EndTime@.
newGetMetricData ::
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  GetMetricData
newGetMetricData pStartTime_ pEndTime_ =
  GetMetricData'
    { nextToken = Prelude.Nothing,
      labelOptions = Prelude.Nothing,
      scanBy = Prelude.Nothing,
      maxDatapoints = Prelude.Nothing,
      metricDataQueries = Prelude.mempty,
      startTime = Data._Time Lens.# pStartTime_,
      endTime = Data._Time Lens.# pEndTime_
    }

-- | Include this value, if it was returned by the previous @GetMetricData@
-- operation, to get the next set of data points.
getMetricData_nextToken :: Lens.Lens' GetMetricData (Prelude.Maybe Prelude.Text)
getMetricData_nextToken = Lens.lens (\GetMetricData' {nextToken} -> nextToken) (\s@GetMetricData' {} a -> s {nextToken = a} :: GetMetricData)

-- | This structure includes the @Timezone@ parameter, which you can use to
-- specify your time zone so that the labels of returned data display the
-- correct time for your time zone.
getMetricData_labelOptions :: Lens.Lens' GetMetricData (Prelude.Maybe LabelOptions)
getMetricData_labelOptions = Lens.lens (\GetMetricData' {labelOptions} -> labelOptions) (\s@GetMetricData' {} a -> s {labelOptions = a} :: GetMetricData)

-- | The order in which data points should be returned. @TimestampDescending@
-- returns the newest data first and paginates when the @MaxDatapoints@
-- limit is reached. @TimestampAscending@ returns the oldest data first and
-- paginates when the @MaxDatapoints@ limit is reached.
getMetricData_scanBy :: Lens.Lens' GetMetricData (Prelude.Maybe ScanBy)
getMetricData_scanBy = Lens.lens (\GetMetricData' {scanBy} -> scanBy) (\s@GetMetricData' {} a -> s {scanBy = a} :: GetMetricData)

-- | The maximum number of data points the request should return before
-- paginating. If you omit this, the default of 100,800 is used.
getMetricData_maxDatapoints :: Lens.Lens' GetMetricData (Prelude.Maybe Prelude.Int)
getMetricData_maxDatapoints = Lens.lens (\GetMetricData' {maxDatapoints} -> maxDatapoints) (\s@GetMetricData' {} a -> s {maxDatapoints = a} :: GetMetricData)

-- | The metric queries to be returned. A single @GetMetricData@ call can
-- include as many as 500 @MetricDataQuery@ structures. Each of these
-- structures can specify either a metric to retrieve, a Metrics Insights
-- query, or a math expression to perform on retrieved data.
getMetricData_metricDataQueries :: Lens.Lens' GetMetricData [MetricDataQuery]
getMetricData_metricDataQueries = Lens.lens (\GetMetricData' {metricDataQueries} -> metricDataQueries) (\s@GetMetricData' {} a -> s {metricDataQueries = a} :: GetMetricData) Prelude.. Lens.coerced

-- | The time stamp indicating the earliest data to be returned.
--
-- The value specified is inclusive; results include data points with the
-- specified time stamp.
--
-- CloudWatch rounds the specified time stamp as follows:
--
-- -   Start time less than 15 days ago - Round down to the nearest whole
--     minute. For example, 12:32:34 is rounded down to 12:32:00.
--
-- -   Start time between 15 and 63 days ago - Round down to the nearest
--     5-minute clock interval. For example, 12:32:34 is rounded down to
--     12:30:00.
--
-- -   Start time greater than 63 days ago - Round down to the nearest
--     1-hour clock interval. For example, 12:32:34 is rounded down to
--     12:00:00.
--
-- If you set @Period@ to 5, 10, or 30, the start time of your request is
-- rounded down to the nearest time that corresponds to even 5-, 10-, or
-- 30-second divisions of a minute. For example, if you make a query at
-- (HH:mm:ss) 01:05:23 for the previous 10-second period, the start time of
-- your request is rounded down and you receive data from 01:05:10 to
-- 01:05:20. If you make a query at 15:07:17 for the previous 5 minutes of
-- data, using a period of 5 seconds, you receive data timestamped between
-- 15:02:15 and 15:07:15.
--
-- For better performance, specify @StartTime@ and @EndTime@ values that
-- align with the value of the metric\'s @Period@ and sync up with the
-- beginning and end of an hour. For example, if the @Period@ of a metric
-- is 5 minutes, specifying 12:05 or 12:30 as @StartTime@ can get a faster
-- response from CloudWatch than setting 12:07 or 12:29 as the @StartTime@.
getMetricData_startTime :: Lens.Lens' GetMetricData Prelude.UTCTime
getMetricData_startTime = Lens.lens (\GetMetricData' {startTime} -> startTime) (\s@GetMetricData' {} a -> s {startTime = a} :: GetMetricData) Prelude.. Data._Time

-- | The time stamp indicating the latest data to be returned.
--
-- The value specified is exclusive; results include data points up to the
-- specified time stamp.
--
-- For better performance, specify @StartTime@ and @EndTime@ values that
-- align with the value of the metric\'s @Period@ and sync up with the
-- beginning and end of an hour. For example, if the @Period@ of a metric
-- is 5 minutes, specifying 12:05 or 12:30 as @EndTime@ can get a faster
-- response from CloudWatch than setting 12:07 or 12:29 as the @EndTime@.
getMetricData_endTime :: Lens.Lens' GetMetricData Prelude.UTCTime
getMetricData_endTime = Lens.lens (\GetMetricData' {endTime} -> endTime) (\s@GetMetricData' {} a -> s {endTime = a} :: GetMetricData) Prelude.. Data._Time

instance Core.AWSPager GetMetricData where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getMetricDataResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getMetricDataResponse_metricDataResults
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getMetricDataResponse_messages Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getMetricData_nextToken
          Lens..~ rs
          Lens.^? getMetricDataResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetMetricData where
  type
    AWSResponse GetMetricData =
      GetMetricDataResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetMetricDataResult"
      ( \s h x ->
          GetMetricDataResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "Messages" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> ( x Data..@? "MetricDataResults"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMetricData where
  hashWithSalt _salt GetMetricData' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` labelOptions
      `Prelude.hashWithSalt` scanBy
      `Prelude.hashWithSalt` maxDatapoints
      `Prelude.hashWithSalt` metricDataQueries
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData GetMetricData where
  rnf GetMetricData' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf labelOptions
      `Prelude.seq` Prelude.rnf scanBy
      `Prelude.seq` Prelude.rnf maxDatapoints
      `Prelude.seq` Prelude.rnf metricDataQueries
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Data.ToHeaders GetMetricData where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetMetricData where
  toPath = Prelude.const "/"

instance Data.ToQuery GetMetricData where
  toQuery GetMetricData' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetMetricData" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken,
        "LabelOptions" Data.=: labelOptions,
        "ScanBy" Data.=: scanBy,
        "MaxDatapoints" Data.=: maxDatapoints,
        "MetricDataQueries"
          Data.=: Data.toQueryList "member" metricDataQueries,
        "StartTime" Data.=: startTime,
        "EndTime" Data.=: endTime
      ]

-- | /See:/ 'newGetMetricDataResponse' smart constructor.
data GetMetricDataResponse = GetMetricDataResponse'
  { -- | A token that marks the next batch of returned results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Contains a message about this @GetMetricData@ operation, if the
    -- operation results in such a message. An example of a message that might
    -- be returned is @Maximum number of allowed metrics exceeded@. If there is
    -- a message, as much of the operation as possible is still executed.
    --
    -- A message appears here only if it is related to the global
    -- @GetMetricData@ operation. Any message about a specific metric returned
    -- by the operation appears in the @MetricDataResult@ object returned for
    -- that metric.
    messages :: Prelude.Maybe [MessageData],
    -- | The metrics that are returned, including the metric name, namespace, and
    -- dimensions.
    metricDataResults :: Prelude.Maybe [MetricDataResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMetricDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getMetricDataResponse_nextToken' - A token that marks the next batch of returned results.
--
-- 'messages', 'getMetricDataResponse_messages' - Contains a message about this @GetMetricData@ operation, if the
-- operation results in such a message. An example of a message that might
-- be returned is @Maximum number of allowed metrics exceeded@. If there is
-- a message, as much of the operation as possible is still executed.
--
-- A message appears here only if it is related to the global
-- @GetMetricData@ operation. Any message about a specific metric returned
-- by the operation appears in the @MetricDataResult@ object returned for
-- that metric.
--
-- 'metricDataResults', 'getMetricDataResponse_metricDataResults' - The metrics that are returned, including the metric name, namespace, and
-- dimensions.
--
-- 'httpStatus', 'getMetricDataResponse_httpStatus' - The response's http status code.
newGetMetricDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMetricDataResponse
newGetMetricDataResponse pHttpStatus_ =
  GetMetricDataResponse'
    { nextToken = Prelude.Nothing,
      messages = Prelude.Nothing,
      metricDataResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that marks the next batch of returned results.
getMetricDataResponse_nextToken :: Lens.Lens' GetMetricDataResponse (Prelude.Maybe Prelude.Text)
getMetricDataResponse_nextToken = Lens.lens (\GetMetricDataResponse' {nextToken} -> nextToken) (\s@GetMetricDataResponse' {} a -> s {nextToken = a} :: GetMetricDataResponse)

-- | Contains a message about this @GetMetricData@ operation, if the
-- operation results in such a message. An example of a message that might
-- be returned is @Maximum number of allowed metrics exceeded@. If there is
-- a message, as much of the operation as possible is still executed.
--
-- A message appears here only if it is related to the global
-- @GetMetricData@ operation. Any message about a specific metric returned
-- by the operation appears in the @MetricDataResult@ object returned for
-- that metric.
getMetricDataResponse_messages :: Lens.Lens' GetMetricDataResponse (Prelude.Maybe [MessageData])
getMetricDataResponse_messages = Lens.lens (\GetMetricDataResponse' {messages} -> messages) (\s@GetMetricDataResponse' {} a -> s {messages = a} :: GetMetricDataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The metrics that are returned, including the metric name, namespace, and
-- dimensions.
getMetricDataResponse_metricDataResults :: Lens.Lens' GetMetricDataResponse (Prelude.Maybe [MetricDataResult])
getMetricDataResponse_metricDataResults = Lens.lens (\GetMetricDataResponse' {metricDataResults} -> metricDataResults) (\s@GetMetricDataResponse' {} a -> s {metricDataResults = a} :: GetMetricDataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getMetricDataResponse_httpStatus :: Lens.Lens' GetMetricDataResponse Prelude.Int
getMetricDataResponse_httpStatus = Lens.lens (\GetMetricDataResponse' {httpStatus} -> httpStatus) (\s@GetMetricDataResponse' {} a -> s {httpStatus = a} :: GetMetricDataResponse)

instance Prelude.NFData GetMetricDataResponse where
  rnf GetMetricDataResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf messages
      `Prelude.seq` Prelude.rnf metricDataResults
      `Prelude.seq` Prelude.rnf httpStatus
