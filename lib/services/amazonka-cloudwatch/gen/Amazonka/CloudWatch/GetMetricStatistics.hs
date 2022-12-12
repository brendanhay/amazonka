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
-- Module      : Amazonka.CloudWatch.GetMetricStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets statistics for the specified metric.
--
-- The maximum number of data points returned from a single call is 1,440.
-- If you request more than 1,440 data points, CloudWatch returns an error.
-- To reduce the number of data points, you can narrow the specified time
-- range and make multiple requests across adjacent time ranges, or you can
-- increase the specified period. Data points are not returned in
-- chronological order.
--
-- CloudWatch aggregates data points based on the length of the period that
-- you specify. For example, if you request statistics with a one-hour
-- period, CloudWatch aggregates all data points with time stamps that fall
-- within each one-hour period. Therefore, the number of values aggregated
-- by CloudWatch is larger than the number of data points returned.
--
-- CloudWatch needs raw data points to calculate percentile statistics. If
-- you publish data using a statistic set instead, you can only retrieve
-- percentile statistics for this data if one of the following conditions
-- is true:
--
-- -   The SampleCount value of the statistic set is 1.
--
-- -   The Min and the Max values of the statistic set are equal.
--
-- Percentile statistics are not available for metrics when any of the
-- metric values are negative numbers.
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
-- CloudWatch started retaining 5-minute and 1-hour metric data as of July
-- 9, 2016.
--
-- For information about metrics and dimensions supported by Amazon Web
-- Services services, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CW_Support_For_AWS.html Amazon CloudWatch Metrics and Dimensions Reference>
-- in the /Amazon CloudWatch User Guide/.
module Amazonka.CloudWatch.GetMetricStatistics
  ( -- * Creating a Request
    GetMetricStatistics (..),
    newGetMetricStatistics,

    -- * Request Lenses
    getMetricStatistics_dimensions,
    getMetricStatistics_extendedStatistics,
    getMetricStatistics_statistics,
    getMetricStatistics_unit,
    getMetricStatistics_namespace,
    getMetricStatistics_metricName,
    getMetricStatistics_startTime,
    getMetricStatistics_endTime,
    getMetricStatistics_period,

    -- * Destructuring the Response
    GetMetricStatisticsResponse (..),
    newGetMetricStatisticsResponse,

    -- * Response Lenses
    getMetricStatisticsResponse_datapoints,
    getMetricStatisticsResponse_label,
    getMetricStatisticsResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMetricStatistics' smart constructor.
data GetMetricStatistics = GetMetricStatistics'
  { -- | The dimensions. If the metric contains multiple dimensions, you must
    -- include a value for each dimension. CloudWatch treats each unique
    -- combination of dimensions as a separate metric. If a specific
    -- combination of dimensions was not published, you can\'t retrieve
    -- statistics for it. You must specify the same dimensions that were used
    -- when the metrics were created. For an example, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#dimension-combinations Dimension Combinations>
    -- in the /Amazon CloudWatch User Guide/. For more information about
    -- specifying dimensions, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publishing Metrics>
    -- in the /Amazon CloudWatch User Guide/.
    dimensions :: Prelude.Maybe [Dimension],
    -- | The percentile statistics. Specify values between p0.0 and p100. When
    -- calling @GetMetricStatistics@, you must specify either @Statistics@ or
    -- @ExtendedStatistics@, but not both. Percentile statistics are not
    -- available for metrics when any of the metric values are negative
    -- numbers.
    extendedStatistics :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The metric statistics, other than percentile. For percentile statistics,
    -- use @ExtendedStatistics@. When calling @GetMetricStatistics@, you must
    -- specify either @Statistics@ or @ExtendedStatistics@, but not both.
    statistics :: Prelude.Maybe (Prelude.NonEmpty Statistic),
    -- | The unit for a given metric. If you omit @Unit@, all data that was
    -- collected with any unit is returned, along with the corresponding units
    -- that were specified when the data was reported to CloudWatch. If you
    -- specify a unit, the operation returns only data that was collected with
    -- that unit specified. If you specify a unit that does not match the data
    -- collected, the results of the operation are null. CloudWatch does not
    -- perform unit conversions.
    unit :: Prelude.Maybe StandardUnit,
    -- | The namespace of the metric, with or without spaces.
    namespace :: Prelude.Text,
    -- | The name of the metric, with or without spaces.
    metricName :: Prelude.Text,
    -- | The time stamp that determines the first data point to return. Start
    -- times are evaluated relative to the time that CloudWatch receives the
    -- request.
    --
    -- The value specified is inclusive; results include data points with the
    -- specified time stamp. In a raw HTTP query, the time stamp must be in ISO
    -- 8601 UTC format (for example, 2016-10-03T23:00:00Z).
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
    startTime :: Data.ISO8601,
    -- | The time stamp that determines the last data point to return.
    --
    -- The value specified is exclusive; results include data points up to the
    -- specified time stamp. In a raw HTTP query, the time stamp must be in ISO
    -- 8601 UTC format (for example, 2016-10-10T23:00:00Z).
    endTime :: Data.ISO8601,
    -- | The granularity, in seconds, of the returned data points. For metrics
    -- with regular resolution, a period can be as short as one minute (60
    -- seconds) and must be a multiple of 60. For high-resolution metrics that
    -- are collected at intervals of less than one minute, the period can be 1,
    -- 5, 10, 30, 60, or any multiple of 60. High-resolution metrics are those
    -- metrics stored by a @PutMetricData@ call that includes a
    -- @StorageResolution@ of 1 second.
    --
    -- If the @StartTime@ parameter specifies a time stamp that is greater than
    -- 3 hours ago, you must specify the period as follows or no data points in
    -- that time range is returned:
    --
    -- -   Start time between 3 hours and 15 days ago - Use a multiple of 60
    --     seconds (1 minute).
    --
    -- -   Start time between 15 and 63 days ago - Use a multiple of 300
    --     seconds (5 minutes).
    --
    -- -   Start time greater than 63 days ago - Use a multiple of 3600 seconds
    --     (1 hour).
    period :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMetricStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'getMetricStatistics_dimensions' - The dimensions. If the metric contains multiple dimensions, you must
-- include a value for each dimension. CloudWatch treats each unique
-- combination of dimensions as a separate metric. If a specific
-- combination of dimensions was not published, you can\'t retrieve
-- statistics for it. You must specify the same dimensions that were used
-- when the metrics were created. For an example, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#dimension-combinations Dimension Combinations>
-- in the /Amazon CloudWatch User Guide/. For more information about
-- specifying dimensions, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publishing Metrics>
-- in the /Amazon CloudWatch User Guide/.
--
-- 'extendedStatistics', 'getMetricStatistics_extendedStatistics' - The percentile statistics. Specify values between p0.0 and p100. When
-- calling @GetMetricStatistics@, you must specify either @Statistics@ or
-- @ExtendedStatistics@, but not both. Percentile statistics are not
-- available for metrics when any of the metric values are negative
-- numbers.
--
-- 'statistics', 'getMetricStatistics_statistics' - The metric statistics, other than percentile. For percentile statistics,
-- use @ExtendedStatistics@. When calling @GetMetricStatistics@, you must
-- specify either @Statistics@ or @ExtendedStatistics@, but not both.
--
-- 'unit', 'getMetricStatistics_unit' - The unit for a given metric. If you omit @Unit@, all data that was
-- collected with any unit is returned, along with the corresponding units
-- that were specified when the data was reported to CloudWatch. If you
-- specify a unit, the operation returns only data that was collected with
-- that unit specified. If you specify a unit that does not match the data
-- collected, the results of the operation are null. CloudWatch does not
-- perform unit conversions.
--
-- 'namespace', 'getMetricStatistics_namespace' - The namespace of the metric, with or without spaces.
--
-- 'metricName', 'getMetricStatistics_metricName' - The name of the metric, with or without spaces.
--
-- 'startTime', 'getMetricStatistics_startTime' - The time stamp that determines the first data point to return. Start
-- times are evaluated relative to the time that CloudWatch receives the
-- request.
--
-- The value specified is inclusive; results include data points with the
-- specified time stamp. In a raw HTTP query, the time stamp must be in ISO
-- 8601 UTC format (for example, 2016-10-03T23:00:00Z).
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
-- 'endTime', 'getMetricStatistics_endTime' - The time stamp that determines the last data point to return.
--
-- The value specified is exclusive; results include data points up to the
-- specified time stamp. In a raw HTTP query, the time stamp must be in ISO
-- 8601 UTC format (for example, 2016-10-10T23:00:00Z).
--
-- 'period', 'getMetricStatistics_period' - The granularity, in seconds, of the returned data points. For metrics
-- with regular resolution, a period can be as short as one minute (60
-- seconds) and must be a multiple of 60. For high-resolution metrics that
-- are collected at intervals of less than one minute, the period can be 1,
-- 5, 10, 30, 60, or any multiple of 60. High-resolution metrics are those
-- metrics stored by a @PutMetricData@ call that includes a
-- @StorageResolution@ of 1 second.
--
-- If the @StartTime@ parameter specifies a time stamp that is greater than
-- 3 hours ago, you must specify the period as follows or no data points in
-- that time range is returned:
--
-- -   Start time between 3 hours and 15 days ago - Use a multiple of 60
--     seconds (1 minute).
--
-- -   Start time between 15 and 63 days ago - Use a multiple of 300
--     seconds (5 minutes).
--
-- -   Start time greater than 63 days ago - Use a multiple of 3600 seconds
--     (1 hour).
newGetMetricStatistics ::
  -- | 'namespace'
  Prelude.Text ->
  -- | 'metricName'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'period'
  Prelude.Natural ->
  GetMetricStatistics
newGetMetricStatistics
  pNamespace_
  pMetricName_
  pStartTime_
  pEndTime_
  pPeriod_ =
    GetMetricStatistics'
      { dimensions = Prelude.Nothing,
        extendedStatistics = Prelude.Nothing,
        statistics = Prelude.Nothing,
        unit = Prelude.Nothing,
        namespace = pNamespace_,
        metricName = pMetricName_,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_,
        period = pPeriod_
      }

-- | The dimensions. If the metric contains multiple dimensions, you must
-- include a value for each dimension. CloudWatch treats each unique
-- combination of dimensions as a separate metric. If a specific
-- combination of dimensions was not published, you can\'t retrieve
-- statistics for it. You must specify the same dimensions that were used
-- when the metrics were created. For an example, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#dimension-combinations Dimension Combinations>
-- in the /Amazon CloudWatch User Guide/. For more information about
-- specifying dimensions, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publishing Metrics>
-- in the /Amazon CloudWatch User Guide/.
getMetricStatistics_dimensions :: Lens.Lens' GetMetricStatistics (Prelude.Maybe [Dimension])
getMetricStatistics_dimensions = Lens.lens (\GetMetricStatistics' {dimensions} -> dimensions) (\s@GetMetricStatistics' {} a -> s {dimensions = a} :: GetMetricStatistics) Prelude.. Lens.mapping Lens.coerced

-- | The percentile statistics. Specify values between p0.0 and p100. When
-- calling @GetMetricStatistics@, you must specify either @Statistics@ or
-- @ExtendedStatistics@, but not both. Percentile statistics are not
-- available for metrics when any of the metric values are negative
-- numbers.
getMetricStatistics_extendedStatistics :: Lens.Lens' GetMetricStatistics (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getMetricStatistics_extendedStatistics = Lens.lens (\GetMetricStatistics' {extendedStatistics} -> extendedStatistics) (\s@GetMetricStatistics' {} a -> s {extendedStatistics = a} :: GetMetricStatistics) Prelude.. Lens.mapping Lens.coerced

-- | The metric statistics, other than percentile. For percentile statistics,
-- use @ExtendedStatistics@. When calling @GetMetricStatistics@, you must
-- specify either @Statistics@ or @ExtendedStatistics@, but not both.
getMetricStatistics_statistics :: Lens.Lens' GetMetricStatistics (Prelude.Maybe (Prelude.NonEmpty Statistic))
getMetricStatistics_statistics = Lens.lens (\GetMetricStatistics' {statistics} -> statistics) (\s@GetMetricStatistics' {} a -> s {statistics = a} :: GetMetricStatistics) Prelude.. Lens.mapping Lens.coerced

-- | The unit for a given metric. If you omit @Unit@, all data that was
-- collected with any unit is returned, along with the corresponding units
-- that were specified when the data was reported to CloudWatch. If you
-- specify a unit, the operation returns only data that was collected with
-- that unit specified. If you specify a unit that does not match the data
-- collected, the results of the operation are null. CloudWatch does not
-- perform unit conversions.
getMetricStatistics_unit :: Lens.Lens' GetMetricStatistics (Prelude.Maybe StandardUnit)
getMetricStatistics_unit = Lens.lens (\GetMetricStatistics' {unit} -> unit) (\s@GetMetricStatistics' {} a -> s {unit = a} :: GetMetricStatistics)

-- | The namespace of the metric, with or without spaces.
getMetricStatistics_namespace :: Lens.Lens' GetMetricStatistics Prelude.Text
getMetricStatistics_namespace = Lens.lens (\GetMetricStatistics' {namespace} -> namespace) (\s@GetMetricStatistics' {} a -> s {namespace = a} :: GetMetricStatistics)

-- | The name of the metric, with or without spaces.
getMetricStatistics_metricName :: Lens.Lens' GetMetricStatistics Prelude.Text
getMetricStatistics_metricName = Lens.lens (\GetMetricStatistics' {metricName} -> metricName) (\s@GetMetricStatistics' {} a -> s {metricName = a} :: GetMetricStatistics)

-- | The time stamp that determines the first data point to return. Start
-- times are evaluated relative to the time that CloudWatch receives the
-- request.
--
-- The value specified is inclusive; results include data points with the
-- specified time stamp. In a raw HTTP query, the time stamp must be in ISO
-- 8601 UTC format (for example, 2016-10-03T23:00:00Z).
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
getMetricStatistics_startTime :: Lens.Lens' GetMetricStatistics Prelude.UTCTime
getMetricStatistics_startTime = Lens.lens (\GetMetricStatistics' {startTime} -> startTime) (\s@GetMetricStatistics' {} a -> s {startTime = a} :: GetMetricStatistics) Prelude.. Data._Time

-- | The time stamp that determines the last data point to return.
--
-- The value specified is exclusive; results include data points up to the
-- specified time stamp. In a raw HTTP query, the time stamp must be in ISO
-- 8601 UTC format (for example, 2016-10-10T23:00:00Z).
getMetricStatistics_endTime :: Lens.Lens' GetMetricStatistics Prelude.UTCTime
getMetricStatistics_endTime = Lens.lens (\GetMetricStatistics' {endTime} -> endTime) (\s@GetMetricStatistics' {} a -> s {endTime = a} :: GetMetricStatistics) Prelude.. Data._Time

-- | The granularity, in seconds, of the returned data points. For metrics
-- with regular resolution, a period can be as short as one minute (60
-- seconds) and must be a multiple of 60. For high-resolution metrics that
-- are collected at intervals of less than one minute, the period can be 1,
-- 5, 10, 30, 60, or any multiple of 60. High-resolution metrics are those
-- metrics stored by a @PutMetricData@ call that includes a
-- @StorageResolution@ of 1 second.
--
-- If the @StartTime@ parameter specifies a time stamp that is greater than
-- 3 hours ago, you must specify the period as follows or no data points in
-- that time range is returned:
--
-- -   Start time between 3 hours and 15 days ago - Use a multiple of 60
--     seconds (1 minute).
--
-- -   Start time between 15 and 63 days ago - Use a multiple of 300
--     seconds (5 minutes).
--
-- -   Start time greater than 63 days ago - Use a multiple of 3600 seconds
--     (1 hour).
getMetricStatistics_period :: Lens.Lens' GetMetricStatistics Prelude.Natural
getMetricStatistics_period = Lens.lens (\GetMetricStatistics' {period} -> period) (\s@GetMetricStatistics' {} a -> s {period = a} :: GetMetricStatistics)

instance Core.AWSRequest GetMetricStatistics where
  type
    AWSResponse GetMetricStatistics =
      GetMetricStatisticsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetMetricStatisticsResult"
      ( \s h x ->
          GetMetricStatisticsResponse'
            Prelude.<$> ( x Data..@? "Datapoints" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "Label")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMetricStatistics where
  hashWithSalt _salt GetMetricStatistics' {..} =
    _salt `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` extendedStatistics
      `Prelude.hashWithSalt` statistics
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` period

instance Prelude.NFData GetMetricStatistics where
  rnf GetMetricStatistics' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf extendedStatistics
      `Prelude.seq` Prelude.rnf statistics
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf period

instance Data.ToHeaders GetMetricStatistics where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetMetricStatistics where
  toPath = Prelude.const "/"

instance Data.ToQuery GetMetricStatistics where
  toQuery GetMetricStatistics' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetMetricStatistics" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "Dimensions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> dimensions),
        "ExtendedStatistics"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> extendedStatistics
            ),
        "Statistics"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> statistics),
        "Unit" Data.=: unit,
        "Namespace" Data.=: namespace,
        "MetricName" Data.=: metricName,
        "StartTime" Data.=: startTime,
        "EndTime" Data.=: endTime,
        "Period" Data.=: period
      ]

-- | /See:/ 'newGetMetricStatisticsResponse' smart constructor.
data GetMetricStatisticsResponse = GetMetricStatisticsResponse'
  { -- | The data points for the specified metric.
    datapoints :: Prelude.Maybe [Datapoint],
    -- | A label for the specified metric.
    label :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMetricStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datapoints', 'getMetricStatisticsResponse_datapoints' - The data points for the specified metric.
--
-- 'label', 'getMetricStatisticsResponse_label' - A label for the specified metric.
--
-- 'httpStatus', 'getMetricStatisticsResponse_httpStatus' - The response's http status code.
newGetMetricStatisticsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMetricStatisticsResponse
newGetMetricStatisticsResponse pHttpStatus_ =
  GetMetricStatisticsResponse'
    { datapoints =
        Prelude.Nothing,
      label = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The data points for the specified metric.
getMetricStatisticsResponse_datapoints :: Lens.Lens' GetMetricStatisticsResponse (Prelude.Maybe [Datapoint])
getMetricStatisticsResponse_datapoints = Lens.lens (\GetMetricStatisticsResponse' {datapoints} -> datapoints) (\s@GetMetricStatisticsResponse' {} a -> s {datapoints = a} :: GetMetricStatisticsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A label for the specified metric.
getMetricStatisticsResponse_label :: Lens.Lens' GetMetricStatisticsResponse (Prelude.Maybe Prelude.Text)
getMetricStatisticsResponse_label = Lens.lens (\GetMetricStatisticsResponse' {label} -> label) (\s@GetMetricStatisticsResponse' {} a -> s {label = a} :: GetMetricStatisticsResponse)

-- | The response's http status code.
getMetricStatisticsResponse_httpStatus :: Lens.Lens' GetMetricStatisticsResponse Prelude.Int
getMetricStatisticsResponse_httpStatus = Lens.lens (\GetMetricStatisticsResponse' {httpStatus} -> httpStatus) (\s@GetMetricStatisticsResponse' {} a -> s {httpStatus = a} :: GetMetricStatisticsResponse)

instance Prelude.NFData GetMetricStatisticsResponse where
  rnf GetMetricStatisticsResponse' {..} =
    Prelude.rnf datapoints
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf httpStatus
