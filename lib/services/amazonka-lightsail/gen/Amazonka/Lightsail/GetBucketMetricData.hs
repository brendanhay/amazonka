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
-- Module      : Amazonka.Lightsail.GetBucketMetricData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data points of a specific metric for an Amazon Lightsail
-- bucket.
--
-- Metrics report the utilization of a bucket. View and collect metric data
-- regularly to monitor the number of objects stored in a bucket (including
-- object versions) and the storage space used by those objects.
module Amazonka.Lightsail.GetBucketMetricData
  ( -- * Creating a Request
    GetBucketMetricData (..),
    newGetBucketMetricData,

    -- * Request Lenses
    getBucketMetricData_bucketName,
    getBucketMetricData_metricName,
    getBucketMetricData_startTime,
    getBucketMetricData_endTime,
    getBucketMetricData_period,
    getBucketMetricData_statistics,
    getBucketMetricData_unit,

    -- * Destructuring the Response
    GetBucketMetricDataResponse (..),
    newGetBucketMetricDataResponse,

    -- * Response Lenses
    getBucketMetricDataResponse_metricData,
    getBucketMetricDataResponse_metricName,
    getBucketMetricDataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBucketMetricData' smart constructor.
data GetBucketMetricData = GetBucketMetricData'
  { -- | The name of the bucket for which to get metric data.
    bucketName :: Prelude.Text,
    -- | The metric for which you want to return information.
    --
    -- Valid bucket metric names are listed below, along with the most useful
    -- statistics to include in your request, and the published unit value.
    --
    -- These bucket metrics are reported once per day.
    --
    -- -   __@BucketSizeBytes@__ - The amount of data in bytes stored in a
    --     bucket. This value is calculated by summing the size of all objects
    --     in the bucket (including object versions), including the size of all
    --     parts for all incomplete multipart uploads to the bucket.
    --
    --     Statistics: The most useful statistic is @Maximum@.
    --
    --     Unit: The published unit is @Bytes@.
    --
    -- -   __@NumberOfObjects@__ - The total number of objects stored in a
    --     bucket. This value is calculated by counting all objects in the
    --     bucket (including object versions) and the total number of parts for
    --     all incomplete multipart uploads to the bucket.
    --
    --     Statistics: The most useful statistic is @Average@.
    --
    --     Unit: The published unit is @Count@.
    metricName :: BucketMetricName,
    -- | The timestamp indicating the earliest data to be returned.
    startTime :: Data.POSIX,
    -- | The timestamp indicating the latest data to be returned.
    endTime :: Data.POSIX,
    -- | The granularity, in seconds, of the returned data points.
    --
    -- Bucket storage metrics are reported once per day. Therefore, you should
    -- specify a period of 86400 seconds, which is the number of seconds in a
    -- day.
    period :: Prelude.Natural,
    -- | The statistic for the metric.
    --
    -- The following statistics are available:
    --
    -- -   @Minimum@ - The lowest value observed during the specified period.
    --     Use this value to determine low volumes of activity for your
    --     application.
    --
    -- -   @Maximum@ - The highest value observed during the specified period.
    --     Use this value to determine high volumes of activity for your
    --     application.
    --
    -- -   @Sum@ - The sum of all values submitted for the matching metric. You
    --     can use this statistic to determine the total volume of a metric.
    --
    -- -   @Average@ - The value of @Sum@ \/ @SampleCount@ during the specified
    --     period. By comparing this statistic with the @Minimum@ and @Maximum@
    --     values, you can determine the full scope of a metric and how close
    --     the average use is to the @Minimum@ and @Maximum@ values. This
    --     comparison helps you to know when to increase or decrease your
    --     resources.
    --
    -- -   @SampleCount@ - The count, or number, of data points used for the
    --     statistical calculation.
    statistics :: [MetricStatistic],
    -- | The unit for the metric data request.
    --
    -- Valid units depend on the metric data being requested. For the valid
    -- units with each available metric, see the @metricName@ parameter.
    unit :: MetricUnit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketMetricData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'getBucketMetricData_bucketName' - The name of the bucket for which to get metric data.
--
-- 'metricName', 'getBucketMetricData_metricName' - The metric for which you want to return information.
--
-- Valid bucket metric names are listed below, along with the most useful
-- statistics to include in your request, and the published unit value.
--
-- These bucket metrics are reported once per day.
--
-- -   __@BucketSizeBytes@__ - The amount of data in bytes stored in a
--     bucket. This value is calculated by summing the size of all objects
--     in the bucket (including object versions), including the size of all
--     parts for all incomplete multipart uploads to the bucket.
--
--     Statistics: The most useful statistic is @Maximum@.
--
--     Unit: The published unit is @Bytes@.
--
-- -   __@NumberOfObjects@__ - The total number of objects stored in a
--     bucket. This value is calculated by counting all objects in the
--     bucket (including object versions) and the total number of parts for
--     all incomplete multipart uploads to the bucket.
--
--     Statistics: The most useful statistic is @Average@.
--
--     Unit: The published unit is @Count@.
--
-- 'startTime', 'getBucketMetricData_startTime' - The timestamp indicating the earliest data to be returned.
--
-- 'endTime', 'getBucketMetricData_endTime' - The timestamp indicating the latest data to be returned.
--
-- 'period', 'getBucketMetricData_period' - The granularity, in seconds, of the returned data points.
--
-- Bucket storage metrics are reported once per day. Therefore, you should
-- specify a period of 86400 seconds, which is the number of seconds in a
-- day.
--
-- 'statistics', 'getBucketMetricData_statistics' - The statistic for the metric.
--
-- The following statistics are available:
--
-- -   @Minimum@ - The lowest value observed during the specified period.
--     Use this value to determine low volumes of activity for your
--     application.
--
-- -   @Maximum@ - The highest value observed during the specified period.
--     Use this value to determine high volumes of activity for your
--     application.
--
-- -   @Sum@ - The sum of all values submitted for the matching metric. You
--     can use this statistic to determine the total volume of a metric.
--
-- -   @Average@ - The value of @Sum@ \/ @SampleCount@ during the specified
--     period. By comparing this statistic with the @Minimum@ and @Maximum@
--     values, you can determine the full scope of a metric and how close
--     the average use is to the @Minimum@ and @Maximum@ values. This
--     comparison helps you to know when to increase or decrease your
--     resources.
--
-- -   @SampleCount@ - The count, or number, of data points used for the
--     statistical calculation.
--
-- 'unit', 'getBucketMetricData_unit' - The unit for the metric data request.
--
-- Valid units depend on the metric data being requested. For the valid
-- units with each available metric, see the @metricName@ parameter.
newGetBucketMetricData ::
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'metricName'
  BucketMetricName ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'period'
  Prelude.Natural ->
  -- | 'unit'
  MetricUnit ->
  GetBucketMetricData
newGetBucketMetricData
  pBucketName_
  pMetricName_
  pStartTime_
  pEndTime_
  pPeriod_
  pUnit_ =
    GetBucketMetricData'
      { bucketName = pBucketName_,
        metricName = pMetricName_,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_,
        period = pPeriod_,
        statistics = Prelude.mempty,
        unit = pUnit_
      }

-- | The name of the bucket for which to get metric data.
getBucketMetricData_bucketName :: Lens.Lens' GetBucketMetricData Prelude.Text
getBucketMetricData_bucketName = Lens.lens (\GetBucketMetricData' {bucketName} -> bucketName) (\s@GetBucketMetricData' {} a -> s {bucketName = a} :: GetBucketMetricData)

-- | The metric for which you want to return information.
--
-- Valid bucket metric names are listed below, along with the most useful
-- statistics to include in your request, and the published unit value.
--
-- These bucket metrics are reported once per day.
--
-- -   __@BucketSizeBytes@__ - The amount of data in bytes stored in a
--     bucket. This value is calculated by summing the size of all objects
--     in the bucket (including object versions), including the size of all
--     parts for all incomplete multipart uploads to the bucket.
--
--     Statistics: The most useful statistic is @Maximum@.
--
--     Unit: The published unit is @Bytes@.
--
-- -   __@NumberOfObjects@__ - The total number of objects stored in a
--     bucket. This value is calculated by counting all objects in the
--     bucket (including object versions) and the total number of parts for
--     all incomplete multipart uploads to the bucket.
--
--     Statistics: The most useful statistic is @Average@.
--
--     Unit: The published unit is @Count@.
getBucketMetricData_metricName :: Lens.Lens' GetBucketMetricData BucketMetricName
getBucketMetricData_metricName = Lens.lens (\GetBucketMetricData' {metricName} -> metricName) (\s@GetBucketMetricData' {} a -> s {metricName = a} :: GetBucketMetricData)

-- | The timestamp indicating the earliest data to be returned.
getBucketMetricData_startTime :: Lens.Lens' GetBucketMetricData Prelude.UTCTime
getBucketMetricData_startTime = Lens.lens (\GetBucketMetricData' {startTime} -> startTime) (\s@GetBucketMetricData' {} a -> s {startTime = a} :: GetBucketMetricData) Prelude.. Data._Time

-- | The timestamp indicating the latest data to be returned.
getBucketMetricData_endTime :: Lens.Lens' GetBucketMetricData Prelude.UTCTime
getBucketMetricData_endTime = Lens.lens (\GetBucketMetricData' {endTime} -> endTime) (\s@GetBucketMetricData' {} a -> s {endTime = a} :: GetBucketMetricData) Prelude.. Data._Time

-- | The granularity, in seconds, of the returned data points.
--
-- Bucket storage metrics are reported once per day. Therefore, you should
-- specify a period of 86400 seconds, which is the number of seconds in a
-- day.
getBucketMetricData_period :: Lens.Lens' GetBucketMetricData Prelude.Natural
getBucketMetricData_period = Lens.lens (\GetBucketMetricData' {period} -> period) (\s@GetBucketMetricData' {} a -> s {period = a} :: GetBucketMetricData)

-- | The statistic for the metric.
--
-- The following statistics are available:
--
-- -   @Minimum@ - The lowest value observed during the specified period.
--     Use this value to determine low volumes of activity for your
--     application.
--
-- -   @Maximum@ - The highest value observed during the specified period.
--     Use this value to determine high volumes of activity for your
--     application.
--
-- -   @Sum@ - The sum of all values submitted for the matching metric. You
--     can use this statistic to determine the total volume of a metric.
--
-- -   @Average@ - The value of @Sum@ \/ @SampleCount@ during the specified
--     period. By comparing this statistic with the @Minimum@ and @Maximum@
--     values, you can determine the full scope of a metric and how close
--     the average use is to the @Minimum@ and @Maximum@ values. This
--     comparison helps you to know when to increase or decrease your
--     resources.
--
-- -   @SampleCount@ - The count, or number, of data points used for the
--     statistical calculation.
getBucketMetricData_statistics :: Lens.Lens' GetBucketMetricData [MetricStatistic]
getBucketMetricData_statistics = Lens.lens (\GetBucketMetricData' {statistics} -> statistics) (\s@GetBucketMetricData' {} a -> s {statistics = a} :: GetBucketMetricData) Prelude.. Lens.coerced

-- | The unit for the metric data request.
--
-- Valid units depend on the metric data being requested. For the valid
-- units with each available metric, see the @metricName@ parameter.
getBucketMetricData_unit :: Lens.Lens' GetBucketMetricData MetricUnit
getBucketMetricData_unit = Lens.lens (\GetBucketMetricData' {unit} -> unit) (\s@GetBucketMetricData' {} a -> s {unit = a} :: GetBucketMetricData)

instance Core.AWSRequest GetBucketMetricData where
  type
    AWSResponse GetBucketMetricData =
      GetBucketMetricDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBucketMetricDataResponse'
            Prelude.<$> (x Data..?> "metricData" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "metricName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketMetricData where
  hashWithSalt _salt GetBucketMetricData' {..} =
    _salt
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` statistics
      `Prelude.hashWithSalt` unit

instance Prelude.NFData GetBucketMetricData where
  rnf GetBucketMetricData' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf statistics
      `Prelude.seq` Prelude.rnf unit

instance Data.ToHeaders GetBucketMetricData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetBucketMetricData" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetBucketMetricData where
  toJSON GetBucketMetricData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("bucketName" Data..= bucketName),
            Prelude.Just ("metricName" Data..= metricName),
            Prelude.Just ("startTime" Data..= startTime),
            Prelude.Just ("endTime" Data..= endTime),
            Prelude.Just ("period" Data..= period),
            Prelude.Just ("statistics" Data..= statistics),
            Prelude.Just ("unit" Data..= unit)
          ]
      )

instance Data.ToPath GetBucketMetricData where
  toPath = Prelude.const "/"

instance Data.ToQuery GetBucketMetricData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBucketMetricDataResponse' smart constructor.
data GetBucketMetricDataResponse = GetBucketMetricDataResponse'
  { -- | An array of objects that describe the metric data returned.
    metricData :: Prelude.Maybe [MetricDatapoint],
    -- | The name of the metric returned.
    metricName :: Prelude.Maybe BucketMetricName,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketMetricDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricData', 'getBucketMetricDataResponse_metricData' - An array of objects that describe the metric data returned.
--
-- 'metricName', 'getBucketMetricDataResponse_metricName' - The name of the metric returned.
--
-- 'httpStatus', 'getBucketMetricDataResponse_httpStatus' - The response's http status code.
newGetBucketMetricDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketMetricDataResponse
newGetBucketMetricDataResponse pHttpStatus_ =
  GetBucketMetricDataResponse'
    { metricData =
        Prelude.Nothing,
      metricName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the metric data returned.
getBucketMetricDataResponse_metricData :: Lens.Lens' GetBucketMetricDataResponse (Prelude.Maybe [MetricDatapoint])
getBucketMetricDataResponse_metricData = Lens.lens (\GetBucketMetricDataResponse' {metricData} -> metricData) (\s@GetBucketMetricDataResponse' {} a -> s {metricData = a} :: GetBucketMetricDataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the metric returned.
getBucketMetricDataResponse_metricName :: Lens.Lens' GetBucketMetricDataResponse (Prelude.Maybe BucketMetricName)
getBucketMetricDataResponse_metricName = Lens.lens (\GetBucketMetricDataResponse' {metricName} -> metricName) (\s@GetBucketMetricDataResponse' {} a -> s {metricName = a} :: GetBucketMetricDataResponse)

-- | The response's http status code.
getBucketMetricDataResponse_httpStatus :: Lens.Lens' GetBucketMetricDataResponse Prelude.Int
getBucketMetricDataResponse_httpStatus = Lens.lens (\GetBucketMetricDataResponse' {httpStatus} -> httpStatus) (\s@GetBucketMetricDataResponse' {} a -> s {httpStatus = a} :: GetBucketMetricDataResponse)

instance Prelude.NFData GetBucketMetricDataResponse where
  rnf GetBucketMetricDataResponse' {..} =
    Prelude.rnf metricData
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf httpStatus
