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
-- Module      : Amazonka.Lightsail.GetContainerServiceMetricData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data points of a specific metric of your Amazon Lightsail
-- container service.
--
-- Metrics report the utilization of your resources. Monitor and collect
-- metric data regularly to maintain the reliability, availability, and
-- performance of your resources.
module Amazonka.Lightsail.GetContainerServiceMetricData
  ( -- * Creating a Request
    GetContainerServiceMetricData (..),
    newGetContainerServiceMetricData,

    -- * Request Lenses
    getContainerServiceMetricData_serviceName,
    getContainerServiceMetricData_metricName,
    getContainerServiceMetricData_startTime,
    getContainerServiceMetricData_endTime,
    getContainerServiceMetricData_period,
    getContainerServiceMetricData_statistics,

    -- * Destructuring the Response
    GetContainerServiceMetricDataResponse (..),
    newGetContainerServiceMetricDataResponse,

    -- * Response Lenses
    getContainerServiceMetricDataResponse_metricData,
    getContainerServiceMetricDataResponse_metricName,
    getContainerServiceMetricDataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetContainerServiceMetricData' smart constructor.
data GetContainerServiceMetricData = GetContainerServiceMetricData'
  { -- | The name of the container service for which to get metric data.
    serviceName :: Prelude.Text,
    -- | The metric for which you want to return information.
    --
    -- Valid container service metric names are listed below, along with the
    -- most useful statistics to include in your request, and the published
    -- unit value.
    --
    -- -   @CPUUtilization@ - The average percentage of compute units that are
    --     currently in use across all nodes of the container service. This
    --     metric identifies the processing power required to run containers on
    --     each node of the container service.
    --
    --     Statistics: The most useful statistics are @Maximum@ and @Average@.
    --
    --     Unit: The published unit is @Percent@.
    --
    -- -   @MemoryUtilization@ - The average percentage of available memory
    --     that is currently in use across all nodes of the container service.
    --     This metric identifies the memory required to run containers on each
    --     node of the container service.
    --
    --     Statistics: The most useful statistics are @Maximum@ and @Average@.
    --
    --     Unit: The published unit is @Percent@.
    metricName :: ContainerServiceMetricName,
    -- | The start time of the time period.
    startTime :: Data.POSIX,
    -- | The end time of the time period.
    endTime :: Data.POSIX,
    -- | The granularity, in seconds, of the returned data points.
    --
    -- All container service metric data is available in 5-minute (300 seconds)
    -- granularity.
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
    -- -   @Sum@ - All values submitted for the matching metric added together.
    --     You can use this statistic to determine the total volume of a
    --     metric.
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
    statistics :: [MetricStatistic]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContainerServiceMetricData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceName', 'getContainerServiceMetricData_serviceName' - The name of the container service for which to get metric data.
--
-- 'metricName', 'getContainerServiceMetricData_metricName' - The metric for which you want to return information.
--
-- Valid container service metric names are listed below, along with the
-- most useful statistics to include in your request, and the published
-- unit value.
--
-- -   @CPUUtilization@ - The average percentage of compute units that are
--     currently in use across all nodes of the container service. This
--     metric identifies the processing power required to run containers on
--     each node of the container service.
--
--     Statistics: The most useful statistics are @Maximum@ and @Average@.
--
--     Unit: The published unit is @Percent@.
--
-- -   @MemoryUtilization@ - The average percentage of available memory
--     that is currently in use across all nodes of the container service.
--     This metric identifies the memory required to run containers on each
--     node of the container service.
--
--     Statistics: The most useful statistics are @Maximum@ and @Average@.
--
--     Unit: The published unit is @Percent@.
--
-- 'startTime', 'getContainerServiceMetricData_startTime' - The start time of the time period.
--
-- 'endTime', 'getContainerServiceMetricData_endTime' - The end time of the time period.
--
-- 'period', 'getContainerServiceMetricData_period' - The granularity, in seconds, of the returned data points.
--
-- All container service metric data is available in 5-minute (300 seconds)
-- granularity.
--
-- 'statistics', 'getContainerServiceMetricData_statistics' - The statistic for the metric.
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
-- -   @Sum@ - All values submitted for the matching metric added together.
--     You can use this statistic to determine the total volume of a
--     metric.
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
newGetContainerServiceMetricData ::
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'metricName'
  ContainerServiceMetricName ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'period'
  Prelude.Natural ->
  GetContainerServiceMetricData
newGetContainerServiceMetricData
  pServiceName_
  pMetricName_
  pStartTime_
  pEndTime_
  pPeriod_ =
    GetContainerServiceMetricData'
      { serviceName =
          pServiceName_,
        metricName = pMetricName_,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_,
        period = pPeriod_,
        statistics = Prelude.mempty
      }

-- | The name of the container service for which to get metric data.
getContainerServiceMetricData_serviceName :: Lens.Lens' GetContainerServiceMetricData Prelude.Text
getContainerServiceMetricData_serviceName = Lens.lens (\GetContainerServiceMetricData' {serviceName} -> serviceName) (\s@GetContainerServiceMetricData' {} a -> s {serviceName = a} :: GetContainerServiceMetricData)

-- | The metric for which you want to return information.
--
-- Valid container service metric names are listed below, along with the
-- most useful statistics to include in your request, and the published
-- unit value.
--
-- -   @CPUUtilization@ - The average percentage of compute units that are
--     currently in use across all nodes of the container service. This
--     metric identifies the processing power required to run containers on
--     each node of the container service.
--
--     Statistics: The most useful statistics are @Maximum@ and @Average@.
--
--     Unit: The published unit is @Percent@.
--
-- -   @MemoryUtilization@ - The average percentage of available memory
--     that is currently in use across all nodes of the container service.
--     This metric identifies the memory required to run containers on each
--     node of the container service.
--
--     Statistics: The most useful statistics are @Maximum@ and @Average@.
--
--     Unit: The published unit is @Percent@.
getContainerServiceMetricData_metricName :: Lens.Lens' GetContainerServiceMetricData ContainerServiceMetricName
getContainerServiceMetricData_metricName = Lens.lens (\GetContainerServiceMetricData' {metricName} -> metricName) (\s@GetContainerServiceMetricData' {} a -> s {metricName = a} :: GetContainerServiceMetricData)

-- | The start time of the time period.
getContainerServiceMetricData_startTime :: Lens.Lens' GetContainerServiceMetricData Prelude.UTCTime
getContainerServiceMetricData_startTime = Lens.lens (\GetContainerServiceMetricData' {startTime} -> startTime) (\s@GetContainerServiceMetricData' {} a -> s {startTime = a} :: GetContainerServiceMetricData) Prelude.. Data._Time

-- | The end time of the time period.
getContainerServiceMetricData_endTime :: Lens.Lens' GetContainerServiceMetricData Prelude.UTCTime
getContainerServiceMetricData_endTime = Lens.lens (\GetContainerServiceMetricData' {endTime} -> endTime) (\s@GetContainerServiceMetricData' {} a -> s {endTime = a} :: GetContainerServiceMetricData) Prelude.. Data._Time

-- | The granularity, in seconds, of the returned data points.
--
-- All container service metric data is available in 5-minute (300 seconds)
-- granularity.
getContainerServiceMetricData_period :: Lens.Lens' GetContainerServiceMetricData Prelude.Natural
getContainerServiceMetricData_period = Lens.lens (\GetContainerServiceMetricData' {period} -> period) (\s@GetContainerServiceMetricData' {} a -> s {period = a} :: GetContainerServiceMetricData)

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
-- -   @Sum@ - All values submitted for the matching metric added together.
--     You can use this statistic to determine the total volume of a
--     metric.
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
getContainerServiceMetricData_statistics :: Lens.Lens' GetContainerServiceMetricData [MetricStatistic]
getContainerServiceMetricData_statistics = Lens.lens (\GetContainerServiceMetricData' {statistics} -> statistics) (\s@GetContainerServiceMetricData' {} a -> s {statistics = a} :: GetContainerServiceMetricData) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    GetContainerServiceMetricData
  where
  type
    AWSResponse GetContainerServiceMetricData =
      GetContainerServiceMetricDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerServiceMetricDataResponse'
            Prelude.<$> (x Data..?> "metricData" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "metricName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetContainerServiceMetricData
  where
  hashWithSalt _salt GetContainerServiceMetricData' {..} =
    _salt
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` statistics

instance Prelude.NFData GetContainerServiceMetricData where
  rnf GetContainerServiceMetricData' {..} =
    Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf statistics

instance Data.ToHeaders GetContainerServiceMetricData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetContainerServiceMetricData" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetContainerServiceMetricData where
  toJSON GetContainerServiceMetricData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("serviceName" Data..= serviceName),
            Prelude.Just ("metricName" Data..= metricName),
            Prelude.Just ("startTime" Data..= startTime),
            Prelude.Just ("endTime" Data..= endTime),
            Prelude.Just ("period" Data..= period),
            Prelude.Just ("statistics" Data..= statistics)
          ]
      )

instance Data.ToPath GetContainerServiceMetricData where
  toPath = Prelude.const "/"

instance Data.ToQuery GetContainerServiceMetricData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContainerServiceMetricDataResponse' smart constructor.
data GetContainerServiceMetricDataResponse = GetContainerServiceMetricDataResponse'
  { -- | An array of objects that describe the metric data returned.
    metricData :: Prelude.Maybe [MetricDatapoint],
    -- | The name of the metric returned.
    metricName :: Prelude.Maybe ContainerServiceMetricName,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContainerServiceMetricDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricData', 'getContainerServiceMetricDataResponse_metricData' - An array of objects that describe the metric data returned.
--
-- 'metricName', 'getContainerServiceMetricDataResponse_metricName' - The name of the metric returned.
--
-- 'httpStatus', 'getContainerServiceMetricDataResponse_httpStatus' - The response's http status code.
newGetContainerServiceMetricDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContainerServiceMetricDataResponse
newGetContainerServiceMetricDataResponse pHttpStatus_ =
  GetContainerServiceMetricDataResponse'
    { metricData =
        Prelude.Nothing,
      metricName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the metric data returned.
getContainerServiceMetricDataResponse_metricData :: Lens.Lens' GetContainerServiceMetricDataResponse (Prelude.Maybe [MetricDatapoint])
getContainerServiceMetricDataResponse_metricData = Lens.lens (\GetContainerServiceMetricDataResponse' {metricData} -> metricData) (\s@GetContainerServiceMetricDataResponse' {} a -> s {metricData = a} :: GetContainerServiceMetricDataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the metric returned.
getContainerServiceMetricDataResponse_metricName :: Lens.Lens' GetContainerServiceMetricDataResponse (Prelude.Maybe ContainerServiceMetricName)
getContainerServiceMetricDataResponse_metricName = Lens.lens (\GetContainerServiceMetricDataResponse' {metricName} -> metricName) (\s@GetContainerServiceMetricDataResponse' {} a -> s {metricName = a} :: GetContainerServiceMetricDataResponse)

-- | The response's http status code.
getContainerServiceMetricDataResponse_httpStatus :: Lens.Lens' GetContainerServiceMetricDataResponse Prelude.Int
getContainerServiceMetricDataResponse_httpStatus = Lens.lens (\GetContainerServiceMetricDataResponse' {httpStatus} -> httpStatus) (\s@GetContainerServiceMetricDataResponse' {} a -> s {httpStatus = a} :: GetContainerServiceMetricDataResponse)

instance
  Prelude.NFData
    GetContainerServiceMetricDataResponse
  where
  rnf GetContainerServiceMetricDataResponse' {..} =
    Prelude.rnf metricData
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf httpStatus
