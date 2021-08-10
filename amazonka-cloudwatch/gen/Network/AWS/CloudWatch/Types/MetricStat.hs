{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MetricStat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MetricStat where

import Network.AWS.CloudWatch.Types.Metric
import Network.AWS.CloudWatch.Types.StandardUnit
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This structure defines the metric to be returned, along with the
-- statistics, period, and units.
--
-- /See:/ 'newMetricStat' smart constructor.
data MetricStat = MetricStat'
  { -- | When you are using a @Put@ operation, this defines what unit you want to
    -- use when storing the metric.
    --
    -- In a @Get@ operation, if you omit @Unit@ then all data that was
    -- collected with any unit is returned, along with the corresponding units
    -- that were specified when the data was reported to CloudWatch. If you
    -- specify a unit, the operation returns only data that was collected with
    -- that unit specified. If you specify a unit that does not match the data
    -- collected, the results of the operation are null. CloudWatch does not
    -- perform unit conversions.
    unit :: Prelude.Maybe StandardUnit,
    -- | The metric to return, including the metric name, namespace, and
    -- dimensions.
    metric :: Metric,
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
    period :: Prelude.Natural,
    -- | The statistic to return. It can include any CloudWatch statistic or
    -- extended statistic.
    stat :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricStat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'metricStat_unit' - When you are using a @Put@ operation, this defines what unit you want to
-- use when storing the metric.
--
-- In a @Get@ operation, if you omit @Unit@ then all data that was
-- collected with any unit is returned, along with the corresponding units
-- that were specified when the data was reported to CloudWatch. If you
-- specify a unit, the operation returns only data that was collected with
-- that unit specified. If you specify a unit that does not match the data
-- collected, the results of the operation are null. CloudWatch does not
-- perform unit conversions.
--
-- 'metric', 'metricStat_metric' - The metric to return, including the metric name, namespace, and
-- dimensions.
--
-- 'period', 'metricStat_period' - The granularity, in seconds, of the returned data points. For metrics
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
--
-- 'stat', 'metricStat_stat' - The statistic to return. It can include any CloudWatch statistic or
-- extended statistic.
newMetricStat ::
  -- | 'metric'
  Metric ->
  -- | 'period'
  Prelude.Natural ->
  -- | 'stat'
  Prelude.Text ->
  MetricStat
newMetricStat pMetric_ pPeriod_ pStat_ =
  MetricStat'
    { unit = Prelude.Nothing,
      metric = pMetric_,
      period = pPeriod_,
      stat = pStat_
    }

-- | When you are using a @Put@ operation, this defines what unit you want to
-- use when storing the metric.
--
-- In a @Get@ operation, if you omit @Unit@ then all data that was
-- collected with any unit is returned, along with the corresponding units
-- that were specified when the data was reported to CloudWatch. If you
-- specify a unit, the operation returns only data that was collected with
-- that unit specified. If you specify a unit that does not match the data
-- collected, the results of the operation are null. CloudWatch does not
-- perform unit conversions.
metricStat_unit :: Lens.Lens' MetricStat (Prelude.Maybe StandardUnit)
metricStat_unit = Lens.lens (\MetricStat' {unit} -> unit) (\s@MetricStat' {} a -> s {unit = a} :: MetricStat)

-- | The metric to return, including the metric name, namespace, and
-- dimensions.
metricStat_metric :: Lens.Lens' MetricStat Metric
metricStat_metric = Lens.lens (\MetricStat' {metric} -> metric) (\s@MetricStat' {} a -> s {metric = a} :: MetricStat)

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
metricStat_period :: Lens.Lens' MetricStat Prelude.Natural
metricStat_period = Lens.lens (\MetricStat' {period} -> period) (\s@MetricStat' {} a -> s {period = a} :: MetricStat)

-- | The statistic to return. It can include any CloudWatch statistic or
-- extended statistic.
metricStat_stat :: Lens.Lens' MetricStat Prelude.Text
metricStat_stat = Lens.lens (\MetricStat' {stat} -> stat) (\s@MetricStat' {} a -> s {stat = a} :: MetricStat)

instance Core.FromXML MetricStat where
  parseXML x =
    MetricStat'
      Prelude.<$> (x Core..@? "Unit")
      Prelude.<*> (x Core..@ "Metric")
      Prelude.<*> (x Core..@ "Period")
      Prelude.<*> (x Core..@ "Stat")

instance Prelude.Hashable MetricStat

instance Prelude.NFData MetricStat

instance Core.ToQuery MetricStat where
  toQuery MetricStat' {..} =
    Prelude.mconcat
      [ "Unit" Core.=: unit,
        "Metric" Core.=: metric,
        "Period" Core.=: period,
        "Stat" Core.=: stat
      ]
