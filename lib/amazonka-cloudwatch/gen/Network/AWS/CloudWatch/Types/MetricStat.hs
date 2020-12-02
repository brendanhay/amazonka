{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MetricStat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MetricStat where

import Network.AWS.CloudWatch.Types.Metric
import Network.AWS.CloudWatch.Types.StandardUnit
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This structure defines the metric to be returned, along with the statistics, period, and units.
--
--
--
-- /See:/ 'metricStat' smart constructor.
data MetricStat = MetricStat'
  { _msUnit :: !(Maybe StandardUnit),
    _msMetric :: !Metric,
    _msPeriod :: !Nat,
    _msStat :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricStat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msUnit' - When you are using a @Put@ operation, this defines what unit you want to use when storing the metric. In a @Get@ operation, if you omit @Unit@ then all data that was collected with any unit is returned, along with the corresponding units that were specified when the data was reported to CloudWatch. If you specify a unit, the operation returns only data that was collected with that unit specified. If you specify a unit that does not match the data collected, the results of the operation are null. CloudWatch does not perform unit conversions.
--
-- * 'msMetric' - The metric to return, including the metric name, namespace, and dimensions.
--
-- * 'msPeriod' - The granularity, in seconds, of the returned data points. For metrics with regular resolution, a period can be as short as one minute (60 seconds) and must be a multiple of 60. For high-resolution metrics that are collected at intervals of less than one minute, the period can be 1, 5, 10, 30, 60, or any multiple of 60. High-resolution metrics are those metrics stored by a @PutMetricData@ call that includes a @StorageResolution@ of 1 second. If the @StartTime@ parameter specifies a time stamp that is greater than 3 hours ago, you must specify the period as follows or no data points in that time range is returned:     * Start time between 3 hours and 15 days ago - Use a multiple of 60 seconds (1 minute).     * Start time between 15 and 63 days ago - Use a multiple of 300 seconds (5 minutes).     * Start time greater than 63 days ago - Use a multiple of 3600 seconds (1 hour).
--
-- * 'msStat' - The statistic to return. It can include any CloudWatch statistic or extended statistic.
metricStat ::
  -- | 'msMetric'
  Metric ->
  -- | 'msPeriod'
  Natural ->
  -- | 'msStat'
  Text ->
  MetricStat
metricStat pMetric_ pPeriod_ pStat_ =
  MetricStat'
    { _msUnit = Nothing,
      _msMetric = pMetric_,
      _msPeriod = _Nat # pPeriod_,
      _msStat = pStat_
    }

-- | When you are using a @Put@ operation, this defines what unit you want to use when storing the metric. In a @Get@ operation, if you omit @Unit@ then all data that was collected with any unit is returned, along with the corresponding units that were specified when the data was reported to CloudWatch. If you specify a unit, the operation returns only data that was collected with that unit specified. If you specify a unit that does not match the data collected, the results of the operation are null. CloudWatch does not perform unit conversions.
msUnit :: Lens' MetricStat (Maybe StandardUnit)
msUnit = lens _msUnit (\s a -> s {_msUnit = a})

-- | The metric to return, including the metric name, namespace, and dimensions.
msMetric :: Lens' MetricStat Metric
msMetric = lens _msMetric (\s a -> s {_msMetric = a})

-- | The granularity, in seconds, of the returned data points. For metrics with regular resolution, a period can be as short as one minute (60 seconds) and must be a multiple of 60. For high-resolution metrics that are collected at intervals of less than one minute, the period can be 1, 5, 10, 30, 60, or any multiple of 60. High-resolution metrics are those metrics stored by a @PutMetricData@ call that includes a @StorageResolution@ of 1 second. If the @StartTime@ parameter specifies a time stamp that is greater than 3 hours ago, you must specify the period as follows or no data points in that time range is returned:     * Start time between 3 hours and 15 days ago - Use a multiple of 60 seconds (1 minute).     * Start time between 15 and 63 days ago - Use a multiple of 300 seconds (5 minutes).     * Start time greater than 63 days ago - Use a multiple of 3600 seconds (1 hour).
msPeriod :: Lens' MetricStat Natural
msPeriod = lens _msPeriod (\s a -> s {_msPeriod = a}) . _Nat

-- | The statistic to return. It can include any CloudWatch statistic or extended statistic.
msStat :: Lens' MetricStat Text
msStat = lens _msStat (\s a -> s {_msStat = a})

instance FromXML MetricStat where
  parseXML x =
    MetricStat'
      <$> (x .@? "Unit")
      <*> (x .@ "Metric")
      <*> (x .@ "Period")
      <*> (x .@ "Stat")

instance Hashable MetricStat

instance NFData MetricStat

instance ToQuery MetricStat where
  toQuery MetricStat' {..} =
    mconcat
      [ "Unit" =: _msUnit,
        "Metric" =: _msMetric,
        "Period" =: _msPeriod,
        "Stat" =: _msStat
      ]
