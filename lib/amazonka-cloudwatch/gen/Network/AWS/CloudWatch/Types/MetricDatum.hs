{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MetricDatum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MetricDatum where

import Network.AWS.CloudWatch.Types.Dimension
import Network.AWS.CloudWatch.Types.StandardUnit
import Network.AWS.CloudWatch.Types.StatisticSet
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Encapsulates the information sent to either create a metric or add new values to be aggregated into an existing metric.
--
--
--
-- /See:/ 'metricDatum' smart constructor.
data MetricDatum = MetricDatum'
  { _mdValues :: !(Maybe [Double]),
    _mdCounts :: !(Maybe [Double]),
    _mdValue :: !(Maybe Double),
    _mdStorageResolution :: !(Maybe Nat),
    _mdDimensions :: !(Maybe [Dimension]),
    _mdUnit :: !(Maybe StandardUnit),
    _mdTimestamp :: !(Maybe ISO8601),
    _mdStatisticValues :: !(Maybe StatisticSet),
    _mdMetricName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricDatum' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdValues' - Array of numbers representing the values for the metric during the period. Each unique value is listed just once in this array, and the corresponding number in the @Counts@ array specifies the number of times that value occurred during the period. You can include up to 150 unique values in each @PutMetricData@ action that specifies a @Values@ array. Although the @Values@ array accepts numbers of type @Double@ , CloudWatch rejects values that are either too small or too large. Values must be in the range of -2^360 to 2^360. In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
--
-- * 'mdCounts' - Array of numbers that is used along with the @Values@ array. Each number in the @Count@ array is the number of times the corresponding value in the @Values@ array occurred during the period.  If you omit the @Counts@ array, the default of 1 is used as the value for each count. If you include a @Counts@ array, it must include the same amount of values as the @Values@ array.
--
-- * 'mdValue' - The value for the metric. Although the parameter accepts numbers of type Double, CloudWatch rejects values that are either too small or too large. Values must be in the range of -2^360 to 2^360. In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
--
-- * 'mdStorageResolution' - Valid values are 1 and 60. Setting this to 1 specifies this metric as a high-resolution metric, so that CloudWatch stores the metric with sub-minute resolution down to one second. Setting this to 60 specifies this metric as a regular-resolution metric, which CloudWatch stores at 1-minute resolution. Currently, high resolution is available only for custom metrics. For more information about high-resolution metrics, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics> in the /Amazon CloudWatch User Guide/ .  This field is optional, if you do not specify it the default of 60 is used.
--
-- * 'mdDimensions' - The dimensions associated with the metric.
--
-- * 'mdUnit' - When you are using a @Put@ operation, this defines what unit you want to use when storing the metric. In a @Get@ operation, this displays the unit that is used for the metric.
--
-- * 'mdTimestamp' - The time the metric data was received, expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
--
-- * 'mdStatisticValues' - The statistical values for the metric.
--
-- * 'mdMetricName' - The name of the metric.
metricDatum ::
  -- | 'mdMetricName'
  Text ->
  MetricDatum
metricDatum pMetricName_ =
  MetricDatum'
    { _mdValues = Nothing,
      _mdCounts = Nothing,
      _mdValue = Nothing,
      _mdStorageResolution = Nothing,
      _mdDimensions = Nothing,
      _mdUnit = Nothing,
      _mdTimestamp = Nothing,
      _mdStatisticValues = Nothing,
      _mdMetricName = pMetricName_
    }

-- | Array of numbers representing the values for the metric during the period. Each unique value is listed just once in this array, and the corresponding number in the @Counts@ array specifies the number of times that value occurred during the period. You can include up to 150 unique values in each @PutMetricData@ action that specifies a @Values@ array. Although the @Values@ array accepts numbers of type @Double@ , CloudWatch rejects values that are either too small or too large. Values must be in the range of -2^360 to 2^360. In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
mdValues :: Lens' MetricDatum [Double]
mdValues = lens _mdValues (\s a -> s {_mdValues = a}) . _Default . _Coerce

-- | Array of numbers that is used along with the @Values@ array. Each number in the @Count@ array is the number of times the corresponding value in the @Values@ array occurred during the period.  If you omit the @Counts@ array, the default of 1 is used as the value for each count. If you include a @Counts@ array, it must include the same amount of values as the @Values@ array.
mdCounts :: Lens' MetricDatum [Double]
mdCounts = lens _mdCounts (\s a -> s {_mdCounts = a}) . _Default . _Coerce

-- | The value for the metric. Although the parameter accepts numbers of type Double, CloudWatch rejects values that are either too small or too large. Values must be in the range of -2^360 to 2^360. In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
mdValue :: Lens' MetricDatum (Maybe Double)
mdValue = lens _mdValue (\s a -> s {_mdValue = a})

-- | Valid values are 1 and 60. Setting this to 1 specifies this metric as a high-resolution metric, so that CloudWatch stores the metric with sub-minute resolution down to one second. Setting this to 60 specifies this metric as a regular-resolution metric, which CloudWatch stores at 1-minute resolution. Currently, high resolution is available only for custom metrics. For more information about high-resolution metrics, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics> in the /Amazon CloudWatch User Guide/ .  This field is optional, if you do not specify it the default of 60 is used.
mdStorageResolution :: Lens' MetricDatum (Maybe Natural)
mdStorageResolution = lens _mdStorageResolution (\s a -> s {_mdStorageResolution = a}) . mapping _Nat

-- | The dimensions associated with the metric.
mdDimensions :: Lens' MetricDatum [Dimension]
mdDimensions = lens _mdDimensions (\s a -> s {_mdDimensions = a}) . _Default . _Coerce

-- | When you are using a @Put@ operation, this defines what unit you want to use when storing the metric. In a @Get@ operation, this displays the unit that is used for the metric.
mdUnit :: Lens' MetricDatum (Maybe StandardUnit)
mdUnit = lens _mdUnit (\s a -> s {_mdUnit = a})

-- | The time the metric data was received, expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
mdTimestamp :: Lens' MetricDatum (Maybe UTCTime)
mdTimestamp = lens _mdTimestamp (\s a -> s {_mdTimestamp = a}) . mapping _Time

-- | The statistical values for the metric.
mdStatisticValues :: Lens' MetricDatum (Maybe StatisticSet)
mdStatisticValues = lens _mdStatisticValues (\s a -> s {_mdStatisticValues = a})

-- | The name of the metric.
mdMetricName :: Lens' MetricDatum Text
mdMetricName = lens _mdMetricName (\s a -> s {_mdMetricName = a})

instance Hashable MetricDatum

instance NFData MetricDatum

instance ToQuery MetricDatum where
  toQuery MetricDatum' {..} =
    mconcat
      [ "Values" =: toQuery (toQueryList "member" <$> _mdValues),
        "Counts" =: toQuery (toQueryList "member" <$> _mdCounts),
        "Value" =: _mdValue,
        "StorageResolution" =: _mdStorageResolution,
        "Dimensions" =: toQuery (toQueryList "member" <$> _mdDimensions),
        "Unit" =: _mdUnit,
        "Timestamp" =: _mdTimestamp,
        "StatisticValues" =: _mdStatisticValues,
        "MetricName" =: _mdMetricName
      ]
