{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.CloudWatchAlarmDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.CloudWatchAlarmDefinition where

import Network.AWS.EMR.Types.ComparisonOperator
import Network.AWS.EMR.Types.MetricDimension
import Network.AWS.EMR.Types.Statistic
import Network.AWS.EMR.Types.Unit
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The definition of a CloudWatch metric alarm, which determines when an automatic scaling activity is triggered. When the defined alarm conditions are satisfied, scaling activity begins.
--
--
--
-- /See:/ 'cloudWatchAlarmDefinition' smart constructor.
data CloudWatchAlarmDefinition = CloudWatchAlarmDefinition'
  { _cwadEvaluationPeriods ::
      !(Maybe Int),
    _cwadNamespace :: !(Maybe Text),
    _cwadDimensions ::
      !(Maybe [MetricDimension]),
    _cwadUnit :: !(Maybe Unit),
    _cwadStatistic :: !(Maybe Statistic),
    _cwadComparisonOperator ::
      !ComparisonOperator,
    _cwadMetricName :: !Text,
    _cwadPeriod :: !Int,
    _cwadThreshold :: !Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudWatchAlarmDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwadEvaluationPeriods' - The number of periods, in five-minute increments, during which the alarm condition must exist before the alarm triggers automatic scaling activity. The default value is @1@ .
--
-- * 'cwadNamespace' - The namespace for the CloudWatch metric. The default is @AWS/ElasticMapReduce@ .
--
-- * 'cwadDimensions' - A CloudWatch metric dimension.
--
-- * 'cwadUnit' - The unit of measure associated with the CloudWatch metric being watched. The value specified for @Unit@ must correspond to the units specified in the CloudWatch metric.
--
-- * 'cwadStatistic' - The statistic to apply to the metric associated with the alarm. The default is @AVERAGE@ .
--
-- * 'cwadComparisonOperator' - Determines how the metric specified by @MetricName@ is compared to the value specified by @Threshold@ .
--
-- * 'cwadMetricName' - The name of the CloudWatch metric that is watched to determine an alarm condition.
--
-- * 'cwadPeriod' - The period, in seconds, over which the statistic is applied. EMR CloudWatch metrics are emitted every five minutes (300 seconds), so if an EMR CloudWatch metric is specified, specify @300@ .
--
-- * 'cwadThreshold' - The value against which the specified statistic is compared.
cloudWatchAlarmDefinition ::
  -- | 'cwadComparisonOperator'
  ComparisonOperator ->
  -- | 'cwadMetricName'
  Text ->
  -- | 'cwadPeriod'
  Int ->
  -- | 'cwadThreshold'
  Double ->
  CloudWatchAlarmDefinition
cloudWatchAlarmDefinition
  pComparisonOperator_
  pMetricName_
  pPeriod_
  pThreshold_ =
    CloudWatchAlarmDefinition'
      { _cwadEvaluationPeriods = Nothing,
        _cwadNamespace = Nothing,
        _cwadDimensions = Nothing,
        _cwadUnit = Nothing,
        _cwadStatistic = Nothing,
        _cwadComparisonOperator = pComparisonOperator_,
        _cwadMetricName = pMetricName_,
        _cwadPeriod = pPeriod_,
        _cwadThreshold = pThreshold_
      }

-- | The number of periods, in five-minute increments, during which the alarm condition must exist before the alarm triggers automatic scaling activity. The default value is @1@ .
cwadEvaluationPeriods :: Lens' CloudWatchAlarmDefinition (Maybe Int)
cwadEvaluationPeriods = lens _cwadEvaluationPeriods (\s a -> s {_cwadEvaluationPeriods = a})

-- | The namespace for the CloudWatch metric. The default is @AWS/ElasticMapReduce@ .
cwadNamespace :: Lens' CloudWatchAlarmDefinition (Maybe Text)
cwadNamespace = lens _cwadNamespace (\s a -> s {_cwadNamespace = a})

-- | A CloudWatch metric dimension.
cwadDimensions :: Lens' CloudWatchAlarmDefinition [MetricDimension]
cwadDimensions = lens _cwadDimensions (\s a -> s {_cwadDimensions = a}) . _Default . _Coerce

-- | The unit of measure associated with the CloudWatch metric being watched. The value specified for @Unit@ must correspond to the units specified in the CloudWatch metric.
cwadUnit :: Lens' CloudWatchAlarmDefinition (Maybe Unit)
cwadUnit = lens _cwadUnit (\s a -> s {_cwadUnit = a})

-- | The statistic to apply to the metric associated with the alarm. The default is @AVERAGE@ .
cwadStatistic :: Lens' CloudWatchAlarmDefinition (Maybe Statistic)
cwadStatistic = lens _cwadStatistic (\s a -> s {_cwadStatistic = a})

-- | Determines how the metric specified by @MetricName@ is compared to the value specified by @Threshold@ .
cwadComparisonOperator :: Lens' CloudWatchAlarmDefinition ComparisonOperator
cwadComparisonOperator = lens _cwadComparisonOperator (\s a -> s {_cwadComparisonOperator = a})

-- | The name of the CloudWatch metric that is watched to determine an alarm condition.
cwadMetricName :: Lens' CloudWatchAlarmDefinition Text
cwadMetricName = lens _cwadMetricName (\s a -> s {_cwadMetricName = a})

-- | The period, in seconds, over which the statistic is applied. EMR CloudWatch metrics are emitted every five minutes (300 seconds), so if an EMR CloudWatch metric is specified, specify @300@ .
cwadPeriod :: Lens' CloudWatchAlarmDefinition Int
cwadPeriod = lens _cwadPeriod (\s a -> s {_cwadPeriod = a})

-- | The value against which the specified statistic is compared.
cwadThreshold :: Lens' CloudWatchAlarmDefinition Double
cwadThreshold = lens _cwadThreshold (\s a -> s {_cwadThreshold = a})

instance FromJSON CloudWatchAlarmDefinition where
  parseJSON =
    withObject
      "CloudWatchAlarmDefinition"
      ( \x ->
          CloudWatchAlarmDefinition'
            <$> (x .:? "EvaluationPeriods")
            <*> (x .:? "Namespace")
            <*> (x .:? "Dimensions" .!= mempty)
            <*> (x .:? "Unit")
            <*> (x .:? "Statistic")
            <*> (x .: "ComparisonOperator")
            <*> (x .: "MetricName")
            <*> (x .: "Period")
            <*> (x .: "Threshold")
      )

instance Hashable CloudWatchAlarmDefinition

instance NFData CloudWatchAlarmDefinition

instance ToJSON CloudWatchAlarmDefinition where
  toJSON CloudWatchAlarmDefinition' {..} =
    object
      ( catMaybes
          [ ("EvaluationPeriods" .=) <$> _cwadEvaluationPeriods,
            ("Namespace" .=) <$> _cwadNamespace,
            ("Dimensions" .=) <$> _cwadDimensions,
            ("Unit" .=) <$> _cwadUnit,
            ("Statistic" .=) <$> _cwadStatistic,
            Just ("ComparisonOperator" .= _cwadComparisonOperator),
            Just ("MetricName" .= _cwadMetricName),
            Just ("Period" .= _cwadPeriod),
            Just ("Threshold" .= _cwadThreshold)
          ]
      )
