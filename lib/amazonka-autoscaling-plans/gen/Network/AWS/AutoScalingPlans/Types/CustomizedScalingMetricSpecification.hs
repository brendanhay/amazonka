{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.CustomizedScalingMetricSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.CustomizedScalingMetricSpecification where

import Network.AWS.AutoScalingPlans.Types.MetricDimension
import Network.AWS.AutoScalingPlans.Types.MetricStatistic
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a CloudWatch metric of your choosing that can be used for dynamic scaling as part of a target tracking scaling policy.
--
--
-- To create your customized scaling metric specification:
--
--     * Add values for each required parameter from CloudWatch. You can use an existing metric, or a new metric that you create. To use your own metric, you must first publish the metric to CloudWatch. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publish Custom Metrics> in the /Amazon CloudWatch User Guide/ .
--
--     * Choose a metric that changes proportionally with capacity. The value of the metric should increase or decrease in inverse proportion to the number of capacity units. That is, the value of the metric should decrease when capacity increases.
--
--
--
-- For more information about CloudWatch, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch Concepts> .
--
--
-- /See:/ 'customizedScalingMetricSpecification' smart constructor.
data CustomizedScalingMetricSpecification = CustomizedScalingMetricSpecification'
  { _csmsDimensions ::
      !( Maybe
           [MetricDimension]
       ),
    _csmsUnit ::
      !(Maybe Text),
    _csmsMetricName ::
      !Text,
    _csmsNamespace ::
      !Text,
    _csmsStatistic ::
      !MetricStatistic
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomizedScalingMetricSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csmsDimensions' - The dimensions of the metric. Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized scaling metric specification.
--
-- * 'csmsUnit' - The unit of the metric.
--
-- * 'csmsMetricName' - The name of the metric.
--
-- * 'csmsNamespace' - The namespace of the metric.
--
-- * 'csmsStatistic' - The statistic of the metric.
customizedScalingMetricSpecification ::
  -- | 'csmsMetricName'
  Text ->
  -- | 'csmsNamespace'
  Text ->
  -- | 'csmsStatistic'
  MetricStatistic ->
  CustomizedScalingMetricSpecification
customizedScalingMetricSpecification
  pMetricName_
  pNamespace_
  pStatistic_ =
    CustomizedScalingMetricSpecification'
      { _csmsDimensions = Nothing,
        _csmsUnit = Nothing,
        _csmsMetricName = pMetricName_,
        _csmsNamespace = pNamespace_,
        _csmsStatistic = pStatistic_
      }

-- | The dimensions of the metric. Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized scaling metric specification.
csmsDimensions :: Lens' CustomizedScalingMetricSpecification [MetricDimension]
csmsDimensions = lens _csmsDimensions (\s a -> s {_csmsDimensions = a}) . _Default . _Coerce

-- | The unit of the metric.
csmsUnit :: Lens' CustomizedScalingMetricSpecification (Maybe Text)
csmsUnit = lens _csmsUnit (\s a -> s {_csmsUnit = a})

-- | The name of the metric.
csmsMetricName :: Lens' CustomizedScalingMetricSpecification Text
csmsMetricName = lens _csmsMetricName (\s a -> s {_csmsMetricName = a})

-- | The namespace of the metric.
csmsNamespace :: Lens' CustomizedScalingMetricSpecification Text
csmsNamespace = lens _csmsNamespace (\s a -> s {_csmsNamespace = a})

-- | The statistic of the metric.
csmsStatistic :: Lens' CustomizedScalingMetricSpecification MetricStatistic
csmsStatistic = lens _csmsStatistic (\s a -> s {_csmsStatistic = a})

instance FromJSON CustomizedScalingMetricSpecification where
  parseJSON =
    withObject
      "CustomizedScalingMetricSpecification"
      ( \x ->
          CustomizedScalingMetricSpecification'
            <$> (x .:? "Dimensions" .!= mempty)
            <*> (x .:? "Unit")
            <*> (x .: "MetricName")
            <*> (x .: "Namespace")
            <*> (x .: "Statistic")
      )

instance Hashable CustomizedScalingMetricSpecification

instance NFData CustomizedScalingMetricSpecification

instance ToJSON CustomizedScalingMetricSpecification where
  toJSON CustomizedScalingMetricSpecification' {..} =
    object
      ( catMaybes
          [ ("Dimensions" .=) <$> _csmsDimensions,
            ("Unit" .=) <$> _csmsUnit,
            Just ("MetricName" .= _csmsMetricName),
            Just ("Namespace" .= _csmsNamespace),
            Just ("Statistic" .= _csmsStatistic)
          ]
      )
