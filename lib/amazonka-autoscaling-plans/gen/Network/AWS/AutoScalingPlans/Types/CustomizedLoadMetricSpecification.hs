{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.CustomizedLoadMetricSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.CustomizedLoadMetricSpecification where

import Network.AWS.AutoScalingPlans.Types.MetricDimension
import Network.AWS.AutoScalingPlans.Types.MetricStatistic
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a CloudWatch metric of your choosing that can be used for predictive scaling.
--
--
-- For predictive scaling to work with a customized load metric specification, AWS Auto Scaling needs access to the @Sum@ and @Average@ statistics that CloudWatch computes from metric data. Statistics are calculations used to aggregate data over specified time periods.
--
-- When you choose a load metric, make sure that the required @Sum@ and @Average@ statistics for your metric are available in CloudWatch and that they provide relevant data for predictive scaling. The @Sum@ statistic must represent the total load on the resource, and the @Average@ statistic must represent the average load per capacity unit of the resource. For example, there is a metric that counts the number of requests processed by your Auto Scaling group. If the @Sum@ statistic represents the total request count processed by the group, then the @Average@ statistic for the specified metric must represent the average request count processed by each instance of the group.
--
-- For information about terminology, available metrics, or how to publish new metrics, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch Concepts> in the /Amazon CloudWatch User Guide/ .
--
--
-- /See:/ 'customizedLoadMetricSpecification' smart constructor.
data CustomizedLoadMetricSpecification = CustomizedLoadMetricSpecification'
  { _clmsDimensions ::
      !( Maybe
           [MetricDimension]
       ),
    _clmsUnit ::
      !(Maybe Text),
    _clmsMetricName ::
      !Text,
    _clmsNamespace :: !Text,
    _clmsStatistic ::
      !MetricStatistic
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomizedLoadMetricSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clmsDimensions' - The dimensions of the metric. Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized load metric specification.
--
-- * 'clmsUnit' - The unit of the metric.
--
-- * 'clmsMetricName' - The name of the metric.
--
-- * 'clmsNamespace' - The namespace of the metric.
--
-- * 'clmsStatistic' - The statistic of the metric. Currently, the value must always be @Sum@ .
customizedLoadMetricSpecification ::
  -- | 'clmsMetricName'
  Text ->
  -- | 'clmsNamespace'
  Text ->
  -- | 'clmsStatistic'
  MetricStatistic ->
  CustomizedLoadMetricSpecification
customizedLoadMetricSpecification
  pMetricName_
  pNamespace_
  pStatistic_ =
    CustomizedLoadMetricSpecification'
      { _clmsDimensions = Nothing,
        _clmsUnit = Nothing,
        _clmsMetricName = pMetricName_,
        _clmsNamespace = pNamespace_,
        _clmsStatistic = pStatistic_
      }

-- | The dimensions of the metric. Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized load metric specification.
clmsDimensions :: Lens' CustomizedLoadMetricSpecification [MetricDimension]
clmsDimensions = lens _clmsDimensions (\s a -> s {_clmsDimensions = a}) . _Default . _Coerce

-- | The unit of the metric.
clmsUnit :: Lens' CustomizedLoadMetricSpecification (Maybe Text)
clmsUnit = lens _clmsUnit (\s a -> s {_clmsUnit = a})

-- | The name of the metric.
clmsMetricName :: Lens' CustomizedLoadMetricSpecification Text
clmsMetricName = lens _clmsMetricName (\s a -> s {_clmsMetricName = a})

-- | The namespace of the metric.
clmsNamespace :: Lens' CustomizedLoadMetricSpecification Text
clmsNamespace = lens _clmsNamespace (\s a -> s {_clmsNamespace = a})

-- | The statistic of the metric. Currently, the value must always be @Sum@ .
clmsStatistic :: Lens' CustomizedLoadMetricSpecification MetricStatistic
clmsStatistic = lens _clmsStatistic (\s a -> s {_clmsStatistic = a})

instance FromJSON CustomizedLoadMetricSpecification where
  parseJSON =
    withObject
      "CustomizedLoadMetricSpecification"
      ( \x ->
          CustomizedLoadMetricSpecification'
            <$> (x .:? "Dimensions" .!= mempty)
            <*> (x .:? "Unit")
            <*> (x .: "MetricName")
            <*> (x .: "Namespace")
            <*> (x .: "Statistic")
      )

instance Hashable CustomizedLoadMetricSpecification

instance NFData CustomizedLoadMetricSpecification

instance ToJSON CustomizedLoadMetricSpecification where
  toJSON CustomizedLoadMetricSpecification' {..} =
    object
      ( catMaybes
          [ ("Dimensions" .=) <$> _clmsDimensions,
            ("Unit" .=) <$> _clmsUnit,
            Just ("MetricName" .= _clmsMetricName),
            Just ("Namespace" .= _clmsNamespace),
            Just ("Statistic" .= _clmsStatistic)
          ]
      )
