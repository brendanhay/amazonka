{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.CustomizedMetricSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.CustomizedMetricSpecification where

import Network.AWS.AutoScaling.Types.MetricDimension
import Network.AWS.AutoScaling.Types.MetricStatistic
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a CloudWatch metric of your choosing for a target tracking scaling policy to use with Amazon EC2 Auto Scaling.
--
--
-- To create your customized metric specification:
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
-- /See:/ 'customizedMetricSpecification' smart constructor.
data CustomizedMetricSpecification = CustomizedMetricSpecification'
  { _cmsDimensions ::
      !(Maybe [MetricDimension]),
    _cmsUnit :: !(Maybe Text),
    _cmsMetricName :: !Text,
    _cmsNamespace :: !Text,
    _cmsStatistic ::
      !MetricStatistic
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomizedMetricSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmsDimensions' - The dimensions of the metric. Conditional: If you published your metric with dimensions, you must specify the same dimensions in your scaling policy.
--
-- * 'cmsUnit' - The unit of the metric.
--
-- * 'cmsMetricName' - The name of the metric.
--
-- * 'cmsNamespace' - The namespace of the metric.
--
-- * 'cmsStatistic' - The statistic of the metric.
customizedMetricSpecification ::
  -- | 'cmsMetricName'
  Text ->
  -- | 'cmsNamespace'
  Text ->
  -- | 'cmsStatistic'
  MetricStatistic ->
  CustomizedMetricSpecification
customizedMetricSpecification pMetricName_ pNamespace_ pStatistic_ =
  CustomizedMetricSpecification'
    { _cmsDimensions = Nothing,
      _cmsUnit = Nothing,
      _cmsMetricName = pMetricName_,
      _cmsNamespace = pNamespace_,
      _cmsStatistic = pStatistic_
    }

-- | The dimensions of the metric. Conditional: If you published your metric with dimensions, you must specify the same dimensions in your scaling policy.
cmsDimensions :: Lens' CustomizedMetricSpecification [MetricDimension]
cmsDimensions = lens _cmsDimensions (\s a -> s {_cmsDimensions = a}) . _Default . _Coerce

-- | The unit of the metric.
cmsUnit :: Lens' CustomizedMetricSpecification (Maybe Text)
cmsUnit = lens _cmsUnit (\s a -> s {_cmsUnit = a})

-- | The name of the metric.
cmsMetricName :: Lens' CustomizedMetricSpecification Text
cmsMetricName = lens _cmsMetricName (\s a -> s {_cmsMetricName = a})

-- | The namespace of the metric.
cmsNamespace :: Lens' CustomizedMetricSpecification Text
cmsNamespace = lens _cmsNamespace (\s a -> s {_cmsNamespace = a})

-- | The statistic of the metric.
cmsStatistic :: Lens' CustomizedMetricSpecification MetricStatistic
cmsStatistic = lens _cmsStatistic (\s a -> s {_cmsStatistic = a})

instance FromXML CustomizedMetricSpecification where
  parseXML x =
    CustomizedMetricSpecification'
      <$> (x .@? "Dimensions" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Unit")
      <*> (x .@ "MetricName")
      <*> (x .@ "Namespace")
      <*> (x .@ "Statistic")

instance Hashable CustomizedMetricSpecification

instance NFData CustomizedMetricSpecification

instance ToQuery CustomizedMetricSpecification where
  toQuery CustomizedMetricSpecification' {..} =
    mconcat
      [ "Dimensions" =: toQuery (toQueryList "member" <$> _cmsDimensions),
        "Unit" =: _cmsUnit,
        "MetricName" =: _cmsMetricName,
        "Namespace" =: _cmsNamespace,
        "Statistic" =: _cmsStatistic
      ]
