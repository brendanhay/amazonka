{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.CloudWatchAlarmConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.CloudWatchAlarmConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.ComparisonOperator
import Network.AWS.Route53.Types.Dimension
import Network.AWS.Route53.Types.Statistic

-- | A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.
--
--
--
-- /See:/ 'cloudWatchAlarmConfiguration' smart constructor.
data CloudWatchAlarmConfiguration = CloudWatchAlarmConfiguration'
  { _cwacDimensions ::
      !(Maybe [Dimension]),
    _cwacEvaluationPeriods :: !Nat,
    _cwacThreshold :: !Double,
    _cwacComparisonOperator ::
      !ComparisonOperator,
    _cwacPeriod :: !Nat,
    _cwacMetricName :: !Text,
    _cwacNamespace :: !Text,
    _cwacStatistic :: !Statistic
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudWatchAlarmConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwacDimensions' - For the metric that the CloudWatch alarm is associated with, a complex type that contains information about the dimensions for the metric. For information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
--
-- * 'cwacEvaluationPeriods' - For the metric that the CloudWatch alarm is associated with, the number of periods that the metric is compared to the threshold.
--
-- * 'cwacThreshold' - For the metric that the CloudWatch alarm is associated with, the value the metric is compared with.
--
-- * 'cwacComparisonOperator' - For the metric that the CloudWatch alarm is associated with, the arithmetic operation that is used for the comparison.
--
-- * 'cwacPeriod' - For the metric that the CloudWatch alarm is associated with, the duration of one evaluation period in seconds.
--
-- * 'cwacMetricName' - The name of the CloudWatch metric that the alarm is associated with.
--
-- * 'cwacNamespace' - The namespace of the metric that the alarm is associated with. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
--
-- * 'cwacStatistic' - For the metric that the CloudWatch alarm is associated with, the statistic that is applied to the metric.
cloudWatchAlarmConfiguration ::
  -- | 'cwacEvaluationPeriods'
  Natural ->
  -- | 'cwacThreshold'
  Double ->
  -- | 'cwacComparisonOperator'
  ComparisonOperator ->
  -- | 'cwacPeriod'
  Natural ->
  -- | 'cwacMetricName'
  Text ->
  -- | 'cwacNamespace'
  Text ->
  -- | 'cwacStatistic'
  Statistic ->
  CloudWatchAlarmConfiguration
cloudWatchAlarmConfiguration
  pEvaluationPeriods_
  pThreshold_
  pComparisonOperator_
  pPeriod_
  pMetricName_
  pNamespace_
  pStatistic_ =
    CloudWatchAlarmConfiguration'
      { _cwacDimensions = Nothing,
        _cwacEvaluationPeriods = _Nat # pEvaluationPeriods_,
        _cwacThreshold = pThreshold_,
        _cwacComparisonOperator = pComparisonOperator_,
        _cwacPeriod = _Nat # pPeriod_,
        _cwacMetricName = pMetricName_,
        _cwacNamespace = pNamespace_,
        _cwacStatistic = pStatistic_
      }

-- | For the metric that the CloudWatch alarm is associated with, a complex type that contains information about the dimensions for the metric. For information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
cwacDimensions :: Lens' CloudWatchAlarmConfiguration [Dimension]
cwacDimensions = lens _cwacDimensions (\s a -> s {_cwacDimensions = a}) . _Default . _Coerce

-- | For the metric that the CloudWatch alarm is associated with, the number of periods that the metric is compared to the threshold.
cwacEvaluationPeriods :: Lens' CloudWatchAlarmConfiguration Natural
cwacEvaluationPeriods = lens _cwacEvaluationPeriods (\s a -> s {_cwacEvaluationPeriods = a}) . _Nat

-- | For the metric that the CloudWatch alarm is associated with, the value the metric is compared with.
cwacThreshold :: Lens' CloudWatchAlarmConfiguration Double
cwacThreshold = lens _cwacThreshold (\s a -> s {_cwacThreshold = a})

-- | For the metric that the CloudWatch alarm is associated with, the arithmetic operation that is used for the comparison.
cwacComparisonOperator :: Lens' CloudWatchAlarmConfiguration ComparisonOperator
cwacComparisonOperator = lens _cwacComparisonOperator (\s a -> s {_cwacComparisonOperator = a})

-- | For the metric that the CloudWatch alarm is associated with, the duration of one evaluation period in seconds.
cwacPeriod :: Lens' CloudWatchAlarmConfiguration Natural
cwacPeriod = lens _cwacPeriod (\s a -> s {_cwacPeriod = a}) . _Nat

-- | The name of the CloudWatch metric that the alarm is associated with.
cwacMetricName :: Lens' CloudWatchAlarmConfiguration Text
cwacMetricName = lens _cwacMetricName (\s a -> s {_cwacMetricName = a})

-- | The namespace of the metric that the alarm is associated with. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
cwacNamespace :: Lens' CloudWatchAlarmConfiguration Text
cwacNamespace = lens _cwacNamespace (\s a -> s {_cwacNamespace = a})

-- | For the metric that the CloudWatch alarm is associated with, the statistic that is applied to the metric.
cwacStatistic :: Lens' CloudWatchAlarmConfiguration Statistic
cwacStatistic = lens _cwacStatistic (\s a -> s {_cwacStatistic = a})

instance FromXML CloudWatchAlarmConfiguration where
  parseXML x =
    CloudWatchAlarmConfiguration'
      <$> (x .@? "Dimensions" .!@ mempty >>= may (parseXMLList "Dimension"))
      <*> (x .@ "EvaluationPeriods")
      <*> (x .@ "Threshold")
      <*> (x .@ "ComparisonOperator")
      <*> (x .@ "Period")
      <*> (x .@ "MetricName")
      <*> (x .@ "Namespace")
      <*> (x .@ "Statistic")

instance Hashable CloudWatchAlarmConfiguration

instance NFData CloudWatchAlarmConfiguration
