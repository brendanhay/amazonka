{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.InsightRuleMetricDatapoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.InsightRuleMetricDatapoint where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | One data point from the metric time series returned in a Contributor Insights rule report.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetInsightRuleReport.html GetInsightRuleReport> .
--
--
-- /See:/ 'insightRuleMetricDatapoint' smart constructor.
data InsightRuleMetricDatapoint = InsightRuleMetricDatapoint'
  { _irmdMaxContributorValue ::
      !(Maybe Double),
    _irmdSampleCount :: !(Maybe Double),
    _irmdMaximum :: !(Maybe Double),
    _irmdAverage :: !(Maybe Double),
    _irmdMinimum :: !(Maybe Double),
    _irmdUniqueContributors ::
      !(Maybe Double),
    _irmdSum :: !(Maybe Double),
    _irmdTimestamp :: !ISO8601
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InsightRuleMetricDatapoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irmdMaxContributorValue' - The maximum value provided by one contributor during this timestamp. Each timestamp is evaluated separately, so the identity of the max contributor could be different for each timestamp. This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- * 'irmdSampleCount' - The number of occurrences that matched the rule during this data point. This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- * 'irmdMaximum' - The maximum value from a single occurence from a single contributor during the time period represented by that data point. This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- * 'irmdAverage' - The average value from all contributors during the time period represented by that data point. This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- * 'irmdMinimum' - The minimum value from a single contributor during the time period represented by that data point. This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- * 'irmdUniqueContributors' - The number of unique contributors who published data during this timestamp. This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- * 'irmdSum' - The sum of the values from all contributors during the time period represented by that data point. This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- * 'irmdTimestamp' - The timestamp of the data point.
insightRuleMetricDatapoint ::
  -- | 'irmdTimestamp'
  UTCTime ->
  InsightRuleMetricDatapoint
insightRuleMetricDatapoint pTimestamp_ =
  InsightRuleMetricDatapoint'
    { _irmdMaxContributorValue = Nothing,
      _irmdSampleCount = Nothing,
      _irmdMaximum = Nothing,
      _irmdAverage = Nothing,
      _irmdMinimum = Nothing,
      _irmdUniqueContributors = Nothing,
      _irmdSum = Nothing,
      _irmdTimestamp = _Time # pTimestamp_
    }

-- | The maximum value provided by one contributor during this timestamp. Each timestamp is evaluated separately, so the identity of the max contributor could be different for each timestamp. This statistic is returned only if you included it in the @Metrics@ array in your request.
irmdMaxContributorValue :: Lens' InsightRuleMetricDatapoint (Maybe Double)
irmdMaxContributorValue = lens _irmdMaxContributorValue (\s a -> s {_irmdMaxContributorValue = a})

-- | The number of occurrences that matched the rule during this data point. This statistic is returned only if you included it in the @Metrics@ array in your request.
irmdSampleCount :: Lens' InsightRuleMetricDatapoint (Maybe Double)
irmdSampleCount = lens _irmdSampleCount (\s a -> s {_irmdSampleCount = a})

-- | The maximum value from a single occurence from a single contributor during the time period represented by that data point. This statistic is returned only if you included it in the @Metrics@ array in your request.
irmdMaximum :: Lens' InsightRuleMetricDatapoint (Maybe Double)
irmdMaximum = lens _irmdMaximum (\s a -> s {_irmdMaximum = a})

-- | The average value from all contributors during the time period represented by that data point. This statistic is returned only if you included it in the @Metrics@ array in your request.
irmdAverage :: Lens' InsightRuleMetricDatapoint (Maybe Double)
irmdAverage = lens _irmdAverage (\s a -> s {_irmdAverage = a})

-- | The minimum value from a single contributor during the time period represented by that data point. This statistic is returned only if you included it in the @Metrics@ array in your request.
irmdMinimum :: Lens' InsightRuleMetricDatapoint (Maybe Double)
irmdMinimum = lens _irmdMinimum (\s a -> s {_irmdMinimum = a})

-- | The number of unique contributors who published data during this timestamp. This statistic is returned only if you included it in the @Metrics@ array in your request.
irmdUniqueContributors :: Lens' InsightRuleMetricDatapoint (Maybe Double)
irmdUniqueContributors = lens _irmdUniqueContributors (\s a -> s {_irmdUniqueContributors = a})

-- | The sum of the values from all contributors during the time period represented by that data point. This statistic is returned only if you included it in the @Metrics@ array in your request.
irmdSum :: Lens' InsightRuleMetricDatapoint (Maybe Double)
irmdSum = lens _irmdSum (\s a -> s {_irmdSum = a})

-- | The timestamp of the data point.
irmdTimestamp :: Lens' InsightRuleMetricDatapoint UTCTime
irmdTimestamp = lens _irmdTimestamp (\s a -> s {_irmdTimestamp = a}) . _Time

instance FromXML InsightRuleMetricDatapoint where
  parseXML x =
    InsightRuleMetricDatapoint'
      <$> (x .@? "MaxContributorValue")
      <*> (x .@? "SampleCount")
      <*> (x .@? "Maximum")
      <*> (x .@? "Average")
      <*> (x .@? "Minimum")
      <*> (x .@? "UniqueContributors")
      <*> (x .@? "Sum")
      <*> (x .@ "Timestamp")

instance Hashable InsightRuleMetricDatapoint

instance NFData InsightRuleMetricDatapoint
