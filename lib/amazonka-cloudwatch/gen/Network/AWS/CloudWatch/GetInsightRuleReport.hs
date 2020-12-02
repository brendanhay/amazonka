{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.GetInsightRuleReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the time series data collected by a Contributor Insights rule. The data includes the identity and number of contributors to the log group.
--
--
-- You can also optionally return one or more statistics about each data point in the time series. These statistics can include the following:
--
--     * @UniqueContributors@ -- the number of unique contributors for each data point.
--
--     * @MaxContributorValue@ -- the value of the top contributor for each data point. The identity of the contributor might change for each data point in the graph.
--
-- If this rule aggregates by COUNT, the top contributor for each data point is the contributor with the most occurrences in that period. If the rule aggregates by SUM, the top contributor is the contributor with the highest sum in the log field specified by the rule's @Value@ , during that period.
--
--     * @SampleCount@ -- the number of data points matched by the rule.
--
--     * @Sum@ -- the sum of the values from all contributors during the time period represented by that data point.
--
--     * @Minimum@ -- the minimum value from a single observation during the time period represented by that data point.
--
--     * @Maximum@ -- the maximum value from a single observation during the time period represented by that data point.
--
--     * @Average@ -- the average value from all contributors during the time period represented by that data point.
module Network.AWS.CloudWatch.GetInsightRuleReport
  ( -- * Creating a Request
    getInsightRuleReport,
    GetInsightRuleReport,

    -- * Request Lenses
    girrMaxContributorCount,
    girrMetrics,
    girrOrderBy,
    girrRuleName,
    girrStartTime,
    girrEndTime,
    girrPeriod,

    -- * Destructuring the Response
    getInsightRuleReportResponse,
    GetInsightRuleReportResponse,

    -- * Response Lenses
    girrrsKeyLabels,
    girrrsApproximateUniqueCount,
    girrrsAggregationStatistic,
    girrrsAggregateValue,
    girrrsContributors,
    girrrsMetricDatapoints,
    girrrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getInsightRuleReport' smart constructor.
data GetInsightRuleReport = GetInsightRuleReport'
  { _girrMaxContributorCount ::
      !(Maybe Int),
    _girrMetrics :: !(Maybe [Text]),
    _girrOrderBy :: !(Maybe Text),
    _girrRuleName :: !Text,
    _girrStartTime :: !ISO8601,
    _girrEndTime :: !ISO8601,
    _girrPeriod :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetInsightRuleReport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girrMaxContributorCount' - The maximum number of contributors to include in the report. The range is 1 to 100. If you omit this, the default of 10 is used.
--
-- * 'girrMetrics' - Specifies which metrics to use for aggregation of contributor values for the report. You can specify one or more of the following metrics:     * @UniqueContributors@ -- the number of unique contributors for each data point.     * @MaxContributorValue@ -- the value of the top contributor for each data point. The identity of the contributor might change for each data point in the graph. If this rule aggregates by COUNT, the top contributor for each data point is the contributor with the most occurrences in that period. If the rule aggregates by SUM, the top contributor is the contributor with the highest sum in the log field specified by the rule's @Value@ , during that period.     * @SampleCount@ -- the number of data points matched by the rule.     * @Sum@ -- the sum of the values from all contributors during the time period represented by that data point.     * @Minimum@ -- the minimum value from a single observation during the time period represented by that data point.     * @Maximum@ -- the maximum value from a single observation during the time period represented by that data point.     * @Average@ -- the average value from all contributors during the time period represented by that data point.
--
-- * 'girrOrderBy' - Determines what statistic to use to rank the contributors. Valid values are SUM and MAXIMUM.
--
-- * 'girrRuleName' - The name of the rule that you want to see data from.
--
-- * 'girrStartTime' - The start time of the data to use in the report. When used in a raw HTTP Query API, it is formatted as @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
--
-- * 'girrEndTime' - The end time of the data to use in the report. When used in a raw HTTP Query API, it is formatted as @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
--
-- * 'girrPeriod' - The period, in seconds, to use for the statistics in the @InsightRuleMetricDatapoint@ results.
getInsightRuleReport ::
  -- | 'girrRuleName'
  Text ->
  -- | 'girrStartTime'
  UTCTime ->
  -- | 'girrEndTime'
  UTCTime ->
  -- | 'girrPeriod'
  Natural ->
  GetInsightRuleReport
getInsightRuleReport pRuleName_ pStartTime_ pEndTime_ pPeriod_ =
  GetInsightRuleReport'
    { _girrMaxContributorCount = Nothing,
      _girrMetrics = Nothing,
      _girrOrderBy = Nothing,
      _girrRuleName = pRuleName_,
      _girrStartTime = _Time # pStartTime_,
      _girrEndTime = _Time # pEndTime_,
      _girrPeriod = _Nat # pPeriod_
    }

-- | The maximum number of contributors to include in the report. The range is 1 to 100. If you omit this, the default of 10 is used.
girrMaxContributorCount :: Lens' GetInsightRuleReport (Maybe Int)
girrMaxContributorCount = lens _girrMaxContributorCount (\s a -> s {_girrMaxContributorCount = a})

-- | Specifies which metrics to use for aggregation of contributor values for the report. You can specify one or more of the following metrics:     * @UniqueContributors@ -- the number of unique contributors for each data point.     * @MaxContributorValue@ -- the value of the top contributor for each data point. The identity of the contributor might change for each data point in the graph. If this rule aggregates by COUNT, the top contributor for each data point is the contributor with the most occurrences in that period. If the rule aggregates by SUM, the top contributor is the contributor with the highest sum in the log field specified by the rule's @Value@ , during that period.     * @SampleCount@ -- the number of data points matched by the rule.     * @Sum@ -- the sum of the values from all contributors during the time period represented by that data point.     * @Minimum@ -- the minimum value from a single observation during the time period represented by that data point.     * @Maximum@ -- the maximum value from a single observation during the time period represented by that data point.     * @Average@ -- the average value from all contributors during the time period represented by that data point.
girrMetrics :: Lens' GetInsightRuleReport [Text]
girrMetrics = lens _girrMetrics (\s a -> s {_girrMetrics = a}) . _Default . _Coerce

-- | Determines what statistic to use to rank the contributors. Valid values are SUM and MAXIMUM.
girrOrderBy :: Lens' GetInsightRuleReport (Maybe Text)
girrOrderBy = lens _girrOrderBy (\s a -> s {_girrOrderBy = a})

-- | The name of the rule that you want to see data from.
girrRuleName :: Lens' GetInsightRuleReport Text
girrRuleName = lens _girrRuleName (\s a -> s {_girrRuleName = a})

-- | The start time of the data to use in the report. When used in a raw HTTP Query API, it is formatted as @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
girrStartTime :: Lens' GetInsightRuleReport UTCTime
girrStartTime = lens _girrStartTime (\s a -> s {_girrStartTime = a}) . _Time

-- | The end time of the data to use in the report. When used in a raw HTTP Query API, it is formatted as @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
girrEndTime :: Lens' GetInsightRuleReport UTCTime
girrEndTime = lens _girrEndTime (\s a -> s {_girrEndTime = a}) . _Time

-- | The period, in seconds, to use for the statistics in the @InsightRuleMetricDatapoint@ results.
girrPeriod :: Lens' GetInsightRuleReport Natural
girrPeriod = lens _girrPeriod (\s a -> s {_girrPeriod = a}) . _Nat

instance AWSRequest GetInsightRuleReport where
  type Rs GetInsightRuleReport = GetInsightRuleReportResponse
  request = postQuery cloudWatch
  response =
    receiveXMLWrapper
      "GetInsightRuleReportResult"
      ( \s h x ->
          GetInsightRuleReportResponse'
            <$> (x .@? "KeyLabels" .!@ mempty >>= may (parseXMLList "member"))
            <*> (x .@? "ApproximateUniqueCount")
            <*> (x .@? "AggregationStatistic")
            <*> (x .@? "AggregateValue")
            <*> (x .@? "Contributors" .!@ mempty >>= may (parseXMLList "member"))
            <*> ( x .@? "MetricDatapoints" .!@ mempty
                    >>= may (parseXMLList "member")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable GetInsightRuleReport

instance NFData GetInsightRuleReport

instance ToHeaders GetInsightRuleReport where
  toHeaders = const mempty

instance ToPath GetInsightRuleReport where
  toPath = const "/"

instance ToQuery GetInsightRuleReport where
  toQuery GetInsightRuleReport' {..} =
    mconcat
      [ "Action" =: ("GetInsightRuleReport" :: ByteString),
        "Version" =: ("2010-08-01" :: ByteString),
        "MaxContributorCount" =: _girrMaxContributorCount,
        "Metrics" =: toQuery (toQueryList "member" <$> _girrMetrics),
        "OrderBy" =: _girrOrderBy,
        "RuleName" =: _girrRuleName,
        "StartTime" =: _girrStartTime,
        "EndTime" =: _girrEndTime,
        "Period" =: _girrPeriod
      ]

-- | /See:/ 'getInsightRuleReportResponse' smart constructor.
data GetInsightRuleReportResponse = GetInsightRuleReportResponse'
  { _girrrsKeyLabels ::
      !(Maybe [Text]),
    _girrrsApproximateUniqueCount ::
      !(Maybe Integer),
    _girrrsAggregationStatistic ::
      !(Maybe Text),
    _girrrsAggregateValue ::
      !(Maybe Double),
    _girrrsContributors ::
      !(Maybe [InsightRuleContributor]),
    _girrrsMetricDatapoints ::
      !( Maybe
           [InsightRuleMetricDatapoint]
       ),
    _girrrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetInsightRuleReportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girrrsKeyLabels' - An array of the strings used as the keys for this rule. The keys are the dimensions used to classify contributors. If the rule contains more than one key, then each unique combination of values for the keys is counted as a unique contributor.
--
-- * 'girrrsApproximateUniqueCount' - An approximate count of the unique contributors found by this rule in this time period.
--
-- * 'girrrsAggregationStatistic' - Specifies whether this rule aggregates contributor data by COUNT or SUM.
--
-- * 'girrrsAggregateValue' - The sum of the values from all individual contributors that match the rule.
--
-- * 'girrrsContributors' - An array of the unique contributors found by this rule in this time period. If the rule contains multiple keys, each combination of values for the keys counts as a unique contributor.
--
-- * 'girrrsMetricDatapoints' - A time series of metric data points that matches the time period in the rule request.
--
-- * 'girrrsResponseStatus' - -- | The response status code.
getInsightRuleReportResponse ::
  -- | 'girrrsResponseStatus'
  Int ->
  GetInsightRuleReportResponse
getInsightRuleReportResponse pResponseStatus_ =
  GetInsightRuleReportResponse'
    { _girrrsKeyLabels = Nothing,
      _girrrsApproximateUniqueCount = Nothing,
      _girrrsAggregationStatistic = Nothing,
      _girrrsAggregateValue = Nothing,
      _girrrsContributors = Nothing,
      _girrrsMetricDatapoints = Nothing,
      _girrrsResponseStatus = pResponseStatus_
    }

-- | An array of the strings used as the keys for this rule. The keys are the dimensions used to classify contributors. If the rule contains more than one key, then each unique combination of values for the keys is counted as a unique contributor.
girrrsKeyLabels :: Lens' GetInsightRuleReportResponse [Text]
girrrsKeyLabels = lens _girrrsKeyLabels (\s a -> s {_girrrsKeyLabels = a}) . _Default . _Coerce

-- | An approximate count of the unique contributors found by this rule in this time period.
girrrsApproximateUniqueCount :: Lens' GetInsightRuleReportResponse (Maybe Integer)
girrrsApproximateUniqueCount = lens _girrrsApproximateUniqueCount (\s a -> s {_girrrsApproximateUniqueCount = a})

-- | Specifies whether this rule aggregates contributor data by COUNT or SUM.
girrrsAggregationStatistic :: Lens' GetInsightRuleReportResponse (Maybe Text)
girrrsAggregationStatistic = lens _girrrsAggregationStatistic (\s a -> s {_girrrsAggregationStatistic = a})

-- | The sum of the values from all individual contributors that match the rule.
girrrsAggregateValue :: Lens' GetInsightRuleReportResponse (Maybe Double)
girrrsAggregateValue = lens _girrrsAggregateValue (\s a -> s {_girrrsAggregateValue = a})

-- | An array of the unique contributors found by this rule in this time period. If the rule contains multiple keys, each combination of values for the keys counts as a unique contributor.
girrrsContributors :: Lens' GetInsightRuleReportResponse [InsightRuleContributor]
girrrsContributors = lens _girrrsContributors (\s a -> s {_girrrsContributors = a}) . _Default . _Coerce

-- | A time series of metric data points that matches the time period in the rule request.
girrrsMetricDatapoints :: Lens' GetInsightRuleReportResponse [InsightRuleMetricDatapoint]
girrrsMetricDatapoints = lens _girrrsMetricDatapoints (\s a -> s {_girrrsMetricDatapoints = a}) . _Default . _Coerce

-- | -- | The response status code.
girrrsResponseStatus :: Lens' GetInsightRuleReportResponse Int
girrrsResponseStatus = lens _girrrsResponseStatus (\s a -> s {_girrrsResponseStatus = a})

instance NFData GetInsightRuleReportResponse
