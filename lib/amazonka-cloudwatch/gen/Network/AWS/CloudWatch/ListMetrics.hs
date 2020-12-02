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
-- Module      : Network.AWS.CloudWatch.ListMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the specified metrics. You can use the returned metrics with <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricData.html GetMetricData> or <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html GetMetricStatistics> to obtain statistical data.
--
--
-- Up to 500 results are returned for any one call. To retrieve additional results, use the returned token with subsequent calls.
--
-- After you create a metric, allow up to 15 minutes before the metric appears. You can see statistics about the metric sooner by using <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricData.html GetMetricData> or <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html GetMetricStatistics> .
--
-- @ListMetrics@ doesn't return information about metrics if those metrics haven't reported data in the past two weeks. To retrieve those metrics, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricData.html GetMetricData> or <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html GetMetricStatistics> .
--
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.ListMetrics
  ( -- * Creating a Request
    listMetrics,
    ListMetrics,

    -- * Request Lenses
    lmMetricName,
    lmNamespace,
    lmNextToken,
    lmRecentlyActive,
    lmDimensions,

    -- * Destructuring the Response
    listMetricsResponse,
    ListMetricsResponse,

    -- * Response Lenses
    lmrsMetrics,
    lmrsNextToken,
    lmrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listMetrics' smart constructor.
data ListMetrics = ListMetrics'
  { _lmMetricName :: !(Maybe Text),
    _lmNamespace :: !(Maybe Text),
    _lmNextToken :: !(Maybe Text),
    _lmRecentlyActive :: !(Maybe RecentlyActive),
    _lmDimensions :: !(Maybe [DimensionFilter])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmMetricName' - The name of the metric to filter against. Only the metrics with names that match exactly will be returned.
--
-- * 'lmNamespace' - The metric namespace to filter against. Only the namespace that matches exactly will be returned.
--
-- * 'lmNextToken' - The token returned by a previous call to indicate that there is more data available.
--
-- * 'lmRecentlyActive' - To filter the results to show only metrics that have had data points published in the past three hours, specify this parameter with a value of @PT3H@ . This is the only valid value for this parameter. The results that are returned are an approximation of the value you specify. There is a low probability that the returned results include metrics with last published data as much as 40 minutes more than the specified time interval.
--
-- * 'lmDimensions' - The dimensions to filter against. Only the dimensions that match exactly will be returned.
listMetrics ::
  ListMetrics
listMetrics =
  ListMetrics'
    { _lmMetricName = Nothing,
      _lmNamespace = Nothing,
      _lmNextToken = Nothing,
      _lmRecentlyActive = Nothing,
      _lmDimensions = Nothing
    }

-- | The name of the metric to filter against. Only the metrics with names that match exactly will be returned.
lmMetricName :: Lens' ListMetrics (Maybe Text)
lmMetricName = lens _lmMetricName (\s a -> s {_lmMetricName = a})

-- | The metric namespace to filter against. Only the namespace that matches exactly will be returned.
lmNamespace :: Lens' ListMetrics (Maybe Text)
lmNamespace = lens _lmNamespace (\s a -> s {_lmNamespace = a})

-- | The token returned by a previous call to indicate that there is more data available.
lmNextToken :: Lens' ListMetrics (Maybe Text)
lmNextToken = lens _lmNextToken (\s a -> s {_lmNextToken = a})

-- | To filter the results to show only metrics that have had data points published in the past three hours, specify this parameter with a value of @PT3H@ . This is the only valid value for this parameter. The results that are returned are an approximation of the value you specify. There is a low probability that the returned results include metrics with last published data as much as 40 minutes more than the specified time interval.
lmRecentlyActive :: Lens' ListMetrics (Maybe RecentlyActive)
lmRecentlyActive = lens _lmRecentlyActive (\s a -> s {_lmRecentlyActive = a})

-- | The dimensions to filter against. Only the dimensions that match exactly will be returned.
lmDimensions :: Lens' ListMetrics [DimensionFilter]
lmDimensions = lens _lmDimensions (\s a -> s {_lmDimensions = a}) . _Default . _Coerce

instance AWSPager ListMetrics where
  page rq rs
    | stop (rs ^. lmrsNextToken) = Nothing
    | stop (rs ^. lmrsMetrics) = Nothing
    | otherwise = Just $ rq & lmNextToken .~ rs ^. lmrsNextToken

instance AWSRequest ListMetrics where
  type Rs ListMetrics = ListMetricsResponse
  request = postQuery cloudWatch
  response =
    receiveXMLWrapper
      "ListMetricsResult"
      ( \s h x ->
          ListMetricsResponse'
            <$> (x .@? "Metrics" .!@ mempty >>= may (parseXMLList "member"))
            <*> (x .@? "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListMetrics

instance NFData ListMetrics

instance ToHeaders ListMetrics where
  toHeaders = const mempty

instance ToPath ListMetrics where
  toPath = const "/"

instance ToQuery ListMetrics where
  toQuery ListMetrics' {..} =
    mconcat
      [ "Action" =: ("ListMetrics" :: ByteString),
        "Version" =: ("2010-08-01" :: ByteString),
        "MetricName" =: _lmMetricName,
        "Namespace" =: _lmNamespace,
        "NextToken" =: _lmNextToken,
        "RecentlyActive" =: _lmRecentlyActive,
        "Dimensions" =: toQuery (toQueryList "member" <$> _lmDimensions)
      ]

-- | /See:/ 'listMetricsResponse' smart constructor.
data ListMetricsResponse = ListMetricsResponse'
  { _lmrsMetrics ::
      !(Maybe [Metric]),
    _lmrsNextToken :: !(Maybe Text),
    _lmrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMetricsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmrsMetrics' - The metrics that match your request.
--
-- * 'lmrsNextToken' - The token that marks the start of the next batch of returned results.
--
-- * 'lmrsResponseStatus' - -- | The response status code.
listMetricsResponse ::
  -- | 'lmrsResponseStatus'
  Int ->
  ListMetricsResponse
listMetricsResponse pResponseStatus_ =
  ListMetricsResponse'
    { _lmrsMetrics = Nothing,
      _lmrsNextToken = Nothing,
      _lmrsResponseStatus = pResponseStatus_
    }

-- | The metrics that match your request.
lmrsMetrics :: Lens' ListMetricsResponse [Metric]
lmrsMetrics = lens _lmrsMetrics (\s a -> s {_lmrsMetrics = a}) . _Default . _Coerce

-- | The token that marks the start of the next batch of returned results.
lmrsNextToken :: Lens' ListMetricsResponse (Maybe Text)
lmrsNextToken = lens _lmrsNextToken (\s a -> s {_lmrsNextToken = a})

-- | -- | The response status code.
lmrsResponseStatus :: Lens' ListMetricsResponse Int
lmrsResponseStatus = lens _lmrsResponseStatus (\s a -> s {_lmrsResponseStatus = a})

instance NFData ListMetricsResponse
