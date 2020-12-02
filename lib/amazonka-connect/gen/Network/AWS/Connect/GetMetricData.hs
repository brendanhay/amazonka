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
-- Module      : Network.AWS.Connect.GetMetricData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets historical metric data from the specified Amazon Connect instance.
--
--
-- For a description of each historical metric, see <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions> in the /Amazon Connect Administrator Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.GetMetricData
  ( -- * Creating a Request
    getMetricData,
    GetMetricData,

    -- * Request Lenses
    gmdNextToken,
    gmdGroupings,
    gmdMaxResults,
    gmdInstanceId,
    gmdStartTime,
    gmdEndTime,
    gmdFilters,
    gmdHistoricalMetrics,

    -- * Destructuring the Response
    getMetricDataResponse,
    GetMetricDataResponse,

    -- * Response Lenses
    gmdrsMetricResults,
    gmdrsNextToken,
    gmdrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMetricData' smart constructor.
data GetMetricData = GetMetricData'
  { _gmdNextToken :: !(Maybe Text),
    _gmdGroupings :: !(Maybe [Grouping]),
    _gmdMaxResults :: !(Maybe Nat),
    _gmdInstanceId :: !Text,
    _gmdStartTime :: !POSIX,
    _gmdEndTime :: !POSIX,
    _gmdFilters :: !Filters,
    _gmdHistoricalMetrics :: ![HistoricalMetric]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMetricData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmdNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'gmdGroupings' - The grouping applied to the metrics returned. For example, when results are grouped by queue, the metrics returned are grouped by queue. The values returned apply to the metrics for each queue rather than aggregated for all queues. The only supported grouping is @QUEUE@ . If no grouping is specified, a summary of metrics for all queues is returned.
--
-- * 'gmdMaxResults' - The maximimum number of results to return per page.
--
-- * 'gmdInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'gmdStartTime' - The timestamp, in UNIX Epoch time format, at which to start the reporting interval for the retrieval of historical metrics data. The time must be specified using a multiple of 5 minutes, such as 10:05, 10:10, 10:15. The start time cannot be earlier than 24 hours before the time of the request. Historical metrics are available only for 24 hours.
--
-- * 'gmdEndTime' - The timestamp, in UNIX Epoch time format, at which to end the reporting interval for the retrieval of historical metrics data. The time must be specified using an interval of 5 minutes, such as 11:00, 11:05, 11:10, and must be later than the start time timestamp. The time range between the start and end time must be less than 24 hours.
--
-- * 'gmdFilters' - The queues, up to 100, or channels, to use to filter the metrics returned. Metric data is retrieved only for the resources associated with the queues or channels included in the filter. You can include both queue IDs and queue ARNs in the same request. Both @VOICE@ and @CHAT@ channels are supported.
--
-- * 'gmdHistoricalMetrics' - The metrics to retrieve. Specify the name, unit, and statistic for each metric. The following historical metrics are available. For a description of each metric, see <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions> in the /Amazon Connect Administrator Guide/ .     * ABANDON_TIME    * Unit: SECONDS Statistic: AVG     * AFTER_CONTACT_WORK_TIME    * Unit: SECONDS Statistic: AVG     * API_CONTACTS_HANDLED    * Unit: COUNT Statistic: SUM     * CALLBACK_CONTACTS_HANDLED    * Unit: COUNT Statistic: SUM     * CONTACTS_ABANDONED    * Unit: COUNT Statistic: SUM     * CONTACTS_AGENT_HUNG_UP_FIRST    * Unit: COUNT Statistic: SUM     * CONTACTS_CONSULTED    * Unit: COUNT Statistic: SUM     * CONTACTS_HANDLED    * Unit: COUNT Statistic: SUM     * CONTACTS_HANDLED_INCOMING    * Unit: COUNT Statistic: SUM     * CONTACTS_HANDLED_OUTBOUND    * Unit: COUNT Statistic: SUM     * CONTACTS_HOLD_ABANDONS    * Unit: COUNT Statistic: SUM     * CONTACTS_MISSED    * Unit: COUNT Statistic: SUM     * CONTACTS_QUEUED    * Unit: COUNT Statistic: SUM     * CONTACTS_TRANSFERRED_IN    * Unit: COUNT Statistic: SUM     * CONTACTS_TRANSFERRED_IN_FROM_QUEUE    * Unit: COUNT Statistic: SUM     * CONTACTS_TRANSFERRED_OUT    * Unit: COUNT Statistic: SUM     * CONTACTS_TRANSFERRED_OUT_FROM_QUEUE    * Unit: COUNT Statistic: SUM     * HANDLE_TIME    * Unit: SECONDS Statistic: AVG     * HOLD_TIME    * Unit: SECONDS Statistic: AVG     * INTERACTION_AND_HOLD_TIME    * Unit: SECONDS Statistic: AVG     * INTERACTION_TIME    * Unit: SECONDS Statistic: AVG     * OCCUPANCY    * Unit: PERCENT Statistic: AVG     * QUEUE_ANSWER_TIME    * Unit: SECONDS Statistic: AVG     * QUEUED_TIME    * Unit: SECONDS Statistic: MAX     * SERVICE_LEVEL    * Unit: PERCENT Statistic: AVG Threshold: Only "Less than" comparisons are supported, with the following service level thresholds: 15, 20, 25, 30, 45, 60, 90, 120, 180, 240, 300, 600
getMetricData ::
  -- | 'gmdInstanceId'
  Text ->
  -- | 'gmdStartTime'
  UTCTime ->
  -- | 'gmdEndTime'
  UTCTime ->
  -- | 'gmdFilters'
  Filters ->
  GetMetricData
getMetricData pInstanceId_ pStartTime_ pEndTime_ pFilters_ =
  GetMetricData'
    { _gmdNextToken = Nothing,
      _gmdGroupings = Nothing,
      _gmdMaxResults = Nothing,
      _gmdInstanceId = pInstanceId_,
      _gmdStartTime = _Time # pStartTime_,
      _gmdEndTime = _Time # pEndTime_,
      _gmdFilters = pFilters_,
      _gmdHistoricalMetrics = mempty
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
gmdNextToken :: Lens' GetMetricData (Maybe Text)
gmdNextToken = lens _gmdNextToken (\s a -> s {_gmdNextToken = a})

-- | The grouping applied to the metrics returned. For example, when results are grouped by queue, the metrics returned are grouped by queue. The values returned apply to the metrics for each queue rather than aggregated for all queues. The only supported grouping is @QUEUE@ . If no grouping is specified, a summary of metrics for all queues is returned.
gmdGroupings :: Lens' GetMetricData [Grouping]
gmdGroupings = lens _gmdGroupings (\s a -> s {_gmdGroupings = a}) . _Default . _Coerce

-- | The maximimum number of results to return per page.
gmdMaxResults :: Lens' GetMetricData (Maybe Natural)
gmdMaxResults = lens _gmdMaxResults (\s a -> s {_gmdMaxResults = a}) . mapping _Nat

-- | The identifier of the Amazon Connect instance.
gmdInstanceId :: Lens' GetMetricData Text
gmdInstanceId = lens _gmdInstanceId (\s a -> s {_gmdInstanceId = a})

-- | The timestamp, in UNIX Epoch time format, at which to start the reporting interval for the retrieval of historical metrics data. The time must be specified using a multiple of 5 minutes, such as 10:05, 10:10, 10:15. The start time cannot be earlier than 24 hours before the time of the request. Historical metrics are available only for 24 hours.
gmdStartTime :: Lens' GetMetricData UTCTime
gmdStartTime = lens _gmdStartTime (\s a -> s {_gmdStartTime = a}) . _Time

-- | The timestamp, in UNIX Epoch time format, at which to end the reporting interval for the retrieval of historical metrics data. The time must be specified using an interval of 5 minutes, such as 11:00, 11:05, 11:10, and must be later than the start time timestamp. The time range between the start and end time must be less than 24 hours.
gmdEndTime :: Lens' GetMetricData UTCTime
gmdEndTime = lens _gmdEndTime (\s a -> s {_gmdEndTime = a}) . _Time

-- | The queues, up to 100, or channels, to use to filter the metrics returned. Metric data is retrieved only for the resources associated with the queues or channels included in the filter. You can include both queue IDs and queue ARNs in the same request. Both @VOICE@ and @CHAT@ channels are supported.
gmdFilters :: Lens' GetMetricData Filters
gmdFilters = lens _gmdFilters (\s a -> s {_gmdFilters = a})

-- | The metrics to retrieve. Specify the name, unit, and statistic for each metric. The following historical metrics are available. For a description of each metric, see <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions> in the /Amazon Connect Administrator Guide/ .     * ABANDON_TIME    * Unit: SECONDS Statistic: AVG     * AFTER_CONTACT_WORK_TIME    * Unit: SECONDS Statistic: AVG     * API_CONTACTS_HANDLED    * Unit: COUNT Statistic: SUM     * CALLBACK_CONTACTS_HANDLED    * Unit: COUNT Statistic: SUM     * CONTACTS_ABANDONED    * Unit: COUNT Statistic: SUM     * CONTACTS_AGENT_HUNG_UP_FIRST    * Unit: COUNT Statistic: SUM     * CONTACTS_CONSULTED    * Unit: COUNT Statistic: SUM     * CONTACTS_HANDLED    * Unit: COUNT Statistic: SUM     * CONTACTS_HANDLED_INCOMING    * Unit: COUNT Statistic: SUM     * CONTACTS_HANDLED_OUTBOUND    * Unit: COUNT Statistic: SUM     * CONTACTS_HOLD_ABANDONS    * Unit: COUNT Statistic: SUM     * CONTACTS_MISSED    * Unit: COUNT Statistic: SUM     * CONTACTS_QUEUED    * Unit: COUNT Statistic: SUM     * CONTACTS_TRANSFERRED_IN    * Unit: COUNT Statistic: SUM     * CONTACTS_TRANSFERRED_IN_FROM_QUEUE    * Unit: COUNT Statistic: SUM     * CONTACTS_TRANSFERRED_OUT    * Unit: COUNT Statistic: SUM     * CONTACTS_TRANSFERRED_OUT_FROM_QUEUE    * Unit: COUNT Statistic: SUM     * HANDLE_TIME    * Unit: SECONDS Statistic: AVG     * HOLD_TIME    * Unit: SECONDS Statistic: AVG     * INTERACTION_AND_HOLD_TIME    * Unit: SECONDS Statistic: AVG     * INTERACTION_TIME    * Unit: SECONDS Statistic: AVG     * OCCUPANCY    * Unit: PERCENT Statistic: AVG     * QUEUE_ANSWER_TIME    * Unit: SECONDS Statistic: AVG     * QUEUED_TIME    * Unit: SECONDS Statistic: MAX     * SERVICE_LEVEL    * Unit: PERCENT Statistic: AVG Threshold: Only "Less than" comparisons are supported, with the following service level thresholds: 15, 20, 25, 30, 45, 60, 90, 120, 180, 240, 300, 600
gmdHistoricalMetrics :: Lens' GetMetricData [HistoricalMetric]
gmdHistoricalMetrics = lens _gmdHistoricalMetrics (\s a -> s {_gmdHistoricalMetrics = a}) . _Coerce

instance AWSPager GetMetricData where
  page rq rs
    | stop (rs ^. gmdrsNextToken) = Nothing
    | stop (rs ^. gmdrsMetricResults) = Nothing
    | otherwise = Just $ rq & gmdNextToken .~ rs ^. gmdrsNextToken

instance AWSRequest GetMetricData where
  type Rs GetMetricData = GetMetricDataResponse
  request = postJSON connect
  response =
    receiveJSON
      ( \s h x ->
          GetMetricDataResponse'
            <$> (x .?> "MetricResults" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetMetricData

instance NFData GetMetricData

instance ToHeaders GetMetricData where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON GetMetricData where
  toJSON GetMetricData' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _gmdNextToken,
            ("Groupings" .=) <$> _gmdGroupings,
            ("MaxResults" .=) <$> _gmdMaxResults,
            Just ("StartTime" .= _gmdStartTime),
            Just ("EndTime" .= _gmdEndTime),
            Just ("Filters" .= _gmdFilters),
            Just ("HistoricalMetrics" .= _gmdHistoricalMetrics)
          ]
      )

instance ToPath GetMetricData where
  toPath GetMetricData' {..} =
    mconcat ["/metrics/historical/", toBS _gmdInstanceId]

instance ToQuery GetMetricData where
  toQuery = const mempty

-- | /See:/ 'getMetricDataResponse' smart constructor.
data GetMetricDataResponse = GetMetricDataResponse'
  { _gmdrsMetricResults ::
      !(Maybe [HistoricalMetricResult]),
    _gmdrsNextToken :: !(Maybe Text),
    _gmdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMetricDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmdrsMetricResults' - Information about the historical metrics. If no grouping is specified, a summary of metric data is returned.
--
-- * 'gmdrsNextToken' - If there are additional results, this is the token for the next set of results. The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
--
-- * 'gmdrsResponseStatus' - -- | The response status code.
getMetricDataResponse ::
  -- | 'gmdrsResponseStatus'
  Int ->
  GetMetricDataResponse
getMetricDataResponse pResponseStatus_ =
  GetMetricDataResponse'
    { _gmdrsMetricResults = Nothing,
      _gmdrsNextToken = Nothing,
      _gmdrsResponseStatus = pResponseStatus_
    }

-- | Information about the historical metrics. If no grouping is specified, a summary of metric data is returned.
gmdrsMetricResults :: Lens' GetMetricDataResponse [HistoricalMetricResult]
gmdrsMetricResults = lens _gmdrsMetricResults (\s a -> s {_gmdrsMetricResults = a}) . _Default . _Coerce

-- | If there are additional results, this is the token for the next set of results. The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
gmdrsNextToken :: Lens' GetMetricDataResponse (Maybe Text)
gmdrsNextToken = lens _gmdrsNextToken (\s a -> s {_gmdrsNextToken = a})

-- | -- | The response status code.
gmdrsResponseStatus :: Lens' GetMetricDataResponse Int
gmdrsResponseStatus = lens _gmdrsResponseStatus (\s a -> s {_gmdrsResponseStatus = a})

instance NFData GetMetricDataResponse
