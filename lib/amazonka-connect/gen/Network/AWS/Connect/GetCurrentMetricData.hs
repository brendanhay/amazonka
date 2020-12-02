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
-- Module      : Network.AWS.Connect.GetCurrentMetricData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the real-time metric data from the specified Amazon Connect instance.
--
--
-- For a description of each metric, see <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html Real-time Metrics Definitions> in the /Amazon Connect Administrator Guide/ .
module Network.AWS.Connect.GetCurrentMetricData
  ( -- * Creating a Request
    getCurrentMetricData,
    GetCurrentMetricData,

    -- * Request Lenses
    gcmdNextToken,
    gcmdGroupings,
    gcmdMaxResults,
    gcmdInstanceId,
    gcmdFilters,
    gcmdCurrentMetrics,

    -- * Destructuring the Response
    getCurrentMetricDataResponse,
    GetCurrentMetricDataResponse,

    -- * Response Lenses
    gcmdrsMetricResults,
    gcmdrsDataSnapshotTime,
    gcmdrsNextToken,
    gcmdrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCurrentMetricData' smart constructor.
data GetCurrentMetricData = GetCurrentMetricData'
  { _gcmdNextToken ::
      !(Maybe Text),
    _gcmdGroupings :: !(Maybe [Grouping]),
    _gcmdMaxResults :: !(Maybe Nat),
    _gcmdInstanceId :: !Text,
    _gcmdFilters :: !Filters,
    _gcmdCurrentMetrics :: ![CurrentMetric]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCurrentMetricData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcmdNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results. The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
--
-- * 'gcmdGroupings' - The grouping applied to the metrics returned. For example, when grouped by @QUEUE@ , the metrics returned apply to each queue rather than aggregated for all queues. If you group by @CHANNEL@ , you should include a Channels filter. Both @VOICE@ and @CHAT@ channels are supported. If no @Grouping@ is included in the request, a summary of metrics is returned.
--
-- * 'gcmdMaxResults' - The maximimum number of results to return per page.
--
-- * 'gcmdInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'gcmdFilters' - The queues, up to 100, or channels, to use to filter the metrics returned. Metric data is retrieved only for the resources associated with the queues or channels included in the filter. You can include both queue IDs and queue ARNs in the same request. Both @VOICE@ and @CHAT@ channels are supported.
--
-- * 'gcmdCurrentMetrics' - The metrics to retrieve. Specify the name and unit for each metric. The following metrics are available. For a description of all the metrics, see <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html Real-time Metrics Definitions> in the /Amazon Connect Administrator Guide/ .     * AGENTS_AFTER_CONTACT_WORK    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#aftercallwork-real-time ACW>      * AGENTS_AVAILABLE    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#available-real-time Available>      * AGENTS_ERROR    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#error-real-time Error>      * AGENTS_NON_PRODUCTIVE    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#non-productive-time-real-time NPT (Non-Productive Time)>      * AGENTS_ON_CALL    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#on-call-real-time On contact>      * AGENTS_ON_CONTACT    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#on-call-real-time On contact>      * AGENTS_ONLINE    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#online-real-time Online>      * AGENTS_STAFFED    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#staffed-real-time Staffed>      * CONTACTS_IN_QUEUE    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#in-queue-real-time In queue>      * CONTACTS_SCHEDULED    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#scheduled-real-time Scheduled>      * OLDEST_CONTACT_AGE    * Unit: SECONDS When you use groupings, Unit says SECONDS but the Value is returned in MILLISECONDS. For example, if you get a response like this: @{ "Metric": { "Name": "OLDEST_CONTACT_AGE", "Unit": "SECONDS" }, "Value": 24113.0 @ } The actual OLDEST_CONTACT_AGE is 24 seconds. Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#oldest-real-time Oldest>      * SLOTS_ACTIVE    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#active-real-time Active>      * SLOTS_AVAILABLE    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#availability-real-time Availability>
getCurrentMetricData ::
  -- | 'gcmdInstanceId'
  Text ->
  -- | 'gcmdFilters'
  Filters ->
  GetCurrentMetricData
getCurrentMetricData pInstanceId_ pFilters_ =
  GetCurrentMetricData'
    { _gcmdNextToken = Nothing,
      _gcmdGroupings = Nothing,
      _gcmdMaxResults = Nothing,
      _gcmdInstanceId = pInstanceId_,
      _gcmdFilters = pFilters_,
      _gcmdCurrentMetrics = mempty
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results. The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
gcmdNextToken :: Lens' GetCurrentMetricData (Maybe Text)
gcmdNextToken = lens _gcmdNextToken (\s a -> s {_gcmdNextToken = a})

-- | The grouping applied to the metrics returned. For example, when grouped by @QUEUE@ , the metrics returned apply to each queue rather than aggregated for all queues. If you group by @CHANNEL@ , you should include a Channels filter. Both @VOICE@ and @CHAT@ channels are supported. If no @Grouping@ is included in the request, a summary of metrics is returned.
gcmdGroupings :: Lens' GetCurrentMetricData [Grouping]
gcmdGroupings = lens _gcmdGroupings (\s a -> s {_gcmdGroupings = a}) . _Default . _Coerce

-- | The maximimum number of results to return per page.
gcmdMaxResults :: Lens' GetCurrentMetricData (Maybe Natural)
gcmdMaxResults = lens _gcmdMaxResults (\s a -> s {_gcmdMaxResults = a}) . mapping _Nat

-- | The identifier of the Amazon Connect instance.
gcmdInstanceId :: Lens' GetCurrentMetricData Text
gcmdInstanceId = lens _gcmdInstanceId (\s a -> s {_gcmdInstanceId = a})

-- | The queues, up to 100, or channels, to use to filter the metrics returned. Metric data is retrieved only for the resources associated with the queues or channels included in the filter. You can include both queue IDs and queue ARNs in the same request. Both @VOICE@ and @CHAT@ channels are supported.
gcmdFilters :: Lens' GetCurrentMetricData Filters
gcmdFilters = lens _gcmdFilters (\s a -> s {_gcmdFilters = a})

-- | The metrics to retrieve. Specify the name and unit for each metric. The following metrics are available. For a description of all the metrics, see <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html Real-time Metrics Definitions> in the /Amazon Connect Administrator Guide/ .     * AGENTS_AFTER_CONTACT_WORK    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#aftercallwork-real-time ACW>      * AGENTS_AVAILABLE    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#available-real-time Available>      * AGENTS_ERROR    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#error-real-time Error>      * AGENTS_NON_PRODUCTIVE    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#non-productive-time-real-time NPT (Non-Productive Time)>      * AGENTS_ON_CALL    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#on-call-real-time On contact>      * AGENTS_ON_CONTACT    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#on-call-real-time On contact>      * AGENTS_ONLINE    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#online-real-time Online>      * AGENTS_STAFFED    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#staffed-real-time Staffed>      * CONTACTS_IN_QUEUE    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#in-queue-real-time In queue>      * CONTACTS_SCHEDULED    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#scheduled-real-time Scheduled>      * OLDEST_CONTACT_AGE    * Unit: SECONDS When you use groupings, Unit says SECONDS but the Value is returned in MILLISECONDS. For example, if you get a response like this: @{ "Metric": { "Name": "OLDEST_CONTACT_AGE", "Unit": "SECONDS" }, "Value": 24113.0 @ } The actual OLDEST_CONTACT_AGE is 24 seconds. Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#oldest-real-time Oldest>      * SLOTS_ACTIVE    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#active-real-time Active>      * SLOTS_AVAILABLE    * Unit: COUNT Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#availability-real-time Availability>
gcmdCurrentMetrics :: Lens' GetCurrentMetricData [CurrentMetric]
gcmdCurrentMetrics = lens _gcmdCurrentMetrics (\s a -> s {_gcmdCurrentMetrics = a}) . _Coerce

instance AWSRequest GetCurrentMetricData where
  type Rs GetCurrentMetricData = GetCurrentMetricDataResponse
  request = postJSON connect
  response =
    receiveJSON
      ( \s h x ->
          GetCurrentMetricDataResponse'
            <$> (x .?> "MetricResults" .!@ mempty)
            <*> (x .?> "DataSnapshotTime")
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetCurrentMetricData

instance NFData GetCurrentMetricData

instance ToHeaders GetCurrentMetricData where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON GetCurrentMetricData where
  toJSON GetCurrentMetricData' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _gcmdNextToken,
            ("Groupings" .=) <$> _gcmdGroupings,
            ("MaxResults" .=) <$> _gcmdMaxResults,
            Just ("Filters" .= _gcmdFilters),
            Just ("CurrentMetrics" .= _gcmdCurrentMetrics)
          ]
      )

instance ToPath GetCurrentMetricData where
  toPath GetCurrentMetricData' {..} =
    mconcat ["/metrics/current/", toBS _gcmdInstanceId]

instance ToQuery GetCurrentMetricData where
  toQuery = const mempty

-- | /See:/ 'getCurrentMetricDataResponse' smart constructor.
data GetCurrentMetricDataResponse = GetCurrentMetricDataResponse'
  { _gcmdrsMetricResults ::
      !(Maybe [CurrentMetricResult]),
    _gcmdrsDataSnapshotTime ::
      !(Maybe POSIX),
    _gcmdrsNextToken :: !(Maybe Text),
    _gcmdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCurrentMetricDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcmdrsMetricResults' - Information about the real-time metrics.
--
-- * 'gcmdrsDataSnapshotTime' - The time at which the metrics were retrieved and cached for pagination.
--
-- * 'gcmdrsNextToken' - If there are additional results, this is the token for the next set of results. The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
--
-- * 'gcmdrsResponseStatus' - -- | The response status code.
getCurrentMetricDataResponse ::
  -- | 'gcmdrsResponseStatus'
  Int ->
  GetCurrentMetricDataResponse
getCurrentMetricDataResponse pResponseStatus_ =
  GetCurrentMetricDataResponse'
    { _gcmdrsMetricResults = Nothing,
      _gcmdrsDataSnapshotTime = Nothing,
      _gcmdrsNextToken = Nothing,
      _gcmdrsResponseStatus = pResponseStatus_
    }

-- | Information about the real-time metrics.
gcmdrsMetricResults :: Lens' GetCurrentMetricDataResponse [CurrentMetricResult]
gcmdrsMetricResults = lens _gcmdrsMetricResults (\s a -> s {_gcmdrsMetricResults = a}) . _Default . _Coerce

-- | The time at which the metrics were retrieved and cached for pagination.
gcmdrsDataSnapshotTime :: Lens' GetCurrentMetricDataResponse (Maybe UTCTime)
gcmdrsDataSnapshotTime = lens _gcmdrsDataSnapshotTime (\s a -> s {_gcmdrsDataSnapshotTime = a}) . mapping _Time

-- | If there are additional results, this is the token for the next set of results. The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
gcmdrsNextToken :: Lens' GetCurrentMetricDataResponse (Maybe Text)
gcmdrsNextToken = lens _gcmdrsNextToken (\s a -> s {_gcmdrsNextToken = a})

-- | -- | The response status code.
gcmdrsResponseStatus :: Lens' GetCurrentMetricDataResponse Int
gcmdrsResponseStatus = lens _gcmdrsResponseStatus (\s a -> s {_gcmdrsResponseStatus = a})

instance NFData GetCurrentMetricDataResponse
