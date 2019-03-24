{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.GetMetricData
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetMetricData@ operation retrieves historical metrics data from your Amazon Connect instance.
--
--
-- If you are using an IAM account, it must have permission to the @connect:GetMetricData@ action.
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.GetMetricData
    (
    -- * Creating a Request
      getMetricData
    , GetMetricData
    -- * Request Lenses
    , gmdNextToken
    , gmdGroupings
    , gmdMaxResults
    , gmdInstanceId
    , gmdStartTime
    , gmdEndTime
    , gmdFilters
    , gmdHistoricalMetrics

    -- * Destructuring the Response
    , getMetricDataResponse
    , GetMetricDataResponse
    -- * Response Lenses
    , gmdrsMetricResults
    , gmdrsNextToken
    , gmdrsResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMetricData' smart constructor.
data GetMetricData = GetMetricData'
  { _gmdNextToken         :: !(Maybe Text)
  , _gmdGroupings         :: !(Maybe [Grouping])
  , _gmdMaxResults        :: !(Maybe Nat)
  , _gmdInstanceId        :: !Text
  , _gmdStartTime         :: !POSIX
  , _gmdEndTime           :: !POSIX
  , _gmdFilters           :: !Filters
  , _gmdHistoricalMetrics :: ![HistoricalMetric]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMetricData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmdNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'gmdGroupings' - The grouping applied to the metrics returned. For example, when results are grouped by queueId, the metrics returned are grouped by queue. The values returned apply to the metrics for each queue rather than aggregated for all queues. The current version supports grouping by Queue If no @Grouping@ is included in the request, a summary of @HistoricalMetrics@ for all queues is returned.
--
-- * 'gmdMaxResults' - Indicates the maximum number of results to return per page in the response, between 1-100.
--
-- * 'gmdInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
--
-- * 'gmdStartTime' - The timestamp, in UNIX Epoch time format, at which to start the reporting interval for the retrieval of historical metrics data. The time must be specified using a multiple of 5 minutes, such as 10:05, 10:10, 10:15. @StartTime@ cannot be earlier than 24 hours before the time of the request. Historical metrics are available in Amazon Connect only for 24 hours.
--
-- * 'gmdEndTime' - The timestamp, in UNIX Epoch time format, at which to end the reporting interval for the retrieval of historical metrics data. The time must be specified using an interval of 5 minutes, such as 11:00, 11:05, 11:10, and must be later than the @StartTime@ timestamp. The time range between @StartTime@ and @EndTime@ must be less than 24 hours.
--
-- * 'gmdFilters' - A @Filters@ object that contains a list of queue IDs or queue ARNs, up to 100, or a list of Channels to use to filter the metrics returned in the response. Metric data is retrieved only for the resources associated with the IDs, ARNs, or Channels included in the filter. You can use both IDs and ARNs together in a request. Only VOICE is supported for Channel. To find the ARN for a queue, open the queue you want to use in the Amazon Connect Queue editor. The ARN for the queue is displayed in the address bar as part of the URL. For example, the queue ARN is the set of characters at the end of the URL, after 'id=' such as @arn:aws:connect:us-east-1:270923740243:instance/78fb859d-1b7d-44b1-8aa3-12f0835c5855/queue/1d1a4575-9618-40ab-bbeb-81e45795fe61@ . The queue ID is also included in the URL, and is the string after 'queue/'.
--
-- * 'gmdHistoricalMetrics' - A list of @HistoricalMetric@ objects that contain the metrics to retrieve with the request. A @HistoricalMetric@ object contains: @HistoricalMetricName@ , @Statistic@ , @Threshold@ , and @Unit@ . You must list each metric to retrieve data for in the request. For each historical metric you include in the request, you must include a @Unit@ and a @Statistic@ .  The following historical metrics are available:     * CONTACTS_QUEUED    * Unit: COUNT Statistic: SUM     * CONTACTS_HANDLED    * Unit: COUNT Statistics: SUM     * CONTACTS_ABANDONED    * Unit: COUNT Statistics: SUM     * CONTACTS_CONSULTED    * Unit: COUNT Statistics: SUM     * CONTACTS_AGENT_HUNG_UP_FIRST    * Unit: COUNT Statistics: SUM     * CONTACTS_HANDLED_INCOMING    * Unit: COUNT Statistics: SUM     * CONTACTS_HANDLED_OUTBOUND    * Unit: COUNT Statistics: SUM     * CONTACTS_HOLD_ABANDONS    * Unit: COUNT Statistics: SUM     * CONTACTS_TRANSFERRED_IN    * Unit: COUNT Statistics: SUM     * CONTACTS_TRANSFERRED_OUT    * Unit: COUNT Statistics: SUM     * CONTACTS_TRANSFERRED_IN_FROM_QUEUE    * Unit: COUNT Statistics: SUM     * CONTACTS_TRANSFERRED_OUT_FROM_QUEUE    * Unit: COUNT Statistics: SUM     * CALLBACK_CONTACTS_HANDLED    * Unit: COUNT Statistics: SUM     * CALLBACK_CONTACTS_HANDLED    * Unit: COUNT Statistics: SUM     * API_CONTACTS_HANDLED    * Unit: COUNT Statistics: SUM     * CONTACTS_MISSED    * Unit: COUNT Statistics: SUM     * OCCUPANCY    * Unit: PERCENT Statistics: AVG     * HANDLE_TIME    * Unit: SECONDS Statistics: AVG     * AFTER_CONTACT_WORK_TIME    * Unit: SECONDS Statistics: AVG     * QUEUED_TIME    * Unit: SECONDS Statistics: MAX     * ABANDON_TIME    * Unit: COUNT Statistics: SUM     * QUEUE_ANSWER_TIME    * Unit: SECONDS Statistics: AVG     * HOLD_TIME    * Unit: SECONDS Statistics: AVG     * INTERACTION_TIME    * Unit: SECONDS Statistics: AVG     * INTERACTION_AND_HOLD_TIME    * Unit: SECONDS Statistics: AVG     * SERVICE_LEVEL    * Unit: PERCENT Statistics: AVG Threshold: Only "Less than" comparisons are supported, with the following service level thresholds: 15, 20, 25, 30, 45, 60, 90, 120, 180, 240, 300, 600
getMetricData
    :: Text -- ^ 'gmdInstanceId'
    -> UTCTime -- ^ 'gmdStartTime'
    -> UTCTime -- ^ 'gmdEndTime'
    -> Filters -- ^ 'gmdFilters'
    -> GetMetricData
getMetricData pInstanceId_ pStartTime_ pEndTime_ pFilters_ =
  GetMetricData'
    { _gmdNextToken = Nothing
    , _gmdGroupings = Nothing
    , _gmdMaxResults = Nothing
    , _gmdInstanceId = pInstanceId_
    , _gmdStartTime = _Time # pStartTime_
    , _gmdEndTime = _Time # pEndTime_
    , _gmdFilters = pFilters_
    , _gmdHistoricalMetrics = mempty
    }


-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
gmdNextToken :: Lens' GetMetricData (Maybe Text)
gmdNextToken = lens _gmdNextToken (\ s a -> s{_gmdNextToken = a})

-- | The grouping applied to the metrics returned. For example, when results are grouped by queueId, the metrics returned are grouped by queue. The values returned apply to the metrics for each queue rather than aggregated for all queues. The current version supports grouping by Queue If no @Grouping@ is included in the request, a summary of @HistoricalMetrics@ for all queues is returned.
gmdGroupings :: Lens' GetMetricData [Grouping]
gmdGroupings = lens _gmdGroupings (\ s a -> s{_gmdGroupings = a}) . _Default . _Coerce

-- | Indicates the maximum number of results to return per page in the response, between 1-100.
gmdMaxResults :: Lens' GetMetricData (Maybe Natural)
gmdMaxResults = lens _gmdMaxResults (\ s a -> s{_gmdMaxResults = a}) . mapping _Nat

-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
gmdInstanceId :: Lens' GetMetricData Text
gmdInstanceId = lens _gmdInstanceId (\ s a -> s{_gmdInstanceId = a})

-- | The timestamp, in UNIX Epoch time format, at which to start the reporting interval for the retrieval of historical metrics data. The time must be specified using a multiple of 5 minutes, such as 10:05, 10:10, 10:15. @StartTime@ cannot be earlier than 24 hours before the time of the request. Historical metrics are available in Amazon Connect only for 24 hours.
gmdStartTime :: Lens' GetMetricData UTCTime
gmdStartTime = lens _gmdStartTime (\ s a -> s{_gmdStartTime = a}) . _Time

-- | The timestamp, in UNIX Epoch time format, at which to end the reporting interval for the retrieval of historical metrics data. The time must be specified using an interval of 5 minutes, such as 11:00, 11:05, 11:10, and must be later than the @StartTime@ timestamp. The time range between @StartTime@ and @EndTime@ must be less than 24 hours.
gmdEndTime :: Lens' GetMetricData UTCTime
gmdEndTime = lens _gmdEndTime (\ s a -> s{_gmdEndTime = a}) . _Time

-- | A @Filters@ object that contains a list of queue IDs or queue ARNs, up to 100, or a list of Channels to use to filter the metrics returned in the response. Metric data is retrieved only for the resources associated with the IDs, ARNs, or Channels included in the filter. You can use both IDs and ARNs together in a request. Only VOICE is supported for Channel. To find the ARN for a queue, open the queue you want to use in the Amazon Connect Queue editor. The ARN for the queue is displayed in the address bar as part of the URL. For example, the queue ARN is the set of characters at the end of the URL, after 'id=' such as @arn:aws:connect:us-east-1:270923740243:instance/78fb859d-1b7d-44b1-8aa3-12f0835c5855/queue/1d1a4575-9618-40ab-bbeb-81e45795fe61@ . The queue ID is also included in the URL, and is the string after 'queue/'.
gmdFilters :: Lens' GetMetricData Filters
gmdFilters = lens _gmdFilters (\ s a -> s{_gmdFilters = a})

-- | A list of @HistoricalMetric@ objects that contain the metrics to retrieve with the request. A @HistoricalMetric@ object contains: @HistoricalMetricName@ , @Statistic@ , @Threshold@ , and @Unit@ . You must list each metric to retrieve data for in the request. For each historical metric you include in the request, you must include a @Unit@ and a @Statistic@ .  The following historical metrics are available:     * CONTACTS_QUEUED    * Unit: COUNT Statistic: SUM     * CONTACTS_HANDLED    * Unit: COUNT Statistics: SUM     * CONTACTS_ABANDONED    * Unit: COUNT Statistics: SUM     * CONTACTS_CONSULTED    * Unit: COUNT Statistics: SUM     * CONTACTS_AGENT_HUNG_UP_FIRST    * Unit: COUNT Statistics: SUM     * CONTACTS_HANDLED_INCOMING    * Unit: COUNT Statistics: SUM     * CONTACTS_HANDLED_OUTBOUND    * Unit: COUNT Statistics: SUM     * CONTACTS_HOLD_ABANDONS    * Unit: COUNT Statistics: SUM     * CONTACTS_TRANSFERRED_IN    * Unit: COUNT Statistics: SUM     * CONTACTS_TRANSFERRED_OUT    * Unit: COUNT Statistics: SUM     * CONTACTS_TRANSFERRED_IN_FROM_QUEUE    * Unit: COUNT Statistics: SUM     * CONTACTS_TRANSFERRED_OUT_FROM_QUEUE    * Unit: COUNT Statistics: SUM     * CALLBACK_CONTACTS_HANDLED    * Unit: COUNT Statistics: SUM     * CALLBACK_CONTACTS_HANDLED    * Unit: COUNT Statistics: SUM     * API_CONTACTS_HANDLED    * Unit: COUNT Statistics: SUM     * CONTACTS_MISSED    * Unit: COUNT Statistics: SUM     * OCCUPANCY    * Unit: PERCENT Statistics: AVG     * HANDLE_TIME    * Unit: SECONDS Statistics: AVG     * AFTER_CONTACT_WORK_TIME    * Unit: SECONDS Statistics: AVG     * QUEUED_TIME    * Unit: SECONDS Statistics: MAX     * ABANDON_TIME    * Unit: COUNT Statistics: SUM     * QUEUE_ANSWER_TIME    * Unit: SECONDS Statistics: AVG     * HOLD_TIME    * Unit: SECONDS Statistics: AVG     * INTERACTION_TIME    * Unit: SECONDS Statistics: AVG     * INTERACTION_AND_HOLD_TIME    * Unit: SECONDS Statistics: AVG     * SERVICE_LEVEL    * Unit: PERCENT Statistics: AVG Threshold: Only "Less than" comparisons are supported, with the following service level thresholds: 15, 20, 25, 30, 45, 60, 90, 120, 180, 240, 300, 600
gmdHistoricalMetrics :: Lens' GetMetricData [HistoricalMetric]
gmdHistoricalMetrics = lens _gmdHistoricalMetrics (\ s a -> s{_gmdHistoricalMetrics = a}) . _Coerce

instance AWSPager GetMetricData where
        page rq rs
          | stop (rs ^. gmdrsNextToken) = Nothing
          | stop (rs ^. gmdrsMetricResults) = Nothing
          | otherwise =
            Just $ rq & gmdNextToken .~ rs ^. gmdrsNextToken

instance AWSRequest GetMetricData where
        type Rs GetMetricData = GetMetricDataResponse
        request = postJSON connect
        response
          = receiveJSON
              (\ s h x ->
                 GetMetricDataResponse' <$>
                   (x .?> "MetricResults" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetMetricData where

instance NFData GetMetricData where

instance ToHeaders GetMetricData where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetMetricData where
        toJSON GetMetricData'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gmdNextToken,
                  ("Groupings" .=) <$> _gmdGroupings,
                  ("MaxResults" .=) <$> _gmdMaxResults,
                  Just ("StartTime" .= _gmdStartTime),
                  Just ("EndTime" .= _gmdEndTime),
                  Just ("Filters" .= _gmdFilters),
                  Just ("HistoricalMetrics" .= _gmdHistoricalMetrics)])

instance ToPath GetMetricData where
        toPath GetMetricData'{..}
          = mconcat
              ["/metrics/historical/", toBS _gmdInstanceId]

instance ToQuery GetMetricData where
        toQuery = const mempty

-- | /See:/ 'getMetricDataResponse' smart constructor.
data GetMetricDataResponse = GetMetricDataResponse'
  { _gmdrsMetricResults  :: !(Maybe [HistoricalMetricResult])
  , _gmdrsNextToken      :: !(Maybe Text)
  , _gmdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMetricDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmdrsMetricResults' - A list of @HistoricalMetricResult@ objects, organized by @Dimensions@ , which is the ID of the resource specified in the @Filters@ used for the request. The metrics are combined with the metrics included in @Collections@ , which is a list of @HisotricalMetricData@ objects. If no @Grouping@ is specified in the request, @Collections@ includes summary data for the @HistoricalMetrics@ .
--
-- * 'gmdrsNextToken' - A string returned in the response. Use the value returned in the response as the value of the NextToken in a subsequent request to retrieve the next set of results. The token expires after 5 minutes from the time it is created. Subsequent requests that use the NextToken must use the same request parameters as the request that generated the token.
--
-- * 'gmdrsResponseStatus' - -- | The response status code.
getMetricDataResponse
    :: Int -- ^ 'gmdrsResponseStatus'
    -> GetMetricDataResponse
getMetricDataResponse pResponseStatus_ =
  GetMetricDataResponse'
    { _gmdrsMetricResults = Nothing
    , _gmdrsNextToken = Nothing
    , _gmdrsResponseStatus = pResponseStatus_
    }


-- | A list of @HistoricalMetricResult@ objects, organized by @Dimensions@ , which is the ID of the resource specified in the @Filters@ used for the request. The metrics are combined with the metrics included in @Collections@ , which is a list of @HisotricalMetricData@ objects. If no @Grouping@ is specified in the request, @Collections@ includes summary data for the @HistoricalMetrics@ .
gmdrsMetricResults :: Lens' GetMetricDataResponse [HistoricalMetricResult]
gmdrsMetricResults = lens _gmdrsMetricResults (\ s a -> s{_gmdrsMetricResults = a}) . _Default . _Coerce

-- | A string returned in the response. Use the value returned in the response as the value of the NextToken in a subsequent request to retrieve the next set of results. The token expires after 5 minutes from the time it is created. Subsequent requests that use the NextToken must use the same request parameters as the request that generated the token.
gmdrsNextToken :: Lens' GetMetricDataResponse (Maybe Text)
gmdrsNextToken = lens _gmdrsNextToken (\ s a -> s{_gmdrsNextToken = a})

-- | -- | The response status code.
gmdrsResponseStatus :: Lens' GetMetricDataResponse Int
gmdrsResponseStatus = lens _gmdrsResponseStatus (\ s a -> s{_gmdrsResponseStatus = a})

instance NFData GetMetricDataResponse where
