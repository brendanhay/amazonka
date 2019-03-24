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
-- Module      : Network.AWS.Connect.GetCurrentMetricData
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetCurrentMetricData@ operation retrieves current metric data from your Amazon Connect instance.
--
--
-- If you are using an IAM account, it must have permission to the @connect:GetCurrentMetricData@ action.
--
module Network.AWS.Connect.GetCurrentMetricData
    (
    -- * Creating a Request
      getCurrentMetricData
    , GetCurrentMetricData
    -- * Request Lenses
    , gcmdNextToken
    , gcmdGroupings
    , gcmdMaxResults
    , gcmdInstanceId
    , gcmdFilters
    , gcmdCurrentMetrics

    -- * Destructuring the Response
    , getCurrentMetricDataResponse
    , GetCurrentMetricDataResponse
    -- * Response Lenses
    , gcmdrsMetricResults
    , gcmdrsDataSnapshotTime
    , gcmdrsNextToken
    , gcmdrsResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCurrentMetricData' smart constructor.
data GetCurrentMetricData = GetCurrentMetricData'
  { _gcmdNextToken      :: !(Maybe Text)
  , _gcmdGroupings      :: !(Maybe [Grouping])
  , _gcmdMaxResults     :: !(Maybe Nat)
  , _gcmdInstanceId     :: !Text
  , _gcmdFilters        :: !Filters
  , _gcmdCurrentMetrics :: ![CurrentMetric]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCurrentMetricData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcmdNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results. The token expires after 5 minutes from the time it is created. Subsequent requests that use the < NextToken> must use the same request parameters as the request that generated the token.
--
-- * 'gcmdGroupings' - The grouping applied to the metrics returned. For example, when grouped by QUEUE, the metrics returned apply to each queue rather than aggregated for all queues. If you group by CHANNEL, you should include a Channels filter. The only supported channel is VOICE. If no @Grouping@ is included in the request, a summary of @CurrentMetrics@ is returned.
--
-- * 'gcmdMaxResults' - @MaxResults@ indicates the maximum number of results to return per page in the response, between 1 and 100.
--
-- * 'gcmdInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
--
-- * 'gcmdFilters' - A @Filters@ object that contains a list of queue IDs or queue ARNs, up to 100, or list of Channels to use to filter the metrics returned in the response. Metric data is retrieved only for the resources associated with the queue IDs, ARNs, or Channels included in the filter. You can include both IDs and ARNs in the same request. To retrieve metrics for all queues, add the queue ID or ARN for each queue in your instance. Only VOICE is supported for Channels. To find the ARN for a queue, open the queue you want to use in the Amazon Connect Queue editor. The ARN for the queue is displayed in the address bar as part of the URL. For example, the queue ARN is the set of characters at the end of the URL, after 'id=' such as @arn:aws:connect:us-east-1:270923740243:instance/78fb859d-1b7d-44b1-8aa3-12f0835c5855/queue/1d1a4575-9618-40ab-bbeb-81e45795fe61@ . The queue ID is also included in the URL, and is the string after 'queue/'.
--
-- * 'gcmdCurrentMetrics' - A list of @CurrentMetric@ objects for the metrics to retrieve. Each @CurrentMetric@ includes a name of a metric to retrieve and the unit to use for it. You must list each metric to retrieve data for in the request. The following metrics are available:     * AGENTS_AVAILABLE    * Unit: COUNT     * AGENTS_ONLINE    * Unit: COUNT     * AGENTS_ON_CALL    * Unit: COUNT     * AGENTS_STAFFED    * Unit: COUNT     * AGENTS_AFTER_CONTACT_WORK    * Unit: COUNT     * AGENTS_NON_PRODUCTIVE    * Unit: COUNT     * AGENTS_ERROR    * Unit: COUNT     * CONTACTS_IN_QUEUE    * Unit: COUNT     * OLDEST_CONTACT_AGE    * Unit: SECONDS     * CONTACTS_SCHEDULED    * Unit: COUNT
getCurrentMetricData
    :: Text -- ^ 'gcmdInstanceId'
    -> Filters -- ^ 'gcmdFilters'
    -> GetCurrentMetricData
getCurrentMetricData pInstanceId_ pFilters_ =
  GetCurrentMetricData'
    { _gcmdNextToken = Nothing
    , _gcmdGroupings = Nothing
    , _gcmdMaxResults = Nothing
    , _gcmdInstanceId = pInstanceId_
    , _gcmdFilters = pFilters_
    , _gcmdCurrentMetrics = mempty
    }


-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results. The token expires after 5 minutes from the time it is created. Subsequent requests that use the < NextToken> must use the same request parameters as the request that generated the token.
gcmdNextToken :: Lens' GetCurrentMetricData (Maybe Text)
gcmdNextToken = lens _gcmdNextToken (\ s a -> s{_gcmdNextToken = a})

-- | The grouping applied to the metrics returned. For example, when grouped by QUEUE, the metrics returned apply to each queue rather than aggregated for all queues. If you group by CHANNEL, you should include a Channels filter. The only supported channel is VOICE. If no @Grouping@ is included in the request, a summary of @CurrentMetrics@ is returned.
gcmdGroupings :: Lens' GetCurrentMetricData [Grouping]
gcmdGroupings = lens _gcmdGroupings (\ s a -> s{_gcmdGroupings = a}) . _Default . _Coerce

-- | @MaxResults@ indicates the maximum number of results to return per page in the response, between 1 and 100.
gcmdMaxResults :: Lens' GetCurrentMetricData (Maybe Natural)
gcmdMaxResults = lens _gcmdMaxResults (\ s a -> s{_gcmdMaxResults = a}) . mapping _Nat

-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
gcmdInstanceId :: Lens' GetCurrentMetricData Text
gcmdInstanceId = lens _gcmdInstanceId (\ s a -> s{_gcmdInstanceId = a})

-- | A @Filters@ object that contains a list of queue IDs or queue ARNs, up to 100, or list of Channels to use to filter the metrics returned in the response. Metric data is retrieved only for the resources associated with the queue IDs, ARNs, or Channels included in the filter. You can include both IDs and ARNs in the same request. To retrieve metrics for all queues, add the queue ID or ARN for each queue in your instance. Only VOICE is supported for Channels. To find the ARN for a queue, open the queue you want to use in the Amazon Connect Queue editor. The ARN for the queue is displayed in the address bar as part of the URL. For example, the queue ARN is the set of characters at the end of the URL, after 'id=' such as @arn:aws:connect:us-east-1:270923740243:instance/78fb859d-1b7d-44b1-8aa3-12f0835c5855/queue/1d1a4575-9618-40ab-bbeb-81e45795fe61@ . The queue ID is also included in the URL, and is the string after 'queue/'.
gcmdFilters :: Lens' GetCurrentMetricData Filters
gcmdFilters = lens _gcmdFilters (\ s a -> s{_gcmdFilters = a})

-- | A list of @CurrentMetric@ objects for the metrics to retrieve. Each @CurrentMetric@ includes a name of a metric to retrieve and the unit to use for it. You must list each metric to retrieve data for in the request. The following metrics are available:     * AGENTS_AVAILABLE    * Unit: COUNT     * AGENTS_ONLINE    * Unit: COUNT     * AGENTS_ON_CALL    * Unit: COUNT     * AGENTS_STAFFED    * Unit: COUNT     * AGENTS_AFTER_CONTACT_WORK    * Unit: COUNT     * AGENTS_NON_PRODUCTIVE    * Unit: COUNT     * AGENTS_ERROR    * Unit: COUNT     * CONTACTS_IN_QUEUE    * Unit: COUNT     * OLDEST_CONTACT_AGE    * Unit: SECONDS     * CONTACTS_SCHEDULED    * Unit: COUNT
gcmdCurrentMetrics :: Lens' GetCurrentMetricData [CurrentMetric]
gcmdCurrentMetrics = lens _gcmdCurrentMetrics (\ s a -> s{_gcmdCurrentMetrics = a}) . _Coerce

instance AWSRequest GetCurrentMetricData where
        type Rs GetCurrentMetricData =
             GetCurrentMetricDataResponse
        request = postJSON connect
        response
          = receiveJSON
              (\ s h x ->
                 GetCurrentMetricDataResponse' <$>
                   (x .?> "MetricResults" .!@ mempty) <*>
                     (x .?> "DataSnapshotTime")
                     <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetCurrentMetricData where

instance NFData GetCurrentMetricData where

instance ToHeaders GetCurrentMetricData where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCurrentMetricData where
        toJSON GetCurrentMetricData'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gcmdNextToken,
                  ("Groupings" .=) <$> _gcmdGroupings,
                  ("MaxResults" .=) <$> _gcmdMaxResults,
                  Just ("Filters" .= _gcmdFilters),
                  Just ("CurrentMetrics" .= _gcmdCurrentMetrics)])

instance ToPath GetCurrentMetricData where
        toPath GetCurrentMetricData'{..}
          = mconcat ["/metrics/current/", toBS _gcmdInstanceId]

instance ToQuery GetCurrentMetricData where
        toQuery = const mempty

-- | /See:/ 'getCurrentMetricDataResponse' smart constructor.
data GetCurrentMetricDataResponse = GetCurrentMetricDataResponse'
  { _gcmdrsMetricResults    :: !(Maybe [CurrentMetricResult])
  , _gcmdrsDataSnapshotTime :: !(Maybe POSIX)
  , _gcmdrsNextToken        :: !(Maybe Text)
  , _gcmdrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCurrentMetricDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcmdrsMetricResults' - A list of @CurrentMetricResult@ objects organized by @Dimensions@ combining with @CurrentMetricDataCollections@ . @Dimensions@ is the resourceId specified in the @Filters@ of the request.  @Collections@ is a list of @CurrentMetricData@ objects with corresponding values to the @CurrentMetrics@ specified in the request. If no @Grouping@ is specified in the request, @Collections@ is a summary for the @CurrentMetric@ returned.
--
-- * 'gcmdrsDataSnapshotTime' - The time at which @CurrentMetricData@ was retrieved and cached for pagination.
--
-- * 'gcmdrsNextToken' - A string returned in the response. Use the value returned in the response as the value of the NextToken in a subsequent request to retrieve the next set of results. The token expires after 5 minutes from the time it is created. Subsequent requests that use the NextToken must use the same request parameters as the request that generated the token.
--
-- * 'gcmdrsResponseStatus' - -- | The response status code.
getCurrentMetricDataResponse
    :: Int -- ^ 'gcmdrsResponseStatus'
    -> GetCurrentMetricDataResponse
getCurrentMetricDataResponse pResponseStatus_ =
  GetCurrentMetricDataResponse'
    { _gcmdrsMetricResults = Nothing
    , _gcmdrsDataSnapshotTime = Nothing
    , _gcmdrsNextToken = Nothing
    , _gcmdrsResponseStatus = pResponseStatus_
    }


-- | A list of @CurrentMetricResult@ objects organized by @Dimensions@ combining with @CurrentMetricDataCollections@ . @Dimensions@ is the resourceId specified in the @Filters@ of the request.  @Collections@ is a list of @CurrentMetricData@ objects with corresponding values to the @CurrentMetrics@ specified in the request. If no @Grouping@ is specified in the request, @Collections@ is a summary for the @CurrentMetric@ returned.
gcmdrsMetricResults :: Lens' GetCurrentMetricDataResponse [CurrentMetricResult]
gcmdrsMetricResults = lens _gcmdrsMetricResults (\ s a -> s{_gcmdrsMetricResults = a}) . _Default . _Coerce

-- | The time at which @CurrentMetricData@ was retrieved and cached for pagination.
gcmdrsDataSnapshotTime :: Lens' GetCurrentMetricDataResponse (Maybe UTCTime)
gcmdrsDataSnapshotTime = lens _gcmdrsDataSnapshotTime (\ s a -> s{_gcmdrsDataSnapshotTime = a}) . mapping _Time

-- | A string returned in the response. Use the value returned in the response as the value of the NextToken in a subsequent request to retrieve the next set of results. The token expires after 5 minutes from the time it is created. Subsequent requests that use the NextToken must use the same request parameters as the request that generated the token.
gcmdrsNextToken :: Lens' GetCurrentMetricDataResponse (Maybe Text)
gcmdrsNextToken = lens _gcmdrsNextToken (\ s a -> s{_gcmdrsNextToken = a})

-- | -- | The response status code.
gcmdrsResponseStatus :: Lens' GetCurrentMetricDataResponse Int
gcmdrsResponseStatus = lens _gcmdrsResponseStatus (\ s a -> s{_gcmdrsResponseStatus = a})

instance NFData GetCurrentMetricDataResponse where
