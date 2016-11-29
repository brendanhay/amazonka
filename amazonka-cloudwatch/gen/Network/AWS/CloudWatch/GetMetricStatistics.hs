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
-- Module      : Network.AWS.CloudWatch.GetMetricStatistics
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets statistics for the specified metric.
--
--
-- Amazon CloudWatch retains metric data as follows:
--
--     * Data points with a period of 60 seconds (1 minute) are available for 15 days
--
--     * Data points with a period of 300 seconds (5 minute) are available for 63 days
--
--     * Data points with a period of 3600 seconds (1 hour) are available for 455 days (15 months)
--
--
--
-- Note that CloudWatch started retaining 5-minute and 1-hour metric data as of 9 July 2016.
--
-- The maximum number of data points returned from a single call is 1,440. If you request more than 1,440 data points, Amazon CloudWatch returns an error. To reduce the number of data points, you can narrow the specified time range and make multiple requests across adjacent time ranges, or you can increase the specified period. A period can be as short as one minute (60 seconds). Note that data points are not returned in chronological order.
--
-- Amazon CloudWatch aggregates data points based on the length of the period that you specify. For example, if you request statistics with a one-hour period, Amazon CloudWatch aggregates all data points with time stamps that fall within each one-hour period. Therefore, the number of values aggregated by CloudWatch is larger than the number of data points returned.
--
-- For a list of metrics and dimensions supported by AWS services, see the <http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CW_Support_For_AWS.html Amazon CloudWatch Metrics and Dimensions Reference> in the /Amazon CloudWatch User Guide/ .
--
module Network.AWS.CloudWatch.GetMetricStatistics
    (
    -- * Creating a Request
      getMetricStatistics
    , GetMetricStatistics
    -- * Request Lenses
    , gmsExtendedStatistics
    , gmsStatistics
    , gmsDimensions
    , gmsUnit
    , gmsNamespace
    , gmsMetricName
    , gmsStartTime
    , gmsEndTime
    , gmsPeriod

    -- * Destructuring the Response
    , getMetricStatisticsResponse
    , GetMetricStatisticsResponse
    -- * Response Lenses
    , gmsrsDatapoints
    , gmsrsLabel
    , gmsrsResponseStatus
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.CloudWatch.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getMetricStatistics' smart constructor.
data GetMetricStatistics = GetMetricStatistics'
    { _gmsExtendedStatistics :: !(Maybe (List1 Text))
    , _gmsStatistics         :: !(Maybe (List1 Statistic))
    , _gmsDimensions         :: !(Maybe [Dimension])
    , _gmsUnit               :: !(Maybe StandardUnit)
    , _gmsNamespace          :: !Text
    , _gmsMetricName         :: !Text
    , _gmsStartTime          :: !ISO8601
    , _gmsEndTime            :: !ISO8601
    , _gmsPeriod             :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetMetricStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmsExtendedStatistics' - The percentile statistics. Specify values between p0.0 and p100.
--
-- * 'gmsStatistics' - The metric statistics, other than percentile. For percentile statistics, use @ExtendedStatistic@ .
--
-- * 'gmsDimensions' - The dimensions. CloudWatch treats each unique combination of dimensions as a separate metric. You can't retrieve statistics using combinations of dimensions that were not specially published. You must specify the same dimensions that were used when the metrics were created. For an example, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#dimension-combinations Dimension Combinations> in the /Amazon CloudWatch User Guide/ .
--
-- * 'gmsUnit' - The unit for a given metric. Metrics may be reported in multiple units. Not supplying a unit results in all units being returned. If the metric only ever reports one unit, specifying a unit has no effect.
--
-- * 'gmsNamespace' - The namespace of the metric, with or without spaces.
--
-- * 'gmsMetricName' - The name of the metric, with or without spaces.
--
-- * 'gmsStartTime' - The time stamp that determines the first data point to return. Note that start times are evaluated relative to the time that CloudWatch receives the request. The value specified is inclusive; results include data points with the specified time stamp. The time stamp must be in ISO 8601 UTC format (for example, 2016-10-03T23:00:00Z). CloudWatch rounds the specified time stamp as follows:     * Start time less than 15 days ago - Round down to the nearest whole minute. For example, 12:32:34 is rounded down to 12:32:00.     * Start time between 15 and 63 days ago - Round down to the nearest 5-minute clock interval. For example, 12:32:34 is rounded down to 12:30:00.     * Start time greater than 63 days ago - Round down to the nearest 1-hour clock interval. For example, 12:32:34 is rounded down to 12:00:00.
--
-- * 'gmsEndTime' - The time stamp that determines the last data point to return. The value specified is exclusive; results will include data points up to the specified time stamp. The time stamp must be in ISO 8601 UTC format (for example, 2016-10-10T23:00:00Z).
--
-- * 'gmsPeriod' - The granularity, in seconds, of the returned data points. A period can be as short as one minute (60 seconds) and must be a multiple of 60. The default value is 60. If the @StartTime@ parameter specifies a time stamp that is greater than 15 days ago, you must specify the period as follows or no data points in that time range is returned:     * Start time between 15 and 63 days ago - Use a multiple of 300 seconds (5 minutes).     * Start time greater than 63 days ago - Use a multiple of 3600 seconds (1 hour).
getMetricStatistics
    :: Text -- ^ 'gmsNamespace'
    -> Text -- ^ 'gmsMetricName'
    -> UTCTime -- ^ 'gmsStartTime'
    -> UTCTime -- ^ 'gmsEndTime'
    -> Natural -- ^ 'gmsPeriod'
    -> GetMetricStatistics
getMetricStatistics pNamespace_ pMetricName_ pStartTime_ pEndTime_ pPeriod_ =
    GetMetricStatistics'
    { _gmsExtendedStatistics = Nothing
    , _gmsStatistics = Nothing
    , _gmsDimensions = Nothing
    , _gmsUnit = Nothing
    , _gmsNamespace = pNamespace_
    , _gmsMetricName = pMetricName_
    , _gmsStartTime = _Time # pStartTime_
    , _gmsEndTime = _Time # pEndTime_
    , _gmsPeriod = _Nat # pPeriod_
    }

-- | The percentile statistics. Specify values between p0.0 and p100.
gmsExtendedStatistics :: Lens' GetMetricStatistics (Maybe (NonEmpty Text))
gmsExtendedStatistics = lens _gmsExtendedStatistics (\ s a -> s{_gmsExtendedStatistics = a}) . mapping _List1;

-- | The metric statistics, other than percentile. For percentile statistics, use @ExtendedStatistic@ .
gmsStatistics :: Lens' GetMetricStatistics (Maybe (NonEmpty Statistic))
gmsStatistics = lens _gmsStatistics (\ s a -> s{_gmsStatistics = a}) . mapping _List1;

-- | The dimensions. CloudWatch treats each unique combination of dimensions as a separate metric. You can't retrieve statistics using combinations of dimensions that were not specially published. You must specify the same dimensions that were used when the metrics were created. For an example, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#dimension-combinations Dimension Combinations> in the /Amazon CloudWatch User Guide/ .
gmsDimensions :: Lens' GetMetricStatistics [Dimension]
gmsDimensions = lens _gmsDimensions (\ s a -> s{_gmsDimensions = a}) . _Default . _Coerce;

-- | The unit for a given metric. Metrics may be reported in multiple units. Not supplying a unit results in all units being returned. If the metric only ever reports one unit, specifying a unit has no effect.
gmsUnit :: Lens' GetMetricStatistics (Maybe StandardUnit)
gmsUnit = lens _gmsUnit (\ s a -> s{_gmsUnit = a});

-- | The namespace of the metric, with or without spaces.
gmsNamespace :: Lens' GetMetricStatistics Text
gmsNamespace = lens _gmsNamespace (\ s a -> s{_gmsNamespace = a});

-- | The name of the metric, with or without spaces.
gmsMetricName :: Lens' GetMetricStatistics Text
gmsMetricName = lens _gmsMetricName (\ s a -> s{_gmsMetricName = a});

-- | The time stamp that determines the first data point to return. Note that start times are evaluated relative to the time that CloudWatch receives the request. The value specified is inclusive; results include data points with the specified time stamp. The time stamp must be in ISO 8601 UTC format (for example, 2016-10-03T23:00:00Z). CloudWatch rounds the specified time stamp as follows:     * Start time less than 15 days ago - Round down to the nearest whole minute. For example, 12:32:34 is rounded down to 12:32:00.     * Start time between 15 and 63 days ago - Round down to the nearest 5-minute clock interval. For example, 12:32:34 is rounded down to 12:30:00.     * Start time greater than 63 days ago - Round down to the nearest 1-hour clock interval. For example, 12:32:34 is rounded down to 12:00:00.
gmsStartTime :: Lens' GetMetricStatistics UTCTime
gmsStartTime = lens _gmsStartTime (\ s a -> s{_gmsStartTime = a}) . _Time;

-- | The time stamp that determines the last data point to return. The value specified is exclusive; results will include data points up to the specified time stamp. The time stamp must be in ISO 8601 UTC format (for example, 2016-10-10T23:00:00Z).
gmsEndTime :: Lens' GetMetricStatistics UTCTime
gmsEndTime = lens _gmsEndTime (\ s a -> s{_gmsEndTime = a}) . _Time;

-- | The granularity, in seconds, of the returned data points. A period can be as short as one minute (60 seconds) and must be a multiple of 60. The default value is 60. If the @StartTime@ parameter specifies a time stamp that is greater than 15 days ago, you must specify the period as follows or no data points in that time range is returned:     * Start time between 15 and 63 days ago - Use a multiple of 300 seconds (5 minutes).     * Start time greater than 63 days ago - Use a multiple of 3600 seconds (1 hour).
gmsPeriod :: Lens' GetMetricStatistics Natural
gmsPeriod = lens _gmsPeriod (\ s a -> s{_gmsPeriod = a}) . _Nat;

instance AWSRequest GetMetricStatistics where
        type Rs GetMetricStatistics =
             GetMetricStatisticsResponse
        request = postQuery cloudWatch
        response
          = receiveXMLWrapper "GetMetricStatisticsResult"
              (\ s h x ->
                 GetMetricStatisticsResponse' <$>
                   (x .@? "Datapoints" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Label")
                     <*> (pure (fromEnum s)))

instance Hashable GetMetricStatistics

instance NFData GetMetricStatistics

instance ToHeaders GetMetricStatistics where
        toHeaders = const mempty

instance ToPath GetMetricStatistics where
        toPath = const "/"

instance ToQuery GetMetricStatistics where
        toQuery GetMetricStatistics'{..}
          = mconcat
              ["Action" =: ("GetMetricStatistics" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "ExtendedStatistics" =:
                 toQuery
                   (toQueryList "member" <$> _gmsExtendedStatistics),
               "Statistics" =:
                 toQuery (toQueryList "member" <$> _gmsStatistics),
               "Dimensions" =:
                 toQuery (toQueryList "member" <$> _gmsDimensions),
               "Unit" =: _gmsUnit, "Namespace" =: _gmsNamespace,
               "MetricName" =: _gmsMetricName,
               "StartTime" =: _gmsStartTime,
               "EndTime" =: _gmsEndTime, "Period" =: _gmsPeriod]

-- | /See:/ 'getMetricStatisticsResponse' smart constructor.
data GetMetricStatisticsResponse = GetMetricStatisticsResponse'
    { _gmsrsDatapoints     :: !(Maybe [Datapoint])
    , _gmsrsLabel          :: !(Maybe Text)
    , _gmsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetMetricStatisticsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmsrsDatapoints' - The data points for the specified metric.
--
-- * 'gmsrsLabel' - A label for the specified metric.
--
-- * 'gmsrsResponseStatus' - -- | The response status code.
getMetricStatisticsResponse
    :: Int -- ^ 'gmsrsResponseStatus'
    -> GetMetricStatisticsResponse
getMetricStatisticsResponse pResponseStatus_ =
    GetMetricStatisticsResponse'
    { _gmsrsDatapoints = Nothing
    , _gmsrsLabel = Nothing
    , _gmsrsResponseStatus = pResponseStatus_
    }

-- | The data points for the specified metric.
gmsrsDatapoints :: Lens' GetMetricStatisticsResponse [Datapoint]
gmsrsDatapoints = lens _gmsrsDatapoints (\ s a -> s{_gmsrsDatapoints = a}) . _Default . _Coerce;

-- | A label for the specified metric.
gmsrsLabel :: Lens' GetMetricStatisticsResponse (Maybe Text)
gmsrsLabel = lens _gmsrsLabel (\ s a -> s{_gmsrsLabel = a});

-- | -- | The response status code.
gmsrsResponseStatus :: Lens' GetMetricStatisticsResponse Int
gmsrsResponseStatus = lens _gmsrsResponseStatus (\ s a -> s{_gmsrsResponseStatus = a});

instance NFData GetMetricStatisticsResponse
