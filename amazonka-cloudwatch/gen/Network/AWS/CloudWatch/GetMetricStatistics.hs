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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets statistics for the specified metric.
--
-- The maximum number of data points returned from a single
-- 'GetMetricStatistics' request is 1,440, wereas the maximum number of
-- data points that can be queried is 50,850. If you make a request that
-- generates more than 1,440 data points, Amazon CloudWatch returns an
-- error. In such a case, you can alter the request by narrowing the
-- specified time range or increasing the specified period. Alternatively,
-- you can make multiple requests across adjacent time ranges.
--
-- Amazon CloudWatch aggregates data points based on the length of the
-- 'period' that you specify. For example, if you request statistics with a
-- one-minute granularity, Amazon CloudWatch aggregates data points with
-- time stamps that fall within the same one-minute period. In such a case,
-- the data points queried can greatly outnumber the data points returned.
--
-- The following examples show various statistics allowed by the data point
-- query maximum of 50,850 when you call 'GetMetricStatistics' on Amazon
-- EC2 instances with detailed (one-minute) monitoring enabled:
--
-- -   Statistics for up to 400 instances for a span of one hour
-- -   Statistics for up to 35 instances over a span of 24 hours
-- -   Statistics for up to 2 instances over a span of 2 weeks
--
-- For information about the namespace, metric names, and dimensions that
-- other Amazon Web Services products use to send metrics to Cloudwatch, go
-- to
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Metrics, Namespaces, and Dimensions Reference>
-- in the /Amazon CloudWatch Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html AWS API Reference> for GetMetricStatistics.
module Network.AWS.CloudWatch.GetMetricStatistics
    (
    -- * Creating a Request
      getMetricStatistics
    , GetMetricStatistics
    -- * Request Lenses
    , gmsDimensions
    , gmsUnit
    , gmsNamespace
    , gmsMetricName
    , gmsStartTime
    , gmsEndTime
    , gmsPeriod
    , gmsStatistics

    -- * Destructuring the Response
    , getMetricStatisticsResponse
    , GetMetricStatisticsResponse
    -- * Response Lenses
    , gmsrsDatapoints
    , gmsrsLabel
    , gmsrsStatus
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.CloudWatch.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getMetricStatistics' smart constructor.
data GetMetricStatistics = GetMetricStatistics'
    { _gmsDimensions :: !(Maybe [Dimension])
    , _gmsUnit       :: !(Maybe StandardUnit)
    , _gmsNamespace  :: !Text
    , _gmsMetricName :: !Text
    , _gmsStartTime  :: !ISO8601
    , _gmsEndTime    :: !ISO8601
    , _gmsPeriod     :: !Nat
    , _gmsStatistics :: !(List1 Statistic)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetMetricStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmsDimensions'
--
-- * 'gmsUnit'
--
-- * 'gmsNamespace'
--
-- * 'gmsMetricName'
--
-- * 'gmsStartTime'
--
-- * 'gmsEndTime'
--
-- * 'gmsPeriod'
--
-- * 'gmsStatistics'
getMetricStatistics
    :: Text -- ^ 'gmsNamespace'
    -> Text -- ^ 'gmsMetricName'
    -> UTCTime -- ^ 'gmsStartTime'
    -> UTCTime -- ^ 'gmsEndTime'
    -> Natural -- ^ 'gmsPeriod'
    -> NonEmpty Statistic -- ^ 'gmsStatistics'
    -> GetMetricStatistics
getMetricStatistics pNamespace_ pMetricName_ pStartTime_ pEndTime_ pPeriod_ pStatistics_ =
    GetMetricStatistics'
    { _gmsDimensions = Nothing
    , _gmsUnit = Nothing
    , _gmsNamespace = pNamespace_
    , _gmsMetricName = pMetricName_
    , _gmsStartTime = _Time # pStartTime_
    , _gmsEndTime = _Time # pEndTime_
    , _gmsPeriod = _Nat # pPeriod_
    , _gmsStatistics = _List1 # pStatistics_
    }

-- | A list of dimensions describing qualities of the metric.
gmsDimensions :: Lens' GetMetricStatistics [Dimension]
gmsDimensions = lens _gmsDimensions (\ s a -> s{_gmsDimensions = a}) . _Default . _Coerce;

-- | The unit for the metric.
gmsUnit :: Lens' GetMetricStatistics (Maybe StandardUnit)
gmsUnit = lens _gmsUnit (\ s a -> s{_gmsUnit = a});

-- | The namespace of the metric, with or without spaces.
gmsNamespace :: Lens' GetMetricStatistics Text
gmsNamespace = lens _gmsNamespace (\ s a -> s{_gmsNamespace = a});

-- | The name of the metric, with or without spaces.
gmsMetricName :: Lens' GetMetricStatistics Text
gmsMetricName = lens _gmsMetricName (\ s a -> s{_gmsMetricName = a});

-- | The time stamp to use for determining the first datapoint to return. The
-- value specified is inclusive; results include datapoints with the time
-- stamp specified.
gmsStartTime :: Lens' GetMetricStatistics UTCTime
gmsStartTime = lens _gmsStartTime (\ s a -> s{_gmsStartTime = a}) . _Time;

-- | The time stamp to use for determining the last datapoint to return. The
-- value specified is exclusive; results will include datapoints up to the
-- time stamp specified.
gmsEndTime :: Lens' GetMetricStatistics UTCTime
gmsEndTime = lens _gmsEndTime (\ s a -> s{_gmsEndTime = a}) . _Time;

-- | The granularity, in seconds, of the returned datapoints. 'Period' must
-- be at least 60 seconds and must be a multiple of 60. The default value
-- is 60.
gmsPeriod :: Lens' GetMetricStatistics Natural
gmsPeriod = lens _gmsPeriod (\ s a -> s{_gmsPeriod = a}) . _Nat;

-- | The metric statistics to return. For information about specific
-- statistics returned by GetMetricStatistics, go to
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/index.html?CHAP_TerminologyandKeyConcepts.html#Statistic Statistics>
-- in the /Amazon CloudWatch Developer Guide/.
--
-- Valid Values: 'Average | Sum | SampleCount | Maximum | Minimum'
gmsStatistics :: Lens' GetMetricStatistics (NonEmpty Statistic)
gmsStatistics = lens _gmsStatistics (\ s a -> s{_gmsStatistics = a}) . _List1;

instance AWSRequest GetMetricStatistics where
        type Sv GetMetricStatistics = CloudWatch
        type Rs GetMetricStatistics =
             GetMetricStatisticsResponse
        request = postQuery
        response
          = receiveXMLWrapper "GetMetricStatisticsResult"
              (\ s h x ->
                 GetMetricStatisticsResponse' <$>
                   (x .@? "Datapoints" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Label")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetMetricStatistics where
        toHeaders = const mempty

instance ToPath GetMetricStatistics where
        toPath = const "/"

instance ToQuery GetMetricStatistics where
        toQuery GetMetricStatistics'{..}
          = mconcat
              ["Action" =: ("GetMetricStatistics" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "Dimensions" =:
                 toQuery (toQueryList "member" <$> _gmsDimensions),
               "Unit" =: _gmsUnit, "Namespace" =: _gmsNamespace,
               "MetricName" =: _gmsMetricName,
               "StartTime" =: _gmsStartTime,
               "EndTime" =: _gmsEndTime, "Period" =: _gmsPeriod,
               "Statistics" =: toQueryList "member" _gmsStatistics]

-- | The output for the GetMetricStatistics action.
--
-- /See:/ 'getMetricStatisticsResponse' smart constructor.
data GetMetricStatisticsResponse = GetMetricStatisticsResponse'
    { _gmsrsDatapoints :: !(Maybe [Datapoint])
    , _gmsrsLabel      :: !(Maybe Text)
    , _gmsrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetMetricStatisticsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmsrsDatapoints'
--
-- * 'gmsrsLabel'
--
-- * 'gmsrsStatus'
getMetricStatisticsResponse
    :: Int -- ^ 'gmsrsStatus'
    -> GetMetricStatisticsResponse
getMetricStatisticsResponse pStatus_ =
    GetMetricStatisticsResponse'
    { _gmsrsDatapoints = Nothing
    , _gmsrsLabel = Nothing
    , _gmsrsStatus = pStatus_
    }

-- | The datapoints for the specified metric.
gmsrsDatapoints :: Lens' GetMetricStatisticsResponse [Datapoint]
gmsrsDatapoints = lens _gmsrsDatapoints (\ s a -> s{_gmsrsDatapoints = a}) . _Default . _Coerce;

-- | A label describing the specified metric.
gmsrsLabel :: Lens' GetMetricStatisticsResponse (Maybe Text)
gmsrsLabel = lens _gmsrsLabel (\ s a -> s{_gmsrsLabel = a});

-- | The response status code.
gmsrsStatus :: Lens' GetMetricStatisticsResponse Int
gmsrsStatus = lens _gmsrsStatus (\ s a -> s{_gmsrsStatus = a});
