{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatch.GetMetricStatistics
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Gets statistics for the specified metric.
--
-- The maximum number of data points returned from a single
-- @GetMetricStatistics@ request is 1,440, wereas the maximum number of
-- data points that can be queried is 50,850. If you make a request that
-- generates more than 1,440 data points, Amazon CloudWatch returns an
-- error. In such a case, you can alter the request by narrowing the
-- specified time range or increasing the specified period. Alternatively,
-- you can make multiple requests across adjacent time ranges.
--
-- Amazon CloudWatch aggregates data points based on the length of the
-- @period@ that you specify. For example, if you request statistics with a
-- one-minute granularity, Amazon CloudWatch aggregates data points with
-- time stamps that fall within the same one-minute period. In such a case,
-- the data points queried can greatly outnumber the data points returned.
--
-- The following examples show various statistics allowed by the data point
-- query maximum of 50,850 when you call @GetMetricStatistics@ on Amazon
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
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html>
module Network.AWS.CloudWatch.GetMetricStatistics
    (
    -- * Request
      GetMetricStatistics
    -- ** Request constructor
    , getMetricStatistics
    -- ** Request lenses
    , gmsDimensions
    , gmsUnit
    , gmsNamespace
    , gmsMetricName
    , gmsStartTime
    , gmsEndTime
    , gmsPeriod
    , gmsStatistics

    -- * Response
    , GetMetricStatisticsResponse
    -- ** Response constructor
    , getMetricStatisticsResponse
    -- ** Response lenses
    , gmsrDatapoints
    , gmsrLabel
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudWatch.Types

-- | /See:/ 'getMetricStatistics' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
data GetMetricStatistics = GetMetricStatistics'{_gmsDimensions :: Maybe [Dimension], _gmsUnit :: Maybe StandardUnit, _gmsNamespace :: Text, _gmsMetricName :: Text, _gmsStartTime :: ISO8601, _gmsEndTime :: ISO8601, _gmsPeriod :: Nat, _gmsStatistics :: List1 Statistic} deriving (Eq, Read, Show)

-- | 'GetMetricStatistics' smart constructor.
getMetricStatistics :: Text -> Text -> UTCTime -> UTCTime -> Natural -> NonEmpty Statistic -> GetMetricStatistics
getMetricStatistics pNamespace pMetricName pStartTime pEndTime pPeriod pStatistics = GetMetricStatistics'{_gmsDimensions = Nothing, _gmsUnit = Nothing, _gmsNamespace = pNamespace, _gmsMetricName = pMetricName, _gmsStartTime = _Time # pStartTime, _gmsEndTime = _Time # pEndTime, _gmsPeriod = _Nat # pPeriod, _gmsStatistics = _List1 # pStatistics};

-- | A list of dimensions describing qualities of the metric.
gmsDimensions :: Lens' GetMetricStatistics [Dimension]
gmsDimensions = lens _gmsDimensions (\ s a -> s{_gmsDimensions = a}) . _Default;

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

-- | The granularity, in seconds, of the returned datapoints. @Period@ must
-- be at least 60 seconds and must be a multiple of 60. The default value
-- is 60.
gmsPeriod :: Lens' GetMetricStatistics Natural
gmsPeriod = lens _gmsPeriod (\ s a -> s{_gmsPeriod = a}) . _Nat;

-- | The metric statistics to return. For information about specific
-- statistics returned by GetMetricStatistics, go to
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/index.html?CHAP_TerminologyandKeyConcepts.html#Statistic Statistics>
-- in the /Amazon CloudWatch Developer Guide/.
--
-- Valid Values: @Average | Sum | SampleCount | Maximum | Minimum@
gmsStatistics :: Lens' GetMetricStatistics (NonEmpty Statistic)
gmsStatistics = lens _gmsStatistics (\ s a -> s{_gmsStatistics = a}) . _List1;

instance AWSRequest GetMetricStatistics where
        type Sv GetMetricStatistics = CloudWatch
        type Rs GetMetricStatistics =
             GetMetricStatisticsResponse
        request = post
        response
          = receiveXMLWrapper "GetMetricStatisticsResult"
              (\ s h x ->
                 GetMetricStatisticsResponse' <$>
                   (x .@? "Datapoints" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Label"))

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

-- | /See:/ 'getMetricStatisticsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gmsrDatapoints'
--
-- * 'gmsrLabel'
data GetMetricStatisticsResponse = GetMetricStatisticsResponse'{_gmsrDatapoints :: Maybe [Datapoint], _gmsrLabel :: Maybe Text} deriving (Eq, Read, Show)

-- | 'GetMetricStatisticsResponse' smart constructor.
getMetricStatisticsResponse :: GetMetricStatisticsResponse
getMetricStatisticsResponse = GetMetricStatisticsResponse'{_gmsrDatapoints = Nothing, _gmsrLabel = Nothing};

-- | The datapoints for the specified metric.
gmsrDatapoints :: Lens' GetMetricStatisticsResponse [Datapoint]
gmsrDatapoints = lens _gmsrDatapoints (\ s a -> s{_gmsrDatapoints = a}) . _Default;

-- | A label describing the specified metric.
gmsrLabel :: Lens' GetMetricStatisticsResponse (Maybe Text)
gmsrLabel = lens _gmsrLabel (\ s a -> s{_gmsrLabel = a});
