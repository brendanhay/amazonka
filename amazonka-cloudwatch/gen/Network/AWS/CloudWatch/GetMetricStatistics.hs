{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.GetMetricStatistics
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets statistics for the specified metric.
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
    , gmsrqDimensions
    , gmsrqUnit
    , gmsrqNamespace
    , gmsrqMetricName
    , gmsrqStartTime
    , gmsrqEndTime
    , gmsrqPeriod
    , gmsrqStatistics

    -- * Response
    , GetMetricStatisticsResponse
    -- ** Response constructor
    , getMetricStatisticsResponse
    -- ** Response lenses
    , gmsrsDatapoints
    , gmsrsLabel
    , gmsrsStatus
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getMetricStatistics' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gmsrqDimensions'
--
-- * 'gmsrqUnit'
--
-- * 'gmsrqNamespace'
--
-- * 'gmsrqMetricName'
--
-- * 'gmsrqStartTime'
--
-- * 'gmsrqEndTime'
--
-- * 'gmsrqPeriod'
--
-- * 'gmsrqStatistics'
data GetMetricStatistics = GetMetricStatistics'
    { _gmsrqDimensions :: !(Maybe [Dimension])
    , _gmsrqUnit       :: !(Maybe StandardUnit)
    , _gmsrqNamespace  :: !Text
    , _gmsrqMetricName :: !Text
    , _gmsrqStartTime  :: !ISO8601
    , _gmsrqEndTime    :: !ISO8601
    , _gmsrqPeriod     :: !Nat
    , _gmsrqStatistics :: !(List1 Statistic)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetMetricStatistics' smart constructor.
getMetricStatistics :: Text -> Text -> UTCTime -> UTCTime -> Natural -> NonEmpty Statistic -> GetMetricStatistics
getMetricStatistics pNamespace_ pMetricName_ pStartTime_ pEndTime_ pPeriod_ pStatistics_ =
    GetMetricStatistics'
    { _gmsrqDimensions = Nothing
    , _gmsrqUnit = Nothing
    , _gmsrqNamespace = pNamespace_
    , _gmsrqMetricName = pMetricName_
    , _gmsrqStartTime = _Time # pStartTime_
    , _gmsrqEndTime = _Time # pEndTime_
    , _gmsrqPeriod = _Nat # pPeriod_
    , _gmsrqStatistics = _List1 # pStatistics_
    }

-- | A list of dimensions describing qualities of the metric.
gmsrqDimensions :: Lens' GetMetricStatistics [Dimension]
gmsrqDimensions = lens _gmsrqDimensions (\ s a -> s{_gmsrqDimensions = a}) . _Default;

-- | The unit for the metric.
gmsrqUnit :: Lens' GetMetricStatistics (Maybe StandardUnit)
gmsrqUnit = lens _gmsrqUnit (\ s a -> s{_gmsrqUnit = a});

-- | The namespace of the metric, with or without spaces.
gmsrqNamespace :: Lens' GetMetricStatistics Text
gmsrqNamespace = lens _gmsrqNamespace (\ s a -> s{_gmsrqNamespace = a});

-- | The name of the metric, with or without spaces.
gmsrqMetricName :: Lens' GetMetricStatistics Text
gmsrqMetricName = lens _gmsrqMetricName (\ s a -> s{_gmsrqMetricName = a});

-- | The time stamp to use for determining the first datapoint to return. The
-- value specified is inclusive; results include datapoints with the time
-- stamp specified.
gmsrqStartTime :: Lens' GetMetricStatistics UTCTime
gmsrqStartTime = lens _gmsrqStartTime (\ s a -> s{_gmsrqStartTime = a}) . _Time;

-- | The time stamp to use for determining the last datapoint to return. The
-- value specified is exclusive; results will include datapoints up to the
-- time stamp specified.
gmsrqEndTime :: Lens' GetMetricStatistics UTCTime
gmsrqEndTime = lens _gmsrqEndTime (\ s a -> s{_gmsrqEndTime = a}) . _Time;

-- | The granularity, in seconds, of the returned datapoints. @Period@ must
-- be at least 60 seconds and must be a multiple of 60. The default value
-- is 60.
gmsrqPeriod :: Lens' GetMetricStatistics Natural
gmsrqPeriod = lens _gmsrqPeriod (\ s a -> s{_gmsrqPeriod = a}) . _Nat;

-- | The metric statistics to return. For information about specific
-- statistics returned by GetMetricStatistics, go to
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/index.html?CHAP_TerminologyandKeyConcepts.html#Statistic Statistics>
-- in the /Amazon CloudWatch Developer Guide/.
--
-- Valid Values: @Average | Sum | SampleCount | Maximum | Minimum@
gmsrqStatistics :: Lens' GetMetricStatistics (NonEmpty Statistic)
gmsrqStatistics = lens _gmsrqStatistics (\ s a -> s{_gmsrqStatistics = a}) . _List1;

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
                 toQuery (toQueryList "member" <$> _gmsrqDimensions),
               "Unit" =: _gmsrqUnit, "Namespace" =: _gmsrqNamespace,
               "MetricName" =: _gmsrqMetricName,
               "StartTime" =: _gmsrqStartTime,
               "EndTime" =: _gmsrqEndTime, "Period" =: _gmsrqPeriod,
               "Statistics" =:
                 toQueryList "member" _gmsrqStatistics]

-- | The output for the GetMetricStatistics action.
--
-- /See:/ 'getMetricStatisticsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gmsrsDatapoints'
--
-- * 'gmsrsLabel'
--
-- * 'gmsrsStatus'
data GetMetricStatisticsResponse = GetMetricStatisticsResponse'
    { _gmsrsDatapoints :: !(Maybe [Datapoint])
    , _gmsrsLabel      :: !(Maybe Text)
    , _gmsrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetMetricStatisticsResponse' smart constructor.
getMetricStatisticsResponse :: Int -> GetMetricStatisticsResponse
getMetricStatisticsResponse pStatus_ =
    GetMetricStatisticsResponse'
    { _gmsrsDatapoints = Nothing
    , _gmsrsLabel = Nothing
    , _gmsrsStatus = pStatus_
    }

-- | The datapoints for the specified metric.
gmsrsDatapoints :: Lens' GetMetricStatisticsResponse [Datapoint]
gmsrsDatapoints = lens _gmsrsDatapoints (\ s a -> s{_gmsrsDatapoints = a}) . _Default;

-- | A label describing the specified metric.
gmsrsLabel :: Lens' GetMetricStatisticsResponse (Maybe Text)
gmsrsLabel = lens _gmsrsLabel (\ s a -> s{_gmsrsLabel = a});

-- | FIXME: Undocumented member.
gmsrsStatus :: Lens' GetMetricStatisticsResponse Int
gmsrsStatus = lens _gmsrsStatus (\ s a -> s{_gmsrsStatus = a});
